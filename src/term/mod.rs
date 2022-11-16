//! AST of a Nickel expression.
//!
//! # Core language
//!
//! At its core, Nickel is a lazy JSON with higher-order functions. It includes:
//! - Basic values: booleans, numerals, string
//! - Data structures: arrays and records
//! - Binders: functions and let bindings
//!
//! It also features type annotations (promise and assume), and other typechecking related
//! constructs (label, symbols, etc.).
//!
//! # Enriched values
//!
//! Enriched values are special terms used to represent metadata about record fields: types or
//! contracts, default values, documentation, etc. They bring such usually external object down to
//! the term level, and together with [crate::eval::merge], they allow for flexible and modular
//! definitions of contracts, record and metadata all together.

pub mod array;
pub mod record;

use array::{Array, ArrayAttrs};
use record::{Field, FieldDeps, FieldMetadata, RecordData, RecordDeps};

use crate::{
    destructuring::RecordPattern,
    error::{EvalError, ParseError},
    identifier::Ident,
    label::Label,
    match_sharedterm,
    position::TermPos,
    types::{TypeF, Types, UnboundTypeVariableError},
};

use codespan::FileId;

use serde::{Deserialize, Serialize};

use std::{
    cmp::{Ordering, PartialOrd},
    collections::HashMap,
    ffi::OsString,
    fmt,
    ops::Deref,
    rc::Rc,
};

/// The AST of a Nickel expression.
///
/// Parsed terms also need to store their position in the source for error reporting.  This is why
/// this type is nested with [`RichTerm`].
///
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Term {
    /// The null value.
    Null,
    /// A boolean value.
    Bool(bool),
    /// A floating-point value.
    #[serde(serialize_with = "crate::serialize::serialize_num")]
    Num(f64),
    /// A literal string.
    Str(String),
    /// A string containing interpolated expressions, represented as a list of either literals or
    /// expressions.
    ///
    /// /|\ CHUNKS ARE STORED IN REVERSE ORDER. As they will be only popped one by one from the
    /// head of the list during evaluation, doing so on a `Vec` is costly, and using a more complex
    /// data structure is not really necessary, as once created, no other than popping is ever
    /// done.  In consequence, we just reverse the vector at parsing time, so that we can then pop
    /// efficiently from the back of it.
    #[serde(skip)]
    StrChunks(Vec<StrChunk<RichTerm>>),
    /// A standard function.
    #[serde(skip)]
    Fun(Ident, RichTerm),
    /// A function able to destruct its arguments.
    #[serde(skip)]
    FunPattern(Option<Ident>, RecordPattern, RichTerm),
    /// A blame label.
    #[serde(skip)]
    Lbl(Label),

    /// A let binding.
    #[serde(skip)]
    Let(Ident, RichTerm, RichTerm, LetAttrs),
    /// A destructuring let-binding.
    #[serde(skip)]
    LetPattern(Option<Ident>, RecordPattern, RichTerm, RichTerm),
    /// An application.
    #[serde(skip)]
    App(RichTerm, RichTerm),
    /// A variable.
    #[serde(skip)]
    Var(Ident),

    /// An enum variant.
    Enum(Ident),

    /// A record, mapping identifiers to terms.
    #[serde(serialize_with = "crate::serialize::serialize_record")]
    #[serde(deserialize_with = "crate::serialize::deserialize_record")]
    Record(RecordData),
    /// A recursive record, where the fields can reference each others.
    #[serde(skip)]
    RecRecord(
        RecordData,
        Vec<(RichTerm, Field)>, /* field whose name is defined by interpolation */
        Option<RecordDeps>, /* dependency tracking between fields. None before the free var pass */
    ),
    /// A match construct. Correspond only to the match cases: this expression is still to be
    /// applied to an argument to match on. Once applied, the evaluation is done by the
    /// corresponding primitive operator. Still, we need this construct for typechecking and being
    /// able to handle yet unapplied match expressions.
    #[serde(skip)]
    Match {
        cases: HashMap<Ident, RichTerm>,
        default: Option<RichTerm>,
    },

    /// An array.
    #[serde(serialize_with = "crate::serialize::serialize_array")]
    #[serde(deserialize_with = "crate::serialize::deserialize_array")]
    Array(Array, ArrayAttrs),

    /// A primitive unary operator.
    #[serde(skip)]
    Op1(UnaryOp, RichTerm),
    /// A primitive binary operator.
    #[serde(skip)]
    Op2(BinaryOp, RichTerm, RichTerm),
    /// An primitive n-ary operator.
    #[serde(skip)]
    OpN(NAryOp, Vec<RichTerm>),

    /// A key locking a sealed term.
    ///
    /// A unique key corresponding to a type variable. See [`Term::Sealed`] below.
    #[serde(skip)]
    SealingKey(SealingKey),

    /// A sealed term.
    ///
    /// Sealed terms are introduced by contracts on polymorphic types. Take the following example:
    ///
    /// ```text
    /// let f = Assume(forall a. forall b. a -> b -> a, fun x y => y) in
    /// f true "a"
    /// ```
    ///
    /// This function is ill-typed. To check that, a polymorphic contract will:
    /// - Assign a unique identifier to each type variable: say `a => 1`, `b => 2`
    /// - For each cast on a negative occurrence of a type variable `a` or `b` (corresponding to an
    /// argument position), tag the argument with the associated identifier. In our example, `f
    /// true "a"` will push `Sealed(1, true)` then `Sealed(2, "a")` on the stack.
    /// - For each cast on a positive occurrence of a type variable, this contract check that the
    /// term is of the form `Sealed(id, term)` where `id` corresponds to the identifier of the
    /// type variable. In our example, the last cast to `a` finds `Sealed(2, "a")`, while it
    /// expected `Sealed(1, _)`, hence it raises a positive blame.
    #[serde(skip)]
    Sealed(SealingKey, RichTerm, Label),

    /// A term with a type and/or contract annotation.
    #[serde(serialize_with = "crate::serialize::serialize_annotated_value")]
    #[serde(skip_deserializing)]
    Annotated(TypeAnnotation, RichTerm),

    /// An unresolved import.
    #[serde(skip)]
    Import(OsString),
    /// A resolved import (which has already been loaded and parsed).
    #[serde(skip)]
    ResolvedImport(FileId),

    /// A term that couldn't be parsed properly. Used by the LSP to handle partially valid
    /// programs.
    #[serde(skip)]
    ParseError(ParseError),
    /// A delayed runtime error. Usually, errors are raised and abort the execution right away,
    /// without the need to store them in the AST. However, some cases require a term which aborts
    /// with a specific error if evaluated, but is fine being stored and passed around.
    ///
    /// The main use-cae is currently missing field definitions: when evaluating a recursive record
    /// to a normal record with a recursive environment, we might find fields that aren't defined
    /// currently, eg:
    ///
    /// ```nickel
    /// let r = {
    ///   foo = bar + 1,
    ///   bar | Num,
    ///   baz = 2,
    /// } in
    /// r.baz + (r & {bar = 1}).foo
    /// ```
    ///
    /// This program is valid, but when evaluating `r` in `r.baz`, `bar` doesn't have a definition
    /// yet. This is fine because we don't evaluate `bar` nor `foo`. Still, we have to put
    /// something in the recursive environment. And if we wrote `r.foo` instead, we should raise a
    /// missing field definition error. Thus, we need to bind `bar` to a term wich, if ever
    /// evaluated, will raise a proper missing field definition error. This is precisely the
    /// behavior of `RuntimeError` behaves.
    #[serde(skip)]
    RuntimeError(EvalError),
}

pub type SealingKey = i32;

/// Type of let-binding. This only affects run-time behavior. Revertible bindings introduce
/// revertible cache elements at evaluation, which are devices used for the implementation of recursive
/// records merging. See the [`crate::eval::merge`] and [`crate::eval`] modules for more details.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BindingType {
    Normal,
    /// In the revertible case, we also store an optional set of dependencies. See
    /// [`crate::transform::free_vars`] for more details.
    Revertible(FieldDeps),
}

impl Default for BindingType {
    fn default() -> Self {
        BindingType::Normal
    }
}

/// A contract with its associated data.
#[derive(Debug, PartialEq, Clone)]
pub struct PendingContract {
    /// The pending contract, can be a function or a record.
    pub contract: RichTerm,
    /// The blame label.
    pub label: Label,
}

impl PendingContract {
    pub fn new(contract: RichTerm, label: Label) -> Self {
        PendingContract { contract, label }
    }

    /// Map a function over the term representing the underlying contract.
    pub fn map_contract<F>(self, f: F) -> Self
    where
        F: FnOnce(RichTerm) -> RichTerm,
    {
        PendingContract {
            contract: f(self.contract),
            ..self
        }
    }

    /// Apply a series of pending contracts to a given term.
    pub fn apply_all<I>(rt: RichTerm, contracts: I, pos: TermPos) -> RichTerm
    where
        I: Iterator<Item = Self>,
    {
        use crate::mk_app;

        contracts.fold(rt, |acc, ctr| {
            mk_app!(
                make::op2(BinaryOp::Assume(), ctr.contract, Term::Lbl(ctr.label)).with_pos(pos),
                acc
            )
            .with_pos(pos)
        })
    }
}

impl Traverse<RichTerm> for PendingContract {
    fn traverse<F, S, E>(self, f: &F, state: &mut S, order: TraverseOrder) -> Result<Self, E>
    where
        F: Fn(RichTerm, &mut S) -> Result<RichTerm, E>,
    {
        let contract = self.contract.traverse(f, state, order)?;
        Ok(PendingContract { contract, ..self })
    }
}

impl std::convert::TryFrom<LabeledType> for PendingContract {
    type Error = UnboundTypeVariableError;

    fn try_from(labeled_ty: LabeledType) -> Result<Self, Self::Error> {
        Ok(PendingContract::new(
            labeled_ty.types.contract()?,
            labeled_ty.label,
        ))
    }
}

/// The attributes of a let binding.
#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct LetAttrs {
    /// The type of a let binding. See the documentation of [`BindingType`].
    pub binding_type: BindingType,
    /// A recursive let binding adds its binding to the environment of the expression.
    pub rec: bool,
}

/// The metadata that can be attached to a let.
#[derive(Default, Clone)]
pub struct LetMetadata {
    pub doc: Option<String>,
    pub annotation: TypeAnnotation,
}

impl From<LetMetadata> for record::FieldMetadata {
    fn from(let_metadata: LetMetadata) -> Self {
        record::FieldMetadata {
            annotation: let_metadata.annotation,
            doc: let_metadata.doc,
            ..Default::default()
        }
    }
}

/// A wrapper around f64 which makes `NaN` not representable. As opposed to floats, it is `Eq` and
/// `Ord`.
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub struct NumeralPriority(f64);

/// Error raised when trying to convert a float with `NaN` value to a `NumeralPriority`.
#[derive(Debug, Copy, Clone)]
pub struct PriorityIsNaN;

// The following impl are ok because `NumeralPriority(NaN)` can't be constructed.
impl Eq for NumeralPriority {}

// We can't derive `Ord` because there is an `f64` inside
// but it is actually an `Ord` because `NaN` is forbidden.
// See `TryFrom` smart constructor.
#[allow(clippy::derive_ord_xor_partial_ord)]
impl Ord for NumeralPriority {
    fn cmp(&self, other: &Self) -> Ordering {
        // Ok: NaN is forbidden
        self.partial_cmp(other).unwrap()
    }
}

impl NumeralPriority {
    pub fn zero() -> Self {
        NumeralPriority(0.0)
    }
}

impl TryFrom<f64> for NumeralPriority {
    type Error = PriorityIsNaN;

    fn try_from(f: f64) -> Result<Self, Self::Error> {
        if f.is_nan() {
            Err(PriorityIsNaN)
        } else {
            Ok(NumeralPriority(f))
        }
    }
}

impl From<NumeralPriority> for f64 {
    fn from(n: NumeralPriority) -> Self {
        n.0
    }
}

impl fmt::Display for NumeralPriority {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum MergePriority {
    /// The priority of default values that are overridden by everything else.
    Bottom,
    /// The priority by default, when no priority annotation (`default`, `force`, `priority`) is
    /// provided.
    ///
    /// Act as the value `MergePriority::Numeral(0.0)` with respect to ordering and equality
    /// testing. The only way to discriminate this variant is to pattern match on it.
    Neutral,
    /// A numeral priority.
    Numeral(NumeralPriority),
    /// The priority of values that override everything else and can't be overridden.
    Top,
}

impl PartialOrd for MergePriority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for MergePriority {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (MergePriority::Bottom, MergePriority::Bottom)
            | (MergePriority::Neutral, MergePriority::Neutral)
            | (MergePriority::Top, MergePriority::Top) => true,
            (MergePriority::Numeral(p1), MergePriority::Numeral(p2)) => p1 == p2,
            (MergePriority::Neutral, MergePriority::Numeral(p))
            | (MergePriority::Numeral(p), MergePriority::Neutral)
                if p == &NumeralPriority::zero() =>
            {
                true
            }
            _ => false,
        }
    }
}

impl Eq for MergePriority {}

impl Ord for MergePriority {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            // Equalities
            (MergePriority::Bottom, MergePriority::Bottom)
            | (MergePriority::Top, MergePriority::Top)
            | (MergePriority::Neutral, MergePriority::Neutral) => Ordering::Equal,
            (MergePriority::Numeral(p1), MergePriority::Numeral(p2)) => p1.cmp(p2),

            // Top and bottom.
            (MergePriority::Bottom, _) | (_, MergePriority::Top) => Ordering::Less,
            (MergePriority::Top, _) | (_, MergePriority::Bottom) => Ordering::Greater,

            // Neutral and numeral.
            (MergePriority::Neutral, MergePriority::Numeral(n)) => NumeralPriority::zero().cmp(n),
            (MergePriority::Numeral(n), MergePriority::Neutral) => n.cmp(&NumeralPriority::zero()),
        }
    }
}

impl Default for MergePriority {
    fn default() -> Self {
        Self::Neutral
    }
}

impl fmt::Display for MergePriority {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MergePriority::Bottom => write!(f, "default"),
            MergePriority::Neutral => write!(f, "{}", NumeralPriority::zero()),
            MergePriority::Numeral(p) => write!(f, "{p}"),
            MergePriority::Top => write!(f, "force"),
        }
    }
}

/// A type or a contract together with its corresponding label.
#[derive(Debug, PartialEq, Clone)]
pub struct LabeledType {
    pub types: Types,
    pub label: Label,
}

impl Traverse<RichTerm> for LabeledType {
    // Note that this function doesn't traverse the label, which is most often what you want. The
    // terms that may hide in a label are mostly types used for error reporting, but are never
    // evaluated.
    fn traverse<F, S, E>(self, f: &F, state: &mut S, order: TraverseOrder) -> Result<LabeledType, E>
    where
        F: Fn(RichTerm, &mut S) -> Result<RichTerm, E>,
    {
        let LabeledType { types, label } = self;
        types
            .traverse(f, state, order)
            .map(|types| LabeledType { types, label })
    }
}

/// A type and/or contract annotation.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct TypeAnnotation {
    /// The type annotation (using `:`).
    pub types: Option<LabeledType>,
    /// The contracts annotation (using `|`).
    pub contracts: Vec<LabeledType>,
}

impl TypeAnnotation {
    /// Return the main annotation, which is either the type annotation if any, or the first
    /// contract annotation.
    pub fn first(&self) -> Option<&LabeledType> {
        self.iter().next()
    }

    /// Iterate over the annotations, starting by the type and followed by the contracts.
    pub fn iter(&self) -> impl Iterator<Item = &LabeledType> {
        self.types.iter().chain(self.contracts.iter())
    }

    /// Mutably iterate over the annotations, starting by the type and followed by the contracts.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut LabeledType> {
        self.types.iter_mut().chain(self.contracts.iter_mut())
    }

    /// Return a string representation of the contracts (without the static type annotation) as a
    /// comma-separated list.
    pub fn contracts_to_string(&self) -> Option<String> {
        (!self.contracts.is_empty()).then(|| {
            self.contracts
                .iter()
                .map(|contract| format!("{}", contract.label.types,))
                .collect::<Vec<_>>()
                .join(",")
        })
    }

    /// Build a list of pending contracts from this type annotation.
    pub fn as_pending_contracts(&self) -> Result<Vec<PendingContract>, UnboundTypeVariableError> {
        self.iter()
            .cloned()
            .map(PendingContract::try_from)
            .collect::<Result<Vec<_>, _>>()
    }
}

impl From<TypeAnnotation> for LetMetadata {
    fn from(annotation: TypeAnnotation) -> Self {
        LetMetadata {
            annotation,
            ..Default::default()
        }
    }
}

impl Traverse<RichTerm> for TypeAnnotation {
    fn traverse<F, S, E>(self, f: &F, state: &mut S, order: TraverseOrder) -> Result<Self, E>
    where
        F: Fn(RichTerm, &mut S) -> Result<RichTerm, E>,
    {
        let TypeAnnotation { types, contracts } = self;

        let contracts = contracts
            .into_iter()
            .map(|labeled_ty| labeled_ty.traverse(f, state, order))
            .collect::<Result<Vec<_>, _>>()?;

        let types = types
            .map(|labeled_ty| labeled_ty.traverse(f, state, order))
            .transpose()?;

        Ok(TypeAnnotation { types, contracts })
    }
}

/// A chunk of a string with interpolated expressions inside. Can be either a string literal or an
/// interpolated expression.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StrChunk<E> {
    /// A string literal.
    Literal(String),
    /// An interpolated expression.
    Expr(
        E,     /* the expression */
        usize, /* the indentation level (see parser::utils::strip_indent) */
    ),
}

#[cfg(test)]
impl<E> StrChunk<E> {
    pub fn expr(e: E) -> Self {
        StrChunk::Expr(e, 0)
    }
}

impl Term {
    /// Recursively apply a function to all `Term`s contained in a `RichTerm`.
    pub fn apply_to_rich_terms<F>(&mut self, func: F)
    where
        F: Fn(&mut RichTerm),
    {
        use self::Term::*;
        match self {
            Null | ParseError(_) | RuntimeError(_) => (),
            Match {
                ref mut cases,
                ref mut default,
            } => {
                cases.iter_mut().for_each(|c| {
                    let (_, t) = c;
                    func(t);
                });
                if let Some(default) = default {
                    func(default)
                }
            }
            Record(ref mut r) => {
                r.fields.iter_mut().for_each(|(_, field)| {
                    if let Some(ref mut value) = field.value {
                        func(value);
                    }
                });
            }
            RecRecord(ref mut r, ref mut dyn_fields, ..) => {
                r.fields.iter_mut().for_each(|(_, field)| {
                    if let Some(ref mut value) = field.value {
                        func(value);
                    }
                });
                dyn_fields.iter_mut().for_each(|(id_t, field)| {
                    func(id_t);

                    if let Some(ref mut value) = field.value {
                        func(value);
                    }
                });
            }
            Bool(_) | Num(_) | Str(_) | Lbl(_) | Var(_) | SealingKey(_) | Enum(_) | Import(_)
            | ResolvedImport(_) => {}
            Fun(_, ref mut t)
            | FunPattern(_, _, ref mut t)
            | Op1(_, ref mut t)
            | Sealed(_, ref mut t, _) => {
                func(t);
            }
            Annotated(ref mut annot, ref mut t) => {
                annot.iter_mut().for_each(|LabeledType { types, .. }| {
                    if let TypeF::Flat(ref mut rt) = types.types {
                        func(rt)
                    }
                });

                func(t);
            }
            Let(_, ref mut t1, ref mut t2, _)
            | LetPattern(_, _, ref mut t1, ref mut t2)
            | App(ref mut t1, ref mut t2)
            | Op2(_, ref mut t1, ref mut t2) => {
                func(t1);
                func(t2);
            }
            OpN(_, ref mut terms) => terms.iter_mut().for_each(|t| {
                func(t);
            }),
            Array(ref mut terms, _) => terms.make_mut().iter_mut().for_each(func),
            StrChunks(chunks) => chunks.iter_mut().for_each(|chunk| match chunk {
                StrChunk::Literal(_) => (),
                StrChunk::Expr(e, _) => func(e),
            }),
        }
    }

    /// Return the class of an expression in WHNF.
    ///
    /// The class of an expression is an approximation of its type used in error reporting. Class
    /// and type coincide for constants (numbers, strings and booleans) and arrays. Otherwise the
    /// class is less precise than the type and indicates the general shape of the term: `"Record"`
    /// for records, `"Fun`" for functions, etc. If the term is not a WHNF, `None` is returned.
    pub fn type_of(&self) -> Option<String> {
        match self {
            Term::Null => Some("Null"),
            Term::Bool(_) => Some("Bool"),
            Term::Num(_) => Some("Num"),
            Term::Str(_) => Some("Str"),
            Term::Fun(_, _) | Term::FunPattern(_, _, _) => Some("Fun"),
            Term::Match { .. } => Some("MatchExpression"),
            Term::Lbl(_) => Some("Label"),
            Term::Enum(_) => Some("Enum"),
            Term::Record(..) | Term::RecRecord(..) => Some("Record"),
            Term::Array(..) => Some("Array"),
            Term::SealingKey(_) => Some("SealingKey"),
            Term::Sealed(..) => Some("Sealed"),
            Term::Annotated(..) => Some("Annotated"),
            Term::Let(..)
            | Term::LetPattern(..)
            | Term::App(_, _)
            | Term::Var(_)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::OpN(..)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_)
            | Term::ParseError(_)
            | Term::RuntimeError(_) => None,
        }
        .map(String::from)
    }

    /// Return a shallow string representation of a term, used for error reporting.
    pub fn shallow_repr(&self) -> String {
        match self {
            Term::Null => String::from("null"),
            Term::Bool(true) => String::from("true"),
            Term::Bool(false) => String::from("false"),
            Term::Num(n) => format!("{n}"),
            Term::Str(s) => format!("\"{s}\""),
            Term::StrChunks(chunks) => {
                let chunks_str: Vec<String> = chunks
                    .iter()
                    .map(|chunk| match chunk {
                        StrChunk::Literal(s) => s,
                        StrChunk::Expr(..) => "${ ... }",
                    })
                    .map(String::from)
                    .collect();

                format!("\"{}\"", chunks_str.join(""))
            }
            Term::Fun(_, _) | Term::FunPattern(_, _, _) => String::from("<func>"),
            Term::Match { .. } => String::from("<func (match expr)>"),
            Term::Lbl(_) => String::from("<label>"),
            Term::Enum(id) => {
                let re = regex::Regex::new("_?[a-zA-Z][_a-zA-Z0-9]*").unwrap();
                let s = id.to_string();
                if re.is_match(&s) {
                    format!("`{s}")
                } else {
                    format!("`\"{s}\"")
                }
            }
            Term::Record(..) | Term::RecRecord(..) => String::from("{ ... }"),
            Term::Array(..) => String::from("[ ... ]"),
            Term::SealingKey(_) => String::from("<sealing key>"),
            Term::Sealed(..) => String::from("<sealed>"),
            Term::Annotated(_, t) => t.as_ref().shallow_repr(),
            Term::Var(id) => id.to_string(),
            Term::ParseError(_) => String::from("<parse error>"),
            Term::RuntimeError(_) => String::from("<runtime error>"),
            Term::Let(..)
            | Term::LetPattern(..)
            | Term::App(_, _)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::OpN(..)
            | Term::Import(_)
            | Term::ResolvedImport(_) => String::from("<unevaluated>"),
        }
    }

    /// Return a deep string representation of a term, used for printing in the REPL
    pub fn deep_repr(&self) -> String {
        match self {
            Term::Record(r) | Term::RecRecord(r, ..) => {
                let fields_str: Vec<String> = r
                    .fields
                    .iter()
                    .map(|(ident, field)| {
                        if let Some(ref value) = field.value {
                            format!("{} = {}", ident, value.as_ref().deep_repr())
                        } else {
                            format!("{ident}")
                        }
                    })
                    .collect();

                let suffix = match self {
                    Term::RecRecord(_, dyn_fields, ..) if !dyn_fields.is_empty() => ", ..",
                    _ => "",
                };

                format!("{{ {}{} }}", fields_str.join(", "), suffix)
            }
            Term::Array(elements, _) => {
                let elements_str: Vec<String> = elements
                    .iter()
                    .map(|term| term.as_ref().deep_repr())
                    .collect();
                format!("[ {} ]", elements_str.join(", "))
            }
            _ => self.shallow_repr(),
        }
    }

    /// Determine if a term is in evaluated from, called weak head normal form (WHNF).
    pub fn is_whnf(&self) -> bool {
        match self {
            Term::Null
            | Term::Bool(_)
            | Term::Num(_)
            | Term::Str(_)
            | Term::Fun(..)
            // match expressions are function
            | Term::Match {..}
            | Term::Lbl(_)
            | Term::Enum(_)
            | Term::Record(..)
            | Term::Array(..)
            | Term::SealingKey(_) => true,
            Term::Let(..)
            | Term::LetPattern(..)
            | Term::FunPattern(..)
            | Term::App(..)
            | Term::Var(_)
            | Term::Op1(..)
            | Term::Op2(..)
            | Term::OpN(..)
            | Term::Sealed(..)
            | Term::Annotated(..)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_)
            | Term::RecRecord(..)
            | Term::ParseError(_)
            | Term::RuntimeError(_) => false,
        }
    }

    /// Determine if a term is annotated.
    pub fn is_annotated(&self) -> bool {
        matches!(self, Term::Annotated(..))
    }

    /// Determine if a term is a constant.
    ///
    /// In this context, a constant is an atomic literal of the language: null, a boolean, a number, a
    /// string, a label, an enum tag or a symbol.
    pub fn is_constant(&self) -> bool {
        match self {
            Term::Null
            | Term::Bool(_)
            | Term::Num(_)
            | Term::Str(_)
            | Term::Lbl(_)
            | Term::Enum(_)
            | Term::SealingKey(_) => true,
            Term::Let(..)
            | Term::LetPattern(..)
            | Term::Record(..)
            | Term::Array(..)
            | Term::Fun(..)
            | Term::FunPattern(..)
            | Term::App(_, _)
            | Term::Match { .. }
            | Term::Var(_)
            | Term::Op1(..)
            | Term::Op2(..)
            | Term::OpN(..)
            | Term::Sealed(..)
            | Term::Annotated(..)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_)
            | Term::RecRecord(..)
            | Term::ParseError(_)
            | Term::RuntimeError(_) => false,
        }
    }

    /// determine if a term is atomic
    pub fn is_atom(&self) -> bool {
        match self {
            Term::Null
            | Term::Bool(..)
            | Term::Num(..)
            | Term::Str(..)
            | Term::StrChunks(..)
            | Term::Lbl(..)
            | Term::Enum(..)
            | Term::Record(..)
            | Term::RecRecord(..)
            | Term::Array(..)
            | Term::Var(..)
            | Term::SealingKey(..)
            // Those special cases aren't really atoms, but mustn't be parenthesized because they
            // are really functions taking additional non-strict arguments and printed as "partial"
            // infix operators.
            //
            // For example, `Op1(BoolOr, Var("x"))` is currently printed as `x ||`. Such operators
            // must never be parenthesized, such as in `(x ||)`.
            //
            // We might want a more robust mechanism for pretty printing such operators.
            | Term::Op1(UnaryOp::BoolAnd(), _)
            | Term::Op1(UnaryOp::BoolOr(), _) => true,
            Term::Let(..)
            | Term::Match { .. }
            | Term::LetPattern(..)
            | Term::Fun(..)
            | Term::FunPattern(..)
            | Term::App(..)
            | Term::Op1(..)
            | Term::Op2(..)
            | Term::OpN(..)
            | Term::Sealed(..)
            | Term::Annotated(..)
            | Term::Import(..)
            | Term::ResolvedImport(..)
            | Term::ParseError(_)
            | Term::RuntimeError(_) => false,
        }
    }

    /// Extract the static literal from string chunk. It only returns a `Some(..)`
    /// when the term is a `Term::StrChunk` and all the chunks are `StrChunk::Literal(..)`
    pub fn try_str_chunk_as_static_str(&self) -> Option<String> {
        match self {
            Term::StrChunks(chunks) => {
                chunks
                    .iter()
                    .fold(Some(String::new()), |acc, next| match (acc, next) {
                        (Some(mut acc), StrChunk::Literal(lit)) => {
                            acc.push_str(lit);
                            Some(acc)
                        }
                        _ => None,
                    })
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SharedTerm {
    shared: Rc<Term>,
}

impl SharedTerm {
    pub fn new(term: Term) -> Self {
        Self {
            shared: Rc::new(term),
        }
    }

    pub fn into_owned(self) -> Term {
        Rc::try_unwrap(self.shared).unwrap_or_else(|rc| Term::clone(&rc))
    }

    pub fn make_mut(this: &mut Self) -> &mut Term {
        Rc::make_mut(&mut this.shared)
    }

    pub fn ptr_eq(this: &SharedTerm, that: &SharedTerm) -> bool {
        Rc::ptr_eq(&this.shared, &that.shared)
    }
}

impl AsRef<Term> for SharedTerm {
    fn as_ref(&self) -> &Term {
        self.shared.as_ref()
    }
}

impl From<SharedTerm> for Term {
    fn from(st: SharedTerm) -> Self {
        st.into_owned()
    }
}

impl Deref for SharedTerm {
    type Target = Term;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

/// Primitive unary operators.
///
/// Some operators, such as if-then-else or `seq`, actually take several arguments but are only
/// strict in one (the tested boolean for example, in the case of if-then-else). They are encoded
/// as unary operators of this argument: indeed, in an expression `if-then-else boolean thenBlock
/// elseBlock`, `if-then-else` can be seen as a unary operator taking a `Bool` argument and
/// evaluating to either the first projection `fun x y => x` or the second projection `fun x y =>
/// y`.
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    /// If-then-else.
    Ite(),

    /// Return an enum tag representing the type of the term.
    Typeof(),

    // Boolean AND and OR operator are encoded as unary operators so that they can be lazy in their
    // second argument.
    /// Boolean AND operator.
    BoolAnd(),
    /// Boolean OR operator.
    BoolOr(),
    /// Boolean NOT operator.
    BoolNot(),

    /// Raise a blame, which stops the execution and prints an error according to the label argument.
    Blame(),

    /// Typecast an enum to a larger enum type.
    ///
    /// `Embed` is used to upcast enums. For example, if a value `x` has enum type `a | b`, then
    /// `embed c x` will have enum type `a | b | c`. It only affects typechecking as at runtime
    /// `embed someId` act like the identity.
    Embed(Ident),
    /// Evaluate a match block applied to an argument.
    Match { has_default: bool },

    /// Static access to a record field.
    ///
    /// Static means that the field identifier is a statically known string inside the source.
    StaticAccess(Ident),

    /// Map a function on each element of an array.
    ArrayMap(),
    /// Map a function on a record.
    ///
    /// The mapped function must take two arguments, the name of the field as a string, and the
    /// content of the field. `RecordMap` then replaces the content of each field by the result of the
    /// function: i.e., `recordMap f {a=2;}` evaluates to `{a=(f "a" 2);}`.
    RecordMap(),

    /// Inverse the polarity of a label.
    ChangePolarity(),

    /// Get the polarity of a label.
    Pol(),
    /// Go to the domain in the type path of a label.
    ///
    /// If the argument is a label with a [type path][crate::label::TyPath) representing some
    /// subtype of the type of the original contract, as in:
    ///
    /// ```text
    /// (Num -> Num) -> Num
    ///  ^^^^^^^^^^ type path
    /// ------------------- original type
    /// ```
    ///
    /// Then `GoDom` evaluates to a copy of this label, where the path has gone forward into the domain:
    ///
    /// ```text
    /// (Num -> Num) -> Num
    ///  ^^^ new type path
    /// ------------------- original type
    /// ```
    GoDom(),
    /// Go to the codomain in the type path of a label.
    ///
    /// See `GoDom`.
    GoCodom(),
    /// Go to the array in the type path of a label.
    ///
    /// See `GoDom`.
    GoArray(),
    /// Go to the type ascribed to every field in a dictionary.
    ///
    /// See `GoDom`.
    GoDict(),

    /// Force the evaluation of its argument and proceed with the second.
    Seq(),
    /// Recursively force the evaluation of its first argument then returns the second.
    ///
    /// Recursive here means that the evaluation does not stop at a WHNF, but the content of arrays
    /// and records is also recursively forced.
    ///
    /// The parameter is a fix used for error reporting of missing field definitions ocurring
    /// during the deep sequencing of a record. This is temporary and should be stored somewhere
    /// else ideally (like on the stack).
    DeepSeq(Option<crate::eval::callstack::StackElem>),

    /// Return the head of an array.
    ArrayHead(),
    /// Return the tail of an array.
    ArrayTail(),
    /// Return the length of an array.
    ArrayLength(),
    /// Generate an array of a given length by mapping a `Num -> Num` function onto `[1,..,n]`.
    ArrayGen(),

    /// Generated by the evaluation of a string with interpolated expressions. `ChunksConcat`
    /// applied to the current chunk to evaluate. As additional state, it uses a string
    /// accumulator, the indentation of the chunk being evaluated, and the remaining chunks to be
    /// evaluated, all stored on the stack.
    ChunksConcat(),

    /// Return the names of the fields of a record as a string array.
    FieldsOf(),
    /// Return the values of the fields of a record as an array.
    ValuesOf(),

    /// Remove heading and trailing spaces from a string.
    StrTrim(),
    /// Return the array of characters of a string.
    StrChars(),
    /// Return the code of a character (givne as a string of length 1).
    CharCode(),
    /// Return the character corresponding to a code.
    CharFromCode(),
    /// Transform a string to uppercase.
    StrUppercase(),
    /// Transform a string to lowercase.
    StrLowercase(),
    /// Return the length of a string.
    StrLength(),
    /// Transform a data to a string.
    ToStr(),
    /// Transform a string to a number.
    NumFromStr(),
    /// Transform a string to an enum.
    EnumFromStr(),
    /// Test if a regex matches a string.
    /// Like [`UnaryOp::StrFind`], this is a unary operator because we would like a way to share the
    /// same "compiled regex" for many matching calls. This is done by returning functions
    /// wrapping [`UnaryOp::StrIsMatchCompiled`] and [`UnaryOp::StrFindCompiled`]
    StrIsMatch(),
    /// Match a regex on a string, and returns the captured groups together, the index of the
    /// match, etc.
    StrFind(),
    /// Version of [`UnaryOp::StrIsMatch`] which remembers the compiled regex.
    StrIsMatchCompiled(CompiledRegex),
    /// Version of [`UnaryOp::StrFind`] which remembers the compiled regex.
    StrFindCompiled(CompiledRegex),
    /// Force full evaluation of a term and return it.
    ///
    /// This was added in the context of [`BinaryOp::ArrayLazyAssume`],
    /// in particular to make serialization work with lazy array contracts.
    ///
    /// # `Force` vs. `DeepSeq`
    ///
    /// [`UnaryOp::Force`] updates at the indices containing arrays with a new version where the lazy contracts have all been applied,
    /// whereas [`UnaryOp::DeepSeq`] evaluates the same expressions, but it never updates at the index of an array with lazy contracts
    /// with an array where those contracts have been applied. In a way, the result of lazy contract application in arrays is "lost"
    /// in [`UnaryOp::DeepSeq`], while it's returned in [`UnaryOp::Force`].
    ///
    /// This means we can observe different results between `deep_seq x x` and `force x`, in some cases.
    ///
    /// It's also worth noting that [`UnaryOp::DeepSeq`] should be, in principle, more efficient that [`UnaryOp::Force`]
    /// as it does less cloning.
    Force(Option<crate::eval::callstack::StackElem>),
    /// Recursive default priority operator. Recursively propagates a default priority through a
    /// record, stopping whenever a field isn't a record anymore to then turn into a simple
    /// `default`.
    ///
    /// For example:
    ///
    /// ```nickel
    /// {
    ///   foo | rec default = {
    ///     bar = 1,
    ///     baz = "a",
    ///   }
    /// }
    /// ```
    ///
    /// Is evaluated to:
    ///
    /// ```nickel
    /// {
    ///   foo = {
    ///     bar | default = 1,
    ///     baz | default = "a",
    ///   }
    /// }
    /// ```
    ///
    /// If a value has any explicit priority annotation, then the original annotation takes
    /// precedence and the default doesn't apply.
    RecDefault(),
    /// Recursive force priority operator. Similar to [UnaryOp::RecDefault], but propagate the
    /// `force` annotation.
    ///
    /// As opposed to `RecDefault`, the `force` takes precedence and erase any prior explicit
    /// priority annotation.
    RecForce(),

    /// Creates an "empty" record with the sealed tail of its [`Term::Record`]
    /// argument.
    ///
    /// Used in the `$record` contract implementation to ensure that we can
    /// define a `field_diff` function that preserves the sealed polymorphic
    /// tail of its argument.
    RecordEmptyWithTail(),

    /// Print a message when encountered during evaluation and proceed with the evaluation of the argument
    /// on the top of the stack. Operationally the same as the identity
    /// function
    Trace(),
}

// See: https://github.com/rust-lang/regex/issues/178
/// [`regex::Regex`] which implements [`PartialEq`].
#[derive(Debug, Clone)]
pub struct CompiledRegex(regex::Regex);

impl PartialEq for CompiledRegex {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_str() == other.0.as_str()
    }
}

impl Deref for CompiledRegex {
    type Target = regex::Regex;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<regex::Regex> for CompiledRegex {
    fn from(item: regex::Regex) -> Self {
        CompiledRegex(item)
    }
}

/// Position of a unary operator
pub enum OpPos {
    Infix,
    Postfix,
    Prefix,
    /// A special operator like `if ... then ... else ...`
    Special,
}

impl UnaryOp {
    pub fn pos(&self) -> OpPos {
        use UnaryOp::*;
        match self {
            BoolAnd() | BoolOr() | StaticAccess(_) => OpPos::Postfix,
            Ite() => OpPos::Special,
            _ => OpPos::Prefix,
        }
    }
}

/// The kind of a dynamic record extension. Kind indicates if a definition is expected for the
/// field being inserted, or if the inserted field doesn't have a definition.
#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub enum RecordExtKind {
    WithValue,
    WithoutValue,
}

/// Primitive binary operators
#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    /// Addition of numerals.
    Plus(),
    /// Subtraction of numerals.
    Sub(),
    /// Multiplication of numerals.
    Mult(),
    /// Floating-point division of numerals.
    Div(),
    /// Modulo of numerals.
    Modulo(),
    /// Raise a number to a power.
    Pow(),
    /// Concatenation of strings.
    StrConcat(),
    /// Polymorphic equality.
    Eq(),
    /// Strictly less than comparison operator.
    LessThan(),
    /// Less than or equal comparison operator.
    LessOrEq(),
    /// Strictly greater than comparison operator.
    GreaterThan(),
    /// Greater than or equal comparison operator.
    GreaterOrEq(),
    /// An assume.
    ///
    /// Apply a contract to a label and a value. The value is is stored on the stack unevaluated,
    /// while the contract and the label are the strict arguments to this operator. Assume also
    /// accepts contracts as records, that are translates to a function that performs a merge
    /// operation with its argument. Finally, this operator marks the location of the contract
    /// argument for better error reporting.
    Assume(),
    /// Unseal a sealed term.
    ///
    /// See [`BinaryOp::Seal`].
    Unseal(),
    /// Go to a specific field in the type path of a label.
    ///
    /// See `GoDom`.
    GoField(),
    /// Set the tag text of a blame label.
    Tag(),
    /// Extend a record with a dynamic field.
    ///
    /// Dynamic means that the field name may be an expression instead of a statically known
    /// string. `DynExtend` tries to evaluate this name to a string, and in case of success, add a
    /// field with this name to the given record with the expression on top of the stack as
    /// content.
    ///
    /// The field may have been defined with attached metadata, pending contracts and may or may
    /// not have a defined value. We can't store those information as a term argument (metadata
    /// aren't first class values, at least at the time of writing), so for now we attach it
    /// directly to the extend primop. This isn't ideal, and in the future we may want to have a
    /// more principled primop.
    DynExtend {
        metadata: FieldMetadata,
        pending_contracts: Vec<PendingContract>,
        ext_kind: RecordExtKind,
    },
    /// Remove a field from a record. The field name is given as an arbitrary Nickel expression.
    DynRemove(),
    /// Access the field of record. The field name is given as an arbitrary Nickel expression.
    DynAccess(),
    /// Test if a record has a specific field.
    HasField(),
    /// Concatenate two arrays.
    ArrayConcat(),
    /// Access the n-th element of an array.
    ArrayElemAt(),
    /// The merge operator (see [crate::eval::merge]).
    Merge(),

    /// Hash a string.
    Hash(),
    /// Serialize a value to a string.
    Serialize(),
    /// Deserialize a string to a value.
    Deserialize(),

    /// Split a string into an array.
    StrSplit(),
    /// Determine if a string is a substring of another one.
    StrContains(),
    /// Seal a term with a sealing key (see [`Term::Sealed`]).
    Seal(),

    /// Lazily apply a contract to an Array.
    /// This simply inserts a contract into the array attributes.
    ArrayLazyAssume(),

    /// Apply a dictionary contract to a record. The arguments are a label
    /// and the contract contained in the dictionary type. Results in a function from
    /// records to records that applies the dictionary contract to its argument.
    ///
    /// When used with a dictionary contract `{_: C}` and a record `R`,
    /// synthesizes a record contract whose fields are the fields of `R` with the
    /// contract `C` attached to each of them. Then uses `NAryOp::MergeContract` to
    /// apply this record contract to `R`.
    DictionaryAssume(),
}

impl BinaryOp {
    pub fn pos(&self) -> OpPos {
        use BinaryOp::*;
        match self {
            Plus()
            | Sub()
            | Mult()
            | Div()
            | Modulo()
            | Pow()
            | StrConcat()
            | Eq()
            | LessThan()
            | LessOrEq()
            | GreaterThan()
            | GreaterOrEq()
            | DynExtend { .. }
            | DynRemove()
            | DynAccess()
            | ArrayConcat()
            | Merge() => OpPos::Infix,
            _ => OpPos::Prefix,
        }
    }
}

/// Primitive n-ary operators. Unary and binary operator make up for most of operators and are
/// hence special cased. `NAryOp` handles strict operations of arity greater than 2.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NAryOp {
    /// Replace a substring by another one in a string.
    StrReplace(),
    /// Same as [`NAryOp::StrReplace`], but the pattern is interpreted as a regular expression.
    StrReplaceRegex(),
    /// Return a substring of an original string.
    StrSubstr(),
    /// The merge operator in contract mode (see [crate::eval::merge]). The arguments are in order
    /// the contract's label, the value to check, and the contract as a record.
    MergeContract(),
    /// Seals one record into the tail of another. Used to ensure that functions
    /// using polymorphic record contracts do not violate parametricity.
    ///
    /// Takes four arguments:
    ///   - a [sealing key](Term::SealingKey), which must be provided later to unseal the tail,
    ///   - a [label](Term::Lbl), which will be used to assign blame correctly tail access is attempted,
    ///   - a [record](Term::Record), which is the record we wish to seal the tail into,
    ///   - the [record](Term::Record) that we wish to seal.
    RecordSealTail(),
    /// Unseals a term from the tail of a record and returns it.
    ///
    /// Takes three arguments:
    ///   - the [sealing key](Term::SealingKey), which was used to seal the tail,
    ///   - a [label](Term::Lbl) which will be used to assign blame correctly if
    ///     something goes wrong while unsealing,
    ///   - the [record](Term::Record) whose tail we wish to unseal.
    RecordUnsealTail(),
}

impl NAryOp {
    pub fn arity(&self) -> usize {
        match self {
            NAryOp::StrReplace()
            | NAryOp::StrReplaceRegex()
            | NAryOp::StrSubstr()
            | NAryOp::MergeContract()
            | NAryOp::RecordUnsealTail() => 3,
            NAryOp::RecordSealTail() => 4,
        }
    }
}

impl fmt::Display for NAryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NAryOp::StrReplace() => write!(f, "strReplace"),
            NAryOp::StrReplaceRegex() => write!(f, "strReplaceRegex"),
            NAryOp::StrSubstr() => write!(f, "substring"),
            NAryOp::MergeContract() => write!(f, "mergeContract"),
            NAryOp::RecordSealTail() => write!(f, "%record_seal_tail%"),
            NAryOp::RecordUnsealTail() => write!(f, "%record_unseal_tail%"),
        }
    }
}

#[derive(Copy, Clone)]
pub enum TraverseOrder {
    TopDown,
    BottomUp,
}

/// Wrap [Term] with positional information.
#[derive(Debug, PartialEq, Clone)]
pub struct RichTerm {
    pub term: SharedTerm,
    pub pos: TermPos,
}

impl RichTerm {
    /// Create a new value from a term and an optional position.
    pub fn new(t: Term, pos: TermPos) -> Self {
        RichTerm {
            term: SharedTerm::new(t),
            pos,
        }
    }

    /// Erase recursively the positional information.
    ///
    /// It allows to use rust `Eq` trait to compare the values of the underlying terms.
    //#[cfg(test)]
    pub fn without_pos(mut self) -> Self {
        fn clean_pos(rt: &mut RichTerm) {
            rt.pos = TermPos::None;
            SharedTerm::make_mut(&mut rt.term).apply_to_rich_terms(clean_pos);
        }

        clean_pos(&mut self);
        self
    }

    /// Set the position and return the term updated.
    pub fn with_pos(mut self, pos: TermPos) -> Self {
        self.pos = pos;
        self
    }

    /// Pretty print a term capped to a given max length (in characters). Useful to limit the size
    /// of terms reported e.g. in typechecking errors. If the output of pretty printing is greater
    /// than the bound, the string is truncated to `max_width` and the last character after
    /// truncate is replaced by the ellipsis unicode character U+2026.
    pub fn pretty_print_cap(&self, max_width: usize) -> String {
        let output = format!("{self}");

        if output.len() <= max_width {
            output
        } else {
            let (end, _) = output.char_indices().nth(max_width).unwrap();
            let mut truncated = String::from(&output[..end]);

            if max_width >= 2 {
                truncated.pop();
                truncated.push('\u{2026}');
            }

            truncated
        }
    }
}

pub trait Traverse<T>: Sized {
    /// Apply a transformation on a object containing syntactic elements of type `T` (terms, types,
    /// etc.) by mapping a faillible function `f` on each such node as prescribed by the order.
    ///
    /// `f` may return a generic error `E` and use the state `S` which is passed around.
    fn traverse<F, S, E>(self, f: &F, state: &mut S, order: TraverseOrder) -> Result<Self, E>
    where
        F: Fn(T, &mut S) -> Result<T, E>;
}

impl Traverse<RichTerm> for RichTerm {
    fn traverse<F, S, E>(self, f: &F, state: &mut S, order: TraverseOrder) -> Result<RichTerm, E>
    where
        F: Fn(RichTerm, &mut S) -> Result<RichTerm, E>,
    {
        let rt = match order {
            TraverseOrder::TopDown => f(self, state)?,
            TraverseOrder::BottomUp => self,
        };
        let pos = rt.pos;

        let result = match_sharedterm! {rt.term, with {
            Term::Fun(id, t) => {
                let t = t.traverse(f, state, order)?;
                RichTerm::new(
                    Term::Fun(id, t),
                    pos,
                )
            },
            Term::FunPattern(id, d, t) => {
                let t = t.traverse(f, state, order)?;
                RichTerm::new(
                    Term::FunPattern(id, d, t),
                    pos,
                )
            },
            Term::Let(id, t1, t2, attrs) => {
                let t1 = t1.traverse(f, state, order)?;
                let t2 = t2.traverse(f, state, order)?;
                RichTerm::new(
                    Term::Let(id, t1, t2, attrs),
                    pos,
                )
            },
            Term::LetPattern(id, pat, t1, t2) => {
                let t1 = t1.traverse(f, state, order)?;
                let t2 = t2.traverse(f, state, order)?;
                RichTerm::new(
                    Term::LetPattern(id, pat, t1, t2),
                    pos,
                )
            },
            Term::App(t1, t2) => {
                let t1 = t1.traverse(f, state, order)?;
                let t2 = t2.traverse(f, state, order)?;
                RichTerm::new(
                    Term::App(t1, t2),
                    pos,
                )
            },
            Term::Match { cases, default } => {
                // The annotation on `map_res` use Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let cases_result : Result<HashMap<Ident, RichTerm>, E> = cases
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,RichTerm), E>
                    .map(|(id, t)| t.traverse(f, state, order).map(|t_ok| (id, t_ok)))
                    .collect();

                let default = default.map(|t| t.traverse(f, state, order)).transpose()?;

                RichTerm::new(
                    Term::Match {cases: cases_result?, default },
                    pos,
                )
            },
            Term::Op1(op, t) => {
                let t = t.traverse(f, state, order)?;
                RichTerm::new(
                    Term::Op1(op, t),
                    pos,
                )
            },
            Term::Op2(op, t1, t2) => {
                let t1 = t1.traverse(f, state, order)?;
                let t2 = t2.traverse(f, state, order)?;
                RichTerm::new(Term::Op2(op, t1, t2),
                    pos,
                )
            },
            Term::OpN(op, ts) => {
                let ts_res: Result<Vec<RichTerm>, E> = ts
                    .into_iter()
                    .map(|t| t.traverse(f, state, order))
                    .collect();
                RichTerm::new(
                    Term::OpN(op, ts_res?),
                    pos,
                )
            },
            Term::Sealed(i, t1, lbl) => {
                let t1 = t1.traverse(f, state, order)?;
                RichTerm::new(
                    Term::Sealed(i, t1, lbl),
                    pos,
                )
            },
            Term::Record(record) => {
                // The annotation on `fields_res` uses Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let fields_res: Result<HashMap<Ident, Field>, E> = record.fields
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,RichTerm), E>
                    .map(|(id, field)| {
                        let field = field.traverse(f, state, order)?;
                        Ok((id, field))
                    })
                    .collect();
                RichTerm::new(Term::Record(RecordData::new(fields_res?, record.attrs, record.sealed_tail)), pos)
            },
            Term::RecRecord(record, dyn_fields, deps) => {
                // The annotation on `map_res` uses Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let static_fields_res: Result<HashMap<Ident, Field>, E> = record.fields
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,Field), E>
                    .map(|(id, field)| {
                        let field = field.traverse(f, state, order)?;
                        Ok((id, field))
                    })
                    .collect();
                let dyn_fields_res: Result<Vec<(RichTerm, Field)>, E> = dyn_fields
                    .into_iter()
                    .map(|(id_t, field)| {
                        let id_t = id_t.traverse(f, state, order)?;
                        let field = field.traverse(f, state, order)?;

                        Ok((id_t, field,))
                    })
                    .collect();
                RichTerm::new(
                    Term::RecRecord(RecordData::new(static_fields_res?, record.attrs, record.sealed_tail), dyn_fields_res?, deps),
                    pos,
                )
            },
            Term::Array(ts, attrs) => {
                let ts_res = Array::new(ts
                    .into_iter()
                    .map(|t| t.traverse(f, state, order))
                    .collect::<Result<Rc<[_]>, _>>()?);

                RichTerm::new(
                    Term::Array(ts_res, attrs),
                    pos,
                )
            },
            Term::StrChunks(chunks) => {
                let chunks_res: Result<Vec<StrChunk<RichTerm>>, E> = chunks
                    .into_iter()
                    .map(|chunk| match chunk {
                        chunk @ StrChunk::Literal(_) => Ok(chunk),
                        StrChunk::Expr(t, indent) => {
                            Ok(StrChunk::Expr(t.traverse(f, state, order)?, indent))
                        }
                    })
                    .collect();

                RichTerm::new(
                    Term::StrChunks(chunks_res?),
                    pos,
                )
            },
            Term::Annotated(annot, term) => {
                let annot = annot.traverse(f, state, order)?;
                let term = term.traverse(f, state, order)?;
                RichTerm::new(
                    Term::Annotated(annot, term),
                    pos,
                )
            }
        } else rt};

        match order {
            TraverseOrder::TopDown => Ok(result),
            TraverseOrder::BottomUp => f(result, state),
        }
    }
}

impl From<RichTerm> for Term {
    fn from(rt: RichTerm) -> Self {
        rt.term.into_owned()
    }
}

impl AsRef<Term> for RichTerm {
    fn as_ref(&self) -> &Term {
        &self.term
    }
}

impl From<Term> for RichTerm {
    fn from(t: Term) -> Self {
        RichTerm {
            term: SharedTerm::new(t),
            pos: TermPos::None,
        }
    }
}

impl std::fmt::Display for RichTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use crate::pretty::*;
        use pretty::BoxAllocator;

        let allocator = BoxAllocator;

        let doc: DocBuilder<_, ()> = self.clone().pretty(&allocator);
        doc.render_fmt(80, f)
    }
}

/// Allows to match on SharedTerm without taking ownership of the matched part until the match.
/// In the `else` clause, we haven't taken ownership yet, so we can still use the richterm at that point.
///
/// It is used somehow as a match statement, going from
/// ```
/// # use nickel_lang::term::{RichTerm, Term};
/// let rt = RichTerm::from(Term::Num(5.0));
///
/// match rt.term.into_owned() {
///     Term::Num(x) => x as usize,
///     Term::Str(s) => s.len(),
///     _ => 42,
/// };
/// ```
/// to
/// ```
/// # use nickel_lang::term::{RichTerm, Term};
/// # use nickel_lang::match_sharedterm;
/// let rt = RichTerm::from(Term::Num(5.0));
///
/// match_sharedterm!{rt.term, with {
///         Term::Num(x) => x as usize,
///         Term::Str(s) => s.len(),
///     } else 42
/// };
/// ```
///
/// Known limitation: cannot use a `mut` inside the patterns.
#[macro_export]
macro_rules! match_sharedterm {
    (
        $st: expr, with {
            $($($pat: pat_param)|+ $(if $if_expr: expr)? => $expr: expr),+ $(,)?
        } else $else_clause: expr
    ) => {
        match $st.as_ref() {
            $(
                #[allow(unused_variables, unreachable_patterns, unused_mut)]
                $($pat)|+ $(if $if_expr)? =>
                    match Term::from($st) {
                        $($pat)|+ => $expr,
                        _ => unsafe {::core::hint::unreachable_unchecked()}
                    },
            )+
            _ => $else_clause
        }
    };
}

#[macro_use]
/// Helpers to build `RichTerm` objects.
pub mod make {
    use super::*;

    /// Multi-ary application for types implementing `Into<RichTerm>`.
    #[macro_export]
    macro_rules! mk_app {
        ( $f:expr, $arg:expr) => {
            $crate::term::RichTerm::from($crate::term::Term::App($crate::term::RichTerm::from($f), $crate::term::RichTerm::from($arg)))
        };
        ( $f:expr, $fst:expr , $( $args:expr ),+ ) => {
            mk_app!(mk_app!($f, $fst), $( $args ),+)
        };
    }

    /// Multi-ary application for types implementing `Into<RichTerm>`.
    #[macro_export]
    macro_rules! mk_opn {
        ( $op:expr, $( $args:expr ),+) => {
            {
                let args = vec![$( RichTerm::from($args) ),+];
                $crate::term::RichTerm::from($crate::term::Term::OpN($op, args))
            }
        };
    }

    /// Multi argument function for types implementing `Into<Ident>` (for the identifiers), and
    /// `Into<RichTerm>` for the body.
    #[macro_export]
    macro_rules! mk_fun {
        ( $id:expr, $body:expr ) => {
            $crate::term::RichTerm::from($crate::term::Term::Fun($crate::identifier::Ident::from($id), $crate::term::RichTerm::from($body)))
        };
        ( $id1:expr, $id2:expr , $( $rest:expr ),+ ) => {
            mk_fun!($crate::identifier::Ident::from($id1), mk_fun!($id2, $( $rest ),+))
        };
    }

    /// Multi field record for types implementing `Into<Ident>` (for the identifiers), and
    /// `Into<RichTerm>` for the fields. Identifiers and corresponding content are specified as a
    /// tuple: `mk_record!(("field1", t1), ("field2", t2))` corresponds to the record `{ field1 =
    /// t1; field2 = t2 }`.
    #[macro_export]
    macro_rules! mk_record {
        ( $( ($id:expr, $body:expr) ),* ) => {
            {
                let mut fields = std::collections::HashMap::new();
                $(
                    fields.insert($id.into(), $body.into());
                )*
                $crate::term::RichTerm::from($crate::term::Term::Record($crate::term::record::RecordData::with_field_values(fields)))
            }
        };
    }

    /// Switch for types implementing `Into<Ident>` (for patterns) and `Into<RichTerm>` for the
    /// body of each case. Cases are specified as tuple, and the default case (optional) is separated by a `;`:
    /// `mk_switch!(format, ("Json", json_case), ("Yaml", yaml_case) ; def)` corresponds to
    /// ``switch { `Json => json_case, `Yaml => yaml_case, _ => def} format``.
    #[macro_export]
    macro_rules! mk_match {
        ( $( ($id:expr, $body:expr) ),* ; $default:expr ) => {
            {
                let mut cases = std::collections::HashMap::new();
                $(
                    cases.insert($id.into(), $body.into());
                )*
                $crate::term::RichTerm::from($crate::term::Term::Match {cases, default: Some($crate::term::RichTerm::from($default)) })
            }
        };
        ( $( ($id:expr, $body:expr) ),*) => {
                let mut cases = std::collections::HashMap::new();
                $(
                    cases.insert($id.into(), $body.into());
                )*
                $crate::term::RichTerm::from($crate::term::Term::Match {cases, default: None})
        };
    }

    /// Array for types implementing `Into<RichTerm>` (for elements).
    /// The array's attributes are a trailing (optional) `ArrayAttrs`, separated by a `;`.
    /// `mk_array!(Term::Num(42))` corresponds to `\[42\]`. Here the attributes are `ArrayAttrs::default()`, though the evaluated array may have different attributes.
    #[macro_export]
    macro_rules! mk_array {
        ( $( $terms:expr ),* ; $attrs:expr ) => {
            {
                let ts = $crate::term::array::Array::new(std::rc::Rc::new([$( $crate::term::RichTerm::from($terms) ),*]));
                $crate::term::RichTerm::from($crate::term::Term::Array(ts, $attrs))
            }
        };
        ( $( $terms:expr ),* ) => {
            {
                let ts = $crate::term::array::Array::new(std::rc::Rc::new([$( $crate::term::RichTerm::from($terms) ),*]));
                $crate::term::RichTerm::from(Term::Array(ts, ArrayAttrs::default()))
            }
        };
    }

    pub fn var<I>(v: I) -> RichTerm
    where
        I: Into<Ident>,
    {
        Term::Var(v.into()).into()
    }

    fn let_in_<I, T1, T2>(rec: bool, id: I, t1: T1, t2: T2) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
        I: Into<Ident>,
    {
        let attrs = LetAttrs {
            binding_type: BindingType::Normal,
            rec,
        };
        Term::Let(id.into(), t1.into(), t2.into(), attrs).into()
    }

    pub fn let_in<I, T1, T2>(id: I, t1: T1, t2: T2) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
        I: Into<Ident>,
    {
        let_in_(false, id, t1, t2)
    }

    pub fn let_rec_in<I, T1, T2>(id: I, t1: T1, t2: T2) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
        I: Into<Ident>,
    {
        let_in_(true, id, t1, t2)
    }

    pub fn let_pat<I, D, T1, T2>(id: Option<I>, pat: D, t1: T1, t2: T2) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
        D: Into<RecordPattern>,
        I: Into<Ident>,
    {
        Term::LetPattern(id.map(|i| i.into()), pat.into(), t1.into(), t2.into()).into()
    }

    pub fn if_then_else<T1, T2, T3>(cond: T1, t1: T2, t2: T3) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
        T3: Into<RichTerm>,
    {
        mk_app!(Term::Op1(UnaryOp::Ite(), cond.into()), t1.into(), t2.into())
    }

    pub fn op1<T>(op: UnaryOp, t: T) -> RichTerm
    where
        T: Into<RichTerm>,
    {
        Term::Op1(op, t.into()).into()
    }

    pub fn op2<T1, T2>(op: BinaryOp, t1: T1, t2: T2) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
    {
        Term::Op2(op, t1.into(), t2.into()).into()
    }

    pub fn opn<T>(op: NAryOp, args: Vec<T>) -> RichTerm
    where
        T: Into<RichTerm>,
    {
        Term::OpN(op, args.into_iter().map(T::into).collect()).into()
    }

    pub fn assume<T>(types: Types, l: Label, t: T) -> Result<RichTerm, UnboundTypeVariableError>
    where
        T: Into<RichTerm>,
    {
        Ok(mk_app!(
            op2(BinaryOp::Assume(), types.contract()?, Term::Lbl(l)),
            t.into()
        ))
    }

    pub fn string<S>(s: S) -> RichTerm
    where
        S: Into<String>,
    {
        Term::Str(s.into()).into()
    }

    pub fn id() -> RichTerm {
        mk_fun!("x", var("x"))
    }

    pub fn import<S>(path: S) -> RichTerm
    where
        S: Into<OsString>,
    {
        Term::Import(path.into()).into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Regression test for issue [#548](https://github.com/tweag/nickel/issues/548)
    #[test]
    fn annot_flatten() {
        use crate::parser::utils::Annot;

        let inner = TypeAnnotation {
            types: Some(LabeledType {
                types: Types::from(TypeF::Num),
                label: Label::dummy(),
            }),
            ..Default::default()
        };
        let outer = TypeAnnotation::default();
        let res = TypeAnnotation::combine(outer, inner);
        assert_ne!(res.types, None);
    }
}
