//! AST of a Nickel expression. [Term] used to be the main and single representation across the
//! whole Nickel pipeline, but is now used only as a runtime representation, and will be phased out
//! progressively as the implementation of the bytecode virtual machine (RFC007) progresses.
pub mod pattern;
pub mod record;
pub mod string;

use pattern::Pattern;
use record::{Field, FieldDeps, Include, RecordData, RecordDeps, SharedMetadata};
use smallvec::SmallVec;
use string::NickelString;

use crate::{
    cache::InputFormat,
    combine::Combine,
    error::{EvalErrorKind, ParseError},
    eval::{Environment, contract_eq, value::NickelValue},
    files::FileId,
    identifier::{Ident, LocIdent},
    impl_display_from_pretty,
    label::{Label, MergeLabel},
    position::{PosIdx, PosTable, RawSpan},
    pretty::PrettyPrintCap,
    traverse::*,
    typ::{Type, UnboundTypeVariableError},
};

pub use crate::ast::{MergePriority, RecordOpKind, StringChunk as StrChunk};

use crate::metrics::increment;

pub use malachite::{
    Integer,
    base::{
        num::{
            basic::traits::Zero,
            conversion::traits::{IsInteger, RoundingFrom, ToSci},
        },
        rounding_modes::RoundingMode,
    },
    rational::Rational,
};

pub use crate::ast::Number;

use serde::{Serialize, Serializer};

// Because we use `IndexMap` for records, consumer of Nickel (as a library) might have to
// manipulate values of this type, so we re-export it.
pub use indexmap::IndexMap;

use std::{ffi::OsString, fmt, ops::Deref, rc::Rc};

/// The payload of a `Term::ForeignId`.
pub type ForeignIdPayload = u64;

#[derive(Clone, Debug, PartialEq)]
pub struct FunPatternData {
    pub pattern: Pattern,
    pub body: NickelValue,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunData {
    pub arg: LocIdent,
    pub body: NickelValue,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetData {
    pub bindings: SmallVec<[(LocIdent, NickelValue); 4]>,
    pub body: NickelValue,
    pub attrs: LetAttrs,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetPatternData {
    pub bindings: SmallVec<[(Pattern, NickelValue); 1]>,
    pub body: NickelValue,
    pub attrs: LetAttrs,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecRecordData {
    pub record: RecordData,
    /// Fields defined through `include` expressions.
    pub includes: Vec<Include>,
    /// Dynamic fields, whose name is defined by interpolation.
    pub dyn_fields: Vec<(NickelValue, Field)>,
    /// Dependency tracking between fields. It is filled by the free var transformation, and is
    /// `None` before.
    pub deps: Option<RecordDeps>,
    /// If the recursive record has been closurized already (currently this is mostly the case for
    /// records coming out from merging.
    pub closurized: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Op1Data {
    pub op: UnaryOp,
    pub arg: NickelValue,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Op2Data {
    pub op: BinaryOp,
    pub arg1: NickelValue,
    pub arg2: NickelValue,
}

#[derive(Clone, Debug, PartialEq)]
pub struct OpNData {
    pub op: NAryOp,
    pub args: Vec<NickelValue>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SealedData {
    pub key: SealingKey,
    pub inner: NickelValue,
    pub label: Rc<Label>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AnnotatedData {
    pub annot: Rc<TypeAnnotation>,
    pub inner: NickelValue,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AppData {
    pub head: NickelValue,
    pub arg: NickelValue,
}

/// The runtime representation of a Nickel computation.
///
/// # History
///
/// [Term] used to be the single representation from parsing to execution, but a new, more compact
/// and closer-to-source AST has been introduced in [nickel_lang_parser::ast] and is now used for the
/// front-end (parser, typechecker, and LSP).
///
/// [Term] remains as a temporary runtime representation until the bytecode virtual machine and
/// compiler are fully implemented (see
/// [RFC007](https://github.com/tweag/nickel/blob/master/rfcs/007-bytecode-interpreter.md)). [Term]
/// is a hybrid representation where values (weak head normal forms) use the the compact
/// representation described in RFC007. The remaining constructors of [Term] are the equivalent of
/// "code" in the future VM, that is, computations.
///
/// # Memory representation
///
/// We try to strike a balance between the memory consumption of an expression, and runtime: having
/// more data inline and less indirection can make evaluation faster, but it consumes more memory
/// (and is detrimental past some threshold). In practice, we try to keep the size [Self] to fit in
/// a modern cache line (smaller or equal to 64 bytes). Some data are boxed, other are reference
/// counted: this reflects their usage. Typically, patterns in let or function are only seen during
/// program transformation currently as they are then compiled away; there shouldn't be any sharing
/// here, so it doesn't make sense to use [std::rc::Rc]. Other values might be cloned a lot on the
/// other hand, so if they aren't expected to be modified much, they are put behind `Rc`.
#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    /// A string containing interpolated expressions, represented as a list of either literals or
    /// expressions yet to be concatenated.
    ///
    /// /|\ CHUNKS ARE STORED IN REVERSE ORDER. As they will be only popped one by one from the
    /// head of the list during evaluation, doing so on a `Vec` is costly, and using a more complex
    /// data structure is not really necessary, as once created, no other than popping is ever
    /// done.  In consequence, we just reverse the vector at parsing time, so that we can then pop
    /// efficiently from the back of it.
    StrChunks(Vec<StrChunk<NickelValue>>),

    /// A function. Grabs the value of its parameter on the stack, puts it in the environment and
    /// proceeds with the valuation of the body.
    Fun(FunData),

    /// A destructuring function.
    FunPattern(Box<FunPatternData>),

    /// A let binding. Adds the binding to the environment and proceeds with the evaluation of the
    /// body.
    Let(Box<LetData>),
    /// A destructuring let-binding.
    LetPattern(Box<LetPatternData>),

    /// An application. Push the argument on the stack and proceed with the evaluation of the head.
    App(AppData),

    /// A variable. Fetch the corresponding value from the environment.
    Var(LocIdent),

    /// A recursive record, where the fields can reference each others. Computes the fixpoint and
    /// produces a record value with the proper recursive environment.
    RecRecord(Box<RecRecordData>),

    /// A container value (array or record) that has yet to be closurized. This will closurize each
    /// elements of the container in the current environment.
    Closurize(NickelValue),

    /// A match expression. Corresponds only to the case branches: this expression is still to be
    /// applied to an argument to match on.
    Match(MatchData),

    /// A primitive unary operator.
    Op1(Op1Data),

    /// A primitive binary operator.
    Op2(Op2Data),

    /// An primitive n-ary operator.
    OpN(OpNData),

    /// A sealed term.
    ///
    /// Sealed terms are introduced by contracts on polymorphic types. Take the following example:
    ///
    /// ```text
    /// let f | forall a b. a -> b -> a = fun x y => y in
    /// f true "a"
    /// ```
    ///
    /// This function is ill-typed. To check that, a polymorphic contract will:
    ///
    /// - Assign a unique identifier to each type variable: say `a => 1`, `b => 2`
    /// - For each cast on a negative occurrence of a type variable `a` or `b` (corresponding to an
    ///   argument position), tag the argument with the associated identifier. In our example, `f
    ///   true "a"` will push `Sealed(1, true)` then `Sealed(2, "a")` on the stack.
    /// - For each cast on a positive occurrence of a type variable, this contract check that the
    ///   term is of the form `Sealed(id, term)` where `id` corresponds to the identifier of the
    ///   type variable. In our example, the last cast to `a` finds `Sealed(2, "a")`, while it
    ///   expected `Sealed(1, _)`, hence it raises a positive blame.
    Sealed(SealedData),

    /// A term with a type and/or contract annotation.
    Annotated(AnnotatedData),

    /// An unresolved import.
    Import(Import),

    /// A resolved import (which has already been loaded and parsed).
    ResolvedImport(FileId),

    /// A term that couldn't be parsed properly. Used by the LSP to handle partially valid
    /// programs.
    ParseError(Box<ParseError>),

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
    ///   bar | Number,
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
    /// behavior of `RuntimeError`.
    RuntimeError(Box<EvalErrorKind>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// Specifies where something should be imported from.
pub enum Import {
    Path {
        path: OsString,
        format: InputFormat,
    },
    /// Importing packges requires a [`crate::package::PackageMap`] to translate the location
    /// to a path. The format is always Nickel.
    Package {
        id: Ident,
    },
}

/// A unique sealing key, introduced by polymorphic contracts.
pub type SealingKey = i32;

/// Type of let-binding. This only affects run-time behavior. Revertible bindings introduce
/// revertible cache elements at evaluation, which are devices used for the implementation of
/// recursive records merging. See the [`crate::eval::merge`] and [`crate::eval`] modules for more
/// details.
#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub enum BindingType {
    #[default]
    Normal,

    /// In the revertible case, we also store an optional set of dependencies. See
    /// [`crate::transform::free_vars`] for more details.
    Revertible(FieldDeps),
}

/// A runtime representation of a contract, as a term and a label ready to be applied via
/// [BinaryOp::ContractApply].
#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeContract {
    /// The pending contract, which can be a function, a type, a custom contract or a record.
    pub contract: NickelValue,
    /// The blame label.
    pub label: Label,
}

impl RuntimeContract {
    pub fn new(contract: NickelValue, label: Label) -> Self {
        RuntimeContract { contract, label }
    }

    /// Generate a runtime contract from a type used as a static type annotation and a label. Use
    /// the guarantees of the static type system to optimize and simplify the contract.
    pub fn from_static_type(
        pos_table: &mut PosTable,
        labeled_typ: LabeledType,
    ) -> Result<Self, UnboundTypeVariableError> {
        Ok(RuntimeContract {
            contract: labeled_typ.typ.contract_static(pos_table)?,
            label: labeled_typ.label,
        })
    }

    /// Generate a runtime contract from a type used as a contract annotation and a label.
    pub fn from_type(
        pos_table: &mut PosTable,
        labeled_ty: LabeledType,
    ) -> Result<Self, UnboundTypeVariableError> {
        Ok(RuntimeContract::new(
            labeled_ty.typ.contract(pos_table)?,
            labeled_ty.label,
        ))
    }

    /// Map a function over the term representing the underlying contract.
    pub fn map_contract<F>(self, f: F) -> Self
    where
        F: FnOnce(NickelValue) -> NickelValue,
    {
        RuntimeContract {
            contract: f(self.contract),
            ..self
        }
    }

    /// Apply this contract to a value.
    pub fn apply(self, value: NickelValue, pos_idx: PosIdx) -> NickelValue {
        use crate::mk_app;

        mk_app!(
            make::op2(
                BinaryOp::ContractApply,
                self.contract,
                NickelValue::label_posless(self.label),
            )
            .with_pos_idx(pos_idx),
            value
        )
        .with_pos_idx(pos_idx)
    }

    /// Apply a series of contracts to a value, in order.
    pub fn apply_all<I>(value: NickelValue, contracts: I, pos_idx: PosIdx) -> NickelValue
    where
        I: IntoIterator<Item = Self>,
    {
        contracts
            .into_iter()
            .fold(value, |acc, ctr| ctr.apply(acc, pos_idx))
    }

    /// Push a pending contract to a vector of contracts if the contract to add isn't already
    /// present in the vector, according to the notion of contract equality defined in
    /// [crate::eval::contract_eq].
    pub fn push_dedup(
        contracts: &mut Vec<RuntimeContract>,
        env1: &Environment,
        ctr: Self,
        env2: &Environment,
    ) {
        for c in contracts.iter() {
            increment!("contracts:equality-checks");

            if contract_eq::contract_eq(&c.contract, env1, &ctr.contract, env2) {
                increment!("contracts:deduped");
                return;
            }
        }

        contracts.push(ctr);
    }

    /// Concatenate two deduplicated contract vectors into a deduplicated contract vector.
    pub fn combine_dedup(
        contracts1: Vec<RuntimeContract>,
        env1: &Environment,
        contracts2: Vec<RuntimeContract>,
        env2: &Environment,
    ) -> Vec<RuntimeContract> {
        let len1 = contracts1.len();
        let mut result = contracts1;
        result.reserve(contracts2.len());

        for ctr2 in contracts2 {
            let is_duplicate = result[..len1].iter().any(|ctr1| {
                increment!("contracts:equality-checks");
                contract_eq::contract_eq(&ctr1.contract, env1, &ctr2.contract, env2)
            });

            if !is_duplicate {
                result.push(ctr2);
            } else {
                increment!("contracts:deduped");
            }
        }

        result
    }

    /// Check if this contract might have polymorphic subcontracts. See
    /// [crate::label::Label::can_have_poly_ctrs].
    pub fn can_have_poly_ctrs(&self) -> bool {
        self.label.can_have_poly_ctrs()
    }
}

impl Traverse<NickelValue> for RuntimeContract {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(NickelValue) -> Result<NickelValue, E>,
    {
        let contract = self.contract.traverse(f, order)?;
        Ok(RuntimeContract { contract, ..self })
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&NickelValue, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        self.contract.traverse_ref(f, state)
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
#[derive(Debug, Default, Clone, PartialEq)]
pub struct LetMetadata {
    pub doc: Option<String>,
    pub annotation: TypeAnnotation,
}

impl From<LetMetadata> for record::FieldMetadata {
    fn from(let_metadata: LetMetadata) -> Self {
        record::FieldMetadata {
            annotation: let_metadata.annotation,
            doc: let_metadata.doc.map(Rc::from),
            ..Default::default()
        }
    }
}

/// A branch of a match expression.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchBranch {
    /// The pattern on the left hand side of `=>`.
    pub pattern: Pattern,
    /// A potential guard, which is an additional side-condition defined as `if cond`. The value
    /// stored in this field is the boolean condition itself.
    pub guard: Option<NickelValue>,
    /// The body of the branch, on the right hand side of `=>`.
    pub body: NickelValue,
}

/// Content of a match expression.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchData {
    /// Branches of the match expression, where the first component is the pattern on the left hand
    /// side of `=>` and the second component is the body of the branch.
    pub branches: Vec<MatchBranch>,
}

/// A type or a contract together with its corresponding label.
#[derive(Debug, PartialEq, Clone)]
pub struct LabeledType {
    pub typ: Type,
    pub label: Label,
}

impl LabeledType {
    /// Create a labeled type from a type and a span, which are the minimal information required to
    /// instantiate the type and the underlying label. All other values are set to the defaults.
    pub fn new(typ: Type, span: RawSpan) -> Self {
        Self {
            typ: typ.clone(),
            label: Label {
                typ: Rc::new(typ),
                span: Some(span),
                ..Default::default()
            },
        }
    }

    /// Modify the label's `field_name` field.
    pub fn with_field_name(self, ident: Option<LocIdent>) -> Self {
        LabeledType {
            label: self.label.with_field_name(ident),
            ..self
        }
    }
}

impl Serialize for LabeledType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.label.typ.to_string())
    }
}

impl Traverse<NickelValue> for LabeledType {
    // Note that this function doesn't traverse the label, which is most often what you want. The
    // terms that may hide in a label are mostly types used for error reporting, but are never
    // evaluated.
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<LabeledType, E>
    where
        F: FnMut(NickelValue) -> Result<NickelValue, E>,
    {
        let LabeledType { typ, label } = self;
        typ.traverse(f, order).map(|typ| LabeledType { typ, label })
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&NickelValue, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        self.typ.traverse_ref(f, state)
    }
}

/// A type and/or contract annotation.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct TypeAnnotation {
    /// The type annotation (using `:`).
    pub typ: Option<LabeledType>,

    /// The contracts annotation (using `|`).
    pub contracts: Vec<LabeledType>,
}

impl TypeAnnotation {
    /// Return the main annotation, which is either the type annotation if any, or the first
    /// contract annotation.
    pub fn first(&self) -> Option<&LabeledType> {
        self.typ.iter().chain(self.contracts.iter()).next()
    }

    /// Iterate over the annotations, starting by the type and followed by the contracts.
    pub fn iter(&self) -> impl Iterator<Item = &LabeledType> {
        self.typ.iter().chain(self.contracts.iter())
    }

    /// Mutably iterate over the annotations, starting by the type and followed by the contracts.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut LabeledType> {
        self.typ.iter_mut().chain(self.contracts.iter_mut())
    }

    /// Return a string representation of the contracts (without the static type annotation) as a
    /// comma-separated list.
    pub fn contracts_to_string(&self) -> Option<String> {
        (!self.contracts.is_empty()).then(|| {
            self.contracts
                .iter()
                .map(|contract| format!("{}", contract.label.typ,))
                .collect::<Vec<_>>()
                .join(",")
        })
    }

    /// Build a list of pending contracts from this annotation, to be stored alongside the metadata
    /// of a field. Similar to [Self::all_contracts], but including the contracts from
    /// `self.contracts` only, while `types` is excluded. Contracts derived from type annotations
    /// aren't treated the same since they don't propagate through merging.
    pub fn pending_contracts(
        &self,
        pos_table: &mut PosTable,
    ) -> Result<Vec<RuntimeContract>, UnboundTypeVariableError> {
        self.contracts
            .iter()
            .cloned()
            .map(|labeled_ty| RuntimeContract::from_type(pos_table, labeled_ty))
            .collect::<Result<Vec<_>, _>>()
    }

    /// Build the contract derived from the static type annotation, applying the specific
    /// optimizations along the way.
    pub fn static_contract(
        &self,
        pos_table: &mut PosTable,
    ) -> Option<Result<RuntimeContract, UnboundTypeVariableError>> {
        self.typ
            .as_ref()
            .cloned()
            .map(|labeled_ty| RuntimeContract::from_static_type(pos_table, labeled_ty))
    }

    /// Convert all the contracts of this annotation, including the potential type annotation as
    /// the first element, to a runtime representation. Apply contract optimizations to the static
    /// type annotation.
    pub fn all_contracts(
        &self,
        pos_table: &mut PosTable,
    ) -> Result<Vec<RuntimeContract>, UnboundTypeVariableError> {
        self.typ
            .as_ref()
            .cloned()
            .map(|labeled_ty| RuntimeContract::from_static_type(pos_table, labeled_ty))
            .into_iter()
            .chain(
                self.contracts
                    .iter()
                    .cloned()
                    .map(|labeled_ty| RuntimeContract::from_type(pos_table, labeled_ty)),
            )
            .collect::<Result<Vec<_>, _>>()
    }

    /// Set the `field_name` attribute of the labels of the type and contracts annotations.
    pub fn with_field_name(self, field_name: Option<LocIdent>) -> Self {
        TypeAnnotation {
            typ: self.typ.map(|t| t.with_field_name(field_name)),
            contracts: self
                .contracts
                .into_iter()
                .map(|t| t.with_field_name(field_name))
                .collect(),
        }
    }

    /// Return `true` if this annotation is empty, i.e. hold neither a type annotation nor
    /// contracts annotations.
    pub fn is_empty(&self) -> bool {
        self.typ.is_none() && self.contracts.is_empty()
    }

    /// **Warning**: the contract equality check used in this function behaves like syntactic
    /// equality, and doesn't take the environment into account. It's unsound for execution (it
    /// could equate contracts that are actually totally distinct), but we use it only to trim
    /// accumulated contracts before pretty-printing. Do not use prior to any form of evaluation.
    ///
    /// Same as [`crate::combine::Combine`], but eliminate duplicate contracts. As there's no
    /// notion of environment when considering mere annotations, we use an unsound contract
    /// equality checking which correspond to comparing contracts syntactically.
    pub fn combine_dedup(left: Self, right: Self) -> Self {
        let len1 = left.contracts.len();
        let mut contracts = left.contracts;
        contracts.reserve(right.contracts.len() + 1);

        let typ = match (left.typ, right.typ) {
            (left_ty @ Some(_), Some(right_ty)) => {
                contracts.push(right_ty);
                left_ty
            }
            (left_ty, right_ty) => left_ty.or(right_ty),
        };

        for ctr in right.contracts.into_iter() {
            if !contracts[..len1]
                .iter()
                .any(|c| contract_eq::type_eq_noenv(&c.typ, &ctr.typ))
            {
                contracts.push(ctr);
            }
        }

        TypeAnnotation { typ, contracts }
    }
}

impl Combine for TypeAnnotation {
    fn combine(left: Self, right: Self) -> Self {
        let (typ, leftover) = match (left.typ, right.typ) {
            (left_ty @ Some(_), right_ty @ Some(_)) => (left_ty, right_ty),
            (left_ty, right_ty) => (left_ty.or(right_ty), None),
        };

        let contracts: Vec<_> = left
            .contracts
            .iter()
            .cloned()
            .chain(leftover)
            .chain(right.contracts.iter().cloned())
            .collect();

        TypeAnnotation { typ, contracts }
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

impl Traverse<NickelValue> for TypeAnnotation {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(NickelValue) -> Result<NickelValue, E>,
    {
        let TypeAnnotation { typ, contracts } = self;

        let contracts = contracts
            .into_iter()
            .map(|labeled_ty| labeled_ty.traverse(f, order))
            .collect::<Result<Vec<_>, _>>()?;

        let typ = typ
            .map(|labeled_ty| labeled_ty.traverse(f, order))
            .transpose()?;

        Ok(TypeAnnotation { typ, contracts })
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&NickelValue, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        self.contracts
            .iter()
            .find_map(|c| c.traverse_ref(f, state))
            .or_else(|| self.typ.as_ref().and_then(|t| t.traverse_ref(f, state)))
    }
}

impl Term {
    /// Return the class of an expression in WHNF. See
    /// [crate::eval::value::NickelValue::type_of].
    pub fn type_of(&self) -> Option<&'static str> {
        match self {
            Term::Closurize(value) => value.type_of(),
            Term::RecRecord(..) => Some("Record"),
            Term::Fun(_) | Term::FunPattern(_) => Some("Function"),
            // We could print a separate type for predicates. For the time being, we just consider
            // it to be the function resulting of `$predicate_to_ctr pred`.
            Term::Match { .. } => Some("MatchExpression"),
            Term::Sealed(..) => Some("Sealed"),
            Term::Annotated(..) => Some("Annotated"),
            Term::Let(..)
            | Term::LetPattern(..)
            | Term::App(_)
            | Term::Var(_)
            | Term::Op1(_)
            | Term::Op2(_)
            | Term::OpN(_)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_)
            | Term::ParseError(_)
            | Term::RuntimeError(_) => None,
        }
    }

    /// Determine if a term is in evaluated form, called weak head normal form (WHNF). A weak head
    /// normal form isn't evaluated further by the virtual machine.
    pub fn is_whnf(&self) -> bool {
        match self {
            Term::Fun(..)
            // Match expressions are function
            | Term::Match {..} => true,
            Term::Closurize(_)
            | Term::Let(..)
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

    /// Determine if a term is an atom of the surface syntax. Atoms are basic elements of the
    /// syntax that can freely substituted without being parenthesized.
    pub fn fmt_is_atom(&self) -> bool {
        match self {
            Term::Closurize(value) => value.fmt_is_atom(),
            Term::StrChunks(..) | Term::RecRecord(..) | Term::Var(..) => true,
            // Those special cases aren't really atoms, but mustn't be parenthesized because they
            // are really functions taking additional non-strict arguments and printed as "partial"
            // infix operators.
            //
            // For example, `Op1(BoolOr, Var("x"))` is currently printed as `x ||`. Such operators
            // must never be parenthesized, such as in `(x ||)`.
            //
            // We might want a more robust mechanism for pretty printing such operators.
            Term::Op1(data) => matches!(
                &data.op,
                UnaryOp::RecordAccess(_) | UnaryOp::BoolAnd | UnaryOp::BoolOr
            ),
            Term::Op2(data) => matches!(&data.op, BinaryOp::RecordGet),
            // A number with a minus sign as a prefix isn't a proper atom
            Term::Let(..)
            | Term::Match { .. }
            | Term::LetPattern(..)
            | Term::Fun(..)
            | Term::FunPattern(..)
            | Term::App(..)
            | Term::OpN(..)
            | Term::Sealed(..)
            | Term::Annotated(..)
            | Term::Import(_)
            | Term::ResolvedImport(..)
            | Term::ParseError(_)
            | Term::RuntimeError(_) => false,
        }
    }

    /// Extract the static literal from string chunk. It only returns a `Some(..)`
    /// when the term is a `Term::StrChunk` and all the chunks are `StrChunk::Literal(..)`
    pub fn try_str_chunk_as_static_str(&self) -> Option<String> {
        match self {
            Term::StrChunks(chunks) => StrChunk::try_chunks_as_static_str(chunks),
            _ => None,
        }
    }

    /// Builds a term representing a function.
    pub fn fun(arg: LocIdent, body: NickelValue) -> Self {
        Term::Fun(FunData { arg, body })
    }

    /// Builds a term representing a function that patterns match on its argument.
    pub fn fun_pattern(pattern: Pattern, body: NickelValue) -> Self {
        Term::FunPattern(Box::new(FunPatternData { pattern, body }))
    }

    pub fn let_in(
        bindings: SmallVec<[(LocIdent, NickelValue); 4]>,
        body: NickelValue,
        attrs: LetAttrs,
    ) -> Self {
        Term::Let(Box::new(LetData {
            bindings,
            body,
            attrs,
        }))
    }

    pub fn let_pattern(
        bindings: SmallVec<[(Pattern, NickelValue); 1]>,
        body: NickelValue,
        attrs: LetAttrs,
    ) -> Self {
        Term::LetPattern(Box::new(LetPatternData {
            bindings,
            body,
            attrs,
        }))
    }

    pub fn rec_record(
        record: RecordData,
        includes: Vec<Include>,
        dyn_fields: Vec<(NickelValue, Field)>,
        deps: Option<RecordDeps>,
        closurized: bool,
    ) -> Self {
        Term::RecRecord(Box::new(RecRecordData {
            record,
            includes,
            dyn_fields,
            deps,
            closurized,
        }))
    }

    pub fn op1(op: UnaryOp, arg: NickelValue) -> Self {
        Term::Op1(Op1Data { op, arg })
    }

    pub fn op2(op: BinaryOp, arg1: NickelValue, arg2: NickelValue) -> Self {
        Term::Op2(Op2Data { op, arg1, arg2 })
    }

    pub fn opn(op: NAryOp, args: Vec<NickelValue>) -> Self {
        Term::OpN(OpNData { op, args })
    }

    pub fn sealed(key: SealingKey, inner: NickelValue, label: Label) -> Self {
        Term::Sealed(SealedData {
            key,
            inner,
            label: Rc::new(label),
        })
    }

    pub fn annotated(annot: TypeAnnotation, inner: NickelValue) -> Self {
        Term::Annotated(AnnotatedData {
            annot: Rc::new(annot),
            inner,
        })
    }

    pub fn app(head: NickelValue, arg: NickelValue) -> Self {
        Term::App(AppData { head, arg })
    }

    pub fn parse_error(error: ParseError) -> Self {
        Term::ParseError(Box::new(error))
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
    IfThenElse,

    /// Return an enum tag representing the type of the term.
    Typeof,

    /// Return an enum whose tag represents the type of the term, carrying a payload with a statically
    /// typed value.
    Cast,

    // Boolean AND and OR operator are encoded as unary operators so that they can be lazy in their
    // second argument.
    /// Boolean AND operator.
    BoolAnd,

    /// Boolean OR operator.
    BoolOr,

    /// Boolean NOT operator.
    BoolNot,

    /// Raise a blame, which stops the execution and prints an error according to the label
    /// argument.
    Blame,

    /// Typecast an enum to a larger enum type.
    ///
    /// `EnumEmbed` is used to upcast enums. For example, if a value `x` has enum type `a | b`,
    /// then `%enum/embed% c x` will have enum type `a | b | c`. It only affects typechecking as at
    /// runtime `%enum/embed% someId` acts like the identity function.
    EnumEmbed(LocIdent),

    /// A specialized primop for match when all patterns are enum tags. In that case, instead of
    /// compiling to a generic sequence of if-then-else, we can be much more efficient by indexing
    /// into a hashmap. [Self::TagsOnlyMatch] takes additional lazy arguments: a record mapping
    /// tags to the corresponding branches, and the default case when `has_default` is `true`.
    TagsOnlyMatch {
        has_default: bool,
    },

    /// Static record access.
    ///
    /// Static means that the field identifier is a statically known string inside the source.
    RecordAccess(LocIdent),

    /// Map a function on each element of an array.
    ArrayMap,

    /// Map a function on a record.
    ///
    /// The mapped function must take two arguments, the name of the field as a string, and the
    /// content of the field. `RecordMap` then replaces the content of each field by the result of
    /// the function: i.e., `%record/map% f {a=2;}` evaluates to `{a=(f "a" 2);}`.
    RecordMap,

    /// Inverse the polarity of a label.
    LabelFlipPol,

    /// Get the polarity of a label.
    LabelPol,

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
    /// Then `GoDom` evaluates to a copy of this label, where the path has gone forward into the
    /// domain:
    ///
    /// ```text
    /// (Num -> Num) -> Num
    ///  ^^^ new type path
    /// ------------------- original type
    /// ```
    LabelGoDom,

    /// Go to the codomain in the type path of a label.
    ///
    /// See `GoDom`.
    LabelGoCodom,

    /// Go to the array in the type path of a label.
    ///
    /// See `GoDom`.
    LabelGoArray,

    /// Go to the type ascribed to every field in a dictionary.
    ///
    /// See `GoDom`.
    LabelGoDict,

    /// Force the evaluation of its argument and proceed with the second.
    Seq,

    /// Recursively force the evaluation of its first argument then returns the second.
    ///
    /// Recursive here means that the evaluation does not stop at a WHNF, but the content of arrays
    /// and records is also recursively forced.
    DeepSeq,

    /// Return the length of an array.
    ArrayLength,

    /// Generate an array of a given length by mapping a `Num -> Num` function onto `[1,..,n]`.
    ArrayGen,

    /// Generated by the evaluation of a string with interpolated expressions. `ChunksConcat`
    /// applied to the current chunk to evaluate. As additional state, it uses a string
    /// accumulator, the indentation of the chunk being evaluated, and the remaining chunks to be
    /// evaluated, all stored on the stack.
    ChunksConcat,

    /// Return the names of the fields of a record as a string array.
    RecordFields(RecordOpKind),

    /// Return the values of the fields of a record as an array.
    RecordValues,

    /// Remove heading and trailing spaces from a string.
    StringTrim,

    /// Return the array of characters of a string.
    StringChars,

    /// Transform a string to uppercase.
    StringUppercase,

    /// Transform a string to lowercase.
    StringLowercase,

    /// Return the length of a string.
    StringLength,

    /// Transform a data to a string.
    ToString,

    /// Transform a string to a number.
    NumberFromString,

    /// Transform a string to an enum.
    EnumFromString,

    /// Test if a regex matches a string.
    /// Like [`UnaryOp::StringFind`], this is a unary operator because we would like a way to share
    /// the same "compiled regex" for many matching calls. This is done by returning functions
    /// wrapping [`UnaryOp::StringIsMatchCompiled`] and [`UnaryOp::StringFindCompiled`]
    StringIsMatch,

    /// Match a regex on a string, and returns the captured groups together, the index of the
    /// match, etc.
    StringFind,

    /// Returns all matches of a regex on a string, as an array of matches. Each
    /// match contains the match groups, the starting index of the match and the
    /// matched string.
    StringFindAll,

    /// Version of [`UnaryOp::StringIsMatch`] which remembers the compiled regex.
    StringIsMatchCompiled(CompiledRegex),

    /// Version of [`UnaryOp::StringFind`] which remembers the compiled regex.
    StringFindCompiled(CompiledRegex),

    /// Version of [`UnaryOp::StringFindAll`] which remembers the compiled regex.
    StringFindAllCompiled(CompiledRegex),

    /// Force full evaluation of a term and return it.
    ///
    /// This was added in the context of [`BinaryOp::ContractArrayLazyApp`], in particular to make
    /// serialization work with lazy array contracts.
    ///
    /// # `Force` vs. `DeepSeq`
    ///
    /// [`UnaryOp::Force`] updates at the indices containing arrays with a new version where the
    /// lazy contracts have all been applied, whereas [`UnaryOp::DeepSeq`] evaluates the same
    /// expressions, but it never updates at the index of an array with lazy contracts with an array
    /// where those contracts have been applied. In a way, the result of lazy contract application
    /// in arrays is "lost" in [`UnaryOp::DeepSeq`], while it's returned in [`UnaryOp::Force`].
    ///
    /// This means we can observe different results between `deep_seq x x` and `force x`, in some
    /// cases.
    ///
    /// It's also worth noting that [`UnaryOp::DeepSeq`] should be, in principle, more efficient
    /// that [`UnaryOp::Force`] as it does less cloning.
    ///
    /// # About `for_export`
    ///
    /// When exporting a Nickel term, we first apply `Force` to the term to evaluate it. If there
    /// are record fields that have been marked `not_exported`, they would still be evaluated
    /// ordinarily, see [#1230](https://github.com/tweag/nickel/issues/1230). To stop this from
    /// happening, we introduce the `for_export` parameter here. When `for_export` is `true`, the
    /// evaluation of `Force` will skip fields that are marked as `not_exported`. When `for_export`
    /// is `false`, these fields are evaluated.
    Force {
        ignore_not_exported: bool,
    },

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
    RecDefault,

    /// Recursive force priority operator. Similar to [UnaryOp::RecDefault], but propagate the
    /// `force` annotation.
    ///
    /// As opposed to `RecDefault`, the `force` takes precedence and erase any prior explicit
    /// priority annotation.
    RecForce,

    /// Creates an "empty" record with the sealed tail of its record argument.
    ///
    /// Used in the `$record` contract implementation to ensure that we can define a `field_diff`
    /// function that preserves the sealed polymorphic tail of its argument.
    RecordEmptyWithTail,

    /// Freezes a recursive record to make it a static dictionary. Apply all pending lazy contracts
    /// (and flush them), and remove all dependency information, so that the value of the fields is
    /// fixed in time and subsequent overrides will only impact the overriden field.
    RecordFreeze,

    /// Print a message when encountered during evaluation and proceed with the evaluation of the
    /// argument on the top of the stack. Operationally the same as the identity function
    Trace,

    /// Push a new, fresh diagnostic on the diagnostic stack of a contract label. This has the
    /// effect of saving the current diagnostic, as following calls to primop that modifies the
    /// label's current diagnostic will modify the fresh one, istead of the one being stacked.
    /// This primop shouldn't be used directly by user a priori, but is used internally during e.g.
    /// contract application.
    LabelPushDiag,

    /// Evaluate a string of nix code into a resulting nickel value. Currently completely
    /// (strictly) evaluates the nix code, and must result in a value serializable into JSON.
    #[cfg(feature = "nix-experimental")]
    EvalNix,

    /// Retrive the argument from an enum variant: `%enum/get_arg% ('Foo t) := t`
    EnumGetArg,
    /// Create an enum variant from a tag and an argument. This operator is strict in tag and
    /// return a function that can be further applied to an argument.
    EnumMakeVariant,
    /// Return true if the given parameter is an enum variant.
    EnumIsVariant,
    /// Extract the tag from an enum tag or an enum variant.
    EnumGetTag,

    /// Wrap a contract implementation as a custom contract. You can think of this primop as a
    /// type constructor for custom contracts.
    ContractCustom,

    /// After applying a custom contract (or a builtin contract), the result is either `'Ok value`
    /// or `'Error err_data`. This primop post-processes this return value (the first argument) to
    /// either produce `value` in the first case or to attach the error data to the label (the
    /// second argument, taken from the stack, as this op isn't strict in the label) and blame in
    /// the second.
    ContractPostprocessResult,

    ContractAttachDefaultLabel,

    /// The cosinus function.
    NumberArcCos,

    /// The sinus function.
    NumberArcSin,

    /// The tangent function.
    NumberArcTan,

    /// The cosinus function.
    NumberCos,

    /// The sinus function.
    NumberSin,

    /// The tangent function.
    NumberTan,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use UnaryOp::*;
        match self {
            IfThenElse => write!(f, "if_then_else"),
            Typeof => write!(f, "typeof"),
            Cast => write!(f, "cast"),
            BoolAnd => write!(f, "(&&)"),
            BoolOr => write!(f, "(||)"),
            BoolNot => write!(f, "bool/not"),
            Blame => write!(f, "blame"),
            EnumEmbed(_) => write!(f, "enum/embed"),
            TagsOnlyMatch { .. } => write!(f, "match"),
            RecordAccess(_) => write!(f, "record/access"),
            ArrayMap => write!(f, "array/map"),
            RecordMap => write!(f, "record/map"),
            LabelFlipPol => write!(f, "label/flip_polarity"),
            LabelPol => write!(f, "label/polarity"),
            LabelGoDom => write!(f, "label/go_dom"),
            LabelGoCodom => write!(f, "label/go_codom"),
            LabelGoArray => write!(f, "label/go_array"),
            LabelGoDict => write!(f, "label/go_dict"),
            Seq => write!(f, "seq"),
            DeepSeq => write!(f, "deep_seq"),
            ArrayLength => write!(f, "array/length"),
            ArrayGen => write!(f, "array/generate"),
            ChunksConcat => write!(f, "chunks_concat"),
            RecordFields(RecordOpKind::IgnoreEmptyOpt) => write!(f, "record/fields"),
            RecordFields(RecordOpKind::ConsiderAllFields) => write!(f, "record/fields_with_opts"),
            RecordValues => write!(f, "record/values"),
            StringTrim => write!(f, "string/trim"),
            StringChars => write!(f, "string/chars"),
            StringUppercase => write!(f, "string/uppercase"),
            StringLowercase => write!(f, "string/lowercase"),
            StringLength => write!(f, "string/length"),
            ToString => write!(f, "to_string"),
            NumberFromString => write!(f, "number/from_string"),
            EnumFromString => write!(f, "enum/from_string"),
            StringIsMatch => write!(f, "string/is_match"),
            StringFind => write!(f, "string/find"),
            StringFindAll => write!(f, "string/find_all"),
            StringIsMatchCompiled(_) => write!(f, "string/is_match_compiled"),
            StringFindCompiled(_) => write!(f, "string/find_compiled"),
            StringFindAllCompiled(_) => write!(f, "string/find_all_compiled"),
            Force { .. } => write!(f, "force"),
            RecDefault => write!(f, "rec_default"),
            RecForce => write!(f, "rec_force"),
            RecordEmptyWithTail => write!(f, "record/empty_with_tail"),
            RecordFreeze => write!(f, "record/freeze"),
            Trace => write!(f, "trace"),
            LabelPushDiag => write!(f, "label/push_diag"),

            #[cfg(feature = "nix-experimental")]
            EvalNix => write!(f, "eval_nix"),

            EnumGetArg => write!(f, "enum/get_arg"),
            EnumMakeVariant => write!(f, "enum/make_variant"),
            EnumIsVariant => write!(f, "enum/is_variant"),
            EnumGetTag => write!(f, "enum/get_tag"),

            ContractCustom => write!(f, "contract/custom"),
            ContractPostprocessResult => write!(f, "contract/postprocess_result"),
            ContractAttachDefaultLabel => write!(f, "contract/attach_default_label"),

            NumberArcCos => write!(f, "number/arccos"),
            NumberArcSin => write!(f, "number/arcsin"),
            NumberArcTan => write!(f, "number/arctan"),
            NumberCos => write!(f, "number/cos"),
            NumberSin => write!(f, "number/sin"),
            NumberTan => write!(f, "number/tan"),
        }
    }
}

// See: https://github.com/rust-lang/regex/issues/178
/// [`regex::Regex`] which implements [`PartialEq`].
#[derive(Debug, Clone)]
pub struct CompiledRegex(pub regex::Regex);

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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
            BoolAnd | BoolOr | RecordAccess(_) => OpPos::Postfix,
            IfThenElse => OpPos::Special,
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
    Plus,

    /// Subtraction of numerals.
    Sub,

    /// Multiplication of numerals.
    Mult,

    /// Floating-point division of numerals.
    Div,

    /// Modulo of numerals.
    Modulo,

    /// Give the four quadrant arctangent of y and x.
    NumberArcTan2,

    /// Give the logarithm of a number.
    NumberLog,

    /// Raise a number to a power.
    Pow,

    /// Concatenation of strings.
    StringConcat,

    /// Polymorphic equality.
    Eq,

    /// Strictly less than comparison operator.
    LessThan,

    /// Less than or equal comparison operator.
    LessOrEq,

    /// Strictly greater than comparison operator.
    GreaterThan,

    /// Greater than or equal comparison operator.
    GreaterOrEq,

    /// Apply a contract to a label and a value. The value is is stored on the stack unevaluated,
    /// while the contract and the label are the strict arguments to this operator. `ApplyContract`
    /// also accepts contracts as records, which are translated to a function that merge said
    /// contract with its argument. Finally, this operator marks the location of the contract
    /// argument on the stack for better error reporting.
    ///
    /// Either the contract raises a blame error, or the primop evaluates to the value returned by
    /// the contract that can be used in place of the original value.
    ContractApply,

    /// Variant of [Self::ContractApply] which also applies an arbitrary contract to a label and a
    /// value, but instead of either blaming or returning the value, it has the same return value
    /// as contract built via [UnaryOp::ContractCustom], that is `[| 'Ok Dyn, 'Error {..}|]`. The
    /// value returned through `'Ok` can still have lazy blame expressions inside, of course.
    ///
    /// Put differently, `%contract/check% (%contract/custom% custom) label value` is equivalent to
    /// `custom label value`, modulo argument tracking. [Self::ContractCheck] doesn't only work on
    /// custom contracts, but on builtin contracts as well.
    ///
    /// This operation is useful for contract composition, that is when calling a contract from
    /// another contract. In theory, one could use [Self::ContractApply], but the caller then needs
    /// to wrap the result in `'Ok`, and much more importantly, `%contract/apply%` converts all
    /// immediate errors returned as `'Error` into blame errors. This is not desirable, as blame
    /// errors can't be caught, which artificially makes the called contract entirely delayed. This
    /// typically wouldn't play very well with boolean combinators. On the other hand,
    /// [Self::ContractCheck] preserves the immediate/delayed part of the called contract.
    ContractCheck,

    /// Take a record of type `{message | String | optional, notes | String | optional}`.
    LabelWithErrorData,

    /// Unseal a sealed term.
    ///
    /// See [`BinaryOp::Seal`].
    Unseal,

    /// Go to a specific field in the type path of a label.
    ///
    /// See `LabelGoDom`.
    LabelGoField,

    /// Extend a record with a dynamic field.
    ///
    /// Dynamic means that the field name may be an expression instead of a statically known
    /// string. `RecordExtend` tries to evaluate this name to a string, and in case of success, add
    /// a field with this name to the given record with the expression on top of the stack as
    /// content.
    ///
    /// The field may have been defined with attached metadata, pending contracts and may or may
    /// not have a defined value. We can't store those information as a term argument (metadata
    /// aren't first class values, at least at the time of writing), so for now we attach it
    /// directly to the extend primop. This isn't ideal, and in the future we may want to have a
    /// more principled primop.
    RecordInsert {
        metadata: SharedMetadata,
        pending_contracts: Vec<RuntimeContract>,
        ext_kind: RecordExtKind,
        op_kind: RecordOpKind,
    },

    /// Remove a field from a record. The field name is given as an argument.
    RecordRemove(RecordOpKind),

    /// Dynamically access a field of record. The field name is given as an argument which should
    /// evaluate to a string.
    RecordGet,

    /// Test if a record has a specific field.
    RecordHasField(RecordOpKind),

    /// Test if the field of a record exists and has a definition.
    RecordFieldIsDefined(RecordOpKind),

    /// Take a pair of records and split them into four separate records:
    ///
    /// - `left_only`: fields of the left argument but not in the right
    /// - `left_center`: fields of the left argument that happens to also be in the right (but the
    ///   value and the metadata are taken from the left)
    /// - `right_center`: fields of the right argument that happens to also be in the left (but the
    ///   value and the metadata are taken from the right)
    /// - `right_only`: fields of the right argument but not in the left
    ///
    /// As opposed to an equivalent user-defined implementation, this primop has better performance
    /// and is able to preserve field metadata.
    ///
    /// If `left` (resp. `right`) is open or has a sealed tail, then `left_only` (resp.
    /// `right_only`) will inherit the same properties. `left_center` (resp. `right_center`) are
    /// always closed and without a sealed tail.
    RecordSplitPair,

    /// Take a pair of disjoint records (i.e. records with no common field) and combine them into
    /// one. It's a form of merging, but based on the assumption that the records are disjoint and
    /// thus non-conflicting, it's simpler and more efficient than a general merge.
    ///
    /// As for merge, this raises a blame error if one of the arguments has a sealed tail.
    RecordDisjointMerge,

    /// Concatenate two arrays.
    ArrayConcat,

    /// Access the n-th element of an array.
    ArrayAt,

    /// The merge operator (see [crate::eval::merge]). `Merge` is parametrized by a
    /// [crate::label::MergeLabel], which carries additional information for error-reporting
    /// purpose.
    Merge(MergeLabel),

    /// Hash a string.
    Hash,

    /// Serialize a value to a string.
    Serialize,

    /// Deserialize a string to a value.
    Deserialize,

    /// Split a string into an array.
    StringSplit,

    /// Determine if a string is a substring of another one.
    StringContains,

    /// Compare two strings lexicographically.
    StringCompare,

    /// Seal a term with a sealing key (see [`Term::Sealed`]).
    Seal,

    /// Lazily apply a contract to an Array.
    /// This simply inserts a contract into the array attributes.
    ContractArrayLazyApp,

    /// Lazily map contracts over a record. The arguments are a label and a function which takes
    /// the name of the field as a parameter and returns the corresponding contract.
    ContractRecordLazyApp,

    /// Set the message of the current diagnostic of a label.
    LabelWithMessage,

    /// Set the notes of the current diagnostic of a label.
    LabelWithNotes,

    /// Append a note to the current diagnostic of a label.
    LabelAppendNote,

    /// Look up the [`crate::label::TypeVarData`] associated with a [`SealingKey`] in the type
    /// environment of a label.
    LabelLookupTypeVar,
}

impl BinaryOp {
    pub fn pos(&self) -> OpPos {
        use BinaryOp::*;
        match self {
            Plus | Sub | Mult | Div | Modulo | StringConcat | Eq | LessThan | LessOrEq
            | GreaterThan | GreaterOrEq | ArrayConcat | Merge(_) => OpPos::Infix,
            _ => OpPos::Prefix,
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BinaryOp::*;
        match self {
            Plus => write!(f, "(+)"),
            Sub => write!(f, "(-)"),
            Mult => write!(f, "(*)"),
            Div => write!(f, "(/)"),
            Modulo => write!(f, "(%)"),
            NumberArcTan2 => write!(f, "number/arctan2"),
            NumberLog => write!(f, "number/log"),
            Pow => write!(f, "pow"),
            StringConcat => write!(f, "string/concat"),
            Eq => write!(f, "(==)"),
            LessThan => write!(f, "(<)"),
            LessOrEq => write!(f, "(<=)"),
            GreaterThan => write!(f, "(>)"),
            GreaterOrEq => write!(f, "(>=)"),
            ContractApply => write!(f, "contract/apply"),
            ContractCheck => write!(f, "contract/check"),
            LabelWithErrorData => write!(f, "label/with_error_data"),
            Unseal => write!(f, "unseal"),
            LabelGoField => write!(f, "label/go_field"),
            RecordInsert {
                op_kind: RecordOpKind::IgnoreEmptyOpt,
                ..
            } => write!(f, "record/insert"),
            RecordInsert {
                op_kind: RecordOpKind::ConsiderAllFields,
                ..
            } => write!(f, "record/insert_with_opts"),
            RecordRemove(RecordOpKind::IgnoreEmptyOpt) => write!(f, "record/remove"),
            RecordRemove(RecordOpKind::ConsiderAllFields) => write!(f, "record/remove_with_opts"),
            RecordGet => write!(f, "record/get"),
            RecordHasField(RecordOpKind::IgnoreEmptyOpt) => write!(f, "record/has_field"),
            RecordHasField(RecordOpKind::ConsiderAllFields) => {
                write!(f, "record/has_field_with_opts")
            }
            RecordFieldIsDefined(RecordOpKind::IgnoreEmptyOpt) => {
                write!(f, "record/field_is_defined")
            }
            RecordFieldIsDefined(RecordOpKind::ConsiderAllFields) => {
                write!(f, "record/field_is_defined_with_opts")
            }
            Self::RecordSplitPair => write!(f, "record/split_pair"),
            Self::RecordDisjointMerge => write!(f, "record/disjoint_merge"),
            ArrayConcat => write!(f, "(@)"),
            ArrayAt => write!(f, "array/at"),
            Merge(_) => write!(f, "(&)"),
            Hash => write!(f, "hash"),
            Serialize => write!(f, "serialize"),
            Deserialize => write!(f, "deserialize"),
            StringSplit => write!(f, "string/split"),
            StringContains => write!(f, "string/contains"),
            StringCompare => write!(f, "string/compare"),
            Seal => write!(f, "seal"),
            ContractArrayLazyApp => write!(f, "contract/array_lazy_apply"),
            ContractRecordLazyApp => write!(f, "contract/record_lazy_apply"),
            LabelWithMessage => write!(f, "label/with_message"),
            LabelWithNotes => write!(f, "label/with_notes"),
            LabelAppendNote => write!(f, "label/append_note"),
            LabelLookupTypeVar => write!(f, "label/lookup_type_variable"),
        }
    }
}

/// Primitive n-ary operators. Unary and binary operator make up for most of operators and are
/// hence special cased. `NAryOp` handles strict operations of arity greater than 2.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NAryOp {
    /// Replace a substring by another one in a string.
    StringReplace,

    /// Same as [`NAryOp::StringReplace`], but the pattern is interpreted as a regular expression.
    StringReplaceRegex,

    /// Return a substring of an original string.
    StringSubstr,

    /// The merge operator in contract mode (see [crate::eval::merge]). The arguments are in order
    /// the contract's label, the value to check, and the contract as a record.
    MergeContract,

    /// Seals one record into the tail of another. Used to ensure that functions using polymorphic
    /// record contracts do not violate parametricity.
    ///
    /// Takes four arguments:
    ///   - a sealing key, which must be provided later to unseal the tail,
    ///   - a label, which will be used to assign blame correctly tail access is
    ///     attempted,
    ///   - a record, which is the record we wish to seal the tail into,
    ///   - the record that we wish to seal.
    RecordSealTail,

    /// Unseals a term from the tail of a record and returns it.
    ///
    /// Takes three arguments:
    ///   - the sealing key, which was used to seal the tail,
    ///   - a label which will be used to assign blame correctly if
    ///     something goes wrong while unsealing,
    ///   - the record whose tail we wish to unseal.
    RecordUnsealTail,

    /// Insert type variable data into the `type_environment` of a [`crate::label::Label`]
    ///
    /// Takes four arguments:
    ///   - the sealing key assigned to the type variable
    ///   - the [introduction polarity](crate::label::Polarity) of the type variable
    ///   - the [kind](crate::typ::VarKind) of the type variable
    ///   - a label on which to operate
    LabelInsertTypeVar,

    /// Return a sub-array corresponding to a range. Given that Nickel uses array slices under the
    /// hood, as long as the array isn't modified later, this operation is constant in time and
    /// memory.
    ArraySlice,
}

impl NAryOp {
    pub fn arity(&self) -> usize {
        match self {
            NAryOp::StringReplace
            | NAryOp::StringReplaceRegex
            | NAryOp::StringSubstr
            | NAryOp::MergeContract
            | NAryOp::RecordUnsealTail
            | NAryOp::LabelInsertTypeVar
            | NAryOp::ArraySlice => 3,
            NAryOp::RecordSealTail => 4,
        }
    }
}

impl fmt::Display for NAryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use NAryOp::*;
        match self {
            StringReplace => write!(f, "string/replace"),
            StringReplaceRegex => write!(f, "string/replace_regex"),
            StringSubstr => write!(f, "string/substr"),
            MergeContract => write!(f, "record/merge_contract"),
            RecordSealTail => write!(f, "record/seal_tail"),
            RecordUnsealTail => write!(f, "record/unseal_tail"),
            LabelInsertTypeVar => write!(f, "label/insert_type_variable"),
            ArraySlice => write!(f, "array/slice"),
        }
    }
}

impl PrettyPrintCap for NickelValue {}

impl Traverse<NickelValue> for Term {
    /// Traverse through all expressions in the tree.
    ///
    /// This also recurses into the terms that are contained in `Type` subtrees.
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Term, E>
    where
        F: FnMut(NickelValue) -> Result<NickelValue, E>,
    {
        Ok(match self {
            Term::Fun(data) => Term::Fun(FunData {
                arg: data.arg,
                body: data.body.traverse(f, order)?,
            }),
            Term::FunPattern(mut data) => {
                data.body = data.body.traverse(f, order)?;
                Term::FunPattern(data)
            }
            Term::Let(mut data) => {
                data.bindings = data
                    .bindings
                    .into_iter()
                    .map(|(key, val)| Ok((key, val.traverse(f, order)?)))
                    .collect::<Result<_, E>>()?;
                data.body = data.body.traverse(f, order)?;

                Term::Let(data)
            }
            Term::LetPattern(mut data) => {
                data.bindings = data
                    .bindings
                    .into_iter()
                    .map(|(key, val)| Ok((key, val.traverse(f, order)?)))
                    .collect::<Result<_, E>>()?;
                data.body = data.body.traverse(f, order)?;

                Term::LetPattern(data)
            }
            Term::App(mut data) => {
                data.head = data.head.traverse(f, order)?;
                data.arg = data.arg.traverse(f, order)?;
                Term::App(data)
            }
            Term::Match(data) => {
                // The annotation on `map_res` use Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let branches: Result<Vec<MatchBranch>, E> = data
                    .branches
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,RichTerm), E>
                    .map(
                        |MatchBranch {
                             pattern,
                             guard,
                             body,
                         }| {
                            let guard = guard.map(|cond| cond.traverse(f, order)).transpose()?;
                            let body = body.traverse(f, order)?;

                            Ok(MatchBranch {
                                pattern,
                                guard,
                                body,
                            })
                        },
                    )
                    .collect();

                Term::Match(MatchData {
                    branches: branches?,
                })
            }
            Term::Op1(mut data) => {
                data.arg = data.arg.traverse(f, order)?;
                Term::Op1(data)
            }
            Term::Op2(mut data) => {
                data.arg1 = data.arg1.traverse(f, order)?;
                data.arg2 = data.arg2.traverse(f, order)?;
                Term::Op2(data)
            }
            Term::OpN(mut data) => {
                data.args = data
                    .args
                    .into_iter()
                    .map(|t| t.traverse(f, order))
                    .collect::<Result<Vec<NickelValue>, E>>()?;

                Term::OpN(data)
            }
            Term::Sealed(mut data) => {
                data.inner = data.inner.traverse(f, order)?;
                Term::Sealed(data)
            }
            Term::RecRecord(mut data) => {
                // The annotation on `map_res` uses Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let static_fields_res: Result<IndexMap<LocIdent, Field>, E> = data
                    .record
                    .fields
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,Field), E>
                    .map(|(id, field)| Ok((id, field.traverse(f, order)?)))
                    .collect();

                data.dyn_fields = data
                    .dyn_fields
                    .into_iter()
                    .map(|(id_t, field)| {
                        let id_t = id_t.traverse(f, order)?;
                        let field = field.traverse(f, order)?;

                        Ok((id_t, field))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                data.record = RecordData::new_shared_tail(
                    static_fields_res?,
                    data.record.attrs,
                    data.record.sealed_tail,
                );

                Term::RecRecord(data)
            }
            Term::StrChunks(chunks) => {
                let chunks_res: Result<Vec<StrChunk<NickelValue>>, E> = chunks
                    .into_iter()
                    .map(|chunk| match chunk {
                        chunk @ StrChunk::Literal(_) => Ok(chunk),
                        StrChunk::Expr(t, indent) => {
                            Ok(StrChunk::Expr(t.traverse(f, order)?, indent))
                        }
                    })
                    .collect();

                Term::StrChunks(chunks_res?)
            }
            Term::Annotated(mut data) => {
                data.annot = Rc::new(Rc::unwrap_or_clone(data.annot).traverse(f, order)?);
                data.inner = data.inner.traverse(f, order)?;
                Term::Annotated(data)
            }
            Term::Closurize(value) => {
                let value = value.traverse(f, order)?;
                Term::Closurize(value)
            }
            Term::Var(_)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::ParseError(_)
            | Term::RuntimeError(_) => self,
        })
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&NickelValue, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        match self {
            Term::Var(_)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::ParseError(_)
            | Term::RuntimeError(_) => None,
            Term::StrChunks(chunks) => chunks.iter().find_map(|ch| {
                if let StrChunk::Expr(term, _) = ch {
                    term.traverse_ref(f, state)
                } else {
                    None
                }
            }),
            Term::Op1(data) => data.arg.traverse_ref(f, state),
            Term::Sealed(data) => data.inner.traverse_ref(f, state),
            Term::Fun(FunData { arg: _, body: t }) | Term::Closurize(t) => t.traverse_ref(f, state),
            Term::FunPattern(data) => data.body.traverse_ref(f, state),
            Term::Let(data) => data
                .bindings
                .iter()
                .find_map(|(_id, t)| t.traverse_ref(f, state))
                .or_else(|| data.body.traverse_ref(f, state)),
            Term::LetPattern(data) => data
                .bindings
                .iter()
                .find_map(|(_pat, t)| t.traverse_ref(f, state))
                .or_else(|| data.body.traverse_ref(f, state)),
            Term::Op2(data) => data
                .arg1
                .traverse_ref(f, state)
                .or_else(|| data.arg2.traverse_ref(f, state)),
            Term::App(data) => data
                .head
                .traverse_ref(f, state)
                .or_else(|| data.arg.traverse_ref(f, state)),
            Term::RecRecord(data) => data
                .record
                .fields
                .values()
                .find_map(|field| field.traverse_ref(f, state))
                .or_else(|| {
                    data.dyn_fields.iter().find_map(|(id, field)| {
                        id.traverse_ref(f, state)
                            .or_else(|| field.traverse_ref(f, state))
                    })
                }),
            Term::Match(data) => data.branches.iter().find_map(
                |MatchBranch {
                     pattern: _,
                     guard,
                     body,
                 }| {
                    if let Some(cond) = guard.as_ref() {
                        cond.traverse_ref(f, state)?;
                    }

                    body.traverse_ref(f, state)
                },
            ),
            Term::OpN(data) => data.args.iter().find_map(|t| t.traverse_ref(f, state)),
            Term::Annotated(data) => data
                .inner
                .traverse_ref(f, state)
                .or_else(|| data.annot.traverse_ref(f, state)),
        }
    }
}

/// A helper for some reference counted term data that we expect to be unique (1-referenced
/// counted) and we need to map in place. This function make it possible to do it without
/// discarding the `Rc` allocation.
///
/// If the `Rc` isn't unique, this method still works but will allocate (in that case, it's better
/// to avoid using this function and rather clone the content and allocate a new `Rc` manually).
pub fn unique_map_in_place<T: Default + Clone>(rc: &mut Rc<T>, f: impl FnOnce(T) -> T) {
    let data_slot = Rc::make_mut(rc);
    let mapped = f(std::mem::take(data_slot));
    let _ = std::mem::replace(data_slot, mapped);
}

/// A fallible version of [unique_map_in_place].
pub fn fallible_unique_map_in_place<T: Default + Clone, E>(
    rc: &mut Rc<T>,
    f: impl FnOnce(T) -> Result<T, E>,
) -> Result<(), E> {
    let data_slot = Rc::make_mut(rc);
    let mapped = f(std::mem::take(data_slot))?;
    let _ = std::mem::replace(data_slot, mapped);
    Ok(())
}

impl_display_from_pretty!(Term);

#[macro_use]
/// Helpers to build [Term] objects as [values][crate::eval::value::NickelValue] from other
/// values.
pub mod make {
    use super::*;

    pub mod builder;

    /// Multi-ary application for types implementing `Into<NickelValue>`.
    #[macro_export]
    macro_rules! mk_app {
        ( $f:expr, $arg:expr) => {
            $crate::eval::value::NickelValue::from(
                $crate::term::Term::app(
                    $crate::eval::value::NickelValue::from($f),
                    $crate::eval::value::NickelValue::from($arg)
                )
            )
        };
        ( $f:expr, $fst:expr , $( $args:expr ),+ ) => {
            mk_app!(mk_app!($f, $fst), $( $args ),+)
        };
    }

    /// Multi-ary application for types implementing `Into<NickelValue>`.
    #[macro_export]
    macro_rules! mk_opn {
        ( $op:expr, $( $args:expr ),+) => {
            {
                let args = vec![$( $crate::eval::value::NickelValue::from($args) ),+];
                $crate::eval::value::NickelValue::from($crate::term::Term::OpN($op, args))
            }
        };
    }

    /// Multi argument function for types implementing `Into<Ident>` (for the identifiers), and
    /// `Into<NickelValue>` for the body.
    #[macro_export]
    macro_rules! mk_fun {
        ( $id:expr, $body:expr ) => {
            //MARKER
            $crate::eval::value::NickelValue::from(
                $crate::term::Term::fun(
                    $crate::identifier::LocIdent::from($id),
                    $crate::eval::value::NickelValue::from($body)
                )
            )
        };
        ( $id1:expr, $id2:expr , $( $rest:expr ),+ ) => {
            mk_fun!($crate::identifier::LocIdent::from($id1), mk_fun!($id2, $( $rest ),+))
        };
    }

    /// Multi field record for types implementing `Into<Ident>` (for the identifiers), and
    /// `Into<NickelValue>` for the fields. Identifiers and corresponding content are specified as a
    /// tuple: `mk_record!(("field1", t1), ("field2", t2))` corresponds to the record `{ field1 =
    /// t1; field2 = t2 }`.
    #[macro_export]
    macro_rules! mk_record {
        ( $( ($id:expr, $body:expr) ),* ) => {
            {
                let mut fields = indexmap::IndexMap::<$crate::identifier::LocIdent, $crate::eval::value::NickelValue>::new();
                $(
                    fields.insert($id.into(), $body.into());
                )*
                $crate::eval::value::NickelValue::record_posless(
                    $crate::term::record::RecordData::with_field_values(fields)
                )
            }
        };
    }

    /// Array for types implementing `Into<NickelValue>` (for elements). The array's attributes are a
    /// trailing (optional) `ArrayAttrs`, separated by a `;`. `mk_array!(Term::Num(42))` corresponds
    /// to `\[42\]`. Here the attributes are `ArrayAttrs::default()`, though the evaluated array may
    /// have different attributes.
    #[macro_export]
    macro_rules! mk_array {
        ( $( $terms:expr ),* ) => {
            {
                let ts : $crate::eval::value::Array =
                    [$( $crate::eval::value::NickelValue::from($terms) ),*]
                    .into_iter()
                    .collect();
                $crate::eval::value::NickelValue::array_posless(ts, Vec::new())
            }
        };
    }

    pub fn var<I>(v: I) -> NickelValue
    where
        I: Into<LocIdent>,
    {
        Term::Var(v.into()).into()
    }

    pub fn let_in<I, T1, T2, Iter>(rec: bool, bindings: Iter, t2: T2) -> NickelValue
    where
        T1: Into<NickelValue>,
        T2: Into<NickelValue>,
        I: Into<LocIdent>,
        Iter: IntoIterator<Item = (I, T1)>,
    {
        Term::let_in(
            bindings
                .into_iter()
                .map(|(id, t)| (id.into(), t.into()))
                .collect(),
            t2.into(),
            LetAttrs {
                binding_type: BindingType::Normal,
                rec,
            },
        )
        .into()
    }

    pub fn let_one_in<I, T1, T2>(id: I, t1: T1, t2: T2) -> NickelValue
    where
        T1: Into<NickelValue>,
        T2: Into<NickelValue>,
        I: Into<LocIdent>,
    {
        let_in(false, std::iter::once((id, t1)), t2)
    }

    pub fn let_one_rec_in<I, T1, T2>(id: I, t1: T1, t2: T2) -> NickelValue
    where
        T1: Into<NickelValue>,
        T2: Into<NickelValue>,
        I: Into<LocIdent>,
    {
        let_in(true, std::iter::once((id, t1)), t2)
    }

    pub fn let_one_pat<D, T1, T2>(pat: D, t1: T1, t2: T2) -> NickelValue
    where
        T1: Into<NickelValue>,
        T2: Into<NickelValue>,
        D: Into<Pattern>,
    {
        Term::let_pattern(
            std::iter::once((pat.into(), t1.into())).collect(),
            t2.into(),
            LetAttrs::default(),
        )
        .into()
    }

    pub fn let_pat_in<D, T1, T2, Iter>(rec: bool, bindings: Iter, body: T2) -> NickelValue
    where
        T1: Into<NickelValue>,
        T2: Into<NickelValue>,
        D: Into<Pattern>,
        Iter: IntoIterator<Item = (D, T1)>,
    {
        Term::let_pattern(
            bindings
                .into_iter()
                .map(|(pat, t)| (pat.into(), t.into()))
                .collect(),
            body.into(),
            LetAttrs {
                binding_type: BindingType::Normal,
                rec,
            },
        )
        .into()
    }

    pub fn if_then_else<T1, T2, T3>(cond: T1, t1: T2, t2: T3) -> NickelValue
    where
        T1: Into<NickelValue>,
        T2: Into<NickelValue>,
        T3: Into<NickelValue>,
    {
        mk_app!(
            Term::op1(UnaryOp::IfThenElse, cond.into()),
            t1.into(),
            t2.into()
        )
    }

    pub fn op1<T>(op: UnaryOp, t: T) -> NickelValue
    where
        T: Into<NickelValue>,
    {
        Term::op1(op, t.into()).into()
    }

    pub fn op2<T1, T2>(op: BinaryOp, t1: T1, t2: T2) -> NickelValue
    where
        T1: Into<NickelValue>,
        T2: Into<NickelValue>,
    {
        Term::op2(op, t1.into(), t2.into()).into()
    }

    pub fn opn<T>(op: NAryOp, args: Vec<T>) -> NickelValue
    where
        T: Into<NickelValue>,
    {
        Term::opn(op, args.into_iter().map(T::into).collect()).into()
    }

    pub fn apply_contract<T>(
        pos_table: &mut PosTable,
        typ: Type,
        l: Label,
        t: T,
    ) -> Result<NickelValue, UnboundTypeVariableError>
    where
        T: Into<NickelValue>,
    {
        Ok(mk_app!(
            op2(
                BinaryOp::ContractApply,
                typ.contract(pos_table)?,
                NickelValue::label_posless(l)
            ),
            t.into()
        ))
    }

    pub fn id() -> NickelValue {
        mk_fun!("x", var("x"))
    }

    pub fn import<S>(path: S, format: InputFormat) -> NickelValue
    where
        S: Into<OsString>,
    {
        Term::Import(Import::Path {
            path: path.into(),
            format,
        })
        .into()
    }

    pub fn integer(n: impl Into<i64>) -> NickelValue {
        NickelValue::number_posless(n.into())
    }

    pub fn static_access<I, S, T>(record: T, fields: I) -> NickelValue
    where
        I: IntoIterator<Item = S>,
        I::IntoIter: DoubleEndedIterator,
        S: Into<LocIdent>,
        T: Into<NickelValue>,
    {
        fields.into_iter().fold(record.into(), |value, field| {
            make::op1(UnaryOp::RecordAccess(field.into()), value)
        })
    }

    pub fn enum_variant<S, T>(tag: S, arg: T) -> NickelValue
    where
        S: Into<LocIdent>,
        T: Into<NickelValue>,
    {
        NickelValue::enum_variant_posless(tag.into(), Some(arg.into()))
    }

    pub fn custom_contract<T>(contract: T) -> NickelValue
    where
        T: Into<NickelValue>,
    {
        NickelValue::custom_contract_posless(contract.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        combine::Combine,
        label::Label,
        typ::{Type, TypeF},
    };

    #[test]
    fn make_static_access() {
        let t = make::op1(
            UnaryOp::RecordAccess("record".into()),
            make::op1(
                UnaryOp::RecordAccess("records".into()),
                make::var("predicates"),
            ),
        );
        assert_eq!(
            make::static_access(make::var("predicates"), ["records", "record"]),
            t
        );
    }

    #[test]
    fn contract_annotation_order() {
        let ty1 = LabeledType {
            typ: TypeF::Number.into(),
            label: Label::dummy(),
        };
        let annot1 = TypeAnnotation {
            typ: None,
            contracts: vec![ty1.clone()],
        };

        let ty2 = LabeledType {
            typ: TypeF::Bool.into(),
            label: Label::dummy(),
        };
        let annot2 = TypeAnnotation {
            typ: None,
            contracts: vec![ty2.clone()],
        };

        assert_eq!(Combine::combine(annot1, annot2).contracts, vec![ty1, ty2])
    }

    /// Regression test for issue [#548](https://github.com/tweag/nickel/issues/548)
    #[test]
    fn type_annotation_combine() {
        let inner = TypeAnnotation {
            typ: Some(LabeledType {
                typ: Type::from(TypeF::Number),
                label: Label::dummy(),
            }),
            ..Default::default()
        };
        let outer = TypeAnnotation::default();
        let res = TypeAnnotation::combine(outer, inner);
        assert_ne!(res.typ, None);
    }
}
