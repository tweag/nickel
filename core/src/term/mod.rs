//! AST of a Nickel expression.
//!
//! # Core language
//!
//! At its core, Nickel is a lazy JSON with higher-order functions. It includes:
//! - Basic values: booleans, numerals, string
//! - Data structures: arrays and records
//! - Binders: functions and let bindings
//!
//! It also features types and type annotations, and other typechecking or contracts-related
//! constructs (label, symbols, etc.).
pub mod array;
pub mod pattern;
pub mod record;
pub mod string;

use array::{Array, ArrayAttrs};
use pattern::Pattern;
use record::{Field, FieldDeps, FieldMetadata, Include, RecordData, RecordDeps};
use smallvec::SmallVec;
use string::NickelString;

use crate::{
    cache::InputFormat,
    combine::Combine,
    error::{EvalError, ParseError},
    eval::{cache::CacheIndex, contract_eq, Environment},
    files::FileId,
    identifier::{Ident, LocIdent},
    impl_display_from_pretty,
    label::{Label, MergeLabel},
    match_sharedterm,
    position::{RawSpan, TermPos},
    pretty::PrettyPrintCap,
    traverse::*,
    typ::{Type, UnboundTypeVariableError},
};

use crate::metrics::increment;

pub use malachite::{
    base::{
        num::{
            basic::traits::Zero,
            conversion::traits::{IsInteger, RoundingFrom, ToSci},
        },
        rounding_modes::RoundingMode,
    },
    rational::Rational,
    Integer,
};

use serde::{Deserialize, Serialize, Serializer};

// Because we use `IndexMap` for records, consumer of Nickel (as a library) might have to
// manipulate values of this type, so we re-export it.
pub use indexmap::IndexMap;

use std::{
    cmp::{Ordering, PartialOrd},
    convert::Infallible,
    ffi::OsString,
    fmt,
    ops::Deref,
    rc::Rc,
};

/// The payload of a `Term::ForeignId`.
pub type ForeignIdPayload = u64;

/// The AST of a Nickel expression.
///
/// Parsed terms also need to store their position in the source for error reporting.  This is why
/// this type is nested with [`RichTerm`].
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Term {
    /// The null value.
    Null,

    /// A boolean value.
    Bool(bool),

    /// A floating-point value.
    #[serde(serialize_with = "crate::serialize::serialize_num")]
    #[serde(deserialize_with = "crate::serialize::deserialize_num")]
    Num(Number),

    /// A literal string.
    Str(NickelString),

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
    Fun(LocIdent, RichTerm),

    /// A destructuring function.
    #[serde(skip)]
    FunPattern(Pattern, RichTerm),

    /// A blame label.
    #[serde(skip)]
    Lbl(Label),

    /// A let binding.
    #[serde(skip)]
    Let(SmallVec<[(LocIdent, RichTerm); 4]>, RichTerm, LetAttrs),

    /// A destructuring let-binding.
    #[serde(skip)]
    LetPattern(SmallVec<[(Pattern, RichTerm); 1]>, RichTerm, LetAttrs),

    /// An application.
    #[serde(skip)]
    App(RichTerm, RichTerm),

    /// A variable.
    #[serde(skip)]
    Var(LocIdent),

    /// An enum tag, or equivalently, an enum variant without any argument.
    Enum(LocIdent),
    /// An applied enum variant (an algebraic data type). In Nickel ADTs can have at most one
    /// argument: [Self::Enum] is the version with no argument, and [Self::EnumVariant] is the
    /// version with one argument. Note that one can just use a record to store multiple named
    /// values in the argument.
    #[serde(skip)]
    EnumVariant {
        tag: LocIdent,
        arg: RichTerm,
        attrs: EnumVariantAttrs,
    },

    /// A record, mapping identifiers to terms.
    #[serde(serialize_with = "crate::serialize::serialize_record")]
    #[serde(deserialize_with = "crate::serialize::deserialize_record")]
    Record(RecordData),

    /// A recursive record, where the fields can reference each others.
    #[serde(skip)]
    RecRecord(
        RecordData,
        Vec<Include>,           /* fields defined through `include` expressions */
        Vec<(RichTerm, Field)>, /* field whose name is defined by interpolation */
        Option<RecordDeps>, /* dependency tracking between fields. None before the free var pass */
    ),

    /// A match expression. Corresponds only to the cases: this expression is still to be applied
    /// to an argument to match on.
    #[serde(skip)]
    Match(MatchData),

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
    #[serde(skip)]
    Sealed(SealingKey, RichTerm, Label),

    /// A term with a type and/or contract annotation.
    #[serde(serialize_with = "crate::serialize::serialize_annotated_value")]
    #[serde(skip_deserializing)]
    Annotated(TypeAnnotation, RichTerm),

    /// An unresolved import.
    #[serde(skip)]
    Import(Import),

    /// A resolved import (which has already been loaded and parsed).
    #[serde(skip)]
    ResolvedImport(FileId),

    /// A type in term position, such as in `let my_contract = Number -> Number in ...`.
    ///
    /// During evaluation, this will get turned into a contract.
    #[serde(skip)]
    Type {
        /// The static type.
        typ: Type,
        /// The conversion of this type to a contract, that is, `typ.contract()?`. This field
        /// serves as a caching mechanism so we only run the contract generation code once per type
        /// written by the user.
        contract: RichTerm,
    },

    /// A custom contract. The content must be a function (or function-like terms like a match
    /// expression) of two arguments: a label and the value to be checked. In particular, it must
    /// be a weak-head normal form, and this invariant may be relied upon elsewhere in the
    /// codebase (although it's not the case at the time of writing, to the best of my knowledge).
    ///
    /// Having a separate node for custom contracts lets us leverage the additional information for
    /// example to implement a restricted `or` combinator on contracts, which needs to know which
    /// contracts support booleans operations (predicates and validators), or for better error
    /// messages in the future when parametric contracts aren't fully applied
    /// ([#1460](https://github.com/tweag/nickel/issues/1460)). In the future, the custom contract
    /// node might also include even more metadata.
    ///
    /// # Immediate and delayed parts
    ///
    /// Custom contracts usually have two parts, an immediate part and a delayed part.
    ///
    /// The immediate part is similar to a predicate or a validator: this is a function that takes a
    /// value and return either `'Ok` or `'Error {..}`. The immediate part gathers the checks that can
    /// be done eagerly, without forcing the value (the immediate part can actually force the value,
    /// but it's up to the implementer to decide - for builtin contracts, the immediate part never
    /// forces values)
    ///
    /// The delayed part is a partial identity which takes a label and the value and either blames or
    /// return the value with potential delayed checks buried inside.
    ///
    /// Note that this is a conceptual distinction. It did happen that we experimented with making
    /// this distinction explicit, with custom contracts being represented by two different
    /// functions, one for each part. But this proved to be cumbersome in many ways (both for us
    /// language developers and for users). Instead, we decided to make custom contracts just one
    /// function of type `Label -> Dyn -> [| 'Ok Dyn, 'Error {..} |]`, which gives enough
    /// information to extract the immediate and the delayed part anyway. The delayed part, if any,
    /// is embedded in the return value of the case `'Ok Dyn`, where the argument is the original
    /// value with the delayed checks inside.
    ///
    /// # Naked functions as custom contracts
    ///
    /// Nowadays, using dedicated constructors is the only documented way of creating custom
    /// contracts: `std.contract.custom`, `std.contract.from_validator`, etc. The requirement to
    /// use those dedicated constructors is unfortunately a breaking change (prior to Nickel 1.8)
    /// as custom contracts were written as naked functions before. Using naked functions is
    /// discouraged and will be deprecated in the future, but `%contract/apply%` still supports
    /// them.
    #[serde(skip)]
    CustomContract(RichTerm),

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
    /// behavior of `RuntimeError` behaves.
    #[serde(skip)]
    RuntimeError(EvalError),

    #[serde(skip)]
    /// A "pointer" (cache index, which can see as a kind of generic pointer to the memory managed
    /// by the evaluation cache) to a term together with its environment. Unfortunately, this is an
    /// evaluation object leaking into the AST: ideally, we would have one concrete syntax tree
    /// coming out of the parser, and a different representation for later stages, storing closures
    /// and whatnot.
    ///
    /// This is not the case yet, so in the meantime, we have to mix everything together. The
    /// ability to store closures directly in the AST without having to generate a variable and bind
    /// it in the environment is an important performance boost and we couldn't wait for the AST to
    /// be split to implement it.
    ///
    /// For all intent of purpose, you should consider `Closure` as a "inline" variable: before its
    /// introduction, it was encoded as a variable bound in the environment.
    ///
    /// This is a temporary solution, and will be removed in the future.
    Closure(CacheIndex),

    #[serde(skip)]
    /// An opaque value that cannot be constructed within Nickel code.
    ///
    /// This can be used by programs that embed Nickel, as they can inject these opaque
    /// values into the AST.
    ForeignId(ForeignIdPayload),
}

// PartialEq is mostly used for tests, when it's handy to compare something to an expected result.
// Most of the instances aren't really meaningful to use outside of very simple cases, and you
// should avoid comparing terms directly.
//
// We have to implement this instance by hand because of the `Closure` node.
impl PartialEq for Term {
    #[track_caller]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Num(l0), Self::Num(r0)) => l0 == r0,
            (Self::Str(l0), Self::Str(r0)) => l0 == r0,
            (Self::StrChunks(l0), Self::StrChunks(r0)) => l0 == r0,
            (Self::Fun(l0, l1), Self::Fun(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::FunPattern(l0, l1), Self::FunPattern(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Lbl(l0), Self::Lbl(r0)) => l0 == r0,
            (Self::Let(l0, l1, l2), Self::Let(r0, r1, r2)) => l0 == r0 && l1 == r1 && l2 == r2,
            (Self::LetPattern(l0, l1, l2), Self::LetPattern(r0, r1, r2)) => {
                l0 == r0 && l1 == r1 && l2 == r2
            }
            (Self::App(l0, l1), Self::App(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Var(l0), Self::Var(r0)) => l0 == r0,
            (Self::Enum(l0), Self::Enum(r0)) => l0 == r0,
            (Self::Record(l0), Self::Record(r0)) => l0 == r0,
            (Self::RecRecord(l0, l1, l2, l3), Self::RecRecord(r0, r1, r2, r3)) => {
                l0 == r0 && l1 == r1 && l2 == r2 && l3 == r3
            }
            (Self::Match(l_data), Self::Match(r_data)) => l_data == r_data,
            (Self::Array(l0, l1), Self::Array(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Op1(l0, l1), Self::Op1(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Op2(l0, l1, l2), Self::Op2(r0, r1, r2)) => l0 == r0 && l1 == r1 && l2 == r2,
            (Self::OpN(l0, l1), Self::OpN(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::SealingKey(l0), Self::SealingKey(r0)) => l0 == r0,
            (Self::Sealed(l0, l1, l2), Self::Sealed(r0, r1, r2)) => {
                l0 == r0 && l1 == r1 && l2 == r2
            }
            (Self::Annotated(l0, l1), Self::Annotated(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Import(l), Self::Import(r)) => l == r,
            (Self::ResolvedImport(l0), Self::ResolvedImport(r0)) => l0 == r0,
            (
                Self::Type {
                    typ: l0,
                    contract: l1,
                },
                Self::Type {
                    typ: r0,
                    contract: r1,
                },
            ) => l0 == r0 && l1 == r1,
            (Self::ParseError(l0), Self::ParseError(r0)) => l0 == r0,
            (Self::RuntimeError(l0), Self::RuntimeError(r0)) => l0 == r0,
            // We don't compare closure, because we can't, without the evaluation cache at hand.
            // It's ok even if the cache index are the same: we implement PartialEq, so we can have
            // `x != x`. In practice, this case shouldn't even be triggered, because tests usually
            // compare simple terms without closures in it (or terms where closures have
            // been substituted for their value).
            (Self::Closure(_l0), Self::Closure(_r0)) => false,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
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

/// The underlying type representing Nickel numbers. Currently, numbers are arbitrary precision
/// rationals.
///
/// Basic arithmetic operations are exact, without loss of precision (within the limits of available
/// memory).
///
/// Raising to a power that doesn't fit in a signed 64bits number will lead to converting both
/// operands to 64-bits floats, performing the floating-point power operation, and converting back
/// to rationals, which can incur a loss of precision.
///
/// [^number-serialization]: Conversion to string and serialization try to first convert the
///     rational as an exact signed or usigned 64-bits integer. If this succeeds, such operations
///     don't lose precision. Otherwise, the number is converted to the nearest 64bit float and then
///     serialized/printed, which can incur a loss of information.
pub type Number = Rational;

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

pub struct CustomContract(pub RichTerm);

/// A runtime representation of a contract, as a term and a label ready to be applied via
/// [BinaryOp::ContractApply].
#[derive(Debug, PartialEq, Clone)]
pub struct RuntimeContract {
    /// The pending contract, which can be a function, a type, a [CustomContract] or a record.
    pub contract: RichTerm,
    /// The blame label.
    pub label: Label,
}

impl RuntimeContract {
    pub fn new(contract: RichTerm, label: Label) -> Self {
        RuntimeContract { contract, label }
    }

    /// Generate a runtime contract from a type used as a static type annotation and a label. Use
    /// the guarantees of the static type system to optimize and simplify the contract.
    pub fn from_static_type(labeled_typ: LabeledType) -> Result<Self, UnboundTypeVariableError> {
        Ok(RuntimeContract {
            contract: labeled_typ.typ.contract_static()?,
            label: labeled_typ.label,
        })
    }

    /// Map a function over the term representing the underlying contract.
    pub fn map_contract<F>(self, f: F) -> Self
    where
        F: FnOnce(RichTerm) -> RichTerm,
    {
        RuntimeContract {
            contract: f(self.contract),
            ..self
        }
    }

    /// Apply this contract to a term.
    pub fn apply(self, rt: RichTerm, pos: TermPos) -> RichTerm {
        use crate::mk_app;

        mk_app!(
            make::op2(
                BinaryOp::ContractApply,
                self.contract,
                Term::Lbl(self.label)
            )
            .with_pos(pos),
            rt
        )
        .with_pos(pos)
    }

    /// Apply a series of contracts to a term, in order.
    pub fn apply_all<I>(rt: RichTerm, contracts: I, pos: TermPos) -> RichTerm
    where
        I: IntoIterator<Item = Self>,
    {
        contracts
            .into_iter()
            .fold(rt, |acc, ctr| ctr.apply(acc, pos))
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

impl Traverse<RichTerm> for RuntimeContract {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(RichTerm) -> Result<RichTerm, E>,
    {
        let contract = self.contract.traverse(f, order)?;
        Ok(RuntimeContract { contract, ..self })
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&RichTerm, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        self.contract.traverse_ref(f, state)
    }
}

impl std::convert::TryFrom<LabeledType> for RuntimeContract {
    type Error = UnboundTypeVariableError;

    fn try_from(labeled_ty: LabeledType) -> Result<Self, Self::Error> {
        Ok(RuntimeContract::new(
            labeled_ty.typ.contract()?,
            labeled_ty.label,
        ))
    }
}

/// The attributes of a enum variant.
#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct EnumVariantAttrs {
    /// An enum variant is closurized if its argument is a [crate::term::Term::Closure] or a
    /// constant.
    ///
    /// When initially produced by the parser, data structures such as enum variants or arrays
    /// aren't closurized. At the first evaluation, they will be turned into closurized versions,
    /// by allocating cache nodes (think thunks) for non constant elements. Once done, this flag is
    /// set to `true`.
    ///
    /// Ideally, we would have a different AST representation for evaluation, where enum variants
    /// would always be closurized. In the meantime, while we need to cope with a unique AST across
    /// the whole pipeline, we use this flag to remember closurization.
    pub closurized: bool,
}

impl EnumVariantAttrs {
    /// Create new enum variant attributes. By default, the `closurized` flag is set to `false`.
    pub fn new() -> Self {
        Default::default()
    }

    /// Set the `closurized` flag to `true`.
    pub fn closurized(mut self) -> Self {
        self.closurized = true;
        self
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
#[derive(Debug, Default, Clone)]
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

#[derive(Debug, Clone)]
pub enum MergePriority {
    /// The priority of default values that are overridden by everything else.
    Bottom,

    /// The priority by default, when no priority annotation (`default`, `force`, `priority`) is
    /// provided.
    ///
    /// Act as the value `MergePriority::Numeral(0)` with respect to ordering and equality
    /// testing. The only way to discriminate this variant is to pattern match on it.
    Neutral,

    /// A numeral priority.
    Numeral(Number),

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
                if p == &Number::ZERO =>
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
            (MergePriority::Neutral, MergePriority::Numeral(n)) => Number::ZERO.cmp(n),
            (MergePriority::Numeral(n), MergePriority::Neutral) => n.cmp(&Number::ZERO),
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
            MergePriority::Neutral => write!(f, "{}", Number::ZERO),
            MergePriority::Numeral(p) => write!(f, "{p}"),
            MergePriority::Top => write!(f, "force"),
        }
    }
}

impl Serialize for MergePriority {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

/// A branch of a match expression.
#[derive(Debug, PartialEq, Clone)]
pub struct MatchBranch {
    /// The pattern on the left hand side of `=>`.
    pub pattern: Pattern,
    /// A potential guard, which is an additional side-condition defined as `if cond`. The value
    /// stored in this field is the boolean condition itself.
    pub guard: Option<RichTerm>,
    /// The body of the branch, on the right hand side of `=>`.
    pub body: RichTerm,
}

/// Content of a match expression.
#[derive(Debug, PartialEq, Clone)]
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
                span,
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

impl Traverse<RichTerm> for LabeledType {
    // Note that this function doesn't traverse the label, which is most often what you want. The
    // terms that may hide in a label are mostly types used for error reporting, but are never
    // evaluated.
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<LabeledType, E>
    where
        F: FnMut(RichTerm) -> Result<RichTerm, E>,
    {
        let LabeledType { typ, label } = self;
        typ.traverse(f, order).map(|typ| LabeledType { typ, label })
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&RichTerm, &S) -> TraverseControl<S, U>,
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
    pub fn pending_contracts(&self) -> Result<Vec<RuntimeContract>, UnboundTypeVariableError> {
        self.contracts
            .iter()
            .cloned()
            .map(RuntimeContract::try_from)
            .collect::<Result<Vec<_>, _>>()
    }

    /// Build the contract derived from the static type annotation, applying the specific
    /// optimizations along the way.
    pub fn static_contract(&self) -> Option<Result<RuntimeContract, UnboundTypeVariableError>> {
        self.typ
            .as_ref()
            .cloned()
            .map(RuntimeContract::from_static_type)
    }

    /// Convert all the contracts of this annotation, including the potential type annotation as
    /// the first element, to a runtime representation. Apply contract optimizations to the static
    /// type annotation.
    pub fn all_contracts(&self) -> Result<Vec<RuntimeContract>, UnboundTypeVariableError> {
        self.typ
            .as_ref()
            .cloned()
            .map(RuntimeContract::from_static_type)
            .into_iter()
            .chain(
                self.contracts
                    .iter()
                    .cloned()
                    .map(RuntimeContract::try_from),
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

impl Traverse<RichTerm> for TypeAnnotation {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(RichTerm) -> Result<RichTerm, E>,
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
        f: &mut dyn FnMut(&RichTerm, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        self.contracts
            .iter()
            .find_map(|c| c.traverse_ref(f, state))
            .or_else(|| self.typ.as_ref().and_then(|t| t.traverse_ref(f, state)))
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

impl<E> StrChunk<E> {
    #[cfg(test)]
    pub fn expr(e: E) -> Self {
        StrChunk::Expr(e, 0)
    }

    pub fn try_chunks_as_static_str<'a, I>(chunks: I) -> Option<String>
    where
        I: IntoIterator<Item = &'a StrChunk<E>>,
        E: 'a,
    {
        chunks
            .into_iter()
            .try_fold(String::new(), |mut acc, next| match next {
                StrChunk::Literal(lit) => {
                    acc.push_str(lit);
                    Some(acc)
                }
                _ => None,
            })
    }
}

impl Term {
    /// Return the class of an expression in WHNF.
    ///
    /// The class of an expression is an approximation of its type used in error reporting. Class
    /// and type coincide for constants (numbers, strings and booleans) and arrays. Otherwise the
    /// class is less precise than the type and indicates the general shape of the term: `"Record"`
    /// for records, `"Fun`" for functions, etc. If the term is not a WHNF, `None` is returned.
    pub fn type_of(&self) -> Option<String> {
        match self {
            Term::Null => Some("Null".to_owned()),
            Term::Bool(_) => Some("Bool".to_owned()),
            Term::Num(_) => Some("Number".to_owned()),
            Term::Str(_) => Some("String".to_owned()),
            Term::Fun(_, _) | Term::FunPattern(_, _) => Some("Function".to_owned()),
            // We could print a separate type for predicates. For the time being, we just consider
            // it to be the function resulting of `$predicate_to_ctr pred`.
            Term::Match { .. } => Some("MatchExpression".to_owned()),
            Term::Lbl(_) => Some("Label".to_owned()),
            Term::Enum(_) => Some("EnumTag".to_owned()),
            Term::EnumVariant { .. } => Some("EnumVariant".to_owned()),
            Term::Record(..) | Term::RecRecord(..) => Some("Record".to_owned()),
            Term::Array(..) => Some("Array".to_owned()),
            Term::SealingKey(_) => Some("SealingKey".to_owned()),
            Term::Sealed(..) => Some("Sealed".to_owned()),
            Term::Annotated(..) => Some("Annotated".to_owned()),
            Term::Type { .. } => Some("Type".to_owned()),
            Term::ForeignId(_) => Some("ForeignId".to_owned()),
            Term::CustomContract(_) => Some("CustomContract".to_owned()),
            Term::Let(..)
            | Term::LetPattern(..)
            | Term::App(_, _)
            | Term::Var(_)
            | Term::Closure(_)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::OpN(..)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_)
            | Term::ParseError(_)
            | Term::RuntimeError(_) => None,
        }
    }

    /// Determine if a term is in evaluated form, called weak head normal form (WHNF). This test is
    /// purely syntactic, which has the non-obvious consequence that some terms might be in WHNF
    /// according to [Self::is_whnf] but might still be evaluated further.
    ///
    /// This is due to implementation details of the evaluation around closurization. The first
    /// time an array or a record is evaluated, it will be closurized - thunks will be allocated to
    /// store its elements and make them shareable. Thus, if `self` is `Term::Array(data, attrs)`
    /// with `attrs.closurized` set to `false`, evaluation will rewrite it to a different array,
    /// although in the surface language of Nickel, arrays are weak head normal forms.
    ///
    /// For everything happening pre-evaluation, you probably shouldn't care about this subtlety
    /// and you can use `is_whnf` directly.
    ///
    /// However, at run-time, in particular if the property you care about is "is this term going
    /// to be evaluate further", then you should use [Self::is_eff_whnf] instead.
    pub fn is_whnf(&self) -> bool {
        match self {
            Term::Null
            | Term::Bool(_)
            | Term::Num(_)
            | Term::Str(_)
            | Term::Fun(..)
            // match expressions are function
            | Term::Match {..}
            // Custom contracts are values, usually wrapping a function
            | Term::CustomContract(_)
            | Term::Lbl(_)
            | Term::Enum(_)
            | Term::EnumVariant {..}
            | Term::Record(..)
            | Term::Array(..)
            | Term::ForeignId(_)
            | Term::SealingKey(_)
            | Term::Type {..} => true,
            Term::Let(..)
            | Term::LetPattern(..)
            | Term::FunPattern(..)
            | Term::App(..)
            | Term::Var(_)
            | Term::Closure(_)
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

    /// Helper used by [Self::is_eff_whnf] to determine if a term is a data structure that hasn't
    /// been closurized yet.
    fn is_unclosurized_datastructure(&self) -> bool {
        match self {
            Term::Array(_, attrs) => !attrs.closurized,
            Term::Record(data) | Term::RecRecord(data, ..) => !data.attrs.closurized,
            Term::EnumVariant { attrs, .. } => !attrs.closurized,
            _ => false,
        }
    }

    /// Determine if an expression is an effective weak head normal form, that is a value that
    /// won't be evaluated further by the virtual machine. Being an effective WHNF implies being a
    /// WHNF, but the converse isn't true. See [Self::is_whnf] for more details.
    pub fn is_eff_whnf(&self) -> bool {
        self.is_whnf() && !self.is_unclosurized_datastructure()
    }

    /// Determine if a term is annotated.
    pub fn is_annotated(&self) -> bool {
        matches!(self, Term::Annotated(..))
    }

    /// Determine if a term is a constant.
    ///
    /// In this context, a constant is an atomic literal of the language: null, a boolean, a number,
    /// a string, a label, an enum tag or a symbol.
    pub fn is_constant(&self) -> bool {
        match self {
            Term::Null
            | Term::Bool(_)
            | Term::Num(_)
            | Term::Str(_)
            | Term::Lbl(_)
            | Term::Enum(_)
            | Term::ForeignId(_)
            | Term::SealingKey(_) => true,
            Term::Let(..)
            | Term::LetPattern(..)
            | Term::Record(..)
            | Term::Array(..)
            | Term::Fun(..)
            | Term::FunPattern(..)
            | Term::CustomContract(_)
            | Term::App(_, _)
            | Term::Match { .. }
            | Term::Var(_)
            | Term::Closure(_)
            | Term::Op1(..)
            | Term::Op2(..)
            | Term::OpN(..)
            | Term::Sealed(..)
            | Term::Annotated(..)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_)
            | Term::RecRecord(..)
            | Term::Type { .. }
            | Term::ParseError(_)
            | Term::EnumVariant { .. }
            | Term::RuntimeError(_) => false,
        }
    }

    /// Determine if a term is an atom of the surface syntax. Atoms are basic elements of the
    /// syntax that can freely substituted without being parenthesized.
    pub fn is_atom(&self) -> bool {
        match self {
            Term::Null
            | Term::Bool(..)
            | Term::Str(..)
            | Term::StrChunks(..)
            | Term::Lbl(..)
            | Term::Enum(..)
            | Term::Record(..)
            | Term::RecRecord(..)
            | Term::Array(..)
            | Term::Var(..)
            | Term::SealingKey(..)
            | Term::ForeignId(..)
            | Term::Op1(UnaryOp::RecordAccess(_), _)
            | Term::Op2(BinaryOp::RecordGet, _, _)
            // Those special cases aren't really atoms, but mustn't be parenthesized because they
            // are really functions taking additional non-strict arguments and printed as "partial"
            // infix operators.
            //
            // For example, `Op1(BoolOr, Var("x"))` is currently printed as `x ||`. Such operators
            // must never be parenthesized, such as in `(x ||)`.
            //
            // We might want a more robust mechanism for pretty printing such operators.
            | Term::Op1(UnaryOp::BoolAnd, _)
            | Term::Op1(UnaryOp::BoolOr, _) => true,
            // A number with a minus sign as a prefix isn't a proper atom
            Term::Num(n) if *n >= 0 => true,
            Term::Type {typ, contract: _} => typ.fmt_is_atom(),
            Term::Let(..)
            | Term::Num(..)
            | Term::EnumVariant {..}
            | Term::Match { .. }
            | Term::LetPattern(..)
            | Term::Fun(..)
            | Term::FunPattern(..)
            | Term::CustomContract(_)
            | Term::App(..)
            | Term::Op1(..)
            | Term::Op2(..)
            | Term::OpN(..)
            | Term::Sealed(..)
            | Term::Annotated(..)
            | Term::Import(_)
            | Term::ResolvedImport(..)
            | Term::Closure(_)
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

    /// Extract the cache index (thunk) from a closure. If `self` isn't a closure, `None` is
    /// returned.
    pub fn try_as_closure(&self) -> Option<CacheIndex> {
        match self {
            Term::Closure(idx) => Some(idx.clone()),
            _ => None,
        }
    }

    /// Converts a primitive value (number, string, boolean, enum tag or null) to a Nickel string,
    /// or returns `None` if the term isn't primitive.
    pub fn to_nickel_string(&self) -> Option<NickelString> {
        match self {
            Term::Num(n) => Some(format!("{}", n.to_sci()).into()),
            Term::Str(s) => Some(s.clone()),
            Term::Bool(b) => Some(b.to_string().into()),
            Term::Enum(id) => Some((*id).into()),
            Term::Null => Some("null".into()),
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

impl From<Term> for SharedTerm {
    fn from(t: Term) -> Self {
        SharedTerm::new(t)
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

    /// Creates an "empty" record with the sealed tail of its [`Term::Record`] argument.
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

    /// Take a record representing bindings to be added to the local environment and proceed to
    /// evaluate a pattern branch given as a second argument (which isn't a proper primop argument
    /// but is stored on the stack) in its environment augmented with the bindings.
    ///
    /// [Self::PatternBranch] isn't specific to pattern branches: what it does is to take a set of
    /// extra bindings and a term, and run the term in the augmented environment. While it could
    /// useful to implement other operations, it would be fragile as a generic `with_env` operator,
    /// because the term to be run must not be burried into a closure, or the environment
    /// augmentation would be shallow and have no effect on the actual content of the term (we have
    /// the same kind of constraints when updating record fields with the recursive environment of
    /// a record, for example). This is why the name tries to make it clear that it shouldn't be
    /// used blindly for something else.
    PatternBranch,

    /// Wrap a contract implementation as a [CustomContract]. You can think of this primop as a
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

            PatternBranch => write!(f, "pattern_branch"),
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

/// A flavor for record operations. By design, we want empty optional values to be transparent for
/// record operations, because they would otherwise make many operations fail spuriously (e.g.
/// trying to map over such an empty value). So they are most of the time silently ignored.
///
/// However, it's sometimes useful and even necessary to take them into account. This behavior is
/// controlled by [RecordOpKind].
#[derive(Clone, Debug, PartialEq, Eq, Copy, Default)]
pub enum RecordOpKind {
    #[default]
    IgnoreEmptyOpt,
    ConsiderAllFields,
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
        metadata: FieldMetadata,
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
    /// environment of a [label](Term::Lbl)
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
    ///   - a [sealing key](Term::SealingKey), which must be provided later to unseal the tail,
    ///   - a [label](Term::Lbl), which will be used to assign blame correctly tail access is
    ///     attempted,
    ///   - a [record](Term::Record), which is the record we wish to seal the tail into,
    ///   - the [record](Term::Record) that we wish to seal.
    RecordSealTail,

    /// Unseals a term from the tail of a record and returns it.
    ///
    /// Takes three arguments:
    ///   - the [sealing key](Term::SealingKey), which was used to seal the tail,
    ///   - a [label](Term::Lbl) which will be used to assign blame correctly if
    ///     something goes wrong while unsealing,
    ///   - the [record](Term::Record) whose tail we wish to unseal.
    RecordUnsealTail,

    /// Insert type variable data into the `type_environment` of a [`crate::label::Label`]
    ///
    /// Takes four arguments:
    ///   - the [sealing key](Term::SealingKey) assigned to the type variable
    ///   - the [introduction polarity](crate::label::Polarity) of the type variable
    ///   - the [kind](crate::typ::VarKind) of the type variable
    ///   - a [label](Term::Lbl) on which to operate
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

    /// Erase recursively (most of) the positional information.
    ///
    /// It allows to use rust `Eq` trait to compare the values of the underlying terms.
    ///
    /// This is currently only used in test code, but because it's used from integration tests
    /// which are located in their own separate crate, we cannot hide it behind cfg(test).
    ///
    /// Note that `Ident`s retain their position. This position is ignored in comparison, so it's
    /// good enough for the tests.
    pub fn without_pos(self) -> Self {
        self.traverse(
            &mut |t: Type| -> Result<_, Infallible> {
                Ok(Type {
                    pos: TermPos::None,
                    ..t
                })
            },
            TraverseOrder::BottomUp,
        )
        .unwrap()
        .traverse(
            &mut |t: RichTerm| -> Result<_, Infallible> {
                Ok(RichTerm {
                    pos: TermPos::None,
                    ..t
                })
            },
            TraverseOrder::BottomUp,
        )
        .unwrap()
    }

    /// Set the position and return the term updated.
    pub fn with_pos(mut self, pos: TermPos) -> Self {
        self.pos = pos;
        self
    }
}

impl PrettyPrintCap for RichTerm {}

impl Traverse<RichTerm> for RichTerm {
    /// Traverse through all `RichTerm`s in the tree.
    ///
    /// This also recurses into the terms that are contained in `Type` subtrees.
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<RichTerm, E>
    where
        F: FnMut(RichTerm) -> Result<RichTerm, E>,
    {
        let rt = match order {
            TraverseOrder::TopDown => f(self)?,
            TraverseOrder::BottomUp => self,
        };
        let pos = rt.pos;

        let result = match_sharedterm!(match (rt.term) {
            Term::Fun(id, t) => {
                let t = t.traverse(f, order)?;
                RichTerm::new(Term::Fun(id, t), pos)
            }
            Term::FunPattern(pat, t) => {
                let t = t.traverse(f, order)?;
                RichTerm::new(Term::FunPattern(pat, t), pos)
            }
            Term::CustomContract(t) => {
                let t = t.traverse(f, order)?;
                RichTerm::new(Term::CustomContract(t), pos)
            }
            Term::Let(bindings, body, attrs) => {
                let bindings = bindings
                    .into_iter()
                    .map(|(key, val)| Ok((key, val.traverse(f, order)?)))
                    .collect::<Result<_, E>>()?;
                let body = body.traverse(f, order)?;
                RichTerm::new(Term::Let(bindings, body, attrs), pos)
            }
            Term::LetPattern(bindings, body, attrs) => {
                let bindings = bindings
                    .into_iter()
                    .map(|(key, val)| Ok((key, val.traverse(f, order)?)))
                    .collect::<Result<_, E>>()?;
                let body = body.traverse(f, order)?;
                RichTerm::new(Term::LetPattern(bindings, body, attrs), pos)
            }
            Term::App(t1, t2) => {
                let t1 = t1.traverse(f, order)?;
                let t2 = t2.traverse(f, order)?;
                RichTerm::new(Term::App(t1, t2), pos)
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

                RichTerm::new(
                    Term::Match(MatchData {
                        branches: branches?,
                    }),
                    pos,
                )
            }
            Term::Op1(op, t) => {
                let t = t.traverse(f, order)?;
                RichTerm::new(Term::Op1(op, t), pos)
            }
            Term::Op2(op, t1, t2) => {
                let t1 = t1.traverse(f, order)?;
                let t2 = t2.traverse(f, order)?;
                RichTerm::new(Term::Op2(op, t1, t2), pos)
            }
            Term::OpN(op, ts) => {
                let ts_res: Result<Vec<RichTerm>, E> =
                    ts.into_iter().map(|t| t.traverse(f, order)).collect();
                RichTerm::new(Term::OpN(op, ts_res?), pos)
            }
            Term::Sealed(i, t1, lbl) => {
                let t1 = t1.traverse(f, order)?;
                RichTerm::new(Term::Sealed(i, t1, lbl), pos)
            }
            Term::Record(record) => {
                // The annotation on `fields_res` uses Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let fields_res: Result<IndexMap<LocIdent, Field>, E> = record
                    .fields
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,RichTerm), E>
                    .map(|(id, field)| Ok((id, field.traverse(f, order)?)))
                    .collect();
                RichTerm::new(
                    Term::Record(RecordData::new(
                        fields_res?,
                        record.attrs,
                        record.sealed_tail,
                    )),
                    pos,
                )
            }
            Term::RecRecord(record, includes, dyn_fields, deps) => {
                // The annotation on `map_res` uses Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let static_fields_res: Result<IndexMap<LocIdent, Field>, E> = record
                    .fields
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,Field), E>
                    .map(|(id, field)| Ok((id, field.traverse(f, order)?)))
                    .collect();
                let dyn_fields_res: Result<Vec<(RichTerm, Field)>, E> = dyn_fields
                    .into_iter()
                    .map(|(id_t, field)| {
                        let id_t = id_t.traverse(f, order)?;
                        let field = field.traverse(f, order)?;

                        Ok((id_t, field))
                    })
                    .collect();
                RichTerm::new(
                    Term::RecRecord(
                        RecordData::new(static_fields_res?, record.attrs, record.sealed_tail),
                        includes,
                        dyn_fields_res?,
                        deps,
                    ),
                    pos,
                )
            }
            Term::Array(ts, attrs) => {
                let ts_res = ts
                    .into_iter()
                    .map(|t| t.traverse(f, order))
                    .collect::<Result<Array, _>>()?;

                RichTerm::new(Term::Array(ts_res, attrs), pos)
            }
            Term::StrChunks(chunks) => {
                let chunks_res: Result<Vec<StrChunk<RichTerm>>, E> = chunks
                    .into_iter()
                    .map(|chunk| match chunk {
                        chunk @ StrChunk::Literal(_) => Ok(chunk),
                        StrChunk::Expr(t, indent) => {
                            Ok(StrChunk::Expr(t.traverse(f, order)?, indent))
                        }
                    })
                    .collect();

                RichTerm::new(Term::StrChunks(chunks_res?), pos)
            }
            Term::Annotated(annot, term) => {
                let annot = annot.traverse(f, order)?;
                let term = term.traverse(f, order)?;
                RichTerm::new(Term::Annotated(annot, term), pos)
            }
            Term::Type { typ, contract } => {
                let typ = typ.traverse(f, order)?;
                let contract = contract.traverse(f, order)?;

                RichTerm::new(Term::Type { typ, contract }, pos)
            }
            Term::EnumVariant { tag, arg, attrs } => {
                let arg = arg.traverse(f, order)?;
                RichTerm::new(Term::EnumVariant { tag, attrs, arg }, pos)
            }
            _ => rt,
        });

        match order {
            TraverseOrder::TopDown => Ok(result),
            TraverseOrder::BottomUp => f(result),
        }
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&RichTerm, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        let child_state = match f(self, state) {
            TraverseControl::Continue => None,
            TraverseControl::ContinueWithScope(s) => Some(s),
            TraverseControl::SkipBranch => {
                return None;
            }
            TraverseControl::Return(ret) => {
                return Some(ret);
            }
        };
        let state = child_state.as_ref().unwrap_or(state);

        match &*self.term {
            Term::Null
            | Term::Bool(_)
            | Term::Num(_)
            | Term::Str(_)
            | Term::Lbl(_)
            | Term::Var(_)
            | Term::Closure(_)
            | Term::Enum(_)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::SealingKey(_)
            | Term::ForeignId(_)
            | Term::ParseError(_)
            | Term::RuntimeError(_) => None,
            Term::StrChunks(chunks) => chunks.iter().find_map(|ch| {
                if let StrChunk::Expr(term, _) = ch {
                    term.traverse_ref(f, state)
                } else {
                    None
                }
            }),
            Term::Fun(_, t)
            | Term::FunPattern(_, t)
            | Term::EnumVariant { arg: t, .. }
            | Term::Op1(_, t)
            | Term::Sealed(_, t, _)
            | Term::CustomContract(t) => t.traverse_ref(f, state),
            Term::Let(bindings, body, _) => bindings
                .iter()
                .find_map(|(_id, t)| t.traverse_ref(f, state))
                .or_else(|| body.traverse_ref(f, state)),
            Term::LetPattern(bindings, body, _) => bindings
                .iter()
                .find_map(|(_pat, t)| t.traverse_ref(f, state))
                .or_else(|| body.traverse_ref(f, state)),
            Term::App(t1, t2) | Term::Op2(_, t1, t2) => t1
                .traverse_ref(f, state)
                .or_else(|| t2.traverse_ref(f, state)),
            Term::Record(data) => data
                .fields
                .values()
                .find_map(|field| field.traverse_ref(f, state)),
            Term::RecRecord(data, _, dyn_data, _) => data
                .fields
                .values()
                .find_map(|field| field.traverse_ref(f, state))
                .or_else(|| {
                    dyn_data.iter().find_map(|(id, field)| {
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
            Term::Array(ts, _) => ts.iter().find_map(|t| t.traverse_ref(f, state)),
            Term::OpN(_, ts) => ts.iter().find_map(|t| t.traverse_ref(f, state)),
            Term::Annotated(annot, t) => t
                .traverse_ref(f, state)
                .or_else(|| annot.traverse_ref(f, state)),
            Term::Type { typ, contract } => {
                typ.traverse_ref(f, state)?;
                contract.traverse_ref(f, state)
            }
        }
    }
}

impl Traverse<Type> for RichTerm {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<RichTerm, E>
    where
        F: FnMut(Type) -> Result<Type, E>,
    {
        self.traverse(
            &mut |rt: RichTerm| {
                match_sharedterm!(match (rt.term) {
                    Term::Type { typ, contract } => {
                        let typ = typ.traverse(f, order)?;
                        Ok(RichTerm::new(Term::Type { typ, contract }, rt.pos))
                    }
                    _ => Ok(rt),
                })
            },
            order,
        )
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&Type, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        self.traverse_ref(
            &mut |rt: &RichTerm, state: &S| match &*rt.term {
                Term::Type { typ, contract: _ } => typ.traverse_ref(f, state).into(),
                _ => TraverseControl::Continue,
            },
            state,
        )
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

impl_display_from_pretty!(RichTerm);
impl_display_from_pretty!(Term);

/// Allows to match on SharedTerm without taking ownership of the matched part
/// until the match. In the wildcard pattern, we don't take ownership, so we can
/// still use the richterm at that point.
///
/// It is used somehow as a match statement, going from
/// ```
/// # use nickel_lang_core::term::{RichTerm, Term};
/// let rt = RichTerm::from(Term::Bool(true));
///
/// match rt.term.into_owned() {
///     Term::Bool(x) => usize::from(x),
///     Term::Str(s) => s.len(),
///     _ => 42,
/// };
/// ```
/// to
/// ```
/// # use nickel_lang_core::term::{RichTerm, Term};
/// # use nickel_lang_core::match_sharedterm;
/// let rt = RichTerm::from(Term::Bool(true));
///
/// match_sharedterm!(match (rt.term) {
///         Term::Bool(x) => usize::from(x),
///         Term::Str(s) => s.len(),
///         _ => 42,
///     }
/// );
/// ```
///
/// Unlike a regular match statement, the expression being matched on must be
/// surrounded in parentheses
///
/// Known limitation: cannot use a `mut` inside the patterns.
#[macro_export]
macro_rules! match_sharedterm {
    (
        match ($st: expr) {
            $(%PROCESSED% $($pat: pat_param)|+ $(if $if_expr: expr)? => $expr: expr,)+
            _ => $else_expr: expr $(,)?
        }
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
            _ => $else_expr
        }
    };


    // recurse through the match arms prepending %PROCESSED% for two reasons:
    // 1. to standardize match arms with trailing commas on <pattern> => { <body> }
    // 2. so there's no ambiguity between a normal match arm and the final _ => <body>
    (
        match ($st: expr) {
            $(%PROCESSED% $($pat1: pat_param)|+ $(if $if_expr1: expr)? => $expr1: expr,)*
            $($pat2: pat_param)|+ $(if $if_expr2: expr)? => $expr2: expr,
            $($rest:tt)*
        }
    ) => {
        match_sharedterm!(match ($st) {
            $(%PROCESSED% $($pat1)|+ $(if $if_expr1)? => $expr1,)*
            %PROCESSED% $($pat2)|+ $(if $if_expr2)? => $expr2,
            $($rest)*
        })
    };
    (
        match ($st: expr) {
            $(%PROCESSED% $($pat1: pat_param)|+ $(if $if_expr1: expr)? => $expr1: expr,)*
            $($pat2: pat_param)|+ $(if $if_expr2: expr)? => $expr2: block
            $($rest:tt)*
        }
    ) => {
        match_sharedterm!(match ($st) {
            $(%PROCESSED% $($pat1)|+ $(if $if_expr1)? => $expr1,)*
            %PROCESSED% $($pat2)|+ $(if $if_expr2)? => $expr2,
            $($rest)*
        })
    };

    // throw nice error messages for common mistakes
    (
        match ($st: expr) {
            $(%PROCESSED% $($pat: pat_param)|+ $(if $if_expr: expr)? => $expr: expr,)+
        }
    ) => {
        compile_error!("`match_sharedterm!` used without a final wildcard match arm. You can just match on `shared_term.into_owned()`")
    };
    (
        match ($st: expr) {
            _ => $else_expr: expr $(,)?
        }
    ) => {
        compile_error!("`match_sharedterm!` used with only a wildcard match arm. You can just unconditionally execute that arm")
    };
}

#[macro_use]
/// Helpers to build `RichTerm` objects.
pub mod make {
    use super::*;

    pub mod builder;

    /// Multi-ary application for types implementing `Into<RichTerm>`.
    #[macro_export]
    macro_rules! mk_app {
        ( $f:expr, $arg:expr) => {
            $crate::term::RichTerm::from(
                $crate::term::Term::App(
                    $crate::term::RichTerm::from($f),
                    $crate::term::RichTerm::from($arg)
                )
            )
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
            $crate::term::RichTerm::from(
                $crate::term::Term::Fun(
                    $crate::identifier::LocIdent::from($id),
                    $crate::term::RichTerm::from($body)
                )
            )
        };
        ( $id1:expr, $id2:expr , $( $rest:expr ),+ ) => {
            mk_fun!($crate::identifier::LocIdent::from($id1), mk_fun!($id2, $( $rest ),+))
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
                let mut fields = indexmap::IndexMap::<LocIdent, RichTerm>::new();
                $(
                    fields.insert($id.into(), $body.into());
                )*
                $crate::term::RichTerm::from(
                    $crate::term::Term::Record(
                        $crate::term::record::RecordData::with_field_values(fields)
                    )
                )
            }
        };
    }

    /// Array for types implementing `Into<RichTerm>` (for elements). The array's attributes are a
    /// trailing (optional) `ArrayAttrs`, separated by a `;`. `mk_array!(Term::Num(42))` corresponds
    /// to `\[42\]`. Here the attributes are `ArrayAttrs::default()`, though the evaluated array may
    /// have different attributes.
    #[macro_export]
    macro_rules! mk_array {
        ( $( $terms:expr ),* ; $attrs:expr ) => {
            {
                let ts = $crate::term::array::Array::new(
                    [$( $crate::term::RichTerm::from($terms) ),*]
                );
                $crate::term::RichTerm::from($crate::term::Term::Array(ts, $attrs))
            }
        };
        ( $( $terms:expr ),* ) => {
            {
                let ts = [$( $crate::term::RichTerm::from($terms) ),*].into_iter().collect();
                $crate::term::RichTerm::from(Term::Array(ts, ArrayAttrs::default()))
            }
        };
    }

    pub fn var<I>(v: I) -> RichTerm
    where
        I: Into<LocIdent>,
    {
        Term::Var(v.into()).into()
    }

    pub fn let_in<I, T1, T2, Iter>(rec: bool, bindings: Iter, t2: T2) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
        I: Into<LocIdent>,
        Iter: IntoIterator<Item = (I, T1)>,
    {
        let attrs = LetAttrs {
            binding_type: BindingType::Normal,
            rec,
        };
        Term::Let(
            bindings
                .into_iter()
                .map(|(id, t)| (id.into(), t.into()))
                .collect(),
            t2.into(),
            attrs,
        )
        .into()
    }

    pub fn let_one_in<I, T1, T2>(id: I, t1: T1, t2: T2) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
        I: Into<LocIdent>,
    {
        let_in(false, std::iter::once((id, t1)), t2)
    }

    pub fn let_one_rec_in<I, T1, T2>(id: I, t1: T1, t2: T2) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
        I: Into<LocIdent>,
    {
        let_in(true, std::iter::once((id, t1)), t2)
    }

    pub fn let_one_pat<D, T1, T2>(pat: D, t1: T1, t2: T2) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
        D: Into<Pattern>,
    {
        Term::LetPattern(
            std::iter::once((pat.into(), t1.into())).collect(),
            t2.into(),
            LetAttrs::default(),
        )
        .into()
    }

    pub fn let_pat_in<D, T1, T2, Iter>(rec: bool, bindings: Iter, body: T2) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
        D: Into<Pattern>,
        Iter: IntoIterator<Item = (D, T1)>,
    {
        let attrs = LetAttrs {
            binding_type: BindingType::Normal,
            rec,
        };
        Term::LetPattern(
            bindings
                .into_iter()
                .map(|(pat, t)| (pat.into(), t.into()))
                .collect(),
            body.into(),
            attrs,
        )
        .into()
    }

    pub fn if_then_else<T1, T2, T3>(cond: T1, t1: T2, t2: T3) -> RichTerm
    where
        T1: Into<RichTerm>,
        T2: Into<RichTerm>,
        T3: Into<RichTerm>,
    {
        mk_app!(
            Term::Op1(UnaryOp::IfThenElse, cond.into()),
            t1.into(),
            t2.into()
        )
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

    pub fn apply_contract<T>(
        typ: Type,
        l: Label,
        t: T,
    ) -> Result<RichTerm, UnboundTypeVariableError>
    where
        T: Into<RichTerm>,
    {
        Ok(mk_app!(
            op2(BinaryOp::ContractApply, typ.contract()?, Term::Lbl(l)),
            t.into()
        ))
    }

    pub fn string<S>(s: S) -> RichTerm
    where
        S: Into<NickelString>,
    {
        Term::Str(s.into()).into()
    }

    pub fn id() -> RichTerm {
        mk_fun!("x", var("x"))
    }

    pub fn import<S>(path: S, format: InputFormat) -> RichTerm
    where
        S: Into<OsString>,
    {
        Term::Import(Import::Path {
            path: path.into(),
            format,
        })
        .into()
    }

    pub fn integer(n: impl Into<i64>) -> RichTerm {
        Term::Num(Number::from(n.into())).into()
    }

    pub fn static_access<I, S, T>(record: T, fields: I) -> RichTerm
    where
        I: IntoIterator<Item = S>,
        I::IntoIter: DoubleEndedIterator,
        S: Into<LocIdent>,
        T: Into<RichTerm>,
    {
        let mut term = record.into();
        for f in fields.into_iter() {
            term = make::op1(UnaryOp::RecordAccess(f.into()), term);
        }
        term
    }

    pub fn enum_variant<S, T>(tag: S, arg: T) -> RichTerm
    where
        S: Into<LocIdent>,
        T: Into<RichTerm>,
    {
        Term::EnumVariant {
            tag: tag.into(),
            arg: arg.into(),
            attrs: Default::default(),
        }
        .into()
    }

    pub fn custom_contract<T>(contract: T) -> RichTerm
    where
        T: Into<RichTerm>,
    {
        Term::CustomContract(contract.into()).into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
