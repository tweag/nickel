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
use array::Array;

use crate::{
    destruct::Destruct,
    error::ParseError,
    eval::EvalMode,
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
    collections::{HashMap, HashSet},
    ffi::OsString,
    fmt,
    ops::Deref,
    rc::Rc,
};

use self::record::RecordData;

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
    FunPattern(Option<Ident>, Destruct, RichTerm),
    /// A blame label.
    #[serde(skip)]
    Lbl(Label),

    /// A let binding.
    #[serde(skip)]
    Let(Ident, RichTerm, RichTerm, LetAttrs),
    /// A destructuring let-binding.
    #[serde(skip)]
    LetPattern(Option<Ident>, Destruct, RichTerm, RichTerm),
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
        Vec<(RichTerm, RichTerm)>, /* field whose name is defined by interpolation */
        Option<RecordDeps>, /* dependency tracking between fields. None before the free var pass */
    ),
    /// A switch construct. The evaluation is done by the corresponding unary operator, but we
    /// still need this one for typechecking.
    #[serde(skip)]
    Switch(
        RichTerm,                 /* tested expression */
        HashMap<Ident, RichTerm>, /* cases */
        Option<RichTerm>,         /* default */
    ),

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

    #[serde(serialize_with = "crate::serialize::serialize_meta_value")]
    #[serde(skip_deserializing)]
    MetaValue(MetaValue),

    /// An unresolved import.
    #[serde(skip)]
    Import(OsString),
    /// A resolved import (which has already been loaded and parsed).
    #[serde(skip)]
    ResolvedImport(FileId),
    #[serde(skip)]
    ParseError(ParseError),
}

pub type SealingKey = i32;

impl From<MetaValue> for Term {
    fn from(m: MetaValue) -> Self {
        Term::MetaValue(m)
    }
}

/// Type of let-binding. This only affects run-time behavior. Revertible bindings introduce
/// revertible thunks at evaluation, which are devices used for the implementation of recursive
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
}

pub mod array {
    use std::mem::transmute;
    use std::mem::ManuallyDrop;

    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    pub struct Array {
        inner: Rc<[RichTerm]>,
        start: usize,
        end: usize,
    }

    impl Array {
        /// Creates a Nickel array from reference-counted slice.
        pub fn new(inner: Rc<[RichTerm]>) -> Self {
            let start = 0;
            let end = inner.len();

            Self { inner, start, end }
        }

        /// Returns the effective length of the array.
        pub fn len(&self) -> usize {
            self.end - self.start
        }

        /// Returns `true` if the array is empty.
        pub fn is_empty(&self) -> bool {
            self.end == self.start
        }

        /// Returns a reference to the term at the given index.
        pub fn get(&self, idx: usize) -> Option<&RichTerm> {
            self.inner.get(self.start + idx)
        }

        /// Discards the first `diff` terms of the array.
        pub fn advance_by(mut self, diff: usize) -> Self {
            self.start += usize::min(diff, self.len());
            self
        }

        /// Makes a mutable slice into the given `Array`.
        pub fn make_mut(&mut self) -> &mut [RichTerm] {
            // NOTE: trying to use `Rc::make_mut` will result in the following compiler error:
            // > the trait `Clone` is not implemented for `[term::RichTerm]`
            // This seems to be an edge-case in the standard library.
            // As a workaround, if `get_mut` fails we recollect the array into a new `Rc` by cloning all terms.
            // Thus we make sure that the second call to `get_mut` won't fail.

            // This condition is the same as `!Rc::is_unique(&mut self.inner)`, but that function
            // is not public.
            if Rc::strong_count(&self.inner) != 1 || Rc::weak_count(&self.inner) != 0 {
                self.inner = self.iter().cloned().collect::<Rc<[_]>>();
            }

            Rc::get_mut(&mut self.inner).expect("non-unique Rc after deep-cloning Array")
        }

        /// Returns an iterator of references over the array.
        pub fn iter(&self) -> std::slice::Iter<'_, RichTerm> {
            self.as_ref().iter()
        }
    }

    impl Default for Array {
        fn default() -> Self {
            Self::new(Rc::new([]))
        }
    }

    impl FromIterator<RichTerm> for Array {
        fn from_iter<T: IntoIterator<Item = RichTerm>>(iter: T) -> Self {
            let inner = iter.into_iter().collect::<Rc<[_]>>();
            let start = 0;
            let end = inner.len();

            Self { inner, start, end }
        }
    }

    impl AsRef<[RichTerm]> for Array {
        fn as_ref(&self) -> &[RichTerm] {
            &self.inner[self.start..self.end]
        }
    }

    impl IntoIterator for Array {
        type Item = RichTerm;

        type IntoIter = IntoIter;

        fn into_iter(self) -> Self::IntoIter {
            // SAFETY: If there are no other strong or weak references
            // to the inner slice, then we can:
            // - Drop the elements outside our inner view,
            // - Move out the elements inside out inner view.
            // Otherwise, we clone everything.

            unsafe {
                let mut inner: Rc<[ManuallyDrop<RichTerm>]> = transmute(self.inner);
                let idx = self.start;
                let end = self.end;

                if let Some(slice) = Rc::get_mut(&mut inner) {
                    for term in &mut slice[..idx] {
                        ManuallyDrop::drop(term)
                    }
                    for term in &mut slice[end..] {
                        ManuallyDrop::drop(term)
                    }
                }

                IntoIter { inner, idx, end }
            }
        }
    }

    pub struct IntoIter {
        inner: Rc<[ManuallyDrop<RichTerm>]>,
        idx: usize,
        end: usize,
    }

    impl Iterator for IntoIter {
        type Item = RichTerm;

        fn next(&mut self) -> Option<Self::Item> {
            if self.idx == self.end {
                None
            } else {
                let term = match Rc::get_mut(&mut self.inner) {
                    None => self.inner[..self.end]
                        .get(self.idx)
                        .cloned()
                        .map(ManuallyDrop::into_inner),
                    Some(slice) => slice[..self.end]
                        .get_mut(self.idx)
                        .map(|t| unsafe { ManuallyDrop::take(t) }),
                };

                self.idx += 1;
                term
            }
        }
    }

    impl ExactSizeIterator for IntoIter {
        fn len(&self) -> usize {
            self.end - self.idx
        }
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct ArrayAttrs {
    /// A `closurized` array verifies the following conditions:
    ///   - Each element is a generated variable with a unique name (although the same
    ///     variable can occur in several places, it should always refer to the same thunk anyway).
    ///   - The environment of the array's closure only contains those generated variables.
    pub closurized: bool,
    /// List of lazily-applied contracts.
    /// These are only observed when data enters or leaves the array.
    pub pending_contracts: Vec<PendingContract>,
}

impl ArrayAttrs {
    /// Create an `ArrayAttrs`.
    /// By default, the `closurized` flag is set to `false`,
    /// and `pending_contracts` is empty.
    pub fn new() -> Self {
        Default::default()
    }

    /// Set the `closurized` flag to `true`.
    pub fn closurized(mut self) -> Self {
        self.closurized = true;
        self
    }

    /// Drop the pending contracts.
    pub fn contracts_cleared(mut self) -> Self {
        self.pending_contracts.clear();
        self
    }

    /// Extend contracts from an iterator of `PendingContract`.
    /// De-duplicate equal contracts. Note that current contract
    /// equality testing is very limited, but this may change in the
    /// future
    pub fn with_extra_contracts<I>(mut self, iter: I) -> Self
    where
        I: IntoIterator<Item = PendingContract>,
    {
        for ctr in iter {
            if !self.pending_contracts.contains(&ctr) {
                self.pending_contracts.push(ctr)
            }
        }

        self
    }
}

pub mod record {
    use super::{RecordAttrs, RichTerm, SealingKey};
    use crate::{identifier::Ident, label::Label};
    use std::collections::HashMap;

    /// The base structure of a Nickel record.
    ///
    /// Used to group together fields common to both the [Record](Term::Record) and
    /// [RecRecord](Term::RecRecord) terms.
    #[derive(Clone, Debug, Default, PartialEq)]
    pub struct RecordData {
        /// Fields whose names are known statically.
        pub fields: HashMap<Ident, RichTerm>,
        /// Attributes which may be applied to a record.
        pub attrs: RecordAttrs,
        /// The hidden part of a record under a polymorphic contract.
        pub sealed_tail: Option<SealedTail>,
    }

    impl RecordData {
        pub fn new(
            fields: HashMap<Ident, RichTerm>,
            attrs: RecordAttrs,
            sealed_tail: Option<SealedTail>,
        ) -> Self {
            RecordData {
                fields,
                attrs,
                sealed_tail,
            }
        }

        /// A record with no fields and the default set of attributes.
        pub fn empty() -> Self {
            Default::default()
        }

        /// A record with the provided fields & the default set of attributes.
        pub fn with_fields(fields: HashMap<Ident, RichTerm>) -> Self {
            let attrs = Default::default();
            let sealed_tail = Default::default();
            RecordData {
                fields,
                attrs,
                sealed_tail,
            }
        }

        /// Returns the record resulting from applying the provided function
        /// to each field.
        ///
        /// Note that `f` is taken as `mut` in order to allow it to mutate
        /// external state while iterating.
        pub fn map_fields<F>(self, mut f: F) -> Self
        where
            F: FnMut(Ident, RichTerm) -> RichTerm,
        {
            let fields = self
                .fields
                .into_iter()
                .map(|(id, t)| (id, f(id, t)))
                .collect();
            RecordData { fields, ..self }
        }
    }

    /// The sealed tail of a Nickel record under a polymorphic contract.
    ///
    /// Note that access to the enclosed [term] must only be allowed when a
    /// matching [sealing_key] is provided. If this is not enforced it will
    /// lead to parametricity violations.
    #[derive(Clone, Debug, PartialEq)]
    pub struct SealedTail {
        /// The key with which the tail is sealed.
        sealing_key: SealingKey,
        /// The label to which blame will be attributed if code tries to
        /// interact with the sealed tail in any way.
        pub label: Label,
        /// The term which is sealed.
        term: RichTerm,
    }

    impl SealedTail {
        pub fn new(sealing_key: SealingKey, label: Label, term: RichTerm) -> Self {
            Self {
                sealing_key,
                label,
                term,
            }
        }

        /// Returns the sealed term if the key matches, otherwise returns None.
        pub fn unseal(&self, key: &SealingKey) -> Option<&RichTerm> {
            if key == &self.sealing_key {
                Some(&self.term)
            } else {
                None
            }
        }
    }
}

#[derive(Debug, Default, Eq, PartialEq, Copy, Clone)]
pub struct RecordAttrs {
    pub open: bool,
}

/// The attributes of a let binding.
#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct LetAttrs {
    /// The type of a let binding. See the documentation of [`BindingType`].
    pub binding_type: BindingType,
    /// A recursive let binding adds its binding to the environment of the expression.
    pub rec: bool,
}

impl RecordAttrs {
    pub fn merge(attrs1: RecordAttrs, attrs2: RecordAttrs) -> RecordAttrs {
        RecordAttrs {
            open: attrs1.open || attrs2.open,
        }
    }
}

/// Dependencies of a field or a thunk over the other recursive fields of a recursive record.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FieldDeps {
    /// The set of dependencies is fixed and has been computed. When attached to a thunk, an empty
    /// set of dependency means that the thunk isn't revertible, but standard.
    Known(Rc<HashSet<Ident>>),
    /// The thunk is revertible, but the set of dependencies hasn't been computed. In that case,
    /// the interpreter should be conservative and assume that any recursive references can appear
    /// in the content of the corresponding thunk.
    Unknown,
}

impl FieldDeps {
    /// Compute the union of two thunk dependencies. [`FieldDeps::Unknown`] can be see as the top
    /// element, meaning that if one of the two set of dependencies is [`FieldDeps::Unknown`], so
    /// is the result.
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            // If one of the field has unknown dependencies (understand: may depend on all the other
            // fields), then the resulting fields has unknown dependencies as well
            (FieldDeps::Unknown, _) | (_, FieldDeps::Unknown) => FieldDeps::Unknown,
            (FieldDeps::Known(deps1), FieldDeps::Known(deps2)) => {
                let union: HashSet<Ident> = deps1.union(&*deps2).cloned().collect();
                FieldDeps::Known(Rc::new(union))
            }
        }
    }

    /// Return an empty set of dependencies
    pub fn empty() -> Self {
        FieldDeps::Known(Rc::new(HashSet::new()))
    }

    /// Return `true` if the dependencies are known and are empty, or `false` otherwise.
    pub fn is_empty(&self) -> bool {
        matches!(self, FieldDeps::Known(deps) if deps.is_empty())
    }
}

impl From<HashSet<Ident>> for FieldDeps {
    fn from(set: HashSet<Ident>) -> Self {
        FieldDeps::Known(Rc::new(set))
    }
}

/// Store field interdependencies in a recursive record. Map each static and dynamic field to the
/// set of recursive fields that syntactically appears in their definition as free variables.
#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct RecordDeps {
    /// Must have exactly the same keys as the static fields map of the recursive record.
    pub stat_fields: HashMap<Ident, FieldDeps>,
    /// Must have exactly the same length as the dynamic fields list of the recursive record.
    pub dyn_fields: Vec<FieldDeps>,
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
            MergePriority::Numeral(p) => write!(f, "{}", p),
            MergePriority::Top => write!(f, "force"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Contract {
    pub types: Types,
    pub label: Label,
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct MetaValue {
    pub doc: Option<String>,
    pub types: Option<Contract>,
    pub contracts: Vec<Contract>,
    /// If the field is optional.
    pub opt: bool,
    pub priority: MergePriority,
    pub value: Option<RichTerm>,
}

impl From<RichTerm> for MetaValue {
    fn from(rt: RichTerm) -> Self {
        MetaValue {
            value: Some(rt),
            ..Default::default()
        }
    }
}

impl MetaValue {
    pub fn new() -> Self {
        Default::default()
    }

    /// Flatten two nested metavalues into one, combining their metadata. If data that can't be
    /// combined (typically, the documentation or the type annotation) are set by both metavalues,
    /// outer's one are kept.
    ///
    /// Note that no environment management such as closurization takes place, because this
    /// function is expected to be used on the AST before the evaluation (in the parser or during
    /// program transformation).
    ///
    /// # Preconditions
    ///
    /// - `outer.value` is assumed to be `inner`. While `flatten` may still work fine if this
    ///   condition is not fulfilled, the value of the final metavalue is set to be `inner`'s one,
    ///   and `outer`'s one is dropped.
    pub fn flatten(mut outer: MetaValue, mut inner: MetaValue) -> MetaValue {
        // Keep the outermost value for non-mergeable information, such as documentation, type annotation,
        // and so on, which is the one that is accessible from the outside anyway (by queries, by the typechecker, and
        // so on).
        // Keep the inner value.

        if outer.types.is_some() {
            // If both have type annotations, the result will have the outer one as a type annotation.
            // However we still need to enforce the corresponding contract to preserve the operational
            // semantics. Thus, the inner type annotation is derelicted to a contract.
            if let Some(ctr) = inner.types.take() {
                outer.contracts.push(ctr)
            }
        }

        outer.contracts.extend(inner.contracts.into_iter());

        let priority = match (outer.priority, inner.priority) {
            // Neutral corresponds to the case where no priority was specified. In that case, the
            // other priority takes precedence.
            (MergePriority::Neutral, p) | (p, MergePriority::Neutral) => p,
            // Otherwise, we keep the maximum of both priorities, as we would do when merging
            // values.
            (p1, p2) => std::cmp::max(p1, p2),
        };

        MetaValue {
            doc: outer.doc.or(inner.doc),
            types: outer.types.or(inner.types),
            contracts: outer.contracts,
            opt: outer.opt || inner.opt,
            priority,
            value: inner.value,
        }
    }
}

/// A chunk of a string with interpolated expressions inside. Same as `Either<String,
/// RichTerm>` but with explicit constructor names.
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
    //#[cfg(test)]
    /// Recursively apply a function to all `Term`s contained in a `RichTerm`.
    pub fn apply_to_rich_terms<F>(&mut self, func: F)
    where
        F: Fn(&mut RichTerm),
    {
        use self::Term::*;
        match self {
            Null | ParseError(_) => (),
            Switch(ref mut t, ref mut cases, ref mut def) => {
                cases.iter_mut().for_each(|c| {
                    let (_, t) = c;
                    func(t);
                });
                func(t);
                if let Some(def) = def {
                    func(def)
                }
            }
            Record(ref mut r) => {
                r.fields.iter_mut().for_each(|(_, t)| func(t));
            }
            RecRecord(ref mut r, ref mut dyn_fields, ..) => {
                r.fields.iter_mut().for_each(|(_, t)| func(t));
                dyn_fields.iter_mut().for_each(|(t1, t2)| {
                    func(t1);
                    func(t2);
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
            MetaValue(ref mut meta) => {
                meta.contracts
                    .iter_mut()
                    .for_each(|Contract { types, .. }| {
                        if let TypeF::Flat(ref mut rt) = types.0 {
                            func(rt)
                        }
                    });
                meta.value.iter_mut().for_each(func);
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
            Term::Lbl(_) => Some("Label"),
            Term::Enum(_) => Some("Enum"),
            Term::Record(..) | Term::RecRecord(..) => Some("Record"),
            Term::Array(..) => Some("Array"),
            Term::SealingKey(_) => Some("SealingKey"),
            Term::Sealed(..) => Some("Sealed"),
            Term::MetaValue(_) => Some("Metavalue"),
            Term::Let(..)
            | Term::LetPattern(..)
            | Term::App(_, _)
            | Term::Var(_)
            | Term::Switch(..)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::OpN(..)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_)
            | Term::ParseError(_) => None,
        }
        .map(String::from)
    }

    /// Return a shallow string representation of a term, used for error reporting.
    pub fn shallow_repr(&self) -> String {
        match self {
            Term::Null => String::from("null"),
            Term::Bool(true) => String::from("true"),
            Term::Bool(false) => String::from("false"),
            Term::Num(n) => format!("{}", n),
            Term::Str(s) => format!("\"{}\"", s),
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
            Term::Lbl(_) => String::from("<label>"),
            Term::Enum(id) => {
                let re = regex::Regex::new("_?[a-zA-Z][_a-zA-Z0-9]*").unwrap();
                let s = id.to_string();
                if re.is_match(&s) {
                    format!("`{}", s)
                } else {
                    format!("`\"{}\"", s)
                }
            }
            Term::Record(..) | Term::RecRecord(..) => String::from("{ ... }"),
            Term::Array(..) => String::from("[ ... ]"),
            Term::SealingKey(_) => String::from("<sealing key>"),
            Term::Sealed(..) => String::from("<sealed>"),
            Term::MetaValue(ref meta) => {
                let mut content = String::new();

                if meta.doc.is_some() {
                    content.push_str("doc,");
                }
                if !meta.contracts.is_empty() {
                    content.push_str("contract,");
                }

                let value_label = if meta.priority == MergePriority::Bottom {
                    "default"
                } else {
                    "value"
                };
                let value = if let Some(t) = &meta.value {
                    t.as_ref().shallow_repr()
                } else {
                    String::from("none")
                };

                format!("<{}{}={}>", content, value_label, value)
            }
            Term::Var(id) => id.to_string(),
            Term::ParseError(_) => String::from("<parse error>"),
            Term::Let(..)
            | Term::LetPattern(..)
            | Term::App(_, _)
            | Term::Switch(..)
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
                    .map(|(ident, term)| format!("{} = {}", ident, term.as_ref().deep_repr()))
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
            | Term::Fun(_, _)
            | Term::Lbl(_)
            | Term::Enum(_)
            | Term::Record(..)
            | Term::Array(..)
            | Term::SealingKey(_) => true,
            Term::Let(..)
            | Term::LetPattern(..)
            | Term::FunPattern(..)
            | Term::App(_, _)
            | Term::Var(_)
            | Term::Switch(..)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::OpN(..)
            | Term::Sealed(..)
            | Term::MetaValue(_)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_)
            | Term::RecRecord(..)
            | Term::ParseError(_) => false,
        }
    }

    /// Determine if a term is a metavalue.
    pub fn is_metavalue(&self) -> bool {
        matches!(self, Term::MetaValue(..))
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
            | Term::Fun(_, _)
            | Term::FunPattern(_, _, _)
            | Term::App(_, _)
            | Term::Switch(..)
            | Term::Var(_)
            | Term::Op1(_, _)
            | Term::Op2(_, _, _)
            | Term::OpN(..)
            | Term::Sealed(..)
            | Term::MetaValue(_)
            | Term::Import(_)
            | Term::ResolvedImport(_)
            | Term::StrChunks(_)
            | Term::RecRecord(..)
            | Term::ParseError(_) => false,
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
            | Term::Op1(..)
            | Term::SealingKey(..) => true,
            Term::Let(..)
            | Term::Switch(..)
            | Term::LetPattern(..)
            | Term::Fun(..)
            | Term::FunPattern(..)
            | Term::App(..)
            | Term::Op2(..)
            | Term::OpN(..)
            | Term::Sealed(..)
            | Term::MetaValue(..)
            | Term::Import(..)
            | Term::ResolvedImport(..)
            | Term::ParseError(_) => false,
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
    /// A switch block. Used to match on a enumeration.
    Switch(bool /* presence of a default case */), //HashMap<Ident, CapturedTerm>, Option<CapturedTerm>),

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
    /// Like [`UnaryOp::StrMatch`], this is a unary operator because we would like a way to share the
    /// same "compiled regex" for many matching calls. This is done by returning functions
    /// wrapping [`UnaryOp::StrIsMatchCompiled`] and [`UnaryOp::StrMatchCompiled`]
    StrIsMatch(),
    /// Match a regex on a string, and returns the captured groups together, the index of the
    /// match, etc.
    StrMatch(),
    /// Version of [`UnaryOp::StrIsMatch`] which remembers the compiled regex.
    StrIsMatchCompiled(CompiledRegex),
    /// Version of [`UnaryOp::StrMatch`] which remembers the compiled regex.
    StrMatchCompiled(CompiledRegex),
    /// Force full evaluation of a term and return it.
    ///
    /// This was added in the context of [`BinaryOp::ArrayLazyAssume`],
    /// in particular to make serialization work with lazy array contracts.
    ///
    /// # `Force` vs. `DeepSeq`
    ///
    /// [`UnaryOp::Force`] updates the thunks containing arrays with a new version where the lazy contracts have all been applied,
    /// whereas [`UnaryOp::DeepSeq`] evaluates the same expressions, but it never updates the thunk of an array with lazy contracts
    /// with an array where those contracts have been applied. In a way, the result of lazy contract application in arrays is "lost"
    /// in [`UnaryOp::DeepSeq`], while it's returned in [`UnaryOp::Force`].
    ///
    /// This means we can observe different results between `deep_seq x x` and `force x`, in some cases.
    ///
    /// It's also worth noting that [`UnaryOp::DeepSeq`] should be, in principle, more efficient that [`UnaryOp::Force`]
    /// as it does less cloning.
    Force(Option<crate::eval::callstack::StackElem>),

    /// doc: TODO
    PushDefault(),
    /// doc: TODO
    PushForce(),

    /// Creates an "empty" record with the sealed tail of its [`Term::Record`]
    /// argument.
    ///
    /// Used in the `$record` contract implementation to ensure that we can
    /// define a `field_diff` function that preserves the sealed polymorphic
    /// tail of its argument.
    RecordEmptyWithTail(),
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
    pub fn eval_mode(&self) -> EvalMode {
        match self {
            UnaryOp::PushDefault() | UnaryOp::PushForce() => EvalMode::StopAtMeta,
            _ => EvalMode::default(),
        }
    }

    pub fn pos(&self) -> OpPos {
        use UnaryOp::*;
        match self {
            BoolAnd() | BoolOr() | StaticAccess(_) => OpPos::Postfix,
            Ite() => OpPos::Special,
            _ => OpPos::Prefix,
        }
    }
}

/// Primitive binary operators
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinaryOp {
    /// Addition of numerals.
    Plus(),
    /// Substraction of numerals.
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
    /// Stricty less than comparison operator.
    LessThan(),
    /// Less than or equal comparison operator.
    LessOrEq(),
    /// Stricty greater than comparison operator.
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
    /// See [`UnaryOp::Seal`].
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
    DynExtend(),
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
}

impl BinaryOp {
    pub fn eval_mode(&self) -> EvalMode {
        match self {
            BinaryOp::Merge() => EvalMode::StopAtMeta,
            _ => EvalMode::default(),
        }
    }

    pub fn pos(&self) -> OpPos {
        use BinaryOp::*;
        match self {
            Plus() | Sub() | Mult() | Div() | Modulo() | Pow() | StrConcat() | Eq()
            | LessThan() | LessOrEq() | GreaterThan() | GreaterOrEq() | DynExtend()
            | DynRemove() | DynAccess() | ArrayConcat() | Merge() => OpPos::Infix,
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
    ///   - a [label](Term::Label) which will be used to assign blame correctly if
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

    pub fn eval_mode(&self) -> EvalMode {
        EvalMode::default()
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

    /// Apply a transformation on a whole term by mapping a faillible function `f` on each node in
    /// manner as prescribed by the order.
    /// `f` may return a generic error `E` and use the state `S` which is passed around.
    pub fn traverse<F, S, E>(
        self,
        f: &F,
        state: &mut S,
        order: TraverseOrder,
    ) -> Result<RichTerm, E>
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
            Term::Switch(t, cases, default) => {
                // The annotation on `map_res` use Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let cases_res: Result<HashMap<Ident, RichTerm>, E> = cases
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,RichTerm), E>
                    .map(|(id, t)| t.traverse(f, state, order).map(|t_ok| (id, t_ok)))
                    .collect();

                let default = default.map(|t| t.traverse(f, state, order)).transpose()?;

                let t = t.traverse(f, state, order)?;

                RichTerm::new(
                    Term::Switch(t, cases_res?, default),
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
                let fields_res: Result<HashMap<Ident, RichTerm>, E> = record.fields
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,RichTerm), E>
                    .map(|(id, t)| t.traverse(f, state, order).map(|t_ok| (id, t_ok)))
                    .collect();
                RichTerm::new(Term::Record(RecordData::new(fields_res?, record.attrs, record.sealed_tail)), pos)
            },
            Term::RecRecord(record, dyn_fields, deps) => {
                // The annotation on `map_res` uses Result's corresponding trait to convert from
                // Iterator<Result> to a Result<Iterator>
                let static_fields_res: Result<HashMap<Ident, RichTerm>, E> = record.fields
                    .into_iter()
                    // For the conversion to work, note that we need a Result<(Ident,RichTerm), E>
                    .map(|(id, t)| Ok((id, t.traverse(f, state, order)?)))
                    .collect();
                let dyn_fields_res: Result<Vec<(RichTerm, RichTerm)>, E> = dyn_fields
                    .into_iter()
                    .map(|(id_t, t)| {
                        Ok((
                            id_t.traverse(f, state, order)?,
                            t.traverse(f, state, order)?,
                        ))
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
            Term::MetaValue(meta) => {
                let mut f_on_type = |ty: Types, s: &mut S| {
                    match ty.0 {
                        TypeF::Flat(t) => t.traverse(f, s, order).map(|t| Types(TypeF::Flat(t))),
                        _ => Ok(ty),
                    }
                };

                let contracts: Result<Vec<Contract>, _> = meta
                    .contracts
                    .into_iter()
                    .map(|ctr| {
                        let Contract {types, label} = ctr;
                        types.traverse(&f_on_type, state, order).map(|types| Contract { types, label })
                    })
                    .collect();
                let contracts = contracts?;

                let types = meta
                    .types
                    .map(|ctr| {
                        let Contract {types, label} = ctr;
                        types.traverse(&f_on_type, state, order).map(|types| Contract { types, label })
                    })
                    .transpose()?;

                let value = meta
                    .value
                    .map(|t| t.traverse(f, state, order))
                    .map_or(Ok(None), |res| res.map(Some))?;
                    let meta = MetaValue {
                        doc: meta.doc,
                        types,
                        contracts,
                        opt: meta.opt,
                        priority: meta.priority,
                        value,
                    };

                RichTerm::new(
                    Term::MetaValue(meta),
                    pos,
                )
            }} else rt
        };

        match order {
            TraverseOrder::TopDown => Ok(result),
            TraverseOrder::BottomUp => f(result, state),
        }
    }

    /// Pretty print a term capped to a given max length (in characters). Useful to limit the size
    /// of terms reported e.g. in typechecking errors. If the output of pretty printing is greater
    /// than the bound, the string is truncated to `max_width` and the last character after
    /// truncate is replaced by the ellipsis unicode character U+2026.
    pub fn pretty_print_cap(&self, max_width: usize) -> String {
        let output = format!("{}", self);

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
                $crate::term::RichTerm::from($crate::term::Term::Record($crate::term::record::RecordData::with_fields(fields)))
            }
        };
    }

    /// Switch for types implementing `Into<Ident>` (for patterns) and `Into<RichTerm>` for the
    /// body of each case. Cases are specified as tuple, and the default case (optional) is separated by a `;`:
    /// `mk_switch!(format, ("Json", json_case), ("Yaml", yaml_case) ; def)` corresponds to
    /// ``switch { `Json => json_case, `Yaml => yaml_case, _ => def} format``.
    #[macro_export]
    macro_rules! mk_switch {
        ( $exp:expr, $( ($id:expr, $body:expr) ),* ; $default:expr ) => {
            {
                let mut map = std::collections::HashMap::new();
                $(
                    map.insert($id.into(), $body.into());
                )*
                $crate::term::RichTerm::from($crate::term::Term::Switch($crate::term::RichTerm::from($exp), map, Some($crate::term::RichTerm::from($default))))
            }
        };
        ( $exp:expr, $( ($id:expr, $body:expr) ),*) => {
                let mut map = std::collections::HashMap::new();
                $(
                    map.insert($id.into(), $body.into());
                )*
                $crate::term::RichTerm::from($crate::term::Term::Switch($crate::term::RichTerm::from($exp), map, None))
        };
    }

    /// Array for types implementing `Into<RichTerm>` (for elements).
    /// The array's attributes are a trailing (optional) `ArrayAttrs`, separated by a `;`.
    /// `mk_array!(Term::Num(42)) corresponds to `[42]`. Here the attributes are `ArrayAttrs::default()`, though the evaluated array may have different attributes.
    #[macro_export]
    macro_rules! mk_array {
        ( $( $terms:expr ),* ; $attrs:expr ) => {
            {
                let ts = $crate::term::array::Array::new(std::rc::Rc::new([$( $crate::term::RichTerm::from($terms) ),*]));
                $crate::term::RichTerm::from(Term::Array(ts, $attrs))
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
        D: Into<Destruct>,
        I: Into<Ident>,
    {
        Term::LetPattern(id.map(|i| i.into()), pat.into(), t1.into(), t2.into()).into()
    }

    #[cfg(test)]
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
    fn metavalue_flatten() {
        let mut inner = MetaValue::new();
        inner.types = Some(Contract {
            types: Types(TypeF::Num),
            label: Label::dummy(),
        });
        let outer = MetaValue::new();
        let res = MetaValue::flatten(outer, inner);
        assert_ne!(res.types, None);
    }
}
