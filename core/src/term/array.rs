use nickel_lang_funcarray::FunctionalArray;

use super::*;

#[derive(Debug, Default, PartialEq, Clone)]
pub struct ArrayAttrs {
    /// An array is closurized when each element is a [crate::term::Term::Closure] or a constant.
    ///
    /// When initially produced by the parser, arrays aren't closurized. At the first evaluation,
    /// they will be turned into closurized arrays, by allocating cache nodes (think thunks) for
    /// non constant elements, and this flag is set to true.
    ///
    /// Ideally, we would have a different AST representation for evaluation, where arrays would
    /// always be closurized. In the meantime, while we need to cope with a unique AST across the
    /// whole pipeline, we use this flag.
    pub closurized: bool,
    /// List of lazily-applied contracts.
    /// These are only observed when data enters or leaves the array.
    pub pending_contracts: Vec<RuntimeContract>,
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
}

pub type Array = FunctionalArray<RichTerm, 32>;

// TODO: one common use of this collect function is `arr.into_iter().map(|_| ...).collect()`.
// Maybe it's worth having an optimized `map_in_place` function, or an `iter_mut`.
impl FromIterator<RichTerm> for Array {
    fn from_iter<T: IntoIterator<Item = RichTerm>>(iter: T) -> Self {
        // This needs an extra allocation, because FunctionalArray only supports non-allocating construction from reversed iterators.
        let items = iter.into_iter().collect::<Vec<_>>();
        Array::collect(items.into_iter())
    }
}
