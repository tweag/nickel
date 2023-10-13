/// The Nickel generic evaluation cache. This module abstracts away the details for managing
/// suspended computations and their memoization strategies.
///
/// Terminology:
/// An *element* of the cache is what is stored inside of it.
/// An *index* into the cache points to a given element.
use super::Closure;
use crate::{
    identifier::Ident,
    term::{record::FieldDeps, BindingType, RichTerm},
};

pub mod lazy;
// pub mod incremental;

/// An index to a specific item stored in the cache
pub type CacheIndex = lazy::Thunk;
// pub type CacheIndex = usize;

/// The current Cache implementation
pub type CacheImpl = lazy::CBNCache;
// pub type CacheImpl = incremental::IncCache;

/// A black-holed node was accessed, which would lead to infinite recursion.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct BlackholedError;

pub trait Cache: Clone {
    /// Temporary: as of now we only need this for [lazy::CBNCache].
    type UpdateIndex;

    /// Gets the [Closure] from the element at index `idx`.
    fn get(&self, idx: CacheIndex) -> Closure;

    /// Checks whether the element at index `idx` is blackholed and returns a
    /// [BlackholedError] if it is. Otherwise, returns `Some(idx)` if the element
    /// needs to be updated, returns `None` if not.
    /// Should use [Cache::make_update_index].
    fn get_update_index(
        &mut self,
        idx: &mut CacheIndex,
    ) -> Result<Option<Self::UpdateIndex>, BlackholedError>;

    /// Adds an element into the [Cache] and returns its index.
    fn add(&mut self, clos: Closure, bty: BindingType) -> CacheIndex;

    /// Applies `f` to the [Closure] stored inside the element at index `idx`.
    fn patch<F: Fn(&mut Closure)>(&mut self, idx: CacheIndex, f: F);

    /// Clones the [Closure] from the element at index `idx` and applies `f` to it.
    fn get_then<T, F: FnOnce(&Closure) -> T>(&self, idx: CacheIndex, f: F) -> T;

    /// Updates the [Closure] from the element at index `idx` with `clos`.
    fn update(&mut self, clos: Closure, idx: Self::UpdateIndex);

    /// Initializes a new [Cache].
    fn new() -> Self;

    /// Resets the state of the element at index `idx` to `Suspended`
    fn reset_index_state(&mut self, idx: &mut Self::UpdateIndex);

    fn map_at_index<F: FnMut(&mut Self, &Closure) -> Closure>(
        &mut self,
        idx: &CacheIndex,
        f: F,
    ) -> CacheIndex;

    /// Initializes the cached value of the element at index `idx` with the given `rec_env`.
    fn build_cached(&mut self, idx: &mut CacheIndex, rec_env: &[(Ident, CacheIndex)]);

    /// Revert the element at index `idx`, abstract over its dependencies to get back a function,
    /// and apply the function to the given variables. The function part is allocated in a new
    /// cache entry, stored as a generated variable, with the same environment as the original
    /// expression.
    fn saturate<I: DoubleEndedIterator<Item = Ident> + Clone>(
        &mut self,
        idx: CacheIndex,
        fields: I,
    ) -> RichTerm;

    /// Reverts the element stored at index `idx` to its original value.
    fn revert(&mut self, idx: &CacheIndex) -> CacheIndex;

    /// Returns the dependencies of the element stored at index `idx`, if it has any.
    fn deps(&self, idx: &CacheIndex) -> Option<FieldDeps>;

    /// Checks whether the element at index `idx` is blackholed and returns a
    /// [BlackholedError] if it is. Otherwise, returns `idx`.
    fn make_update_index(
        &mut self,
        idx: &mut CacheIndex,
    ) -> Result<Self::UpdateIndex, BlackholedError>;
}
