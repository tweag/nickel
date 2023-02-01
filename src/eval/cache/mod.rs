use super::{Closure, Environment, IdentKind};
use crate::{
    identifier::Ident,
    term::{record::FieldDeps, BindingType, RichTerm},
};

// pub mod lazy;
pub mod incremental;

/// An index to a specific item stored in the cache
pub type CacheIndex = usize;

//pub type CacheIndex = Thunk;

/// A black-holed thunk was accessed, which would lead to infinite recursion.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct BlackholedError;

pub trait Cache: Clone {
    type UpdateIndex; // Temporary: we won't really need that once an alternative caching mechanism gets implemented

    fn get(&self, idx: CacheIndex) -> Closure;
    fn get_update_index(
        &mut self,
        idx: &CacheIndex,
    ) -> Result<Option<Self::UpdateIndex>, BlackholedError>;
    fn add(&mut self, clos: Closure, kind: IdentKind, bty: BindingType) -> CacheIndex;
    fn patch<F: Fn(&mut Closure)>(&mut self, idx: CacheIndex, f: F);
    fn get_then<T, F: FnOnce(&Closure) -> T>(&self, idx: CacheIndex, f: F) -> T;
    fn update(&mut self, clos: Closure, idx: Self::UpdateIndex);
    fn new() -> Self;
    fn reset_index_state(&mut self, idx: &mut Self::UpdateIndex);
    fn map_at_index<F: FnMut(&Closure) -> Closure>(&mut self, idx: &CacheIndex, f: F)
        -> CacheIndex;
    fn build_cached(&mut self, idx: &mut CacheIndex, rec_env: &[(Ident, CacheIndex)]);
    fn ident_kind(&self, idx: &CacheIndex) -> IdentKind;
    fn saturate<'a, I: DoubleEndedIterator<Item = &'a Ident> + Clone>(
        &mut self,
        idx: CacheIndex,
        env: &mut Environment,
        fields: I,
    ) -> RichTerm;
    fn revert(&mut self, idx: &CacheIndex) -> CacheIndex;
    fn deps(&self, idx: &CacheIndex) -> Option<FieldDeps>;
    fn make_update_index(&mut self, idx: &CacheIndex)
        -> Result<Self::UpdateIndex, BlackholedError>;
}
