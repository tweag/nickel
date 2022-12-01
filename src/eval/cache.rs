use std::fmt::Debug;

use super::{
    lazy::{BlackholedError, Thunk, ThunkState, ThunkUpdateFrame},
    IdentKind,
};
use crate::{eval::Closure, identifier::Ident, term::BindingType};

pub trait Cache: Clone {
    type UpdateIndex; // Temporary: we won't really need that once an alternative caching mechanism gets implemented

    fn get(&self, idx: CacheIndex) -> Closure;
    fn get_update_index(
        &mut self,
        idx: &mut CacheIndex,
    ) -> Result<Option<Self::UpdateIndex>, BlackholedError>;
    fn add(&mut self, clos: Closure, kind: IdentKind, bty: BindingType) -> CacheIndex;
    fn patch<F: FnOnce(&mut Closure)>(&mut self, idx: CacheIndex, f: F);
    fn get_then<T, F: FnOnce(&Closure) -> T>(&self, idx: CacheIndex, f: F) -> T;
    fn update(&mut self, clos: Closure, idx: Self::UpdateIndex);
    fn new() -> Self;
    fn reset_index_state(&self, idx: &mut Self::UpdateIndex);
    fn map_at_index<F: FnMut(&Closure) -> Closure>(&mut self, idx: &CacheIndex, f: F)
        -> CacheIndex;
    // TODO: Needs a better name
    fn build_cached(&mut self, idx: &mut CacheIndex, rec_env: &[(Ident, CacheIndex)]);
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CBNCache {}

pub type CacheIndex = Thunk;

impl Cache for CBNCache {
    type UpdateIndex = ThunkUpdateFrame;

    fn get(&self, idx: CacheIndex) -> Closure {
        idx.get_owned()
    }

    fn get_update_index(
        &mut self,
        idx: &mut CacheIndex,
    ) -> Result<Option<Self::UpdateIndex>, BlackholedError> {
        if idx.state() != ThunkState::Evaluated {
            if idx.should_update() {
                idx.mk_update_frame().map(Some)
            }
            // If the thunk isn't to be updated, directly set the evaluated flag.
            else {
                idx.set_evaluated();
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn add(&mut self, clos: Closure, kind: IdentKind, bty: BindingType) -> CacheIndex {
        match bty {
            BindingType::Normal => Thunk::new(clos, kind),
            BindingType::Revertible(deps) => Thunk::new_rev(clos, kind, deps),
        }
    }

    fn patch<F: FnOnce(&mut Closure)>(&mut self, mut idx: CacheIndex, f: F) {
        f(&mut idx.borrow_mut());
    }

    fn get_then<T, F: FnOnce(&Closure) -> T>(&self, idx: CacheIndex, f: F) -> T {
        f(&idx.borrow())
    }

    fn update(&mut self, clos: Closure, uidx: Self::UpdateIndex) {
        uidx.update(clos);
    }

    fn new() -> Self {
        CBNCache {}
    }

    fn reset_index_state(&self, idx: &mut Self::UpdateIndex) {
        idx.reset_state();
    }

    fn map_at_index<F: FnMut(&Closure) -> Closure>(
        &mut self,
        idx: &CacheIndex,
        f: F,
    ) -> CacheIndex {
        idx.map(f)
    }

    fn build_cached(&mut self, idx: &mut CacheIndex, rec_env: &[(Ident, CacheIndex)]) {
        idx.build_cached(rec_env)
    }
}
