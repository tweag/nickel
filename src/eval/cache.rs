use std::fmt::Debug;

use super::{
    lazy::{BlackholedError, Thunk, ThunkDeps, ThunkState, ThunkUpdateFrame},
    IdentKind,
};
use crate::{eval::Closure, term::BindingType};

pub trait Cache: Clone {
    type Index: CacheIndex;
    type UpdateIndex;

    fn get(&self, idx: Self::Index) -> Closure<Self>
    where
        Self: Sized;
    fn get_update_index(
        &mut self,
        idx: &mut Self::Index,
    ) -> Result<Option<Self::UpdateIndex>, BlackholedError>;
    // TODO: This is strange and too CBN-minded
    fn add(&mut self, clos: Closure<Self>, kind: IdentKind, bty: BindingType) -> Self::Index
    where
        Self: Sized;
    fn patch<F: FnOnce(&mut Closure<Self>)>(&mut self, idx: Self::Index, f: F);
    fn get_then<T, F: FnOnce(&Closure<Self>) -> T>(&self, idx: Self::Index, f: F) -> T;
    fn update(&mut self, clos: Closure<Self>, idx: Self::UpdateIndex)
    where
        Self: Sized;
    fn new() -> Self;
    fn reset_index_state(&self, idx: &mut Self::UpdateIndex);
    fn map_at_index<F: FnMut(&Closure<Self>) -> Closure<Self>>(
        &mut self,
        idx: &Self::Index,
        f: F,
    ) -> Self::Index;
}

pub trait CacheIndex: Clone + PartialEq + Debug {
    fn ident_kind(&self) -> IdentKind;
    fn deps(&self) -> ThunkDeps;
    fn revert(&self) -> Self;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CBNCache {}

impl CacheIndex for Thunk {
    fn ident_kind(&self) -> IdentKind {
        self.ident_kind()
    }

    fn deps(&self) -> ThunkDeps {
        self.deps()
    }

    fn revert(&self) -> Self {
        self.revert()
    }
}

impl Cache for CBNCache {
    type Index = Thunk;
    type UpdateIndex = ThunkUpdateFrame;

    fn get(&self, idx: Self::Index) -> Closure<Self> {
        idx.get_owned()
    }

    fn get_update_index(
        &mut self,
        idx: &mut Self::Index,
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

    fn add(&mut self, clos: Closure<Self>, kind: IdentKind, bty: BindingType) -> Self::Index {
        match bty {
            BindingType::Normal => Thunk::new(clos, kind),
            BindingType::Revertible(deps) => Thunk::new_rev(clos, kind, deps),
        }
    }

    fn patch<F: FnOnce(&mut Closure<Self>)>(&mut self, mut idx: Self::Index, f: F) {
        f(&mut idx.borrow_mut());
    }

    fn get_then<T, F: FnOnce(&Closure<Self>) -> T>(&self, idx: Self::Index, f: F) -> T {
        f(&idx.borrow())
    }

    fn update(&mut self, clos: Closure<Self>, uidx: Self::UpdateIndex) {
        uidx.update(clos);
    }

    fn new() -> Self {
        CBNCache {}
    }

    fn reset_index_state(&self, idx: &mut Self::UpdateIndex) {
        idx.reset_state();
    }

    fn map_at_index<F: FnMut(&Closure<Self>) -> Closure<Self>>(
        &mut self,
        idx: &Self::Index,
        f: F,
    ) -> Self::Index {
        idx.map(f)
    }
}
