use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use super::{
    //lazy::{BlackholedError, Thunk, ThunkState, ThunkUpdateFrame},
    Closure,
    Environment,
    IdentKind,
};
use crate::{
    identifier::Ident,
    term::{record::FieldDeps, BindingType, RichTerm, Term},
};

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

pub type CacheIndex = usize;

/*
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

    fn reset_index_state(&mut self, idx: &mut Self::UpdateIndex) {
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

    fn ident_kind(&self, idx: &CacheIndex) -> IdentKind {
        idx.ident_kind()
    }

    fn saturate<'a, I: DoubleEndedIterator<Item = &'a Ident> + Clone>(
        &mut self,
        idx: CacheIndex,
        env: &mut Environment,
        fields: I,
    ) -> RichTerm {
        idx.saturate(env, fields)
    }

    fn deps(&self, idx: &CacheIndex) -> Option<FieldDeps> {
        Some(idx.deps())
    }

    fn revert(&mut self, idx: &CacheIndex) -> CacheIndex {
        idx.revert()
    }

    fn make_update_index(
        &self,
        idx: &mut CacheIndex,
    ) -> Result<Self::UpdateIndex, BlackholedError> {
        idx.mk_update_frame()
    }
}
*/

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub enum IncNodeState {
    #[default]
    Suspended,
    Blackholed,
}

#[derive(Debug, Clone)]
pub struct IncNode {
    orig: Closure,
    cached: Option<Closure>,
    kind: IdentKind,
    bty: BindingType,
    state: IncNodeState,
}

impl IncNode {
    fn new(clos: Closure, kind: IdentKind, bty: BindingType) -> Self {
        IncNode {
            orig: clos,
            cached: None,
            kind,
            bty,
            state: IncNodeState::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IncCache {
    store: HashMap<CacheIndex, IncNode>,
    next: CacheIndex,
}

impl IncCache {
    fn add_node(&mut self, node: IncNode) -> CacheIndex {
        let idx = self.next;
        self.store.insert(idx, node);
        self.next += 1;

        idx
    }
}

impl Cache for IncCache {
    type UpdateIndex = CacheIndex;

    fn new() -> Self {
        IncCache {
            store: HashMap::new(),
            next: 0,
        }
    }

    fn get(&self, idx: CacheIndex) -> Closure {
        let node = self.store.get(&idx).unwrap();

        node.cached.clone().unwrap_or(node.orig.clone())
    }

    fn get_update_index(
        &mut self,
        idx: &CacheIndex,
    ) -> Result<Option<Self::UpdateIndex>, BlackholedError> {
        let node = self.store.get(idx).unwrap();

        if let IncNodeState::Blackholed = node.state {
            Err(BlackholedError)
        } else if node.cached.is_some() {
            Ok(None)
        } else {
            Ok(Some(*idx))
        }
    }

    fn add(&mut self, clos: Closure, kind: IdentKind, bty: BindingType) -> CacheIndex {
        let node = IncNode::new(clos, kind, bty);

        self.add_node(node)
    }

    fn update(&mut self, clos: Closure, idx: Self::UpdateIndex) {
        let node = self.store.get_mut(&idx).unwrap();

        node.cached = Some(clos);
        node.state = IncNodeState::default();
    }

    fn revert(&mut self, idx: &CacheIndex) -> CacheIndex {
        let node = self.store.get(idx).unwrap();

        let new_node = match node.bty.clone() {
            BindingType::Normal => node.clone(),
            BindingType::Revertible(_) => {
                IncNode::new(node.orig.clone(), node.kind, node.bty.clone())
            }
        };

        self.add_node(new_node)
    }

    fn ident_kind(&self, idx: &CacheIndex) -> IdentKind {
        self.store.get(idx).unwrap().kind
    }

    fn deps(&self, idx: &CacheIndex) -> Option<FieldDeps> {
        let node = self.store.get(idx).unwrap();
        match node.bty.clone() {
            BindingType::Normal => None,
            BindingType::Revertible(deps) => Some(deps),
        }
    }

    fn get_then<T, F: FnOnce(&Closure) -> T>(&self, idx: CacheIndex, f: F) -> T {
        f(&self.get(idx))
    }

    fn patch<F: Fn(&mut Closure)>(&mut self, idx: CacheIndex, f: F) {
        let node = self.store.get_mut(&idx).unwrap();

        f(&mut node.orig);
        node.cached.as_mut().map(|mut clos| f(&mut clos));
    }

    fn make_update_index(
        &mut self,
        idx: &CacheIndex,
    ) -> Result<Self::UpdateIndex, BlackholedError> {
        let node = self.store.get_mut(idx).unwrap();

        if node.state == IncNodeState::Blackholed {
            return Err(BlackholedError);
        } else {
            node.state = IncNodeState::Blackholed;

            Ok(*idx)
        }
    }

    fn reset_index_state(&mut self, idx: &mut Self::UpdateIndex) {
        let node = self.store.get_mut(idx).unwrap();

        node.state = IncNodeState::default();
    }

    fn map_at_index<F: FnMut(&Closure) -> Closure>(
        &mut self,
        idx: &CacheIndex,
        mut f: F,
    ) -> CacheIndex {
        let node = self.store.get(idx).unwrap();

        let new_node = IncNode {
            orig: f(&node.orig.clone()),
            cached: node.cached.clone().map(|clos| f(&clos)),
            kind: node.kind,
            bty: node.bty.clone(),
            state: node.state,
        };

        self.add_node(new_node)
    }

    fn build_cached(&mut self, idx: &mut CacheIndex, rec_env: &[(Ident, CacheIndex)]) {
        let node = self.store.get_mut(idx).unwrap();

        if node.cached.is_none() {
            return ();
        }

        let mut new_cached = Closure::clone(&node.orig);

        match node.bty {
            BindingType::Normal => (),
            BindingType::Revertible(ref deps) => match deps {
                FieldDeps::Unknown => new_cached.env.extend(rec_env.iter().cloned()),
                FieldDeps::Known(deps) if deps.is_empty() => (),
                FieldDeps::Known(deps) => new_cached
                    .env
                    .extend(rec_env.iter().filter(|(id, _)| deps.contains(id)).cloned()),
            },
        }

        node.cached = Some(new_cached);
    }

    fn saturate<'a, I: DoubleEndedIterator<Item = &'a Ident> + Clone>(
        &mut self,
        idx: CacheIndex,
        env: &mut Environment,
        fields: I,
    ) -> RichTerm {
        let node = self.store.get(&idx).unwrap();

        let deps = match node.bty.clone() {
            BindingType::Normal => FieldDeps::Known(Rc::new(HashSet::new())),
            BindingType::Revertible(deps) => deps.clone(),
        };

        let mut deps_filter: Box<dyn FnMut(&&Ident) -> bool> = match deps {
            FieldDeps::Known(deps) => Box::new(move |id: &&Ident| deps.contains(id)),
            FieldDeps::Unknown => Box::new(|_: &&Ident| true),
        };

        let Closure {
            body,
            env: env_orig,
        } = node.orig.clone();

        let as_function = fields
            .clone()
            .filter(&mut deps_filter)
            .rfold(body, |built, id| RichTerm::from(Term::Fun(*id, built)));

        let fresh_var = Ident::fresh();
        let as_function_closurized = RichTerm::from(Term::Var(fresh_var));
        let args = fields.filter_map(|id| deps_filter(&id).then(|| RichTerm::from(Term::Var(*id))));

        let node_as_function = self.add_node(IncNode::new(
            Closure {
                body: as_function,
                env: env_orig,
            },
            IdentKind::Lambda,
            BindingType::Normal,
        ));

        env.insert(fresh_var, node_as_function);

        args.fold(as_function_closurized, |partial_app, arg| {
            RichTerm::from(Term::App(partial_app, arg))
        })
    }
}
