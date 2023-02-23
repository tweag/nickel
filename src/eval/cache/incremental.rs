/// A [Cache] implementation with incremental computation features.

use super::{BlackholedError, Cache, CacheIndex, Closure, Environment, IdentKind};
use crate::{
    identifier::Ident,
    term::{record::FieldDeps, BindingType, RichTerm, Term},
};

/// The evaluation state of an [IncNode].
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub enum IncNodeState {
    #[default]
    Suspended,
    Blackholed,
    Evaluated,
}

/// A node in the dependent computation graph stored in [IncCache].
#[derive(Debug, Clone)]
pub struct IncNode {
    // The original closure.
    orig: Closure,
    // The cached value of the closure.
    cached: Option<Closure>,
    kind: IdentKind,
    bty: BindingType,
    // The state of the node.
    state: IncNodeState,
    // Backlinks to nodes depending on this node.
    backlinks: Vec<CacheIndex>,
}

impl IncNode {
    fn new(clos: Closure, kind: IdentKind, bty: BindingType) -> Self {
        IncNode {
            orig: clos,
            cached: None,
            kind,
            bty,
            state: IncNodeState::default(),
            backlinks: Vec::new(),
        }
    }
}

/// A [Cache] implementation supporting incremental computation features.
/// Stores a dependent computation graph to keep track of what needs to be recomputed.
#[derive(Debug, Clone)]
pub struct IncCache {
    store: Vec<IncNode>,
    next: CacheIndex,
}

impl IncCache {
    fn add_node(&mut self, node: IncNode) -> CacheIndex {
        let idx = self.next;
        self.store.insert(idx, node);
        self.next += 1;

        idx
    }

    fn revnode_as_explicit_fun<'a, I>(node: &IncNode, args: I) -> IncNode
    where
        I: DoubleEndedIterator<Item = &'a Ident>,
    {
        match &node.bty {
            BindingType::Revertible(deps) if !deps.is_empty() => {
                let Closure { body, env } = node.orig.clone();

                // Build a list of the arguments that the function will need in the same order as
                // the original iterator. If the identifiers inside `args` are `a`, `b` and `c`, in
                // that order, we want to build `fun a => (fun b => (fun c => body))`. We thus need a
                // reverse fold.
                let as_function =
                    args.rfold(body, |built, id| RichTerm::from(Term::Fun(*id, built)));

                IncNode::new(
                    Closure {
                        body: as_function,
                        env,
                    },
                    node.kind,
                    node.bty.clone(),
                )
            }
            _ => node.clone(),
        }
    }
}

impl Cache for IncCache {
    type UpdateIndex = CacheIndex;

    fn new() -> Self {
        IncCache {
            store: Vec::new(),
            next: 0,
        }
    }

    fn get(&self, idx: CacheIndex) -> Closure {
        let node = self.store.get(idx).unwrap();

        node.cached.clone().unwrap_or(node.orig.clone())
    }

    fn get_update_index(
        &mut self,
        idx: &mut CacheIndex,
    ) -> Result<Option<Self::UpdateIndex>, BlackholedError> {
        let node = self.store.get_mut(*idx).unwrap();

        if node.state == IncNodeState::Blackholed {
            Err(BlackholedError)
        } else if node.state == IncNodeState::Evaluated {
            Ok(None)
        } else {
            node.state = IncNodeState::Blackholed;
            Ok(Some(*idx))
        }
    }

    fn add(&mut self, clos: Closure, kind: IdentKind, bty: BindingType) -> CacheIndex {
        let node = IncNode::new(clos, kind, bty);

        self.add_node(node)
    }

    fn update(&mut self, clos: Closure, idx: Self::UpdateIndex) {
        let node = self.store.get_mut(idx).unwrap();

        node.cached = Some(clos);
        node.state = IncNodeState::Evaluated;
    }

    fn revert(&mut self, idx: &CacheIndex) -> CacheIndex {
        let node = self.store.get(*idx).unwrap();

        let new_node = match node.bty.clone() {
            BindingType::Normal => node.clone(),
            BindingType::Revertible(_) => {
                IncNode::new(node.orig.clone(), node.kind, node.bty.clone())
            }
        };

        self.add_node(new_node)
    }

    fn ident_kind(&self, idx: &CacheIndex) -> IdentKind {
        self.store.get(*idx).unwrap().kind
    }

    fn deps(&self, idx: &CacheIndex) -> Option<FieldDeps> {
        let node = self.store.get(*idx).unwrap();
        match node.bty.clone() {
            BindingType::Normal => None,
            BindingType::Revertible(deps) => Some(deps),
        }
    }

    fn get_then<T, F: FnOnce(&Closure) -> T>(&self, idx: CacheIndex, f: F) -> T {
        f(&self.get(idx))
    }

    fn patch<F: Fn(&mut Closure)>(&mut self, idx: CacheIndex, f: F) {
        let node = self.store.get_mut(idx).unwrap();

        f(&mut node.orig);
        node.cached.as_mut().map(|mut clos| f(&mut clos));
    }

    fn make_update_index(
        &mut self,
        idx: &mut CacheIndex,
    ) -> Result<Self::UpdateIndex, BlackholedError> {
        let node = self.store.get_mut(*idx).unwrap();

        if node.state == IncNodeState::Blackholed {
            return Err(BlackholedError);
        } else {
            node.state = IncNodeState::Blackholed;

            Ok(*idx)
        }
    }

    fn reset_index_state(&mut self, idx: &mut Self::UpdateIndex) {
        let node = self.store.get_mut(*idx).unwrap();

        node.state = IncNodeState::default();
    }

    fn map_at_index<F: FnMut(&mut Self, &Closure) -> Closure>(
        &mut self,
        idx: &CacheIndex,
        mut f: F,
    ) -> CacheIndex {
        let node = self.store.get(*idx).unwrap().clone();

        let new_node = IncNode {
            orig: f(self, &node.orig),
            cached: node.cached.clone().map(|clos| f(self, &clos)),
            kind: node.kind,
            bty: node.bty.clone(),
            state: node.state,
            backlinks: node.backlinks.clone(),
        };

        self.add_node(new_node)
    }

    fn build_cached(&mut self, idx: &mut CacheIndex, rec_env: &[(Ident, CacheIndex)]) {
        let node = self.store.get_mut(*idx).unwrap();

        if node.cached.is_some() {
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
        let node = self.store.get(idx).unwrap();

        let mut deps_filter: Box<dyn FnMut(&&Ident) -> bool> = match node.bty.clone() {
            BindingType::Revertible(FieldDeps::Known(deps)) => {
                Box::new(move |id: &&Ident| deps.contains(id))
            }
            BindingType::Revertible(FieldDeps::Unknown) => Box::new(|_: &&Ident| true),
            BindingType::Normal => Box::new(|_: &&Ident| false),
        };

        let node_as_function = self.add_node(IncCache::revnode_as_explicit_fun(
            node,
            fields.clone().filter(&mut deps_filter),
        ));

        let fresh_var = Ident::fresh();
        env.insert(fresh_var, node_as_function);

        let as_function_closurized = RichTerm::from(Term::Var(fresh_var));
        let args = fields.filter_map(|id| deps_filter(&id).then(|| RichTerm::from(Term::Var(*id))));

        args.fold(as_function_closurized, |partial_app, arg| {
            RichTerm::from(Term::App(partial_app, arg))
        })
    }
}
