//! The Program store during evaluation.
//!
//! The data structure defined here relies on "Random-Access Lists" as described
//! in Chris Okasaki's 1995 ACM FPCA paper Purely Functional Random-Access Lists.
//!
//! [`Store`] allows for adding new environment layers efficiently, in O(1) time.
//! Indexing through the layers, however is an O(log n) operation, where n is the
//! number of layers. This, combined with De Bruijn indices, allows for precisely
//! resolving a symbol's binding environment.
//!
//! The implementation currently relies on the `fral` crate.

use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    fmt,
    rc::Rc,
};

use fral::rc::Fral;

use crate::{eval::lazy::Thunk, identifier::Ident, term::Symbol};

/// A store layer.
#[derive(Clone, Debug, PartialEq)]
pub struct Layer(HashMap<Ident, Thunk>);

// NOTE: The inner HashMap is meant to be replaced by an IntMap for faster lookups.
// Hence why the seemingly useless typewrapper.
impl Layer {
    /// Creates an empty layer.
    pub fn new() -> Self {
        Layer(HashMap::new())
    }

    /// Binds the `thunk` to `ident` in the layer.
    pub fn insert(&mut self, ident: Ident, thunk: Thunk) {
        let Layer(map) = self;
        map.insert(ident, thunk);
    }

    /// Creates an new layer containing bindings from both layers.
    pub fn merge(&self, other: &Self) -> Self {
        let Layer(left) = self;
        let Layer(right) = other;

        let mut map = HashMap::with_capacity(left.len() + right.len());
        map.extend(left.iter().map(|(i, t)| (i.clone(), t.clone())));
        map.extend(right.iter().map(|(i, t)| (i.clone(), t.clone())));

        Layer(map)
    }
}

impl FromIterator<(Ident, Thunk)> for Layer {
    fn from_iter<T: IntoIterator<Item = (Ident, Thunk)>>(iter: T) -> Self {
        Layer(iter.into_iter().collect())
    }
}

impl Extend<(Ident, Thunk)> for Layer {
    fn extend<T: IntoIterator<Item = (Ident, Thunk)>>(&mut self, iter: T) {
        let Layer(map) = self;
        map.extend(iter);
    }
}

/// The program store during evaluation.
#[derive(Clone, PartialEq)]
pub struct Store(Fral<RefCell<Layer>>);

impl Store {
    /// Creates a store with an empty layer.
    pub fn new() -> Self {
        Self::singleton(Layer::new())
    }

    /// Creates a store containing one layer.
    pub fn singleton(layer: Layer) -> Store {
        let fral = Fral::new();
        Store(fral.cons(RefCell::new(layer)))
    }

    /// Inserts an environment at the front of the list.
    #[must_use]
    pub fn push(&self, layer: Layer) -> Self {
        let Store(fral) = self;
        Store(fral.cons(RefCell::new(layer)))
    }

    /// Returns the thunk corresponding to the symbol.
    pub fn get(&self, sym: &Symbol) -> Option<Thunk> {
        // FIXME: it's better if this function doesn't clone the thunk,
        // as that needlessly increment the reference-count. But since
        // Fral returns an `Rc<T>` and not a `&T` we cannot do that, yet.

        let Store(fral) = self;

        fral.get(sym.level).and_then(|layer| {
            let Layer(map) = &*layer.borrow();
            map.get(&sym.ident).cloned()
        })
    }

    /// Merges the front layer of both stores.
    pub fn merge_fronts(self, other: Self) -> Self {
        let Store(left) = self;
        let Store(right) = other;

        left.get(0)
            .and_then(|lhead| {
                right.get(0).and_then(|rhead| {
                    Some(Store::singleton(lhead.borrow().merge(&*rhead.borrow())))
                })
            })
            .unwrap_or(Store::new())
    }

    /// Returns the front layer of the environment.
    pub fn with_front_mut<F, T>(&self, f: F) -> T
    where
        F: FnOnce(&mut Layer) -> T,
    {
        let Store(fral) = self;

        f(&mut fral.get(0).expect("empty store.").borrow_mut())
    }
}

impl fmt::Debug for Store {
    /// Dump the store to an XML representation.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Store(fral) = self;

        writeln!(f, "<store>")?;

        if let Some((head, tail)) = fral.uncons() {
            writeln!(f, "<addr>{:?}</addr>", std::rc::Rc::as_ptr(&head))?;

            writeln!(f, "<body>")?;
            for (k, v) in head.borrow().0.iter() {
                writeln!(f, "<item ident='{k}'>{v:?}</item>")?;
            }
            writeln!(f, "</body>")?;

            write!(f, "<prev>{:?}</prev>", Store(tail))?;
        }

        writeln!(f, "</store>")
    }
}
