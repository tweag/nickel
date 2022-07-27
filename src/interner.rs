//! Nickel string interner.

use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug, Clone)]
/// A handle to an interned string.
pub struct Symbol(Arc<str>);

impl Symbol {
    /// Resolve an interned `Symbol`.
    /// The interner isn't required and so its state needn't 
    /// be passed around the evaluator.
    pub fn resolve(&self) -> &str {
        &*self.0
    }
}

// NOTE: `Rc`'s PartialEq implementation compares the underlying values.
// If we use that, cheap comparison on `Symbol` would fly out the window.
impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

/// Interner structure.
#[derive(Debug)]
pub struct Interner {
    /// This map acts as a way to deduplicate
    map: HashMap<Box<str>, Symbol>,
}

impl Interner {
    /// Create an interner.
    pub fn new() -> Self {
        Self {
            map: HashMap::new()
        }
    }

    /// Create an interner with initial capacity.
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            map: HashMap::with_capacity(cap)
        }
    }

    /// Intern a string, or get the existing `Symbol` 
    // if the string happened to be interned before.
    pub fn intern(&mut self, string: &str) -> Symbol {
        match self.map.get(string) {
            Some(sym) => sym.clone(),
            None => {
                let sym = Symbol(Arc::from(string));
                self.map.insert(Box::from(string), sym.clone());
                sym
            }
        }
    }
}
