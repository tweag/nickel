//! Nickel string interner.

use std::collections::HashMap;
use std::rc::Rc;
use std::{fmt, hash::Hash};

#[derive(Debug, Clone)]
/// A handle to an interned string.
pub struct Symbol(Rc<str>);

impl Symbol {
    /// Resolve an interned `Symbol`.
    /// The interner isn't required and so its state needn't
    /// be passed around the evaluator.
    pub fn as_str(&self) -> &str {
        &*self.0
    }

    /// Create a Symbol from a String.
    pub fn new(string: String) -> Self {
        Self(Rc::from(string))
    }
}

// NOTE: `Rc`'s PartialEq implementation compares the underlying values.
// If we use that, cheap comparison on `Symbol` would fly out the window.
impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

// TODO: document why this is reasonable.
// See: https://doc.rust-lang.org/stable/std/hash/trait.Hash.html#hash-and-eq
impl Eq for Symbol {}

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl Ord for Symbol {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(&*self.0, state);
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
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
            map: HashMap::new(),
        }
    }

    /// Create an interner with initial capacity.
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            map: HashMap::with_capacity(cap),
        }
    }

    /// Intern a string, or get the existing `Symbol`
    // if the string happened to be interned before.
    pub fn intern(&mut self, string: &str) -> Symbol {
        match self.map.get(string) {
            Some(sym) => sym.clone(),
            None => {
                let sym = Symbol(Rc::from(string));
                self.map.insert(Box::from(string), sym.clone());
                sym
            }
        }
    }
}
