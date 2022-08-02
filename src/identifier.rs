//! Define the type of an identifier.
use serde::{Deserialize, Serialize};
use std::{fmt, hash::Hash};

use crate::{interner::Symbol, position::TermPos};

/// Nickel string type.
/// Either a handle to a reference counted [`Symbol`] or an owned `str`.
#[derive(Debug, Clone, Eq, Ord)]
pub enum NickelStr {
    Shared(Symbol),
    Unique(Box<str>),
}

impl NickelStr {
    pub fn as_str(&self) -> &str {
        match self {
            NickelStr::Shared(s) => s.as_str(),
            NickelStr::Unique(s) => &*s,
        }
    }
}

impl fmt::Display for NickelStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NickelStr::Shared(s) => write!(f, "{}", s),
            NickelStr::Unique(s) => write!(f, "{}", s),
        }
    }
}

impl From<String> for NickelStr {
    fn from(s: String) -> Self {
        Self::Unique(Box::from(s))
    }
}

impl From<Symbol> for NickelStr {
    fn from(s: Symbol) -> Self {
        Self::Shared(s)
    }
}

impl PartialEq for NickelStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_str().eq(other.as_str())
    }
}

impl PartialOrd for NickelStr {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

// FIXME(fuzzypixelz): we have to hash the entire striing
// even if it's shared. Otherwise the hashes of `Symbol("x")`
// and `Box("x")` wouldn't be the same, as `Symbol` uses pointer hashing.
impl Hash for NickelStr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(into = "String", from = "String")]
pub struct Ident {
    pub label: NickelStr,
    pub pos: TermPos,
}

/// Special character used for generating fresh identifiers. It must be syntactically impossible to
/// use to write in a standard Nickel program, to avoid name clashes.
pub const GEN_PREFIX: char = '%';

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.label.partial_cmp(&other.label)
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.label.cmp(&other.label)
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.label == other.label
    }
}

impl Eq for Ident {}

impl Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.label.hash(state);
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.label)
    }
}

impl<F> From<F> for Ident
where
    String: From<F>,
{
    fn from(val: F) -> Self {
        Ident {
            label: NickelStr::Unique(Box::from(String::from(val))),
            pos: TermPos::None,
        }
    }
}

impl Into<String> for Ident {
    fn into(self) -> String {
        self.label.as_str().to_owned()
    }
}

impl Ident {
    pub fn is_generated(&self) -> bool {
        self.label.as_str().starts_with(GEN_PREFIX)
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.label.as_str()
    }
}
