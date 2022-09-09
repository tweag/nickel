//! Define the type of an identifier.
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use std::{fmt, hash::Hash};

use crate::position::TermPos;

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(into = "SmolStr", from = "SmolStr")]
pub struct Ident {
    pub label: SmolStr,
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
            label: SmolStr::new(String::from(val)),
            pos: TermPos::None,
        }
    }
}

impl Into<String> for Ident {
    fn into(self) -> String {
        self.label.to_string()
    }
}

impl Ident {
    pub fn is_generated(&self) -> bool {
        self.label.starts_with(GEN_PREFIX)
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.label
    }
}
