//! Define the type of an identifier.
use serde::{Deserialize, Serialize};
use std::{fmt, hash::Hash};

use crate::position::TermPos;

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(into = "String", from = "String")]
pub struct Ident {
    pub label: String,
    pub pos: TermPos,
}

pub const UNIQUE_PREFIX: char = '%';

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
            label: String::from(val),
            pos: TermPos::None,
        }
    }
}

impl Into<String> for Ident {
    fn into(self) -> String {
        self.label
    }
}

impl Ident {
    pub fn is_generated(&self) -> bool {
        self.label.starts_with('%')
    }
}

impl Ident {
    pub fn is_unique(&self) -> bool {
        self.0.starts_with(UNIQUE_PREFIX)
    }
}
