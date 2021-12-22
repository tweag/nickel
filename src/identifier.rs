//! Define the type of an identifier.
use serde::{Deserialize, Serialize};
use std::{fmt, hash::Hash};

use crate::position::TermPos;

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(into = "String", from = "String")]
pub struct Ident(pub String, pub TermPos);

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for Ident {}

impl Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Ident(ident, _) = self;
        write!(f, "{}", ident)
    }
}

impl<F> From<F> for Ident
where
    String: From<F>,
{
    fn from(val: F) -> Self {
        Ident(String::from(val), TermPos::None)
    }
}

impl Into<String> for Ident {
    fn into(self) -> String {
        self.0
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        &self.0
    }
}
