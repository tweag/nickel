//! Define the type of an identifier.
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;
use std::sync::{Arc, Mutex};

use crate::position::TermPos;

static INTERNER: Lazy<Mutex<interner::Interner>> =
    Lazy::new(|| Mutex::new(interner::Interner::new()));

#[derive(Debug, Clone, Default, Deserialize, Serialize)]
#[serde(into = "String", from = "String")]
pub struct Ident {
    label: Arc<String>,
    pos: TermPos,
}

impl Ident {
    pub fn new_with_pos(label: impl AsRef<str>, pos: TermPos) -> Self {
        Self {
            label: INTERNER.lock().unwrap().get_or_intern(label),
            pos,
        }
    }

    pub fn new(label: impl AsRef<str>) -> Self {
        Self::new_with_pos(label, TermPos::None)
    }

    pub fn label(&self) -> &str {
        &self.label
    }

    pub fn pos(&self) -> TermPos {
        self.pos
    }
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
        Self::new(String::from(val))
    }
}

impl Into<String> for Ident {
    fn into(self) -> String {
        self.label().to_string()
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

mod interner {
    use std::collections::HashMap;
    use std::sync::Arc;

    #[derive(Default)]
    pub struct Interner {
        map: HashMap<String, Arc<String>>, // we need Arc as this struct will be declared static
    }

    impl Interner {
        pub(crate) fn new() -> Self {
            Self {
                map: HashMap::new(),
            }
        }

        pub(crate) fn get_or_intern(&mut self, string: impl AsRef<str>) -> Arc<String> {
            if !self.map.contains_key(string.as_ref()) {
                let string = string.as_ref().to_string();
                self.map.insert(string.clone(), Arc::new(string));
            }
            self.map[string.as_ref()].clone()
        }
    }
}
