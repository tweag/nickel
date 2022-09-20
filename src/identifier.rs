//! Define the type of an identifier.
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;
use std::sync::Mutex;

use crate::position::TermPos;

static INTERNER: Lazy<Mutex<interner::Interner>> =
    Lazy::new(|| Mutex::new(interner::Interner::new()));

#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
#[serde(into = "String", from = "String")]
pub struct Ident {
    label: interner::Symbol,
    pos: TermPos,
    generated: bool,
}

impl Ident {
    pub fn new_with_pos(label: impl AsRef<str>, pos: TermPos) -> Self {
        let generated = label.as_ref().starts_with(GEN_PREFIX);
        Self {
            label: INTERNER.lock().unwrap().intern(label),
            pos,
            generated,
        }
    }

    pub fn new(label: impl AsRef<str>) -> Self {
        Self::new_with_pos(label, TermPos::None)
    }

    pub fn label(&self) -> &str {
        INTERNER.lock().unwrap().lookup(self.label)
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
        self.label().partial_cmp(other.label())
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.label().cmp(other.label())
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
        write!(f, "{}", self.label())
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
        self.as_ref().to_owned()
    }
}

impl Ident {
    pub fn is_generated(&self) -> bool {
        self.generated
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.label()
    }
}

mod interner {
    use std::collections::HashMap;

    use typed_arena::Arena;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub(crate) struct Symbol(u32);

    pub(crate) struct Interner {
        map: HashMap<&'static str, Symbol>,
        vec: Vec<&'static str>,
        arena: Arena<u8>,
    }

    impl Interner {
        pub(crate) fn new() -> Self {
            Self {
                map: HashMap::new(),
                vec: Vec::new(),
                arena: Arena::new(),
            }
        }

        pub(crate) fn intern(&mut self, string: impl AsRef<str>) -> Symbol {
            if let Some(sym) = self.map.get(string.as_ref()) {
                return *sym;
            }
            let string_in = unsafe { &*(self.arena.alloc_str(string.as_ref()) as *const str) };
            let sym = Symbol(self.vec.len() as u32);
            self.vec.push(string_in);
            self.map.insert(string_in, sym);
            sym
        }

        pub(crate) fn lookup(&self, sym: Symbol) -> &'static str {
            self.vec[sym.0 as usize]
        }
    }
}
