//! Define the type of an identifier.
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::hash::Hash;

use crate::position::TermPos;

simple_counter::generate_counter!(GeneratedCounter, usize);
static INTERNER: Lazy<interner::Interner> = Lazy::new(interner::Interner::new);

#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
#[serde(into = "String", from = "String")]
pub struct Ident {
    symbol: interner::Symbol,
    pub pos: TermPos,
    generated: bool,
}

impl Ident {
    pub fn new_with_pos(label: impl AsRef<str>, pos: TermPos) -> Self {
        let generated = label.as_ref().starts_with(GEN_PREFIX);
        Self {
            symbol: INTERNER.intern(label),
            pos,
            generated,
        }
    }

    pub fn new(label: impl AsRef<str>) -> Self {
        Self::new_with_pos(label, TermPos::None)
    }

    pub fn generate() -> Self {
        Self::new(format!("{}{}", GEN_PREFIX, GeneratedCounter::next()))
    }

    pub fn label(&self) -> &str {
        INTERNER.lookup(self.symbol)
    }

    pub fn into_label(self) -> String {
        self.label().to_owned()
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
        self.symbol == other.symbol
    }
}

impl Eq for Ident {}

impl Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.symbol.hash(state);
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
        self.into_label()
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
    use std::sync::{Mutex, RwLock};

    use typed_arena::Arena;

    /// A symbol is a correspondance between an [Ident](super::Ident) and its string representation stored in the [Interner].
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Symbol(u32);

    /// The interner, which serves a double purpose: it pre-allocates space
    /// so that [Ident](super::Ident) labels are created faster
    /// and it makes it so that labels are stored only once, saving space.
    pub(crate) struct Interner<'a>(RwLock<InnerInterner<'a>>);

    impl<'a> Interner<'a> {
        /// Creates an empty [Interner].
        pub(crate) fn new() -> Self {
            Self(RwLock::new(InnerInterner::new()))
        }

        /// Stores a string inside the [Interner] if it does not exists, and returns the corresponding [Symbol].
        pub(crate) fn intern(&self, string: impl AsRef<str>) -> Symbol {
            self.0.write().unwrap().intern(string)
        }

        /// Looks up for the stored string corresponding to the [Symbol].
        ///
        /// This operation cannot fails since the only way to have a [Symbol] is to have [interned](Interner::intern)
        /// the corresponding string first.
        pub(crate) fn lookup(&self, sym: Symbol) -> &str {
            // SAFETY: We are making the returned &str lifetime the same as our struct,
            // which is okay here since the InnerInterner uses a typed_arena which prevents
            // deallocations, so the reference will be valid while the InnerInterner exists,
            // hence while the struct exists.
            unsafe { std::mem::transmute(self.0.read().unwrap().lookup(sym)) }
        }
    }

    /// The main part of the Interner.
    ///
    /// It is made out of 3 parts:
    /// - the arena, that preallocates space where strings are stored
    /// - the map, which prevents the arena from creating different [Symbols](Symbol) for the same string,
    /// - the vec, which allows for retrieving a string from a [Symbol].
    struct InnerInterner<'a> {
        arena: Mutex<Arena<u8>>,
        map: HashMap<&'a str, Symbol>,
        vec: Vec<&'a str>,
    }

    impl<'a> InnerInterner<'a> {
        /// Creates an empty [InnerInterner].
        fn new() -> Self {
            Self {
                arena: Mutex::new(Arena::new()),
                map: HashMap::new(),
                vec: Vec::new(),
            }
        }

        /// Stores a string inside the [InnerInterner] if it does not exists, and returns the corresponding [Symbol].
        fn intern(&mut self, string: impl AsRef<str>) -> Symbol {
            if let Some(sym) = self.map.get(string.as_ref()) {
                return *sym;
            }
            // SAFETY: here we are transmuting the reference lifetime:
            // &'arena str -> &'self str
            // This is okay since the lifetime of the arena is identical to the one of the struct.
            // It is also okay to use it from inside the mutex, since typed_arena does not allow deallocation,
            // so references are valid until the arena drop, which is tied to the struct drop.
            let in_string = unsafe {
                std::mem::transmute(self.arena.lock().unwrap().alloc_str(string.as_ref()))
            };
            let sym = Symbol(self.vec.len() as u32);
            self.vec.push(in_string);
            self.map.insert(in_string, sym);
            sym
        }
        /// Looks up for the stored string corresponding to the [Symbol].
        ///
        /// This operation cannot fails since the only way to have a [Symbol]
        /// is to have [interned](InnerInterner::intern) the corresponding string first.
        fn lookup(&self, sym: Symbol) -> &str {
            self.vec[sym.0 as usize]
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_intern_then_lookup() {
            let interner = Interner::new();
            let test_string = "test_string";
            let sym = interner.intern(test_string);
            assert_eq!(interner.lookup(sym), test_string);
        }

        #[test]
        fn test_intern_twice_has_same_symbol() {
            let interner = Interner::new();
            let test_string = "test_string";
            let sym1 = interner.intern(test_string);
            let sym2 = interner.intern(test_string);
            assert_eq!(sym1, sym2);
        }

        #[test]
        fn test_intern_two_different_has_different_symbols() {
            let interner = Interner::new();
            let sym1 = interner.intern("a");
            let sym2 = interner.intern("b");
            assert_ne!(sym1, sym2);
        }

        #[test]
        fn test_large_number_of_interns() {
            let interner = Interner::new();
            for i in 0..10000 {
                let i = i.to_string();
                let sym = interner.intern(&i);
                assert_eq!(i, interner.lookup(sym));
            }
            assert_eq!(10000, interner.0.read().unwrap().map.len());
            assert_eq!(10000, interner.0.read().unwrap().vec.len());
            // doing the same a second time should not add anything to the interner
            for i in 0..10000 {
                let i = i.to_string();
                let sym = interner.intern(&i);
                assert_eq!(i, interner.lookup(sym));
            }
            assert_eq!(10000, interner.0.read().unwrap().map.len());
            assert_eq!(10000, interner.0.read().unwrap().vec.len());
        }
    }
}
