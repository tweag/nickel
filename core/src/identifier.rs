//! Define the type of an identifier.
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Borrow,
    fmt::{self, Debug},
    hash::Hash,
};

use crate::{metrics::increment, position::TermPos, term::string::NickelString};

simple_counter::generate_counter!(GeneratedCounter, usize);
static INTERNER: Lazy<interner::Interner> = Lazy::new(interner::Interner::new);

/// An interned identifier.
//
// Implementation-wise, this is just a wrapper around interner::Symbol that uses a hard-coded,
// static `Interner`.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(into = "&'static str", from = "String")]
pub struct Ident(interner::Symbol);

impl Ident {
    pub fn new(s: impl AsRef<str>) -> Self {
        Self(INTERNER.intern(s.as_ref()))
    }

    /// Return the string representation of this identifier.
    pub fn label(&self) -> &'static str {
        INTERNER.lookup(self.0)
    }

    pub fn into_label(self) -> String {
        self.label().to_owned()
    }

    /// Create a new fresh identifier. This identifier is unique and is guaranteed not to collide
    /// with any identifier defined before. Generated identifiers start with a special prefix that
    /// can't be used by normal, user-defined identifiers.
    ///
    /// FIXME: the comment above isn't true: the identifier isn't guaranteed to
    /// be unique because there are ways to introduce identifiers that aren't
    /// parsed as Nickel identifiers. For example, `std.record.insert "%1" 42 {}` causes
    /// an identifier to be created with the name "%1".
    pub fn fresh() -> Self {
        increment!("Ident::fresh");
        Self::new(format!("{}{}", GEN_PREFIX, GeneratedCounter::next()))
    }

    /// Attaches a position to this identifier, making it a `LocIdent`.
    pub fn spanned(self, pos: TermPos) -> LocIdent {
        LocIdent {
            ident: self,
            pos,
            generated: self.label().starts_with(GEN_PREFIX),
        }
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.label())
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "`{}`", self.label())
    }
}

impl From<Ident> for LocIdent {
    fn from(ident: Ident) -> Self {
        ident.spanned(TermPos::None)
    }
}

impl From<&LocIdent> for Ident {
    fn from(ident: &LocIdent) -> Self {
        ident.ident()
    }
}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.label().cmp(other.label())
    }
}

impl From<Ident> for NickelString {
    fn from(sym: Ident) -> Self {
        sym.to_string().into()
    }
}

impl<'a> From<&'a str> for Ident {
    fn from(s: &'a str) -> Self {
        Ident::new(s)
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident::new(s)
    }
}

impl From<Ident> for &'static str {
    fn from(id: Ident) -> &'static str {
        id.label()
    }
}

/// An identifier with a location.
///
/// The location is ignored for equality comparison and hashing; it's mainly
/// intended for error messages.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(into = "String", from = "String")]
pub struct LocIdent {
    ident: Ident,
    pub pos: TermPos,
    generated: bool,
}

impl LocIdent {
    pub fn new_with_pos(label: impl AsRef<str>, pos: TermPos) -> Self {
        let generated = label.as_ref().starts_with(GEN_PREFIX);
        Self {
            ident: Ident::new(label),
            pos,
            generated,
        }
    }

    pub fn new(label: impl AsRef<str>) -> Self {
        Self::new_with_pos(label, TermPos::None)
    }

    /// Create an identifier with the same label as this one, but a specified position.
    pub fn with_pos(self, pos: TermPos) -> LocIdent {
        LocIdent { pos, ..self }
    }

    /// Create a fresh identifier with no position. See [Ident::fresh].
    pub fn fresh() -> Self {
        Ident::fresh().into()
    }

    /// Return the identifier without its position.
    pub fn ident(&self) -> Ident {
        self.ident
    }

    /// Return the string representation of this identifier.
    pub fn label(&self) -> &'static str {
        self.ident.label()
    }

    pub fn into_label(self) -> String {
        self.label().to_owned()
    }
}

/// Special character used for generating fresh identifiers. It must be syntactically impossible to
/// use to write in a standard Nickel program, to avoid name clashes.
pub const GEN_PREFIX: char = '%';

impl PartialOrd for LocIdent {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LocIdent {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.label().cmp(other.label())
    }
}

impl PartialEq for LocIdent {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
    }
}

impl Eq for LocIdent {}

impl Hash for LocIdent {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ident.hash(state)
    }
}

impl Borrow<Ident> for LocIdent {
    fn borrow(&self) -> &Ident {
        &self.ident
    }
}

impl fmt::Display for LocIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.label())
    }
}

/// Wrapper around [Ident] with a fast ordering function that only compares the underlying symbols.
/// Useful when a bunch of idents need to be sorted for algorithmic reasons, but one doesn't need
/// the actual natural order on strings nor care about the specific order.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FastOrdIdent(pub Ident);

impl PartialOrd for FastOrdIdent {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FastOrdIdent {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0 .0.cmp(&other.0 .0)
    }
}

impl<'a> From<&'a str> for LocIdent {
    fn from(s: &'a str) -> Self {
        LocIdent::new(s)
    }
}

impl From<String> for LocIdent {
    fn from(s: String) -> Self {
        LocIdent::new(s)
    }
}

impl From<LocIdent> for &'static str {
    fn from(id: LocIdent) -> &'static str {
        id.label()
    }
}

// TODO: among all the `From` impls here, this is the only one that allocates.
// Allocations aren't forbidden in `From` (e.g. `String: From<&str>`), but it
// would still be nice to get rid of this implicit allocation. It's mainly used
// in `Term` right now.
impl From<LocIdent> for String {
    fn from(id: LocIdent) -> String {
        id.label().to_owned()
    }
}

impl LocIdent {
    pub fn is_generated(&self) -> bool {
        self.generated
    }
}

impl AsRef<str> for LocIdent {
    fn as_ref(&self) -> &str {
        self.label()
    }
}

mod interner {
    use std::collections::HashMap;
    use std::sync::{Mutex, RwLock};

    use typed_arena::Arena;

    /// A symbol is a correspondence between an [Ident](super::Ident) and its string representation
    /// stored in the [Interner].
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub struct Symbol(u32);

    /// The interner, which serves a double purpose: it pre-allocates space
    /// so that [Ident](super::Ident) labels are created faster
    /// and it makes it so that labels are stored only once, saving space.
    pub(crate) struct Interner(RwLock<InnerInterner>);

    impl Interner {
        /// Creates an empty [Interner].
        pub(crate) fn new() -> Self {
            Self(RwLock::new(InnerInterner::new()))
        }

        /// Stores a string inside the [Interner] if it does not exists, and returns the
        /// corresponding [Symbol].
        pub(crate) fn intern(&self, string: impl AsRef<str>) -> Symbol {
            self.0.write().unwrap().intern(string)
        }

        /// Looks up the stored string corresponding to the [Symbol].
        ///
        /// This operation cannot fail since the only way to have a [Symbol] is to have
        /// [interned](Interner::intern) the corresponding string first.
        pub(crate) fn lookup(&self, sym: Symbol) -> &str {
            self.0.read().unwrap().lookup(sym)
        }
    }

    /// The main part of the Interner.
    struct InnerInterner {
        /// Preallocates space where strings are stored.
        arena: Mutex<&'static mut Arena<u8>>,

        /// Prevents the arena from creating different [Symbols](Symbol) for the same string.
        map: HashMap<&'static str, Symbol>,

        /// Allows retrieving a string from a [Symbol].
        vec: Vec<&'static str>,
    }

    impl InnerInterner {
        /// Creates an empty [InnerInterner].
        fn new() -> Self {
            Self {
                arena: Mutex::new(Box::leak(Box::new(Arena::new()))),
                map: HashMap::new(),
                vec: Vec::new(),
            }
        }

        /// Stores a string inside the [InnerInterner] if it does not exists, and returns the
        /// corresponding [Symbol].
        fn intern(&mut self, string: impl AsRef<str>) -> Symbol {
            if let Some(sym) = self.map.get(string.as_ref()) {
                return *sym;
            }
            // SAFETY: Here we are transmuting the reference lifetime: &'lock str -> &'static str.
            // This is okay because references to data in the arena are valid until the arena
            // is destroyed, and we've leaked the arena.
            let in_string = unsafe {
                std::mem::transmute::<&'_ str, &'static str>(
                    self.arena.lock().unwrap().alloc_str(string.as_ref()),
                )
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
        fn lookup(&self, sym: Symbol) -> &'static str {
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
