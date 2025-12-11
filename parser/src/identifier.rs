//! Define the type of an identifier.
use serde::{Deserialize, Serialize};
use std::{
    borrow::Borrow,
    fmt::{self, Debug},
    hash::Hash,
    sync::{
        LazyLock,
        atomic::{AtomicUsize, Ordering},
    },
};

use crate::{metrics::increment, position::TermPos};

static INTERNER: LazyLock<interner::Interner> = LazyLock::new(interner::Interner::new);
static COUNTER: AtomicUsize = AtomicUsize::new(0);

/// An interned identifier.
//
// Implementation-wise, this is just a wrapper around interner::Symbol that uses a hard-coded,
// static `Interner`.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(into = "&'static str", from = "String")]
pub struct Ident(interner::Symbol);

impl Ident {
    pub fn new(s: impl AsRef<str>) -> Self {
        Self(INTERNER.get_or_intern(s.as_ref()))
    }

    /// Return the string representation of this identifier.
    pub fn label(&self) -> &'static str {
        INTERNER.lookup(self.0)
    }

    pub fn into_label(self) -> String {
        self.label().to_owned()
    }

    /// Look up a generated identifier by name, panicking if it doesn't exist.
    ///
    /// This is extremely slow because it scans over all symbols. It's only used
    /// for tests that look at pretty-printed output.
    ///
    /// Public only because we use it in tests outside of `nickel_lang_parser`.
    #[doc(hidden)]
    pub fn find_generated(s: &str) -> Self {
        Self(INTERNER.find_generated(s))
    }

    /// Create a new fresh identifier. This identifier is unique and is
    /// guaranteed not to collide with any identifier defined before.
    ///
    /// Generated identifiers start with a special prefix that isn't valid
    /// for Nickel identifiers. This doesn't actually guarantee that the
    /// [label][Self::label] of a fresh identifier will always be different
    /// from the label of a normal identifier created by [`Ident::new`]: there
    /// are ways to introduce normal identifiers that aren't parsed as Nickel
    /// identifiers (for example, `std.record.insert "%1" 42 {}` causes an
    /// identifier to be created with the name "%1").
    ///
    /// The consequence of all this is that different `Ident`s can have the
    /// same label. `Ident::new("%1")` and `Ident::fresh()` might both generate
    /// idents with label "%1", but they will be different idents.
    pub fn fresh() -> Self {
        increment!("Ident::fresh");
        Self(INTERNER.intern_generated(format!(
            "{}{}",
            GEN_PREFIX,
            COUNTER.fetch_add(1, Ordering::Relaxed)
        )))
    }

    /// Attaches a position to this identifier, making it a `LocIdent`.
    pub fn spanned(self, pos: TermPos) -> LocIdent {
        LocIdent { ident: self, pos }
    }

    /// Checks whether this identifier was generated with [`Ident::fresh`].
    pub fn is_generated(&self) -> bool {
        INTERNER.is_generated(self.0)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.label())
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_generated() {
            write!(f, "`{}` (generated)", self.label())
        } else {
            write!(f, "`{}`", self.label())
        }
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
}

impl LocIdent {
    pub fn new_with_pos(label: impl AsRef<str>, pos: TermPos) -> Self {
        Self {
            ident: Ident::new(label),
            pos,
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

    /// Checks whether this identifier was generated by [`Ident::fresh`].
    ///
    /// Note that this is not optimized for speed: it involves taking a lock
    /// and looking up a table.
    pub fn is_generated(&self) -> bool {
        self.ident.is_generated()
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
        self.0.0.cmp(&other.0.0)
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
            Self(RwLock::new(InnerInterner::empty()))
        }

        /// Stores a string inside the [Interner] if it does not exists, and returns the
        /// corresponding [Symbol].
        pub(crate) fn get_or_intern(&self, string: impl AsRef<str>) -> Symbol {
            self.0.write().unwrap().get_or_intern(string)
        }

        /// Stores the name of a generated string in the [Interner].
        ///
        /// This always generates a new symbol and does not check for duplicate strings.
        pub(crate) fn intern_generated(&self, string: impl AsRef<str>) -> Symbol {
            self.0.write().unwrap().intern(string, true)
        }

        /// Looks up the stored string corresponding to the [Symbol].
        ///
        /// This operation cannot fail since the only way to have a [Symbol] is to have
        /// [interned](Interner::intern) the corresponding string first.
        pub(crate) fn lookup(&self, sym: Symbol) -> &str {
            // SAFETY: Here we are transmuting the reference lifetime: &'lock str -> &'slf str.
            // This is okay because InnerInterner::lookup guarantees stable references, and we
            // never replace our InnerInterner.
            unsafe { std::mem::transmute::<&'_ str, &'_ str>(self.0.read().unwrap().lookup(sym)) }
        }

        /// Look up a generated identifier by name, panicking if it doesn't exist.
        ///
        /// This is extremely slow because it scans over all symbols. It's only used
        /// for tests that look at pretty-printed output.
        pub(crate) fn find_generated(&self, s: &str) -> Symbol {
            let inner = self.0.read().unwrap();
            inner.with(|inner| {
                let idx = (0..inner.vec.len())
                    .find(|&idx| inner.generated[idx] && inner.vec[idx] == s)
                    .unwrap();
                Symbol(idx as u32)
            })
        }

        pub(crate) fn is_generated(&self, sym: Symbol) -> bool {
            self.0.read().unwrap().is_generated(sym)
        }
    }

    /// The main part of the Interner.
    #[ouroboros::self_referencing]
    struct InnerInterner {
        /// Preallocates space where strings are stored.
        arena: Mutex<Arena<u8>>,

        /// Prevents the arena from creating different [Symbols](Symbol) for the same string.
        #[borrows(arena)]
        #[covariant]
        map: HashMap<&'this str, Symbol>,

        /// Allows retrieving a string from a [Symbol].
        #[borrows(arena)]
        #[covariant]
        vec: Vec<&'this str>,

        /// Allows checking whether an identifier was generated.
        generated: Vec<bool>,
    }

    impl InnerInterner {
        /// Creates an empty [InnerInterner].
        fn empty() -> Self {
            Self::new(
                Mutex::new(Arena::new()),
                |_arena| HashMap::new(),
                |_arena| Vec::new(),
                Vec::new(),
            )
        }

        /// Stores a string inside the [InnerInterner] if it does not exists, and returns the
        /// corresponding [Symbol].
        fn get_or_intern(&mut self, string: impl AsRef<str>) -> Symbol {
            if let Some(sym) = self.borrow_map().get(string.as_ref()) {
                return *sym;
            }
            self.intern(string, false)
        }

        /// Interns a string without checking for deduplication.
        fn intern(&mut self, string: impl AsRef<str>, generated: bool) -> Symbol {
            // SAFETY: Here we are transmuting the reference lifetime: &'lock str -> &'slf str.
            // This is okay because references to data in the arena are valid until the arena
            // is destroyed.
            let in_string = unsafe {
                std::mem::transmute::<&'_ str, &'_ str>(
                    self.borrow_arena()
                        .lock()
                        .unwrap()
                        .alloc_str(string.as_ref()),
                )
            };
            let sym = Symbol(self.borrow_vec().len() as u32);
            self.with_vec_mut(|v| v.push(in_string));
            self.with_generated_mut(|g| g.push(generated));
            // We only insert non-generated ids into the map, because we don't
            // want deduplication to ever hit a generated id: if someone does
            // `Ident::new("%0") they should get a non-generated id.
            if !generated {
                self.with_map_mut(|m| m.insert(in_string, sym));
            }
            sym
        }

        /// Looks up for the stored string corresponding to the [Symbol].
        ///
        /// This operation cannot fail since the only way to have a [Symbol]
        /// is to have [interned](InnerInterner::intern) the corresponding string first.
        ///
        /// References returned by this method are valid until this `InnerInterner` is
        /// destroyed: they won't be invalidated by, for example, [`Self::intern`].
        fn lookup(&self, sym: Symbol) -> &str {
            self.borrow_vec()[sym.0 as usize]
        }

        fn is_generated(&self, sym: Symbol) -> bool {
            self.borrow_generated()[sym.0 as usize]
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_intern_then_lookup() {
            let interner = Interner::new();
            let test_string = "test_string";
            let sym = interner.get_or_intern(test_string);
            assert_eq!(interner.lookup(sym), test_string);
        }

        #[test]
        fn test_intern_twice_has_same_symbol() {
            let interner = Interner::new();
            let test_string = "test_string";
            let sym1 = interner.get_or_intern(test_string);
            let sym2 = interner.get_or_intern(test_string);
            assert_eq!(sym1, sym2);
        }

        #[test]
        fn test_intern_two_different_has_different_symbols() {
            let interner = Interner::new();
            let sym1 = interner.get_or_intern("a");
            let sym2 = interner.get_or_intern("b");
            assert_ne!(sym1, sym2);
        }

        #[test]
        fn test_large_number_of_interns() {
            let interner = Interner::new();
            for i in 0..10000 {
                let i = i.to_string();
                let sym = interner.get_or_intern(&i);
                assert_eq!(i, interner.lookup(sym));
            }
            assert_eq!(10000, interner.0.read().unwrap().borrow_map().len());
            assert_eq!(10000, interner.0.read().unwrap().borrow_vec().len());
            // doing the same a second time should not add anything to the interner
            for i in 0..10000 {
                let i = i.to_string();
                let sym = interner.get_or_intern(&i);
                assert_eq!(i, interner.lookup(sym));
            }
            assert_eq!(10000, interner.0.read().unwrap().borrow_map().len());
            assert_eq!(10000, interner.0.read().unwrap().borrow_vec().len());
        }
    }
}
