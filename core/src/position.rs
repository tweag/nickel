//! Define types of positions and position spans.
//!
//! The positions defined in this module are represented by the id of the corresponding source and
//! raw byte indices.  They are prefixed with Raw to differentiate them from codespan's types and
//! indicate that they do not store human friendly data like lines and columns.

// TODO(parser migration): compatibility shim
pub use nickel_lang_parser::position::{RawPos, RawSpan, TermPos};

/// An index into the position table.
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct PosIdx(u32);

impl From<PosIdx> for usize {
    fn from(value: PosIdx) -> Self {
        usize::try_from(value.0)
            .expect("Nickel doesn't support architecture with pointer size smaller than 32bits")
    }
}

impl PosIdx {
    /// A special value indicating that an value doesn't have a position defined. This is the first
    /// available position index for values.
    pub const NONE: PosIdx = Self(0);

    /// Returns a new position index pointing to the same position but tagged as inherited,
    /// if it wasn't already. If `self` refers to a position that is already inherited or `None`,
    /// it is returned unchanged.
    pub fn to_inherited(self, table: &mut PosTable) -> Self {
        let pos = table.get(self);

        if let TermPos::Original(raw_span) = pos {
            table.push(TermPos::Inherited(raw_span))
        } else {
            self
        }
    }

    /// Same as `usize::from(self)`, but as a `const fn`.
    pub const fn to_usize(self) -> usize {
        self.0 as usize
    }
}

impl PosIdx {
    /// Creates a position index from a usize, truncating the higher bits if set.
    pub fn from_usize_truncate(value: usize) -> Self {
        Self(value as u32)
    }
}

// While we could derive this instance (as `NONE` is just `usize::default()`), it's safer to make
// sure the `Default` instance and `NONE` do agree.
impl Default for PosIdx {
    fn default() -> Self {
        Self::NONE
    }
}

/// An immutable table storing the position of values.
#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct PosTable {
    positions: Vec<TermPos>,
}

impl PosTable {
    pub fn new() -> Self {
        // We always populate the first entry with `TermPos::None`, so that we can safely use the
        // index `0` (`PosIdx::NONE`) to mean unintialized position, without having to special case
        // the undefined position.
        Self {
            positions: vec![TermPos::None],
        }
    }

    /// Inserts a new position for value and returns its index.
    pub fn push(&mut self, pos: TermPos) -> PosIdx {
        let next = self.positions.len();
        self.positions.push(pos);
        PosIdx(u32::try_from(next).expect("maximum number of positions reached for values"))
    }

    /// Returns the position at index `idx`.
    pub fn get(&self, idx: impl Into<PosIdx>) -> TermPos {
        self.positions[usize::try_from(idx.into().0)
            .expect("Nickel doesn't support architecture with pointer size smaller than 32bits")]
    }
}
