//! Define types of positions and position spans.
//!
//! The positions defined in this module are represented by the id of the corresponding source and
//! raw byte indices.  They are prefixed with Raw to differentiate them from codespan's types and
//! indicate that they do not store human friendly data like lines and columns.

pub use nickel_lang_parser::position::{RawPos, RawSpan, TermPos};

/// An index into the position table.
#[derive(Clone, Copy, Eq, PartialEq, Debug, rkyv::Archive)]
pub struct PosIdx(u32);

impl From<PosIdx> for usize {
    fn from(value: PosIdx) -> Self {
        usize::try_from(value.0)
            .expect("Nickel doesn't support architecture with pointer size smaller than 32bits")
    }
}

impl PosIdx {
    /// A special value indicating that an value doesn't have a position defined.
    pub const NONE: PosIdx = Self(u32::MAX);
    /// We use the highest-order bit to indicate that the position is inherited.
    const INHERITED_BIT: u32 = (1 << 31);
    /// Excluding the highest-order bit gives us a mask turning a `PosIdx` into
    /// and index into a `PosTable`.
    const IDX_MASK: u32 = PosIdx::INHERITED_BIT - 1;
    /// Don't allow `0b0111...1` as an index, because making it inherited would
    /// give us `PosIdx::NONE`. The biggest allowed index is `0b0111...110`.
    const MAX: u32 = PosIdx::INHERITED_BIT - 2;

    /// Returns a new position index pointing to the same position but tagged as inherited,
    /// if it wasn't already. If `self` refers to a position that is already inherited or `None`,
    /// it is returned unchanged.
    pub fn to_inherited(self) -> Self {
        if self != PosIdx::NONE {
            PosIdx(self.0 | PosIdx::INHERITED_BIT)
        } else {
            self
        }
    }

    fn is_inherited(self) -> bool {
        self != PosIdx::NONE && (self.0 & PosIdx::INHERITED_BIT) != 0
    }

    /// Same as `usize::from(self)`, but as a `const fn`.
    pub const fn to_usize(self) -> usize {
        self.0 as usize
    }

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
    positions: Vec<RawSpan>,
}

impl PosTable {
    pub fn new() -> Self {
        // We always populate the first entry with `TermPos::None`, so that we can safely use the
        // index `0` (`PosIdx::NONE`) to mean unintialized position, without having to special case
        // the undefined position.
        Self { positions: vec![] }
    }

    /// Inserts a new position for value and returns its index.
    pub fn push(&mut self, pos: TermPos) -> PosIdx {
        match pos {
            TermPos::Original(span) | TermPos::Inherited(span) => {
                let next = self.positions.len();
                self.positions.push(span);
                // The conversion can't overflow, because this method is
                // the only one that increases the length and it panics at
                // `PosIdx::MAX`
                let idx = next as u32;
                if idx > PosIdx::MAX {
                    panic!("maximum number of positions reached");
                }
                if matches!(pos, TermPos::Inherited(_)) {
                    PosIdx(idx).to_inherited()
                } else {
                    PosIdx(idx)
                }
            }
            TermPos::None => PosIdx::NONE,
        }
    }

    /// Returns the position at index `idx`.
    pub fn get(&self, idx: PosIdx) -> TermPos {
        if idx == PosIdx::NONE {
            TermPos::None
        } else {
            let span = self.positions[usize::try_from(idx.0 & PosIdx::IDX_MASK).expect(
                "Nickel doesn't support architecture with pointer size smaller than 32bits",
            )];
            if idx.is_inherited() {
                TermPos::Inherited(span)
            } else {
                TermPos::Original(span)
            }
        }
    }
}
