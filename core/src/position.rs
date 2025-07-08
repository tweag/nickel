//! Define types of positions and position spans.
//!
//! The positions defined in this module are represented by the id of the corresponding source and
//! raw byte indices.  They are prefixed with Raw to differentiate them from codespan's types and
//! indicate that they do not store human friendly data like lines and columns.
use crate::files::FileId;
use codespan::{self, ByteIndex};
use std::{
    cmp::{max, min, Ordering},
    ops::Range,
};

/// A simple wrapper trait for numeric types that define a `MAX` constant. This is useful to make
/// interfaces more generic.
pub trait Max {
    const MAX: Self;
}

impl Max for u32 {
    const MAX: Self = Self::MAX;
}

impl Max for usize {
    const MAX: Self = Self::MAX;
}

/// A position identified by a byte offset in a file.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct RawPos {
    pub src_id: FileId,
    pub index: ByteIndex,
}

impl RawPos {
    pub fn new(src_id: FileId, index: ByteIndex) -> Self {
        Self { src_id, index }
    }
}

/// A position span identified by a starting byte offset and an ending byte offset in a file.
///
/// `end` is the offset of the last character plus one.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct RawSpan {
    pub src_id: FileId,
    pub start: ByteIndex,
    pub end: ByteIndex,
}

impl RawSpan {
    /// Fuse two spans if they are from the same source file. The resulting span is the smallest
    /// span that contain both `self` and `other`.
    pub fn fuse(self, other: RawSpan) -> Option<RawSpan> {
        if self.src_id == other.src_id {
            Some(RawSpan {
                src_id: self.src_id,
                start: min(self.start, other.start),
                end: max(self.end, other.end),
            })
        } else {
            None
        }
    }

    /// Create a `RawSpan` from a span as represented by the codespan library.
    pub fn from_codespan(src_id: FileId, span: codespan::Span) -> Self {
        RawSpan {
            src_id,
            start: span.start(),
            end: span.end(),
        }
    }

    /// Create a span from a numeric range. If either start or end is too large to be represented,
    /// `u32::MAX` is used instead.
    pub fn from_range<T>(src_id: FileId, range: Range<T>) -> Self
    where
        u32: TryFrom<T>,
    {
        RawSpan {
            src_id,
            start: ByteIndex(u32::try_from(range.start).unwrap_or(u32::MAX)),
            end: ByteIndex(u32::try_from(range.end).unwrap_or(u32::MAX)),
        }
    }

    /// Convert this span to a numeric index range. If either start or end is too large to be
    /// represented, `T::MAX` is used instead.
    pub fn to_range<T>(self) -> Range<T>
    where
        T: TryFrom<u32> + Max,
    {
        T::try_from(self.start.0).unwrap_or(T::MAX)..T::try_from(self.end.0).unwrap_or(T::MAX)
    }

    /// Return the start of this range.
    pub fn start_pos(&self) -> RawPos {
        RawPos {
            src_id: self.src_id,
            index: self.start,
        }
    }

    /// Check whether this span contains a position.
    pub fn contains(&self, pos: RawPos) -> bool {
        self.src_id == pos.src_id && (self.start..self.end).contains(&pos.index)
    }

    /// Check whether this span contains another span.
    pub fn contains_span(&self, other: RawSpan) -> bool {
        self.src_id == other.src_id && self.start <= other.start && self.end >= other.end
    }
}

impl From<RawSpan> for codespan::Span {
    fn from(span: RawSpan) -> Self {
        codespan::Span::new(span.start, span.end)
    }
}

/// The position span of a term.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub enum TermPos {
    /// The term exactly corresponds to an original expression in the source, or is a construct
    /// introduced by program transformation that corresponds to an original span in the source.
    Original(RawSpan),
    /// The term is the result of the evaluation of an original expression in the source.
    Inherited(RawSpan),
    /// The term couldn't be assigned a position (usually generated during execution or program
    /// transformations).
    #[default]
    None,
}

impl TermPos {
    /// Apply a transformation to the inner position, if any.
    pub fn map<F: FnOnce(RawSpan) -> RawSpan>(self, f: F) -> Self {
        match self {
            TermPos::Original(x) => TermPos::Original(f(x)),
            TermPos::Inherited(x) => TermPos::Inherited(f(x)),
            TermPos::None => TermPos::None,
        }
    }

    pub fn as_opt_ref(&self) -> Option<&RawSpan> {
        match self {
            TermPos::Original(ref pos) | TermPos::Inherited(ref pos) => Some(pos),
            TermPos::None => None,
        }
    }

    // In principle, this should rather be implemented in a trait impl `impl Into<Option<RawSpan>>
    // for TermPos`, but the type inference was working too badly.
    pub fn into_opt(self) -> Option<RawSpan> {
        match self {
            TermPos::Original(pos) | TermPos::Inherited(pos) => Some(pos),
            TermPos::None => None,
        }
    }

    /// Returns the file id associated to this position, if the position is defined, or `None`
    /// otherwise.
    pub fn src_id(&self) -> Option<FileId> {
        match self {
            TermPos::Original(raw_span) | TermPos::Inherited(raw_span) => Some(raw_span.src_id),
            TermPos::None => None,
        }
    }

    /// Return `self` if `self` not [Self::None], or `other` otherwise.
    pub fn or(self, other: Self) -> Self {
        if let TermPos::None = self {
            other
        } else {
            self
        }
    }

    /// Return either `self` or `other` if and only if exactly one of them is defined. If both are
    /// `None` or both are defined, `None` is returned.
    pub fn xor(self, other: Self) -> Self {
        match (self, other) {
            (defn, TermPos::None) | (TermPos::None, defn) => defn,
            _ => TermPos::None,
        }
    }

    /// Determine is the position is defined. Return `false` if it is `None`, and `true` otherwise.
    pub fn is_def(&self) -> bool {
        matches!(self, TermPos::Original(_) | TermPos::Inherited(_))
    }

    /// Try to unwrap the underlying span. Panic if `self` is `None`.
    #[track_caller]
    pub fn unwrap(self) -> RawSpan {
        match self {
            TermPos::Original(x) | TermPos::Inherited(x) => x,
            TermPos::None => panic!("TermPos::unwrap"),
        }
    }

    /// Try to set the position to inherited, if it wasn't already. If `self` was `None`, `None` is
    /// returned.
    pub fn into_inherited(self) -> Self {
        match self {
            TermPos::Original(pos) => TermPos::Inherited(pos),
            p => p,
        }
    }

    /// Check whether this span contains a position.
    pub fn contains(&self, pos: RawPos) -> bool {
        self.as_opt_ref().is_some_and(|sp| sp.contains(pos))
    }

    /// Fuse two positions if they are from the same source file.
    ///
    /// - If both positions are defined and from the same file, the resulting position is the
    ///   smallest span that contain both.
    /// - If both positions are defined but aren't from the same file, this returns `TermPos::None`
    /// - If at most one position is defined, the other is returned (whether defined or not).
    pub fn fuse(self, other: Self) -> Self {
        match (self, other) {
            (TermPos::Original(sp1), TermPos::Original(sp2)) => {
                if let Some(span) = sp1.fuse(sp2) {
                    TermPos::Original(span)
                } else {
                    TermPos::None
                }
            }
            (TermPos::Inherited(sp1), TermPos::Inherited(sp2))
            | (TermPos::Original(sp1), TermPos::Inherited(sp2))
            | (TermPos::Inherited(sp1), TermPos::Original(sp2)) => {
                if let Some(span) = sp1.fuse(sp2) {
                    TermPos::Inherited(span)
                } else {
                    TermPos::None
                }
            }
            (TermPos::None, maybe_def) | (maybe_def, TermPos::None) => maybe_def,
        }
    }
}

/// A natural ordering for positions: `p1` is smaller than `p2` if they are located in the same
/// file and `p1.offset` is smaller than `p2.offset`.
impl PartialOrd for RawPos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.src_id == other.src_id {
            Some(self.index.cmp(&other.index))
        } else {
            None
        }
    }
}

/// An inclusion ordering for spans: `s1` is smaller than `s2` if they are located in the same file
/// and `s1` is included in `s2` when seen as position intervals.
impl PartialOrd for RawSpan {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.src_id != other.src_id {
            None
        } else if self.start == other.start && self.end == other.end {
            Some(Ordering::Equal)
        } else if self.start >= other.start && self.end <= other.end {
            Some(Ordering::Less)
        } else if self.start <= other.start && self.end >= other.end {
            Some(Ordering::Greater)
        } else {
            None
        }
    }
}

impl From<RawSpan> for TermPos {
    fn from(span: RawSpan) -> Self {
        TermPos::Original(span)
    }
}

impl From<Option<RawSpan>> for TermPos {
    fn from(value: Option<RawSpan>) -> Self {
        match value {
            Some(span) => TermPos::Original(span),
            None => TermPos::None,
        }
    }
}

/// The index of an inline value into the position table. This way, we can attach a compact
/// position and still fit in one machine word in total (on 64 bits platform, at least).
#[derive(Debug, Clone, Copy)]
pub struct InlinePosIdx(u32);

impl Default for InlinePosIdx {
    fn default() -> Self {
        Self::NONE
    }
}

impl From<InlinePosIdx> for u32 {
    fn from(value: InlinePosIdx) -> Self {
        value.0
    }
}

#[cfg(any(target_pointer_width = "64", target_pointer_width = "32"))]
impl From<InlinePosIdx> for usize {
    fn from(value: InlinePosIdx) -> Self {
        value.0 as usize
    }
}

impl InlinePosIdx {
    /// A special index indicating that an inline value has no position attached.
    pub const NONE: InlinePosIdx = Self(0);

    /// Creates an inline position index from a usize, truncating the higher bits if set.
    pub fn from_usize_truncate(value: usize) -> Self {
        Self(value as u32)
    }
}

/// An index into the position table.
///
/// On 64 bit archs, the position table uses a unified index encoded on 64 bits. Values smaller or
/// equals to [`u32::MAX`] are reserved for inline values[^reserved], since inline values need to
/// fit (together with their index) into one word. The remaining range can be used by value blocks,
/// which leaves plenty of available indices (`2^64 - 2^32 ~ 2.0e19`).
///
/// On 32 bits archs (or less), we use only a single shared `u32` index for both inline values and
/// value blocks. It doesn't make sense to accomodate more positions that the addressable memory
/// anyway.
///
/// [^reserved]: this is not entirely true, as a value block may re-use an existing index
///     attributed to an inline value if it inherits its position (as typically the case with some
///     primitive operations). However, since the inline table index must fit within 32 bits, we
///     never allocate an index smaller than `u32::MAX` for a value block. Although we are able to
///     reuse an existing inline index for a value block.
#[cfg(target_pointer_width = "64")]
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct PosIdx(usize);
#[cfg(not(target_pointer_width = "64"))]
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct PosIdx(u32);

impl PosIdx {
    /// A special value indicating that an inline value or value block doesn't have a position
    /// defined. This is the first available position index for values.
    pub const NONE: PosIdx = Self(0);
}

#[cfg(not(target_pointer_width = "64"))]
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

#[cfg(target_pointer_width = "64")]
impl TryFrom<PosIdx> for InlinePosIdx {
    type Error = ();

    fn try_from(pos_idx: PosIdx) -> Result<Self, Self::Error> {
        Ok(InlinePosIdx(u32::try_from(pos_idx.0).map_err(|_| ())?))
    }
}

#[cfg(not(target_pointer_width = "64"))]
impl From<PosIdx> for InlinePosIdx {
    fn from(pos_idx: PosIdx) -> Self {
        InlinePosIdx(pos_idx.0)
    }
}

#[cfg(target_pointer_width = "64")]
impl From<InlinePosIdx> for PosIdx {
    fn from(pos_idx: InlinePosIdx) -> Self {
        PosIdx(pos_idx.0 as usize)
    }
}

#[cfg(not(target_pointer_width = "64"))]
impl From<InlinePosIdx> for PosIdx {
    fn from(pos_idx: InlinePosIdx) -> Self {
        PosIdx(pos_idx.0)
    }
}

/// An immutable table storing the position of values, both inline and blocks, addressed using a
/// unified indexing scheme.
pub struct PosTable {
    inlines: Vec<TermPos>,
    // On non-64-bits arch, we use only one common table. See PosIdx.
    #[cfg(target_pointer_width = "64")]
    blocks: Vec<TermPos>,
}

#[cfg(target_pointer_width = "64")]
impl PosTable {
    /// The first available index of a value block position in the range of combined indices.
    const FIRST_BLOCK_IDX: usize = u32::MAX as usize + 1;
    /// The maximum index of a value block position inside the [Self::blocks] table (before being
    /// adjusted into a combined index).
    const MAX_BLOCK_IDX: usize = usize::MAX - Self::FIRST_BLOCK_IDX;

    pub fn new() -> Self {
        // We always populate the first entry (of the inline table) with `TermPos::None`, so that
        // we can safely use the index `0` (`PosIdx::NONE` and `InlinePosIdx::NONE`) to mean
        // unintialized position, without having to special case the undefined position.
        Self {
            inlines: vec![TermPos::None],
            blocks: Vec::new(),
        }
    }

    /// Inserts a new position for an inline value and returns its index.
    pub fn push_inline_pos(&mut self, pos: TermPos) -> InlinePosIdx {
        let next = self.inlines.len();
        self.inlines.push(pos);
        InlinePosIdx(
            u32::try_from(next).expect("maximum number of positions reached for inline values"),
        )
    }

    /// Inserts a new position for a value block and returns its index.
    pub fn push_block_pos(&mut self, pos: TermPos) -> PosIdx {
        let next = self.blocks.len();
        self.blocks.push(pos);
        assert!(
            next <= Self::MAX_BLOCK_IDX,
            "maximum number of positions reached for value blocks"
        );
        PosIdx(next + Self::FIRST_BLOCK_IDX)
    }

    /// Returns the position at index `idx`.
    pub fn get(&self, idx: impl Into<PosIdx>) -> TermPos {
        let PosIdx(idx) = idx.into();
        // We're looking at the index of an inline value.
        if idx < Self::FIRST_BLOCK_IDX {
            self.inlines[idx]
        } else {
            self.blocks[idx - Self::FIRST_BLOCK_IDX]
        }
    }
}

#[cfg(not(target_pointer_width = "64"))]
impl PosTable {
    pub fn new() -> Self {
        Self {
            inlines: vec![TermPos::None],
        }
    }

    /// Pushes a new position for an inline value and returns its index.
    pub fn push_inline_pos(&mut self, pos: TermPos) -> InlinePosIdx {
        let next = self.inlines.len();
        self.inlines.push(pos);
        InlinePosIdx(u32::try_from(next).expect("maximum number of positions reached for values"))
    }

    /// Pushes a new position for a value block and returns its index.
    pub fn push_block_pos(&mut self, pos: TermPos) -> PosIdx {
        PosIdx(self.push_inline_pos(pos).0)
    }

    /// Returns the position at index `idx`.
    pub fn get(&self, idx: impl Into<PosIdx>) -> TermPos {
        self.inlines[usize::try_from(idx.into().0)
            .expect("position index out of bounds (doesn't fit in usize)")]
    }
}
