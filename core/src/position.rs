//! Define types of positions and position spans.
//!
//! The positions defined in this module are represented by the id of the corresponding source and
//! raw byte indices.  They are prefixed with Raw to differentiate them from codespan's types and
//! indicate that they do not store human friendly data like lines and columns.
use codespan::{self, ByteIndex, FileId};
use std::cmp::{max, min, Ordering};

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
        self.as_opt_ref().map_or(false, |sp| sp.contains(pos))
    }

    /// Fuse two positions if they are from the same source file.
    ///
    /// - If both positions are defined and from the same file, the resulting position is the
    /// smallest span that contain both.
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
