//! Define types of positions and position spans.
//!
//! The positions defined in this module are represented by the id of the corresponding source and
//! raw byte indices.  They are prefixed with Raw to differentiate them from codespan's types and
//! indicate that they do not store human friendly data like lines and columns.
use codespan::{ByteIndex, FileId};
use std::cmp::{max, min, Ordering};

/// A position identified by a byte offset in a file.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct RawPos {
    pub src_id: FileId,
    pub index: ByteIndex,
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
    /// span that contain both `span1` and `span2`.
    pub fn fuse(span1: RawSpan, span2: RawSpan) -> Option<RawSpan> {
        if span1.src_id == span2.src_id {
            Some(RawSpan {
                src_id: span1.src_id,
                start: min(span1.start, span2.start),
                end: max(span1.end, span2.end),
            })
        } else {
            None
        }
    }
}

/// The position span of a term.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum TermPos {
    /// The term exactly corresponds to an original expression in the source, or is a construct
    /// introduced by program transformation that correponds to an original span in the source.
    Original(RawSpan),
    /// The term is the result of the evaluation of an original expression in the source.
    Inherited(RawSpan),
    /// The term couldn't be assigned a position (usually generated during execution or program
    /// transformations).
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
