//! Define types of positions and position spans.
//!
//! The positions defined in this module are represented by the id of the corresponding source and
//! raw byte indices.  They are prefixed with Raw to differentiate them from codespan's types and
//! indicate that they do not store human friendly data like lines and columns.
use codespan::{ByteIndex, FileId};
use std::cmp::Ordering;

/// A position identified by a byte offset in a file.
#[derive(Debug, Clone, PartialEq)]
pub struct RawPos {
    pub src_id: FileId,
    pub index: ByteIndex,
}

/// A position span identified by a starting byte offset and an ending byte offset in a file.
///
/// `end` is the offset of the last character plus one.
#[derive(Debug, Clone, PartialEq)]
pub struct RawSpan {
    pub src_id: FileId,
    pub start: ByteIndex,
    pub end: ByteIndex,
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
