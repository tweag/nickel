//! Define types of positions and position spans.
//!
//! The positions defined in this module are represented by the id of the corresponding source and
//! raw byte indices.  They are prefixed with Raw to differentiate them from codespan's types and
//! indicate that they do not store human friendly data like lines and columns.
use codespan::{ByteIndex, FileId};

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
