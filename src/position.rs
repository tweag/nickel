//! Define types for positions and spans as filled by the parser, represented by the id of the
//! corresponding source and raw byte indices.  They are prefixed with Raw to differentiate them
//! from codespan's types and indicate that they do not store human friendly data like lines and
//! columns.
use codespan::{ByteIndex, FileId};

#[derive(Debug, Clone, PartialEq)]
pub struct RawLoc {
    pub src_id: FileId,
    pub index: ByteIndex,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RawSpan {
    pub src_id: FileId,
    pub start: ByteIndex,
    pub end: ByteIndex,
}
