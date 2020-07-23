//! Define types of positions and position spans
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
