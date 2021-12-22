use std::ops::Range;

use codespan::FileId;
use nickel::position::{RawSpan, TermPos};

pub trait TermPosExt {
    fn try_to_range(&self) -> Option<(FileId, Range<usize>)>;
}

impl TermPosExt for TermPos {
    fn try_to_range(&self) -> Option<(FileId, Range<usize>)> {
        match self {
            TermPos::Inherited(RawSpan { src_id, start, end })
            | TermPos::Original(RawSpan { src_id, start, end }) => {
                Some((*src_id, (start.0 as usize..end.0 as usize)))
            }
            TermPos::None => None,
        }
    }
}
