use std::ops::Range;

use codespan::FileId;
use nickel::position::RawSpan;

pub trait RawSpanExt {
    fn to_range(self) -> (FileId, Range<usize>);
}

impl RawSpanExt for RawSpan {
    fn to_range(self) -> (FileId, Range<usize>) {
        (self.src_id, (self.start.to_usize()..self.end.to_usize()))
    }
}
