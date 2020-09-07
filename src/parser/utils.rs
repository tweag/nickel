/// A few helpers to generate position spans and labels easily during parsing
use crate::label::{Label, TyPath};
use crate::position::RawSpan;
use codespan::FileId;

/// Make a span from parser byte offsets.
pub fn mk_span(src_id: FileId, l: usize, r: usize) -> RawSpan {
    RawSpan {
        src_id,
        start: (l as u32).into(),
        end: (r as u32).into(),
    }
}

/// Same as `mk_span`, but for labels
pub fn mk_label(tag: &str, src_id: FileId, l: usize, r: usize) -> Label {
    Label {
        tag: String::from(tag),
        span: mk_span(src_id, l, r),
        polarity: true,
        path: TyPath::Nil(),
    }
}
