/// A few helpers to generate position spans and labels easily during parsing
use crate::label::{Label, TyPath};
use crate::position::RawSpan;
use codespan::FileId;

/// Make a span from parser byte offsets. The last parameter is an offset, corresponding to the
/// size of the automatically included contracts, that is substracted from `l` and `r` (see
/// include_contracts in program.rs) for correct positions in error reporting. If span starts
/// before the offset, `None` is returned.
pub fn mk_span(src_id: &FileId, l: usize, r: usize, offset: usize) -> Option<RawSpan> {
    if l < offset {
        None
    } else {
        Some(RawSpan {
            src_id: src_id.clone(),
            start: ((l - offset) as u32).into(),
            end: ((r - offset) as u32).into(),
        })
    }
}

/// Same as `mk_span`, but for labels
pub fn mk_label(tag: &str, src_id: &FileId, l: usize, r: usize, offset: usize) -> Label {
    Label {
        tag: String::from(tag),
        span: mk_span(src_id, l, r, offset)
            .expect("Automatically included contracts should not define labels"),
        polarity: true,
        path: TyPath::Nil(),
    }
}
