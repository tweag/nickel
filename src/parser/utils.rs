/// A few helpers to generate position spans and labels easily during parsing
use crate::label::{ContractKind, Label, TyPath};
use crate::position::RawSpan;
use crate::types::Types;
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
pub fn mk_label(kind: ContractKind, types: Types, src_id: FileId, l: usize, r: usize) -> Label {
    Label {
        kind,
        types,
        tag: String::new(),
        span: mk_span(src_id, l, r),
        polarity: true,
        path: TyPath::Nil(),
    }
}
