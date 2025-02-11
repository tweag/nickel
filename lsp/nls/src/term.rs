use std::{hash::Hash, ops::Range, ptr};

use nickel_lang_core::{bytecode::ast::Ast, files::FileId, position::RawSpan};

// A term that uses a pointer to Term to implement Eq and Hash.
#[derive(Clone, Debug, Copy)]
pub struct AstPtr<'ast>(pub &'ast Ast<'ast>);

impl PartialEq for AstPtr<'_> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl Hash for AstPtr<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0 as *const Ast<'_>).hash(state)
    }
}

impl Eq for AstPtr<'_> {}

pub trait RawSpanExt {
    fn to_range(self) -> (FileId, Range<usize>);
}

impl RawSpanExt for RawSpan {
    fn to_range(self) -> (FileId, Range<usize>) {
        (self.src_id, (self.start.to_usize()..self.end.to_usize()))
    }
}
