use nickel_lang_core::bytecode::ast::Ast;
use std::{hash::Hash, ptr};

/// A wrapper around a reference to an [nickel_lang_core::bytecode::ast::Ast] to implement custom
/// fast pointer-based `Eq` and `Hash`.
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
