//! Module for the Combine trait
//!
//! Defines `Combine` traits.

use crate::bytecode::ast::AstAlloc;

/// Trait for structures representing a series of annotation that can be combined (flattened).
/// Pedantically, `Combine` is just a monoid.
pub trait Combine {
    /// Combine two elements.
    fn combine(left: Self, right: Self) -> Self;
}

/// [Combine] doesn't work for new ast nodes, which requires an external allocator to create new
/// nodes. This trait is a version that takes this additional allocator. It's temporary: I suspect
/// we won't need the original general [Combine] trait once we move to the bytecode VM, as
/// [Combine] is used mostly on ast-like data, and we will rename [CombineAlloc] to [Combine].
pub trait CombineAlloc<'ast> {
    fn combine(alloc: &'ast AstAlloc, left: Self, right: Self) -> Self;
}

impl<T: Combine> Combine for Option<T> {
    fn combine(left: Self, right: Self) -> Self {
        match (left, right) {
            (None, None) => None,
            (None, Some(x)) | (Some(x), None) => Some(x),
            (Some(left), Some(right)) => Some(Combine::combine(left, right)),
        }
    }
}

impl<'ast, T: CombineAlloc<'ast>> CombineAlloc<'ast> for Option<T> {
    fn combine(alloc: &'ast AstAlloc, left: Self, right: Self) -> Self {
        match (left, right) {
            (None, None) => None,
            (None, Some(x)) | (Some(x), None) => Some(x),
            (Some(left), Some(right)) => Some(CombineAlloc::combine(alloc, left, right)),
        }
    }
}
