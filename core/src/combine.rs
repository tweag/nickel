//! Module for the Combine trait
//!
//! Defines `Combine` traits.

use crate::bytecode::ast::AstAlloc;

/// Trait for structures representing a series of annotation that can be combined (flattened).
/// Pedantically, `Combine` is just a monoid.
pub trait Combine: Default {
    /// Combine two elements.
    fn combine(left: Self, right: Self) -> Self;
}

/// [combine::Combine] doens't work for new ast nodes, which requires an external allocator to
/// create new nodes. This trait is a version that takes this additional allocator. It's temporary:
/// I suspect we won't need the original general `Combine` trait once we move to the bytecode vm,
/// as [crate::combine::Combine] is used mostly on ast-like data.
pub trait CombineAlloc<'ast> {
    fn combine(alloc: &'ast AstAlloc, left: Self, right: Self) -> Self;
}
