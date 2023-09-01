//! Module for the Combine trait
//!
//! Defines the `Combine` trait.

/// Trait for structures representing a series of annotation that can be combined (flattened).
/// Pedantically, `Combine` is just a monoid.
pub trait Combine: Default {
    /// Combine two elements.
    fn combine(left: Self, right: Self) -> Self;
}
