//! This crate provides persistent data structures tailored to Nickel's needs.
//!
//! [`Vector`] is a persistent vector (also known as a "bitmapped vector trie")
//! with cheap clones and efficient copy-on-write modifications. [`Slice`]
//! backs the implementation of arrays in Nickel. It's basically a [`Vector`]
//! with support for slicing.

// Not yet implemented (do we need them?)
// - deletion
// - mutable indexing

pub mod slice;
pub mod vector;

/// [`Vector`] takes a "branching factor" parameter, which must be a
/// reasonably-sized power of two. We use this trait to enforce that.
pub trait ValidBranchingConstant {}
pub struct Const<const N: usize> {}

impl ValidBranchingConstant for Const<2> {}
impl ValidBranchingConstant for Const<4> {}
impl ValidBranchingConstant for Const<8> {}
impl ValidBranchingConstant for Const<16> {}
impl ValidBranchingConstant for Const<32> {}
impl ValidBranchingConstant for Const<64> {}
impl ValidBranchingConstant for Const<128> {}

pub use slice::Slice;
pub use vector::Vector;
