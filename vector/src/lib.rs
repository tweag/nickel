//! This crate provides persistent data structures tailored to Nickel's needs.
//!
//! [`Vector`] is a persistent vector (also known as a "bitmapped vector trie"),
//! as described [here](https://hypirion.com/musings/understanding-persistent-vector-pt-1).
//! The same structure is implemented in [rpds](https://crates.io/crates/rpds), but our
//! implementation is faster for Nickel's use-cases:
//! - rpds's internal nodes are implemented with `Vec`, meaning that there's a double pointer
//!   indirection. We store our internal nodes inline.
//! - rpds wraps its leaves in `Rc` pointers, but we are mainly interested in storing things
//!   that are already reference-counted under the hood. We store our leaves inline, and
//!   require that the be `Clone`.
//! - we have optimized implementations of `Extend`, and support fast iteration over
//!   subslices.
//!
//! [`Slice`] backs the implementation of arrays in Nickel. It's basically a
//! [`Vector`] with support for slicing.

// Not yet implemented (do we need them?)
// - deletion
// - mutable indexing
// - mutable iteration

pub mod slice;
pub mod vector;

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
