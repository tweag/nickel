//! Experimental bytecode compiler, virtual machine, and their intermediate representations.
//!
//! This implementation is under construction; it's currently not usable and it's not available by
//! default in mainline Nickel.

// TODO(parser migration): compatibility shim
pub use nickel_lang_parser::pretty;

pub mod ast;
