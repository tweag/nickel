// TODO: many (most?) of our error variants are large. This might be
// automatically solved by converting to the new runtime representation,
// but if not then we should reduce them.
#![allow(clippy::result_large_err)]

pub mod annotated_test;
pub mod bench;
pub mod project_root;
pub mod test_program;

pub use nickel_lang_core;
