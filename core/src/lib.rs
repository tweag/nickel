// TODO: many (most?) of our error variants are large. This might be
// automatically solved by converting to the new runtime representation,
// but if not then we should reduce them.
#![allow(clippy::result_large_err)]

use nickel_lang_parser::metrics;
pub use nickel_lang_parser::{combine, environment, files, identifier, traverse};

pub mod ast;
pub mod cache;
pub mod closurize;
pub mod deserialize;
pub mod error;
pub mod eval;
pub mod label;
#[cfg(feature = "nix-experimental")]
pub mod nix_ffi;
pub mod package;
pub mod parser;
pub mod position;
pub mod pretty;
pub mod program;
pub mod repl;
pub mod serialize;
pub mod stdlib;
pub mod term;
pub mod transform;
pub mod typ;
pub mod typecheck;

#[cfg(feature = "format")]
pub mod format;
