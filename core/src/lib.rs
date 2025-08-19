// TODO: many (most?) of our error variants are large. This might be
// automatically solved by converting to the new runtime representation,
// but if not then we should reduce them.
#![allow(clippy::result_large_err)]

pub mod bytecode;
pub mod cache;
pub mod closurize;
pub mod deserialize;
pub mod environment;
pub mod error;
pub mod eval;
pub mod files;
pub mod label;
#[cfg(feature = "nix-experimental")]
pub mod nix_ffi;
pub mod package;
pub mod parser;
pub mod pretty;
pub mod program;
pub mod repl;
pub mod serialize;
pub mod stdlib;
pub mod term;
pub mod transform;
pub mod typ;
pub mod typecheck;

pub(crate) use nickel_lang_parser::metrics;

#[cfg(feature = "format")]
pub mod format;
