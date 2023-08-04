//! Command-line options and subcommands.
use std::path::PathBuf;

use git_version::git_version;

use crate::{
    completions::GenCompletionsOptions, doc::DocOptions, eval::EvalOptions, export::ExportOptions,
    format::FormatOptions, pprint_ast::PprintAstOptions, query::QueryOptions, repl::ReplOptions,
    typecheck::TypecheckOptions,
};

#[derive(clap::Parser, Debug)]
/// The interpreter of the Nickel language.
#[command(
    author,
    about,
    long_about = None,
    version = format!("{} {} (rev {})", env!("CARGO_BIN_NAME"), env!("CARGO_PKG_VERSION"), git_version!(fallback = env!("NICKEL_NIX_BUILD_REV")))
)]
pub struct Options {
    #[command(flatten)]
    pub global: GlobalOptions,

    #[command(subcommand)]
    pub command: Command,
}

#[derive(clap::Parser, Debug)]
pub struct GlobalOptions {
    #[cfg(debug_assertions)]
    /// Skips the standard library import. For debugging only. This does not affect REPL
    #[arg(long, global = true)]
    pub nostdlib: bool,

    /// Configure when to output messages in color
    #[arg(long, global = true, value_enum, default_value_t)]
    pub color: clap::ColorChoice,
}

#[derive(clap::Parser, Debug)]
pub struct Files {
    /// Input files, omit to read from stdin
    pub files: Option<PathBuf>,
}

/// Available subcommands.
#[derive(clap::Subcommand, Debug)]
pub enum Command {
    /// Evaluate a Nickel program and pretty print the result.
    Eval(EvalOptions),
    /// Converts the parsed representation (AST) back to Nickel source code and prints it. Used for
    /// debugging purpose
    PprintAst(PprintAstOptions),
    /// Exports the result to a different format
    Export(ExportOptions),
    /// Prints the metadata attached to an attribute, given as a path
    Query(QueryOptions),
    /// Typechecks the program but do not run it
    Typecheck(TypecheckOptions),
    /// Starts a REPL session
    #[cfg(feature = "repl")]
    Repl(ReplOptions),
    /// Generates the documentation files for the specified nickel file
    #[cfg(feature = "doc")]
    Doc(DocOptions),
    /// Format Nickel files
    #[cfg(feature = "format")]
    Format(FormatOptions),

    #[command(hide = true)]
    GenCompletions(GenCompletionsOptions),
}
