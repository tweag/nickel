//! Command-line options and subcommands.

use git_version::git_version;

use crate::{
    completions::GenCompletionsCommand, eval::EvalCommand, export::ExportCommand,
    pprint_ast::PprintAstCommand, query::QueryCommand, typecheck::TypecheckCommand,
};

use nickel_lang_core::{config::Config, error::report::ErrorFormat};

#[cfg(feature = "repl")]
use crate::repl::ReplCommand;

#[cfg(feature = "doc")]
use crate::{doc::DocCommand, doctest::TestCommand};

#[cfg(feature = "format")]
use crate::format::FormatCommand;

#[derive(clap::Parser, Debug)]
/// The interpreter of the Nickel language.
#[command(
    author,
    about,
    long_about = None,
    version = format!(
        "{} {} (rev {})",
        env!("CARGO_BIN_NAME"),
        env!("CARGO_PKG_VERSION"),
        // 7 is the length of self.shortRev. the string is padded out so it can
        // be searched for in the binary
        // The crate published on cargo doesn't have the git version, so we use "cargorel" as a
        // fallback value
        git_version!(fallback = &option_env!("NICKEL_NIX_BUILD_REV").unwrap_or("cargorel")[0..7])
    )
)]
pub struct Options {
    #[command(flatten)]
    pub global: GlobalOptions,

    #[command(subcommand)]
    pub command: Command,
}

#[derive(clap::Parser, Debug)]
pub struct GlobalOptions {
    /// Configure when to output messages in color
    #[arg(long, global = true, value_enum, default_value_t)]
    pub color: clap::ColorChoice,

    /// Output error messages in a specific format. The schema of serialized errors reflects the
    /// way they are currently rendered. This schema is not guaranteed to be stable yet.
    #[arg(long, global = true, value_enum, default_value_t)]
    pub error_format: ErrorFormat,

    #[cfg(feature = "metrics")]
    /// Print all recorded metrics at the very end of the program
    #[arg(long, global = true, default_value_t = false)]
    pub metrics: bool,
}

impl GlobalOptions {
    pub fn apply_to_config(&self, config: &mut Config) {
        config.color_opt = self.color.into();
    }
}

/// Available subcommands.
#[derive(clap::Subcommand, Debug)]
pub enum Command {
    /// Evaluates a Nickel program and pretty-prints the result.
    Eval(EvalCommand),
    /// Converts the parsed representation (AST) back to Nickel source code and prints it. Used for
    /// debugging purpose
    PprintAst(PprintAstCommand),
    /// Evaluates a Nickel program and serializes the result to a given format
    Export(ExportCommand),
    /// Prints the metadata attached to an attribute, given as a path
    Query(QueryCommand),
    /// Typechecks the program but does not run it
    Typecheck(TypecheckCommand),
    /// Starts a REPL session
    #[cfg(feature = "repl")]
    Repl(ReplCommand),
    /// Generates the documentation files for the a specified nickel file
    #[cfg(feature = "doc")]
    Doc(DocCommand),
    /// Tests the documentation examples in the specified nickel file
    #[cfg(feature = "doc")]
    Test(TestCommand),
    /// Formats Nickel files
    #[cfg(feature = "format")]
    Format(FormatCommand),

    /// Generate shell completion files
    GenCompletions(GenCompletionsCommand),
}
