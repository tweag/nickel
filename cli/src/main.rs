//! Entry point of the program.
//!
// TODO: many (most?) of our error variants are large. This might be
// automatically solved by converting to the new runtime representation,
// but if not then we should reduce them.
#![allow(clippy::result_large_err)]
#![allow(clippy::large_enum_variant)]

#[cfg(feature = "doc")]
mod doc;
#[cfg(feature = "doc")]
mod doctest;
#[cfg(feature = "format")]
mod format;
#[cfg(feature = "metrics")]
mod metrics;
#[cfg(feature = "package-experimental")]
mod package;
#[cfg(feature = "repl")]
mod repl;

mod cli;
mod completions;
mod customize;
mod error;
mod eval;
mod export;
mod global;
mod input;
mod pprint_ast;
mod query;
mod typecheck;

use std::process::ExitCode;

use global::GlobalContext;

use crate::cli::{Command, Options};

/// How many warnings do we emit?
const WARNING_LIMIT: usize = 10;

fn main() -> ExitCode {
    #[cfg(feature = "metrics")]
    let metrics = metrics::Recorder::install();

    let opts = <Options as clap::Parser>::parse();

    let error_format = opts.global.error_format;
    let color = opts.global.color;
    #[cfg(feature = "metrics")]
    let report_metrics = opts.global.metrics;

    let mut ctxt = GlobalContext::new(opts.global);

    match opts.command {
        Command::Eval(eval) => eval.run(&mut ctxt),
        Command::PprintAst(pprint_ast) => pprint_ast.run(&mut ctxt),
        Command::Export(export) => export.run(&mut ctxt),
        Command::Query(query) => query.run(&mut ctxt),
        Command::Typecheck(typecheck) => typecheck.run(&mut ctxt),
        Command::GenCompletions(completions) => completions.run(&mut ctxt),

        #[cfg(feature = "package-experimental")]
        Command::Package(package) => package.run(&mut ctxt),

        #[cfg(feature = "repl")]
        Command::Repl(repl) => repl.run(&mut ctxt),

        #[cfg(feature = "doc")]
        Command::Doc(doc) => doc.run(&mut ctxt),
        #[cfg(feature = "doc")]
        Command::Test(test) => test.run(&mut ctxt),

        #[cfg(feature = "format")]
        Command::Format(format) => format.run(&mut ctxt),
    };

    #[cfg(feature = "metrics")]
    if report_metrics {
        metrics.report();
    }

    let mut warnings = ctxt.deduplicated_warnings();
    for w in warnings.drain(..WARNING_LIMIT.min(warnings.len())) {
        w.report(error_format, color_opt_from_clap(color));
    }
    if !warnings.is_empty() {
        eprintln!("(suppressed {} additional warnings)", warnings.len());
    }

    let mut code = ExitCode::SUCCESS;
    for e in ctxt.errors.try_iter() {
        e.report(error_format, color_opt_from_clap(color));
        code = ExitCode::FAILURE;
    }

    code
}

fn color_opt_from_clap(c: clap::ColorChoice) -> nickel_lang_core::error::report::ColorOpt {
    use nickel_lang_core::error::report::ColorOpt;
    match c {
        clap::ColorChoice::Auto => ColorOpt::Auto,
        clap::ColorChoice::Always => ColorOpt::Always,
        clap::ColorChoice::Never => ColorOpt::Never,
    }
}
