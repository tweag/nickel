use std::{ffi::OsString, process};

use nickel_lang_core::{eval::cache::lazy::CBNCache, program::Program};

use crate::cli::{Files, GlobalOptions};

#[derive(clap::Parser, Debug)]
pub struct EvalOptions {
    #[command(flatten)]
    pub sources: Files,

    /// freeform args after --
    #[arg(last = true)]
    pub freeform: Vec<OsString>,
}

impl EvalOptions {
    pub fn run(self, global: GlobalOptions) {
        let mut program = prepare(&self.sources, &global);
        if let Err(err) = program.eval_full().map(|t| println!("{t}")) {
            program.report(err);
            process::exit(1)
        }
    }

    pub fn prepare(&self, global: &GlobalOptions) -> Program<CBNCache> {
        prepare(&self.sources, global)
    }
}

pub fn prepare(sources: &Files, global: &GlobalOptions) -> Program<CBNCache> {
    let mut program = sources
        .files
        .clone()
        .map(|f| Program::new_from_file(f, std::io::stderr()))
        .unwrap_or_else(|| Program::new_from_stdin(std::io::stderr()))
        .unwrap_or_else(|err| {
            eprintln!("Error when reading input: {err}");
            process::exit(1)
        });

    #[cfg(debug_assertions)]
    if global.nostdlib {
        program.set_skip_stdlib();
    }

    program.set_color(global.color.into());

    program
}
