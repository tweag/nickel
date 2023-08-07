use nickel_lang_core::{eval::cache::lazy::CBNCache, program::Program};

use crate::{
    cli::{Files, GlobalOptions},
    error::{CliResult, WithProgram},
};

#[derive(clap::Parser, Debug)]
pub struct EvalOptions {}

impl EvalOptions {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = prepare(&global.files, &global)?;
        program
            .eval_full()
            .map(|t| println!("{t}"))
            .with_program(program)
    }

    pub fn prepare(&self, global: &GlobalOptions) -> CliResult<Program<CBNCache>> {
        prepare(&global.files, global)
    }
}

pub fn prepare(sources: &Files, global: &GlobalOptions) -> CliResult<Program<CBNCache>> {
    let mut program = sources
        .file
        .clone()
        .map(|f| Program::new_from_file(f, std::io::stderr()))
        .unwrap_or_else(|| Program::new_from_stdin(std::io::stderr()))?;

    #[cfg(debug_assertions)]
    if global.nostdlib {
        program.set_skip_stdlib();
    }

    program.set_color(global.color.into());

    Ok(program)
}
