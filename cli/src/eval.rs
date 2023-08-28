use nickel_lang_core::{eval::cache::lazy::CBNCache, program::Program};

use crate::{
    cli::GlobalOptions,
    error::{CliResult, ResultErrorExt},
};

#[derive(clap::Parser, Debug)]
pub struct EvalCommand {}

impl EvalCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = prepare(&global)?;
        program
            .eval_full()
            .map(|t| println!("{t}"))
            .report_with_program(program)
    }

    pub fn prepare(&self, global: &GlobalOptions) -> CliResult<Program<CBNCache>> {
        prepare(global)
    }
}

pub fn prepare(global: &GlobalOptions) -> CliResult<Program<CBNCache>> {
    let mut program = global
        .nickel_file()?
        .map(|f| Program::new_from_file(f, std::io::stderr()))
        .unwrap_or_else(|| Program::new_from_stdin(std::io::stderr()))?;

    #[cfg(debug_assertions)]
    if global.nostdlib {
        program.set_skip_stdlib();
    }

    program.set_color(global.color.into());

    Ok(program)
}
