use nickel_lang_core::{eval::cache::lazy::CBNCache, program::Program};

use crate::{
    cli::{GlobalOptions, InputOptions},
    error::{CliResult, ResultErrorExt},
};

#[derive(clap::Parser, Debug)]
pub struct EvalCommand {
    #[cfg(debug_assertions)]
    /// Skips the standard library import. For debugging only. This does not affect REPL
    #[arg(long, global = true)]
    pub nostdlib: bool,

    #[command(flatten)]
    pub input: InputOptions,
}

impl EvalCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.prepare(&global)?;
        program
            .eval_full()
            .map(|t| println!("{t}"))
            .report_with_program(program)
    }

    pub fn prepare(&self, global: &GlobalOptions) -> CliResult<Program<CBNCache>> {
        let mut program = self
            .input
            .file
            .clone()
            .map(|f| Program::new_from_file(f, std::io::stderr()))
            .unwrap_or_else(|| Program::new_from_stdin(std::io::stderr()))?;

        #[cfg(debug_assertions)]
        if self.nostdlib {
            program.set_skip_stdlib();
        }

        program.color_opt = global.color.into();

        Ok(program)
    }
}
