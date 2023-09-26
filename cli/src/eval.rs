use nickel_lang_core::{eval::cache::lazy::CBNCache, program::Program};

use crate::{
    cli::GlobalOptions,
    customize::CustomizeMode,
    error::{CliResult, ResultErrorExt},
    input::{InputOptions, Prepare},
};

#[derive(clap::Parser, Debug)]
pub struct EvalCommand {
    #[cfg(debug_assertions)]
    /// Skips the standard library import. For debugging only
    #[arg(long, global = true)]
    pub nostdlib: bool,

    #[command(flatten)]
    pub input: InputOptions<CustomizeMode>,
}

impl EvalCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.prepare(&global)?;
        program
            .eval_full()
            .map(|t| println!("{t}"))
            .report_with_program(program)
    }
}

impl Prepare for EvalCommand {
    fn prepare(&self, global: &GlobalOptions) -> CliResult<Program<CBNCache>> {
        // Rust warns about `program` not needing to be mutable if
        // `debug_assertions` are off
        #![allow(unused_mut)]
        let mut program = self.input.prepare(global)?;

        #[cfg(debug_assertions)]
        if self.nostdlib {
            program.set_skip_stdlib();
        }

        Ok(program)
    }
}
