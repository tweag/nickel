use crate::{
    cli::GlobalOptions,
    error::{CliResult, ResultErrorExt},
    eval,
};

#[derive(clap::Parser, Debug)]
pub struct TypecheckCommand {}

impl TypecheckCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = eval::prepare(&global)?;
        program.typecheck().report_with_program(program)
    }
}
