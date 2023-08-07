use crate::{
    cli::GlobalOptions,
    error::{CliResult, WithProgram},
    eval,
};

#[derive(clap::Parser, Debug)]
pub struct TypecheckCommand {}

impl TypecheckCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = eval::prepare(&global)?;
        program.typecheck().with_program(program)
    }
}
