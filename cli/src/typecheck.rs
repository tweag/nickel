use crate::{
    cli::GlobalOptions,
    error::{CliResult, WithProgram},
    eval,
};

#[derive(clap::Parser, Debug)]
pub struct TypecheckOptions {}

impl TypecheckOptions {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = eval::prepare(&global.files, &global)?;
        program.typecheck().with_program(program)
    }
}
