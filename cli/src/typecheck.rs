use crate::{
    cli::{Files, GlobalOptions},
    error::{CliResult, WithProgram},
    eval,
};

#[derive(clap::Parser, Debug)]
pub struct TypecheckOptions {
    #[command(flatten)]
    sources: Files,
}

impl TypecheckOptions {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = eval::prepare(&self.sources, &global)?;
        Ok(program.typecheck().with_program(program)?)
    }
}
