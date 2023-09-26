use crate::{
    cli::GlobalOptions,
    customize::NoCustomizeMode,
    error::{CliResult, ResultErrorExt},
    input::{InputOptions, Prepare},
};

#[derive(clap::Parser, Debug)]
pub struct TypecheckCommand {
    #[command(flatten)]
    inputs: InputOptions<NoCustomizeMode>,
}

impl TypecheckCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.inputs.prepare(&global)?;
        program.typecheck().report_with_program(program)
    }
}
