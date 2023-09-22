use crate::{
    cli::GlobalOptions,
    error::{CliResult, ResultErrorExt},
    eval::EvalCommand,
};

#[derive(clap::Parser, Debug)]
pub struct TypecheckCommand {
    #[command(flatten)]
    evaluation: EvalCommand,
}

impl TypecheckCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.evaluation.prepare(&global)?;
        program.typecheck().report_with_program(program)
    }
}
