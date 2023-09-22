use crate::{
    cli::GlobalOptions,
    error::{CliResult, ResultErrorExt},
    eval::EvalCommand,
};

#[derive(clap::Parser, Debug)]
pub struct PprintAstCommand {
    /// Performs code transformations before printing
    #[arg(long)]
    pub transform: bool,

    #[command(flatten)]
    pub evaluation: EvalCommand,
}

impl PprintAstCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.evaluation.prepare(&global)?;
        program
            .pprint_ast(&mut std::io::stdout(), self.transform)
            .report_with_program(program)
    }
}
