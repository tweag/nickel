use crate::{
    cli::GlobalOptions,
    customize::NoCustomizeMode,
    error::{CliResult, ResultErrorExt},
    input::{InputOptions, Prepare},
};

#[derive(clap::Parser, Debug)]
pub struct PprintAstCommand {
    /// Performs code transformations before printing
    #[arg(long)]
    pub transform: bool,

    #[command(flatten)]
    pub inputs: InputOptions<NoCustomizeMode>,
}

impl PprintAstCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.inputs.prepare(&global)?;
        program
            .pprint_ast(&mut std::io::stdout(), self.transform)
            .report_with_program(program)
    }
}
