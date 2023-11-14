use crate::{
    cli::GlobalOptions,
    customize::CustomizeMode,
    error::{CliResult, ResultErrorExt},
    input::{InputOptions, Prepare},
};

#[derive(clap::Parser, Debug)]
pub struct EvalCommand {
    /// Evaluate a specific field of the configuration
    #[arg(long, short, value_name = "FIELD_PATH")]
    pub field: Option<String>,

    #[command(flatten)]
    pub input: InputOptions<CustomizeMode>,
}

impl EvalCommand {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = self.input.prepare(&global)?;

        program
            .eval_full()
            .map(|t| println!("{t}"))
            .report_with_program(program)
    }
}
