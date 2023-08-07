use crate::{
    cli::GlobalOptions,
    error::{CliResult, WithProgram},
    eval,
};

#[derive(clap::Parser, Debug)]
pub struct PprintAstOptions {
    /// Performs code transformations before printing
    #[arg(long)]
    pub transform: bool,
}

impl PprintAstOptions {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = eval::prepare(&global.files, &global)?;
        program
            .pprint_ast(&mut std::io::stdout(), self.transform)
            .with_program(program)
    }
}
