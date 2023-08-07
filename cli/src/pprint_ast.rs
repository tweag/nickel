use crate::{
    cli::{Files, GlobalOptions},
    error::{CliResult, WithProgram},
    eval,
};

#[derive(clap::Parser, Debug)]
pub struct PprintAstOptions {
    /// Performs code transformations before printing
    #[arg(long)]
    pub transform: bool,

    /// Input file, omit to read from stdin
    #[command(flatten)]
    pub sources: Files,
}

impl PprintAstOptions {
    pub fn run(self, global: GlobalOptions) -> CliResult<()> {
        let mut program = eval::prepare(&self.sources, &global)?;
        program
            .pprint_ast(&mut std::io::stdout(), self.transform)
            .with_program(program)
    }
}
