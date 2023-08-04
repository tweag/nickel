use std::process;

use crate::{
    cli::{Files, GlobalOptions},
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
    pub fn run(self, global: GlobalOptions) {
        let mut program = eval::prepare(&self.sources, &global);

        if let Err(err) = program.pprint_ast(&mut std::io::stdout(), self.transform) {
            program.report(err);
            process::exit(1);
        }
    }
}
