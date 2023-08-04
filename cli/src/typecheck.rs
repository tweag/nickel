use std::process;

use crate::{
    cli::{Files, GlobalOptions},
    eval,
};

#[derive(clap::Parser, Debug)]
pub struct TypecheckOptions {
    #[command(flatten)]
    sources: Files,
}

impl TypecheckOptions {
    pub fn run(self, global: GlobalOptions) {
        let mut program = eval::prepare(&self.sources, &global);
        if let Err(err) = program.typecheck() {
            program.report(err);
            process::exit(1);
        }
    }
}
