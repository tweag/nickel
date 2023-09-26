use std::path::PathBuf;

use nickel_lang_core::{eval::cache::lazy::CBNCache, program::Program};

use crate::{cli::GlobalOptions, customize::Customize, error::CliResult};

#[derive(clap::Parser, Debug)]
pub struct InputOptions<Customize: clap::Args> {
    /// Input file, omit to read from stdin
    pub files: Vec<PathBuf>,

    #[command(flatten)]
    pub customize_mode: Customize,
}

pub trait Prepare {
    fn prepare(&self, global: &GlobalOptions) -> CliResult<Program<CBNCache>>;
}

impl<C: clap::Args + Customize> Prepare for InputOptions<C> {
    fn prepare(&self, global: &GlobalOptions) -> CliResult<Program<CBNCache>> {
        let mut program = match self.files.as_slice() {
            [] => Program::new_from_stdin(std::io::stderr()),
            [p] => Program::new_from_file(p, std::io::stderr()),
            files => Program::new_from_files(files, std::io::stderr()),
        }?;

        program.color_opt = global.color.into();

        self.customize_mode.customize(program)
    }
}
