use std::path::PathBuf;

use nickel_lang_core::{eval::cache::lazy::CBNCache, program::Program};

use crate::{cli::GlobalOptions, customize::Customize, error::CliResult};

#[derive(clap::Parser, Debug)]
pub struct InputOptions<Customize: clap::Args> {
    /// Input files, omit to read from stdin
    pub files: Vec<PathBuf>,

    #[cfg(debug_assertions)]
    /// Skips the standard library import. For debugging only
    #[arg(long, global = true)]
    pub nostdlib: bool,

    /// Specifies a (comma-separated) list of paths to search for imports in.
    ///
    /// When importing a file, nickel searches for it relative to the file doing the
    /// import. If not found, it searches in the paths specified by `--nickel-path`.
    /// If not found there, it searches in the (colon-separated) list of paths contained
    /// in the environment variable `NICKEL_PATH`.
    #[arg(long, global = true, value_delimiter = ',', num_args = 1..)]
    pub nickel_path: Option<Vec<PathBuf>>,

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

        if let Some(nickel_path) = &self.nickel_path {
            program.add_import_paths(nickel_path.iter());
        }

        if let Ok(nickel_path) = std::env::var("NICKEL_PATH") {
            program.add_import_paths(nickel_path.split(':'));
        }

        #[cfg(debug_assertions)]
        if self.nostdlib {
            program.set_skip_stdlib();
        }

        self.customize_mode.customize(program)
    }
}
