use std::path::PathBuf;

use nickel_lang_core::{config::Config, eval::cache::lazy::CBNCache, program::Program};

use crate::{cli::GlobalOptions, customize::Customize, error::CliResult};

#[derive(clap::Parser, Debug)]
pub struct InputOptions<Customize: clap::Args> {
    /// Input files. Omit to read from stdin. If multiple files are provided, the corresponding
    /// Nickel expressions are merged (combined with `&`) to produce the result.
    pub files: Vec<PathBuf>,

    #[cfg(debug_assertions)]
    /// Skips the standard library import. For debugging only
    #[arg(long, global = true)]
    pub nostdlib: bool,

    /// Adds a directory to the list of paths to search for imports in.
    ///
    /// When importing a file, nickel searches for it relative to the file doing the
    /// import. If not found, it searches in the paths specified by `--import-path`.
    /// If not found there, it searches in the (colon-separated) list of paths contained
    /// in the environment variable `NICKEL_IMPORT_PATH`.
    #[arg(long, short = 'I', global = true)]
    pub import_path: Vec<PathBuf>,

    /// Start the type-checker in strict mode, so that the entire input is treated as a typed block.
    #[arg(long, global = true)]
    pub strict_typechecking: bool,

    #[command(flatten)]
    pub customize_mode: Customize,
}

impl<C: clap::Args> InputOptions<C> {
    pub fn apply_to_config(&self, config: &mut Config) {
        config.strict_typechecking = self.strict_typechecking;
    }
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

        global.apply_to_config(&mut program.config);
        self.apply_to_config(&mut program.config);

        program.add_import_paths(self.import_path.iter());

        if let Ok(nickel_path) = std::env::var("NICKEL_IMPORT_PATH") {
            program.add_import_paths(nickel_path.split(':'));
        }

        #[cfg(debug_assertions)]
        if self.nostdlib {
            program.set_skip_stdlib();
        }

        self.customize_mode.customize(program)
    }
}
