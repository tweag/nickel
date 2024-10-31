use std::path::PathBuf;

use nickel_lang_core::{eval::cache::lazy::CBNCache, program::Program};

use crate::{
    cli::GlobalOptions, color_opt_from_clap, customize::Customize, error::CliResult,
    global::GlobalContext,
};

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

    #[command(flatten)]
    pub customize_mode: Customize,
}

pub enum PrepareError {
    /// Not a real error.
    ///
    /// Input preparation sometimes wants to print some info and the exit,
    /// without proceeding to program evaluation. This error variant is used to
    /// trigger the early return, but we don't print an error message or exit
    /// with an error status.
    EarlyReturn,
    /// A real error.
    Error(crate::error::Error),
}

impl<E: Into<crate::error::Error>> From<E> for PrepareError {
    fn from(e: E) -> Self {
        PrepareError::Error(e.into())
    }
}

pub type PrepareResult<T> = Result<T, PrepareError>;

pub trait Prepare {
    fn prepare<'ctx>(&self, ctx: &'ctx mut GlobalContext)
        -> PrepareResult<Program<'ctx, CBNCache>>;
}

impl<C: clap::Args + Customize> Prepare for InputOptions<C> {
    fn prepare<'ctx>(
        &self,
        ctx: &'ctx mut GlobalContext,
    ) -> PrepareResult<Program<'ctx, CBNCache>> {
        let mut program = match self.files.as_slice() {
            [] => Program::new_from_stdin(std::io::stderr(), ctx),
            [p] => Program::new_from_file(p, std::io::stderr(), ctx),
            files => Program::new_from_files(files, std::io::stderr(), ctx),
        }?;

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
