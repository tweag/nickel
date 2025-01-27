use std::path::PathBuf;

use nickel_lang_core::{eval::cache::lazy::CBNCache, program::Program};

#[cfg(feature = "package-experimental")]
use nickel_lang_package::config::Config as PackageConfig;
#[cfg(feature = "package-experimental")]
use nickel_lang_package::ManifestFile;

use crate::{customize::Customize, global::GlobalContext};

#[cfg(feature = "package-experimental")]
use crate::error::Error;

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

    /// Path to a package lock file.
    ///
    /// This is required for evaluations or imports that import packages.
    /// (Future versions may auto-detect or auto-create a lock file, in which
    /// case this argument will become optional.)
    #[arg(long, global = true)]
    pub manifest_path: Option<PathBuf>,

    #[arg(long, global = true)]
    /// Filesystem location for caching fetched packages.
    ///
    /// Defaults to an appropriate platform-dependent value, like
    /// `$XDG_CACHE_HOME/nickel` on linux.
    pub package_cache_dir: Option<PathBuf>,
}

pub enum PrepareError {
    /// Not a real error.
    ///
    /// Input preparation sometimes wants to print some info and then exit,
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
    fn prepare(&self, ctx: &mut GlobalContext) -> PrepareResult<Program<CBNCache>>;
}

impl<C: clap::Args + Customize> Prepare for InputOptions<C> {
    fn prepare(&self, ctx: &mut GlobalContext) -> PrepareResult<Program<CBNCache>> {
        let mut program = match self.files.as_slice() {
            [] => Program::new_from_stdin(std::io::stderr(), ctx.reporter.clone()),
            [p] => Program::new_from_file(p, std::io::stderr(), ctx.reporter.clone()),
            files => Program::new_from_files(files, std::io::stderr(), ctx.reporter.clone()),
        }?;

        program.add_import_paths(self.import_path.iter());

        if let Ok(nickel_path) = std::env::var("NICKEL_IMPORT_PATH") {
            program.add_import_paths(nickel_path.split(':'));
        }

        #[cfg(feature = "package-experimental")]
        {
            let manifest_path = self
                .manifest_path
                .clone()
                .or_else(|| crate::package::find_manifest().ok());

            if let Some(manifest_path) = manifest_path {
                let manifest = ManifestFile::from_path(&manifest_path)?;
                let mut config = PackageConfig::new()?;
                if let Some(cache_dir) = self.package_cache_dir.as_ref() {
                    config = config.with_cache_dir(cache_dir.to_owned());
                };

                let (_lock, realization) = manifest.lock(config.clone())?;
                program.set_package_map(realization.package_map(&manifest)?);
            }
        }

        #[cfg(debug_assertions)]
        if self.nostdlib {
            program.set_skip_stdlib();
        }

        self.customize_mode.customize(program)
    }
}
