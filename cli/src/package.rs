//! CLI commands for package management.

use std::{
    env::current_dir,
    path::{Path, PathBuf},
};

use nickel_lang_package::{
    config::Config,
    index::{self, PackageIndex},
    manifest::MANIFEST_NAME,
    resolve, ManifestFile,
};

use crate::{
    error::{CliResult, Error},
    global::GlobalContext,
};

#[derive(clap::Subcommand, Debug)]
pub enum Command {
    /// Generate a lock file for a package.
    ///
    /// The lock file contains exact information about a package's dependencies,
    /// including recursive dependencies.
    Lock {
        /// The path at which to write the lock file.
        ///
        /// Defaults to the filename `Nickel-pkg.lock` in the same directory
        /// as the manifest file.
        out: Option<PathBuf>,
    },
    /// Make a package map and print it out. For internal debugging.
    DebugResolution,
    /// Download all of a package's non-local dependencies into the given directory.
    DownloadDeps {
        #[arg(long)]
        out_dir: PathBuf,
    },
    /// Modify a local copy of the index, by adding a new version of a package.
    ///
    /// You must first push your package to a github repository, and make a note of
    /// the commit id that you want to publish.
    ///
    /// To actually publish to the global registry, you need to do a bunch more
    /// steps. Eventually, we'll provide tooling to automate this.
    ///
    /// 1. Fork the nickel mine (github.com/nickel-lang/nickel-mine) on github.
    /// 2. Clone your fork onto your local machine.
    /// 3. Run `nickel publish-local --index <directory-of-your-clone> --package-id github/you/your-package --commit-id <git hash> --version 0.1.0`
    /// 4. You should see that your local machine's index was modified. Commit that modification
    ///    and open a pull request to the nickel mine.
    PublishToLocalIndex {
        /// The location of the index to modify.
        #[arg(long)]
        index: PathBuf,

        /// The package id (like "github/nickel-lang/json-schema-lib") that you
        /// want to publish.
        #[arg(long)]
        package_id: index::Id,
    },
}

#[derive(clap::Parser, Debug)]
pub struct PackageCommand {
    #[command(subcommand)]
    pub command: Command,

    /// The location of the package's manifest file.
    ///
    /// If not found, nickel will attempt to find it by starting in the current directory
    /// and searching upwards until it finds a file named `Nickel-pkg.ncl`.
    #[arg(long, global = true)]
    pub manifest_path: Option<PathBuf>,
}

impl PackageCommand {
    fn find_manifest(&self) -> CliResult<PathBuf> {
        match &self.manifest_path {
            Some(p) => Ok(p.clone()),
            None => find_manifest(&current_dir()?),
        }
    }

    fn load_manifest(&self) -> CliResult<ManifestFile> {
        Ok(ManifestFile::from_path(self.find_manifest()?)?)
    }

    pub fn run(self, ctxt: &mut GlobalContext) {
        ctxt.reporter.report_result(self.run_result());
    }

    pub fn run_result(self) -> CliResult<()> {
        // TODO: have some global commands to change the config
        let config = Config::new()?;
        match &self.command {
            Command::Lock { out } => {
                let manifest = self.load_manifest()?;
                let out = match out {
                    Some(o) => o.clone(),
                    None => manifest.default_lockfile_path()?,
                };

                let (lock, _) = manifest.regenerate_lock(config)?;
                lock.write(&out)?;
            }
            Command::DebugResolution => {
                let path = self.find_manifest()?;
                let manifest = ManifestFile::from_path(path.clone())?;
                let snap = manifest.snapshot_dependencies(config.clone())?;
                let index = PackageIndex::shared(config.clone())?;
                let resolution = resolve::resolve(&manifest, snap, index, config)?;
                let package_map = resolution.package_map(&manifest)?;
                eprintln!("{package_map}");
            }
            Command::DownloadDeps { out_dir } => {
                let path = self.find_manifest()?;
                let manifest = ManifestFile::from_path(path.clone())?;
                let config = Config {
                    git_package_dir: out_dir.join("git-packages"),
                    ..config
                };

                manifest.snapshot_dependencies(config)?;
                // TODO: download index packages also
            }
            Command::PublishToLocalIndex { index, package_id } => {
                let config = config.with_index_dir(index.clone());
                let path = self.find_manifest()?;
                let manifest = ManifestFile::from_path(path.clone())?;
                let package =
                    nickel_lang_package::index::read_from_manifest(package_id, &manifest)?;
                let mut package_index = PackageIndex::exclusive(config)?;
                let version = package.version.clone();
                package_index.save(package)?;
                eprintln!(
                    "Added package {package_id}@{version} to the index at {}",
                    index.display()
                );
            }
        }

        Ok(())
    }
}

pub fn find_manifest(dir: &Path) -> CliResult<PathBuf> {
    let mut dir = dir.to_owned();

    loop {
        let path = dir.join(MANIFEST_NAME);
        if path.is_file() {
            return Ok(path);
        }

        if !dir.pop() {
            return Err(Error::NoManifest);
        }
    }
}
