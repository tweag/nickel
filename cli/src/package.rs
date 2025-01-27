//! CLI commands for package management.

use std::{
    collections::HashMap,
    env::current_dir,
    path::{Path, PathBuf},
};

use nickel_lang_core::{identifier::Ident, package::PackageMap};
use nickel_lang_package::{config::Config, ManifestFile};

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
    GenerateLockfile {
        /// The path at which to write the lock file.
        ///
        /// Defaults to the filename `package.ncl.lock` in the same directory
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
}

#[derive(clap::Parser, Debug)]
pub struct PackageCommand {
    #[command(subcommand)]
    pub command: Command,

    /// The location of the package's manifest file.
    ///
    /// If not found, nickel will attempt to find it by starting in the current directory
    /// and searching upwards until it finds a file named `package.ncl`.
    #[arg(long, global = true)]
    pub manifest_path: Option<PathBuf>,
}

impl PackageCommand {
    fn find_manifest(&self) -> CliResult<PathBuf> {
        match &self.manifest_path {
            Some(p) => Ok(p.clone()),
            None => find_manifest(),
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
            Command::GenerateLockfile { out } => {
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
                let realization = manifest.realize_dependencies(config)?;
                let package_map = realization.package_map(&manifest)?;
                print_package_map(&package_map);
            }
            Command::DownloadDeps { out_dir } => {
                let path = self.find_manifest()?;
                let manifest = ManifestFile::from_path(path.clone())?;
                let config = Config {
                    git_package_dir: out_dir.join("git-packages"),
                    ..config
                };

                manifest.realize_dependencies(config)?;
            }
        }

        Ok(())
    }
}

fn print_package_map(map: &PackageMap) {
    let mut by_parent: HashMap<&Path, Vec<(Ident, &Path)>> = HashMap::new();
    for ((parent, name), child) in &map.packages {
        by_parent
            .entry(parent.as_path())
            .or_default()
            .push((*name, child));
    }

    if map.top_level.is_empty() {
        eprintln!("No top-level dependencies");
    } else {
        eprintln!("Top-level dependencies:");
        let mut top_level = map.top_level.iter().collect::<Vec<_>>();
        top_level.sort();
        for (name, path) in top_level {
            eprintln!("  {} -> {}", name, path.display());
        }
    }

    let mut by_parent = by_parent.into_iter().collect::<Vec<_>>();
    by_parent.sort();
    if by_parent.is_empty() {
        eprintln!("No transitive dependencies");
    } else {
        eprintln!("Transitive dependencies:");

        for (parent, mut deps) in by_parent {
            deps.sort();
            eprintln!("  {}", parent.display());

            for (name, path) in deps {
                eprintln!("    {} -> {}", name, path.display());
            }
        }
    }
}
pub fn find_manifest() -> CliResult<PathBuf> {
    let mut dir = current_dir()?;

    loop {
        let path = dir.join("package.ncl");
        if path.is_file() {
            return Ok(path);
        }

        if !dir.pop() {
            return Err(Error::NoManifest);
        }
    }
}
