use std::{
    collections::HashMap,
    env::current_dir,
    path::{Path, PathBuf},
};

use nickel_lang_core::package::{Name, PackageMap};
use nickel_lang_package::ManifestFile;

use crate::{
    cli::GlobalOptions,
    error::{CliResult, Error},
};

#[derive(clap::Subcommand, Debug)]
pub enum Command {
    GenerateLockfile,
    DebugResolution,
}

#[derive(clap::Parser, Debug)]
pub struct PackageCommand {
    #[command(subcommand)]
    pub command: Command,

    #[arg(long, global = true)]
    pub manifest_path: Option<PathBuf>,
}

impl PackageCommand {
    fn find_manifest(&self) -> CliResult<PathBuf> {
        match &self.manifest_path {
            Some(p) => Ok(p.clone()),
            None => {
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
        }
    }

    fn load_manifest(&self) -> CliResult<ManifestFile> {
        Ok(ManifestFile::from_path(self.find_manifest()?)?)
    }

    pub fn run(self, _global: GlobalOptions) -> CliResult<()> {
        match &self.command {
            Command::GenerateLockfile => {
                self.load_manifest()?.lock()?;
            }
            Command::DebugResolution => {
                let path = self.find_manifest()?;
                let root_path = path.parent().unwrap(); // FIXME
                let lock = ManifestFile::from_path(path.clone())?.lock()?;
                let resolved = lock.resolve_package_map(root_path.to_owned())?;
                print_package_map(&resolved);
            }
        }

        Ok(())
    }
}

fn print_package_map(map: &PackageMap) {
    let mut by_parent: HashMap<&Path, Vec<(&Name, &Path)>> = HashMap::new();
    for ((parent, name), child) in &map.packages {
        by_parent
            .entry(parent.as_path())
            .or_default()
            .push((name, child));
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
