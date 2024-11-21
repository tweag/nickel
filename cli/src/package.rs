use std::{
    collections::HashMap,
    env::current_dir,
    path::{Path, PathBuf},
};

use nickel_lang_core::{identifier::Ident, package::PackageMap};
use nickel_lang_package::{
    config::Config,
    index::{self, PackageIndex},
    version::SemVer,
    ManifestFile, ObjectId,
};

use crate::{
    error::{CliResult, Error},
    global::GlobalContext,
};

#[derive(clap::Subcommand, Debug)]
pub enum Command {
    GenerateLockfile,
    DebugResolution,
    DownloadDeps {
        #[arg(long)]
        out_dir: PathBuf,
    },
    RefreshIndex,
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
    PublishLocal {
        #[arg(long)]
        index: PathBuf,

        #[arg(long)]
        version: SemVer,

        #[arg(long)]
        commit_id: ObjectId,

        #[arg(long)]
        package_id: index::Id,
    },
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

    pub fn run(self, ctxt: &mut GlobalContext) {
        ctxt.reporter.report_result(self.run_result());
    }

    pub fn run_result(self) -> CliResult<()> {
        // TODO: have some global commands to change the config
        match &self.command {
            Command::GenerateLockfile => {
                self.load_manifest()?.regenerate_lock(Config::default())?;
            }
            Command::DebugResolution => {
                let path = self.find_manifest()?;
                let manifest = ManifestFile::from_path(path.clone())?;
                let resolution = manifest.resolve(Config::default())?;
                let package_map = resolution.package_map(&manifest)?;
                print_package_map(&package_map);
            }
            Command::PublishLocal {
                index,
                package_id,
                version,
                commit_id,
            } => {
                let package =
                    nickel_lang_package::index::fetch_git(package_id, version.clone(), commit_id)?;
                let config = Config::default().with_index_dir(index.clone());
                let mut package_index = PackageIndex::new(config);
                package_index.save(package)?;
                eprintln!(
                    "Added package {package_id}@{version} to the index at {}",
                    index.display()
                );
            }
            Command::RefreshIndex => {
                let index = PackageIndex::new(Config::default());
                index.fetch_from_github()?;
            }
            Command::DownloadDeps { out_dir } => {
                let path = self.find_manifest()?;
                let manifest = ManifestFile::from_path(path.clone())?;
                let config = Config {
                    index_package_dir: out_dir.join("index-packages"),
                    git_package_dir: out_dir.join("git-packages"),
                    ..Config::default()
                };

                let resolution = manifest.resolve(config)?;

                for (pkg, versions) in resolution.index_packages {
                    for v in versions {
                        resolution.index.ensure_downloaded(&pkg, v).unwrap();
                    }
                }
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
