use std::{env::current_dir, path::PathBuf};

use nickel_lang_package::ManifestFile;

use crate::{
    cli::GlobalOptions,
    error::{CliResult, Error},
};

#[derive(clap::Subcommand, Debug)]
pub enum Command {
    GenerateLockfile,
    FullyResolve,
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
            Command::FullyResolve => {
                let path = self.find_manifest()?;
                let root_path = path.parent().unwrap(); // FIXME
                let lock = ManifestFile::from_path(path.clone())?.lock()?;
                let resolved = lock.resolve_package_map(root_path.to_owned())?;
                dbg!(resolved);
            }
        }

        Ok(())
    }
}
