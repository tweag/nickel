use std::path::{Path, PathBuf};

use directories::ProjectDirs;
use serde::{Deserialize, Serialize};

use nickel_lang_core::{eval::cache::CacheImpl, identifier::Ident, program::Program};

pub mod lock;
pub mod manifest;

pub use lock::LockedPackageSource;
pub use manifest::ManifestFile;

pub enum Error {
    Io {
        path: Option<PathBuf>,
        error: std::io::Error,
    },
    ManifestEval {
        package: Option<Ident>,
        program: Program<CacheImpl>,
        error: nickel_lang_core::error::Error,
    },
    RestrictedPath {
        package: Ident,
        attempted: PathBuf,
        restriction: PathBuf,
    },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Io { error, path } => {
                if let Some(path) = path {
                    write!(f, "{}: {error}", path.display())
                } else {
                    error.fmt(f)
                }
            }
            // Just a short and not-very-informative error. To write a better error message to
            // the terminal, use `program.report` like the cli does.
            Error::ManifestEval { package, .. } => {
                if let Some(package) = package {
                    write!(f, "error evaluating manifest for package {package}")
                } else {
                    write!(f, "error evaluating package manifest")
                }
            }
            Error::RestrictedPath {
                package,
                attempted,
                restriction,
            } => {
                write!(
                    f,
                    "package {package} tried to import path {}, but can only import from {}",
                    attempted.display(),
                    restriction.display()
                )
            }
        }
    }
}

trait ResultExt {
    type T;
    fn with_path(self, path: impl AsRef<Path>) -> Result<Self::T, Error>;
    fn without_path(self) -> Result<Self::T, Error>;
}

impl<T> ResultExt for Result<T, std::io::Error> {
    type T = T;
    fn with_path(self, path: impl AsRef<Path>) -> Result<T, Error> {
        self.map_err(|e| Error::Io {
            path: Some(path.as_ref().to_owned()),
            error: e,
        })
    }

    fn without_path(self) -> Result<T, Error> {
        self.map_err(|e| Error::Io {
            path: None,
            error: e,
        })
    }
}

/// A source includes the place to fetch a package from (e.g. git or a registry),
/// along with possibly some narrowing-down of the allowed versions (e.g. a range
/// of versions, or a git commit id).
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum PackageSource {
    // TODO: support branches other than HEAD
    Git {
        url: String,
        //tree: Option<git2::Oid>,
    },
    Path {
        path: PathBuf,
    },
    // TODO: non-git packages
}

impl PackageSource {
    pub fn matches_locked(&self, locked: &LockedPackageSource) -> bool {
        match (self, locked) {
            (PackageSource::Git { url }, LockedPackageSource::Git { repo, .. }) => url == repo,
            (PackageSource::Path { path }, LockedPackageSource::Path { path: locked_path }) => {
                path == locked_path
            }
            _ => false,
        }
    }
}

fn cache_dir() -> PathBuf {
    let dir = ProjectDirs::from("org", "nickel-lang", "nickel").unwrap();
    dir.cache_dir().to_owned()
}
