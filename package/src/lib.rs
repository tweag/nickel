use std::path::PathBuf;

use directories::ProjectDirs;
use manifest::Spec;
use serde::{Deserialize, Serialize};

use nickel_lang_core::{eval::cache::CacheImpl, package::Name, program::Program};

pub mod lock;
pub mod manifest;

pub use lock::LockedPackageSource;
pub use manifest::ManifestFile;

// TODO: enrich all these errors with the location of the upstream manifest file
pub enum Error {
    ManifestEval {
        package: Option<Name>,
        program: Program<CacheImpl>,
        error: nickel_lang_core::error::Error,
    },
    InvalidPathImport {
        spec: Spec,
    },
    RestrictedPath {
        attempted: PathBuf,
        restriction: PathBuf,
    },
}

// TODO use IntoDiagnostics instead of this. Requires preserving positions from manifest files
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ManifestEval { .. } => todo!(),
            Error::InvalidPathImport { spec } => {
                write!(f, "invalid import path: {spec:?}")
            }
            Error::RestrictedPath {
                attempted,
                restriction,
            } => todo!(),
        }
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
