// You can import from packages with the placeholder syntax
// import "path/blah.ncl"@pkgname
// When dealing with recursive dependencies, we have to keep
// track of the package name mapping correctly: if we have
// import "blah.ncl"@foo
// and one of our package dependencies also has
// import "blah.ncl"@foo,
// then the two instances of "foo" might not refer to the same thing.
//
// FIXME: we need some protections on path imports. Like, it shouldn't be
// possible for a git import to import `../../../.cargo/credentials`
// For manifests in git dependencies, we could:
// - forbid path imports altogether, or
// - allow path imports, but only if they stay within the repo.
// For other manifests (from path deps or from registry deps) we should
// probably forbid path imports
use std::path::PathBuf;

use directories::ProjectDirs;
use manifest::{ManifestFile, Spec};
use serde::{Deserialize, Serialize};

use nickel_lang_core::{
    eval::cache::CacheImpl,
    package::{LockedPackageSource, Name},
    program::Program,
};

pub mod lock;
pub mod manifest;

// TODO: enrich all these errors with the location of the upstream manifest file
pub enum Error {
    ManifestEval {
        package: Name,
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
    pub fn matches_locked(&self, manifest: &ManifestFile, locked: &LockedPackageSource) -> bool {
        match (self, locked) {
            (PackageSource::Git { url }, LockedPackageSource::Git { repo, .. }) => url == repo,
            (PackageSource::Path { path }, LockedPackageSource::Path { path: locked_path }) => {
                manifest
                    .parent_dir
                    .as_ref()
                    .is_some_and(|pd| &pd.join(path) == locked_path)
            }
            _ => false,
        }
    }
}

fn cache_dir() -> PathBuf {
    let dir = ProjectDirs::from("org", "nickel-lang", "nickel").unwrap();
    dir.cache_dir().to_owned()
}
