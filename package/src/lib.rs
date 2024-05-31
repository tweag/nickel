use std::path::{Path, PathBuf};

use directories::ProjectDirs;

pub mod error;
pub mod index;
pub mod lock;
pub mod manifest;
pub mod resolve;
pub mod util;

pub use lock::LockedPackageSource;
pub use manifest::ManifestFile;
use nickel_lang_core::package::ObjectId;
use semver::{Version, VersionReq};

/// A source includes the place to fetch a package from (e.g. git or a registry),
/// along with possibly some narrowing-down of the allowed versions (e.g. a range
/// of versions, or a git commit id).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Dependency {
    // TODO: allow targeting branches or revisions, and allow supplying a relative path
    Git {
        url: gix::Url,
        //tree: Option<git2::Oid>,
    },
    Path {
        path: PathBuf,
    },
    Repo {
        id: index::Id,
        version: VersionReq,
    },
}

impl Dependency {
    pub fn matches_locked(&self, locked: &LockedPackageSource) -> bool {
        match (self, locked) {
            (Dependency::Git { url }, LockedPackageSource::Git { repo, .. }) => url == repo,
            (Dependency::Path { path }, LockedPackageSource::Path { path: locked_path }) => {
                path == locked_path
            }
            _ => false,
        }
    }
}

/// A precise package version, in a format suitable for putting into a lockfile.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Precise {
    Git {
        id: ObjectId,
        path: PathBuf,
    },
    /// The path is normalized (i.e., all '..'s are at the beginning), and relative
    /// to the top-level package manifest.
    ///
    /// Note that when normalizing we only look at the path and not at the actual filesystem.
    Path {
        path: PathBuf,
    },
    Repo {
        id: index::Id,
        version: Version,
    },
}

impl Precise {
    /// Where on the local filesystem can this package be found?
    ///
    /// Note: it might not actually be there yet, if it's a git package that hasn't been fetched.
    pub fn local_path(&self) -> PathBuf {
        match self {
            Precise::Git { id, path } => {
                let cache_dir = crate::cache_dir();
                repo_root(id).join(path)
            }
            Precise::Path { path } => Path::new(path).to_owned(),
            Precise::Repo { id, version } => todo!(),
        }
    }
}

fn repo_root(id: &ObjectId) -> PathBuf {
    cache_dir().join(id.to_string())
}

fn cache_dir() -> PathBuf {
    let dir = ProjectDirs::from("org", "nickel-lang", "nickel").unwrap();
    dir.cache_dir().to_owned()
}
