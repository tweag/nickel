use std::path::PathBuf;

use directories::ProjectDirs;
use serde::{Deserialize, Serialize};

pub mod error;
pub mod lock;
pub mod manifest;

pub use lock::LockedPackageSource;
pub use manifest::ManifestFile;

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
