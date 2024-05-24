use std::path::PathBuf;

use directories::ProjectDirs;

pub mod error;
pub mod lock;
pub mod manifest;

pub use lock::LockedPackageSource;
pub use manifest::ManifestFile;

/// A source includes the place to fetch a package from (e.g. git or a registry),
/// along with possibly some narrowing-down of the allowed versions (e.g. a range
/// of versions, or a git commit id).
#[derive(Clone, Debug)]
pub enum PackageSource {
    // TODO: allow targeting branches or revisions, and allow supplying a relative path
    Git {
        url: gix::Url,
        //tree: Option<git2::Oid>,
    },
    Path {
        path: PathBuf,
    },
    // TODO: packages in a repository
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
