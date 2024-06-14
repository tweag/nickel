use std::path::{Path, PathBuf};

pub mod error;
pub mod index;
pub mod lock;
pub mod manifest;
pub mod resolve;
pub mod util;

pub use manifest::ManifestFile;
use nickel_lang_core::{cache::normalize_abs_path, package::ObjectId};
use semver::{Version, VersionReq};
use serde::{Deserialize, Serialize};
use util::cache_dir;

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
    Index {
        id: index::Id,
        version: VersionReq,
    },
}

impl Dependency {
    pub fn matches(&self, precise: &Precise) -> bool {
        match (self, precise) {
            (Dependency::Git { url }, Precise::Git { repo, .. }) => url == repo,
            (Dependency::Path { path }, Precise::Path { path: locked_path }) => path == locked_path,
            (
                Dependency::Index {
                    id: dep_id,
                    version: dep_version,
                },
                Precise::Index { id, version },
            ) => id == dep_id && dep_version.matches(version),
            _ => false,
        }
    }
}

mod serde_url {
    use serde::{de::Error, Deserialize, Serialize as _};

    pub fn serialize<S: serde::Serializer>(url: &gix::Url, ser: S) -> Result<S::Ok, S::Error> {
        // unwrap: locked urls can only come from nickel strings in the manifest file, which must be
        // valid utf-8
        std::str::from_utf8(url.to_bstring().as_slice())
            .unwrap()
            .serialize(ser)
    }

    pub fn deserialize<'de, D: serde::Deserializer<'de>>(de: D) -> Result<gix::Url, D::Error> {
        let s = <&str>::deserialize(de)?;
        gix::Url::try_from(s).map_err(|e| D::Error::custom(e.to_string()))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub struct IndexPrecise {
    id: index::Id,
    version: Version,
}

/// A precise package version, in a format suitable for putting into a lockfile.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub enum Precise {
    Git {
        // We use `Precise` for a few different purposes, and not all of them need the url. (For
        // resolution, for example, we could consider two git deps equal if they have the same id
        // even if they came from different sources.) However, the lockfile should have a repo url in
        // it, because it allows us to fetch the package if it isn't available, and it allows us to
        // check if the locked dependency matches the manifest (which might only have the url).
        #[serde(with = "serde_url")]
        repo: gix::Url,
        id: ObjectId,
        path: PathBuf,
    },
    /// The path is normalized (i.e., all '..'s are at the beginning), and relative
    /// to the top-level package manifest.
    ///
    /// Note that when normalizing we only look at the path and not at the actual filesystem.
    Path { path: PathBuf },
    Index {
        // TODO: IndexPrecise
        id: index::Id,
        version: Version,
    },
}

impl Precise {
    /// Where on the local filesystem can this package be found?
    ///
    /// Note: it might not actually be there yet, if it's a git or index package that hasn't been fetched.
    pub fn local_path(&self) -> PathBuf {
        match self {
            Precise::Git { id, path, .. } => repo_root(id).join(path),
            Precise::Path { path } => Path::new(path).to_owned(),
            Precise::Index { id, version } => cache_dir()
                .join("index-packages")
                .join(format!("{id}-{version}")),
        }
    }

    pub fn is_path(&self) -> bool {
        matches!(self, Precise::Path { .. })
    }

    /// Is this locked package available offline? If not, it needs to be fetched.
    pub fn is_available_offline(&self) -> bool {
        // We consider path-dependencies to be always available offline, even if they don't exist.
        // We consider git-dependencies to be available offline if there's a directory at
        // `~/.cache/nickel/ed8234.../` (or wherever the cache directory is on your system). We
        // don't check if that directory contains the right git repository -- if someone has messed
        // with the contents of `~/.cache/nickel`, that's your problem.
        match self {
            Precise::Path { .. } => true,
            _ => self.local_path().is_dir(),
        }
    }

    pub fn with_abs_path(self, root: &std::path::Path) -> Self {
        match self {
            Precise::Path { path } => Precise::Path {
                path: normalize_abs_path(&root.join(path)),
            },
            x => x,
        }
    }

    pub fn version(&self) -> Option<Version> {
        match self {
            Precise::Index { version, .. } => Some(version.clone()),
            _ => None,
        }
    }
}

fn repo_root(id: &ObjectId) -> PathBuf {
    cache_dir().join(id.to_string())
}
