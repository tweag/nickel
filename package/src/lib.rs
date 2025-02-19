use std::path::{Path, PathBuf};

use gix::{bstr::ByteSlice as _, Url};
use lock::{LockFileDep, LockPrecisePkg};
use nickel_lang_core::cache::normalize_abs_path;

use config::Config;
use error::Error;
use serde::{Deserialize, Serialize};

pub mod config;
pub mod error;
pub mod index;
pub mod lock;
pub mod manifest;
pub mod snapshot;
pub mod version;

pub use gix::ObjectId;
pub use manifest::ManifestFile;
use version::{SemVer, VersionReq};

/// A dependency that comes from a git repository.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub struct GitDependency {
    /// The url of the git repo, in any format understood by `gix`.
    /// For example, it can be a path, an https url, or an ssh url.
    #[serde(with = "serde_url")]
    pub url: gix::Url,
    #[serde(default, rename = "ref")]
    pub target: nickel_lang_git::Target,
    /// The path to the nickel package within the git repo, if it is not at the top level.
    #[serde(default)]
    pub path: PathBuf,
}

impl GitDependency {
    /// If this git dependency specifies a relative path, make it absolute.
    pub fn relative_to(&self, relative_to: Option<&Path>) -> Result<Self, Error> {
        if self.url.scheme.as_str() != "file" {
            return Ok(self.clone());
        }
        // unwrap: the url ultimately came from a nickel file, which is always valid UTF-8.
        let path = Path::new(self.url.path.to_str().unwrap());
        if path.is_absolute() {
            return Ok(self.clone());
        }

        match relative_to {
            Some(relative_to) => {
                let abs_path = relative_to.join(path);
                Ok(GitDependency {
                    url: Url::try_from(abs_path.as_path()).map_err(|e| Error::InvalidUrl {
                        url: abs_path.display().to_string(),
                        msg: e.to_string(),
                    })?,
                    ..self.clone()
                })
            }
            None => Err(Error::RelativeGitImport {
                path: path.to_owned(),
            }),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// A dependency that comes from the global package index.
pub struct IndexDependency {
    id: index::Id,
    version: VersionReq,
}

/// A source includes the place to fetch a package from (e.g. git or a registry),
/// along with possibly some narrowing-down of the allowed versions (e.g. a range
/// of versions, or a git commit id).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Dependency {
    Git(GitDependency),
    Path(PathBuf),
    Index(IndexDependency),
}

impl Dependency {
    pub fn matches(&self, entry: &LockFileDep, precise: &LockPrecisePkg) -> bool {
        match self {
            Dependency::Git(git) => match &entry.spec {
                Some(locked_git) => git == locked_git,
                None => false,
            },
            Dependency::Path(_) => true,
            Dependency::Index(i) => {
                if let LockPrecisePkg::Index { id, version } = precise {
                    i.id == *id && i.version.matches(version)
                } else {
                    false
                }
            }
        }
    }

    pub fn as_index_dep(self, parent_id: index::Id) -> Result<IndexDependency, Error> {
        match self {
            Dependency::Index(i) => Ok(i),
            Dependency::Git(g) => Err(Error::InvalidIndexDep {
                id: parent_id.clone(),
                dep: Box::new(crate::UnversionedDependency::Git(g)),
            }),
            Dependency::Path(path) => Err(Error::InvalidIndexDep {
                id: parent_id.clone(),
                dep: Box::new(crate::UnversionedDependency::Path(path)),
            }),
        }
    }

    pub fn as_unversioned(self) -> Option<UnversionedDependency> {
        match self {
            Dependency::Index(_i) => None,
            Dependency::Git(g) => Some(UnversionedDependency::Git(g)),
            Dependency::Path(p) => Some(UnversionedDependency::Path(p)),
        }
    }
}

/// The same as [`Dependency`], but only for the packages that have fixed, unresolvable, versions.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum UnversionedDependency {
    Git(GitDependency),
    Path(PathBuf),
}

impl From<UnversionedDependency> for Dependency {
    fn from(p: UnversionedDependency) -> Self {
        match p {
            UnversionedDependency::Git(git) => Dependency::Git(git),
            UnversionedDependency::Path(path) => Dependency::Path(path),
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
        let s = String::deserialize(de)?;
        gix::Url::try_from(s).map_err(|e| D::Error::custom(e.to_string()))
    }
}

/// A precise package version, in a format suitable for putting into a lockfile.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum PrecisePkg {
    Git {
        // We use `Precise` for a few different purposes, and not all of them need the url. (For
        // resolution, for example, we could consider two git deps equal if they have the same id
        // even if they came from different sources.) However, the lockfile should have a repo url in
        // it, because it allows us to fetch the package if it isn't available.
        url: gix::Url,
        id: ObjectId,
        path: PathBuf,
    },
    /// The path is normalized (i.e., all '..'s are at the beginning), and relative
    /// to the top-level package manifest.
    ///
    /// Note that when normalizing we only look at the path and not at the actual filesystem.
    Path { path: PathBuf },
    /// A package in the global package index.
    Index { id: index::Id, version: SemVer },
}

impl PrecisePkg {
    /// Where on the local filesystem can this package be found?
    ///
    /// Note that the package might not actually be there yet, if it's a git or
    /// inde package that hasn't been fetched.
    pub fn local_path(&self, config: &Config) -> PathBuf {
        match self {
            PrecisePkg::Git { id, path, .. } => repo_root(config, id).join(path),
            PrecisePkg::Path { path } => Path::new(path).to_owned(),
            PrecisePkg::Index { id, version } => config
                .index_package_dir
                .join(id.path())
                .join(version.to_string()),
        }
    }

    /// Is this a path package?
    pub fn is_path(&self) -> bool {
        matches!(self, PrecisePkg::Path { .. })
    }

    /// Is this package available offline? If not, it needs to be fetched.
    pub fn is_available_offline(&self, config: &Config) -> bool {
        // We consider path-dependencies to be always available offline, even if they don't exist.
        // We consider git-dependencies to be available offline if there's a directory at
        // `~/.cache/nickel/git/ed8234.../` (or wherever the cache directory is on your system). We
        // don't check if that directory contains the right git repository -- if someone has messed
        // with the contents of `~/.cache/nickel`, that's your problem.
        match self {
            PrecisePkg::Path { .. } => true,
            _ => self.local_path(config).is_dir(),
        }
    }

    /// If this is a path package with a relative path, turn it into an abolute path, relative to `root`.
    pub fn with_abs_path(self, root: &std::path::Path) -> Self {
        match self {
            PrecisePkg::Path { path } => PrecisePkg::Path {
                path: normalize_abs_path(&root.join(path)),
            },
            x => x,
        }
    }
}

/// The path in our local filesystem where we store the git repo with the given id.
fn repo_root(config: &Config, id: &ObjectId) -> PathBuf {
    config.git_package_dir.join(id.to_string())
}
