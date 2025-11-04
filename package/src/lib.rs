// TODO: many (most?) of our error variants are large. This might be
// automatically solved by converting to the new runtime representation,
// but if not then we should reduce them.
#![allow(clippy::result_large_err)]

use std::path::{Path, PathBuf};

use gix::{Url, bstr::ByteSlice as _};
use index::{LockType, PackageIndex};
use lock::{LockFileDep, LockPrecisePkg};
use nickel_lang_core::cache::normalize_abs_path;

use config::Config;
use error::Error;
use serde::{Deserialize, Serialize};

macro_rules! warn {
    ($($tts:tt)*) => {
        eprintln!($($tts)*);
    }
}

macro_rules! info {
    ($($tts:tt)*) => {
        eprintln!($($tts)*);
    }
}

pub mod config;
pub mod error;
pub mod index;
pub mod lock;
pub mod manifest;
pub mod resolve;
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
                    url: Url::try_from(abs_path.as_path())?,
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
    pub id: index::Id,
    pub version: VersionReq,
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

    pub fn as_index_dep(self, parent_id: &index::Id) -> Result<IndexDependency, Error> {
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

/// The subtype of [`Dependency`] containing just the git and path variants.
///
/// We call these "unversioned" because they don't have version numbers that get
/// decided during resolution.
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
    use serde::{Deserialize, Serialize as _, de::Error};

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

/// A package that comes from the global package index, with a precise version.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PreciseIndexPkg {
    pub id: index::Id,
    pub version: SemVer,
}

impl PreciseIndexPkg {
    /// Where on the local filesystem can we find the root of the git repository
    /// containing this package?
    ///
    /// We don't currently support git filters, so for index packages that live
    /// in a subdirectory of a git repository we fetch the whole repository
    /// (into the path returned by this method). The package itself lives in a
    /// subdirectory; see [`PreciseIndexPkg::local_path`] for that one.
    pub fn local_path_without_subdir<T: LockType>(
        &self,
        config: &Config,
        index: &PackageIndex<T>,
    ) -> Result<PathBuf, Error> {
        let pkg = index.package(&self.id, &self.version)?;

        Ok(config
            .index_package_dir
            .join("contents")
            .join(pkg.id.object_id().to_string()))
    }

    /// Where on the local filesystem can this package be found?
    //
    /// Note that the package might not actually be there yet.
    pub fn local_path<T: LockType>(
        &self,
        config: &Config,
        index: &PackageIndex<T>,
    ) -> Result<PathBuf, Error> {
        let index::Id::Github { path, .. } = &self.id;
        Ok(self.local_path_without_subdir(config, index)?.join(path))
    }
}

/// A git package, with a precise version.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PreciseGitPkg {
    url: gix::Url,
    id: ObjectId,
    path: PathBuf,
}

impl PreciseGitPkg {
    /// Where on the local filesystem can this package be found?
    //
    /// Note that the package might not actually be there yet.
    pub fn local_path(&self, config: &Config) -> PathBuf {
        repo_root(config, &self.id).join(&self.path)
    }
}

/// A precise package version, in a format suitable for putting into a lockfile.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum PrecisePkg {
    /// A git package.
    Git(PreciseGitPkg),
    /// The path is normalized (i.e., all '..'s are at the beginning), and
    /// relative to the top-level package manifest. (Technically, it could be an
    /// absolute path if that's what they wrote in the manifest. But we haven't
    /// *turned* it into an absolute path.)
    ///
    /// Note that when normalizing we only look at the path and not at the actual filesystem.
    Path(PathBuf),
    /// A package in the global package index.
    Index(PreciseIndexPkg),
}

impl PrecisePkg {
    /// Where on the local filesystem can this package be found?
    ///
    /// Note that the package might not actually be there yet, if it's a git or
    /// index package that hasn't been fetched.
    pub fn local_path<T: LockType>(
        &self,
        config: &Config,
        index: &PackageIndex<T>,
    ) -> Result<PathBuf, Error> {
        match self {
            PrecisePkg::Git(pkg) => Ok(pkg.local_path(config)),
            PrecisePkg::Path(path) => Ok(path.clone()),
            PrecisePkg::Index(PreciseIndexPkg { id, version }) => {
                let pkg = index.package(id, version)?;
                let index::Id::Github { path, .. } = id;
                Ok(config
                    .index_package_dir
                    .join("contents")
                    .join(pkg.id.object_id().to_string())
                    .join(path))
            }
        }
    }

    /// Is this a path package?
    pub fn is_path(&self) -> bool {
        matches!(self, PrecisePkg::Path { .. })
    }

    /// Is this package available offline? If not, it needs to be fetched.
    pub fn is_available_offline<T: LockType>(
        &self,
        config: &Config,
        index: &PackageIndex<T>,
    ) -> bool {
        // We consider path-dependencies to be always available offline, even if they don't exist.
        // We consider git-dependencies to be available offline if there's a directory at
        // `~/.cache/nickel/git/ed8234.../` (or wherever the cache directory is on your system). We
        // don't check if that directory contains the right git repository -- if someone has messed
        // with the contents of `~/.cache/nickel`, that's your problem.
        match self {
            PrecisePkg::Path { .. } => true,
            _ => self.local_path(config, index).is_ok_and(|p| p.is_dir()),
        }
    }

    /// If this is a path package with a relative path, turn it into an abolute path, relative to `root`.
    pub fn with_abs_path(self, root: &std::path::Path) -> Self {
        match self {
            PrecisePkg::Path(path) => PrecisePkg::Path(normalize_abs_path(&root.join(path))),
            x => x,
        }
    }

    pub fn unversioned_or_index(self) -> Result<UnversionedPrecisePkg, PreciseIndexPkg> {
        match self {
            PrecisePkg::Git(g) => Ok(UnversionedPrecisePkg::Git(g)),
            PrecisePkg::Path(p) => Ok(UnversionedPrecisePkg::Path(p)),
            PrecisePkg::Index(i) => Err(i),
        }
    }
}

/// A precise package version, but only the ones that don't have versions to resolve.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UnversionedPrecisePkg {
    Git(PreciseGitPkg),
    Path(PathBuf),
}

impl UnversionedPrecisePkg {
    pub fn local_path(&self, config: &Config) -> PathBuf {
        match self {
            Self::Git(PreciseGitPkg { id, path, .. }) => repo_root(config, id).join(path),
            Self::Path(path) => path.clone(),
        }
    }
}

impl From<UnversionedPrecisePkg> for PrecisePkg {
    fn from(uv: UnversionedPrecisePkg) -> PrecisePkg {
        match uv {
            UnversionedPrecisePkg::Git(g) => PrecisePkg::Git(g),
            UnversionedPrecisePkg::Path(p) => PrecisePkg::Path(p),
        }
    }
}

/// The path in our local filesystem where we store the git repo with the given id.
fn repo_root(config: &Config, id: &ObjectId) -> PathBuf {
    config.git_package_dir.join(id.to_string())
}
