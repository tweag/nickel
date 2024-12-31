use std::{
    path::{Path, PathBuf},
    str::FromStr,
};

use nickel_lang_core::cache::normalize_abs_path;

use config::Config;
use error::Error;
use serde::{Deserialize, Serialize};
use serde_with::{DeserializeFromStr, SerializeDisplay};
use version::{PartialSemVerParseError, SemVer, SemVerParseError, SemVerPrefix};

pub mod config;
pub mod error;
pub mod index;
pub mod lock;
pub mod manifest;
pub mod resolve;
pub mod version;

pub use gix::ObjectId;
pub use manifest::ManifestFile;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize)]
pub struct GitDependency {
    /// The url of the git repo, in any format understood by `gix`.
    /// For example, it can be a path.
    #[serde(with = "serde_url")]
    pub url: gix::Url,
    #[serde(default, rename = "ref")]
    pub target: nickel_lang_git::Target,
    /// The path to the nickel package within the git repo, if it is not at the top level.
    #[serde(default)]
    pub path: PathBuf,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, DeserializeFromStr, SerializeDisplay)]
pub enum VersionReq {
    Compatible(SemVerPrefix),
    Exact(SemVer),
}

impl std::fmt::Display for VersionReq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VersionReq::Compatible(v) => v.fmt(f),
            VersionReq::Exact(v) => write!(f, "={v}"),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum VersionReqParseError {
    #[error(transparent)]
    Exact(#[from] SemVerParseError),
    #[error(transparent)]
    Compatible(#[from] PartialSemVerParseError),
}

impl FromStr for VersionReq {
    type Err = VersionReqParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(v) = s.strip_prefix('=') {
            Ok(VersionReq::Exact(v.parse()?))
        } else {
            Ok(VersionReq::Compatible(SemVerPrefix::from_str(s)?))
        }
    }
}

impl VersionReq {
    pub fn matches(&self, v: &SemVer) -> bool {
        match self {
            VersionReq::Compatible(lower_bound) => lower_bound.matches(v),
            VersionReq::Exact(w) => v == w,
        }
    }
}

/// A source includes the place to fetch a package from (e.g. git or a registry),
/// along with possibly some narrowing-down of the allowed versions (e.g. a range
/// of versions, or a git commit id).
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize)]
pub enum Dependency {
    Git(GitDependency),
    Path { path: PathBuf },
    Index { id: index::Id, version: VersionReq },
}

/// The same as [`Dependency`], but only for the packages that have fixed, unresolvable, versions.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum UnversionedPackage {
    Git(GitDependency),
    Path { path: PathBuf },
}

impl From<UnversionedPackage> for Dependency {
    fn from(p: UnversionedPackage) -> Self {
        match p {
            UnversionedPackage::Git(git) => Dependency::Git(git),
            UnversionedPackage::Path { path } => Dependency::Path { path },
        }
    }
}

impl Dependency {
    pub fn matches(&self, precise: &Precise) -> bool {
        match (self, precise) {
            (Dependency::Git(git), Precise::Git { url: repo, .. }) => &git.url == repo,
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

    pub fn as_index_dep(self, parent_id: index::Id) -> Result<index::IndexDependency, Error> {
        match self {
            Dependency::Index { id, version } => Ok(index::IndexDependency { id, req: version }),
            Dependency::Git(g) => Err(Error::InvalidIndexDep {
                id: parent_id.clone(),
                dep: Box::new(crate::UnversionedPackage::Git(g)),
            }),
            Dependency::Path { path } => Err(Error::InvalidIndexDep {
                id: parent_id.clone(),
                dep: Box::new(crate::UnversionedPackage::Path { path }),
            }),
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub struct IndexPrecise {
    id: index::Id,
    version: SemVer,
}

/// A precise package version, in a format suitable for putting into a lockfile.
#[serde_with::serde_as]
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub enum Precise {
    Git {
        // We use `Precise` for a few different purposes, and not all of them need the url. (For
        // resolution, for example, we could consider two git deps equal if they have the same id
        // even if they came from different sources.) However, the lockfile should have a repo url in
        // it, because it allows us to fetch the package if it isn't available, and it allows us to
        // check if the locked dependency matches the manifest (which might only have the url).
        #[serde(with = "serde_url")]
        url: gix::Url,
        // Serialize/deserialize as hex strings.
        #[serde_as(as = "serde_with::DisplayFromStr")]
        id: ObjectId,
        path: PathBuf,
    },
    /// The path is normalized (i.e., all '..'s are at the beginning), and relative
    /// to the top-level package manifest.
    ///
    /// Note that when normalizing we only look at the path and not at the actual filesystem.
    /// TODO: maybe just leave out the path altogether? cargo does...
    Path {
        path: PathBuf,
    },
    Index {
        id: index::Id,
        version: SemVer,
    },
}

impl Precise {
    /// Where on the local filesystem can this package be found?
    ///
    /// Note that the package might not actually be there yet, if it's a git or
    /// index package that hasn't been fetched.
    pub fn local_path(&self, config: &Config) -> PathBuf {
        match self {
            Precise::Git { id, path, .. } => repo_root(config, id).join(path),
            Precise::Path { path } => Path::new(path).to_owned(),
            Precise::Index { id, version } => config
                .index_package_dir
                .join(id.path())
                .join(version.to_string()),
        }
    }

    pub fn is_path(&self) -> bool {
        matches!(self, Precise::Path { .. })
    }

    /// Is this locked package available offline? If not, it needs to be fetched.
    pub fn is_available_offline(&self, config: &Config) -> bool {
        // We consider path-dependencies to be always available offline, even if they don't exist.
        // We consider git-dependencies to be available offline if there's a directory at
        // `~/.cache/nickel/git/ed8234.../` (or wherever the cache directory is on your system). We
        // don't check if that directory contains the right git repository -- if someone has messed
        // with the contents of `~/.cache/nickel`, that's your problem.
        match self {
            Precise::Path { .. } => true,
            _ => self.local_path(config).is_dir(),
        }
    }

    /// If this is a path package with a relative path, turn it into an abolute path, relative to `root`.
    pub fn with_abs_path(self, root: &std::path::Path) -> Self {
        match self {
            Precise::Path { path } => Precise::Path {
                path: normalize_abs_path(&root.join(path)),
            },
            x => x,
        }
    }

    pub fn version(&self) -> Option<SemVer> {
        match self {
            Precise::Index { version, .. } => Some(version.clone()),
            _ => None,
        }
    }
}

/// The path in our local filesystem where we store the git repo with the given id.
fn repo_root(config: &Config, id: &ObjectId) -> PathBuf {
    config.git_package_dir.join(id.to_string())
}
