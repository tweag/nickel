//! This module contains the part of package management that's needed by the evaluator.
//!
//! In particular, it doesn't contain any support for version resolution or
//! dependency fetching, but defines lockfiles and allows them to be used to
//! resolve package dependencies to paths.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    str::FromStr as _,
};

use directories::ProjectDirs;
use serde::{Deserialize, Serialize};

use crate::{error::ImportError, position::TermPos};

const ID_LEN: usize = 20;

/// A git object id.
///
/// Git uses 160-bit hashes as object ids. To avoid pulling in the full git dependency, we define our
/// own id type.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ObjectId([u8; ID_LEN]);

// Git object ids are typically displayed in base16.
impl std::fmt::Display for ObjectId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&base16::encode_lower(&self.0))
    }
}

#[derive(Clone, Debug)]
pub enum ParseObjectError {
    Length(usize),
    Base16(base16::DecodeError),
}

impl std::fmt::Display for ParseObjectError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseObjectError::Length(x) => {
                write!(f, "invalid object id length: got {x}, expected 40")
            }
            ParseObjectError::Base16(e) => write!(f, "invalid base16 for object id: {e}"),
        }
    }
}

impl std::error::Error for ParseObjectError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ParseObjectError::Length(_) => None,
            ParseObjectError::Base16(e) => Some(e),
        }
    }
}

impl std::str::FromStr for ObjectId {
    type Err = ParseObjectError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Since it's base16, the string should be twice as long as the raw data.
        if s.len() == ID_LEN * 2 {
            let mut ret = ObjectId([0; ID_LEN]);
            base16::decode_slice(s, &mut ret.0)
                .map(move |_| ret)
                .map_err(ParseObjectError::Base16)
        } else {
            Err(ParseObjectError::Length(s.len()))
        }
    }
}

impl serde::Serialize for ObjectId {
    fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        self.to_string().serialize(ser)
    }
}

impl<'de> serde::Deserialize<'de> for ObjectId {
    fn deserialize<D: serde::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
        <&str>::deserialize(de)
            .and_then(|s| ObjectId::from_str(s).map_err(<D::Error as serde::de::Error>::custom))
    }
}

/// Packages are identified by an organization and a package name.
///
/// Both the organization and the package name are required to be valid nickel identifiers. They
/// are typically displayed separated by a slash, as in `example/package`. (Note that a slash
/// is not a valid character in an identifier.)
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(try_from = "&str", into = "String")]
pub struct Name {
    pub org: String,
    pub package: String,
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.org, self.package)
    }
}

impl std::str::FromStr for Name {
    type Err = String; // FIXME

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (org, package) = s.split_once('/').ok_or(String::new())?;
        if package.contains('/') {
            return Err(String::new());
        }

        Ok(Name {
            org: org.to_owned(),
            package: package.to_owned(),
        })
    }
}

impl TryFrom<&str> for Name {
    type Error = <Name as std::str::FromStr>::Err;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        s.parse()
    }
}

impl From<Name> for String {
    fn from(n: Name) -> Self {
        n.to_string()
    }
}

/// A locked package source uniquely identifies the source of the package (with a specific version).
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum LockedPackageSource {
    Git {
        repo: String,
        tree: ObjectId,
        /// Path of the package relative to the git repo root.
        #[serde(default)]
        path: PathBuf,
    },
    Path {
        path: PathBuf,
    },
}

impl LockedPackageSource {
    /// Where on the local filesystem can this package be found?
    ///
    /// Note: it might not actually be there yet, if it's a git package that hasn't been fetched.
    pub fn local_path(&self) -> PathBuf {
        match self {
            // Question: is it ok to only use the tree id? Should we worry
            // about collisions between packages?
            LockedPackageSource::Git { tree, .. } => {
                let cache_dir = cache_dir();
                cache_dir.join(tree.to_string())
            }
            LockedPackageSource::Path { path } => Path::new(path).to_owned(),
        }
    }

    pub fn is_path(&self) -> bool {
        matches!(self, LockedPackageSource::Path { .. })
    }

    /// Is this locked package available offline? If not, it needs to be fetched.
    pub fn is_available_offline(&self) -> bool {
        // We consider path-dependencies to be always available offline, even if they don't exist.
        // We consider git-dependencies to be available offline if there's a directory at
        // `~/.cache/nickel/ed8234.../` (or wherever the cache directory is on your system). We
        // don't check if that directory contains the right git repository -- if someone has messed
        // with the contents of `~/.cache/nickel`, that's your problem.
        match self {
            LockedPackageSource::Path { .. } => true,
            LockedPackageSource::Git { .. } => self.local_path().is_dir(),
        }
    }
}

/// A lock file that's been fully resolved, including path dependencies.
#[derive(Clone, Debug, Default)]
pub struct ResolvedLockFile {
    /// Absolute path to the lock file's parent directory.
    ///
    /// Path dependencies at the top-level are resolved relative to this.
    pub path: PathBuf,
    /// The inner lockfile, which is now guaranteed to have closed dependencies.
    pub inner: LockFile,
}

/// A lock file, specifying versions and names for all recursive dependencies.
///
/// This defines the on-disk format for lock files.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct LockFile {
    /// The dependencies of the current (top-level) package.
    pub dependencies: HashMap<Name, LockedPackageSource>,
    /// All packages that we know about, and the dependencies of each one.
    ///
    /// Note that the package list is not guaranteed to be closed: path dependencies
    /// cannot have their dependencies resolved in the on-disk lockfile because they
    /// can change at any time. *Some* path dependencies (for example, path dependencies
    /// that are local to a git depencency repo) may have resolved dependencies.
    pub packages: HashMap<LockedPackageSource, LockFileEntry>,
}

impl ResolvedLockFile {
    /// Are all the packages mentioned in this lockfile available offline?
    pub fn is_available_offline(&self) -> bool {
        self.inner
            .packages
            .keys()
            .all(LockedPackageSource::is_available_offline)
    }

    pub fn get(
        &self,
        parent: Option<&LockedPackageSource>,
        pkg: &Name,
        pos: &TermPos,
    ) -> Result<&LockedPackageSource, ImportError> {
        // The parent package should have come from the lock file.
        let (deps, parent_name) = if let Some(parent) = parent {
            let parent_pkg =
                self.inner
                    .packages
                    .get(parent)
                    .ok_or_else(|| ImportError::InternalError {
                        msg: format!("unknown parent package {parent:?}"),
                        pos: *pos,
                    })?;

            (&parent_pkg.dependencies, Some(&parent_pkg.name))
        } else {
            (&self.inner.dependencies, None)
        };
        deps.get(pkg).ok_or_else(|| ImportError::MissingDependency {
            parent: parent
                .zip(parent_name)
                .map(|(p, n)| Box::new((n.clone(), p.clone()))),
            missing: pkg.clone(),
            pos: *pos,
        })
    }
}

/// The dependencies of a single package.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LockFileEntry {
    /// The human-readable name of this package.
    ///
    /// This is used for error messages, but is not otherwise useful for identifying a package. For example, it is
    /// not necessarily unique.
    pub name: Name,
    pub dependencies: HashMap<Name, LockedPackageSource>,
}

fn cache_dir() -> PathBuf {
    let dir = ProjectDirs::from("org", "nickel-lang", "nickel").unwrap();
    dir.cache_dir().to_owned()
}
