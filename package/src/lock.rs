// c&p from old file.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use directories::ProjectDirs;
use nickel_lang_core::{
    cache::{normalize_abs_path, normalize_path},
    package::{Name, ObjectId, PackageMap},
};
use serde::{Deserialize, Serialize};

use crate::{manifest::Spec, PackageSource};

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
            LockedPackageSource::Git { tree, path, .. } => {
                let cache_dir = cache_dir();
                cache_dir.join(tree.to_string()).join(path)
            }
            LockedPackageSource::Path { path } => Path::new(path).to_owned(),
        }
    }

    pub fn repo_root(&self) -> Option<PathBuf> {
        match self {
            LockedPackageSource::Git { tree, .. } => {
                let cache_dir = cache_dir();
                Some(cache_dir.join(tree.to_string()))
            }
            LockedPackageSource::Path { .. } => None,
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

    pub fn with_abs_path(self, root: &std::path::Path) -> Self {
        match self {
            x @ LockedPackageSource::Git { .. } => x,
            LockedPackageSource::Path { path } => LockedPackageSource::Path {
                path: normalize_abs_path(&root.join(path)),
            },
        }
    }

    pub fn with_normalized_abs_path(self) -> Self {
        match self {
            x @ LockedPackageSource::Git { .. } => x,
            LockedPackageSource::Path { path } => LockedPackageSource::Path {
                path: normalize_abs_path(&path),
            },
        }
    }
}

mod package_list {
    use std::collections::HashMap;

    use serde::{Deserializer, Serializer};

    use super::*;

    #[derive(Serialize, Deserialize)]
    struct Entry {
        source: LockedPackageSource,
        #[serde(flatten)]
        entry: LockFileEntry,
    }

    pub fn serialize<S: Serializer>(
        h: &HashMap<LockedPackageSource, LockFileEntry>,
        ser: S,
    ) -> Result<S::Ok, S::Error> {
        let entries: Vec<_> = h
            .iter()
            .map(|(source, entry)| Entry {
                source: source.clone(),
                entry: entry.clone(),
            })
            .collect();
        entries.serialize(ser)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(
        de: D,
    ) -> Result<HashMap<LockedPackageSource, LockFileEntry>, D::Error> {
        let entries = Vec::<Entry>::deserialize(de)?;
        Ok(entries.into_iter().map(|e| (e.source, e.entry)).collect())
    }
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
    #[serde(with = "package_list")]
    pub packages: HashMap<LockedPackageSource, LockFileEntry>,
}

impl LockFile {
    pub fn resolve_package_map(&self, root_path: PathBuf) -> Result<PackageMap, crate::Error> {
        // The lock file knows about recursive git dependencies, and path dependencies of the root.
        // There are no path dependencies coming recursively from git dependencies because those have
        // been re-written to git dependencies; and there are no path dependencies of path dependencies
        // because those haven't been expanded yet.

        let root_path = normalize_path(root_path)?;

        let mut ret = PackageMap {
            // Make all path dependencies of the root absolute.
            top_level: self
                .dependencies
                .iter()
                .map(|(name, source)| {
                    (
                        name.clone(),
                        source.clone().with_abs_path(&root_path).local_path(),
                    )
                })
                .collect(),
            // Pass through the (possibly recursive) git dependencies unchanged.
            packages: self
                .packages
                .iter()
                .filter(|&(source, _)| !source.is_path())
                .flat_map(|(source, entry)| {
                    entry.dependencies.iter().map(|(dep_name, dep_source)| {
                        (
                            (source.local_path(), dep_name.clone()),
                            dep_source.local_path(),
                        )
                    })
                })
                .collect(),
        };

        // Expand (and make absolute) all path deps.
        let path_deps = self.dependencies.iter().filter_map(|(name, s)| {
            if let LockedPackageSource::Path { path } = s.clone().with_abs_path(&root_path) {
                Some((name, path))
            } else {
                None
            }
        });

        let root = LockedPackageSource::Path {
            path: root_path.clone(),
        };
        for (name, path) in path_deps {
            let spec = Spec {
                name: name.clone(),
                source: PackageSource::Path { path: path.clone() },
            };
            let locked = spec.realize_rec(Some(&root))?;
            locked.flatten_into_map(&mut ret);
        }

        Ok(ret)
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
