//! Lock files and lock file utilities

use std::{
    collections::{BTreeMap, HashMap},
    path::Path,
};

use nickel_lang_core::{cache::normalize_path, identifier::Ident, package::PackageMap};
use serde::{Deserialize, Serialize};

use crate::{
    error::{Error, IoResultExt},
    realization::Realization,
    Dependency, GitDependency, ManifestFile, Precise,
};

mod package_list {
    use std::collections::HashMap;

    use serde::{Deserializer, Serializer};

    use super::*;

    #[derive(Serialize, Deserialize, PartialEq, PartialOrd, Eq, Ord)]
    struct Entry {
        source: Precise,
        #[serde(flatten)]
        entry: LockFileEntry,
    }

    pub fn serialize<S: Serializer>(
        h: &HashMap<Precise, LockFileEntry>,
        ser: S,
    ) -> Result<S::Ok, S::Error> {
        let mut entries: Vec<_> = h
            .iter()
            .map(|(source, entry)| Entry {
                source: source.clone(),
                entry: entry.clone(),
            })
            .collect();
        entries.sort();
        entries.serialize(ser)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(
        de: D,
    ) -> Result<HashMap<Precise, LockFileEntry>, D::Error> {
        let entries = Vec::<Entry>::deserialize(de)?;
        Ok(entries.into_iter().map(|e| (e.source, e.entry)).collect())
    }
}

/// A lock file, specifying versions and names for all recursive dependencies.
///
/// This defines the on-disk format for lock files.
///
/// # Open question
///
/// There's one big open question about the lock file: should it contain information
/// about path dependencies (and their recursive dependencies)? If it does, you
/// can immediately derive the `PackageMap` from the lock file, meaning that if the
/// interpreter gets the lock file then it can do everything else from there,
/// without doing any package resolution. So that's nice.
///
/// The problem with putting information about path dependencies in the lock file is
/// that path dependencies can change without notice, making the lock file stale.
/// So the interpreter didn't have to do much work, but it ended up running on old
/// information.
///
/// I think the decision here basically comes down to what we want from the CLI
/// interface. If we require a separate update-the-lock-file step (a la npm or poetry),
/// it makes sense to put the path dependency info here. But if we want an
/// auto-refresh step (a la cargo), we want to leave it out. Current strategy is
/// to keep it in, and we'll measure the performance of package resolution before
/// making a final decision.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct LockFile {
    /// The dependencies of the current (top-level) package.
    ///
    /// These should be sorted so that the serialization doesn't change all the time.
    pub dependencies: BTreeMap<String, LockFileDep>,
    /// All packages that we know about, and the dependencies of each one.
    ///
    /// Note that the package list is not guaranteed to be closed: path dependencies
    /// cannot have their dependencies resolved in the on-disk lockfile because they
    /// can change at any time. *Some* path dependencies (for example, path dependencies
    /// that are local to a git depencency repo) may have resolved dependencies.
    #[serde(with = "package_list")]
    pub packages: HashMap<Precise, LockFileEntry>,
}

impl LockFile {
    pub fn new(manifest: &ManifestFile, realization: &Realization) -> Result<Self, Error> {
        // We don't put all packages in the lock file: we ignore dependencies (and therefore also
        // transitive dependencies) of path deps. In order to figure out what to include, we
        // traverse the depencency graph.
        fn collect_packages(
            realization: &Realization,
            pkg: &Precise,
            acc: &mut HashMap<Precise, LockFileEntry>,
        ) -> Result<(), Error> {
            // We exclude path dependencies (and their recursive dependencies)
            // from the lock file. This ensures that the lock file is portable
            // to different systems, but on the other hand it means that the
            // package map cannot be derived straight from the lock file: we
            // always need to read the manifest and look for path dependencies.
            //
            // To clarify the trade-off a little bit, we have a choice between
            // user-refreshed lock files or automatically-refreshed lock files.
            // In the former case, the user runs `nickel package generate-lockfile`
            // and that traverses the entire dependency tree and generates an
            // up-to-date lock file. When the user then runs `nickel eval`, we
            // trust that the lock file is up to date. This only works if the
            // lock file contains everything we need to know, including all
            // about the recursive dependencies of path dependencies.
            //
            // In the automatically-refreshed version, we check on every `nickel
            // eval` whether the lock file needs to be updated. This check is
            // cheap if there are no path dependencies: we read the top-level
            // manifest and the stored lock file and check whether they're
            // compatible. If they are, we're done: we don't need to look at
            // any of the dependencies' manifests because we know they haven't
            // changed. But if there are path dependencies, we don't know if
            // their manifests have changed, so in the automatic-refresh version
            // we need to read and evaluate them all. In this mode, we need to
            // read all path dependencies' manifests files on `nickel eval`, even
            // if they're in the lock file. So we may as well make the lock
            // files portable by leaving out path dependencies.

            let entry = LockFileEntry {
                dependencies: if pkg.is_path() {
                    // Skip dependencies of path deps
                    Default::default()
                } else {
                    realization
                        .dependencies(pkg)
                        .into_iter()
                        .map(|(id, (dep, precise))| {
                            let spec = match dep {
                                Dependency::Git(g) => Some(g.clone()),
                                Dependency::Path { .. } => None,
                            };
                            let entry = LockFileDep {
                                precise: precise.clone(),
                                spec,
                            };
                            (id.label().to_owned(), entry)
                        })
                        .collect()
                },
            };

            // The following commented-out code is what we want if we decide
            // to include path deps in the lock file.
            //
            // let entry = LockFileEntry {
            //     dependencies: realization
            //         .dependencies(pkg)
            //         .into_iter()
            //         .map(|(id, entry)| (id.label().to_owned(), entry))
            //         .collect(),
            // };

            // Only recurse if this is the first time we've encountered this precise package.
            if acc.insert(pkg.clone(), entry).is_none() && !pkg.is_path() {
                for (_id, (_dep, precise)) in realization.dependencies(pkg) {
                    collect_packages(realization, &precise, acc)?;
                }
            }
            Ok(())
        }

        let mut acc = HashMap::new();
        for dep in manifest.dependencies.values() {
            collect_packages(realization, &realization.precise(dep), &mut acc)?;
        }

        Ok(LockFile {
            dependencies: manifest
                .dependencies
                .iter()
                .map(|(name, dep)| {
                    let spec = match dep {
                        Dependency::Git(g) => Some(g.clone()),
                        Dependency::Path { .. } => None,
                    };
                    let entry = LockFileDep {
                        precise: realization.precise(dep),
                        spec,
                    };
                    (name.label().to_owned(), entry)
                })
                .collect(),

            packages: acc,
        })
    }

    /// Read a lock file from disk.
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, Error> {
        let path = path.as_ref();
        let contents = std::fs::read_to_string(path).with_path(path)?;
        serde_json::from_str(&contents).map_err(|error| Error::LockFileDeserialization {
            path: path.to_owned(),
            error,
        })
    }

    /// Build a package map from a lock file.
    ///
    /// `manifest_dir` is the directory containing the manifest file. Relative
    /// path dependencies in the lock file will be interpreted relative to the
    /// manifest directory and turned into absolute paths.
    pub fn package_map(
        &self,
        manifest_dir: &Path,
        realization: &Realization,
    ) -> Result<PackageMap, Error> {
        let config = &realization.config;
        let manifest_dir = normalize_path(manifest_dir).without_path()?;

        let path = |pkg: &Precise| pkg.clone().with_abs_path(&manifest_dir).local_path(config);

        Ok(PackageMap {
            top_level: self
                .dependencies
                .iter()
                .map(|(id, entry)| (Ident::new(id), path(&entry.precise)))
                .collect(),
            packages: self
                .packages
                .iter()
                .flat_map(|(pkg, entry)| {
                    entry
                        .dependencies
                        .iter()
                        .map(|(id, dep)| ((path(pkg), Ident::new(id)), path(&dep.precise)))
                })
                .collect(),
        })
    }

    /// Write out this lock file to the filesystem.
    pub fn write(&self, path: &Path) -> Result<(), Error> {
        // unwrap: serde_json serialization fails if the derived `Serialize`
        // trait fails (which it shouldn't), or if there's a map with
        // non-string keys (all our maps have `Ident` keys).
        let serialized_lock = serde_json::to_string_pretty(self).unwrap();
        std::fs::write(path, serialized_lock).with_path(path)?;
        Ok(())
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct LockFileDep {
    pub precise: Precise,

    /// For git packages, we store their original git spec in the lock-file, so
    /// that if someone changes the spec in the manifest we can tell that we
    /// need to re-fetch the repo.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub spec: Option<GitDependency>,
}

/// The dependencies of a single package.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct LockFileEntry {
    dependencies: BTreeMap<String, LockFileDep>,
}
