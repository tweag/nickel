// c&p from old file.

use std::{
    collections::{BTreeMap, HashMap},
    path::Path,
};

use nickel_lang_core::{cache::normalize_path, identifier::Ident, package::PackageMap};
use serde::{Deserialize, Serialize};

use crate::{
    config::Config,
    error::{Error, IoResultExt},
    resolve::Resolution,
    ManifestFile, Precise,
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
    pub dependencies: BTreeMap<String, Precise>,
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
    pub fn new(manifest: &ManifestFile, resolution: &Resolution) -> Result<Self, Error> {
        // We don't put all packages in the lock file: we ignore dependencies (and therefore also
        // transitive dependencies) of path deps. In order to figure out what to include, we
        // traverse the depencency graph.
        fn collect_packages(
            res: &Resolution,
            pkg: &Precise,
            acc: &mut HashMap<Precise, LockFileEntry>,
        ) -> Result<(), Error> {
            // let entry = LockFileEntry {
            //     dependencies: if pkg.is_path() {
            //         // Skip dependencies of path deps
            //         Default::default()
            //     } else {
            //         res.dependencies(pkg)
            //     },
            // };

            // Let's try out what happens if we include path deps and their
            // dependencies in the lock file. This makes the lock file
            // potentially non-portable to different systems, but on the other
            // hand it allows the package map to be read straight from the lock
            // file. This is probably the way to go if we require manual lock
            // file refreshing.
            let entry = LockFileEntry {
                dependencies: res
                    .dependencies(pkg)?
                    .into_iter()
                    .map(|(id, entry)| (id.label().to_owned(), entry))
                    .collect(),
            };

            // Only recurse if this is the first time we've encountered this precise package.
            if acc.insert(pkg.clone(), entry).is_none() {
                for (_, dep) in acc[pkg].clone().dependencies {
                    collect_packages(res, &dep, acc)?;
                }
            }
            Ok(())
        }

        let mut acc = HashMap::new();
        for dep in manifest.dependencies.values() {
            collect_packages(resolution, &resolution.precise(dep), &mut acc)?;
        }

        Ok(LockFile {
            dependencies: manifest
                .dependencies
                .iter()
                .map(|(name, dep)| (name.label().to_owned(), resolution.precise(dep)))
                .collect(),

            packages: acc,
        })
    }

    // TODO: propagate the error
    pub fn from_path(path: impl AsRef<Path>) -> Self {
        let contents = std::fs::read_to_string(path.as_ref()).unwrap();
        serde_json::from_str(&contents).unwrap()
    }

    /// Build a package map from a lock-file.
    ///
    /// This only works if the lock-file contains path dependencies and their
    /// recursive dependencies. See [`LockFile`].
    ///
    /// `manifest_dir` is the directory containing the manifest file. Relative
    /// path dependencies in the lock-file will be interpreted relative to the
    /// manifest directory and turned into absolute paths.
    pub fn package_map(&self, manifest_dir: &Path, config: &Config) -> Result<PackageMap, Error> {
        let manifest_dir = normalize_path(manifest_dir).without_path()?;

        let path = |pkg: &Precise| pkg.clone().with_abs_path(&manifest_dir).local_path(config);

        Ok(PackageMap {
            top_level: self
                .dependencies
                .iter()
                .map(|(id, pkg)| (Ident::new(id), path(pkg)))
                .collect(),
            packages: self
                .packages
                .iter()
                .flat_map(|(pkg, entry)| {
                    entry
                        .dependencies
                        .iter()
                        .map(|(id, dep)| ((path(pkg), Ident::new(id)), path(dep)))
                })
                .collect(),
        })
    }
}

/// The dependencies of a single package.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, PartialOrd, Eq, Ord)]
pub struct LockFileEntry {
    pub dependencies: BTreeMap<String, Precise>,
}
