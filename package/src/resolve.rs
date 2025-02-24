use std::collections::HashMap;

use nickel_lang_core::{cache::normalize_path, identifier::Ident, package::PackageMap};

use crate::{
    config::Config,
    error::{Error, IoResultExt as _},
    index::{self, PackageIndex, Shared},
    lock::LockFile,
    snapshot::Snapshot,
    version::{SemVer, VersionReq},
    Dependency, IndexDependency, ManifestFile, PrecisePkg,
};

#[derive(Debug)]
pub struct Resolution {
    pub config: Config,
    pub snapshot: Snapshot,
    pub index: PackageIndex<Shared>,
    /// All the index packages in the dependency tree.
    ///
    /// Each package id can resolve to multiple versions, but those versions should all fall
    /// into disjoint semantic-version buckets.
    ///
    /// TODO: this disjointness constraint is not yet implemented, but once we support actual
    /// version resolution then it will be
    pub index_packages: HashMap<index::Id, Vec<SemVer>>,
}

pub fn resolve(
    manifest: &ManifestFile,
    snapshot: Snapshot,
    index: PackageIndex<Shared>,
    config: Config,
) -> Result<Resolution, Error> {
    resolve_with_lock(manifest, &LockFile::default(), snapshot, index, config)
}

pub fn resolve_with_lock(
    _manifest: &ManifestFile,
    // We don't look at the lock-file yet because we only support exact index deps anyway.
    _lock: &LockFile,
    snapshot: Snapshot,
    index: PackageIndex<Shared>,
    config: Config,
) -> Result<Resolution, Error> {
    let mut index_packages: HashMap<index::Id, Vec<SemVer>> = HashMap::new();
    let all_index_deps = snapshot.all_manifests().flat_map(|manifest| {
        manifest.dependencies.values().filter_map(|dep| match dep {
            Dependency::Index(index_dependency) => Some(index_dependency),
            Dependency::Git(_) | Dependency::Path(_) => None,
        })
    });
    for index_dep in all_index_deps {
        let version = match &index_dep.version {
            crate::version::VersionReq::Compatible(_) => {
                // Only exact versions are allowed for now (and this is enforced by the manifest loader)
                unreachable!()
            }
            crate::version::VersionReq::Exact(sem_ver) => sem_ver.clone(),
        };
        index_packages
            .entry(index_dep.id.clone())
            .or_default()
            .push(version);
    }
    for list in index_packages.values_mut() {
        list.sort();
        list.dedup();
    }

    Ok(Resolution {
        config,
        snapshot,
        index,
        index_packages,
    })
}

impl Resolution {
    /// Finds the precise resolved version of this dependency.
    ///
    /// # Panics
    ///
    /// Panics if the dependency was not part of the dependency tree that this resolution
    /// was generated for.
    pub fn precise(&self, dep: &Dependency) -> PrecisePkg {
        match dep {
            Dependency::Git(git) => PrecisePkg::Git {
                url: git.url.clone(),
                id: self.snapshot.git[git],
                path: git.path.clone(),
            },
            Dependency::Path(path) => PrecisePkg::Path {
                path: path.to_owned(),
            },
            Dependency::Index(idx) => {
                let version = match &idx.version {
                    crate::version::VersionReq::Compatible(_) => {
                        // Only exact versions are allowed for now (and this is enforced by the manifest loader)
                        unreachable!()
                    }
                    crate::version::VersionReq::Exact(sem_ver) => sem_ver.clone(),
                };
                PrecisePkg::Index {
                    id: idx.id.clone(),
                    version,
                }
            }
        }
    }

    /// Returns the dependencies of a package.
    ///
    /// # Panics
    ///
    /// Panics if the package was not part of the dependency tree that this resolution
    /// was generated for.
    pub fn sorted_dependencies(&self, pkg: &PrecisePkg) -> Vec<(Ident, (Dependency, PrecisePkg))> {
        match pkg {
            PrecisePkg::Git { .. } | PrecisePkg::Path { .. } => {
                self.snapshot.sorted_dependencies(pkg)
            }
            PrecisePkg::Index { id, version } => {
                // FIXME: unwraps
                let pkg = self.index.package(id, version.clone()).unwrap().unwrap();
                let mut ret: Vec<_> = pkg
                    .dependencies
                    .iter()
                    .map(|(id, index_dep)| {
                        let semver = match &index_dep.version {
                            VersionReq::Exact(semver) => semver.clone(),
                            VersionReq::Compatible(_) => unreachable!(),
                        };
                        (
                            *id,
                            (
                                Dependency::Index(index_dep.clone()),
                                PrecisePkg::Index {
                                    id: index_dep.id.clone(),
                                    version: semver,
                                },
                            ),
                        )
                    })
                    .collect();
                ret.sort_by(|(name0, _), (name1, _)| name0.label().cmp(name1.label()));
                ret
            }
        }
    }

    /// Returns a package map containing the entire dependency tree.
    ///
    /// Once index packages are supported, this will need to move: the entire dependency
    /// tree will involve both snapshotted packages and resolved index packages. We only
    /// know about the first kind.
    pub fn package_map(&self, manifest: &ManifestFile) -> Result<PackageMap, Error> {
        let parent_dir = manifest.parent_dir.clone();
        let manifest_dir = normalize_path(&parent_dir).with_path(&parent_dir)?;
        let config = &self.config;

        let mut all: Vec<PrecisePkg> = self.snapshot.all_packages().cloned().collect();
        all.extend(self.index_packages.iter().flat_map(|(id, versions)| {
            versions.iter().map(|v| PrecisePkg::Index {
                id: id.clone(),
                version: v.clone(),
            })
        }));
        all.sort();
        all.dedup();

        let mut packages = HashMap::new();
        for p in &all {
            let p_path = p.clone().with_abs_path(&manifest_dir).local_path(config);
            let root_path = &manifest_dir;
            for (dep_id, (_, dep_precise)) in self.sorted_dependencies(p) {
                packages.insert(
                    (p_path.clone(), dep_id),
                    dep_precise.with_abs_path(root_path).local_path(config),
                );
            }
        }

        Ok(PackageMap {
            // Copy over dependencies of the root, making paths absolute.
            top_level: manifest
                .dependencies
                .iter()
                .map(|(name, source)| {
                    (
                        *name,
                        self.precise(source)
                            .with_abs_path(&manifest_dir)
                            .local_path(config),
                    )
                })
                .collect(),

            packages,
        })
    }

    /// Returns all the dependencies of a package, along with their package-local names.
    pub fn dependencies(&self, pkg: &PrecisePkg) -> Result<HashMap<Ident, PrecisePkg>, Error> {
        let ret = match pkg {
            PrecisePkg::Path { .. } | PrecisePkg::Git { .. } => {
                let manifest = self.snapshot.manifest(pkg);
                manifest
                    .dependencies
                    .iter()
                    .map(move |(dep_name, dep)| {
                        let pkg = match dep.clone().as_unversioned() {
                            Some(dep) => self.snapshot.dependency(pkg, &dep).clone(),
                            None => {
                                // Since the realization contains all the unversioned deps, if we didn't
                                // find our dep then it must be an index dep.
                                self.precise(dep)
                            }
                        };
                        (*dep_name, pkg)
                    })
                    .collect()
            }
            PrecisePkg::Index { id, version } => {
                let index_pkg = self.index.package(id, version.clone())?.unwrap();
                index_pkg
                    .dependencies
                    .into_iter()
                    .map(move |(dep_name, dep)| {
                        let precise_dep = self.precise(&Dependency::Index(IndexDependency {
                            id: dep.id.clone(),
                            version: dep.version.clone(),
                        }));
                        (dep_name, precise_dep)
                    })
                    .collect()
            }
        };
        Ok(ret)
    }

    /// Returns all the resolved packages in the dependency tree.
    pub fn all_packages(&self) -> Vec<PrecisePkg> {
        let mut ret: Vec<_> = self.snapshot.all_packages().cloned().collect();
        ret.extend(self.index_packages.iter().flat_map(|(id, vs)| {
            vs.iter().map(|v| PrecisePkg::Index {
                id: id.clone(),
                version: v.clone(),
            })
        }));
        ret.sort();
        ret.dedup();
        ret
    }
}
