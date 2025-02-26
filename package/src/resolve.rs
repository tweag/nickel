use std::collections::HashMap;

use nickel_lang_core::{cache::normalize_path, identifier::Ident, package::PackageMap};

use crate::{
    config::Config,
    error::{Error, IoResultExt as _},
    index::{self, PackageIndex, Shared},
    lock::{LockFile, LockPrecisePkg},
    snapshot::Snapshot,
    version::{SemVer, VersionReq},
    Dependency, IndexDependency, ManifestFile, PrecisePkg,
};

/// Stores the result of resolving version constraints to exact versions.
#[derive(Debug)]
pub struct Resolution {
    pub config: Config,
    /// The snapshot (of path and git packages) that was used to construct
    /// this resolution. Note that path and git packages are not "resolved";
    /// they have fixed versions. The snapshot is only used to collect index
    /// dependencies of git and path packages.
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

/// Resolve a package's dependencies, from scratch.
pub fn resolve(
    manifest: &ManifestFile,
    snapshot: Snapshot,
    index: PackageIndex<Shared>,
    config: Config,
) -> Result<Resolution, Error> {
    resolve_with_lock(manifest, &LockFile::default(), snapshot, index, config)
}

/// Resolve a package's dependencies, giving preference to the versions that were previously
/// locked.
///
/// The only guarantee we give is that if the lock file already contains a complete and valid
/// dependency graph then it will be kept. If the lock file is incomplete, or any part of
/// it is invalid then we will try to preserve the valid parts but make no guarantees.
///
/// TODO: the above comment is hypothetical. For now we aren't doing any resolution.
pub fn resolve_with_lock(
    _manifest: &ManifestFile,
    // We don't look at the lock-file yet because we only support exact index deps anyway.
    _lock: &LockFile,
    snapshot: Snapshot,
    index: PackageIndex<Shared>,
    config: Config,
) -> Result<Resolution, Error> {
    let mut index_packages: HashMap<index::Id, Vec<SemVer>> = HashMap::new();
    for index_dep in snapshot.all_index_deps() {
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

/// Builds a resolution by just copying out all the versions of index
/// dependencies that we find in the lock file.
///
/// The resolution returned here is not guaranteed to be a *valid* resolution if
/// the lock file is out-of-date relative to the snapshot: there could be some
/// packages in the snapshot's dependency tree that aren't mentioned in the lock
/// file and will be missing from the resolution.
pub fn copy_from_lock(
    lock: &LockFile,
    snapshot: Snapshot,
    index: PackageIndex<Shared>,
    config: Config,
) -> Result<Resolution, Error> {
    let mut index_packages: HashMap<index::Id, Vec<SemVer>> = HashMap::new();

    for entry in lock.packages.values() {
        if let LockPrecisePkg::Index { id, version } = &entry.precise {
            index_packages
                .entry(id.clone())
                .or_default()
                .push(version.clone());
        }
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
                    // FIXME: validate that this is an allowed version
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
    pub fn sorted_dependencies(
        &self,
        pkg: &PrecisePkg,
    ) -> Result<Vec<(Ident, (Dependency, PrecisePkg))>, Error> {
        match pkg {
            PrecisePkg::Git { .. } | PrecisePkg::Path { .. } => {
                let mut deps = self.snapshot.sorted_unversioned_dependencies(pkg);
                let index_deps =
                    self.snapshot
                        .manifest(pkg)
                        .dependencies
                        .iter()
                        .filter_map(|(id, dep)| {
                            if matches!(dep, Dependency::Index(_)) {
                                Some((*id, (dep.clone(), self.precise(dep))))
                            } else {
                                None
                            }
                        });
                deps.extend(index_deps);
                deps.sort_by(|(name0, _), (name1, _)| name0.label().cmp(name1.label()));
                deps.dedup();
                Ok(deps)
            }
            PrecisePkg::Index { id, version } => {
                let pkg = self.index.package(id, version.clone())?;
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
                Ok(ret)
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
            for (dep_id, (_, dep_precise)) in self.sorted_dependencies(p)? {
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
                let index_pkg = self.index.package(id, version.clone())?;
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
