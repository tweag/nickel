use std::{collections::HashMap, path::PathBuf};

use gix::ObjectId;
use nickel_lang_core::{cache::normalize_path, identifier::Ident, package::PackageMap};
use pubgrub::DependencyProvider;

use crate::{
    config::Config,
    error::{Error, IoResultExt as _},
    index::{self, PackageIndex, Shared},
    lock::{LockFile, LockPrecisePkg},
    snapshot::Snapshot,
    version::{SemVer, VersionReq},
    Dependency, IndexDependency, ManifestFile, PrecisePkg,
};

pub struct PackageRegistry {
    // The packages whose versions were locked in a lockfile; we'll try to prefer using
    // those same versions. We won't absolutely insist on it, because if the manifest
    // changed (or some path-dependency changed) then the old locked versions might not
    // resolve anymore.
    previously_locked: HashMap<Package, SemVer>,
    index: PackageIndex<Shared>,
    snapshot: Snapshot,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Package {
    Git {
        url: gix::Url,
        id: ObjectId,
        path: PathBuf,
    },
    Path {
        path: PathBuf,
    },
    Index(Bucket),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Bucket {
    pub id: index::Id,
    pub version: BucketVersion,
}

/// A bucket version represents a collection of compatible semver versions.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum BucketVersion {
    /// A collection of versions all having the same major version number.
    /// (For example, 1.x.y)
    Major(u64),
    /// A collection of versions all having major version zero, and the same minor version number.
    /// (For example, 0.2.x)
    Minor(u64),
    /// An exact prerelease version.
    ///
    /// The `pre` field of the version must be non-empty.
    Prerelease(SemVer),
}

impl From<VersionReq> for BucketVersion {
    fn from(req: VersionReq) -> Self {
        match req {
            VersionReq::Compatible(prefix) => {
                BucketVersion::major_minor(prefix.major, prefix.minor.unwrap_or_default())
            }
            VersionReq::Exact(v) => {
                if v.pre.is_empty() {
                    BucketVersion::major_minor(v.major, v.minor)
                } else {
                    BucketVersion::Prerelease(v.clone())
                }
            }
        }
    }
}

impl BucketVersion {
    pub fn major_minor(major: u64, minor: u64) -> Self {
        if major == 0 {
            BucketVersion::Minor(minor)
        } else {
            BucketVersion::Major(major)
        }
    }

    pub fn contains(&self, semver: &SemVer) -> bool {
        match self {
            BucketVersion::Major(v) => *v == semver.major && semver.pre.is_empty(),
            BucketVersion::Minor(v) => {
                semver.major == 0 && semver.minor == *v && semver.pre.is_empty()
            }
            BucketVersion::Prerelease(v) => v == semver,
        }
    }
}

impl std::fmt::Display for BucketVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BucketVersion::Major(v) => write!(f, "{v}"),
            BucketVersion::Minor(v) => write!(f, "0.{v}"),
            BucketVersion::Prerelease(v) => write!(f, "{v}"),
        }
    }
}

impl std::fmt::Display for Package {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Package::Git { url, id, path } => {
                // TODO: this is not a very useful display. Maybe we should include a url even
                // though it isn't used for resolution
                write!(f, "{url}@{id}/{}", path.display())
            }
            Package::Path { path } => {
                write!(f, "{}", path.display())
            }
            Package::Index(b) => {
                write!(f, "{}@{}", b.id, b.version)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Version {
    None,
    SemVer(SemVer),
}

impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Version::SemVer(v) = self {
            v.fmt(f)
        } else {
            Ok(())
        }
    }
}

/// Pubgrub's advice for package resolution priority heuristics is
/// that we should first resolve (i.e. assign highest priority to)
///
/// - packages whose version is already known, and
/// - packages that have lots of conflicts.
#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct Priority {
    pub single_version: bool,
    pub conflicts: u32,
}

impl PackageRegistry {
    /// Read git or path dependencies from the snapshot.
    ///
    /// `pkg` must not be an index package.
    ///
    /// # Panics
    ///
    /// Panics if `pkg` was not part of the snapshot.
    fn snapshot_dependencies(
        &self,
        pkg: &PrecisePkg,
    ) -> pubgrub::Map<Package, pubgrub::Ranges<Version>> {
        let index_deps = self
            .snapshot
            .index_deps(pkg)
            .map(index_dep_package_and_range);
        let other_deps = self
            .snapshot
            .sorted_unversioned_dependencies(pkg)
            .into_iter()
            .map(|(_name, _dep, pkg)| {
                let p = match pkg {
                    PrecisePkg::Git { url, id, path } => Package::Git { url, id, path },
                    PrecisePkg::Path { path } => Package::Path { path },
                    // TODO: maybe re-introduce UnversionedPrecisePkg
                    PrecisePkg::Index { .. } => unreachable!(),
                };
                (p, pubgrub::Ranges::full())
            });

        index_deps.chain(other_deps).collect()
    }
}

impl DependencyProvider for PackageRegistry {
    type P = Package;
    type V = Version;
    type VS = pubgrub::Ranges<Version>;
    type Priority = Priority;
    type M = String;
    type Err = crate::Error;

    fn prioritize(
        &self,
        package: &Self::P,
        _range: &Self::VS,
        package_conflicts_counts: &pubgrub::PackageResolutionStatistics,
    ) -> Self::Priority {
        let single_version = match package {
            Package::Git { .. } | Package::Path { .. } => true,
            Package::Index(Bucket {
                version: BucketVersion::Prerelease(_),
                ..
            }) => true,
            // We could be more accurate here, by actually looking at `range` and
            // checking if it defines a single version.
            Package::Index(_) => false,
        };
        Priority {
            single_version,
            conflicts: package_conflicts_counts.conflict_count(),
        }
    }

    fn choose_version(
        &self,
        package: &Self::P,
        range: &Self::VS,
    ) -> Result<Option<Self::V>, Self::Err> {
        match package {
            Package::Git { .. } | Package::Path { .. } => Ok(Some(Version::None)),
            Package::Index(bucket) => {
                // TODO: check previously_locked
                if let BucketVersion::Prerelease(v) = &bucket.version {
                    if self.index.has_version(&bucket.id, v)? {
                        Ok(Some(Version::SemVer(v.clone())))
                    } else {
                        Ok(None)
                    }
                } else {
                    // `available_versions` are sorted in increasing order, so this will return
                    // the smallest version that's in the bucket and the constrained range.
                    let min_version = self.index.available_versions(&bucket.id)?.find(|v| {
                        bucket.version.contains(v) && range.contains(&Version::SemVer(v.clone()))
                    });
                    Ok(min_version.map(Version::SemVer))
                }
            }
        }
    }

    fn get_dependencies(
        &self,
        package: &Self::P,
        version: &Self::V,
    ) -> Result<pubgrub::Dependencies<Self::P, Self::VS, Self::M>, Self::Err> {
        let deps: pubgrub::Map<_, _> = match package {
            Package::Git { url, id, path } => self.snapshot_dependencies(&PrecisePkg::Git {
                url: url.clone(),
                id: *id,
                path: path.clone(),
            }),
            Package::Path { path } => {
                self.snapshot_dependencies(&PrecisePkg::Path { path: path.clone() })
            }
            Package::Index(bucket) => {
                let version = match version {
                    // should be unreachable if there are no bugs. TODO: add an error variant
                    Version::None => todo!(),
                    Version::SemVer(v) => v,
                };
                let index_package = self.index.package(&bucket.id, version)?;
                index_package
                    .dependencies
                    .values()
                    .map(index_dep_package_and_range)
                    .collect()
            }
        };

        Ok(pubgrub::Dependencies::Available(deps))
    }
}

fn index_dep_package_and_range(dep: &IndexDependency) -> (Package, pubgrub::Ranges<Version>) {
    let p = Package::Index(Bucket {
        id: dep.id.clone(),
        version: dep.version.clone().into(),
    });
    let lower_bound = match &dep.version {
        VersionReq::Compatible(v) => SemVer::new(
            v.major,
            v.minor.unwrap_or_default(),
            v.patch.unwrap_or_default(),
        ),
        VersionReq::Exact(req) => req.clone(),
    };
    let range = pubgrub::Ranges::higher_than(Version::SemVer(lower_bound));

    (p, range)
}

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
    /// Finds the resolved version of this index dependency.
    ///
    /// # Panics
    ///
    /// Panics if the dependency was not part of the dependency tree that this resolution
    /// was generated for.
    fn index_dep_version(&self, dep: &IndexDependency) -> &SemVer {
        // unwrap: we can assume `dep` was part of the resolved dependency tree
        self.index_packages
            .get(&dep.id)
            .unwrap()
            .iter()
            // We take the first matching version. Once version resolution is
            // done and we start checking for version conflicts, there will be
            // guaranteed to be only one.
            .find(|v| dep.version.matches(v))
            .unwrap()
    }

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
                let version = self.index_dep_version(idx).clone();
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
    ) -> Result<Vec<(Ident, Dependency, PrecisePkg)>, Error> {
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
                                Some((*id, dep.clone(), self.precise(dep)))
                            } else {
                                None
                            }
                        });
                deps.extend(index_deps);
                deps.sort_by(|(name0, _, _), (name1, _, _)| name0.label().cmp(name1.label()));
                deps.dedup();
                Ok(deps)
            }
            PrecisePkg::Index { id, version } => {
                let pkg = self.index.package(id, version)?;
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
                            Dependency::Index(index_dep.clone()),
                            PrecisePkg::Index {
                                id: index_dep.id.clone(),
                                version: semver,
                            },
                        )
                    })
                    .collect();
                ret.sort_by(|(name0, _, _), (name1, _, _)| name0.label().cmp(name1.label()));
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
            for (dep_id, _, dep_precise) in self.sorted_dependencies(p)? {
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
                let index_pkg = self.index.package(id, version)?;
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
