use std::{collections::HashMap, path::PathBuf};

use nickel_lang_core::{cache::normalize_path, identifier::Ident, package::PackageMap};
use pubgrub::{DefaultStringReporter, DependencyProvider, Reporter as _};

use crate::{
    config::Config,
    error::{Error, IoResultExt as _},
    index::{self, PackageIndex, Shared},
    lock::{LockFile, LockPrecisePkg},
    snapshot::Snapshot,
    version::{SemVer, VersionReq},
    Dependency, IndexDependency, ManifestFile, PreciseGitPkg, PreciseIndexPkg, PrecisePkg,
    UnversionedPrecisePkg,
};

pub type ResolveError = pubgrub::PubGrubError<PackageRegistry>;

pub fn print_resolve_error(f: &mut std::fmt::Formatter<'_>, e: &ResolveError) -> std::fmt::Result {
    match e {
        pubgrub::PubGrubError::NoSolution(derivation_tree) => {
            let mut tree = derivation_tree.clone();
            tree.collapse_no_versions();
            write!(f, "{}", DefaultStringReporter::report(&tree))
        }
        pubgrub::PubGrubError::ErrorRetrievingDependencies {
            package: _,
            version: _,
            source,
        } => write!(f, "{source}"),
        pubgrub::PubGrubError::ErrorChoosingVersion { package: _, source } => write!(f, "{source}"),
        // We don't override should_cancel, so it can't trigger an error
        pubgrub::PubGrubError::ErrorInShouldCancel(_) => unreachable!(),
    }
}

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
    Root,
    Git(PreciseGitPkg),
    Path(PathBuf),
    Index(Bucket),
}

impl Package {
    fn unversioned_or_index(self) -> Result<UnversionedPrecisePkg, Bucket> {
        match self {
            Package::Root => Ok(UnversionedPrecisePkg::Path(PathBuf::new())),
            Package::Path(p) => Ok(UnversionedPrecisePkg::Path(p)),
            Package::Git(g) => Ok(UnversionedPrecisePkg::Git(g)),
            Package::Index(bucket) => Err(bucket),
        }
    }
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
            VersionReq::Exact(v) => v.into(),
        }
    }
}

impl From<SemVer> for BucketVersion {
    fn from(v: SemVer) -> Self {
        if v.pre.is_empty() {
            BucketVersion::major_minor(v.major, v.minor)
        } else {
            BucketVersion::Prerelease(v.clone())
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
            Package::Root => {
                write!(f, "top-level package")
            }
            Package::Git(PreciseGitPkg { url, id, path }) => {
                write!(f, "{url}@{id}/{}", path.display())
            }
            Package::Path(path) => {
                write!(f, "'Path {}", path.display())
            }
            Package::Index(b) => {
                write!(f, "{}", b.id)
            }
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

// Like `collect`, but if it encounters a duplicate entry then the resulting ranges are intersected.
fn collect_intersections(
    deps: impl Iterator<Item = (Package, pubgrub::Ranges<SemVer>)>,
) -> pubgrub::Map<Package, pubgrub::Ranges<SemVer>> {
    let mut ret = pubgrub::Map::default();
    for (pkg, ranges) in deps {
        ret.entry(pkg)
            .and_modify(|e: &mut pubgrub::Ranges<_>| *e = e.intersection(&ranges))
            .or_insert(ranges);
    }

    // TODO: if a package depends on conflicting versions of a package, this will say
    // that it depends on an empty set of versions. Which is sort of correct, but the
    // error message is confusing. Fortunately, you only hit this case if you have
    // two conflicting constraints *in the same manifest*. If the conflicts some from
    // other parts of the dependency tree you get a reasonable error.
    ret
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
        pkg: &UnversionedPrecisePkg,
    ) -> pubgrub::Map<Package, pubgrub::Ranges<SemVer>> {
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
                    UnversionedPrecisePkg::Git(g) => Package::Git(g),
                    UnversionedPrecisePkg::Path(path) => Package::Path(path),
                };
                (p, pubgrub::Ranges::full())
            });

        collect_intersections(index_deps.chain(other_deps))
    }
}

impl DependencyProvider for PackageRegistry {
    type P = Package;
    type V = SemVer;
    type VS = pubgrub::Ranges<SemVer>;
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
            Package::Git { .. } | Package::Path { .. } | Package::Root => true,
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
        let check_version = |v| {
            if range.contains(v) {
                Ok(Some(v.clone()))
            } else {
                Ok(None)
            }
        };

        if let Some(locked_version) = self.previously_locked.get(package) {
            if range.contains(locked_version) {
                return Ok(Some(locked_version.clone()));
            }
        }

        match package {
            Package::Git(g) => check_version(
                &self
                    .snapshot
                    .manifest(&UnversionedPrecisePkg::Git(g.clone()))
                    .version,
            ),
            Package::Path(path) => check_version(
                // TODO: less cloning
                &self
                    .snapshot
                    .manifest(&UnversionedPrecisePkg::Path(path.clone()))
                    .version,
            ),
            Package::Index(bucket) => {
                if let BucketVersion::Prerelease(v) = &bucket.version {
                    if self.index.has_version(&bucket.id, v)? {
                        Ok(Some(v.clone()))
                    } else {
                        Ok(None)
                    }
                } else {
                    // `available_versions` are sorted in increasing order, so this will return
                    // the smallest version that's in the bucket and the constrained range.
                    let min_version = self
                        .index
                        .available_versions(&bucket.id)?
                        .find(|v| bucket.version.contains(v) && range.contains(v));
                    Ok(min_version)
                }
            }
            Package::Root => check_version(
                &self
                    .snapshot
                    .manifest(&UnversionedPrecisePkg::Path(PathBuf::new()))
                    .version,
            ),
        }
    }

    fn get_dependencies(
        &self,
        package: &Self::P,
        version: &Self::V,
    ) -> Result<pubgrub::Dependencies<Self::P, Self::VS, Self::M>, Self::Err> {
        let deps: pubgrub::Map<_, _> = match package.clone().unversioned_or_index() {
            Ok(uv) => self.snapshot_dependencies(&uv),
            Err(bucket) => {
                let index_package = self.index.package(&bucket.id, version)?;
                collect_intersections(
                    index_package
                        .dependencies
                        .values()
                        .map(index_dep_package_and_range),
                )
            }
        };

        Ok(pubgrub::Dependencies::Available(deps))
    }
}

fn index_dep_package_and_range(dep: &IndexDependency) -> (Package, pubgrub::Ranges<SemVer>) {
    let p = Package::Index(Bucket {
        id: dep.id.clone(),
        version: dep.version.clone().into(),
    });
    let range = match &dep.version {
        VersionReq::Compatible(v) => {
            let lower_bound = SemVer::new(
                v.major,
                v.minor.unwrap_or_default(),
                v.patch.unwrap_or_default(),
            );
            pubgrub::Ranges::higher_than(lower_bound)
        }
        VersionReq::Exact(req) => pubgrub::Ranges::singleton(req.clone()),
    };

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
pub fn resolve_with_lock(
    manifest: &ManifestFile,
    lock: &LockFile,
    snapshot: Snapshot,
    index: PackageIndex<Shared>,
    config: Config,
) -> Result<Resolution, Error> {
    let version = manifest.version.clone();
    let registry = PackageRegistry {
        previously_locked: lock
            .packages
            .values()
            .filter_map(|entry| {
                let LockPrecisePkg::Index { id, version } = &entry.precise else {
                    return None;
                };

                let pkg = Package::Index(Bucket {
                    id: id.clone(),
                    version: version.clone().into(),
                });

                Some((pkg, version.clone()))
            })
            .collect(),
        index,
        snapshot,
    };

    let deps = pubgrub::resolve(&registry, Package::Root, version)
        .map_err(|e| Error::Resolution(Box::new(e)))?;

    let mut index_packages: HashMap<index::Id, Vec<SemVer>> = HashMap::new();
    for (pkg, version) in deps {
        if let Package::Index(bucket) = pkg {
            index_packages.entry(bucket.id).or_default().push(version);
        }
    }
    for list in index_packages.values_mut() {
        list.sort();
        list.dedup();
    }

    Ok(Resolution {
        config,
        snapshot: registry.snapshot,
        index: registry.index,
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
            Dependency::Git(git) => PrecisePkg::Git(PreciseGitPkg {
                url: git.url.clone(),
                id: self.snapshot.git[git],
                path: git.path.clone(),
            }),
            Dependency::Path(path) => PrecisePkg::Path(path.to_owned()),
            Dependency::Index(idx) => {
                let version = self.index_dep_version(idx).clone();
                PrecisePkg::Index(PreciseIndexPkg {
                    id: idx.id.clone(),
                    version,
                })
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
        match pkg.clone().unversioned_or_index() {
            Ok(uv) => {
                let mut deps: Vec<_> = self
                    .snapshot
                    .sorted_unversioned_dependencies(&uv)
                    .into_iter()
                    .map(|(i, d, p)| (i, d, p.into()))
                    .collect();
                let index_deps =
                    self.snapshot
                        .manifest(&uv)
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
            Err(PreciseIndexPkg { id, version }) => {
                let pkg = self.index.package(&id, &version)?;
                let mut ret: Vec<_> = pkg
                    .dependencies
                    .iter()
                    .map(|(id, index_dep)| {
                        let semver = self.index_packages[&index_dep.id]
                            .iter()
                            .find(|v| index_dep.version.matches(v))
                            .unwrap();
                        (
                            *id,
                            Dependency::Index(index_dep.clone()),
                            PrecisePkg::Index(PreciseIndexPkg {
                                id: index_dep.id.clone(),
                                version: semver.clone(),
                            }),
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

        let mut all: Vec<PrecisePkg> = self
            .snapshot
            .all_packages()
            .map(|p| p.clone().into())
            .collect();
        all.extend(self.index_packages.iter().flat_map(|(id, versions)| {
            versions.iter().map(|v| {
                PrecisePkg::Index(PreciseIndexPkg {
                    id: id.clone(),
                    version: v.clone(),
                })
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
        let ret = match pkg.clone().unversioned_or_index() {
            Ok(uv) => {
                let manifest = self.snapshot.manifest(&uv);
                manifest
                    .dependencies
                    .iter()
                    .map(move |(dep_name, dep)| {
                        let pkg = match dep.clone().as_unversioned() {
                            Some(dep) => self.snapshot.dependency(&uv, &dep).clone().into(),
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
            Err(PreciseIndexPkg { id, version }) => {
                let index_pkg = self.index.package(&id, &version)?;
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
        let mut ret: Vec<_> = self
            .snapshot
            .all_packages()
            .map(|p| p.clone().into())
            .collect();
        ret.extend(self.index_packages.iter().flat_map(|(id, vs)| {
            vs.iter().map(|v| {
                PrecisePkg::Index(PreciseIndexPkg {
                    id: id.clone(),
                    version: v.clone(),
                })
            })
        }));
        ret.sort();
        ret.dedup();
        ret
    }
}
