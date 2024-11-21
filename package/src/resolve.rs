//! We do cargo-style version resolution, where we allow multiple semver-incompatible
//! copies of a package, but we insist that all semver-compatible verisons must resolve
//! to the exact same version.
//!
//! This is not natively supported in pubgrub, so we use one of the two transformations described
//! in [their book](https://pubgrub-rs-guide.pages.dev/limitations/multiple_versions):
//! we make a new package for every collection of semver-compatible
//! versions of each package. So instead of having `foo` with versions `1.1`, `1.2` and `2.0`,
//! we have a package `foo#1` with versions `1.1` and `1.2` and another package `foo#2`
//! with version `2.0`. Since we present them to pubgrub
//! as different packages, they can both appear in the final resolution.

use std::{borrow::Borrow, collections::HashMap, path::PathBuf};

use nickel_lang_core::{cache::normalize_path, identifier::Ident, package::PackageMap};
use pubgrub::{
    report::{DefaultStringReporter, Reporter as _},
    solver::DependencyProvider,
};

use crate::{
    config::Config,
    error::{Error, IoResultExt as _},
    index::{self, Id, IndexDependency, PackageIndex},
    lock::LockFile,
    manifest::Realization,
    version::SemVer,
    Dependency, IndexPrecise, ManifestFile, ObjectId, Precise, VersionReq,
};

type VersionRange = pubgrub::range::Range<SemVer>;

pub struct PackageRegistry {
    // The packages whose versions were locked in a lockfile; we'll try to prefer using
    // those same versions. We won't absolutely insist on it, because if the manifest
    // changed (or some path-dependency changed) then the old locked versions might not
    // resolve anymore.
    previously_locked: HashMap<Package, SemVer>,
    index: PackageIndex,
    realized_unversioned: Realization,
}

impl PackageRegistry {
    pub fn list_versions<'a>(
        &'a self,
        package: &Package,
    ) -> Result<impl Iterator<Item = SemVer> + 'a, Error> {
        let locked_version = self.previously_locked.get(package).cloned();
        let rest = match package {
            Package::Unversioned(_) => {
                Box::new(std::iter::once(SemVer::new(0, 0, 0))) as Box<dyn Iterator<Item = _>>
            }
            Package::Bucket(b) => {
                let bucket_version = b.version;
                let iter = self
                    .index
                    .available_versions(&b.id)?
                    .filter(move |v| bucket_version.contains(v.clone()));
                Box::new(iter)
            }
        };

        // Put the locked version first, and then the other versions in any order (filtering to ensure that the locked version isn't repeated).
        Ok(locked_version
            .clone()
            .into_iter()
            .chain(rest.filter(move |v| Some(v) != locked_version.as_ref())))
    }

    pub fn dep(&self, pkg: &Package, version: &SemVer, dep_id: &Id) -> Result<VersionReq, Error> {
        let deps = match pkg {
            Package::Unversioned(pkg) => self.unversioned_deps(pkg),
            Package::Bucket(b) => self.index_deps(&b.id, version)?,
        };
        Ok(deps
            .iter()
            .find_map(|d| match d {
                Dependency::Index { id, version } if id == dep_id => Some(version.clone()),
                _ => None,
            })
            .unwrap())
    }

    pub fn unversioned_deps(&self, pkg: &UnversionedPrecise) -> Vec<Dependency> {
        let precise = Precise::from(pkg.clone());
        let manifest = &self.realized_unversioned.manifests[&precise];
        manifest.dependencies.values().cloned().collect()
    }

    pub fn index_deps(&self, id: &Id, version: &SemVer) -> Result<Vec<Dependency>, Error> {
        let all_versions = self.index.all_versions(id)?;
        let pkg = all_versions.get(version).unwrap();
        Ok(pkg
            .deps
            .iter()
            .map(|(_, IndexDependency { id, req })| Dependency::Index {
                id: id.clone(),
                version: req.clone(),
            })
            .collect())
    }
}

/// A bucket version represents a collection of compatible semver versions.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum BucketVersion {
    /// A collection of versions all having the same major version number.
    /// (For example, 1.x.y)
    Major(u64),
    /// A collection of versions all having major version zero, and the same minor version number.
    /// (For example, 0.2.x)
    Minor(u64),
}

impl std::fmt::Display for BucketVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BucketVersion::Major(v) => write!(f, "{v}"),
            BucketVersion::Minor(v) => write!(f, "0.{v}"),
        }
    }
}

impl BucketVersion {
    pub fn contains(&self, semver: SemVer) -> bool {
        match *self {
            BucketVersion::Major(v) => v == semver.major,
            BucketVersion::Minor(v) => semver.major == 0 && semver.minor == v,
        }
    }

    pub fn next(&self) -> BucketVersion {
        match *self {
            BucketVersion::Major(v) => BucketVersion::Major(v + 1),
            BucketVersion::Minor(v) => BucketVersion::Minor(v + 1),
        }
    }

    pub fn compatible_range(&self) -> VersionRange {
        VersionRange::between(SemVer::from(*self), SemVer::from(self.next()))
    }
}

impl From<SemVer> for BucketVersion {
    fn from(v: SemVer) -> Self {
        if v.major == 0 {
            BucketVersion::Minor(v.minor)
        } else {
            BucketVersion::Major(v.major)
        }
    }
}

impl From<BucketVersion> for SemVer {
    fn from(bv: BucketVersion) -> Self {
        match bv {
            BucketVersion::Major(v) => SemVer::new(v, 0, 0),
            BucketVersion::Minor(v) => SemVer::new(0, v, 0),
        }
    }
}

impl From<VersionReq> for BucketVersion {
    fn from(v: VersionReq) -> Self {
        match v {
            VersionReq::Compatible(v) | VersionReq::Exact(v) => v.into(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Bucket {
    pub id: Id,
    pub version: BucketVersion,
}

/// Identical to `Precise`, but contains only the unversioned variants.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum UnversionedPrecise {
    Git {
        url: gix::Url,
        id: ObjectId,
        path: PathBuf,
    },
    Path {
        path: PathBuf,
    },
}

impl From<UnversionedPrecise> for Precise {
    fn from(up: UnversionedPrecise) -> Self {
        match up {
            UnversionedPrecise::Git { url, id, path } => Precise::Git { url, id, path },
            UnversionedPrecise::Path { path } => Precise::Path { path },
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Package {
    /// A package that only comes in one version (like a path or a git dependency).
    /// TODO: right now we say that all unversioned packages have version `0.0.0`, but it
    /// isn't great for error messages
    Unversioned(UnversionedPrecise),
    Bucket(Bucket),
}

impl Package {
    pub fn from_index(id: index::Id, version_req: VersionReq) -> (Self, VersionRange) {
        let pkg = Package::Bucket(Bucket {
            id,
            version: version_req.clone().into(),
        });

        let range = match version_req {
            VersionReq::Compatible(v) => VersionRange::higher_than(v),
            VersionReq::Exact(v) => VersionRange::exact(v),
        };
        (pkg, range)
    }
}

impl std::fmt::Display for Package {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Package::Unversioned(UnversionedPrecise::Git { url, .. }) => {
                write!(f, "{}", url)
            }
            Package::Unversioned(UnversionedPrecise::Path { path }) => {
                write!(f, "{}", path.display())
            }
            Package::Bucket(b) => {
                write!(f, "{}#{}", b.id, b.version)
            }
        }
    }
}

// Makes the precise less precise, by returning the bucket that it falls into.
impl From<Precise> for Package {
    fn from(p: Precise) -> Self {
        match p {
            Precise::Git { url, id, path } => {
                Package::Unversioned(UnversionedPrecise::Git { url, id, path })
            }
            Precise::Path { path } => Package::Unversioned(UnversionedPrecise::Path { path }),
            Precise::Index { id, version } => Package::Bucket(Bucket {
                id,
                version: version.into(),
            }),
        }
    }
}

impl DependencyProvider<Package, SemVer> for PackageRegistry {
    fn choose_package_version<T: Borrow<Package>, U: Borrow<pubgrub::range::Range<SemVer>>>(
        &self,
        potential_packages: impl Iterator<Item = (T, U)>,
    ) -> Result<(T, Option<SemVer>), Box<dyn std::error::Error>> {
        // We try to choose the package with the fewest available versions, as the pubgrub
        // docs recommend this as a reasonably-performant heuristic. We count a previously locked package
        // as having one version (even if we'd theoretically be willing to ignore the lock).
        let count_valid = |(p, range): (T, U)| -> Result<_, Box<dyn std::error::Error>> {
            let count = if self.previously_locked.contains_key(p.borrow()) {
                1
            } else {
                self.list_versions(p.borrow())?
                    .filter(|v| range.borrow().contains(v))
                    .count()
            };
            Ok((count, p, range))
        };

        let pkgs_and_ranges = potential_packages
            .map(count_valid)
            .collect::<Result<Vec<_>, _>>()?;
        let (_count, pkg, range) = pkgs_and_ranges
            .into_iter()
            .min_by_key(|(count, _, _)| *count)
            .expect("potential_packages gave us an empty iterator");
        let version = self
            .list_versions(pkg.borrow())?
            .find(|v| range.borrow().contains(v));
        Ok((pkg, version))
    }

    fn get_dependencies(
        &self,
        package: &Package,
        version: &SemVer,
    ) -> Result<pubgrub::solver::Dependencies<Package, SemVer>, Box<dyn std::error::Error>> {
        match package {
            Package::Unversioned(p) => {
                let precise = Precise::from(p.clone());
                let deps = self
                    .unversioned_deps(p)
                    .into_iter()
                    .map(|dep| match dep {
                        Dependency::Git(_) | Dependency::Path { .. } => {
                            let dep_precise = self.realized_unversioned.dependency
                                [&(precise.clone(), dep.clone())]
                                .clone();
                            (Package::Unversioned(dep_precise), VersionRange::any())
                        }
                        Dependency::Index { id, version } => Package::from_index(id, version),
                    })
                    .collect();
                Ok(pubgrub::solver::Dependencies::Known(deps))
            }
            Package::Bucket(b) => {
                let deps = self
                    .index_deps(&b.id, version)?
                    .into_iter()
                    .map(|dep| {
                        let IndexDependency { id, req } = dep.as_index_dep(b.id.clone())?;
                        Ok(Package::from_index(id, req))
                    })
                    .collect::<Result<_, Error>>()?;
                Ok(pubgrub::solver::Dependencies::Known(deps))
            }
        }
    }
}

#[derive(Debug)]
pub struct Resolution {
    pub realization: Realization,
    pub index_packages: HashMap<Id, Vec<SemVer>>,
    pub index: PackageIndex,
}

pub fn resolve(manifest: &ManifestFile, config: Config) -> Result<Resolution, Error> {
    resolve_with_lock(manifest, &LockFile::default(), config)
}

fn previously_locked(_top_level: &Package, lock: &LockFile) -> HashMap<Package, SemVer> {
    fn precise_to_index(p: &Precise) -> Option<IndexPrecise> {
        match p {
            Precise::Index { id, version } => Some(IndexPrecise {
                id: id.clone(),
                version: version.clone(),
            }),
            _ => None,
        }
    }

    // A list of (package: Package, version of the package: SemVer, dependency: IndexPrecise)
    let pkg_deps = lock
        .dependencies
        .values()
        .filter_map(precise_to_index)
        .chain(
            lock.packages
                .values()
                .flat_map(|entry| entry.dependencies.values().filter_map(precise_to_index)),
        );

    pkg_deps
        .map(|IndexPrecise { id, version }| {
            let dep_bucket: BucketVersion = version.clone().into();
            (
                Package::Bucket(Bucket {
                    id,
                    version: dep_bucket,
                }),
                version,
            )
        })
        .collect()
}

pub fn resolve_with_lock(
    manifest: &ManifestFile,
    lock: &LockFile,
    config: Config,
) -> Result<Resolution, Error> {
    // We're forcing the index's root cache directory to be the same as the one used for downloading git
    // deps. In principle we could decouple them, but I'm not sure it's necessary.
    let index = PackageIndex::new(config.clone());
    let mut realization = Realization::new(config);

    // TODO: this assumes that the top-level package has a path. Is there a a use-case for resolving
    // packages without a top-level path?
    let root_path = manifest.parent_dir.as_deref();
    for dep in manifest.dependencies.values() {
        realization.realize_all(root_path.unwrap(), dep, None)?;
    }
    let top_level = UnversionedPrecise::Path {
        path: root_path.unwrap().to_path_buf(),
    };
    realization
        .manifests
        .insert(top_level.clone().into(), manifest.clone());

    let top_level_pkg = Package::Unversioned(top_level);
    let registry = PackageRegistry {
        previously_locked: previously_locked(&top_level_pkg, lock),
        index,
        realized_unversioned: realization,
    };
    // TODO: we could avoid this if there are no index deps
    registry.index.ensure_exists()?;

    let resolution = match pubgrub::solver::resolve(&registry, top_level_pkg, SemVer::new(0, 0, 0))
    {
        Ok(r) => r,
        Err(pubgrub::error::PubGrubError::NoSolution(derivation_tree)) => {
            //derivation_tree.collapse_no_versions();
            let msg = DefaultStringReporter::report(&derivation_tree);
            return Err(Error::Resolution { msg });
        }
        Err(e) => return Err(Error::Resolution { msg: e.to_string() }),
    };
    let mut selected = HashMap::<Id, Vec<SemVer>>::new();
    for (pkg, vers) in resolution.iter() {
        if let Package::Bucket(Bucket { id, .. }) = pkg {
            selected.entry(id.clone()).or_default().push(vers.clone());
        }
    }
    Ok(Resolution {
        realization: registry.realized_unversioned,
        index: registry.index,
        index_packages: selected,
    })
}

impl Resolution {
    /// Finds the precise resolved version of this dependency.
    ///
    /// # Panics
    ///
    /// Panics if the dependency was not part of the dependency tree that this resolution
    /// was generated for.
    pub fn precise(&self, dep: &Dependency) -> Precise {
        match dep {
            Dependency::Git(git) => Precise::Git {
                url: git.url.clone(),
                id: self.realization.git[git],
                path: git.path.clone(),
            },
            Dependency::Path { path } => Precise::Path {
                path: path.to_owned(),
            },
            Dependency::Index { id, version } => Precise::Index {
                id: id.clone(),
                version: self.index_packages[id]
                    .iter()
                    .filter(|v| version.matches(v))
                    .max()
                    .unwrap()
                    .clone(),
            },
        }
    }

    /// Returns all the dependencies of a package, along with their package-local names.
    pub fn dependencies(&self, pkg: &Precise) -> Result<HashMap<Ident, Precise>, Error> {
        let ret = match pkg {
            Precise::Path { .. } | Precise::Git { .. } => {
                let manifest = &self.realization.manifests[pkg];
                manifest
                    .dependencies
                    .iter()
                    .map(move |(dep_name, dep)| {
                        match self.realization.dependency.get(&(pkg.clone(), dep.clone())) {
                            Some(precise_dep) => (*dep_name, precise_dep.clone().into()),
                            None => {
                                // Since the realization contains all the unversioned deps, if we didn't
                                // find our dep then it must be an index dep.
                                (*dep_name, self.precise(dep))
                            }
                        }
                    })
                    .collect()
            }
            Precise::Index { id, version } => {
                let index_pkg = self.index.package(id, version.clone())?.unwrap();
                index_pkg
                    .deps
                    .into_iter()
                    .map(move |(dep_name, dep)| {
                        let precise_dep = self.precise(&Dependency::Index {
                            id: dep.id.clone(),
                            version: dep.req.clone(),
                        });
                        (dep_name, precise_dep)
                    })
                    .collect()
            }
        };
        Ok(ret)
    }

    /// Returns all the resolved packages in the dependency tree.
    pub fn all_precises(&self) -> Vec<Precise> {
        let mut ret: Vec<_> = self
            .realization
            .dependency
            .values()
            .map(|p| p.clone().into())
            .collect();
        ret.sort();
        ret.dedup();

        let index_precises = self.index_packages.iter().flat_map(|(id, vs)| {
            vs.iter().map(|v| Precise::Index {
                id: id.clone(),
                version: v.clone(),
            })
        });
        ret.extend(index_precises);
        ret
    }

    pub fn package_map(&self, manifest: &ManifestFile) -> Result<PackageMap, Error> {
        // TODO: we can still make a package map without a root directory; we just have to disallow
        // relative path dependencies
        let parent_dir = manifest.parent_dir.clone().unwrap();
        let manifest_dir = normalize_path(&parent_dir).with_path(&parent_dir)?;
        let config = &self.realization.config;

        let all = self.all_precises();

        let mut packages = HashMap::new();
        for p in &all {
            let p_path = p.clone().with_abs_path(&manifest_dir).local_path(config);
            let root_path = &manifest_dir;
            for (dep_id, dep_precise) in self.dependencies(p)? {
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
}
