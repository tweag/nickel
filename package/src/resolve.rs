//! We do cargo-style version resolution, where we allow multiple semver-incompatible
//! copies of a package, but we insist that all semver-compatible verisons must resolve
//! to the exact same version.
//!
//! This is not natively supported in pubgrub, so we use the two transformations described
//! in [their book](https://pubgrub-rs-guide.pages.dev/limitations/multiple_versions).
//! The first transformation is to make a new package for every collection of semver-compatible
//! versions of each package. So instead of having `foo` with versions `1.1`, `1.2` and `2.0`,
//! we have a package `foo#1` with versions `1.1` and `1.2` and another package `foo#2`
//! with version `2.0`. We call `foo#1` and `foo#2` "buckets". Since we present them to pubgrub
//! as different packages, they can both appear in the final resolution.
//!
//! The problem with the approach above is that it introduces "union" dependencies, where
//! a package that required `foo` at version `>=1.0` now has a dependency on either `foo#1`
//! or `foo#2`. Pubgrub doesn't natively support union dependencies, so there's another
//! transformation needed for that. Any time there's a dependency that depends on one of
//! multiple buckets, we insert a "union" package (pubgrub's write-up calls it a "proxy")
//! that has a version for each bucket. See the pubgrub write-up for more details.

use std::{borrow::Borrow, collections::HashMap, path::PathBuf};

use nickel_lang_core::{cache::normalize_path, identifier::Ident, package::PackageMap};
use pubgrub::{
    solver::DependencyProvider,
    version::{SemanticVersion, Version},
};
use semver::{Comparator, VersionReq};

use crate::{
    error::{Error, IoResultExt as _},
    index::{Id, IndexDependency, PackageIndex},
    lock::{LockFile, LockFileEntry},
    manifest::Realization,
    Dependency, ManifestFile, Precise,
};

type VersionRange = pubgrub::range::Range<SemanticVersion>;

pub struct PackageRegistry {
    index: PackageIndex,
    realized_unversioned: Realization,
}

impl PackageRegistry {
    pub fn list_versions<'a>(
        &'a self,
        package: &VirtualPackage,
    ) -> impl Iterator<Item = SemanticVersion> + 'a {
        match package {
            VirtualPackage::Package(Package::Unversioned(_)) => {
                Box::new(std::iter::once(SemanticVersion::zero())) as Box<dyn Iterator<Item = _>>
            }
            VirtualPackage::Package(Package::Bucket(b)) => {
                let bucket_version = b.version;
                let iter = self
                    .index
                    .available_versions(&b.id)
                    .filter(move |v| bucket_version.contains(*v));
                Box::new(iter)
            }
            // The edge package has a version for every bucket of target versions that
            // it could potentially resolve to.
            VirtualPackage::Union {
                source,
                source_version,
                target,
            } => {
                // First, find all buckets of target versions.
                let target_versions = bucket_versions(self.index.available_versions(target));
                // Filter to keep only the ones that could concievably intersect with
                // the allowed version constraints.
                let source_req = self.dep(source, source_version, target);

                let iter = target_versions
                    .filter(move |v| BucketVersion::from(*v).intersects(&source_req));

                Box::new(iter)
            }
        }
    }

    pub fn dep(&self, pkg: &Package, version: &SemanticVersion, dep_id: &Id) -> VersionReq {
        let deps = match pkg {
            Package::Unversioned(pkg) => self.unversioned_deps(pkg),
            Package::Bucket(b) => self.index_deps(&b.id, version),
        };
        deps.iter()
            .find_map(|d| match d {
                Dependency::Index { id, version } if id == dep_id => Some(version.clone()),
                _ => None,
            })
            .unwrap()
    }

    pub fn unversioned_deps(&self, pkg: &UnversionedPackage) -> Vec<Dependency> {
        let dep = Dependency::from(pkg.clone());
        dbg!(pkg, &dep, &self.realized_unversioned);
        let precise = &self.realized_unversioned.precise[&dep];
        let manifest = &self.realized_unversioned.manifests[precise];
        manifest.dependencies.values().cloned().collect()
    }

    pub fn index_deps(&self, id: &Id, version: &SemanticVersion) -> Vec<Dependency> {
        let all_versions = self.index.all_versions(id);
        let pkg = all_versions.get(version).unwrap();
        pkg.deps
            .iter()
            .map(|(_, IndexDependency { id, req })| Dependency::Index {
                id: id.clone(),
                version: req.clone(),
            })
            .collect()
    }
}

fn bucket_versions(
    vs: impl Iterator<Item = SemanticVersion>,
) -> impl Iterator<Item = SemanticVersion> {
    let mut vs: Vec<_> = vs
        .map(BucketVersion::from)
        .map(SemanticVersion::from)
        .collect();
    vs.sort();
    vs.dedup();
    vs.into_iter()
}

/// The same as [`Dependency`], but only for the packages that have fixed, unresolvable, versions.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum UnversionedPackage {
    Git { url: gix::Url },
    Path { path: PathBuf },
}

impl From<UnversionedPackage> for Dependency {
    fn from(p: UnversionedPackage) -> Self {
        match p {
            UnversionedPackage::Git { url } => Dependency::Git { url },
            UnversionedPackage::Path { path } => Dependency::Path { path },
        }
    }
}

/// A bucket version represents a collection of compatible semver versions.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum BucketVersion {
    /// A collection of versions all having the same major version number.
    /// (For example, 1.x.y)
    Major(u32),
    /// A collection of versions all having major version zero, and the same minor version number.
    /// (For example, 0.2.x)
    Minor(u32),
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
    pub fn contains(&self, semver: SemanticVersion) -> bool {
        let (major, minor, _) = semver.into();
        match *self {
            BucketVersion::Major(v) => v == major,
            BucketVersion::Minor(v) => major == 0 && minor == v,
        }
    }

    pub fn next(&self) -> BucketVersion {
        match *self {
            BucketVersion::Major(v) => BucketVersion::Major(v + 1),
            BucketVersion::Minor(v) => BucketVersion::Minor(v + 1),
        }
    }

    pub fn intersects(&self, req: &VersionReq) -> bool {
        req.comparators
            .iter()
            .all(|c| self.intersects_constraint(c))
    }

    pub fn intersects_constraint(&self, comp: &Comparator) -> bool {
        let maj = comp.major as u32; // TODO: avoid casts
        let min = comp.minor.unwrap_or_default() as u32;
        let comp_version = SemanticVersion::new(maj, min, 0);

        let slf: SemanticVersion = (*self).into();
        let next: SemanticVersion = self.next().into();
        let range = slf..next;
        match comp.op {
            semver::Op::Exact => range.contains(&comp_version),
            semver::Op::Greater => next > comp_version,
            semver::Op::GreaterEq => next > comp_version,
            semver::Op::Less => slf < comp_version,
            semver::Op::LessEq => slf <= comp_version,
            semver::Op::Tilde => range.contains(&comp_version),
            semver::Op::Caret => range.contains(&comp_version),
            semver::Op::Wildcard => true,
            _ => todo!(), // FIXME
        }
    }

    pub fn compatible_range(&self) -> VersionRange {
        VersionRange::between(
            SemanticVersion::from(*self),
            SemanticVersion::from(self.next()),
        )
    }
}

impl From<SemanticVersion> for BucketVersion {
    fn from(v: SemanticVersion) -> Self {
        let (major, minor, _) = v.into();
        if major == 0 {
            BucketVersion::Minor(minor)
        } else {
            BucketVersion::Major(major)
        }
    }
}

impl From<BucketVersion> for SemanticVersion {
    fn from(bv: BucketVersion) -> Self {
        match bv {
            BucketVersion::Major(v) => SemanticVersion::new(v, 0, 0),
            BucketVersion::Minor(v) => SemanticVersion::new(0, v, 0),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Bucket {
    pub id: Id,
    pub version: BucketVersion,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Package {
    Unversioned(UnversionedPackage),
    Bucket(Bucket),
}

impl std::fmt::Display for Package {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Package::Unversioned(UnversionedPackage::Git { url }) => {
                write!(f, "{url}")
            }
            Package::Unversioned(UnversionedPackage::Path { path }) => {
                write!(f, "{}", path.display())
            }
            Package::Bucket(b) => {
                write!(f, "{}#{}", b.id, b.version)
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum VirtualPackage {
    Package(Package),
    Union {
        source: Package,
        source_version: SemanticVersion,
        target: Id,
    },
}

fn proxify_dep(pkg: &Package, pkg_version: SemanticVersion, dep: Dependency) -> VirtualPackage {
    match dep {
        Dependency::Git { url } => {
            VirtualPackage::Package(Package::Unversioned(UnversionedPackage::Git { url }))
        }
        Dependency::Path { path } => {
            VirtualPackage::Package(Package::Unversioned(UnversionedPackage::Path { path }))
        }
        // We're making a proxy for every dependency on a repo package, but we could skip
        // the proxy if the dependency range stays within a single semver range.
        Dependency::Index { id, .. } => VirtualPackage::Union {
            source: pkg.clone(),
            source_version: pkg_version,
            target: id,
        },
    }
}

impl std::fmt::Display for VirtualPackage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VirtualPackage::Package(b) => b.fmt(f),
            VirtualPackage::Union {
                source,
                source_version,
                target,
            } => write!(f, "{}@{}->{}", source, source_version, target),
        }
    }
}

impl DependencyProvider<VirtualPackage, SemanticVersion> for PackageRegistry {
    fn choose_package_version<
        T: Borrow<VirtualPackage>,
        U: Borrow<pubgrub::range::Range<SemanticVersion>>,
    >(
        &self,
        potential_packages: impl Iterator<Item = (T, U)>,
    ) -> Result<(T, Option<SemanticVersion>), Box<dyn std::error::Error>> {
        Ok(pubgrub::solver::choose_package_with_fewest_versions(
            |p| self.list_versions(p),
            potential_packages,
        ))
    }

    fn get_dependencies(
        &self,
        package: &VirtualPackage,
        version: &SemanticVersion,
    ) -> Result<
        pubgrub::solver::Dependencies<VirtualPackage, SemanticVersion>,
        Box<dyn std::error::Error>,
    > {
        match package {
            VirtualPackage::Package(b @ Package::Unversioned(p)) => {
                let deps = self
                    .unversioned_deps(p)
                    .into_iter()
                    .map(|dep| (proxify_dep(b, *version, dep), VersionRange::any()))
                    .collect();
                Ok(pubgrub::solver::Dependencies::Known(deps))
            }
            VirtualPackage::Package(Package::Bucket(b)) => {
                let deps = self
                    .index_deps(&b.id, version)
                    .into_iter()
                    .map(|dep| {
                        (
                            proxify_dep(&Package::Bucket(b.clone()), *version, dep),
                            VersionRange::any(),
                        )
                    })
                    .collect();
                Ok(pubgrub::solver::Dependencies::Known(deps))
            }
            VirtualPackage::Union {
                source,
                source_version,
                target,
            } => {
                // A proxy package depends on a single package: its target bucket with the
                // same version as itself.
                let bucket_version = (*version).into();
                let dep = Bucket {
                    id: target.clone(),
                    version: bucket_version,
                };
                let range = version_req_to_range(&self.dep(source, source_version, target));
                let bucket_range = bucket_version.compatible_range().intersection(&range);
                let deps: HashMap<_, _, _> =
                    std::iter::once((VirtualPackage::Package(Package::Bucket(dep)), bucket_range))
                        .collect();
                Ok(pubgrub::solver::Dependencies::Known(deps))
            }
        }
    }
}

fn version_req_to_range(req: &VersionReq) -> pubgrub::range::Range<SemanticVersion> {
    use pubgrub::range::Range;

    let mut ret = pubgrub::range::Range::any();

    fn comp_to_range(comp: &Comparator) -> pubgrub::range::Range<SemanticVersion> {
        let v = SemanticVersion::new(
            comp.major as u32,
            comp.minor.unwrap_or_default() as u32,
            comp.patch.unwrap_or_default() as u32,
        );
        match comp.op {
            semver::Op::Exact => Range::exact(v),
            semver::Op::Greater => Range::higher_than(v.bump()),
            semver::Op::GreaterEq => Range::higher_than(v),
            semver::Op::Less => Range::strictly_lower_than(v),
            semver::Op::LessEq => Range::strictly_lower_than(v).union(&Range::exact(v)),
            semver::Op::Tilde => {
                if comp.minor.is_some() {
                    Range::between(v, v.bump_minor())
                } else {
                    Range::between(v, v.bump_major())
                }
            }
            semver::Op::Caret => {
                if comp.major == 0 {
                    Range::between(v, v.bump_minor())
                } else {
                    Range::between(v, v.bump_major())
                }
            }
            semver::Op::Wildcard => Range::any(),
            _ => panic!("unknown op"),
        }
    }

    for comp in &req.comparators {
        ret = ret.intersection(&comp_to_range(comp));
    }

    ret
}

pub struct Resolution {
    pub realization: Realization,
    pub package_map: HashMap<Id, Vec<semver::Version>>,
    pub index: PackageIndex,
}

pub fn resolve(manifest: &ManifestFile) -> Result<Resolution, Error> {
    let mut realization = Realization::default();

    // pubgrub insists on resolving a top-level package. We'll represent it as a `Path` dependency,
    // so it needs a path...
    let root_path = manifest.parent_dir.as_ref().unwrap();
    for dep in manifest.dependencies.values() {
        realization.realize_all(root_path, dep, None)?;
    }
    let top_level_dep = UnversionedPackage::Path {
        path: root_path.to_owned(),
    };
    let top_level = VirtualPackage::Package(Package::Unversioned(top_level_dep.clone()));
    let precise = Precise::Path {
        path: root_path.to_owned(),
    };
    realization
        .precise
        .insert(top_level_dep.into(), precise.clone());
    realization.manifests.insert(precise, manifest.clone());

    let registry = PackageRegistry {
        index: PackageIndex::new(),
        realized_unversioned: realization,
    };
    registry.index.refresh_from_github();

    let resolution =
        pubgrub::solver::resolve(&registry, top_level, SemanticVersion::zero()).unwrap();
    let mut selected = HashMap::<Id, Vec<semver::Version>>::new();
    for (virt, vers) in resolution.iter() {
        if let VirtualPackage::Package(Package::Bucket(Bucket { id, .. })) = virt {
            let (major, minor, patch) = (*vers).into();
            selected
                .entry(id.clone())
                .or_default()
                .push(semver::Version::new(
                    major.into(),
                    minor.into(),
                    patch.into(),
                ));
        }
    }
    Ok(Resolution {
        realization: registry.realized_unversioned,
        index: registry.index,
        package_map: selected,
    })
}

impl Resolution {
    pub fn precise(&self, dep: &Dependency) -> Precise {
        match dep {
            Dependency::Git { url } => Precise::Git {
                repo: url.clone(),
                id: self.realization.git[url],
                path: PathBuf::new(),
            },
            Dependency::Path { path } => Precise::Path {
                path: path.to_owned(),
            },
            Dependency::Index { id, version } => Precise::Index {
                id: id.clone(),
                version: self.package_map[id]
                    .iter()
                    .filter(|v| version.matches(v))
                    .max()
                    .unwrap()
                    .clone(),
            },
        }
    }

    pub fn dependencies(&self, pkg: &Precise) -> HashMap<Ident, Precise> {
        match pkg {
            p @ Precise::Git { .. } | p @ Precise::Path { .. } => {
                let manifest = &self.realization.manifests[p];
                manifest
                    .dependencies
                    .iter()
                    .map(move |(dep_name, dep)| (*dep_name, self.precise(dep)))
                    .collect()
            }
            Precise::Index { id, version } => {
                let pkg = self
                    .index
                    .package(
                        id,
                        SemanticVersion::new(
                            version.major as u32,
                            version.minor as u32,
                            version.patch as u32,
                        ),
                    )
                    .unwrap();
                pkg.deps
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
        }
    }

    pub fn all_precises(&self) -> Vec<Precise> {
        let mut ret: Vec<_> = self.realization.precise.values().cloned().collect();
        ret.sort();
        ret.dedup();

        let index_precises = self.package_map.iter().flat_map(|(id, vs)| {
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
        let root_path = normalize_path(manifest.parent_dir.clone().unwrap()).without_path()?;

        let all = self.all_precises();

        Ok(PackageMap {
            // Copy over dependencies of the root, making paths absolute.
            top_level: manifest
                .dependencies
                .iter()
                .map(|(name, source)| {
                    (
                        *name,
                        self.precise(source).with_abs_path(&root_path).local_path(),
                    )
                })
                .collect(),

            packages: all
                .iter()
                .flat_map(|p| {
                    let p_path = p.clone().with_abs_path(&root_path).local_path();
                    let root_path = &root_path;
                    self.dependencies(p)
                        .into_iter()
                        .map(move |(dep_id, dep_precise)| {
                            (
                                (p_path.clone(), dep_id),
                                dep_precise.with_abs_path(root_path).local_path(),
                            )
                        })
                })
                .collect(), //,realized_packages.chain(index_packages).collect(),
        })
    }

    pub fn lock_file(&self, manifest: &ManifestFile) -> Result<LockFile, Error> {
        // We don't put all packages in the lock file: we ignore dependencies (and therefore also
        // transitive dependencies) of path deps. In order to figure out what to include, we
        // traverse the depencency graph.
        fn collect_packages(
            slf: &Resolution,
            pkg: &Precise,
            acc: &mut HashMap<Precise, LockFileEntry>,
        ) {
            let entry = LockFileEntry {
                dependencies: if pkg.is_path() {
                    // Skip dependencies of path deps
                    Default::default()
                } else {
                    slf.dependencies(pkg)
                },
            };

            // Only recurse if this is the first time we've encountered this precise package.
            if acc.insert(pkg.clone(), entry).is_none() {
                for (_, dep) in acc[pkg].clone().dependencies {
                    collect_packages(slf, &dep, acc);
                }
            }
        }

        let mut acc = HashMap::new();
        for dep in manifest.dependencies.values() {
            collect_packages(self, &self.precise(dep), &mut acc);
        }

        Ok(LockFile {
            dependencies: manifest
                .dependencies
                .iter()
                .map(|(name, dep)| (*name, self.precise(dep)))
                .collect(),

            packages: acc,
        })
    }
}
