use std::{borrow::Borrow, cell::RefCell, collections::HashMap, path::PathBuf};

use nickel_lang_core::package::ObjectId;
use pubgrub::{
    solver::DependencyProvider,
    version::{SemanticVersion, Version},
};
use semver::{Comparator, VersionReq};

use crate::{
    index::{Id, PackageIndex},
    manifest::{Realization, RealizedDependency},
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
            VirtualPackage::Bucket(PackageBucket::Unversioned(_)) => {
                Box::new(std::iter::once(SemanticVersion::zero())) as Box<dyn Iterator<Item = _>>
            }
            VirtualPackage::Bucket(PackageBucket::Bucket(b)) => {
                let bucket_version = b.version;
                let iter = self
                    .index
                    .available_versions(&b.id)
                    .filter(move |v| bucket_version.contains(*v));
                Box::new(iter)
            }
            // The proxy package has a version for every bucket of target versions that
            // it could potentially resolve to.
            VirtualPackage::Proxy {
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

    pub fn dep(&self, pkg: &PackageBucket, version: &SemanticVersion, dep_id: &Id) -> VersionReq {
        let deps = match pkg {
            PackageBucket::Unversioned(pkg) => self.unversioned_deps(pkg),
            PackageBucket::Bucket(b) => self.index_deps(&b.id, version),
        };
        deps.iter()
            .find_map(|d| match d {
                Dependency::Repo { id, version } if id == dep_id => Some(version.clone()),
                _ => None,
            })
            .unwrap()
    }

    pub fn unversioned_deps(&self, pkg: &UnversionedPackage) -> Vec<Dependency> {
        let dep = Dependency::from(pkg.clone());
        dbg!(pkg, &dep, &self.realized_unversioned);
        let manifest = &self
            .realized_unversioned
            .manifests
            .get(&dep)
            .unwrap()
            .manifest;
        manifest.dependencies.values().cloned().collect()
    }

    pub fn index_deps(&self, id: &Id, version: &SemanticVersion) -> Vec<Dependency> {
        let all_versions = self.index.all_versions(id);
        let pkg = all_versions.get(version).unwrap();
        pkg.deps
            .iter()
            .map(|(id, req)| Dependency::Repo {
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum BucketVersion {
    Major(u32),
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
pub enum PackageBucket {
    Unversioned(UnversionedPackage),
    Bucket(Bucket),
}

impl std::fmt::Display for PackageBucket {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PackageBucket::Unversioned(UnversionedPackage::Git { url }) => {
                write!(f, "{url}")
            }
            PackageBucket::Unversioned(UnversionedPackage::Path { path }) => {
                write!(f, "{}", path.display())
            }
            PackageBucket::Bucket(b) => {
                write!(f, "{}#{}", b.id, b.version)
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum VirtualPackage {
    Bucket(PackageBucket),
    Proxy {
        source: PackageBucket,
        source_version: SemanticVersion,
        target: Id,
    },
}

fn proxify_dep(
    pkg: &PackageBucket,
    pkg_version: SemanticVersion,
    dep: Dependency,
) -> VirtualPackage {
    match dep {
        Dependency::Git { url } => {
            VirtualPackage::Bucket(PackageBucket::Unversioned(UnversionedPackage::Git { url }))
        }
        Dependency::Path { path } => {
            VirtualPackage::Bucket(PackageBucket::Unversioned(UnversionedPackage::Path {
                path,
            }))
        }
        // We're making a proxy for every dependency on a repo package, but we could skip
        // the proxy if the dependency range stays within a single semver range.
        Dependency::Repo { id, .. } => VirtualPackage::Proxy {
            source: pkg.clone(),
            source_version: pkg_version,
            target: id,
        },
    }
}

impl std::fmt::Display for VirtualPackage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VirtualPackage::Bucket(b) => b.fmt(f),
            VirtualPackage::Proxy {
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
            VirtualPackage::Bucket(b @ PackageBucket::Unversioned(p)) => {
                let deps = self
                    .unversioned_deps(p)
                    .into_iter()
                    .map(|dep| (proxify_dep(b, *version, dep), VersionRange::any()))
                    .collect();
                Ok(pubgrub::solver::Dependencies::Known(deps))
            }
            VirtualPackage::Bucket(PackageBucket::Bucket(b)) => {
                let deps = self
                    .index_deps(&b.id, version)
                    .into_iter()
                    .map(|dep| {
                        (
                            proxify_dep(&PackageBucket::Bucket(b.clone()), *version, dep),
                            VersionRange::any(),
                        )
                    })
                    .collect();
                Ok(pubgrub::solver::Dependencies::Known(deps))
            }
            VirtualPackage::Proxy {
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
                let deps: HashMap<_, _, _> = std::iter::once((
                    VirtualPackage::Bucket(PackageBucket::Bucket(dep)),
                    bucket_range,
                ))
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

pub fn resolve(manifest: &ManifestFile) {
    let mut realization = Realization::default();

    // pubgrub insists on resolving a top-level package. We'll represent it as a `Path` dependency,
    // so it needs a path...
    let root_path = manifest.parent_dir.as_ref().unwrap();
    for dep in manifest.dependencies.values() {
        realization.realize_all(root_path, dep, None).ok().unwrap();
    }
    let top_level_dep = UnversionedPackage::Path {
        path: root_path.to_owned(),
    };
    let top_level = VirtualPackage::Bucket(PackageBucket::Unversioned(top_level_dep.clone()));
    realization.manifests.insert(
        top_level_dep.into(),
        RealizedDependency {
            precise: Precise::Path {
                path: root_path.to_owned(),
            },
            manifest: manifest.clone(),
        },
    );

    let registry = PackageRegistry {
        index: PackageIndex::new(PathBuf::from("registry")),
        realized_unversioned: realization,
    };

    let selected = pubgrub::solver::resolve(&registry, top_level, SemanticVersion::zero()).unwrap();
    dbg!(selected);
    // TODO: unvirtualize the result
}
