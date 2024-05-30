use std::{cell::RefCell, collections::HashMap, path::PathBuf};

use nickel_lang_core::package::ObjectId;
use pubgrub::version::SemanticVersion;
use semver::VersionReq;
use serde::{Deserialize, Serialize};

pub struct PackageCache {
    root: PathBuf,
    package_files: HashMap<Id, CachedPackageFile>,
}

pub struct PackageIndex {
    // TODO: finer granularity caching would let us expose an API without refcells
    cache: RefCell<PackageCache>,
}

impl PackageCache {
    fn path(&self, id: &Id) -> PathBuf {
        self.root.join(&id.org).join(&id.name)
    }

    fn load(&mut self, id: &Id) -> Option<&CachedPackageFile> {
        let mut file = CachedPackageFile::default();
        let data = std::fs::read_to_string(self.path(id)).ok()?;
        for line in data.lines() {
            let package: Package = serde_json::from_str(line).unwrap();
            if file
                .packages
                .insert(package.vers.clone(), package.into())
                .is_some()
            {
                panic!("duplicate version");
            }
        }

        self.package_files.insert(id.clone(), file);
        self.package_files.get(id)
    }
}

impl PackageIndex {
    pub fn new(root: PathBuf) -> Self {
        PackageIndex {
            cache: RefCell::new(PackageCache {
                root,
                package_files: HashMap::new(),
            }),
        }
    }

    pub fn available_versions<'a>(&'a self, id: &Id) -> impl Iterator<Item = SemanticVersion> + 'a {
        let mut cache = self.cache.borrow_mut();
        let pkg_file = cache.load(id);
        let versions: Vec<_> = pkg_file
            .map(|pkg_file| {
                pkg_file
                    .packages
                    .keys()
                    .map(|v| SemanticVersion::new(v.major as u32, v.minor as u32, v.patch as u32))
                    .collect()
            })
            .unwrap_or_default();
        versions.into_iter()
    }

    pub fn all_versions(&self, id: &Id) -> HashMap<SemanticVersion, CachedPackage> {
        let mut cache = self.cache.borrow_mut();
        let pkg_file = cache.load(id);
        pkg_file
            .map(|pkg_file| {
                pkg_file
                    .packages
                    .iter()
                    .map(|(v, package)| {
                        (
                            SemanticVersion::new(v.major as u32, v.minor as u32, v.patch as u32),
                            package.clone(),
                        )
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    pub fn dependency(&self, id: &Id, v: SemanticVersion, dep: &Id) -> Option<VersionReq> {
        // TODO: clarify SemanticVersion vs semver::Version. The point is that SemanticVersion comes from pubgrub, which doesn't support prerelease.
        let (maj, min, pat) = v.into();
        let v = semver::Version::new(maj.into(), min.into(), pat.into());
        let mut cache = self.cache.borrow_mut();
        let pkg_file = cache.load(id);
        pkg_file?
            .packages
            .get(&v)
            .and_then(|pkg| pkg.deps.get(dep))
            .cloned()
    }

    pub fn save(&mut self, pkg: &CachedPackage) {
        let mut cache = self.cache.borrow_mut();
        let existing = cache.load(&pkg_id);
        // TODO: here
    }
}

/// Packages in the index are identified by an organization and a package name.
#[derive(Clone, PartialEq, Eq, Debug, Hash, Serialize, Deserialize)]
pub struct Id {
    org: String,
    name: String,
}

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.org, self.name)
    }
}

impl std::str::FromStr for Id {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (org, name) = s.split_once('/').ok_or(())?;
        // TODO: decide on valid identifiers
        Ok(Id {
            org: org.to_owned(),
            name: name.to_owned(),
        })
    }
}

#[derive(Clone, Debug, Default)]
pub struct CachedPackageFile {
    packages: HashMap<semver::Version, CachedPackage>,
}

#[derive(Clone, Debug)]
pub struct CachedPackage {
    pub id: Id,
    pub vers: semver::Version,
    pub nickel_vers: semver::Version,
    pub loc: PackageLocation,
    pub deps: HashMap<Id, semver::VersionReq>,
}

impl From<Package> for CachedPackage {
    fn from(p: Package) -> Self {
        Self {
            id: p.id,
            vers: p.vers,
            nickel_vers: p.nickel_vers,
            loc: p.loc,
            deps: p.deps.into_iter().map(|dep| (dep.id, dep.req)).collect(),
        }
    }
}

/// A single entry in the index, representing a single version of a package.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Package {
    #[serde(flatten)]
    id: Id,
    vers: semver::Version,
    nickel_vers: semver::Version,
    loc: PackageLocation,
    deps: Vec<IndexDependency>,

    /// Version of the index schema. Currently always zero.
    v: u32,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum PackageLocation {
    Github {
        #[serde(flatten)]
        id: Id,
        rev: ObjectId,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct IndexDependency {
    #[serde(flatten)]
    pub id: Id,
    pub req: semver::VersionReq,
}
