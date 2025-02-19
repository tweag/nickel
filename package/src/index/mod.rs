//! The package index.
//!
//! The package index lives in a hard-coded location on github. It gets cached on the local
//! disk, and then lazily loaded from there and cached in memory.
//!
//! TODO:
//! - add file locks to protect the on-disk cache from concurrent modification by multiple nickel
//!   processes

use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    io::Write,
    path::PathBuf,
};

use gix::ObjectId;
use nickel_lang_core::identifier::Ident;
use nickel_lang_git::Spec;
use serde::{Deserialize, Serialize};
use tempfile::{tempdir_in, NamedTempFile};

use crate::{
    config::Config,
    error::{Error, IoResultExt as _},
    version::{SemVer, VersionReq},
    IndexDependency, PrecisePkg,
};

pub mod scrape;

pub use scrape::fetch_git;

/// The in-memory cache.
#[derive(Debug)]
pub struct PackageCache {
    package_files: HashMap<Id, CachedPackageFile>,
    config: Config,
}

#[derive(Debug)]
pub struct PackageIndex {
    cache: RefCell<PackageCache>,
}

fn id_path(config: &Config, id: &Id) -> PathBuf {
    match id {
        Id::Github { org, name } => config.index_dir.join("github").join(org).join(name),
    }
}

impl PackageCache {
    fn path(&self, id: &Id) -> PathBuf {
        id_path(&self.config, id)
    }

    /// Creates a temporary file that's in the same directory as the place that `id`'s
    /// index file would go.
    fn tmp_file(&self, id: &Id) -> NamedTempFile {
        let path = self.path(id);
        // unwrap: the `path` function always outputs a non-empty path
        let parent = path.parent().unwrap();
        std::fs::create_dir_all(parent).unwrap();
        NamedTempFile::new_in(parent).unwrap()
    }

    /// Loads and returns all the version metadata for a single package.
    ///
    /// Reads from disk if necessary; returns a cached result if not.
    fn load(&mut self, id: &Id) -> Result<Option<&CachedPackageFile>, Error> {
        use std::collections::hash_map::Entry;

        match self.package_files.entry(id.clone()) {
            Entry::Occupied(entry) => Ok(Some(entry.into_mut())),
            Entry::Vacant(entry) => {
                let mut file = CachedPackageFile::default();
                let path = id_path(&self.config, id);
                let data = std::fs::read_to_string(&path).with_path(&path)?;
                for line in data.lines() {
                    let package: Package = serde_json::from_str(line).unwrap();
                    if file
                        .packages
                        .insert(package.vers.clone(), package)
                        .is_some()
                    {
                        panic!("duplicate version, index is corrupt");
                    }
                }
                Ok(Some(entry.insert(file)))
            }
        }
    }

    pub fn clear(&mut self) {
        self.package_files.clear();
    }

    /// Saves a package description to disk.
    ///
    /// (Also retains a cached copy in memory.)
    pub fn save(&mut self, pkg: Package) -> Result<(), Error> {
        let id: Id = pkg.id.clone().into();
        let version = pkg.vers.clone();
        let mut existing = self
            .load(&id)?
            .cloned()
            .unwrap_or(CachedPackageFile::default());
        if existing.packages.insert(pkg.vers.clone(), pkg).is_some() {
            return Err(Error::DuplicateIndexPackageVersion { id, version });
        }
        let mut tmp = self.tmp_file(&id);
        for pkg in existing.packages.values() {
            serde_json::to_writer(&mut tmp, pkg).map_err(|error| {
                Error::PackageIndexSerialization {
                    pkg: pkg.clone(),
                    error,
                }
            })?;
            tmp.write_all(b"\n").with_path(tmp.path())?;
        }

        let out_path = self.path(&id);
        tmp.persist(&out_path)?;
        Ok(())
    }
}

impl PackageIndex {
    pub fn new(config: Config) -> Self {
        PackageIndex {
            cache: RefCell::new(PackageCache {
                config,
                package_files: HashMap::new(),
            }),
        }
    }

    /// Fetch an updated package index from github and save it to our cache directory.
    /// TODO: refactor this, since there's a distinction between reading (and appending to)
    /// and index, and caching downloaded packages
    pub fn fetch_from_github(&self) -> Result<(), Error> {
        eprint!("Fetching an updated package index...");
        let config = self.cache.borrow().config.clone();

        // unwrap: we defined the root directory ourselves, and it has a parent. (TODO: now that it's configurable, do we need another check?)
        let parent_dir = config.index_dir.parent().unwrap();
        std::fs::create_dir_all(parent_dir).with_path(parent_dir)?;
        let tree_path = tempdir_in(parent_dir).with_path(parent_dir)?;
        let _id = nickel_lang_git::fetch(&Spec::head(config.index_url), tree_path.path())?;

        // If there's an existing index at the on-disk location, replace it with the
        // fresh one we just downloaded. Doing this atomically and cross-platform is
        // tricky (rename is weird with directories), so we delete and then rename,
        // and possibly fail (platform-dependent) if someone beat us to re-creating the
        // directory.
        //
        // Cargo uses an advisory file lock for all changes to the index, so at least
        // multiple instances of cargo won't mess up (but other process could interfere).
        // Maybe we could do the same.
        if config.index_dir.exists() {
            // We could do better with error messages here: if the recursive delete fails
            // because of some problem with a child, our error message will nevertheless
            // point at the root path.
            std::fs::remove_dir_all(&config.index_dir).with_path(&config.index_dir)?;
        }
        std::fs::rename(tree_path.into_path(), &config.index_dir).with_path(&config.index_dir)?;
        eprintln!("done!");
        Ok(())
    }

    /// Fetch the index if we don't have one.
    pub fn ensure_exists(&self) -> Result<(), Error> {
        let root = self.cache.borrow().config.index_dir.clone();
        if !root.exists() {
            self.fetch_from_github()?;
        }
        Ok(())
    }

    pub fn available_versions<'a>(
        &'a self,
        id: &Id,
    ) -> Result<impl Iterator<Item = SemVer> + 'a, Error> {
        let mut cache = self.cache.borrow_mut();
        let pkg_file = cache.load(id)?;
        let versions: Vec<_> = pkg_file
            .map(|pkg_file| pkg_file.packages.keys().cloned().collect())
            .unwrap_or_default();
        Ok(versions.into_iter())
    }

    pub fn all_versions(&self, id: &Id) -> Result<HashMap<SemVer, Package>, Error> {
        let mut cache = self.cache.borrow_mut();
        let pkg_file = cache.load(id)?;
        Ok(pkg_file
            .map(|pkg_file| {
                pkg_file
                    .packages
                    .iter()
                    .map(|(v, package)| (v.clone(), package.clone()))
                    .collect()
            })
            .unwrap_or_default())
    }

    pub fn package(&self, id: &Id, v: SemVer) -> Result<Option<Package>, Error> {
        Ok(self.all_versions(id)?.get(&v).cloned())
    }

    pub fn save(&mut self, pkg: Package) -> Result<(), Error> {
        self.cache.borrow_mut().save(pkg)
    }

    pub fn ensure_downloaded(&self, id: &Id, v: SemVer) -> Result<(), Error> {
        let package = self
            .package(id, v.clone())?
            .ok_or_else(|| Error::UnknownIndexPackage { id: id.clone() })?;
        let precise = PrecisePkg::Index {
            id: id.clone(),
            version: v,
        };
        self.ensure_loc_downloaded(&precise, &package.id)
    }

    fn ensure_loc_downloaded(
        &self,
        precise: &PrecisePkg,
        // TODO: better naming
        loc: &PreciseId,
    ) -> Result<(), Error> {
        let PreciseId::Github { org, name, commit } = loc;
        let url = format!("https://github.com/{org}/{name}.git");
        // unwrap: the url above is valid (TODO: ensure that org and name are sanitized)
        let url: gix::Url = url.try_into().unwrap();

        let target_dir = precise.local_path(&self.cache.borrow().config);
        if target_dir.exists() {
            eprintln!("Package {org}/{name}@{commit} already exists");
            return Ok(());
        }

        // unwrap: the local path for an index package always has a parent
        let parent_dir = target_dir.parent().unwrap();
        std::fs::create_dir_all(parent_dir).with_path(parent_dir)?;
        eprintln!(
            "Downloading {org}/{name}@{commit} to {}",
            target_dir.display()
        );
        let tmp_dir = tempdir_in(parent_dir).with_path(parent_dir)?;
        let _tree_id = nickel_lang_git::fetch(&Spec::commit(url, *commit), tmp_dir.path())?;

        let tmp_dir = tmp_dir.into_path();
        std::fs::rename(tmp_dir, &target_dir).with_path(target_dir)?;

        Ok(())
    }
}

/// The identifier of a package in the package index.
#[derive(Clone, PartialEq, Eq, Debug, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub enum Id {
    Github { org: String, name: String },
}

impl Id {
    /// Returns the path (relative to the package index base directory) where this
    /// package should be stored.
    pub fn path(&self) -> PathBuf {
        match self {
            Id::Github { org, name } => PathBuf::from(format!("github/{org}/{name}")),
        }
    }

    pub fn remote_url(&self) -> gix::Url {
        match self {
            // TODO: once we ensure validation on org and name, the unwrap will be ok.
            Id::Github { org, name } => format!("https://github.com/{org}/{name}")
                .try_into()
                .unwrap(),
        }
    }
}

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Id::Github { org, name } => write!(f, "github/{org}/{name}"),
        }
    }
}

/// The identifier of a package + version in the package index.
///
/// Includes a content hash of the package.
#[derive(Clone, PartialEq, Eq, Debug, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub enum PreciseId {
    #[serde(rename = "github")]
    Github {
        org: String,
        name: String,
        commit: ObjectId,
    },
}

impl From<PreciseId> for Id {
    fn from(id: PreciseId) -> Self {
        match id {
            PreciseId::Github { org, name, .. } => Id::Github { org, name },
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct CachedPackageFile {
    pub packages: BTreeMap<SemVer, Package>,
}

/// A package record in the index.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Package {
    pub id: PreciseId,
    pub vers: SemVer,
    pub nickel_vers: SemVer,
    pub deps: BTreeMap<Ident, IndexDependencyFormat>,

    /// Version of the index schema. Currently always zero.
    v: u32,
    // TODO: any other metadata that we'd like to store in the index
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct IndexDependencyFormat {
    #[serde(flatten)]
    pub id: Id,
    pub req: VersionReq,
}

impl From<IndexDependency> for IndexDependencyFormat {
    fn from(i: IndexDependency) -> Self {
        IndexDependencyFormat {
            id: i.id,
            req: i.version,
        }
    }
}
