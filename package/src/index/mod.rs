//! The package index.
//!
//! The package index lives in a hard-coded location on github. It gets cached on the local
//! disk, and then lazily loaded from there and cached in memory.

use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    io::Write,
    path::{Path, PathBuf},
    sync::LazyLock,
};

use gix::ObjectId;
use nickel_lang_core::identifier::Ident;
use nickel_lang_flock::FileLock;
use nickel_lang_git::Spec;
use regex::Regex;
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
struct PackageIndexCache<T: LockType> {
    package_files: HashMap<Id, CachedPackageFile>,
    _lock: IndexLock<T>,
    config: Config,
}

#[derive(Debug)]
pub struct PackageIndex<T: LockType> {
    cache: RefCell<PackageIndexCache<T>>,
}

fn id_path(config: &Config, id: &Id) -> PathBuf {
    match id {
        Id::Github { org, name } => config.index_dir.join("github").join(org).join(name),
    }
}

// We use an advisory file lock to prevent the package index from being modified
// by multiple Nickel processes. This lock file goes inside the cache directory
// (e.g. ~/.cache/nickel/) and it controls access to the index directory
// (e.g. ~/.cache/nickel/index).
const LOCK_INDEX_FILENAME: &str = "index.lock";

pub trait LockType {}

#[derive(Debug)]
pub struct Shared;

#[derive(Debug)]
pub struct Exclusive;

impl LockType for Shared {}
impl LockType for Exclusive {}

#[derive(Debug)]
struct IndexLock<T: LockType> {
    config: Config,
    _inner: FileLock,
    lock_type: std::marker::PhantomData<T>,
}

impl IndexLock<Shared> {
    fn shared(config: &Config) -> Result<Self, Error> {
        let path = config.index_dir.parent().unwrap().join(LOCK_INDEX_FILENAME);
        Ok(IndexLock {
            config: config.clone(),
            _inner: nickel_lang_flock::open_ro_shared_create(&path, "package index")
                .with_path(&path)?,
            lock_type: std::marker::PhantomData,
        })
    }
}

impl IndexLock<Exclusive> {
    fn exclusive(config: &Config) -> Result<Self, Error> {
        let path = config.index_dir.parent().unwrap().join(LOCK_INDEX_FILENAME);
        Ok(IndexLock {
            config: config.clone(),
            _inner: nickel_lang_flock::open_rw_exclusive_create(&path, "package index")
                .with_path(path)?,
            lock_type: std::marker::PhantomData,
        })
    }

    fn index_dir_exists(&self) -> bool {
        self.config.index_dir.exists()
    }

    /// Fetch an updated package index from github and save it to our cache directory.
    fn download_from_github(&self) -> Result<(), Error> {
        let config = &self.config;
        let parent_dir = config.index_dir.parent().unwrap();
        std::fs::create_dir_all(parent_dir).with_path(parent_dir)?;

        eprint!("Fetching an updated package index...");
        let tree_path = tempdir_in(parent_dir).with_path(parent_dir)?;
        let _id = nickel_lang_git::fetch(&Spec::head(config.index_url.clone()), tree_path.path())?;

        // If there's an existing index at the on-disk location, replace it with the
        // fresh one we just downloaded. Doing this atomically and cross-platform is
        // tricky (rename is weird with directories), so we delete and then rename.
        // If everyone is honoring the index lock, no one should interfere between
        // the delete and rename.
        if self.index_dir_exists() {
            // We could do better with error messages here: if the recursive delete fails
            // because of some problem with a child, our error message will nevertheless
            // point at the root path.
            std::fs::remove_dir_all(&config.index_dir).with_path(&config.index_dir)?;
        }
        std::fs::rename(tree_path.into_path(), &config.index_dir).with_path(&config.index_dir)?;
        eprintln!("done!");
        Ok(())
    }
}

impl<T: LockType> PackageIndexCache<T> {
    fn path(&self, id: &Id) -> PathBuf {
        id_path(&self.config, id)
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
                    // FIXME: unwrap
                    let package: Package =
                        serde_json::from_str::<PackageFormat>(line).unwrap().into();
                    if file
                        .packages
                        .insert(package.version.clone(), package)
                        .is_some()
                    {
                        panic!("duplicate version, index is corrupt");
                    }
                }
                Ok(Some(entry.insert(file)))
            }
        }
    }
}

impl PackageIndexCache<Exclusive> {
    /// Saves a package description to disk.
    ///
    /// (Also retains a cached copy in memory.)
    pub fn save(&mut self, pkg: Package) -> Result<(), Error> {
        let id: Id = pkg.id.clone().into();
        let version = pkg.version.clone();
        let mut existing = self
            .load(&id)?
            .cloned()
            .unwrap_or(CachedPackageFile::default());
        if existing.packages.insert(pkg.version.clone(), pkg).is_some() {
            return Err(Error::DuplicateIndexPackageVersion { id, version });
        }
        let mut tmp = self.tmp_file(&id);
        for pkg in existing.packages.values() {
            serde_json::to_writer(&mut tmp, &PackageFormat::from(pkg.clone())).map_err(
                |error| Error::PackageIndexSerialization {
                    pkg: pkg.clone(),
                    error,
                },
            )?;
            tmp.write_all(b"\n").with_path(tmp.path())?;
        }

        let out_path = self.path(&id);
        tmp.persist(&out_path)?;
        Ok(())
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
}

impl PackageIndex<Shared> {
    /// Opens the package index for reading.
    ///
    /// If the package index doesn't exist, downloads a fresh one.
    pub fn shared(config: Config) -> Result<Self, Error> {
        if !config.index_dir.exists() {
            let lock = IndexLock::exclusive(&config)?;
            // We checked above that the index doesn't exist, but maybe someone just
            // created it. Now that we have a lock, we can check for real.
            if !lock.index_dir_exists() {
                lock.download_from_github()?;
            }
        }
        let lock = IndexLock::shared(&config)?;
        Ok(PackageIndex {
            cache: RefCell::new(PackageIndexCache {
                config,
                _lock: lock,
                package_files: HashMap::new(),
            }),
        })
    }
}

impl<T: LockType> PackageIndex<T> {
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

    /// Ensures that an index package is available locally, by downloading it
    /// (if necessary) to the on-disk cache.
    pub fn ensure_downloaded(&self, id: &Id, v: SemVer) -> Result<(), Error> {
        let package = self
            .package(id, v.clone())?
            .ok_or_else(|| Error::UnknownIndexPackage { id: id.clone() })?;
        let precise = PrecisePkg::Index {
            id: id.clone(),
            version: v,
        };
        let target_dir = precise.local_path(&self.cache.borrow().config);
        self.ensure_downloaded_to(&package.id, &target_dir)
    }

    fn ensure_downloaded_to(&self, index_id: &PreciseId, target_dir: &Path) -> Result<(), Error> {
        let PreciseId::Github { org, name, commit } = index_id;
        let url = format!("https://github.com/{org}/{name}.git");
        let url: gix::Url = url.try_into()?;

        if target_dir.exists() {
            eprintln!("Package {org}/{name}@{commit} already exists");
            return Ok(());
        }

        // unwrap: the local path for an index package always has a parent
        let parent_dir = target_dir.parent().unwrap();
        std::fs::create_dir_all(parent_dir).with_path(parent_dir)?;

        // Packages are downloaded at most once: their directory name contains a hash
        // so we assume they will never be touched after downloading. We use a lock
        // to avoid two nickel processes downloading the same package at the same time.
        let lock_path = parent_dir.join(format!("{org}-{name}-{commit}.lock"));
        {
            let _download_lock = nickel_lang_flock::open_rw_exclusive_create(
                &lock_path,
                &format!("download for {org}/{name}@{commit}"),
            )
            .with_path(lock_path)?;

            // Now that we hold the download lock, check for existence again.
            if target_dir.exists() {
                eprintln!("Package {org}/{name}@{commit} already exists");
                return Ok(());
            }

            eprintln!(
                "Downloading {org}/{name}@{commit} to {}",
                target_dir.display()
            );
            let tmp_dir = tempdir_in(parent_dir).with_path(parent_dir)?;
            let _tree_id = nickel_lang_git::fetch(&Spec::commit(url, *commit), tmp_dir.path())?;

            let tmp_dir = tmp_dir.into_path();
            std::fs::rename(tmp_dir, target_dir).with_path(target_dir)?;
        }

        Ok(())
    }
}

impl PackageIndex<Exclusive> {
    /// Opens the package index for writing.
    ///
    /// If the package index doesn't exist, creates an empty one.
    pub fn exclusive(config: Config) -> Result<Self, Error> {
        let lock = IndexLock::exclusive(&config)?;
        if !config.index_dir.exists() {
            std::fs::create_dir_all(&config.index_dir).with_path(&config.index_dir)?;
        }
        Ok(PackageIndex {
            cache: RefCell::new(PackageIndexCache {
                config,
                _lock: lock,
                package_files: HashMap::new(),
            }),
        })
    }

    pub fn save(&mut self, pkg: Package) -> Result<(), Error> {
        self.cache.borrow_mut().save(pkg)
    }
}

/// The identifier of a package in the package index.
#[derive(Clone, PartialEq, Eq, Debug, Hash, Deserialize, PartialOrd, Ord)]
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

    pub fn remote_url(&self) -> Result<gix::Url, Error> {
        match self {
            Id::Github { org, name } => Ok(format!("https://github.com/{org}/{name}").try_into()?),
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
#[derive(Debug)]
pub enum IdParseError {
    /// We expect exactly 2 slashes, and return this error if there aren't.
    Slashes,
    /// We only know about github right now, and return this error if they ask for a different one.
    UnknownIndex { index: String },
    /// Our rules for user and package names are currently the same as Nickel's identifier rules.
    InvalidId { id: String },
}

impl std::error::Error for IdParseError {}

impl std::fmt::Display for IdParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IdParseError::Slashes => {
                write!(f, "doesn't match the expected <index>/<org>/<name> pattern")
            }
            IdParseError::UnknownIndex { index } => write!(
                f,
                "unknown index `{index}`, the only valid value is `github`"
            ),
            IdParseError::InvalidId { id } => write!(f, "invalid identifier `{id}`"),
        }
    }
}

static ID_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new("^_*[a-zA-Z][_a-zA-Z0-9-']*$").unwrap());

impl std::str::FromStr for Id {
    type Err = IdParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split('/');
        let index = parts.next().ok_or(IdParseError::Slashes)?;
        let org = parts.next().ok_or(IdParseError::Slashes)?;
        let name = parts.next().ok_or(IdParseError::Slashes)?;
        if parts.next().is_some() {
            return Err(IdParseError::Slashes);
        };

        if index != "github" {
            return Err(IdParseError::UnknownIndex {
                index: index.to_string(),
            });
        }

        if !ID_REGEX.is_match(org) {
            return Err(IdParseError::InvalidId { id: org.to_owned() });
        }

        if !ID_REGEX.is_match(name) {
            return Err(IdParseError::InvalidId {
                id: name.to_owned(),
            });
        }

        Ok(Id::Github {
            org: org.to_owned(),
            name: name.to_owned(),
        })
    }
}

/// The identifier of a package + version in the package index.
///
/// Includes a content hash of the package.
#[serde_with::serde_as]
#[derive(Clone, PartialEq, Eq, Debug, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub enum PreciseId {
    #[serde(rename = "github")]
    Github {
        org: String,
        name: String,
        #[serde_as(as = "serde_with::DisplayFromStr")]
        commit: ObjectId,
        // FIXME: allow a subdirectory
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
#[derive(Clone, Debug)]
pub struct Package {
    pub id: PreciseId,
    pub version: SemVer,
    pub minimal_nickel_version: SemVer,
    pub dependencies: BTreeMap<Ident, IndexDependency>,

    pub authors: Vec<String>,
    pub description: String,
    pub keywords: Vec<String>,
    pub license: String,
}

/// Defines the serialization format for a package record in the index.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PackageFormat {
    pub id: PreciseId,
    pub version: SemVer,
    pub minimal_nickel_version: SemVer,
    pub dependencies: BTreeMap<Ident, IndexDependencyFormat>,

    pub authors: Vec<String>,
    pub description: String,
    pub keywords: Vec<String>,
    pub license: String,

    /// Version of the index schema. Currently always zero.
    v: u32,
}

impl From<Package> for PackageFormat {
    fn from(p: Package) -> Self {
        Self {
            id: p.id,
            version: p.version,
            minimal_nickel_version: p.minimal_nickel_version,
            dependencies: p
                .dependencies
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            authors: p.authors,
            description: p.description,
            keywords: p.keywords,
            license: p.license,
            v: 0,
        }
    }
}

impl From<PackageFormat> for Package {
    fn from(p: PackageFormat) -> Self {
        Self {
            id: p.id,
            version: p.version,
            minimal_nickel_version: p.minimal_nickel_version,
            dependencies: p
                .dependencies
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
            authors: p.authors,
            description: p.description,
            keywords: p.keywords,
            license: p.license,
        }
    }
}

/// Defines the serialization format for `Id` in the package index.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum IdFormat {
    #[serde(rename = "github")]
    Github { org: String, name: String },
}

impl From<Id> for IdFormat {
    fn from(i: Id) -> Self {
        match i {
            Id::Github { org, name } => IdFormat::Github { org, name },
        }
    }
}

impl From<IdFormat> for Id {
    fn from(i: IdFormat) -> Self {
        match i {
            IdFormat::Github { org, name } => Id::Github { org, name },
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct IndexDependencyFormat {
    #[serde(flatten)]
    pub id: IdFormat,
    pub req: VersionReq,
}

impl From<IndexDependency> for IndexDependencyFormat {
    fn from(i: IndexDependency) -> Self {
        IndexDependencyFormat {
            id: i.id.into(),
            req: i.version,
        }
    }
}

impl From<IndexDependencyFormat> for IndexDependency {
    fn from(i: IndexDependencyFormat) -> Self {
        IndexDependency {
            id: i.id.into(),
            version: i.req,
        }
    }
}

#[cfg(test)]
mod tests {
    use tempfile::tempdir;

    use super::*;

    #[test]
    #[ignore]
    fn load_index() {
        let dir = tempdir().unwrap();
        let config = Config::new().unwrap().with_cache_dir(dir.path().to_owned());
        let index = PackageIndex::shared(config).unwrap();
        dbg!(&index);
        let id = Id::Github {
            org: "jneem".to_owned(),
            name: "json-schema-lib-nickel".to_owned(),
        };
        dbg!(index.available_versions(&id).unwrap().collect::<Vec<_>>());
    }

    #[test]
    fn index_lock() {
        let dir = tempdir().unwrap();
        let config = Config::new().unwrap().with_cache_dir(dir.path().to_owned());
        std::thread::spawn({
            let config = config.clone();
            move || {
                let _index = PackageIndex::shared(config).unwrap();
                eprintln!("sleep");
                std::thread::sleep(std::time::Duration::from_secs(5));
                eprintln!("wake");
            }
        });
        std::thread::sleep(std::time::Duration::from_secs(1));
        eprintln!("acquire");
        let _index = PackageIndex::shared(config).unwrap();
        eprintln!("done");
    }
}
