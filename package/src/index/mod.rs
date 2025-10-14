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
use nickel_lang_git::Spec;
use regex::Regex;
use serde::{Deserialize, Serialize};
use serialize::PackageFormat;
use tempfile::{tempdir_in, NamedTempFile};

use crate::{
    config::Config,
    error::{Error, IoResultExt as _},
    index::path::{RelativePath, RelativePathError},
    resolve::Resolution,
    version::SemVer,
    IndexDependency, ManifestFile, PreciseIndexPkg, PrecisePkg,
};

pub mod lock;
pub mod path;
pub mod scrape;
pub mod serialize;

pub use lock::{Exclusive, IndexLock, LockType, Shared};
pub use scrape::{fetch_git, read_from_manifest};

/// The in-memory cache, responsible for doing the disk I/O and caching.
#[derive(Debug)]
struct PackageIndexCache<T: LockType> {
    package_files: HashMap<Id, CachedPackageFile>,
    _lock: IndexLock<T>,
    config: Config,
}

/// The package index.
///
/// This can be opened either read-only (i.e. `PackageIndex<Shared>`,
/// constructed by `[PackageIndex::shared]`) or read-write
/// (`PackageIndex<Exclusive>`, constructed by [`PackageIndex::exclusive`]).
#[derive(Debug)]
pub struct PackageIndex<T: LockType> {
    cache: RefCell<PackageIndexCache<T>>,
}

fn id_path(config: &Config, id: &Id) -> PathBuf {
    let mut p = config.index_dir.to_owned();
    p.push(id.path());
    p
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
                let data = match std::fs::read_to_string(&path) {
                    Ok(s) => s,
                    Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                        // It isn't an error if there's no entry for an id.
                        return Ok(None);
                    }
                    Err(e) => {
                        return Err(e).with_path(&path);
                    }
                };
                for line in data.lines() {
                    let package: Package = serde_json::from_str::<PackageFormat>(line)
                        .map_err(|e| Error::PackageIndexDeserialization { error: e })?
                        .into();
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
        // unwrap: the `path` function always outputs a non-empty path
        let path = self.path(id);
        let parent = path.parent().unwrap();
        std::fs::create_dir_all(parent).unwrap();
        NamedTempFile::new_in(parent).unwrap()
    }
}

impl PackageIndex<Shared> {
    /// Opens the package index for reading.
    pub fn shared(config: Config) -> Result<Self, Error> {
        let lock = IndexLock::shared(&config)?;
        Ok(PackageIndex {
            cache: RefCell::new(PackageIndexCache {
                config,
                _lock: lock,
                package_files: HashMap::new(),
            }),
        })
    }

    /// Opens the package index for reading.
    ///
    /// If the package index doesn't exist, downloads a fresh one.
    pub fn shared_or_initialize(config: Config) -> Result<Self, Error> {
        if !config.index_dir.exists() {
            let lock = IndexLock::exclusive(&config)?;
            // We checked above that the index doesn't exist, but maybe someone just
            // created it. Now that we have a lock, we can check for real.
            if !lock.index_dir_exists() {
                lock.download()?;
            }
        }
        PackageIndex::shared(config)
    }

    /// Downloads a fresh index, and then opens it for reading.
    pub fn refreshed(config: Config) -> Result<Self, Error> {
        {
            let lock = IndexLock::exclusive(&config)?;
            lock.download()?;
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
    /// Lists all available versions of a package.
    ///
    /// If the package doesn't exist, returns an empty iterator (and not an error).
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

    pub fn has_version(&self, id: &Id, version: &SemVer) -> Result<bool, Error> {
        let mut cache = self.cache.borrow_mut();
        let pkg_file = cache.load(id)?;
        Ok(pkg_file.is_some_and(|f| f.packages.contains_key(version)))
    }

    /// Returns all versions of a package, along with the associated metadata for each version.
    ///
    /// If the package doesn't exist, returns an empty map (and not an error).
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

    /// Returns the metadata of a specific package version, if it exists.
    pub fn package(&self, id: &Id, v: &SemVer) -> Result<Package, Error> {
        let mut cache = self.cache.borrow_mut();
        let Some(pkg_file) = cache.load(id)? else {
            return Err(Error::UnknownIndexPackage { id: id.clone() });
        };

        Ok(pkg_file
            .packages
            .get(v)
            .ok_or_else(|| Error::UnknownIndexPackageVersion {
                id: id.clone(),
                requested: v.clone(),
                available: pkg_file.packages.keys().cloned().collect(),
            })?
            .clone())
    }

    /// Ensures that an index package is available locally, by downloading it
    /// (if necessary) to the on-disk cache.
    pub fn ensure_downloaded(&self, id: &Id, v: SemVer) -> Result<(), Error> {
        let package = self.package(id, &v)?;
        let precise = PreciseIndexPkg {
            id: id.clone(),
            version: v,
        };
        let config = self.cache.borrow().config.clone();
        let target_dir = precise.local_path_without_subdir(&config, self)?;
        self.ensure_downloaded_to(&package.id, &target_dir)
    }

    fn ensure_downloaded_to(&self, index_id: &PreciseId, target_dir: &Path) -> Result<(), Error> {
        let PreciseId::Github {
            org,
            name,
            commit,
            path: _,
        } = index_id;
        let url = format!("https://github.com/{org}/{name}.git");
        let url: gix::Url = url.try_into()?;

        if target_dir.exists() {
            info!("Package {org}/{name}@{commit} already exists");
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
                info!("Package {org}/{name}@{commit} already exists");
                return Ok(());
            }

            info!(
                "Downloading {org}/{name}@{commit} to {}",
                target_dir.display()
            );
            let tmp_dir = tempdir_in(parent_dir).with_path(parent_dir)?;
            let _tree_id = nickel_lang_git::fetch(&Spec::commit(url, *commit), tmp_dir.path())?;

            std::fs::rename(tmp_dir.keep(), target_dir).with_path(target_dir)?;
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

    /// Writes a package to the index.
    pub fn save(&mut self, pkg: Package) -> Result<(), Error> {
        self.cache.borrow_mut().save(pkg)
    }
}

/// The identifier of a package in the package index.
#[derive(Clone, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
pub enum Id {
    Github {
        org: String,
        name: String,
        path: RelativePath,
    },
}

impl Id {
    /// Returns the path (relative to the package index base directory) where this
    /// package should be stored.
    pub fn path(&self) -> PathBuf {
        match self {
            Id::Github { org, name, path } if path.is_empty() => {
                let mut p = PathBuf::from("github");
                p.push(org);
                p.push(name);
                p
            }
            // A package that lives in a subdirectory of a git repo gets its
            // name encoded to be unique. We can't put files for `github:nickel-lang/js2n/lib`
            // in a subdirectory of the location for `github:nickel-lang/js2n` because there
            // could be conflicts.
            //
            // Instead, we "encode" the name as `github/nickel-lang%@js2n%@lib` so
            // it won't clash with anything. The special characters are not allowed in
            // github repo names, so there can't be collisions.
            //
            // We map `/` -> `%@` and `%` -> `%%` to ensure that the mapping is invertible.
            // (We don't really care about inverting it, but we do care about avoiding
            // collisions).
            Id::Github { org, name, path } => {
                let mut p = PathBuf::from("github");
                p.push(org);
                let mut dir = name.to_owned();
                for c in path.components() {
                    dir.push_str("%@");
                    if c.contains("%") {
                        dir.push_str(&c.replace('%', "%%"));
                    } else {
                        dir.push_str(c);
                    }
                }
                p.push(dir);
                p
            }
        }
    }

    pub fn remote_url(&self) -> Result<gix::Url, Error> {
        match self {
            Id::Github { org, name, path: _ } => {
                Ok(format!("https://github.com/{org}/{name}").try_into()?)
            }
        }
    }
}

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Id::Github { org, name, path } if path.is_empty() => write!(f, "github:{org}/{name}"),
            Id::Github { org, name, path } => write!(f, "github:{org}/{name}/{path}"),
        }
    }
}

#[derive(Debug)]
pub enum IdParseError {
    /// We expect exactly 2 slashes, and return this error if there aren't.
    Separators,
    /// We only know about github right now, and return this error if they ask for a different one.
    UnknownIndex { index: String },
    /// Our rules for user and package names are currently the same as Nickel's identifier rules.
    InvalidId { id: String },
    /// The path component did not parse.
    InvalidPath { path: PathBuf },
}

impl std::error::Error for IdParseError {}

impl std::fmt::Display for IdParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IdParseError::Separators => {
                write!(f, "doesn't match the expected <index>:<org>/<name> pattern")
            }
            IdParseError::UnknownIndex { index } => write!(
                f,
                "unknown index `{index}`, the only valid value is `github`"
            ),
            IdParseError::InvalidId { id } => write!(f, "invalid identifier `{id}`"),
            IdParseError::InvalidPath { path } => write!(f, "invalid path: `{}`", path.display()),
        }
    }
}

static ID_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r#"(?-u)^[\w.-]+$"#).unwrap());

// Note that this is not used for parsing the manifest file (that's parsed
// in the `std.package.Manifest` contract). Rather, this is used through clap
// integration to parse package ids from the command line.
impl std::str::FromStr for Id {
    type Err = IdParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (index, rest) = s.split_once(':').ok_or(IdParseError::Separators)?;
        let (org, name) = rest.split_once('/').ok_or(IdParseError::Separators)?;
        let (name, path) = name.split_once('/').unwrap_or((name, ""));

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

        let path = PathBuf::from(path.to_owned());
        let path = path
            .try_into()
            .map_err(|e: RelativePathError| IdParseError::InvalidPath { path: e.path })?;

        Ok(Id::Github {
            org: org.to_owned(),
            name: name.to_owned(),
            path,
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
        #[serde(default, skip_serializing_if = "RelativePath::is_empty")]
        path: RelativePath,
        #[serde_as(as = "serde_with::DisplayFromStr")]
        commit: ObjectId,
    },
}

impl PreciseId {
    /// Where should this package be fetched from.
    ///
    /// For now, we assume that all packages can be fetched by git. We may also
    /// want support for fetching tarballs (e.g. using the github REST API).
    pub fn download_spec(&self, config: &Config) -> nickel_lang_git::Spec {
        match self {
            PreciseId::Github {
                org,
                name,
                commit,
                path: _,
            } => {
                let mut url = config.github_package_url.clone();
                url.path
                    .extend_from_slice(format!("/{org}/{name}").as_bytes());
                nickel_lang_git::Spec {
                    url,
                    target: nickel_lang_git::Target::Commit(*commit),
                }
            }
        }
    }

    pub fn object_id(&self) -> ObjectId {
        let PreciseId::Github { commit, .. } = self;
        *commit
    }
}

impl From<PreciseId> for Id {
    fn from(id: PreciseId) -> Self {
        match id {
            PreciseId::Github {
                org, name, path, ..
            } => Id::Github { org, name, path },
        }
    }
}

/// A single file in the index.
///
/// Each file in the index corresponds to a package id, and it has one line for each
/// published version of that package.
#[derive(Clone, Debug, Default)]
struct CachedPackageFile {
    packages: BTreeMap<SemVer, Package>,
}

/// A package record in the index, representing a specific version of a package.
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

impl Package {
    /// Given a package manifest and an index id, makes a package index record for that package.
    pub fn from_manifest_and_id(manifest: &ManifestFile, id: &PreciseId) -> Result<Self, Error> {
        let imprecise_id = Id::from(id.clone());
        let deps = manifest
            .dependencies
            .iter()
            .map(|(name, dep)| Ok((*name, dep.clone().as_index_dep(&imprecise_id)?)))
            .collect::<Result<_, Error>>()?;

        Ok(Package {
            id: id.clone(),
            version: manifest.version.clone(),
            minimal_nickel_version: manifest.minimal_nickel_version.clone(),
            dependencies: deps,
            authors: manifest.authors.clone(),
            description: manifest.description.clone(),
            keywords: manifest.keywords.clone(),
            license: manifest.license.clone(),
        })
    }
}

pub fn ensure_index_packages_downloaded(resolution: &Resolution) -> Result<(), Error> {
    for pkg in resolution.all_packages() {
        if let PrecisePkg::Index(PreciseIndexPkg { id, version }) = pkg {
            resolution.index.ensure_downloaded(&id, version)?;
        }
    }

    Ok(())
}
