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

use anyhow::anyhow;
use nickel_lang_core::{identifier::Ident, package::ObjectId};
use pubgrub::version::SemanticVersion;
use serde::{Deserialize, Serialize};
use tempfile::{tempdir_in, NamedTempFile};

use crate::{
    util::{self, cache_dir, clone_git, semver_to_pg},
    Precise,
};

pub const INDEX_URL: &str = "https://github.com/tweag/nickel-mine.git";

pub mod scrape;

/// The in-memory cache.
pub struct PackageCache {
    root: PathBuf,
    package_files: HashMap<Id, CachedPackageFile>,
}

pub struct PackageIndex {
    cache: RefCell<PackageCache>,
}

impl PackageCache {
    fn path(&self, id: &Id) -> PathBuf {
        self.root.join(&id.org).join(&id.name)
    }

    fn tmp_file(&self, id: &Id) -> NamedTempFile {
        let parent = self.root.join(&id.org);
        std::fs::create_dir_all(&parent).unwrap();
        NamedTempFile::new_in(parent).unwrap()
    }

    fn load(&mut self, id: &Id) -> Option<&CachedPackageFile> {
        let mut file = CachedPackageFile::default();
        let data = std::fs::read_to_string(self.path(id)).ok()?;
        for line in data.lines() {
            let package: Package = serde_json::from_str(line).unwrap();
            if file
                .packages
                .insert(package.vers.clone(), package)
                .is_some()
            {
                panic!("duplicate version");
            }
        }

        self.package_files.insert(id.clone(), file);
        self.package_files.get(id)
    }

    pub fn clear(&mut self) {
        self.package_files.clear();
    }

    /// Saves a package description to disk.
    ///
    /// (Also retains a cached copy in memory.)
    pub fn save(&mut self, pkg: Package) {
        let id = pkg.id.clone();
        let mut existing = self
            .load(&pkg.id)
            .cloned()
            .unwrap_or(CachedPackageFile::default());
        if existing.packages.insert(pkg.vers.clone(), pkg).is_some() {
            panic!("you can't overwrite a package version");
        }
        let mut tmp = self.tmp_file(&id);
        for pkg in existing.packages.values() {
            serde_json::to_writer(&mut tmp, pkg).unwrap();
            tmp.write_all(b"\n").unwrap();
        }
        tmp.persist(self.path(&id)).unwrap();
    }
}

impl PackageIndex {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let root = cache_dir().join("index");
        PackageIndex {
            cache: RefCell::new(PackageCache {
                root,
                package_files: HashMap::new(),
            }),
        }
    }

    pub fn fetch_from_github(&self) {
        let root = self.cache.borrow().root.clone();
        let (tmp_dir, _repo) = util::clone_git(INDEX_URL).unwrap();
        let tmp_path = tmp_dir.into_path();
        std::fs::rename(tmp_path, root).unwrap();
    }

    pub fn refresh_from_github(&self) {
        let root = self.cache.borrow().root.clone();
        if !root.exists() {
            self.fetch_from_github();
            return;
        }

        let repo = gix::open(root.clone()).unwrap();
        let remote = repo
            .find_default_remote(gix::remote::Direction::Fetch)
            .unwrap()
            .unwrap();
        let conn = remote.connect(gix::remote::Direction::Fetch).unwrap();
        let outcome = conn
            .prepare_fetch(
                gix::progress::Discard,
                gix::remote::ref_map::Options::default(),
            )
            .unwrap()
            .receive(gix::progress::Discard, &gix::interrupt::IS_INTERRUPTED)
            .unwrap();

        // Set the new head to track the latest upstream. We don't care if it's a fast-forward
        // or not.
        let new_head = repo.find_reference("refs/remotes/origin/main").unwrap();
        repo.head_ref()
            .unwrap()
            .unwrap()
            .set_target_id(new_head.id().detach(), "refresh")
            .unwrap();
        match outcome.status {
            gix::remote::fetch::Status::NoPackReceived { .. } => eprintln!("already up-to-date"),
            gix::remote::fetch::Status::Change { .. } => {}
        }
        let tree = new_head.id().object().unwrap().peel_to_tree().unwrap().id();
        let mut index = repo.index_from_tree(&tree).unwrap();

        // In principle we should be doing more work to clean up the filesystem state. For example,
        // this doesn't delete files that were deleted in the index. But since the registry is
        // append-only it should be ok.
        //
        // (Another interesting possibility: allow PackageIndex to work directly from a git index
        // instead of requiring the files to be saved out.)
        gix::worktree::state::checkout(
            &mut index,
            root,
            repo.objects.clone(),
            &gix::progress::Discard,
            &gix::progress::Discard,
            &gix::interrupt::IS_INTERRUPTED,
            gix::worktree::state::checkout::Options {
                overwrite_existing: true,
                ..Default::default()
            },
        )
        .unwrap();
        index.write(Default::default()).unwrap();
        self.cache.borrow_mut().clear();
    }

    pub fn new_with_root(root: PathBuf) -> Self {
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

    pub fn all_versions(&self, id: &Id) -> HashMap<SemanticVersion, Package> {
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

    // TODO: clarify SemanticVersion vs semver::Version. The point is that SemanticVersion comes from pubgrub, which doesn't support prerelease.
    pub fn package(&self, id: &Id, v: SemanticVersion) -> Option<Package> {
        self.all_versions(id).get(&v).cloned()
    }

    pub fn save(&mut self, pkg: Package) {
        self.cache.borrow_mut().save(pkg)
    }

    pub fn ensure_downloaded(&self, id: &Id, v: semver::Version) -> anyhow::Result<()> {
        let package = self
            .package(id, semver_to_pg(v.clone()))
            .ok_or(anyhow!("tried to download an unknown package"))?;
        let precise = Precise::Index {
            id: id.clone(),
            version: v,
        };
        self.ensure_loc_downloaded(&precise, &package.loc)
    }

    fn ensure_loc_downloaded(
        &self,
        precise: &Precise,
        loc: &PackageLocation,
    ) -> anyhow::Result<()> {
        let PackageLocation::Github { id, rev } = loc;
        let url = format!("https://github.com/{id}.git");

        let target_dir = precise.local_path();
        if target_dir.exists() {
            eprintln!("Package {id}@{rev} already exists");
            return Ok(());
        }

        eprintln!("Downloading {id}@{rev} to {}", target_dir.display());
        let (_tmp_dir, repo) = clone_git(url.as_str())?;
        let commit_id = gix::ObjectId::Sha1(rev.as_ref().to_owned());
        let commit = repo.find_object(commit_id)?;
        let tree = commit.peel_to_tree()?;
        let mut index = repo.index_from_tree(&tree.id())?;

        let target_dir_parent = target_dir.parent().unwrap();
        std::fs::create_dir_all(target_dir_parent)?;
        let tmp_dir = tempdir_in(target_dir_parent)?;

        gix::worktree::state::checkout(
            &mut index,
            tmp_dir.path(),
            repo.objects.clone(),
            &gix::progress::Discard,
            &gix::progress::Discard,
            &gix::interrupt::IS_INTERRUPTED,
            Default::default(),
        )?;

        let tmp_dir = tmp_dir.into_path();
        std::fs::rename(tmp_dir, target_dir)?;

        Ok(())
    }
}

/// Packages in the index are identified by an organization and a package name.
#[derive(Clone, PartialEq, Eq, Debug, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub struct Id {
    pub org: String,
    pub name: String,
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
    pub packages: BTreeMap<semver::Version, Package>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Package {
    #[serde(flatten)]
    pub id: Id,
    pub vers: semver::Version,
    pub nickel_vers: semver::Version,
    pub loc: PackageLocation,
    pub deps: BTreeMap<Ident, IndexDependency>,

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
