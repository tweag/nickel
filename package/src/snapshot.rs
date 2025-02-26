use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use gix::ObjectId;
use nickel_lang_core::{cache::normalize_rel_path, identifier::Ident};
use nickel_lang_git::Spec;

use crate::{
    config::Config,
    error::{Error, IoResultExt as _},
    lock::{LockFile, LockFileDep, LockPrecisePkg},
    manifest::MANIFEST_NAME,
    repo_root, Dependency, GitDependency, IndexDependency, ManifestFile, PrecisePkg,
    UnversionedDependency,
};

/// Collects and locks all the path and git dependencies in the dependency tree.
///
/// The manifest file(s) can ask for git dependencies without being too specific:
/// it can ask for a branch, for example, without knowing which commit is the head
/// of the branch. The "snapshot" process fetches all the git dependencies in
/// the dependency tree and figures out exactly which commit should be used for
/// each one.
///
/// Snapshotting is different from version resolution in that we don't consider
/// multiple possible versions of any dependency: we just fetch git branches
/// or tags or whatever, and see what version we get.
#[derive(Clone, Debug)]
pub struct Snapshot {
    /// The Nickel package configured used for building this snapshot.
    ///
    /// The snapshotting process can involve downloading and caching git
    /// packages; the configuration tells us where to put them.
    pub(crate) config: Config,
    /// A map from the possibly-underspecified dependencies given in the
    /// manifest to exact git object ids.
    pub(crate) git: HashMap<GitDependency, ObjectId>,
    /// A map from (parent package, dependency) to child package.
    ///
    /// For every manifest in `manifests` and every non-index dependency of that
    /// manifest, this map is guaranteed to have an entry for it.
    ///
    /// The root package can also be a key of this map; it is represented by the
    /// package `PrecisePkg::Path` with an empty path.
    ///
    /// TODO: document the format of paths in PrecisePkg::Path. Are they absolute?
    dependency: HashMap<(PrecisePkg, UnversionedDependency), PrecisePkg>,
    /// The collection of all manifests we encountered during snapshotting.
    manifests: HashMap<PrecisePkg, ManifestFile>,
}

impl Snapshot {
    /// Snapshots the dependency tree, downloading all necessary git dependencies and finding their exact versions.
    ///
    /// We resolve path dependencies relative to `root_path`.
    pub fn new(config: Config, root_path: &Path, manifest: &ManifestFile) -> Result<Self, Error> {
        Self::new_with_lock(config, root_path, manifest, &LockFile::empty())
    }

    pub fn new_with_lock(
        config: Config,
        root_path: &Path,
        manifest: &ManifestFile,
        lock: &LockFile,
    ) -> Result<Self, Error> {
        let mut ret = Self {
            config,
            git: HashMap::new(),
            dependency: HashMap::new(),
            manifests: HashMap::new(),
        };
        let root_pkg = PrecisePkg::Path {
            path: PathBuf::new(),
        };
        ret.manifests.insert(root_pkg.clone(), manifest.clone());
        for (name, dep) in manifest.sorted_dependencies() {
            let lock_entry = lock.dependencies.get(name);
            ret.snapshot_recursive(root_path, lock, lock_entry, dep, &root_pkg)?;
        }
        Ok(ret)
    }

    // TODO: take in an import sequence (like: the dependency was imported from x, which was imported from y) and use it to improve error messages
    fn snapshot_recursive(
        &mut self,
        root_path: &Path,
        lock: &LockFile,
        lock_entry: Option<&LockFileDep>,
        dep: &Dependency,
        // TODO: maybe this is simpler if we make this non-optional, and have the root denoted by PrecisePkg with an empty path?
        // (Or maybe with an absolute path to the root package?)
        relative_to: &PrecisePkg,
    ) -> Result<(), Error> {
        let precise = match dep {
            Dependency::Git(git) => {
                // If the spec hasn't changed since it was locked, take the id from the lock entry.
                let locked_id = lock_entry.and_then(|entry| {
                    if entry.spec.as_ref() == Some(git) {
                        let LockPrecisePkg::Git { id, .. } = lock.packages[&entry.name].precise
                        else {
                            unreachable!("non-git lock entry {:?} has a git spec", entry);
                        };
                        Some(id)
                    } else {
                        None
                    }
                });

                // If this git repo is a recursive dependency with a relative path, make it
                // relative to the root path.
                let git = {
                    let path = match relative_to {
                        PrecisePkg::Path { path } => Some(path.as_path()),
                        _ => None,
                    };
                    git.relative_to(path)?
                };

                let id = match locked_id {
                    Some(id) => {
                        self.git.insert(git.clone(), id);
                        id
                    }
                    None => self.snapshot_git(&git, root_path)?,
                };
                PrecisePkg::Git {
                    id,
                    url: git.url.clone(),
                    path: git.path.clone(),
                }
            }
            Dependency::Path(path) => {
                let p = normalize_rel_path(&relative_to.local_path(&self.config).join(path));
                match relative_to {
                    PrecisePkg::Git {
                        id,
                        url: repo,
                        path,
                    } => {
                        let repo_path = repo_root(&self.config, id);
                        let p = p
                            .strip_prefix(&repo_path)
                            .map_err(|_| Error::RestrictedPath {
                                package_url: Box::new(repo.clone()),
                                package_commit: *id,
                                package_path: path.clone(),
                                attempted: p.clone(),
                                restriction: repo_path.to_owned(),
                            })?;
                        PrecisePkg::Git {
                            id: *id,
                            url: repo.clone(),
                            path: p.to_owned(),
                        }
                    }
                    _ => PrecisePkg::Path { path: p },
                }
            }
            Dependency::Index(_) => {
                return Ok(());
            }
        };

        let path = precise.local_path(&self.config);
        let abs_path = root_path.join(path);

        let parent_precise = relative_to.clone();
        // unwrap: if dep was an index package, we already returned in the big match
        // statement above
        let udep = dep.clone().as_unversioned().unwrap();
        self.dependency
            .insert((parent_precise, udep), precise.clone());

        // Only read the dependency manifest and recurse if it's a manifest we haven't
        // seen yet.
        if !self.manifests.contains_key(&precise) {
            let manifest = ManifestFile::from_path(abs_path.join(MANIFEST_NAME))?;

            self.manifests.insert(precise.clone(), manifest.clone());

            for (name, dep) in manifest.sorted_dependencies() {
                let lock_entry =
                    lock_entry.and_then(|entry| lock.packages[&entry.name].dependencies.get(name));
                self.snapshot_recursive(root_path, lock, lock_entry, dep, &precise)?;
            }
        }

        Ok(())
    }

    // In case `git` refers to a relative path, `root_path` is what it's relative to.
    fn snapshot_git(&mut self, git: &GitDependency, root_path: &Path) -> Result<ObjectId, Error> {
        if let Some(id) = self.git.get(git) {
            return Ok(*id);
        }
        let abs_git = git.relative_to(Some(root_path))?;

        let url = self
            .config
            .git_replacements
            // The git replacements mechanism works with the *specified* url if it's a relative
            // path, not the absolute url.
            .get(&git.url)
            .unwrap_or(&abs_git.url);

        let spec = Spec {
            url: url.clone(),
            target: git.target.clone(),
        };
        std::fs::create_dir_all(&self.config.git_package_dir)
            .with_path(&self.config.git_package_dir)?;
        let tmp_dir = tempfile::tempdir_in(&self.config.git_package_dir)
            .with_path(&self.config.git_package_dir)?;
        let id = nickel_lang_git::fetch(&spec, tmp_dir.path())?;
        // unwrap: gix currently only supports sha-1 hashes, so we know it will be the right size
        let id: ObjectId = id.as_slice().try_into().unwrap();

        // Now that we know the object hash, move the fetched repo to the right place in the cache.
        let precise = PrecisePkg::Git {
            id,
            url: url.clone(),
            path: PathBuf::default(),
        };
        let path = precise.local_path(&self.config);

        if path.is_dir() {
            // Because the path includes the git id, we're pretty confident that if it
            // exists then it already has the right contents.
            eprintln!("Already have a cache entry at {path:?}");
        } else {
            eprintln!("Checking out {url} to {}", path.display());

            // Unwrap: the result of `Precise::local_path` always has a parent directory.
            let parent_dir = path.parent().unwrap();
            std::fs::create_dir_all(parent_dir).with_path(parent_dir)?;
            let tmp_dir = tmp_dir.into_path();
            eprintln!("want to move {} to {}", tmp_dir.display(), path.display());
            std::fs::rename(tmp_dir, &path).with_path(path)?;
        }

        self.git.insert(git.clone(), id);
        Ok(id)
    }

    pub fn dependency(&self, pkg: &PrecisePkg, dep: &UnversionedDependency) -> &PrecisePkg {
        &self.dependency[&(pkg.clone(), dep.clone())]
    }

    /// Returns the path and git dependencies of a package.
    ///
    /// # Panics
    ///
    /// Panics if the package was not part of the snapshot. (Since a snapshot only contains git
    /// and path packages, in particular the packages must be one of those two kinds.)
    pub fn sorted_unversioned_dependencies(
        &self,
        pkg: &PrecisePkg,
    ) -> Vec<(Ident, (Dependency, PrecisePkg))> {
        let manifest = &self.manifests[pkg];
        let mut ret: Vec<_> = manifest
            .dependencies
            .iter()
            .filter_map(move |(dep_name, dep)| {
                let udep = dep.clone().as_unversioned()?;

                // unwrap: we ensure at construction time that our dependency graph is closed, when
                // restricted to git and path deps.
                let precise_dep = self.dependency.get(&(pkg.clone(), udep)).unwrap();
                Some((*dep_name, (dep.clone(), precise_dep.clone())))
            })
            .collect();
        ret.sort_by(|(name0, _), (name1, _)| name0.label().cmp(name1.label()));
        ret
    }

    pub fn manifest(&self, pkg: &PrecisePkg) -> &ManifestFile {
        &self.manifests[pkg]
    }

    pub fn all_manifests(&self) -> impl Iterator<Item = &ManifestFile> {
        self.manifests.values()
    }

    pub fn all_index_deps(&self) -> impl Iterator<Item = &IndexDependency> {
        self.all_manifests().flat_map(|manifest| {
            manifest.dependencies.values().filter_map(|dep| match dep {
                Dependency::Index(index_dependency) => Some(index_dependency),
                Dependency::Git(_) | Dependency::Path(_) => None,
            })
        })
    }

    /// Returns an iterator over all packages in this snapshot, possibly with duplicates.
    ///
    /// This does not include the root package, and does not include any index packages.
    pub fn all_packages(&self) -> impl Iterator<Item = &PrecisePkg> {
        self.dependency.values()
    }
}
