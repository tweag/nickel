use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use gix::ObjectId;
use nickel_lang_core::{
    cache::{normalize_path, normalize_rel_path},
    error::NullReporter,
    eval::cache::CacheImpl,
    identifier::Ident,
    label::Label,
    package::PackageMap,
    program::Program,
    term::{make, RichTerm, RuntimeContract, Term},
};
use nickel_lang_git::Spec;
use serde::Deserialize;

use crate::{
    config::Config,
    error::{Error, IoResultExt},
    lock::LockFile,
    repo_root,
    version::{FullSemVer, SemVer, SemVerPrefix},
    Dependency, GitDependency, Precise, VersionReq,
};

/// This is the format of an evaluated manifest.
///
/// Manifests are nickel files. In order to ingest them, we first evaluate them
/// as nickel files, then use nickel's deserialization support to turn them into
/// rust structs. This struct defines the format of that deserialization.
///
/// Note that the deserialization step gives pretty useless error messages. We
/// get around this by applying the `std.package.Manifest` contract before
/// evaluation. This means that it's important for the validation applied
/// by deserialization to be less strict than the `std.package.Manifest`
/// contract, so that any errors in the manifest will be caught by the contract.
#[derive(Clone, Debug, Deserialize)]
struct ManifestFileFormat {
    name: Ident,
    version: FullSemVer,
    minimal_nickel_version: SemVerPrefix,
    dependencies: HashMap<Ident, DependencyFormat>,
}

/// The deserialization format of a dependency in the manifest file.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize)]
enum DependencyFormat {
    Git(GitDependency),
    Path(String),
    Index {
        package: String,
        version: VersionReq,
    },
}

impl From<DependencyFormat> for Dependency {
    fn from(df: DependencyFormat) -> Self {
        match df {
            DependencyFormat::Git(g) => Dependency::Git(g),
            DependencyFormat::Path(p) => Dependency::Path { path: p.into() },
            // TODO: make this return an error
            DependencyFormat::Index { .. } => unimplemented!(),
        }
    }
}

/// A package manifest file.
#[derive(Clone, Debug, PartialEq)]
pub struct ManifestFile {
    // The directory containing the manifest file. Path deps are resolved relative to this.
    // If `None`, path deps aren't allowed.
    pub parent_dir: Option<PathBuf>,
    /// The name of the package.
    pub name: Ident,
    /// The version of the package.
    pub version: SemVer,
    /// The minimum nickel version supported by the package.
    pub minimal_nickel_version: SemVer,
    /// All the package's dependencies, and the local names that this package will use to refer to them.
    pub dependencies: HashMap<Ident, Dependency>,
}

impl ManifestFile {
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, Error> {
        let path = path.as_ref();
        let prog =
            Program::new_from_file(path, std::io::stderr(), NullReporter {}).with_path(path)?;
        let mut ret = ManifestFile::from_prog(path, prog)?;
        ret.parent_dir = path.parent().map(Path::to_owned);
        Ok(ret)
    }

    pub fn from_contents(data: &[u8]) -> Result<Self, Error> {
        let prog = Program::new_from_source(
            std::io::Cursor::new(data),
            "<in-memory manifest>",
            std::io::stderr(),
            NullReporter {},
        )
        .without_path()?;
        ManifestFile::from_prog("<generated>".as_ref(), prog)
    }

    fn from_prog(path: &Path, mut prog: Program<CacheImpl>) -> Result<Self, Error> {
        // Evaluate the manifest with an extra contract applied, so that nice error message will be generated.
        // (Probably they applied the Manifest contract already, but just in case...)
        // `contract` is `std.package.Manifest`
        use nickel_lang_core::term::UnaryOp::RecordAccess;
        let contract = make::op1(
            RecordAccess("Manifest".into()),
            make::op1(RecordAccess("package".into()), Term::Var("std".into())),
        );
        prog.add_contract(RuntimeContract::new(contract, Label::default()));

        let manifest_term = prog.eval_full().map_err(|e| Error::ManifestEval {
            package: None,
            files: prog.files(),
            error: e,
        })?;
        ManifestFile::from_term(path, &manifest_term)
    }

    /// Returns the location of the
    pub fn default_lockfile_path(&self) -> Result<PathBuf, Error> {
        let parent_dir = self.parent_dir.as_ref().ok_or(Error::NoManifestParent)?;
        Ok(parent_dir.join("package.ncl.lock"))
    }

    pub fn is_lock_file_up_to_date(&self, lock_file: &LockFile) -> bool {
        self.dependencies.iter().all(|(name, src)| {
            lock_file
                .dependencies
                .get(name.label())
                .map_or(false, |id| src.matches(id))
        })
    }

    /// Checks if this manifest already has an up-to-date lockfile.
    ///
    /// Here, by up-to-date we mean that all dependencies in the manifest are present in the lockfile.
    /// But we don't, for example, check whether git deps are fully up-to-date.
    fn find_lockfile(&self) -> Option<LockFile> {
        let lock_file = std::fs::read_to_string(self.default_lockfile_path().ok()?).ok()?;
        let lock_file: LockFile = match serde_json::from_str(&lock_file) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Found a lockfile, but it failed to parse: {e}");
                return None;
            }
        };
        self.is_lock_file_up_to_date(&lock_file)
            .then_some(lock_file)
    }

    /// Determine the fully-resolved dependencies and write the lock-file to disk.
    ///
    /// Re-uses a lock file if there's one that's up-to-date. Otherwise, regenerates the lock file.
    pub fn lock(&self, config: Config) -> Result<LockFile, Error> {
        if let Some(lock) = self.find_lockfile() {
            eprintln!("Found an up-to-date lockfile");
            return Ok(lock);
        }

        let path = self.default_lockfile_path()?;
        let lock = self.regenerate_lock(config)?;
        lock.write(&path)?;
        Ok(lock)
    }

    /// Regenerate the lock file, even if it already exists.
    pub fn regenerate_lock(&self, config: Config) -> Result<LockFile, Error> {
        let realization = self.realize_dependencies(config)?;
        let lock = LockFile::new(self, &realization)?;

        Ok(lock)
    }

    pub fn realize_dependencies(&self, config: Config) -> Result<Realization, Error> {
        let parent_dir = self.parent_dir.as_ref().ok_or(Error::NoManifestParent)?;
        Realization::new(config.clone(), parent_dir, self.dependencies.values())
    }

    // Convert from a `RichTerm` (that we assume was evaluated deeply). We
    // could serialize/deserialize, but that doesn't handle the enums.
    fn from_term(path: &Path, rt: &RichTerm) -> Result<Self, Error> {
        // This is only ever called with terms that have passed the `std.package.Manifest`
        // contract, so we can assume that they have the right fields.
        let ManifestFileFormat {
            name,
            version,
            minimal_nickel_version,
            dependencies,
        } = ManifestFileFormat::deserialize(rt.clone()).map_err(|e| {
            Error::InternalManifestError {
                path: path.to_owned(),
                msg: e.to_string(),
            }
        })?;
        Ok(Self {
            parent_dir: None,
            name,
            version: version.into(),
            minimal_nickel_version: minimal_nickel_version.into(),
            dependencies: dependencies
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
        })
    }
}

#[derive(Clone, Debug)]
pub struct Realization {
    pub config: Config,
    pub git: HashMap<GitDependency, ObjectId>,
    /// A map from (parent package, dependency) to child package.
    pub dependency: HashMap<(Precise, Dependency), Precise>,
    pub manifests: HashMap<Precise, ManifestFile>,
}

impl Realization {
    pub fn new<'a>(
        config: Config,
        root_path: &Path,
        toplevel_deps: impl Iterator<Item = &'a Dependency>,
    ) -> Result<Self, Error> {
        let mut ret = Self {
            config,
            git: HashMap::new(),
            dependency: HashMap::new(),
            manifests: HashMap::new(),
        };
        for dep in toplevel_deps {
            ret.realize_recursive(root_path, dep, None)?;
        }
        Ok(ret)
    }

    // TODO: take in an import sequence (like: the dependency was imported from x, which was imported from y) and use it to improve error messages
    fn realize_recursive(
        &mut self,
        root_path: &Path,
        dep: &Dependency,
        relative_to: Option<&Precise>,
    ) -> Result<(), Error> {
        let precise = match (dep, relative_to) {
            (Dependency::Git(git), _) => {
                let id = self.realize_one(git)?;
                Precise::Git {
                    id,
                    url: git.url.clone(),
                    path: git.path.clone(),
                }
            }
            (Dependency::Path { path }, None) => Precise::Path { path: path.clone() },
            (Dependency::Path { path }, Some(relative_to)) => {
                let p = normalize_rel_path(&relative_to.local_path(&self.config).join(path));
                match relative_to {
                    Precise::Git {
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
                        Precise::Git {
                            id: *id,
                            url: repo.clone(),
                            path: p.to_owned(),
                        }
                    }
                    _ => Precise::Path { path: p },
                }
            }
        };

        let path = precise.local_path(&self.config);
        let abs_path = root_path.join(path);

        let parent_precise = relative_to.cloned().unwrap_or_else(|| Precise::Path {
            path: root_path.to_owned(),
        });
        self.dependency
            .insert((parent_precise, dep.clone()), precise.clone());

        // Only read the dependency manifest and recurse if it's a manifest we haven't
        // seen yet.
        if !self.manifests.contains_key(&precise) {
            let manifest = ManifestFile::from_path(abs_path.join("package.ncl"))?;

            self.manifests.insert(precise.clone(), manifest.clone());

            for dep in manifest.dependencies.values() {
                self.realize_recursive(root_path, dep, Some(&precise))?;
            }
        }

        Ok(())
    }

    fn realize_one(&mut self, git: &GitDependency) -> Result<ObjectId, Error> {
        if let Some(id) = self.git.get(git) {
            return Ok(*id);
        }

        let url = self
            .config
            .git_replacements
            .get(&git.url)
            .unwrap_or(&git.url);

        let spec = Spec {
            url: url.clone(),
            target: git.target.clone(),
        };
        let tmp_dir =
            tempfile::tempdir_in(&self.config.cache_dir).with_path(&self.config.cache_dir)?;
        let id = nickel_lang_git::fetch(&spec, tmp_dir.path())?;
        // unwrap: gix currently only supports sha-1 hashes, so we know it will be the right size
        let id: ObjectId = id.as_slice().try_into().unwrap();

        // Now that we know the object hash, move the fetched repo to the right place in the cache.
        let precise = Precise::Git {
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
            std::fs::rename(tmp_dir, &path).with_path(path)?;
        }

        self.git.insert(git.clone(), id);
        Ok(id)
    }

    pub fn dependencies(&self, pkg: &Precise) -> HashMap<Ident, Precise> {
        let manifest = &self.manifests[pkg];
        manifest
            .dependencies
            .iter()
            .map(move |(dep_name, dep)| {
                // unwrap: we ensure at construction time that our dependency graph is closed
                // Note that this will change when we introduce index packages.
                let precise_dep = self.dependency.get(&(pkg.clone(), dep.clone())).unwrap();
                (*dep_name, precise_dep.clone())
            })
            .collect()
    }

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
                id: self.git[git],
                path: git.path.clone(),
            },
            Dependency::Path { path } => Precise::Path {
                path: path.to_owned(),
            },
        }
    }

    pub fn package_map(&self, manifest: &ManifestFile) -> Result<PackageMap, Error> {
        // TODO: we can still make a package map without a root directory; we just have to disallow
        // relative path dependencies
        let parent_dir = manifest.parent_dir.clone().unwrap();
        let manifest_dir = normalize_path(&parent_dir).with_path(&parent_dir)?;
        let config = &self.config;

        let mut all: Vec<Precise> = self.dependency.values().cloned().collect();
        all.sort();
        all.dedup();

        let mut packages = HashMap::new();
        for p in &all {
            let p_path = p.clone().with_abs_path(&manifest_dir).local_path(config);
            let root_path = &manifest_dir;
            for (dep_id, dep_precise) in self.dependencies(p) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn manifest() {
        let manifest = ManifestFile::from_contents(
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], description = "hi"}"#.as_bytes(),
        )
        .unwrap();
        assert_eq!(
            manifest,
            ManifestFile {
                parent_dir: None,
                name: "foo".into(),
                version: SemVer::new(1, 0, 0),
                minimal_nickel_version: SemVer::new(1, 9, 0),
                dependencies: HashMap::default()
            }
        );

        let manifest = ManifestFile::from_contents(
            r#"{name = "foo", version = "1.0.0-alpha1", minimal_nickel_version = "1.9.0", authors = [], description = "hi"}"#.as_bytes(),
        )
        .unwrap();
        assert_eq!(
            manifest,
            ManifestFile {
                parent_dir: None,
                name: "foo".into(),
                version: SemVer {
                    major: 1,
                    minor: 0,
                    patch: 0,
                    pre: "alpha1".to_owned()
                },
                minimal_nickel_version: SemVer::new(1, 9, 0),
                dependencies: HashMap::default()
            }
        )
    }
}
