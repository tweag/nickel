use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use gix::Url;
use nickel_lang_core::{
    cache::{normalize_abs_path, normalize_rel_path, InputFormat},
    eval::cache::CacheImpl,
    identifier::Ident,
    label::Label,
    package::{ObjectId, PackageMap},
    program::Program,
    term::{make, RichTerm, RuntimeContract, Term},
};
use serde::{Deserialize, Serialize};
use tempfile::tempdir_in;

use crate::{
    cache_dir,
    error::{Error, ResultExt},
    lock::{LockFile, LockFileEntry},
    repo_root, Dependency, LockedPackageSource, Precise,
};

#[derive(Clone, Debug)]
pub struct ManifestFile {
    // The directory containing the manifest file. Path deps are resolved relative to this.
    // If `None`, path deps aren't allowed.
    pub parent_dir: Option<PathBuf>,
    pub name: Ident,
    pub version: semver::Version,
    pub nickel_version: semver::Version,
    pub dependencies: HashMap<Ident, Dependency>,
}

impl ManifestFile {
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, Error> {
        let path = path.as_ref();
        let prog = Program::new_from_file(path, std::io::stderr()).with_path(path)?;
        let mut ret = ManifestFile::from_prog(prog)?;
        ret.parent_dir = path.parent().map(Path::to_owned);
        Ok(ret)
    }

    pub fn from_contents(data: &[u8]) -> Result<Self, Error> {
        let prog = Program::new_from_source(
            std::io::Cursor::new(data),
            "<in-memory manifest>",
            std::io::stderr(),
        )
        .without_path()?;
        ManifestFile::from_prog(prog)
    }

    fn from_prog(mut prog: Program<CacheImpl>) -> Result<Self, Error> {
        // Evaluate the manifest with an extra contract applied, so that nice error message will be generated.
        // (Probably they applied the Manifest contract already, but just in case...)
        // `contract` is `std.package.Manifest`
        use nickel_lang_core::term::UnaryOp::StaticAccess;
        let contract = make::op1(
            StaticAccess("Manifest".into()),
            make::op1(StaticAccess("package".into()), Term::Var("std".into())),
        );
        prog.add_contract(RuntimeContract::new(contract, Label::default()));

        let manifest_term = prog.eval_full().map_err(|e| Error::ManifestEval {
            package: None,
            program: prog,
            error: e,
        })?;
        ManifestFile::from_term(&manifest_term)
    }

    fn lockfile_path(&self) -> Option<PathBuf> {
        let parent_dir = self.parent_dir.as_ref()?;
        Some(parent_dir.join("package.lock"))
    }

    pub fn is_lock_file_up_to_date(&self, lock_file: &LockFile) -> bool {
        self.dependencies.iter().all(|(name, src)| {
            lock_file
                .dependencies
                .get(name)
                .map_or(false, |id| src.matches_locked(id))
        })
    }

    /// Checks if this manifest already has an up-to-date lockfile.
    ///
    /// Here, by up-to-date we mean that all dependencies in the manifest are present in the lockfile.
    /// But we don't, for example, check whether git deps are fully up-to-date.
    fn find_lockfile(&self) -> Option<LockFile> {
        let lock_file = std::fs::read_to_string(self.lockfile_path()?).ok()?;
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

    /// Recursively resolve git dependencies, putting them into a lock file.
    ///
    /// Note that there's no actual version resolution going on yet, because
    /// all dependencies point to specific git revisions.
    ///
    /// Also, path dependencies aren't resolved recursively: path dependencies
    /// don't get locked because they can change at any time.
    pub fn resolve(&self) -> Result<LockFile, Error> {
        crate::resolve::resolve(self);
        let mut ret = LockFile::default();
        for spec in self.dependency_specs() {
            let locked_spec = spec.realize_rec(None)?;
            locked_spec.flatten_into(&mut ret);
            ret.dependencies
                .insert(locked_spec.name, locked_spec.source);
        }
        Ok(ret)
    }

    /// Determine the fully-resolved dependencies.
    ///
    /// Re-uses a lock file if there's one that's up-to-date. Otherwise, regenerates the lock file.
    pub fn lock(&self) -> Result<LockFile, Error> {
        if let Some(lock) = self.find_lockfile() {
            eprintln!("Found an up-to-date lockfile");
            return Ok(lock);
        }

        self.regenerate_lock()
    }

    /// Regenerate the lock file, even if it already exists.
    pub fn regenerate_lock(&self) -> Result<LockFile, Error> {
        let lock = self.resolve()?;

        if let Some(lock_path) = self.lockfile_path() {
            // unwrap: serde_json serialization fails if the derived `Serialize`
            // trait fails (which it shouldn't), or if there's a map with
            // non-string keys (all our maps have `Ident` keys).
            let serialized_lock = serde_json::to_string_pretty(&lock).unwrap();
            if let Err(e) = std::fs::write(lock_path, serialized_lock) {
                eprintln!("Warning: failed to write lock-file: {e}");
            }
        }

        Ok(lock)
    }

    // Convert from a `RichTerm` (that we assume was evaluated deeply). We
    // could serialize/deserialize, but that doesn't handle the enums.
    fn from_term(rt: &RichTerm) -> Result<Self, Error> {
        // This is only ever called with terms that have passed the `std.package.Manifest`
        // contract, so we can assume that they have the right fields.
        fn err(s: &str) -> Error {
            Error::InternalManifestError { msg: s.to_owned() }
        }

        let Term::Record(data) = rt.as_ref() else {
            return Err(err("manifest not a record"));
        };

        // FIXME: yuck
        let name = data
            .fields
            .get(&Ident::new("name"))
            .ok_or_else(|| err("no name"))?
            .value
            .as_ref()
            .ok_or_else(|| err("name has no value"))?;
        let Term::Str(name) = name.as_ref() else {
            return Err(err("name not a string"));
        };

        let version = data
            .fields
            .get(&Ident::new("version"))
            .ok_or_else(|| err("no version"))?
            .value
            .as_ref()
            .ok_or_else(|| err("version has no value"))?;
        let Term::Str(version) = version.as_ref() else {
            return Err(err("version not a string"));
        };

        let nickel_version = data
            .fields
            .get(&Ident::new("nickel-version"))
            .ok_or_else(|| err("no nickel-version"))?
            .value
            .as_ref()
            .ok_or_else(|| err("nickel-version has no value"))?;
        let Term::Str(nickel_version) = nickel_version.as_ref() else {
            return Err(err("nickel-version not a string"));
        };

        let deps = data
            .fields
            .get(&Ident::new("dependencies"))
            .ok_or_else(|| err("no dependencies"))?
            .value
            .as_ref()
            .ok_or_else(|| err("dependencies has no value"))?;
        let Term::Record(deps) = deps.as_ref() else {
            return Err(err("dependencies not a record"));
        };

        let mut ret = Self {
            dependencies: HashMap::new(),
            parent_dir: None,
            version: version.parse().map_err(|_| err("invalid version"))?,
            nickel_version: nickel_version
                .parse()
                .map_err(|_| err("invalid nickel version"))?,
            name: Ident::new(name),
        };

        for (name, dep) in &deps.fields {
            let Term::EnumVariant { tag, arg, .. } = dep
                .value
                .as_ref()
                .ok_or_else(|| err("dependency has no value"))?
                .as_ref()
            else {
                return Err(err("dependency not an enum"));
            };

            match tag.ident().label() {
                "Git" => {
                    let Term::Record(data) = arg.as_ref() else {
                        return Err(err("payload wasn't a record"));
                    };

                    let url = data
                        .fields
                        .get(&Ident::new("url"))
                        .ok_or_else(|| err("no url"))?
                        .value
                        .as_ref()
                        .ok_or_else(|| err("url has no value"))?;
                    let Term::Str(url) = url.as_ref() else {
                        return Err(err("url wasn't a string"));
                    };

                    ret.dependencies.insert(
                        name.ident(),
                        Dependency::Git {
                            url: Url::try_from(url.to_string()).map_err(|e| Error::InvalidUrl {
                                url: url.to_string(),
                                msg: e.to_string(),
                            })?,
                        },
                    );
                }
                "Path" => {
                    let Term::Str(path) = arg.as_ref() else {
                        return Err(err("payload wasn't a string"));
                    };

                    ret.dependencies.insert(
                        name.ident(),
                        Dependency::Path {
                            path: PathBuf::from(path.to_string()),
                        },
                    );
                }
                "Index" => {
                    let payload: IndexPayload = serde_json::from_value(
                        serde_json::to_value(arg.as_ref()).map_err(|_| err("bad payload"))?,
                    )
                    .map_err(|_| err("bad payload"))?;

                    let id: crate::index::Id = payload.name.parse().unwrap();
                    let version: semver::VersionReq = payload.version.parse().unwrap();
                    ret.dependencies
                        .insert(name.ident(), Dependency::Repo { id, version });
                }
                _ => return Err(err("bad tag")),
            }
        }
        Ok(ret)
    }

    pub fn dependency_specs(&self) -> impl Iterator<Item = Spec> + '_ {
        self.dependencies.iter().map(|(&name, source)| Spec {
            name,
            source: source.clone(),
        })
    }
}

#[derive(Deserialize)]
struct IndexPayload {
    name: String,
    version: String,
}

#[derive(Clone, Debug)]
pub struct RealizedDependency {
    /// Either `Git` or `Path`.
    pub precise: Precise,
    pub manifest: ManifestFile,
}

#[derive(Clone, Debug, Default)]
pub struct Realization {
    pub git: HashMap<gix::Url, ObjectId>,
    pub manifests: HashMap<Dependency, RealizedDependency>,
}

impl Realization {
    pub fn realize_all(
        &mut self,
        root_path: &Path,
        dep: &Dependency,
        relative_to: Option<&Precise>,
    ) -> Result<(), Error> {
        let precise = match (dep, relative_to) {
            // Repo dependencies are resolved later. They are not allowed to have
            // transitive git or path dependencies, so we don't even need to recurse.
            (Dependency::Repo { .. }, _) => {
                return Ok(());
            }
            (Dependency::Git { url }, _) => {
                let id = self.realize_one(url)?;
                Precise::Git {
                    id,
                    path: PathBuf::new(),
                }
            }
            (Dependency::Path { path }, None) => Precise::Path { path: path.clone() },
            (Dependency::Path { path }, Some(relative_to)) => {
                let p = normalize_rel_path(&relative_to.local_path().join(path));
                match relative_to {
                    Precise::Git { id, .. } => {
                        let repo = repo_root(id);
                        let p = p.strip_prefix(&repo).map_err(|_| Error::RestrictedPath {
                            package: "TODO".into(),
                            attempted: p.clone(),
                            restriction: repo.to_owned(),
                        })?;
                        Precise::Git {
                            id: *id,
                            path: p.to_owned(),
                        }
                    }
                    _ => Precise::Path { path: p },
                }
            }
        };

        let abs_path = root_path.join(precise.local_path()).join("package.ncl");
        let manifest = ManifestFile::from_path(abs_path)?;
        self.manifests.insert(
            dep.clone(),
            RealizedDependency {
                precise: precise.clone(),
                manifest: manifest.clone(),
            },
        );

        for dep in manifest.dependencies.values() {
            self.realize_all(root_path, dep, Some(&precise))?;
        }

        Ok(())
    }

    fn realize_one(&mut self, url: &gix::Url) -> Result<ObjectId, Error> {
        if let Some(id) = self.git.get(url) {
            return Ok(*id);
        }

        fn err(url: &gix::Url, msg: impl std::fmt::Display) -> Error {
            Error::Git {
                repo: url.to_string(),
                msg: msg.to_string(),
            }
        }

        let cache_dir = cache_dir();
        std::fs::create_dir_all(&cache_dir).with_path(&cache_dir)?;
        let tmp_dir = tempdir_in(&cache_dir).with_path(&cache_dir)?;

        let (mut prepare_checkout, _) = gix::prepare_clone(url.clone(), &tmp_dir)
            .map_err(|e| err(url, e))?
            .fetch_then_checkout(gix::progress::Discard, &gix::interrupt::IS_INTERRUPTED)
            .map_err(|e| err(url, e))?;
        let (repo, _) = prepare_checkout
            .main_worktree(gix::progress::Discard, &gix::interrupt::IS_INTERRUPTED)
            .map_err(|e| err(url, e))?;
        let head = repo.head().map_err(|e| err(url, e))?;
        let id: ObjectId = head
            .into_peeled_id()
            .map_err(|e| err(url, e))?
            .as_bytes()
            .try_into()
            .map_err(|e| err(url, e))?;

        // Now that we know the object hash, move the fetched repo to the right place in the cache.
        let precise = Precise::Git {
            id,
            path: PathBuf::default(),
        };
        let path = precise.local_path();

        if path.is_dir() {
            eprintln!("Already have a cache entry at {path:?}");
        } else {
            let tmp_dir = tmp_dir.into_path();
            std::fs::rename(tmp_dir, &path).with_path(path)?;
        }

        self.git.insert(url.clone(), id);
        Ok(id)
    }
}

/// A single dependency entry in the manifest.
///
/// This is not yet resolved to a specific package version: it could refer to a git repo
/// without yet naming a revision.
#[derive(Clone, Debug)]
pub struct Spec {
    pub name: Ident,
    pub source: Dependency,
}

impl Spec {
    /// If this is a git spec, make the repository available locally and find the precise
    /// revision we want.
    ///
    /// If this is a file spec, just mark it as locked.
    fn realize(&self) -> Result<LockedPackageSource, Error> {
        match &self.source {
            Dependency::Git { url } => {
                fn err(url: &gix::Url, msg: impl std::fmt::Display) -> Error {
                    Error::Git {
                        repo: url.to_string(),
                        msg: msg.to_string(),
                    }
                }

                let cache_dir = cache_dir();
                std::fs::create_dir_all(&cache_dir).with_path(&cache_dir)?;
                let tmp_dir = tempdir_in(&cache_dir).with_path(&cache_dir)?;

                let (mut prepare_checkout, _) = gix::prepare_clone(url.clone(), &tmp_dir)
                    .map_err(|e| err(url, e))?
                    .fetch_then_checkout(gix::progress::Discard, &gix::interrupt::IS_INTERRUPTED)
                    .map_err(|e| err(url, e))?;
                let (repo, _) = prepare_checkout
                    .main_worktree(gix::progress::Discard, &gix::interrupt::IS_INTERRUPTED)
                    .map_err(|e| err(url, e))?;
                let head = repo.head().map_err(|e| err(url, e))?;

                // Now that we know the object hash, move the fetched repo to the right place in the cache.
                let source = LockedPackageSource::Git {
                    repo: url.clone(),
                    tree: head
                        .into_peeled_id()
                        .map_err(|e| err(url, e))?
                        .as_bytes()
                        .try_into()
                        .map_err(|e| err(url, e))?,
                    path: PathBuf::default(),
                };
                let path = source.local_path();

                if path.is_dir() {
                    eprintln!("Already have a cache entry at {path:?}");
                } else {
                    let tmp_dir = tmp_dir.into_path();
                    std::fs::rename(tmp_dir, &path).with_path(path)?;
                }
                Ok(source)
            }
            Dependency::Path { path } => Ok(LockedPackageSource::Path { path: path.clone() }),
            Dependency::Repo { id, version } => todo!(),
        }
    }

    /// Resolve dependencies recursively.
    ///
    /// If `relative_to` is provided, it must be a git repo or an absolute path; path dependencies are resolved relative to it.
    /// Otherwise, path dependencies are left unresolved.
    ///
    /// The path in `LockedPackageSource` must be absolute; if not, we panic. TODO: it might be worth introducing an extra type
    /// that always has absolute paths. `LockedPackageSource` cannot be that thing, because in the lock file itself it
    /// can't be absolute (or lock files couldn't be distributed).
    pub(crate) fn realize_rec(
        &self,
        relative_to: Option<&LockedPackageSource>,
    ) -> Result<LockedSpec, Error> {
        let source = self.realize()?;
        let (abs_path, source) = match (&source, relative_to) {
            (LockedPackageSource::Git { path, .. }, _) => {
                (Some(source.local_path().join(path)), source)
            }
            (LockedPackageSource::Path { .. }, None) => (None, source),
            (LockedPackageSource::Path { path }, Some(relative)) => {
                let p = relative.local_path().join(path);
                assert!(p.is_absolute());
                let p = normalize_abs_path(&p);
                if let Some(r) = relative.repo_root() {
                    let p = p.strip_prefix(&r).map_err(|_| Error::RestrictedPath {
                        package: self.name,
                        attempted: p.clone(),
                        restriction: r.to_owned(),
                    })?;
                    let LockedPackageSource::Git { repo, tree, .. } = relative.clone() else {
                        // Above we checked that relative has a repo_root, so it must be a git repo.
                        // TODO: rethink the types to make this nicer
                        unreachable!();
                    };
                    (
                        Some(p.to_owned()),
                        LockedPackageSource::Git {
                            repo,
                            tree,
                            path: p.to_owned(),
                        },
                    )
                } else {
                    (Some(p.clone()), LockedPackageSource::Path { path: p })
                }
            }
        };

        let dependencies = if let Some(manifest_path) = abs_path {
            let manifest = ManifestFile::from_path(manifest_path.join("package.ncl"))?;
            manifest
                .dependency_specs()
                .map(|s| s.realize_rec(Some(&source)))
                .collect::<Result<_, _>>()?
        } else {
            vec![]
        };

        Ok(LockedSpec {
            name: self.name,
            source,
            dependencies,
        })
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LockedSpec {
    pub name: Ident,
    pub source: LockedPackageSource,
    pub dependencies: Vec<LockedSpec>,
}

impl LockedSpec {
    pub fn flatten(&self) -> LockFile {
        let mut ret = LockFile {
            packages: HashMap::new(),
            dependencies: HashMap::new(),
        };
        self.flatten_into(&mut ret);
        ret
    }

    pub fn flatten_into(&self, lock_file: &mut LockFile) {
        lock_file.packages.insert(
            self.source.clone(),
            LockFileEntry {
                name: self.name,
                dependencies: self
                    .dependencies
                    .iter()
                    .map(|dep| (dep.name, dep.source.clone()))
                    .collect(),
            },
        );

        for dep in &self.dependencies {
            dep.flatten_into(lock_file);
        }
    }

    pub fn flatten_into_map(&self, package_map: &mut PackageMap) {
        package_map
            .packages
            .extend(self.dependencies.iter().map(|dep| {
                (
                    (self.source.local_path(), dep.name),
                    dep.source.local_path(),
                )
            }));

        for dep in &self.dependencies {
            dep.flatten_into_map(package_map);
        }
    }
}
