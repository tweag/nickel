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
    error::{Error, IoResultExt},
    lock::{LockFile, LockFileEntry},
    repo_root,
    resolve::Resolution,
    Dependency, Precise,
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
                .map_or(false, |id| src.matches(id))
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
    /// Path dependencies aren't resolved recursively: path dependencies
    /// don't get locked because they can change at any time.
    pub fn resolve(&self) -> Result<Resolution, Error> {
        crate::resolve::resolve(self)
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
        let resolution = self.resolve()?;
        let lock = resolution.lock_file(self)?;

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
                        .insert(name.ident(), Dependency::Index { id, version });
                }
                _ => return Err(err("bad tag")),
            }
        }
        Ok(ret)
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
    // TODO: the key here should be whatever's in Dependency::Git. Currently that's just the repo url, but it will change
    pub git: HashMap<gix::Url, ObjectId>,
    pub precise: HashMap<Dependency, Precise>,
    pub manifests: HashMap<Precise, ManifestFile>,
}

impl Realization {
    // TODO: take in an import sequence (like: the dependency was imported from x, which was imported from y) and use it to improve error messages
    pub fn realize_all(
        &mut self,
        root_path: &Path,
        dep: &Dependency,
        relative_to: Option<&Precise>,
    ) -> Result<(), Error> {
        let precise = match (dep, relative_to) {
            // Repo dependencies are resolved later. They are not allowed to have
            // transitive git or path dependencies, so we don't even need to recurse.
            (Dependency::Index { .. }, _) => {
                return Ok(());
            }
            (Dependency::Git { url }, _) => {
                let id = self.realize_one(url)?;
                Precise::Git {
                    id,
                    repo: url.clone(),
                    path: PathBuf::new(),
                }
            }
            (Dependency::Path { path }, None) => Precise::Path { path: path.clone() },
            (Dependency::Path { path }, Some(relative_to)) => {
                let p = normalize_rel_path(&relative_to.local_path().join(path));
                match relative_to {
                    Precise::Git { id, repo, .. } => {
                        let repo_path = repo_root(id);
                        let p = p
                            .strip_prefix(&repo_path)
                            .map_err(|_| Error::RestrictedPath {
                                package: "TODO".into(),
                                attempted: p.clone(),
                                restriction: repo_path.to_owned(),
                            })?;
                        Precise::Git {
                            id: *id,
                            repo: repo.clone(),
                            path: p.to_owned(),
                        }
                    }
                    _ => Precise::Path { path: p },
                }
            }
        };

        let abs_path = root_path.join(precise.local_path()).join("package.ncl");
        let manifest = ManifestFile::from_path(abs_path)?;
        self.precise.insert(dep.clone(), precise.clone());
        self.manifests.insert(precise.clone(), manifest.clone());

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
            repo: url.clone(),
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
