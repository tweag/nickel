use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use git2::Repository;
use nickel_lang_core::{
    cache::normalize_abs_path,
    eval::cache::CacheImpl,
    identifier::Ident,
    package::{Name, PackageMap},
    program::Program,
    term::{RichTerm, Term},
};
use serde::{Deserialize, Serialize};
use tempfile::tempdir_in;

use crate::{
    cache_dir,
    lock::{LockFile, LockFileEntry},
    Error, LockedPackageSource, PackageSource,
};

#[derive(Clone, Debug)]
pub struct ManifestFile {
    // The directory containing the manifest file. Path deps are resolved relative to this.
    // If `None`, path deps aren't allowed.
    pub parent_dir: Option<PathBuf>,
    pub dependencies: HashMap<Name, PackageSource>,
}

impl ManifestFile {
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, Error> {
        let path = path.as_ref();
        let mut prog: Program<CacheImpl> = Program::new_from_file(path, std::io::stderr()).unwrap();
        let manifest_term = prog.eval_full().map_err(|e| Error::ManifestEval {
            package: None,
            program: prog,
            error: e,
        })?;
        let mut ret = ManifestFile::from_term(&manifest_term);
        ret.parent_dir = path.parent().map(Path::to_owned);
        Ok(ret)
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
        let lock_file: LockFile = serde_json::from_str(&lock_file).ok()?;
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
    pub fn lock(&self) -> Result<LockFile, Error> {
        if let Some(lock) = self.find_lockfile() {
            eprintln!("Found an up-to-date lockfile");
            return Ok(lock);
        }

        let mut ret = LockFile::default();
        for spec in self.dependency_specs() {
            let locked_spec = spec.realize_rec(None)?;
            locked_spec.flatten_into(&mut ret);
            ret.dependencies
                .insert(locked_spec.name, locked_spec.source);
        }

        // TODO: move this out here so it's possible to compute a lock file without writing it?
        if let Some(lock_path) = self.lockfile_path() {
            // This can fail if paths can't be converted to strings. What to do in that case?
            let serialized_lock = serde_json::to_string_pretty(&ret).unwrap();
            let _ = std::fs::write(lock_path, serialized_lock);
        }

        Ok(ret)
    }

    // Convert from a `RichTerm` (that we assume was evaluated deeply). We
    // could serialize/deserialize, but that doesn't handle the enums...
    //
    // TODO: apply the std.package.Manifest contract, because that will give nice errors
    pub fn from_term(rt: &RichTerm) -> Self {
        let mut ret = Self {
            dependencies: HashMap::new(),
            parent_dir: None,
        };
        let Term::Record(data) = rt.as_ref() else {
            panic!("not a record");
        };

        let deps = data
            .fields
            .get(&Ident::new("dependencies"))
            .unwrap()
            .value
            .as_ref()
            .unwrap();
        let Term::Record(deps) = deps.as_ref() else {
            panic!("deps wasn't an array");
        };

        for (name, dep) in &deps.fields {
            let Term::EnumVariant { tag, arg, .. } = dep.value.as_ref().unwrap().as_ref() else {
                panic!("dep wasn't an enum")
            };

            match tag.ident().label() {
                "Git" => {
                    let Term::Record(data) = arg.as_ref() else {
                        panic!("payload wasn't a record");
                    };

                    let url = data
                        .fields
                        .get(&Ident::new("url"))
                        .unwrap()
                        .value
                        .as_ref()
                        .unwrap();
                    let Term::Str(url) = url.as_ref() else {
                        panic!("url wasn't a string: {url:?}");
                    };

                    ret.dependencies.insert(
                        name.label().parse().unwrap(),
                        PackageSource::Git {
                            url: url.to_string(),
                        },
                    );
                }
                "Path" => {
                    let Term::Str(path) = arg.as_ref() else {
                        panic!("payload wasn't a string");
                    };

                    ret.dependencies.insert(
                        name.label().parse().unwrap(),
                        PackageSource::Path {
                            path: PathBuf::from(path.to_string()),
                        },
                    );
                }
                _ => {
                    panic!("bad tag")
                }
            }
        }
        ret
    }

    pub fn dependency_specs(&self) -> impl Iterator<Item = Spec> + '_ {
        self.dependencies.iter().map(|(name, source)| Spec {
            name: name.clone(),
            source: source.clone(),
        })
    }
}

/// A single dependency entry in the manifest.
///
/// This is not yet resolved to a specific package version: it could refer to a git repo
/// without yet naming a revision.
#[derive(Clone, Debug)]
pub struct Spec {
    pub name: Name,
    pub source: PackageSource,
}

impl Spec {
    /// If this is a git spec, make the repository available locally and find the precise
    /// revision we want.
    ///
    /// If this is a file spec, just mark it as locked.
    fn realize(&self) -> Result<LockedPackageSource, Error> {
        match &self.source {
            PackageSource::Git { url } => {
                // FIXME: unwraps
                let cache_dir = cache_dir();
                std::fs::create_dir_all(&cache_dir).unwrap();
                let tmp_dir = tempdir_in(&cache_dir).unwrap();

                let repo = Repository::clone_recurse(url, &tmp_dir).unwrap();
                let head = repo.head().unwrap().peel_to_commit().unwrap();

                // Now that we know the object hash, move the fetched repo to the right place in the cache.
                let source = LockedPackageSource::Git {
                    repo: url.to_owned(),
                    tree: head.id().as_bytes().try_into().unwrap(),
                    path: PathBuf::default(),
                };
                let path = source.local_path();

                if path.is_dir() {
                    eprintln!("Already have a cache entry at {path:?}");
                } else {
                    let tmp_dir = tmp_dir.into_path();
                    std::fs::rename(tmp_dir, &path).unwrap();
                }
                Ok(source)
            }
            PackageSource::Path { path } => Ok(LockedPackageSource::Path { path: path.clone() }),
        }
    }

    /// Resolve dependencies recursively.
    ///
    /// If `relative_to` is provided, it must be a git repo or an absolute path; path dependencies are resolved relative to it.
    /// Otherwise, path dependencies are left unresolved.
    pub fn realize_rec(
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
                assert!(p.is_absolute()); // FIXME(error handling)
                let p = normalize_abs_path(&p);
                if let Some(r) = relative.repo_root() {
                    let p = p.strip_prefix(&r).map_err(|_| Error::RestrictedPath {
                        attempted: p.clone(),
                        restriction: r.to_owned(),
                    })?;
                    let LockedPackageSource::Git { repo, tree, .. } = relative.clone() else {
                        panic!(); // FIXME
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
            name: self.name.clone(),
            source,
            dependencies,
        })
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LockedSpec {
    pub name: Name,
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
                name: self.name.clone(),
                dependencies: self
                    .dependencies
                    .iter()
                    .map(|dep| (dep.name.clone(), dep.source.clone()))
                    .collect(),
            },
        );

        for dep in &self.dependencies {
            dep.flatten_into(lock_file);
        }
    }

    // FIXME: this is repeated from flatten_into
    pub fn flatten_into_map(&self, package_map: &mut PackageMap) {
        package_map
            .packages
            .extend(self.dependencies.iter().map(|dep| {
                (
                    (self.source.local_path(), dep.name.clone()),
                    dep.source.local_path(),
                )
            }));

        for dep in &self.dependencies {
            dep.flatten_into_map(package_map);
        }
    }
}
