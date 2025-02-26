//! Lock files and lock file utilities

use std::{
    collections::{BTreeMap, HashMap},
    num::ParseIntError,
    path::{Path, PathBuf},
};

use gix::ObjectId;
use serde::{Deserialize, Serialize};
use serde_with::FromInto;

use crate::{
    error::{Error, IoResultExt},
    index::{self},
    resolve::Resolution,
    version::SemVer,
    Dependency, GitDependency, ManifestFile, PrecisePkg,
};

/// We need to give names to entries in the lock-file, so that we can refer to dependencies.
///
/// It's tempting to try to use `Precise` for identifying dependencies, but we can't because
/// the lock file doesn't store paths on the local filesystem. So we identify packages by
/// the name that its importer uses, and add numbers to ensure uniqueness.
#[derive(
    Clone,
    Debug,
    Default,
    serde_with::SerializeDisplay,
    serde_with::DeserializeFromStr,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
)]
pub struct EntryName {
    pub name: String,
    pub id: u32,
}

impl std::fmt::Display for EntryName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.id == 0 {
            write!(f, "{}", self.name)
        } else {
            // There's no ambiguity here because the `std.package.Manifest`
            // contract prohibits package names with spaces.
            write!(f, "{} {}", self.name, self.id)
        }
    }
}

impl std::str::FromStr for EntryName {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<EntryName, ParseIntError> {
        if let Some((name, num)) = s.split_once(' ') {
            Ok(EntryName {
                name: name.to_owned(),
                id: num.parse()?,
            })
        } else {
            Ok(EntryName {
                name: s.to_owned(),
                id: 0,
            })
        }
    }
}

#[derive(Default)]
struct LockFileNamer {
    counts: HashMap<String, u32>,
    assigned: HashMap<PrecisePkg, (String, u32)>,
}

impl LockFileNamer {
    fn name(&mut self, name: &str, pkg: &PrecisePkg) -> EntryName {
        if let Some((old_name, old_num)) = self.assigned.get(pkg) {
            EntryName {
                name: old_name.to_owned(),
                id: *old_num,
            }
        } else {
            let count = *self
                .counts
                .entry(name.to_owned())
                .and_modify(|i| *i += 1)
                .or_insert(0);
            self.assigned.insert(pkg.clone(), (name.to_owned(), count));
            EntryName {
                name: name.to_owned(),
                id: count,
            }
        }
    }
}

/// A lock file, specifying versions and names for all recursive dependencies.
///
/// This defines the on-disk format for lock files.
///
/// # Open question
///
/// There's one big open question about the lock file: should it contain information
/// about path dependencies (and their recursive dependencies)? If it does, you
/// can immediately derive the `PackageMap` from the lock file, meaning that if the
/// interpreter gets the lock file then it can do everything else from there,
/// without doing any package resolution. So that's nice.
///
/// The problem with putting information about path dependencies in the lock file is
/// that path dependencies can change without notice, making the lock file stale.
/// So the interpreter didn't have to do much work, but it ended up running on old
/// information.
///
/// I think the decision here basically comes down to what we want from the CLI
/// interface. If we require a separate update-the-lock-file step (a la npm or poetry),
/// it makes sense to put the path dependency info here. But if we want an
/// auto-refresh step (a la cargo), we want to leave it out.
#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct LockFile {
    /// The dependencies of the current (top-level) package.
    ///
    /// These should be sorted so that the serialization doesn't change all the time.
    pub dependencies: BTreeMap<String, LockFileDep>,
    /// All packages that we know about, and the dependencies of each one.
    ///
    /// Note that the package list is not guaranteed to be closed: path dependencies
    /// cannot have their dependencies resolved in the on-disk lockfile because they
    /// can change at any time. *Some* path dependencies (for example, path dependencies
    /// that are local to a git depencency repo) may have resolved dependencies.
    pub packages: BTreeMap<EntryName, LockFileEntry>,
}

impl LockFile {
    pub fn empty() -> Self {
        Self {
            dependencies: BTreeMap::new(),
            packages: BTreeMap::new(),
        }
    }

    pub fn new(manifest: &ManifestFile, resolution: &Resolution) -> Result<Self, Error> {
        fn collect_packages(
            resolution: &Resolution,
            id: &str,
            pkg: &PrecisePkg,
            acc: &mut BTreeMap<EntryName, LockFileEntry>,
            namer: &mut LockFileNamer,
        ) -> Result<EntryName, Error> {
            let name = namer.name(id, pkg);
            let entry = LockFileEntry {
                precise: pkg.clone().into(),
                dependencies: resolution
                    .sorted_dependencies(pkg)
                    .into_iter()
                    .map(|(id, (dep, precise))| {
                        let spec = match dep {
                            Dependency::Git(g) => Some(g.clone()),
                            Dependency::Path(_) => None,
                            Dependency::Index(_) => None,
                        };
                        let entry = LockFileDep {
                            name: namer.name(id.label(), &precise),
                            spec,
                        };
                        (id.label().to_owned(), entry)
                    })
                    .collect(),
            };

            // Only recurse if this is the first time we've encountered this precise package.
            if acc.insert(name.clone(), entry).is_none() {
                for (id, (_dep, precise)) in resolution.sorted_dependencies(pkg) {
                    collect_packages(resolution, id.label(), &precise, acc, namer)?;
                }
            }
            Ok(name)
        }

        let mut acc = BTreeMap::new();

        let mut dependencies = BTreeMap::new();
        let mut namer = LockFileNamer::default();
        for (id, dep) in manifest.sorted_dependencies() {
            let pkg = resolution.precise(dep);
            let name = collect_packages(resolution, id, &pkg, &mut acc, &mut namer)?;
            let spec = match dep {
                Dependency::Git(g) => Some(g.clone()),
                Dependency::Path(_) => None,
                Dependency::Index(_) => None,
            };
            let entry = LockFileDep { name, spec };
            dependencies.insert(id.to_owned(), entry);
        }

        Ok(LockFile {
            dependencies,
            packages: acc,
        })
    }

    /// Read a lock file from disk.
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, Error> {
        let path = path.as_ref();
        let contents = std::fs::read_to_string(path).with_path(path)?;
        serde_json::from_str(&contents).map_err(|error| Error::LockFileDeserialization {
            path: path.to_owned(),
            error,
        })
    }

    /// Write out this lock file to the filesystem.
    pub fn write(&self, path: &Path) -> Result<(), Error> {
        // unwrap: serde_json serialization fails if the derived `Serialize`
        // trait fails (which it shouldn't), or if there's a map with
        // non-string keys (all our maps have `Ident` keys).
        let serialized_lock = serde_json::to_string_pretty(self).unwrap();
        std::fs::write(path, serialized_lock).with_path(path)?;
        Ok(())
    }

    pub fn dependency<'a>(
        &'a self,
        entry: Option<&'a LockFileEntry>,
        name: &str,
    ) -> Option<&'a LockFileDep> {
        match entry {
            None => self.dependencies.get(name),
            Some(entry) => entry.dependencies.get(name),
        }
    }
}

/// A precise package version, in a format suitable for putting into a lockfile.
///
/// This is like `crate::Precise`, but doesn't store paths. (We remember that there
/// was a path, but not what it was.)
#[serde_with::serde_as]
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub enum LockPrecisePkg {
    Git {
        // We use `Precise` for a few different purposes, and not all of them need the url. (For
        // resolution, for example, we could consider two git deps equal if they have the same id
        // even if they came from different sources.) However, the lockfile should have a repo url in
        // it, because it allows us to fetch the package if it isn't available, and it allows us to
        // check if the locked dependency matches the manifest (which might only have the url).
        #[serde(with = "crate::serde_url")]
        url: gix::Url,
        // Serialize/deserialize as hex strings.
        #[serde_as(as = "serde_with::DisplayFromStr")]
        id: ObjectId,
        path: PathBuf,
    },
    Path,
    Index {
        #[serde_as(as = "FromInto<crate::index::serialize::IdFormat>")]
        id: index::Id,
        version: SemVer,
    },
}

impl From<PrecisePkg> for LockPrecisePkg {
    fn from(p: PrecisePkg) -> Self {
        match p {
            // We don't currently prevent leaking local paths that point to git repos. Should we?
            PrecisePkg::Git { url, id, path } => LockPrecisePkg::Git { url, id, path },
            PrecisePkg::Path { .. } => LockPrecisePkg::Path,
            PrecisePkg::Index { id, version } => LockPrecisePkg::Index { id, version },
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct LockFileDep {
    pub name: EntryName,
    /// For git packages, we store their original git spec in the lock-file, so
    /// that if someone changes the spec in the manifest we can tell that we
    /// need to re-fetch the repo.
    ///
    /// Note that this goes in `LockFileDep` (which represents the dependency
    /// edge between two entries) rather than `LockFileEntry` because there can
    /// be multiple git specs that end up resolving to the same git revision.
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde(default)]
    pub spec: Option<GitDependency>,
}

/// The dependencies of a single package.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct LockFileEntry {
    pub precise: LockPrecisePkg,
    pub dependencies: BTreeMap<String, LockFileDep>,
}
