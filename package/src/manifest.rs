//! Functionality related to package manifests.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use nickel_lang_core::{
    cache::normalize_rel_path,
    error::NullReporter,
    eval::cache::CacheImpl,
    identifier::Ident,
    label::Label,
    program::Program,
    term::{make, RichTerm, RuntimeContract, Term},
};
use serde::Deserialize;

use crate::{
    config::Config,
    error::{Error, IoResultExt},
    index::{self, PackageIndex},
    lock::{LockFile, LockFileEntry},
    resolve::{self, Resolution},
    snapshot::Snapshot,
    version::{FullSemVer, SemVer, SemVerPrefix, VersionReq},
    Dependency, GitDependency, IndexDependency, PrecisePkg,
};

pub const MANIFEST_NAME: &str = "Nickel-pkg.ncl";
pub const LOCK_NAME: &str = "Nickel-pkg.lock";

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

    #[serde(default)]
    authors: Vec<String>,
    #[serde(default)]
    description: String,
    #[serde(default)]
    keywords: Vec<String>,
    #[serde(default)]
    license: String,
}

/// The deserialization format of a dependency in the manifest file.
///
/// This is like [`crate::Dependency`], but we keep it separate so that
/// the deserialization format isn't tied to our internal representation.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize)]
enum DependencyFormat {
    Git(GitDependencyFormat),
    Path(String),
    Index(IndexDependencyFormat),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize)]
/// A dependency that comes from the global package index.
///
/// This is currently identical to `IndexDependency`, but we keep
/// them separate so it's clear which things are part of our public
/// serialization API.
struct IndexDependencyFormat {
    package: index::Id,
    version: VersionReq,
}

impl TryFrom<IndexDependencyFormat> for IndexDependency {
    type Error = Error;
    fn try_from(i: IndexDependencyFormat) -> Result<IndexDependency, Error> {
        if matches!(i.version, VersionReq::Compatible(_)) {
            Err(Error::IndexPackageNeedsExactVersion {
                id: i.package,
                req: i.version,
            })
        } else {
            Ok(IndexDependency {
                id: i.package,
                version: i.version,
            })
        }
    }
}

/// Like GitDependency, but the url hasn't yet been parsed.
///
/// The nickel contract doesn't do url validation, so we do it as an extra step.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize)]
struct GitDependencyFormat {
    /// The url of the git repo, in any format understood by `gix`.
    /// For example, it can be a path, an https url, or an ssh url.
    pub url: String,
    #[serde(default, rename = "ref")]
    pub target: nickel_lang_git::Target,
    /// The path to the nickel package within the git repo, if it is not at the top level.
    #[serde(default)]
    pub path: PathBuf,
}

impl TryFrom<GitDependencyFormat> for GitDependency {
    type Error = Error;

    fn try_from(g: GitDependencyFormat) -> Result<Self, Self::Error> {
        Ok(GitDependency {
            url: gix::Url::try_from(g.url.as_str())?,
            target: g.target,
            path: g.path,
        })
    }
}

impl TryFrom<DependencyFormat> for Dependency {
    type Error = Error;

    fn try_from(df: DependencyFormat) -> Result<Self, Error> {
        match df {
            DependencyFormat::Git(g) => Ok(Dependency::Git(g.try_into()?)),
            DependencyFormat::Path(p) => Ok(Dependency::Path(p.into())),
            DependencyFormat::Index(i) => Ok(Dependency::Index(i.try_into()?)),
        }
    }
}

/// A package manifest file.
#[derive(Clone, Debug, PartialEq)]
pub struct ManifestFile {
    /// The directory containing the manifest file.
    ///
    /// This is used to resolve relative path dependencies.
    pub parent_dir: PathBuf,
    /// The name of the package.
    pub name: Ident,
    /// The version of the package.
    pub version: SemVer,
    /// The minimum nickel version supported by the package.
    pub minimal_nickel_version: SemVer,
    /// All the package's dependencies, and the local names that this package will use to refer to them.
    pub dependencies: HashMap<Ident, Dependency>,

    pub authors: Vec<String>,
    pub description: String,
    pub keywords: Vec<String>,
    pub license: String,
}

impl ManifestFile {
    /// Read a file from the filesystem, evaluate it as a nickel file, and return the evaluated manifest.
    ///
    /// Panics if the path has no parent.
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, Error> {
        let path = path.as_ref();
        let prog =
            Program::new_from_file(path, std::io::stderr(), NullReporter {}).with_path(path)?;
        ManifestFile::from_prog(path, prog)
    }

    /// Parse a file from UTF-8 data, evaluate it as a nickel file, and return the evaluated manifest.
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

    /// Returns the location of the lock file for this manifest.
    pub fn default_lockfile_path(&self) -> Result<PathBuf, Error> {
        Ok(self.parent_dir.join(LOCK_NAME))
    }

    /// Checks whether the given lock file is up to date enough for this manifest.
    ///
    /// "Up to date" means that every dependency in the manifest is matched
    /// by a compatible version in the lock file. We don't, for example, check
    /// whether git deps are fully up-to-date. We also don't check whether the
    /// lock file has stale entries that are no longer needed. Maybe we should?
    ///
    /// This function also recurses into path dependencies and checks whether they're
    /// up-to-date. It reads these path dependencies from the snapshot; we don't do our
    /// own I/O.
    pub fn is_lock_file_up_to_date(&self, snap: &Snapshot, lock_file: &LockFile) -> bool {
        fn up_to_date_rec(
            snap: &Snapshot,
            lock_file: &LockFile,
            manifest: &ManifestFile,
            manifest_path: &Path,
            parent_lock_entry: Option<&LockFileEntry>,
        ) -> bool {
            for (dep_name, dep) in manifest.dependencies.iter() {
                let Some(locked_dep) = lock_file.dependency(parent_lock_entry, dep_name.label())
                else {
                    return false;
                };
                let Some(dep_entry) = lock_file.packages.get(&locked_dep.name) else {
                    return false;
                };
                if !dep.matches(locked_dep, &dep_entry.precise) {
                    return false;
                }
                if let Dependency::Path(path) = dep {
                    let child_path = normalize_rel_path(&manifest_path.join(path));
                    let child_manifest = snap.manifest(&PrecisePkg::Path {
                        path: child_path.clone(),
                    });
                    if !up_to_date_rec(
                        snap,
                        lock_file,
                        child_manifest,
                        &child_path,
                        Some(dep_entry),
                    ) {
                        return false;
                    }
                }
            }

            true
        }

        up_to_date_rec(snap, lock_file, self, Path::new(""), None)
    }

    /// Checks if this manifest already has an up-to-date lockfile.
    fn find_lockfile(&self) -> Option<LockFile> {
        let lock_file = std::fs::read_to_string(self.default_lockfile_path().ok()?).ok()?;
        match serde_json::from_str(&lock_file) {
            Ok(f) => Some(f),
            Err(e) => {
                eprintln!("Found a lockfile, but it failed to parse: {e}");
                None
            }
        }
    }

    /// Determine the fully-resolved dependencies and write the lock-file to disk.
    ///
    /// Re-uses a lock file if there's one that's up-to-date. Otherwise, regenerates the lock file.
    pub fn lock(&self, config: Config) -> Result<(LockFile, Resolution), Error> {
        if let Some(lock) = self.find_lockfile() {
            // We haven't yet checked whether the lock-file is up-to-date, but we use
            // it to generate the snapshot anyway. This allows us to avoid unnecessary
            // git fetches even if unrelated parts of the lock need updating. (Snapshot
            // uses the lock file only to avoid git fetch.)
            let snap = Snapshot::new_with_lock(config.clone(), &self.parent_dir, self, &lock)?;

            if self.is_lock_file_up_to_date(&snap, &lock) {
                eprintln!("lock file up-to-date, keeping it");
                // TODO: we could avoid instantiating the index (which triggers a download if it doesn't exist)
                // if the dependency tree has no index packages.
                let index = PackageIndex::shared(config.clone())?;
                let resolution =
                    resolve::copy_from_lock(&lock, snap.clone(), index, config.clone())?;
                return Ok((lock, resolution));
            }
        }

        let path = self.default_lockfile_path()?;
        let (lock, snap) = self.regenerate_lock(config)?;
        lock.write(&path)?;
        Ok((lock, snap))
    }

    /// Regenerate the lock file, even if it already exists.
    pub fn regenerate_lock(&self, config: Config) -> Result<(LockFile, Resolution), Error> {
        let snap = self.snapshot_dependencies(config.clone())?;
        let has_index_pkg = snap.all_index_deps().next().is_some();
        let index = if has_index_pkg {
            // TODO: we could load the existing index first and check whether there the snapshot
            // references any unknown index packages. If not, we could avoid refreshing the index.
            match PackageIndex::refreshed(config.clone()) {
                Ok(i) => i,
                Err(e) => {
                    eprintln!("failed to refresh package index ({e}), trying with the old one");
                    PackageIndex::shared(config.clone())?
                }
            }
        } else {
            // TODO: we could avoid instantiating the index (which triggers a download if it doesn't exist)
            // if the dependency tree has no index packages.
            PackageIndex::shared(config.clone())?
        };
        // TODO: maybe refresh the index, if the snapshot has index dependencies?
        // Alternatively, we could try to resolve first and only hit the index if there was a reference
        // to an index package we don't know about.
        let resolution = resolve::resolve(self, snap, index, config)?;
        let lock = LockFile::new(self, &resolution)?;

        Ok((lock, resolution))
    }

    pub fn snapshot_dependencies(&self, config: Config) -> Result<Snapshot, Error> {
        Snapshot::new(config.clone(), &self.parent_dir, self)
    }

    // Convert from a `RichTerm` (that we assume was evaluated deeply).
    fn from_term(path: &Path, rt: &RichTerm) -> Result<Self, Error> {
        // This is only ever called with terms that have passed the `std.package.Manifest`
        // contract, so we can assume that they have the right fields.
        let ManifestFileFormat {
            name,
            version,
            minimal_nickel_version,
            dependencies,
            authors,
            description,
            keywords,
            license,
        } = ManifestFileFormat::deserialize(rt.clone()).map_err(|e| {
            Error::InternalManifestError {
                path: path.to_owned(),
                msg: e.to_string(),
            }
        })?;
        Ok(Self {
            parent_dir: path.parent().unwrap().to_owned(),
            name,
            version: version.into(),
            minimal_nickel_version: minimal_nickel_version.into(),
            dependencies: dependencies
                .into_iter()
                .map(|(k, v)| Ok((k, Dependency::try_from(v)?)))
                .collect::<Result<_, Error>>()?,
            authors,
            description,
            keywords,
            license,
        })
    }

    pub fn sorted_dependencies(&self) -> Vec<(&str, &Dependency)> {
        let mut ret: Vec<_> = self
            .dependencies
            .iter()
            .map(|(id, dep)| (id.label(), dep))
            .collect();
        ret.sort_by_key(|(id, _dep)| *id);
        ret
    }
}

#[cfg(test)]
mod tests {
    use core::str;

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
                parent_dir: PathBuf::default(),
                name: "foo".into(),
                version: SemVer::new(1, 0, 0),
                minimal_nickel_version: SemVer::new(1, 9, 0),
                dependencies: HashMap::default(),
                authors: Vec::new(),
                description: "hi".to_owned(),
                keywords: Vec::new(),
                license: String::new(),
            }
        );

        let manifest = ManifestFile::from_contents(
            r#"{name = "foo", version = "1.0.0-alpha1", minimal_nickel_version = "1.9.0", authors = [], description = "hi"}"#.as_bytes(),
        )
        .unwrap();
        assert_eq!(
            manifest,
            ManifestFile {
                parent_dir: PathBuf::default(),
                name: "foo".into(),
                version: SemVer {
                    major: 1,
                    minor: 0,
                    patch: 0,
                    pre: "alpha1".to_owned()
                },
                minimal_nickel_version: SemVer::new(1, 9, 0),
                dependencies: HashMap::default(),
                authors: Vec::new(),
                description: "hi".to_owned(),
                keywords: Vec::new(),
                license: String::new(),
            }
        )
    }

    // A bunch of example manifests where we just check that they serialize
    // without errors. No need to check the contents.
    #[test]
    fn successful_manifest() {
        let files = [
            r#"{name = "foo", version = "1.0.0-alpha1+build", minimal_nickel_version = "1.9.0", authors = []}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = ["Me <me@example.com>"]}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], keywords = ["key"]}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], license = "MIT"}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { dep = 'Path "dep" }}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { dep = 'Git { url = "https://example.com" }}}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { dep = 'Git { url = "https://example.com", ref = 'Head }}}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { dep = 'Git { url = "https://example.com", ref = 'Branch "b" }}}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { dep = 'Git { url = "https://example.com", ref = 'Tag "t" }}}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { dep = 'Git { url = "https://example.com", ref = 'Commit "0c0a82aa4a05cd84ba089bdba2e6a1048058f41b" }}}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { dep = 'Git { url = "https://example.com", path = "subdir" }}}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { dep = 'Index { package = "github/example/example", version = "=1.2.0" }}}"#.as_bytes(),
        ];

        for file in files {
            if let Err(e) = ManifestFile::from_contents(file) {
                panic!("contents {}, error {e}", str::from_utf8(file).unwrap());
            }
        }
    }

    // A bunch of example manifests that fail. We check that they fail with an
    // eval error (presumably because of failing the contract check) instead of
    // a deserialization error (which comes with a bad error message).
    //
    // Error messages are checked in the cli integration tests, so we don't check them here.
    #[test]
    fn failed_manifest() {
        let files = [
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0"}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", authors = []}"#.as_bytes(),
            r#"{name = "foo", minimal_nickel_version = "1.9.0", authors = []}"#.as_bytes(),
            r#"{name = version = "1.0.0", minimal_nickel_version = "1.9.0", authors = []}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], extra_field = 1}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [1]}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], keywords = [1]}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], license = ["MIT"]}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = [ 'Path "dep" ]}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { dep = 'Git { url = "https://example.com", ref = 'Branch }}}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { dep = 'Git { url = "https://example.com", ref = 'Tag 1 }}}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { dep = 'Git { url = "https://example.com", ref = 'Commit "0c0a82aa4" }}}"#.as_bytes(),

            // Invalid dependency names
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { "42" = 'Path "dep" }}"#.as_bytes(),
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { "has space" = 'Path "dep" }}"#.as_bytes(),

            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { dep = 'Index { package = "codeberg/example/example", version = "=1.2.0" }}}"#.as_bytes(),
            // This should become successful once we support version resolution
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { dep = 'Index { package = "github/example/example", version = "1.2.0" }}}"#.as_bytes(),
        ];

        for file in files {
            if let Err(e) = ManifestFile::from_contents(file) {
                if !matches!(
                    e,
                    Error::ManifestEval { .. } | Error::IndexPackageNeedsExactVersion { .. }
                ) {
                    panic!("contents {}, error {e}", str::from_utf8(file).unwrap());
                }
            } else {
                panic!("contents {} didn't error", str::from_utf8(file).unwrap());
            }
        }

        // Here's an exception to the rule that manifest errors are caught at eval time: the contract doesn't attempt
        // to validate urls.
        let file =
            r#"{name = "foo", version = "1.0.0", minimal_nickel_version = "1.9.0", authors = [], dependencies = { dep = 'Git { url = "htp s://example.com" }}}"#.as_bytes();
        let result = ManifestFile::from_contents(file);
        assert!(matches!(result, Err(Error::InvalidUrl { .. })));
    }
}
