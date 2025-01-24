//! Functionality related to package manifests.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use nickel_lang_core::{
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
    lock::LockFile,
    realization::Realization,
    version::{FullSemVer, SemVer, SemVerPrefix, VersionReq},
    Dependency, GitDependency,
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
///
/// This is like [`crate::Dependency`], but we keep it separate so that
/// the deserialization format isn't tied to our internal representation.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Deserialize)]
enum DependencyFormat {
    Git(GitDependency),
    Path(String),
    // We don't support index dependencies in the package manager yet,
    // but it's in the manifest format so we keep this here and error out
    // on converting to a `crate::Dependency`.
    Index {
        package: String,
        version: VersionReq,
    },
}

impl TryFrom<DependencyFormat> for Dependency {
    type Error = Error;

    fn try_from(df: DependencyFormat) -> Result<Self, Error> {
        match df {
            DependencyFormat::Git(g) => Ok(Dependency::Git(g)),
            DependencyFormat::Path(p) => Ok(Dependency::Path { path: p.into() }),
            DependencyFormat::Index { .. } => Err(Error::IndexDep),
        }
    }
}

/// A package manifest file.
#[derive(Clone, Debug, PartialEq)]
pub struct ManifestFile {
    // The directory containing the manifest file.
    //
    // Every manifest that's loaded from the filesystem has a parent directory,
    // which is used to resolve relative path dependencies. We allow this to
    // be empty in case of in-memory manifests that don't need relative path
    // dependencies. (Currently, these are only used for testing, so maybe it
    // would be nicer to make this field mandatory.)
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
    /// Read a file from the filesystem, evaluate it as a nickel file, and return the evaluated manifest.
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, Error> {
        let path = path.as_ref();
        let prog =
            Program::new_from_file(path, std::io::stderr(), NullReporter {}).with_path(path)?;
        let mut ret = ManifestFile::from_prog(path, prog)?;
        ret.parent_dir = path.parent().map(Path::to_owned);
        Ok(ret)
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

    /// Returns the location of the
    pub fn default_lockfile_path(&self) -> Result<PathBuf, Error> {
        let parent_dir = self.parent_dir.as_ref().ok_or(Error::NoManifestParent)?;
        Ok(parent_dir.join("package.ncl.lock"))
    }

    /// Checks whether the given lock file is up to date enough for this manifest.
    ///
    /// "Up to date" means that every dependency in the manifest is matched
    /// by a compatible version in the lock file. We don't, for example, check
    /// whether git deps are fully up-to-date.
    pub fn is_lock_file_up_to_date(&self, lock_file: &LockFile) -> bool {
        self.dependencies.iter().all(|(name, src)| {
            lock_file
                .dependencies
                .get(name.label())
                .map_or(false, |entry| src.matches(&entry))
        })
    }

    /// Checks if this manifest already has an up-to-date lockfile.
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
                .map(|(k, v)| Ok((k, Dependency::try_from(v)?)))
                .collect::<Result<_, Error>>()?,
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
