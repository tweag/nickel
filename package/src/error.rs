use std::path::{Path, PathBuf};

use gix::ObjectId;
use nickel_lang_core::{error::INTERNAL_ERROR_MSG, files::Files, identifier::Ident};

use crate::{
    index,
    version::{SemVer, VersionReq},
    UnversionedDependency,
};

/// Errors related to package management.
pub enum Error {
    Io {
        path: Option<PathBuf>,
        error: std::io::Error,
    },
    LockFileDeserialization {
        path: PathBuf,
        error: serde_json::Error,
    },
    ManifestEval {
        package: Option<Ident>,
        files: Files,
        error: nickel_lang_core::error::Error,
    },
    NoProjectDir,
    RestrictedPath {
        /// The url of the git package that tried the bad import.
        package_url: Box<gix::Url>,
        /// The git id of the bad package.
        package_commit: ObjectId,
        /// The relative path of the bad package within its git repo.
        package_path: PathBuf,
        attempted: PathBuf,
        restriction: PathBuf,
    },
    /// There was an attempt to import a git repository from a relative path,
    /// where the package doing the importing wasn't a path package.
    RelativeGitImport {
        path: PathBuf,
    },
    /// There was some error interacting with a git repository.
    Git(nickel_lang_git::Error),
    InvalidUrl {
        msg: String,
    },
    InternalManifestError {
        path: PathBuf,
        msg: String,
    },
    /// There was an error persisting a temporary file.
    TempFilePersist {
        error: tempfile::PersistError,
    },
    /// A package in the index (or, hopefully, a package potentially destined for
    /// the index, because packages actually *in* the index should be validated)
    /// tried to depend on a path or git dependency.
    InvalidIndexDep {
        id: index::Id,
        dep: Box<UnversionedDependency>,
    },
    /// The package `id` wasn't found in the package index.
    UnknownIndexPackage {
        id: index::Id,
    },
    /// The requested version of package `id` wasn't found in the package index.
    UnknownIndexPackageVersion {
        id: index::Id,
        requested: SemVer,
        available: Vec<SemVer>,
    },
    /// While trying to insert a package in the index, we found that that same
    /// package and version was already present.
    DuplicateIndexPackageVersion {
        id: index::Id,
        version: SemVer,
    },
    /// We failed to serialize the index description of a package.
    PackageIndexSerialization {
        pkg: crate::index::Package,
        error: serde_json::Error,
    },
    /// We failed to deserialize the index description of a package.
    PackageIndexDeserialization {
        error: serde_json::Error,
    },
    /// A temporary error until we support version resolution.
    IndexPackageNeedsExactVersion {
        id: index::Id,
        req: VersionReq,
    },
    /// Some other error interacting with git.
    ///
    /// gix's errors are highly structured, and for many of them we only
    /// care about reporting them as strings.
    OtherGit(anyhow::Error),
}

impl std::error::Error for Error {}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Io { error, path } => {
                if let Some(path) = path {
                    write!(f, "{}: {error}", path.display())
                } else {
                    error.fmt(f)
                }
            }
            // Just a short and not-very-informative error. To write a better error message to
            // the terminal, use `program.report` like the cli does.
            Error::ManifestEval { package, .. } => {
                if let Some(package) = package {
                    write!(f, "error evaluating manifest for package {package}")
                } else {
                    write!(f, "error evaluating package manifest")
                }
            }
            Error::RestrictedPath {
                attempted,
                restriction,
                package_url,
                package_commit,
                package_path,
            } => {
                write!(
                    f,
                    "git package {package_url}@{package_commit}/{} tried to import path {}, but can only import from {}",
                    package_path.display(),
                    attempted.display(),
                    restriction.display()
                )
            }
            Error::Git(e) => e.fmt(f),
            Error::InvalidUrl { msg, .. } => {
                write!(f, "{}", msg)
            }
            Error::InternalManifestError { path, msg } => {
                write!(
                    f,
                    "internal error reading the manifest at {}: {msg}\n{INTERNAL_ERROR_MSG}",
                    path.display()
                )
            }
            Error::RelativeGitImport { path } => write!(
                f,
                "tried to import a relative git path ({}), but we have no root",
                path.display()
            ),
            Error::TempFilePersist { error } => error.fmt(f),
            Error::NoProjectDir => {
                write!(
                    f,
                    "failed to find a location for the nickel cache directory"
                )
            }
            Error::LockFileDeserialization { path, error } => {
                write!(f, "lock file {} is invalid: {error}", path.display())
            }
            Error::UnknownIndexPackage { id } => write!(f, "package {id} not found in the index"),
            Error::UnknownIndexPackageVersion {
                id,
                requested,
                available,
            } => {
                let available: Vec<_> = available.iter().map(|x| x.to_string()).collect();
                write!(
                    f,
                    "package {id}@{requested} not found in the index. Available versions: {}",
                    available.join(", ")
                )
            }
            Error::InvalidIndexDep { id, dep } => match dep.as_ref() {
                UnversionedDependency::Git(g) => write!(
                    f,
                    "package {id} depends on git package {}, so it cannot be put in the index",
                    g.url
                ),
                UnversionedDependency::Path(path) => write!(
                    f,
                    "package {id} depends on path package {}, so it cannot be put in the index",
                    path.display()
                ),
            },
            Error::DuplicateIndexPackageVersion { id, version } => {
                write!(f, "package {id}@{version} is already present in the index")
            }
            Error::PackageIndexSerialization { error, pkg } => {
                write!(f, "error serializing package. Failed package {pkg:?}, caused by {error}\n{INTERNAL_ERROR_MSG}")
            }
            Error::PackageIndexDeserialization { error } => {
                write!(
                    f,
                    "error deserializing package: {error}\n{INTERNAL_ERROR_MSG}"
                )
            }
            Error::IndexPackageNeedsExactVersion { id, req } => {
                write!(f, "index dependency {id} has version req {req}, but only precise versions are supported for now")
            }
            Error::OtherGit(error) => {
                write!(f, "{error}")
            }
        }
    }
}

pub trait ResultExt {
    type T;
    fn in_package(self, package: Ident) -> Result<Self::T, Error>;
}

impl<T> ResultExt for Result<T, Error> {
    type T = T;

    fn in_package(self, package: Ident) -> Result<Self::T, Error> {
        self.map_err(|e| match e {
            Error::ManifestEval { files, error, .. } => Error::ManifestEval {
                package: Some(package),
                files,
                error,
            },
            x => x,
        })
    }
}

pub trait IoResultExt {
    type T;
    fn with_path(self, path: impl AsRef<Path>) -> Result<Self::T, Error>;
    fn without_path(self) -> Result<Self::T, Error>;
}

impl<T> IoResultExt for Result<T, std::io::Error> {
    type T = T;
    fn with_path(self, path: impl AsRef<Path>) -> Result<T, Error> {
        self.map_err(|e| Error::Io {
            path: Some(path.as_ref().to_owned()),
            error: e,
        })
    }

    fn without_path(self) -> Result<T, Error> {
        self.map_err(|e| Error::Io {
            path: None,
            error: e,
        })
    }
}

impl From<nickel_lang_git::Error> for Error {
    fn from(e: nickel_lang_git::Error) -> Self {
        Self::Git(e)
    }
}

impl From<tempfile::PersistError> for Error {
    fn from(error: tempfile::PersistError) -> Self {
        Self::TempFilePersist { error }
    }
}

impl From<gix::url::parse::Error> for Error {
    fn from(e: gix::url::parse::Error) -> Self {
        Self::InvalidUrl { msg: e.to_string() }
    }
}

impl From<gix::open::Error> for Error {
    fn from(e: gix::open::Error) -> Self {
        Self::OtherGit(e.into())
    }
}

impl From<gix::reference::head_tree_id::Error> for Error {
    fn from(e: gix::reference::head_tree_id::Error) -> Self {
        Self::OtherGit(e.into())
    }
}
