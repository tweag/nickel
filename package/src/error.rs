use std::path::{Path, PathBuf};

use gix::ObjectId;
use nickel_lang_core::{files::Files, identifier::Ident};

use crate::{
    index::{self},
    version::SemVer,
    UnversionedPackage,
};

// TODO: implement IntoDiagnostic.
pub enum Error {
    Io {
        path: Option<PathBuf>,
        error: std::io::Error,
    },
    Serialize {
        error: serde_json::Error,
    },
    ManifestEval {
        package: Option<Ident>,
        files: Files,
        error: nickel_lang_core::error::Error,
    },
    NoPackageRoot {
        path: PathBuf,
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
    /// There was some error interacting with a git repository.
    Git(nickel_lang_git::Error),
    InvalidUrl {
        url: String,
        msg: String,
    },
    Resolution {
        msg: String,
    },
    InternalManifestError {
        path: PathBuf,
        msg: String,
    },
    /// A package in the index (or, hopefully, a package potentially destined for
    /// the index, because packages actually *in* the index should be validated)
    /// tried to depend on a path or git dependency.
    InvalidIndexDep {
        id: index::Id,
        dep: Box<UnversionedPackage>,
    },
    /// There was an error persisting a temporary file.
    TempFilePersist {
        error: tempfile::PersistError,
    },
    /// The package `id` wasn't found in the package index.
    UnknownIndexPackage {
        id: index::Id,
    },
    /// While trying to insert a package in the index, we found that that same
    /// package and version was already present.
    DuplicateIndexPackageVersion {
        id: index::Id,
        version: SemVer,
    },
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
            Error::InvalidUrl { url, msg } => {
                write!(f, "invalid url {}: {}", url, msg)
            }
            Error::InternalManifestError { path, msg } => {
                write!(
                    f,
                    "internal error reading the manifest at {}; this is a bug in nickel: {msg}",
                    path.display()
                )
            }
            Error::NoPackageRoot { path } => write!(
                f,
                "tried to import a relative path ({}), but we have no reference",
                path.display()
            ),
            Error::Resolution { msg } => write!(f, "version resolution failed: {msg}"),
            Error::TempFilePersist { error } => error.fmt(f),
            Error::UnknownIndexPackage { id } => write!(f, "package {id} not found in the index"),
            Error::InvalidIndexDep { id, dep } => match dep.as_ref() {
                UnversionedPackage::Git(g) => write!(
                    f,
                    "package {id} depends on git package {}, so it cannot be put in the index",
                    g.url
                ),
                UnversionedPackage::Path { path } => write!(
                    f,
                    "package {id} depends on path package {}, so it cannot be put in the index",
                    path.display()
                ),
            },
            Error::DuplicateIndexPackageVersion { id, version } => {
                write!(f, "package {id}@{version} is already present in the index")
            }
            Error::Serialize { error } => {
                write!(f, "serialization error: {error}")
            }
            Error::NoProjectDir => {
                write!(
                    f,
                    "failed to find a location for the nickel cache directory"
                )
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

impl From<serde_json::Error> for Error {
    fn from(error: serde_json::Error) -> Self {
        Self::Serialize { error }
    }
}
