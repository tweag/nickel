use std::path::{Path, PathBuf};

use nickel_lang_core::{eval::cache::CacheImpl, identifier::Ident, program::Program};

pub enum Error {
    Io {
        path: Option<PathBuf>,
        error: std::io::Error,
    },
    ManifestEval {
        package: Option<Ident>,
        program: Program<CacheImpl>,
        error: nickel_lang_core::error::Error,
    },
    RestrictedPath {
        package: Ident,
        attempted: PathBuf,
        restriction: PathBuf,
    },
    /// There was some error interacting with a git repository.
    Git {
        repo: String,
        msg: String,
    },
    InvalidUrl {
        url: String,
        msg: String,
    },
    InternalManifestError {
        msg: String,
    },
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
                package,
                attempted,
                restriction,
            } => {
                write!(
                    f,
                    "package {package} tried to import path {}, but can only import from {}",
                    attempted.display(),
                    restriction.display()
                )
            }
            Error::Git { repo, msg } => {
                write!(f, "error processing git repository at {}: {}", repo, msg)
            }
            Error::InvalidUrl { url, msg } => {
                write!(f, "invalid url {}: {}", url, msg)
            }
            Error::InternalManifestError { msg } => {
                write!(
                    f,
                    "internal error reading the manifest; this is a bug in nickel: {msg}"
                )
            }
        }
    }
}

pub trait ResultExt {
    type T;
    fn with_path(self, path: impl AsRef<Path>) -> Result<Self::T, Error>;
    fn without_path(self) -> Result<Self::T, Error>;
}

impl<T> ResultExt for Result<T, std::io::Error> {
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
