use std::{ffi::OsStr, fmt::Display, path::PathBuf, time::SystemTime};

use crate::{program::FieldPath, stdlib::StdlibModule};

/// Input data usually comes from files on the file system, but there are also
/// lots of cases where we want to synthesize other kinds of inputs.
///
/// Note that a `SourcePath` does not uniquely identify a cached input:
/// - Some functions (like [`Cache::add_file`]) add a new cached input unconditionally.
/// - [`Cache::get_or_add_file`] will add a new cached input at the same `SourcePath` if
///   the file on disk was updated.
///
/// The equality checking of `SourcePath` only affects [`Cache::replace_string`], which
/// overwrites any previous cached input with the same `SourcePath`.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum SourcePath {
    /// A file at the given path.
    ///
    /// Note that this does not need to be a real file on the filesystem: it could still
    /// be loaded from memory by, e.g, [`Cache::add_string`].
    ///
    /// This is the only `SourcePath` variant that can be resolved as the target
    /// of an import statement.
    Path(PathBuf),
    /// A subrange of a file at the given path.
    ///
    /// This is used by nls to analyze small parts of files that don't fully parse. The
    /// original file path is preserved, because it's needed for resolving imports.
    Snippet(PathBuf),
    Std(StdlibModule),
    Query,
    ReplInput(usize),
    ReplTypecheck,
    ReplQuery,
    CliFieldAssignment,
    Override(FieldPath),
    Generated(String),
    /// A source snippet generated for error reporting. Do not use this directly, instead use
    /// [`Cache::insert_generated`] to avoid potential conflicts.
    GeneratedByEvaluation(usize),
    Main,
    Stdin,
}

impl Display for SourcePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SourcePath::Path(p) | SourcePath::Snippet(p) => write!(f, "{}", p.display()),
            SourcePath::Std(StdlibModule::Std) => write!(f, "<stdlib/std.ncl>"),
            SourcePath::Std(StdlibModule::Internals) => write!(f, "<stdlib/internals.ncl>"),
            SourcePath::Query => write!(f, "<query>"),
            SourcePath::ReplInput(idx) => write!(f, "<repl-input-{idx}>"),
            SourcePath::ReplTypecheck => write!(f, "<repl-typecheck>"),
            SourcePath::ReplQuery => write!(f, "<repl-query>"),
            SourcePath::CliFieldAssignment => write!(f, "<cli-assignment>"),
            SourcePath::Override(path) => write!(f, "<override {path}>",),
            SourcePath::Generated(description) => write!(f, "<generated {}>", description),
            SourcePath::GeneratedByEvaluation(usize) => {
                write!(f, "<generated by evaluation {}>", usize)
            }
            SourcePath::Main => write!(f, "<main>"),
            SourcePath::Stdin => write!(f, "<stdin>"),
        }
    }
}

impl<'a> TryFrom<&'a SourcePath> for &'a OsStr {
    type Error = ();

    fn try_from(value: &'a SourcePath) -> Result<Self, Self::Error> {
        match value {
            SourcePath::Path(p) | SourcePath::Snippet(p) => Ok(p.as_os_str()),
            _ => Err(()),
        }
    }
}

/// Inputs can be read from the filesystem or from in-memory buffers (which come, e.g., from
/// the REPL, the standard library, or the language server).
///
/// Inputs read from the filesystem get auto-refreshed: if we try to access them again and
/// the on-disk file has changed, we read it again. Inputs read from in-memory buffers
/// are not auto-refreshed. If an in-memory buffer has a path that also exists in the
/// filesystem, we will not even check that file to see if it has changed.
#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Source {
    Filesystem {
        last_read: SystemTime,
        normalized_path: PathBuf,
        source: String,
    },
    Memory {
        source: String,
    },
}

impl AsRef<str> for Source {
    fn as_ref(&self) -> &str {
        match self {
            Source::Filesystem { source, .. } => source.as_ref(),
            Source::Memory { source } => source.as_ref(),
        }
    }
}
