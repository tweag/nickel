//! Source cache.

use crate::error::{Error, ImportError, ParseError, TypecheckError};
use crate::identifier::Ident;
use crate::parser::lexer::Lexer;
use crate::position::RawSpan;
use crate::term::{RichTerm, Term};
use crate::typecheck::type_check;
use crate::{eval, parser, transformations};
use codespan::{FileId, Files};
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::result::Result;

/// File and terms cache.
///
/// Manage a file database, which stores a set of sources, and the corresponding terms. Terms
/// possibly undergo typechecking and program transformation. The state of each entry (that is,
/// what operations have been performed on this term) is stored in an
/// [`EntryState`](./enum.EntryState.html).
pub struct Cache {
    /// The content of the program source plus potential imports
    /// made by the program (imports will be supported in a near future).
    files: Files<String>,
    /// The file ids stored in the database indexed by the source name.
    file_ids: HashMap<OsString, FileId>,
    /// Cache storing parsed terms corresponding to the entries of the file database.
    cache: HashMap<FileId, (RichTerm, EntryState)>,
}

/// The state of an entry of the term cache.
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Copy, Clone)]
pub enum EntryState {
    /// The term have just been parsed.
    Parsed,
    /// The term have been parsed and typechecked.
    Typechecked,
    /// The term have been parsed, possibly typechecked (but not necessarily), and transformed.
    Transformed,
}

/// The result of a cache operation, such as parsing, typechecking, etc. which can either have
/// performed actual work, or did nothing if the corresponding entry was already in at a later
/// stage.
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Copy, Clone)]
pub enum CacheOp {
    Done,
    Cached,
}

/// Wrapper around other error to indicate that typechecking or applying program transformations
/// failed because the source has not been parsed yet.
pub enum CacheError<E> {
    Error(E),
    NotParsed,
}

impl<E> From<E> for CacheError<E> {
    fn from(e: E) -> Self {
        CacheError::Error(e)
    }
}

impl<E> CacheError<E> {
    pub fn expect_err(self, msg: &str) -> E {
        match self {
            CacheError::Error(err) => err,
            CacheError::NotParsed => panic!("{}", msg),
        }
    }
}

/// Return status indicating if an import has been resolved from a file (first encounter), or was
/// retrieved from the cache.
///
/// See [`resolve`](./fn.resolve.html).
#[derive(Debug, PartialEq)]
pub enum ResolvedTerm {
    FromFile {
        term: RichTerm, /* the parsed term */
        path: PathBuf,  /* the loaded path */
    },
    FromCache(),
}

impl Cache {
    pub fn new() -> Self {
        Cache {
            files: Files::new(),
            file_ids: HashMap::new(),
            cache: HashMap::new(),
        }
    }

    // pub fn load_file() {
    //
    // }

    /// Load a new source.
    pub fn add_source<T, S>(&mut self, source_name: S, mut source: T) -> std::io::Result<FileId>
    where
        T: Read,
        S: Into<OsString> + Clone,
    {
        let mut buffer = String::new();
        source.read_to_string(&mut buffer)?;
        Ok(self.add_string(source_name, buffer))
    }

    pub fn add_string<S>(&mut self, source_name: S, s: String) -> FileId
    where
        S: Into<OsString> + Clone,
    {
        let id = self.files.add(source_name.clone(), s);
        self.file_ids.insert(source_name.into(), id);
        id
    }

    /// Parse a source file and populate the corresponding entry in the cache, or just get it from
    /// the term cache if it is there. Return a copy of the cached term.
    pub fn parse(&mut self, file_id: FileId) -> Result<CacheOp, ParseError> {
        if self.cache.contains_key(&file_id) {
            Ok(CacheOp::Cached)
        } else {
            let buf = self.files.source(file_id).clone();
            let t = parser::grammar::TermParser::new()
                .parse(file_id, Lexer::new(&buf))
                .map_err(|err| ParseError::from_lalrpop(err, file_id))?;
            self.cache.insert(file_id, (t, EntryState::Parsed));
            Ok(CacheOp::Done)
        }
    }

    /// Typecheck an entry of the cache, and update its state accordingly. Require that the
    /// corresponding source has been parsed.
    pub fn typecheck(
        &mut self,
        file_id: FileId,
        global_env: &eval::Environment,
    ) -> Result<CacheOp, CacheError<TypecheckError>> {
        if !self.cache.contains_key(&file_id) {
            return Err(CacheError::NotParsed);
        }

        // After self.parse(), the cache must be populated
        let (t, state) = self.cache.get(&file_id).unwrap();

        if *state > EntryState::Typechecked {
            Ok(CacheOp::Cached)
        } else if *state == EntryState::Parsed {
            type_check(t, global_env, self)?;
            self.update_state(file_id, EntryState::Typechecked);
            Ok(CacheOp::Done)
        } else {
            panic!()
        }
    }

    /// Apply program transformations to an entry of the cache, and update its state accordingly.
    /// Require that the corresponding source has at been at least parsed. The term may have been
    /// typechecked or not.
    pub fn transform(&mut self, file_id: FileId) -> Result<CacheOp, CacheError<ImportError>> {
        match self.entry_state(file_id) {
            Some(EntryState::Transformed) => Ok(CacheOp::Cached),
            Some(_) => {
                let (t, _) = self.cache.remove(&file_id).unwrap();
                let t = transformations::transform(t, self)?;
                self.cache.insert(file_id, (t, EntryState::Transformed));
                Ok(CacheOp::Done)
            }
            None => Err(CacheError::NotParsed),
        }
    }

    /// Apply program transformation to all the field of a record. Used to transform the standard
    /// library.
    pub fn transform_inner(&mut self, file_id: FileId) -> Result<CacheOp, CacheError<ImportError>> {
        match self.entry_state(file_id) {
            Some(EntryState::Transformed) => Ok(CacheOp::Cached),
            Some(_) => {
                let (mut t, _) = self.cache.remove(&file_id).unwrap();
                match t.term.as_mut() {
                    Term::Record(ref mut map) | Term::RecRecord(ref mut map) => {
                        let map_res: Result<HashMap<Ident, RichTerm>, ImportError> =
                            std::mem::replace(map, HashMap::new())
                                .into_iter()
                                // For the conversion to work, note that we need a Result<(Ident,RichTerm), E>
                                .map(|(id, t)| {
                                    transformations::transform(t, self)
                                        .map(|t_ok| (id.clone(), t_ok))
                                })
                                .collect();
                        std::mem::replace(map, map_res?);
                    }
                    _ => panic!("not a record"),
                }

                self.cache.insert(file_id, (t, EntryState::Transformed));
                Ok(CacheOp::Done)
            }
            None => Err(CacheError::NotParsed),
        }
    }

    /// Prepare a source for evaluation: parse it, typecheck it and apply program transformations,
    /// if it was not already done.
    pub fn prepare(
        &mut self,
        file_id: FileId,
        global_env: &eval::Environment,
    ) -> Result<CacheOp, Error> {
        let mut result = CacheOp::Cached;

        if self.parse(file_id)? == CacheOp::Done {
            result = CacheOp::Done;
        };

        let typecheck_res = self.typecheck(file_id, global_env).map_err(|cache_err| {
            cache_err.expect_err("cache::prepare(): expected source to be parsed")
        })?;
        if typecheck_res == CacheOp::Done {
            result = CacheOp::Done;
        };

        let transform_res = self.transform(file_id).map_err(|cache_err| {
            cache_err.expect_err("cache::prepare(): expected source to be parsed")
        })?;
        if transform_res == CacheOp::Done {
            result = CacheOp::Done;
        };

        Ok(result)
    }

    /// Retrieve the name of a source, given a `FileId`.
    pub fn name(&self, file_id: FileId) -> &OsStr {
        self.files.name(file_id)
    }

    /// Retrieve the id of a source, given a name. Note that files added via `add_file` must be
    /// referenced by their full normalied path (cf [`normalize_path`](fn.normalize_path.html)).
    pub fn file_id(&self, name: impl AsRef<OsStr>) -> Option<FileId> {
        self.file_ids.get(name.as_ref()).copied()
    }

    /// Get a mutable reference to the underlying files. Required by the `to_diagnostic` method of
    /// errors.
    pub fn files_mut<'a>(&'a mut self) -> &'a mut Files<String> {
        &mut self.files
    }

    pub fn update_state(&mut self, file_id: FileId, new: EntryState) -> Option<EntryState> {
        self.cache
            .get_mut(&file_id)
            .map(|(_, old)| std::mem::replace(old, new))
    }

    /// Retrieve the state of an entry. Return `None` if the entry is not in the term cache,
    /// meaning the content of the source has been loaded but has not been parsed yet.
    pub fn entry_state(&self, file_id: FileId) -> Option<EntryState> {
        self.cache.get(&file_id).map(|(_, state)| state).copied()
    }

    // /// Retrieve a reference to a cached term.
    // pub fn get_entry<'a>(&'a self, file_id: FileId) -> Option<&'a RichTerm> {
    //     self.cache.get(&file_id).map(|(t, _)| t)
    // }

    /// Retrieve a fresh clone of a cached term.
    pub fn get_owned(&self, file_id: FileId) -> Option<RichTerm> {
        self.cache.get(&file_id).map(|(t, _)| t.clone())
    }
}

/// Abstract the access to imported files and the import cache. Used by the evaluator, the
/// typechecker and at [import resolution](../transformations/import_resolution/index.html) phase.
///
/// The standard implementation use 2 caches, the file cache for raw contents and the term cache
/// for parsed contents, mirroring the 2 steps when resolving an import:
/// 1. When an import is encountered for the first time, the content of the corresponding file is
///    read and stored in the file cache (consisting of the file database plus a map between paths
///    and ids in the database). The content is parsed, and this term is queued somewhere so that
///    it can undergo the standard [transformations](../transformations/index.html) first, but is
///    not stored in the term cache yet.
/// 2. When it is finally processed, the term cache is updated with the transformed term.
pub trait ImportResolver {
    /// Resolve an import.
    ///
    /// Read and store the content of an import, put it in the file cache (or get it from there if
    /// it is cached), then parse it and return the corresponding term and file id.
    ///
    /// The term and the path are provided only if the import is processed for the first time.
    /// Indeed, at import resolution phase, the term of an import encountered for the first time is
    /// queued to be processed (e.g. having its own imports resolved). The path is needed to
    /// resolve nested imports relatively to this parent. Only after this processing the term is
    /// inserted back in the cache via [`insert`](#tymethod.insert). On the other hand, if it has
    /// been resolved before, it is already transformed in the cache and do not need further
    /// processing.
    fn resolve(
        &mut self,
        path: &OsStr,
        parent: Option<PathBuf>,
        pos: &Option<RawSpan>,
    ) -> Result<(ResolvedTerm, FileId), ImportError>;

    /// Insert an entry in the term cache after transformation.
    fn insert(&mut self, file_id: FileId, term: RichTerm);

    /// Get a resolved import from the term cache.
    fn get(&self, file_id: FileId) -> Option<RichTerm>;

    /// Get a file id from the file cache.
    fn get_id(&self, path: &OsStr, parent: Option<PathBuf>) -> Option<FileId>;
}

impl ImportResolver for Cache {
    fn resolve(
        &mut self,
        path: &OsStr,
        parent: Option<PathBuf>,
        pos: &Option<RawSpan>,
    ) -> Result<(ResolvedTerm, FileId), ImportError> {
        let (path_buf, normalized) = with_parent(path, parent);

        if let Some(file_id) = self.file_ids.get(&normalized) {
            return Ok((ResolvedTerm::FromCache(), *file_id));
        }

        let mut buffer = String::new();
        let file_id = fs::File::open(path_buf)
            .and_then(|mut file| file.read_to_string(&mut buffer))
            .map(|_| self.files.add(path, buffer))
            .map_err(|err| {
                ImportError::IOError(
                    path.to_string_lossy().into_owned(),
                    format!("{}", err),
                    pos.clone(),
                )
            })?;
        self.file_ids.insert(normalized, file_id.clone());

        self.parse(file_id)
            .map_err(|err| ImportError::ParseError(err, pos.clone()))?;
        Ok((
            ResolvedTerm::FromFile {
                term: self.get_owned(file_id).unwrap(),
                path: Path::new(path).to_path_buf(),
            },
            file_id,
        ))
    }

    fn get(&self, file_id: FileId) -> Option<RichTerm> {
        self.cache.get(&file_id).map(|(term, state)| {
            debug_assert!(*state == EntryState::Transformed);
            term.clone()
        })
    }

    fn get_id(&self, path: &OsStr, parent: Option<PathBuf>) -> Option<FileId> {
        let (_, normalized) = with_parent(path, parent);
        self.file_ids.get(&normalized).cloned()
    }

    fn insert(&mut self, file_id: FileId, term: RichTerm) {
        self.cache.insert(file_id, (term, EntryState::Transformed));
    }
}

/// Compute the path of a file relatively to a parent, and a string representation of the
/// normalized full path (see [`normalize_path`](./fn.normalize_path.html). If the path is absolute
/// or if the parent is `None`, the first component is the same as `Path::new(path).to_path_buf()`.
fn with_parent(path: impl AsRef<OsStr>, parent: Option<PathBuf>) -> (PathBuf, OsString) {
    let mut path_buf = parent.unwrap_or(PathBuf::new());
    path_buf.pop();
    path_buf.push(Path::new(path.as_ref()));
    let normalized =
        normalize_path(path_buf.as_path()).unwrap_or_else(|| path.as_ref().to_os_string());

    (path_buf, normalized)
}

/// Normalize the path of a file to uniquely identify names in the cache.
///
/// If an IO error occurs here, `None` is returned.
pub fn normalize_path(path: &Path) -> Option<OsString> {
    path.canonicalize()
        .ok()
        .map(|p_| p_.as_os_str().to_os_string())
}

/// Provide mockup import resolvers for testing purpose.
#[cfg(test)]
pub mod resolvers {
    use super::*;

    /// A dummy resolver that panics when asked to do something. Used to test code that contains no
    /// import.
    pub struct DummyResolver {}

    impl ImportResolver for DummyResolver {
        fn resolve(
            &mut self,
            _path: &OsStr,
            _parent: Option<PathBuf>,
            _pos: &Option<RawSpan>,
        ) -> Result<(ResolvedTerm, FileId), ImportError> {
            panic!("program::resolvers: dummy resolver should not have been invoked");
        }

        fn insert(&mut self, _file_id: FileId, _term: RichTerm) {
            panic!("program::resolvers: dummy resolver should not have been invoked");
        }

        fn get(&self, _file_id: FileId) -> Option<RichTerm> {
            panic!("program::resolvers: dummy resolver should not have been invoked");
        }

        fn get_id(&self, _path: &OsStr, _parent: Option<PathBuf>) -> Option<FileId> {
            panic!("program::resolvers: dummy resolver should not have been invoked");
        }
    }

    /// Resolve imports from a mockup file database. Used to test imports without accessing the
    /// file system. File name are stored as strings, and silently converted from/to `OsString`
    /// when needed: don't use this resolver with source code that import non UTF-8 paths.
    pub struct SimpleResolver {
        files: Files<String>,
        file_cache: HashMap<String, FileId>,
        term_cache: HashMap<FileId, Option<RichTerm>>,
    }

    impl SimpleResolver {
        pub fn new() -> SimpleResolver {
            SimpleResolver {
                files: Files::new(),
                file_cache: HashMap::new(),
                term_cache: HashMap::new(),
            }
        }

        /// Add a mockup file to available imports.
        pub fn add_source(&mut self, name: String, source: String) {
            let id = self.files.add(name.clone(), source);
            self.file_cache.insert(name, id);
        }
    }

    impl ImportResolver for SimpleResolver {
        fn resolve(
            &mut self,
            path: &OsStr,
            _parent: Option<PathBuf>,
            pos: &Option<RawSpan>,
        ) -> Result<(ResolvedTerm, FileId), ImportError> {
            let file_id = self
                .file_cache
                .get(path.to_string_lossy().as_ref())
                .map(|id| id.clone())
                .ok_or(ImportError::IOError(
                    path.to_string_lossy().into_owned(),
                    String::from("Import not found by the mockup resolver."),
                    pos.clone(),
                ))?;

            if self.term_cache.contains_key(&file_id) {
                Ok((ResolvedTerm::FromCache(), file_id))
            } else {
                self.term_cache.insert(file_id, None);
                let buf = self.files.source(file_id);
                let term = parser::grammar::TermParser::new()
                    .parse(file_id, Lexer::new(&buf))
                    .map_err(|e| ParseError::from_lalrpop(e, file_id))
                    .map_err(|e| ImportError::ParseError(e, pos.clone()))?;
                Ok((
                    ResolvedTerm::FromFile {
                        term,
                        path: PathBuf::new(),
                    },
                    file_id,
                ))
            }
        }

        fn insert(&mut self, file_id: FileId, term: RichTerm) {
            self.term_cache.insert(file_id, Some(term));
        }

        fn get(&self, file_id: FileId) -> Option<RichTerm> {
            self.term_cache
                .get(&file_id)
                .map(|opt| opt.as_ref())
                .flatten()
                .cloned()
        }

        fn get_id(&self, path: &OsStr, _parent: Option<PathBuf>) -> Option<FileId> {
            self.file_cache
                .get(path.to_string_lossy().as_ref())
                .copied()
        }
    }
}
