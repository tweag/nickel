//! Source cache.

use crate::error::{Error, ImportError, ParseError, ParseErrors, TypecheckError};
use crate::parser::lexer::Lexer;
use crate::position::TermPos;
use crate::stdlib as nickel_stdlib;
use crate::term::{RichTerm, Term};
use crate::transform::import_resolution;
use crate::typecheck;
use crate::typecheck::{linearization::StubHost, type_check};
use crate::{eval, parser, transform};
use codespan::{FileId, Files};
use io::Read;
use std::collections::hash_map;
use std::collections::{HashMap, HashSet};
use std::ffi::{OsStr, OsString};
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::result::Result;
use std::time::SystemTime;
use void::Void;

/// Supported input formats.
#[derive(Clone, Copy, Eq, Debug, PartialEq)]
pub enum InputFormat {
    Nickel,
    Json,
    Yaml,
    Toml,
}

impl InputFormat {
    fn from_path_buf(path_buf: &Path) -> Option<InputFormat> {
        match path_buf.extension().and_then(OsStr::to_str) {
            Some("ncl") => Some(InputFormat::Nickel),
            Some("json") => Some(InputFormat::Json),
            Some("yaml") | Some("yml") => Some(InputFormat::Yaml),
            Some("toml") => Some(InputFormat::Toml),
            _ => None,
        }
    }
}

/// File and terms cache.
///
/// Manage a file database, which stores a set of sources (the original source code as string) and
/// the corresponding parsed terms. The storage comprises three elements:
///
/// - The file database, holding the string content of sources indexed by unique `FileId`
/// identifiers.
/// - The name-id table, associating source names for standalone inputs, or paths and timestamps
/// for files, to `FileId`s.
/// - The term cache, holding parsed terms indexed by `FileId`s.
///
/// Terms possibly undergo typechecking and program transformation. The state of each entry (that
/// is, the operations that have been performed on this term) is stored in an
/// [`EntryState`](./enum.EntryState.html).
#[derive(Debug, Clone)]
pub struct Cache {
    /// The content of the program sources plus imports.
    files: Files<String>,
    /// The name-id table, holding file ids stored in the database indexed by source names.
    file_ids: HashMap<OsString, NameIdEntry>,
    /// Map containing for each FileIDs a list of files they import.
    imports: HashMap<FileId, HashSet<FileId>>,
    /// The table storing parsed terms corresponding to the entries of the file database.
    terms: HashMap<FileId, CachedTerm>,
    /// The list of ids corresponding to the stdlib modules
    stdlib_ids: Option<Vec<FileId>>,

    #[cfg(debug_assertions)]
    /// Skip loading the stdlib, used for debugging purpose
    pub skip_stdlib: bool,
}

/// wrapping eval environment with typing environment
#[derive(Debug, Clone)]
pub struct GlobalEnv {
    /// The eval environment.
    pub eval_env: eval::Environment,
    /// The typing environment, counterpart of the eval environment for typechecking. Entries are
    /// [`TypeWrapper`](../typecheck/enum.TypeWrapper.html) for the ease of interacting with the
    /// typechecker, but there are not any unification variable in it.
    pub type_env: typecheck::Environment,
}

impl GlobalEnv {
    pub fn new() -> Self {
        GlobalEnv {
            eval_env: eval::Environment::new(),
            type_env: typecheck::Environment::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CachedTerm {
    pub term: RichTerm,
    pub state: EntryState,
    /// Any non fatal parse errors.
    pub parse_errs: ParseErrors,
}

/// Cache keys for sources.
///
/// A source can be either a snippet input by the user, in which case it is only identified by its
/// name in the name-id table, and a unique `FileId`. On the other hand, different versions of the
/// same file can coexist during the same session of the REPL. For this reason, an entry of the
/// name-id table of a file also stores the *modified at* timestamp, such that if a file is
/// imported or loaded again and has been modified in between, the entry is invalidated, the
/// content is loaded again and a new `FileId` is generated.
///
/// Note that in that case, invalidation just means that the `FileId` of a previous version is not
/// accessible anymore in the name-id table. However, terms that contain non evaluated imports or
/// source locations referring to previous version are still able access the corresponding source
/// or term which are kept respectively in `files` and `cache` by using the corresponding `FileId`.
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Copy, Clone)]
pub struct NameIdEntry {
    id: FileId,
    timestamp: Option<SystemTime>,
}

/// The state of an entry of the term cache.
///
/// # Imports
///
/// Usually, when applying a procedure to an entry (typechecking, transformation, ...), we process
/// all of its transitive imports as well. We start by processing the entry, updating the state to
/// `XXXing` (ex: `Typechecking`) upon success. Only when all the imports have been successfully
/// processed, the state is updated to `XXXed` (ex: `Typechecked`).
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Copy, Clone)]
pub enum EntryState {
    /// The term have just been parsed.
    Parsed,
    /// The imports of the entry have been resolved, and the imports of its (transitive) imports are being resolved.
    ImportsResolving,
    /// The imports of the entry and its transitive dependencies has been resolved.
    ImportsResolved,
    /// The entry have been typechecked, and its (transitive) imports are being typechecked.
    Typechecking,
    /// The entry and its transitive imports have been typechecked.
    Typechecked,
    /// The entry have been transformed, and its (transitive) imports are being transformed.
    Transforming,
    /// The entry and its transitive imports have been transformed.
    Transformed,
}

/// The result of a cache operation, such as parsing, typechecking, etc. which can either have
/// performed actual work, or have done nothing if the corresponding entry was already at a later
/// stage.
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Copy, Clone)]
pub enum CacheOp<T> {
    Done(T),
    Cached(T),
}

impl<T> CacheOp<T> {
    pub fn inner(self: CacheOp<T>) -> T {
        match self {
            CacheOp::Done(t) | CacheOp::Cached(t) => t,
        }
    }
}

/// Wrapper around other errors to indicate that typechecking or applying program transformations
/// failed because the source has not been parsed yet.
#[derive(Eq, PartialEq, Debug, Clone)]
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
    pub fn unwrap_error(self, msg: &str) -> E {
        match self {
            CacheError::Error(err) => err,
            CacheError::NotParsed => panic!("{}", msg),
        }
    }
}

/// Return status indicating if an import has been resolved from a file (first encounter), or was
/// retrieved from the cache.
///
/// See [`resolve`](./trait.ImportResolver.html#tymethod.resolve).
#[derive(Debug, PartialEq)]
pub enum ResolvedTerm {
    FromFile {
        path: PathBuf, /* the loaded path */
    },
    FromCache(),
}

impl Cache {
    pub fn new() -> Self {
        Cache {
            files: Files::new(),
            file_ids: HashMap::new(),
            terms: HashMap::new(),
            imports: HashMap::new(),
            stdlib_ids: None,

            #[cfg(debug_assertions)]
            skip_stdlib: false,
        }
    }

    /// Load a file in the file database. Do not insert an entry in the name-id table.
    fn load_file(&mut self, path: impl Into<OsString>) -> io::Result<FileId> {
        let path = path.into();
        let mut buffer = String::new();
        fs::File::open(&path)
            .and_then(|mut file| file.read_to_string(&mut buffer))
            .map(|_| self.files.add(path, buffer))
    }

    /// Same as [`add_file`](#method.add_file), but assume that the path is already normalized,
    /// and take the timestamp as a parameter.
    fn add_file_(
        &mut self,
        path: impl Into<OsString>,
        timestamp: SystemTime,
    ) -> io::Result<FileId> {
        let path = path.into();
        let file_id = self.load_file(path.clone())?;
        self.file_ids.insert(
            path,
            NameIdEntry {
                id: file_id,
                timestamp: Some(timestamp),
            },
        );
        Ok(file_id)
    }

    /// Load a file and add it to the name-id table.
    ///
    /// Use the normalized path and the *modified at* timestamp as the name-id table entry. Do not
    /// check if a source with the same name as the normalized path of the file and the same
    /// *modified at* timestamp already exists: if it is the case, this one will override the old
    /// entry in the name-id table.
    pub fn add_file(&mut self, path: impl Into<OsString>) -> io::Result<FileId> {
        let path = path.into();
        let timestamp = timestamp(&path)?;
        let normalized = normalize_path(PathBuf::from(&path).as_path())?;
        self.add_file_(normalized, timestamp)
    }

    /// Same as [`get_or_add_file`](#method.get_or_add_file), but assume that the path is already
    /// normalized, and take the timestamp as a parameter.
    fn get_or_add_file_(
        &mut self,
        path: impl Into<OsString>,
        timestamp: SystemTime,
    ) -> io::Result<CacheOp<FileId>> {
        let path = path.into();
        if let Some(file_id) = self.id_of_file_(&path, timestamp) {
            Ok(CacheOp::Cached(file_id))
        } else {
            self.add_file_(path, timestamp).map(CacheOp::Done)
        }
    }

    /// Try to retrieve the id of a file from the cache, using the normalized path and comparing
    /// timestamps. If it was not in cache, add it as a new entry.
    pub fn get_or_add_file(&mut self, path: impl Into<OsString>) -> io::Result<CacheOp<FileId>> {
        let path = path.into();
        let timestamp = timestamp(&path)?;
        let normalized = normalize_path(PathBuf::from(&path).as_path())?;
        self.get_or_add_file_(normalized, timestamp)
    }

    /// Load a source and add it to the name-id table.
    ///
    /// Do not check if a source with the same name already exists: if it is the
    /// case, this one will override the old entry in the name-id table.
    pub fn add_source<T, S>(&mut self, source_name: S, mut source: T) -> io::Result<FileId>
    where
        T: Read,
        S: Into<OsString>,
    {
        let mut buffer = String::new();
        source.read_to_string(&mut buffer)?;
        Ok(self.add_string(source_name, buffer))
    }

    /// Load a new source as a string and add it to the name-id table.
    ///
    /// Do not check if a source with the same name already exists: if it is the case, this one
    /// will override the old entry in the name-id table.
    pub fn add_string(&mut self, source_name: impl Into<OsString>, s: String) -> FileId {
        let source_name = source_name.into();
        let id = self.files.add(source_name.clone(), s);
        self.file_ids.insert(
            source_name,
            NameIdEntry {
                id,
                timestamp: None,
            },
        );
        id
    }

    /// Load a temporary source. If a source with the same name exists, clear the corresponding
    /// term cache entry, and destructively update not only the name-id table entry, but also the
    /// content of the source itself.
    ///
    /// Used to store intermediate short-lived generated snippets that needs to have a
    /// corresponding `FileId`, such as when querying or reporting errors.
    pub fn add_tmp(&mut self, source_name: impl Into<OsString>, s: String) -> FileId {
        let source_name = source_name.into();
        if let Some(file_id) = self.id_of(&source_name) {
            self.files.update(file_id, s);
            self.terms.remove(&file_id);
            file_id
        } else {
            let file_id = self.files.add(source_name.clone(), s);
            self.file_ids.insert(
                source_name,
                NameIdEntry {
                    id: file_id,
                    timestamp: None,
                },
            );
            file_id
        }
    }

    /// Parse a source and populate the corresponding entry in the cache, or do nothing if the
    /// entry has already been parsed.
    pub fn parse(&mut self, file_id: FileId) -> Result<CacheOp<ParseErrors>, ParseError> {
        if let Some(CachedTerm { parse_errs, .. }) = self.terms.get(&file_id) {
            Ok(CacheOp::Cached(parse_errs.clone()))
        } else {
            let (term, parse_errs) = self.parse_nocache(file_id)?;
            self.terms.insert(
                file_id,
                CachedTerm {
                    term,
                    state: EntryState::Parsed,
                    parse_errs: parse_errs.clone(),
                },
            );
            Ok(CacheOp::Done(parse_errs))
        }
    }

    /// Parse a source and populate the corresponding entry in the cache, or do nothing if the
    /// entry has already been parsed. Support multiple formats.
    pub fn parse_multi(
        &mut self,
        file_id: FileId,
        format: InputFormat,
    ) -> Result<CacheOp<ParseErrors>, ParseError> {
        if let Some(CachedTerm { parse_errs, .. }) = self.terms.get(&file_id) {
            Ok(CacheOp::Cached(parse_errs.clone()))
        } else {
            let (term, parse_errs) = self.parse_nocache_multi(file_id, format)?;
            self.terms.insert(
                file_id,
                CachedTerm {
                    term,
                    state: EntryState::Parsed,
                    parse_errs: parse_errs.clone(),
                },
            );
            Ok(CacheOp::Done(parse_errs))
        }
    }

    /// Parse a source without querying nor populating the cache.
    pub fn parse_nocache(&self, file_id: FileId) -> Result<(RichTerm, ParseErrors), ParseError> {
        self.parse_nocache_multi(file_id, InputFormat::Nickel)
    }

    /// Parse a source without querying nor populating the cache. Support multiple formats.
    pub fn parse_nocache_multi(
        &self,
        file_id: FileId,
        format: InputFormat,
    ) -> Result<(RichTerm, ParseErrors), ParseError> {
        let buf = self.files.source(file_id);

        match format {
            InputFormat::Nickel => {
                let (t, parse_errs) = parser::grammar::TermParser::new()
                    .parse_term_tolerant(file_id, Lexer::new(buf))?;

                Ok((t, parse_errs))
            }
            InputFormat::Json => serde_json::from_str(self.files.source(file_id))
                .map(|t| (t, ParseErrors::default()))
                .map_err(|err| ParseError::from_serde_json(err, file_id, &self.files)),
            InputFormat::Yaml => serde_yaml::from_str(self.files.source(file_id))
                .map(|t| (t, ParseErrors::default()))
                .map_err(|err| (ParseError::from_serde_yaml(err, file_id))),
            InputFormat::Toml => toml::from_str(self.files.source(file_id))
                .map(|t| (t, ParseErrors::default()))
                .map_err(|err| (ParseError::from_toml(err, file_id, &self.files))),
        }
    }

    /// Typecheck an entry of the cache and update its state accordingly, or do nothing if the
    /// entry has already been typechecked. Require that the corresponding source has been parsed.
    /// If the source contains imports, recursively typecheck on the imports too.
    pub fn typecheck(
        &mut self,
        file_id: FileId,
        global_env: &typecheck::Environment,
    ) -> Result<CacheOp<()>, CacheError<TypecheckError>> {
        match self.terms.get(&file_id) {
            Some(CachedTerm { state, .. }) if *state >= EntryState::Typechecked => {
                Ok(CacheOp::Cached(()))
            }
            Some(CachedTerm { term, state, .. }) if *state >= EntryState::Parsed => {
                if *state < EntryState::Typechecking {
                    type_check(term, global_env, self, StubHost::<(), _>::new())?;
                    self.update_state(file_id, EntryState::Typechecking);
                }

                if let Some(imports) = self.imports.get(&file_id).cloned() {
                    for f in imports.into_iter() {
                        self.typecheck(f, global_env)?;
                    }
                }

                self.update_state(file_id, EntryState::Typechecked);
                Ok(CacheOp::Done(()))
            }
            _ => Err(CacheError::NotParsed),
        }
    }

    /// Apply program transformations to an entry of the cache, and update its state accordingly,
    /// or do nothing if the entry has already been transformed. Require that the corresponding
    /// source has been parsed.
    /// If the source contains imports, recursively perform transformations on the imports too.
    pub fn transform(&mut self, file_id: FileId) -> Result<CacheOp<()>, CacheError<()>> {
        match self.entry_state(file_id) {
            Some(state) if state >= EntryState::Transformed => Ok(CacheOp::Cached(())),
            Some(state) if state >= EntryState::Parsed => {
                if state < EntryState::Transforming {
                    let CachedTerm {
                        term, parse_errs, ..
                    } = self.terms.remove(&file_id).unwrap();
                    let term = transform::transform(term);
                    self.terms.insert(
                        file_id,
                        CachedTerm {
                            term,
                            state: EntryState::Transforming,
                            parse_errs,
                        },
                    );
                }

                if let Some(imports) = self.imports.get(&file_id).cloned() {
                    for f in imports.into_iter() {
                        self.transform(f)?;
                    }
                }

                self.update_state(file_id, EntryState::Transformed);
                Ok(CacheOp::Done(()))
            }
            _ => Err(CacheError::NotParsed),
        }
    }

    /// Apply program transformations to all the fields of a record.
    ///
    /// Used to transform stdlib modules and other records loaded in the environment, when using
    /// e.g. the `load` command of the REPL. If one just uses [`transform`](#method.transform), the
    /// share normal form transformation would add let bindings to a record entry `{ ... }`,
    /// turning it into `let %0 = ... in ... in { ... }`. But stdlib entries are required to be
    /// syntactically records.
    ///
    /// Note that this requirement may be relaxed in the future by e.g. evaluating stdlib entries
    /// before adding their fields to the global environment.
    ///
    /// # Preconditions
    ///
    /// - the entry must syntactically be a record (`Record` or `RecRecord`). Otherwise, this
    /// function panics
    pub fn transform_inner(
        &mut self,
        file_id: FileId,
    ) -> Result<CacheOp<()>, CacheError<ImportError>> {
        match self.entry_state(file_id) {
            Some(state) if state >= EntryState::Transformed => Ok(CacheOp::Cached(())),
            Some(_) => {
                let CachedTerm {
                    mut term,
                    state,
                    parse_errs,
                } = self.terms.remove(&file_id).unwrap();

                if state < EntryState::Transforming {
                    match term.term.as_mut() {
                        Term::Record(ref mut map, _) => {
                            let map_res = std::mem::take(map)
                                .into_iter()
                                .map(|(id, t)| (id, transform::transform(t)))
                                .collect();
                            *map = map_res;
                        }
                        Term::RecRecord(ref mut map, ref mut dyn_fields, _) => {
                            let map_res = std::mem::take(map)
                                .into_iter()
                                .map(|(id, t)| (id, transform::transform(t)))
                                .collect();

                            let dyn_fields_res = std::mem::take(dyn_fields)
                                .into_iter()
                                .map(|(id_t, t)| {
                                    (transform::transform(id_t), transform::transform(t))
                                })
                                .collect();

                            *map = map_res;
                            *dyn_fields = dyn_fields_res;
                        }
                        _ => panic!("cache::transform_inner(): not a record"),
                    }

                    self.terms.insert(
                        file_id,
                        CachedTerm {
                            term,
                            state: EntryState::Transforming,
                            parse_errs,
                        },
                    );
                }

                if let Some(imports) = self.imports.get(&file_id).cloned() {
                    for f in imports.into_iter() {
                        self.transform(f).map_err(|_| CacheError::NotParsed)?;
                    }
                }

                self.update_state(file_id, EntryState::Transformed);
                Ok(CacheOp::Done(()))
            }
            None => Err(CacheError::NotParsed),
        }
    }

    /// Resolve every imports of an entry of the cache, and update its state accordingly, or do
    /// nothing if the imports of the entry have already been resolved. Require that the
    /// corresponding source has been parsed.
    /// If resolved imports contain imports themselves, resolve them recursively.
    pub fn resolve_imports(
        &mut self,
        file_id: FileId,
    ) -> Result<CacheOp<()>, CacheError<ImportError>> {
        match self.entry_state(file_id) {
            Some(state) if state >= EntryState::ImportsResolved => Ok(CacheOp::Cached(())),
            Some(state) if state >= EntryState::Parsed => {
                if state < EntryState::ImportsResolving {
                    let CachedTerm {
                        term, parse_errs, ..
                    } = self.terms.remove(&file_id).unwrap();
                    let (term, pending) = import_resolution::resolve_imports(term, self)?;
                    self.terms.insert(
                        file_id,
                        CachedTerm {
                            term,
                            state: EntryState::ImportsResolving,
                            parse_errs,
                        },
                    );

                    for id in pending {
                        self.resolve_imports(id)?;
                    }
                } else {
                    let pending = self.imports.get(&file_id).cloned().unwrap_or_default();

                    for id in pending {
                        self.resolve_imports(id)?;
                    }
                }

                self.update_state(file_id, EntryState::ImportsResolved);
                Ok(CacheOp::Done(()))
            }
            _ => Err(CacheError::NotParsed),
        }
    }

    /// Prepare a source for evaluation: parse it, resolve the imports,
    /// typecheck it and apply program transformations,
    /// if it was not already done.
    pub fn prepare(
        &mut self,
        file_id: FileId,
        global_env: &typecheck::Environment,
    ) -> Result<CacheOp<()>, Error> {
        let mut result = CacheOp::Cached(());

        match self.parse(file_id)? {
            CacheOp::Done(e) | CacheOp::Cached(e) if !e.no_errors() => return Err(e.into()),
            CacheOp::Done(_) => {
                result = CacheOp::Done(());
            }
            _ => {}
        };

        let import_res = self.resolve_imports(file_id).map_err(|cache_err| {
            cache_err.unwrap_error(
                "cache::prepare(): expected source to be parsed before imports resolutions",
            )
        })?;
        if import_res == CacheOp::Done(()) {
            result = CacheOp::Done(());
        };

        let typecheck_res = self.typecheck(file_id, global_env).map_err(|cache_err| {
            cache_err
                .unwrap_error("cache::prepare(): expected source to be parsed before typechecking")
        })?;
        if typecheck_res == CacheOp::Done(()) {
            result = CacheOp::Done(());
        };

        let transform_res = self.transform(file_id).unwrap_or_else(|_| {
            panic!("cache::prepare(): expected source to be parsed before transformations",)
        });

        if transform_res == CacheOp::Done(()) {
            result = CacheOp::Done(());
        };

        Ok(result)
    }

    /// Same as [`prepare`](#method.prepare), but do not use nor populate the cache. Used for
    /// inputs which are known to not be reused.
    /// In this case, the caller has to process the imports themselves as needed:
    /// - typechecking
    /// - resolve imports performed inside these imports.
    /// - apply program transformations.
    pub fn prepare_nocache(
        &mut self,
        file_id: FileId,
        global_env: &typecheck::Environment,
    ) -> Result<(RichTerm, Vec<FileId>), Error> {
        let (term, errs) = self.parse_nocache(file_id)?;
        if errs.no_errors() {
            return Err(Error::ParseErrors(errs));
        }
        let (term, pending) = import_resolution::resolve_imports(term, self)?;
        type_check(&term, global_env, self, StubHost::<(), _>::new())?;
        let term = transform::transform(term);
        Ok((term, pending))
    }

    /// Retrieve the name of a source given an id.
    pub fn name(&self, file_id: FileId) -> &OsStr {
        self.files.name(file_id)
    }

    /// Retrieve the id of a source given a name.
    ///
    /// Note that files added via [`add_file`](#method.add_file) are indexed by their full
    /// normalized path (cf [`normalize_path`](./fn.normalize_path.html)). When querying file,
    /// rather use [`id_of_file`](#method.id_of_file).
    pub fn id_of(&self, name: impl AsRef<OsStr>) -> Option<FileId> {
        self.file_ids.get(name.as_ref()).map(|entry| entry.id)
    }

    /// Retrieve the id of a file given a path.
    ///
    /// This function normalizes the given path, search it in the name-id table, and check that the
    /// stored timestamp is the same as the current timestamp of the file. If normalization or
    /// metadata retrieval fails, or if the stored entry has no timestamps (it was added as a
    /// stand-alone source), `None` is returned.
    pub fn id_of_file(&self, path: impl AsRef<OsStr>) -> io::Result<Option<FileId>> {
        let normalized = normalize_path(PathBuf::from(path.as_ref()).as_path())?;
        let timestamp = timestamp(path)?;
        Ok(self.id_of_file_(normalized, timestamp))
    }

    /// Retrieve the id of a file given a path. Same as [`id_of_file`](#method.id_of_file), but assume
    /// that the given path is already normalized and take the timestamp as a parameter.
    fn id_of_file_(&self, path: impl AsRef<OsStr>, timestamp: SystemTime) -> Option<FileId> {
        self.file_ids
            .get(path.as_ref())
            .and_then(|entry| match entry.timestamp {
                Some(ts) if ts == timestamp => Some(entry.id),
                _ => None,
            })
    }

    /// Get a reference to the underlying files. Required by
    /// the WASM REPL error reporting code and LSP functions.
    pub fn files(&self) -> &Files<String> {
        &self.files
    }

    /// Get a mutable reference to the underlying files. Required by
    /// [`to_diagnostic`](../error/trait.ToDiagnostic.html#tymethod.to_diagnostic).
    pub fn files_mut(&mut self) -> &mut Files<String> {
        &mut self.files
    }

    /// Get a mutable reference to the cached term roots
    /// (used by the language server to invalidate previously parsed entries)
    pub fn terms_mut(&mut self) -> &mut HashMap<FileId, CachedTerm> {
        &mut self.terms
    }

    /// Get an immutable reference to the cached term roots
    /// (used by the language server to invalidate previously parsed entries)
    pub fn terms(&self) -> &HashMap<FileId, CachedTerm> {
        &self.terms
    }

    /// Update the state of an entry. Return the previous state.
    pub fn update_state(&mut self, file_id: FileId, new: EntryState) -> Option<EntryState> {
        self.terms
            .get_mut(&file_id)
            .map(|CachedTerm { state, .. }| std::mem::replace(state, new))
    }

    /// Retrieve the state of an entry. Return `None` if the entry is not in the term cache,
    /// meaning that the content of the source has been loaded but has not been parsed yet.
    pub fn entry_state(&self, file_id: FileId) -> Option<EntryState> {
        self.terms
            .get(&file_id)
            .map(|CachedTerm { state, .. }| state)
            .copied()
    }

    /// Retrieve a fresh clone of a cached term.
    pub fn get_owned(&self, file_id: FileId) -> Option<RichTerm> {
        self.terms
            .get(&file_id)
            .map(|CachedTerm { term, .. }| term.clone())
    }

    /// Retrieve a reference to a cached term.
    pub fn get_ref(&self, file_id: FileId) -> Option<&RichTerm> {
        self.terms.get(&file_id).map(|CachedTerm { term, .. }| term)
    }

    /// Load and parse the standard library in the cache.
    pub fn load_stdlib(&mut self) -> Result<CacheOp<()>, Error> {
        if self.stdlib_ids.is_some() {
            return Ok(CacheOp::Cached(()));
        }

        let file_ids: Vec<FileId> = nickel_stdlib::modules()
            .into_iter()
            .map(|(name, content)| self.add_string(OsString::from(name), String::from(content)))
            .collect();

        for file_id in file_ids.iter() {
            let errs = self.parse(*file_id)?.inner();
            if !errs.no_errors() {
                return Err(errs.into());
            }
        }
        self.stdlib_ids.replace(file_ids);
        Ok(CacheOp::Done(()))
    }

    /// Typecheck the standard library. Currently only used in the test suite.
    pub fn typecheck_stdlib(&mut self) -> Result<CacheOp<()>, CacheError<TypecheckError>> {
        // We have a small bootstraping problem: to typecheck the global environment, we already
        // need a global evaluation environment, since stdlib parts may reference each other. But
        // typechecking is performed before program transformations, so this environment is not
        // final one. We have create a temporary global environment just for typechecking, which is dropped
        // right after. However:
        // 1. The stdlib is meant to stay relatively light.
        // 2. Typechecking the standard library ought to occur only during development. Once the
        //    stdlib is stable, we won't have typecheck it at every execution.
        if let Some(ids) = self.stdlib_ids.as_ref().cloned() {
            ids.iter()
                .try_fold(CacheOp::Cached(()), |cache_op, file_id| {
                    let global_env = self.mk_types_env().map_err(|err| match err {
                        CacheError::NotParsed => CacheError::NotParsed,
                        CacheError::Error(_) => unreachable!(),
                    })?;
                    match self.typecheck(*file_id, &global_env)? {
                        done @ CacheOp::Done(()) => Ok(done),
                        _ => Ok(cache_op),
                    }
                })
        } else {
            Err(CacheError::NotParsed)
        }
    }

    /// Load, parse, and apply program transformations to the standard library. Do not typecheck
    /// for performance reason: this is done in the test suite.
    /// Return a global environment containing both eval and type environment. If you need only the
    /// type environment, use `load_stdlib()` then `mk_global_type` to avoid
    /// transformations and evaluation preparation.
    pub fn prepare_stdlib(&mut self) -> Result<GlobalEnv, Error> {
        #[cfg(debug_assertions)]
        if self.skip_stdlib {
            return Ok(GlobalEnv::new());
        }
        self.load_stdlib()?;
        let type_env = self.mk_types_env().unwrap();

        self.stdlib_ids
            .as_ref()
            .cloned()
            .expect("cache::prepare_stdlib(): stdlib has been loaded but stdlib_ids is None")
            .into_iter()
            .try_for_each(|file_id| self.transform_inner(file_id).map(|_| ()))
            .map_err(|cache_err| {
                cache_err
                    .unwrap_error("cache::prepare_stdlib(): expected standard library to be parsed")
            })?;
        let eval_env = self.mk_eval_env().unwrap();
        Ok(GlobalEnv { eval_env, type_env })
    }

    /// Generate a global typing environment from the list of `file_ids` corresponding to the standard
    /// library parts.
    pub fn mk_types_env(&self) -> Result<typecheck::Environment, CacheError<Void>> {
        let stdlib_terms_vec =
            self.stdlib_ids
                .as_ref()
                .map_or(Err(CacheError::NotParsed), |ids| {
                    Ok(ids
                        .iter()
                        .map(|file_id| {
                            self.get_owned(*file_id).expect(
                            "cache::mk_global_env(): can't build environment, stdlib not parsed",
                        )
                        })
                        .collect())
                })?;
        Ok(typecheck::Envs::mk_global(stdlib_terms_vec).unwrap())
    }

    /// Generate a global evaluation environment from the list of `file_ids` corresponding to the standard
    /// library parts.
    pub fn mk_eval_env(&self) -> Result<eval::Environment, CacheError<Void>> {
        if let Some(ids) = self.stdlib_ids.as_ref().cloned() {
            let mut eval_env = eval::Environment::new();
            ids.iter().for_each(|file_id| {
                let result = eval::env_add_term(
                    &mut eval_env,
                    self.get_owned(*file_id).expect(
                        "cache::mk_global_env(): can't build environment, stdlib not parsed",
                    ),
                );
                if let Err(eval::EnvBuildError::NotARecord(rt)) = result {
                     panic!(
                        "cache::load_stdlib(): expected the stdlib module {} to be a record, got {:?}",
                        self.name(*file_id).to_string_lossy().as_ref(),
                        rt
                     )
                }
            });
            Ok(eval_env)
        } else {
            Err(CacheError::NotParsed)
        }
    }
}

/// Abstract the access to imported files and the import cache. Used by the evaluator, the
/// typechecker and at [import resolution](../transformations/import_resolution/index.html) phase.
///
/// The standard implementation uses 2 caches, the file cache for raw contents and the term cache
/// for parsed contents, mirroring the 2 steps when resolving an import:
/// 1. When an import is encountered for the first time, the content of the corresponding file is
///    read and stored in the file cache (consisting of the file database plus a map between paths
///    and ids in the database, the name-id table). The content is parsed, stored in the term
///    cache, and queued somewhere so that it can undergo the standard
///    [transformations](../transformations/index.html) (including import resolution) later.
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
    /// inserted back in the cache via [`insert`](#method.insert). On the other hand, if it has
    /// been resolved before, it is already transformed in the cache and do not need further
    /// processing.
    fn resolve(
        &mut self,
        path: &OsStr,
        parent: Option<PathBuf>,
        pos: &TermPos,
    ) -> Result<(ResolvedTerm, FileId), ImportError>;

    /// Get a resolved import from the term cache.
    fn get(&self, file_id: FileId) -> Option<RichTerm>;

    fn get_path(&self, file_id: FileId) -> &OsStr;
}

impl ImportResolver for Cache {
    fn resolve(
        &mut self,
        path: &OsStr,
        parent: Option<PathBuf>,
        pos: &TermPos,
    ) -> Result<(ResolvedTerm, FileId), ImportError> {
        let path_buf = with_parent(path, parent.clone());
        let format = InputFormat::from_path_buf(&path_buf).unwrap_or(InputFormat::Nickel);
        let id_op = self.get_or_add_file(&path_buf).map_err(|err| {
            ImportError::IOError(
                path.to_string_lossy().into_owned(),
                format!("{}", err),
                *pos,
            )
        })?;
        let file_id = match id_op {
            CacheOp::Cached(id) => return Ok((ResolvedTerm::FromCache(), id)),
            CacheOp::Done(id) => {
                if let Some(parent) = parent {
                    let parent_id = self.id_of(parent).unwrap();
                    if let Some(imports) = self.imports.get_mut(&parent_id) {
                        imports.insert(id);
                    } else {
                        let mut imports = HashSet::new();
                        imports.insert(id);
                        self.imports.insert(parent_id, imports);
                    }
                }
                id
            }
        };

        // We ignore non fatal parse errors while importing.
        self.parse_multi(file_id, format)
            .map_err(|err| ImportError::ParseErrors(err.into(), *pos))?;

        Ok((ResolvedTerm::FromFile { path: path_buf }, file_id))
    }

    fn get(&self, file_id: FileId) -> Option<RichTerm> {
        self.terms
            .get(&file_id)
            .map(|CachedTerm { term, state, .. }| {
                debug_assert!(*state >= EntryState::ImportsResolved);
                term.clone()
            })
    }

    fn get_path(&self, file_id: FileId) -> &OsStr {
        self.files.name(file_id)
    }
}

/// Compute the path of a file relatively to a parent.
fn with_parent(path: &OsStr, parent: Option<PathBuf>) -> PathBuf {
    let mut path_buf = parent.unwrap_or_default();
    path_buf.pop();
    path_buf.push(Path::new(path));
    path_buf
}

/// Normalize the path of a file for unique identification in the cache.
///
/// If an IO error occurs here, `None` is returned.
pub fn normalize_path(path: &Path) -> io::Result<OsString> {
    path.canonicalize().map(|p_| p_.as_os_str().to_os_string())
}

/// Return the timestamp of a file. Return `None` if an IO error occurred.
pub fn timestamp(path: impl AsRef<OsStr>) -> io::Result<SystemTime> {
    fs::metadata(path.as_ref())?.modified()
}

/// Provide mockup import resolvers for testing purpose.
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
            _pos: &TermPos,
        ) -> Result<(ResolvedTerm, FileId), ImportError> {
            panic!("cache::resolvers: dummy resolver should not have been invoked");
        }

        fn get(&self, _file_id: FileId) -> Option<RichTerm> {
            panic!("cache::resolvers: dummy resolver should not have been invoked");
        }

        fn get_path(&self, _file_id: FileId) -> &OsStr {
            panic!("cache::resolvers: dummy resolver should not have been invoked");
        }
    }

    /// Resolve imports from a mockup file database. Used to test imports without accessing the
    /// file system. File name are stored as strings, and silently converted from/to `OsString`
    /// when needed: don't use this resolver with source code that import non UTF-8 paths.
    pub struct SimpleResolver {
        files: Files<String>,
        file_cache: HashMap<String, FileId>,
        term_cache: HashMap<FileId, RichTerm>,
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
            pos: &TermPos,
        ) -> Result<(ResolvedTerm, FileId), ImportError> {
            let file_id = self
                .file_cache
                .get(path.to_string_lossy().as_ref())
                .copied()
                .ok_or_else(|| {
                    ImportError::IOError(
                        path.to_string_lossy().into_owned(),
                        String::from("Import not found by the mockup resolver."),
                        *pos,
                    )
                })?;

            if let hash_map::Entry::Vacant(e) = self.term_cache.entry(file_id) {
                let buf = self.files.source(file_id);
                let term = parser::grammar::TermParser::new()
                    .parse_term(file_id, Lexer::new(buf))
                    .map_err(|e| ImportError::ParseErrors(e, *pos))?;
                e.insert(term);
                Ok((
                    ResolvedTerm::FromFile {
                        path: PathBuf::new(),
                    },
                    file_id,
                ))
            } else {
                Ok((ResolvedTerm::FromCache(), file_id))
            }
        }

        fn get(&self, file_id: FileId) -> Option<RichTerm> {
            self.term_cache.get(&file_id).cloned()
        }

        fn get_path(&self, file_id: FileId) -> &OsStr {
            self.files.name(file_id)
        }
    }
}
