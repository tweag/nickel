//! Source cache.

use crate::error::{Error, ImportError, ParseError, TypecheckError};
use crate::identifier::Ident;
use crate::parser::lexer::Lexer;
use crate::position::TermPos;
use crate::stdlib as nickel_stdlib;
use crate::term::{RichTerm, Term};
use crate::transformations::PendingImport;
use crate::typecheck::type_check;
use crate::{eval, parser, transformations};
use codespan::{FileId, Files};
use io::Read;
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
    terms: HashMap<FileId, (RichTerm, EntryState)>,
    /// The list of ids corresponding to the stdlib modules
    stdlib_ids: Option<Vec<FileId>>,
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
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Copy, Clone)]
pub enum EntryState {
    /// The term have just been parsed.
    Parsed,
    /// The term has been parsed and the imports resolved
    ImportsResolved,
    /// The term have been parsed and typechecked.
    Typechecked,
    /// The term have been parsed, possibly typechecked (but not necessarily), and transformed.
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
            terms: HashMap::new(),
            imports: HashMap::new(),
            stdlib_ids: None,
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
    pub fn parse(&mut self, file_id: FileId) -> Result<CacheOp<()>, ParseError> {
        if self.terms.contains_key(&file_id) {
            Ok(CacheOp::Cached(()))
        } else {
            self.terms
                .insert(file_id, (self.parse_nocache(file_id)?, EntryState::Parsed));
            Ok(CacheOp::Done(()))
        }
    }

    /// Parse a source and populate the corresponding entry in the cache, or do nothing if the
    /// entry has already been parsed.
    pub fn parse_multi(
        &mut self,
        file_id: FileId,
        format: InputFormat,
    ) -> Result<CacheOp<()>, ParseError> {
        if self.terms.contains_key(&file_id) {
            Ok(CacheOp::Cached(()))
        } else {
            self.terms.insert(
                file_id,
                (
                    self.parse_nocache_multi(file_id, format)?,
                    EntryState::Parsed,
                ),
            );
            Ok(CacheOp::Done(()))
        }
    }

    /// Parse a source without querying nor populating the cache.
    pub fn parse_nocache(&self, file_id: FileId) -> Result<RichTerm, ParseError> {
        self.parse_nocache_multi(file_id, InputFormat::Nickel)
    }

    /// Parse a source without querying nor populating the cache. Support multiple formats.
    pub fn parse_nocache_multi(
        &self,
        file_id: FileId,
        format: InputFormat,
    ) -> Result<RichTerm, ParseError> {
        let buf = self.files.source(file_id);

        match format {
            InputFormat::Nickel => {
                let t = parser::grammar::TermParser::new()
                    .parse(file_id, Lexer::new(&buf))
                    .map_err(|err| ParseError::from_lalrpop(err, file_id))?;
                Ok(t)
            }
            InputFormat::Json => serde_json::from_str(self.files.source(file_id))
                .map_err(|err| ParseError::from_serde_json(err, file_id, &self.files)),
            InputFormat::Yaml => serde_yaml::from_str(self.files.source(file_id))
                .map_err(|err| ParseError::from_serde_yaml(err, file_id)),
            InputFormat::Toml => toml::from_str(self.files.source(file_id))
                .map_err(|err| ParseError::from_toml(err, file_id, &self.files)),
        }
    }

    /// Typecheck an entry of the cache and update its state accordingly, or do nothing if the
    /// entry has already been typechecked. Require that the corresponding source has been parsed.
    pub fn typecheck(
        &mut self,
        file_id: FileId,
        global_env: &eval::Environment,
    ) -> Result<CacheOp<()>, CacheError<TypecheckError>> {
        if !self.terms.contains_key(&file_id) {
            return Err(CacheError::NotParsed);
        }

        // After self.parse(), the cache must be populated
        let (t, state) = self.terms.get(&file_id).unwrap();

        if *state > EntryState::Typechecked {
            Ok(CacheOp::Cached(()))
        } else if *state >= EntryState::Parsed {
            type_check(t, global_env, self)?;
            self.update_state(file_id, EntryState::Typechecked);
            Ok(CacheOp::Done(()))
        } else {
            panic!()
        }
    }

    /// Apply program transformations to an entry of the cache, and update its state accordingly,
    /// or do nothing if the entry has already been transformed. Require that the corresponding
    /// source has been parsed.
    /// if imports where resolved, perform transformations recursively on thoes.
    pub fn transform(&mut self, file_id: FileId) -> Result<CacheOp<()>, CacheError<ImportError>> {
        match self.entry_state(file_id) {
            Some(EntryState::Transformed) => Ok(CacheOp::Cached(())),
            Some(_) => {
                let (t, _) = self.terms.remove(&file_id).unwrap();
                // self.imports has to be cloned because self.transform  take self as mutable
                // TODO: is it good way to do?
                let imports = self.imports.clone();
                if let Some(imports) = imports.get(&file_id) {
                    for f in imports.iter() {
                        self.transform(*f)?;
                    }
                }
                let t = transformations::transform(t)?;
                self.terms.insert(file_id, (t, EntryState::Transformed));
                Ok(CacheOp::Done(()))
            }
            None => Err(CacheError::NotParsed),
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
            Some(EntryState::Transformed) => Ok(CacheOp::Cached(())),
            Some(_) => {
                let (mut t, _) = self.terms.remove(&file_id).unwrap();
                match t.term.as_mut() {
                    Term::Record(ref mut map, _) => {
                        let map_res: Result<HashMap<Ident, RichTerm>, ImportError> =
                            std::mem::replace(map, HashMap::new())
                                .into_iter()
                                .map(|(id, t)| {
                                    transformations::transform(t).map(|t_ok| (id.clone(), t_ok))
                                })
                                .collect();
                        *map = map_res?;
                    }
                    Term::RecRecord(ref mut map, ref mut dyn_fields, _) => {
                        let map_res: Result<HashMap<Ident, RichTerm>, ImportError> =
                            std::mem::replace(map, HashMap::new())
                                .into_iter()
                                .map(|(id, t)| {
                                    transformations::transform(t).map(|t_ok| (id.clone(), t_ok))
                                })
                                .collect();

                        let dyn_fields_res: Result<Vec<(RichTerm, RichTerm)>, ImportError> =
                            std::mem::replace(dyn_fields, Vec::new())
                                .into_iter()
                                .map(|(id_t, t)| {
                                    Ok((
                                        transformations::transform(id_t)?,
                                        transformations::transform(t)?,
                                    ))
                                })
                                .collect();

                        *map = map_res?;
                        *dyn_fields = dyn_fields_res?;
                    }
                    _ => panic!("cache::transform_inner(): not a record"),
                }

                self.terms.insert(file_id, (t, EntryState::Transformed));
                Ok(CacheOp::Done(()))
            }
            None => Err(CacheError::NotParsed),
        }
    }

    /// Resolve every imports of an entry of the cache, and update its state accordingly,
    /// or do nothing if the entry has already been transformed. Require that the corresponding
    /// source has been parsed.
    /// If resolved imports contains themself imports, resolve them and soo on.
    pub fn resolve_imports(
        &mut self,
        file_id: FileId,
    ) -> Result<CacheOp<()>, CacheError<ImportError>> {
        match self.entry_state(file_id) {
            Some(EntryState::ImportsResolved) => Ok(CacheOp::Cached(())),
            Some(_) => {
                let (t, _) = self.terms.remove(&file_id).unwrap();
                let (t, pending) = transformations::resolve_imports(t, self)?;
                for (t, id, path) in pending {
                    self.terms.insert(id, (t, EntryState::Parsed));
                    self.resolve_imports(id)?;
                }
                self.insert(file_id, t);
                Ok(CacheOp::Done(()))
            }
            None => Err(CacheError::NotParsed),
        }
    }

    /// Prepare a source for evaluation: parse it, resolve the imports,
    /// typecheck it and apply program transformations,
    /// if it was not already done.
    pub fn prepare(
        &mut self,
        file_id: FileId,
        global_env: &eval::Environment,
    ) -> Result<CacheOp<()>, Error> {
        let mut result = CacheOp::Cached(());

        if self.parse(file_id)? == CacheOp::Done(()) {
            result = CacheOp::Done(());
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

        let transform_res = self.transform(file_id).map_err(|cache_err| {
            cache_err.unwrap_error(
                "cache::prepare(): expected source to be parsed before transformations",
            )
        })?;
        if transform_res == CacheOp::Done(()) {
            result = CacheOp::Done(());
        };

        Ok(result)
    }

    /// Same as [`prepare`](#method.prepare), but do not use nor populate the cache. Used for
    /// inputs which are known to not be reused.
    /// In this case, the caller has to treat himself the imports:
    /// - typechecking
    /// - resolve imports performed insid these imports.
    /// - transform them.
    /// as needed.
    pub fn prepare_nocache(
        &mut self,
        file_id: FileId,
        global_env: &eval::Environment,
    ) -> Result<(RichTerm, Vec<PendingImport>), Error> {
        let term = self.parse_nocache(file_id)?;
        let (term, pending) = transformations::resolve_imports(term, self)?;
        type_check(&term, global_env, self)?;
        let term = transformations::transform(term)?;
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
    /// the WASM REPL error reporting code.
    #[cfg(feature = "repl-wasm")]
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
    pub fn terms_mut(&mut self) -> &mut HashMap<FileId, (RichTerm, EntryState)> {
        &mut self.terms
    }

    /// Update the state of an entry. Return the previous state.
    pub fn update_state(&mut self, file_id: FileId, new: EntryState) -> Option<EntryState> {
        self.terms
            .get_mut(&file_id)
            .map(|(_, old)| std::mem::replace(old, new))
    }

    /// Retrieve the state of an entry. Return `None` if the entry is not in the term cache,
    /// meaning that the content of the source has been loaded but has not been parsed yet.
    pub fn entry_state(&self, file_id: FileId) -> Option<EntryState> {
        self.terms.get(&file_id).map(|(_, state)| state).copied()
    }

    /// Retrieve a fresh clone of a cached term.
    pub fn get_owned(&self, file_id: FileId) -> Option<RichTerm> {
        self.terms.get(&file_id).map(|(t, _)| t.clone())
    }

    /// Retrieve a reference to a cached term.
    pub fn get_ref(&self, file_id: FileId) -> Option<&RichTerm> {
        self.terms.get(&file_id).map(|(t, _)| t)
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

        file_ids
            .iter()
            .try_for_each(|file_id| self.parse(*file_id).map(|_| ()))?;
        self.stdlib_ids.replace(file_ids);
        Ok(CacheOp::Done(()))
    }

    /// Typecheck the standard library. Currently only used in the test suite.
    pub fn typecheck_stdlib(&mut self) -> Result<CacheOp<()>, CacheError<TypecheckError>> {
        // We have a small bootstraping problem: to typecheck the global environment, we already
        // need a global evaluation environment, since stdlib parts may reference each other). But
        // typechecking is performed before program transformations, so this environment is not
        // final one. We have create a temporary global environment just for typechecking, which is dropped
        // right after. However:
        // 1. The stdlib is meant to stay relatively light.
        // 2. Typechecking the standard library ought to occur only during development. Once the
        //    stdlib is stable, we won't have typecheck it at every execution.
        if let Some(ids) = self.stdlib_ids.as_ref().cloned() {
            ids.iter()
                .try_fold(CacheOp::Cached(()), |cache_op, file_id| {
                    let global_env = self.mk_global_env().map_err(|err| match err {
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
    pub fn prepare_stdlib(&mut self) -> Result<(), Error> {
        self.load_stdlib()?;
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
        Ok(())
    }

    /// Generate a global environment from the list of `file_ids` corresponding to the standard
    /// library parts.
    pub fn mk_global_env(&self) -> Result<eval::Environment, CacheError<Void>> {
        if let Some(ids) = self.stdlib_ids.as_ref().cloned() {
            let mut env = eval::Environment::new();

            ids.iter().for_each(|file_id| {
                let result = eval::env_add_term(
                    &mut env,
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

            Ok(env)
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

    /// Insert an entry in the term cache after transformation.
    fn insert(&mut self, file_id: FileId, term: RichTerm);

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
                    self.imports.insert(parent_id, HashSet::new());
                    self.imports.get_mut(&parent_id).unwrap().insert(id);
                }
                id
            }
        };

        self.parse_multi(file_id, format)
            .map_err(|err| ImportError::ParseError(err, *pos))?;

        Ok((
            ResolvedTerm::FromFile {
                term: self.get_owned(file_id).unwrap(),
                path: path_buf,
            },
            file_id,
        ))
    }

    fn get(&self, file_id: FileId) -> Option<RichTerm> {
        self.terms.get(&file_id).map(|(term, state)| {
            debug_assert!(*state >= EntryState::ImportsResolved);
            term.clone()
        })
    }

    fn insert(&mut self, file_id: FileId, term: RichTerm) {
        self.terms
            .insert(file_id, (term, EntryState::ImportsResolved));
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

        fn insert(&mut self, _file_id: FileId, _term: RichTerm) {
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

            if self.term_cache.contains_key(&file_id) {
                Ok((ResolvedTerm::FromCache(), file_id))
            } else {
                let buf = self.files.source(file_id);
                let term = parser::grammar::TermParser::new()
                    .parse(file_id, Lexer::new(&buf))
                    .map_err(|e| ParseError::from_lalrpop(e, file_id))
                    .map_err(|e| ImportError::ParseError(e, *pos))?;
                self.insert(file_id, term.clone());
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
            self.term_cache.insert(file_id, term);
        }

        fn get(&self, file_id: FileId) -> Option<RichTerm> {
            self.term_cache.get(&file_id).cloned()
        }

        fn get_path(&self, file_id: FileId) -> &OsStr {
            self.files.name(file_id)
        }
    }
}
