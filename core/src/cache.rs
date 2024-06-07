//! Source cache.

use crate::error::{Error, ImportError, ParseError, ParseErrors, TypecheckError};
use crate::eval::cache::Cache as EvalCache;
use crate::eval::Closure;
#[cfg(feature = "nix-experimental")]
use crate::nix_ffi;
use crate::parser::{lexer::Lexer, ErrorTolerantParser};
use crate::position::TermPos;
use crate::program::FieldPath;
use crate::stdlib::{self as nickel_stdlib, StdlibModule};
use crate::term::array::Array;
use crate::term::record::{Field, RecordData};
use crate::term::{RichTerm, SharedTerm, Term};
use crate::transform::import_resolution;
use crate::typ::UnboundTypeVariableError;
use crate::typecheck::{self, type_check, Wildcards};
use crate::{eval, parser, transform};

use codespan::{FileId, Files};
use io::Read;
use serde::Deserialize;
use std::collections::hash_map;
use std::collections::{HashMap, HashSet};
use std::ffi::{OsStr, OsString};
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::result::Result;
use std::time::SystemTime;
use void::Void;

/// Supported input formats.
#[derive(Default, Clone, Copy, Eq, Debug, PartialEq)]
pub enum InputFormat {
    #[default]
    Nickel,
    Json,
    Yaml,
    Toml,
    #[cfg(feature = "nix-experimental")]
    Nix,
    Raw,
}

impl InputFormat {
    /// Returns an [InputFormat] based on the file extension of a path.
    pub fn from_path(path: &Path) -> Option<InputFormat> {
        match path.extension().and_then(OsStr::to_str) {
            Some("ncl") => Some(InputFormat::Nickel),
            Some("json") => Some(InputFormat::Json),
            Some("yaml") | Some("yml") => Some(InputFormat::Yaml),
            Some("toml") => Some(InputFormat::Toml),
            #[cfg(feature = "nix-experimental")]
            Some("nix") => Some(InputFormat::Nix),
            Some("txt") => Some(InputFormat::Raw),
            _ => None,
        }
    }
    /// Renturns an [InputFormat] based on the extension of a source path.
    pub fn from_source_path(source_path: &SourcePath) -> Option<InputFormat> {
        if let SourcePath::Path(p) = source_path {
            Self::from_path(p)
        } else {
            None
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
/// is, the operations that have been performed on this term) is stored in an [EntryState].
#[derive(Debug, Clone)]
pub struct Cache {
    /// The content of the program sources plus imports.
    files: Files<String>,
    file_paths: HashMap<FileId, SourcePath>,
    /// The name-id table, holding file ids stored in the database indexed by source names.
    file_ids: HashMap<SourcePath, NameIdEntry>,
    /// Map containing for each FileId a list of files they import (directly).
    imports: HashMap<FileId, HashSet<FileId>>,
    /// Map containing for each FileId a list of files importing them (directly).
    rev_imports: HashMap<FileId, HashSet<FileId>>,
    /// The table storing parsed terms corresponding to the entries of the file database.
    terms: HashMap<FileId, TermEntry>,
    /// The list of ids corresponding to the stdlib modules
    stdlib_ids: Option<HashMap<StdlibModule, FileId>>,
    /// The inferred type of wildcards for each `FileId`.
    wildcards: HashMap<FileId, Wildcards>,
    /// Whether processing should try to continue even in case of errors. Needed by the NLS.
    error_tolerance: ErrorTolerance,
    import_paths: Vec<PathBuf>,

    #[cfg(debug_assertions)]
    /// Skip loading the stdlib, used for debugging purpose
    pub skip_stdlib: bool,
}

/// The error tolerance mode used by the parser. The NLS needs to try to
/// continue even in case of errors.
#[derive(Debug, Clone)]
pub enum ErrorTolerance {
    Tolerant,
    Strict,
}

/// The different environments maintained during the REPL session for evaluation and typechecking.
#[derive(Debug, Clone)]
pub struct Envs {
    /// The eval environment.
    pub eval_env: eval::Environment,
    /// The typing context.
    pub type_ctxt: typecheck::Context,
}

impl Envs {
    pub fn new() -> Self {
        Envs {
            eval_env: eval::Environment::new(),
            type_ctxt: typecheck::Context::new(),
        }
    }
}

impl Default for Envs {
    fn default() -> Self {
        Self::new()
    }
}

/// An entry in the term cache. Stores the parsed term together with some metadata and state.
#[derive(Debug, Clone, PartialEq)]
pub struct TermEntry {
    pub term: RichTerm,
    pub state: EntryState,
    /// Any non fatal parse errors.
    pub parse_errs: ParseErrors,
}

/// Inputs can be read from the filesystem or from in-memory buffers (which come, e.g., from
/// the REPL, the standard library, or the language server).
///
/// Inputs read from the filesystem get auto-refreshed: if we try to access them again and
/// the on-disk file has changed, we read it again. Inputs read from in-memory buffers
/// are not auto-refreshed. If an in-memory buffer has a path that also exists in the
/// filesystem, we will not even check that file to see if it has changed.
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Copy, Clone)]
enum SourceKind {
    Filesystem(SystemTime),
    Memory,
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
    source: SourceKind,
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
    /// The imports of the entry have been resolved, and the imports of its (transitive) imports are
    /// being resolved.
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

pub enum EntryOrigin {}

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
    #[track_caller]
    pub fn unwrap_error(self, msg: &str) -> E {
        match self {
            CacheError::Error(err) => err,
            CacheError::NotParsed => panic!("{}", msg),
        }
    }
}

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
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
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

// [`Files`] needs to have an OsString for each file, so we synthesize names even for
// sources that don't have them. They don't need to be unique; they're just used for
// diagnostics.
impl From<SourcePath> for OsString {
    fn from(source_path: SourcePath) -> Self {
        match source_path {
            SourcePath::Path(p) | SourcePath::Snippet(p) => p.into(),
            SourcePath::Std(StdlibModule::Std) => "<stdlib/std.ncl>".into(),
            SourcePath::Std(StdlibModule::Internals) => "<stdlib/internals.ncl>".into(),
            SourcePath::Query => "<query>".into(),
            SourcePath::ReplInput(idx) => format!("<repl-input-{idx}>").into(),
            SourcePath::ReplTypecheck => "<repl-typecheck>".into(),
            SourcePath::ReplQuery => "<repl-query>".into(),
            SourcePath::CliFieldAssignment => "<cli-assignment>".into(),
            SourcePath::Override(path) => format!("<override {path}>",).into(),
            SourcePath::Generated(description) => format!("<generated {}>", description).into(),
        }
    }
}

/// Return status indicating if an import has been resolved from a file (first encounter), or was
/// retrieved from the cache.
///
/// See [ImportResolver::resolve].
#[derive(Debug, PartialEq, Eq)]
pub enum ResolvedTerm {
    FromFile {
        path: PathBuf, /* the loaded path */
    },
    FromCache,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SourceState {
    UpToDate(FileId),
    /// The source is stale because it came from a file on disk that has since been updated.
    /// The data is the timestamp of the new version of the file.
    Stale(SystemTime),
}

impl Cache {
    pub fn new(error_tolerance: ErrorTolerance) -> Self {
        Cache {
            files: Files::new(),
            file_ids: HashMap::new(),
            file_paths: HashMap::new(),
            terms: HashMap::new(),
            wildcards: HashMap::new(),
            imports: HashMap::new(),
            rev_imports: HashMap::new(),
            stdlib_ids: None,
            error_tolerance,
            import_paths: Vec::new(),

            #[cfg(debug_assertions)]
            skip_stdlib: false,
        }
    }

    pub fn add_import_paths<P>(&mut self, paths: impl Iterator<Item = P>)
    where
        PathBuf: From<P>,
    {
        self.import_paths.extend(paths.map(PathBuf::from));
    }

    /// Same as [Self::add_file], but assume that the path is already normalized, and take the
    /// timestamp as a parameter.
    fn add_file_(&mut self, path: PathBuf, timestamp: SystemTime) -> io::Result<FileId> {
        let contents = std::fs::read_to_string(&path)?;
        let file_id = self.files.add(&path, contents);
        self.file_paths
            .insert(file_id, SourcePath::Path(path.clone()));
        self.file_ids.insert(
            SourcePath::Path(path),
            NameIdEntry {
                id: file_id,
                source: SourceKind::Filesystem(timestamp),
            },
        );
        Ok(file_id)
    }

    /// Load a file from the filesystem and add it to the name-id table.
    ///
    /// Uses the normalized path and the *modified at* timestamp as the name-id table entry.
    /// Overrides any existing entry with the same name.
    pub fn add_file(&mut self, path: impl Into<OsString>) -> io::Result<FileId> {
        let path = path.into();
        let timestamp = timestamp(&path)?;
        let normalized = normalize_path(&path)?;
        self.add_file_(normalized, timestamp)
    }

    /// Try to retrieve the id of a file from the cache.
    ///
    /// If it was not in cache, try to read it from the filesystem and add it as a new entry.
    pub fn get_or_add_file(&mut self, path: impl Into<OsString>) -> io::Result<CacheOp<FileId>> {
        let path = path.into();
        let normalized = normalize_path(&path)?;
        match self.id_or_new_timestamp_of(path.as_ref())? {
            SourceState::UpToDate(id) => Ok(CacheOp::Cached(id)),
            SourceState::Stale(timestamp) => {
                self.add_file_(normalized, timestamp).map(CacheOp::Done)
            }
        }
    }

    /// Load a source and add it to the name-id table.
    ///
    /// Do not check if a source with the same name already exists: if it is the
    /// case, this one will override the old entry in the name-id table.
    pub fn add_source<T>(&mut self, source_name: SourcePath, mut source: T) -> io::Result<FileId>
    where
        T: Read,
    {
        let mut buffer = String::new();
        source.read_to_string(&mut buffer)?;
        Ok(self.add_string(source_name, buffer))
    }

    pub fn source(&self, id: FileId) -> &str {
        self.files.source(id)
    }

    /// Load a new source as a string and add it to the name-id table.
    ///
    /// Do not check if a source with the same name already exists: if it is the case, this one
    /// will override the old entry in the name-id table but the old `FileId` will remain valid.
    pub fn add_string(&mut self, source_name: SourcePath, s: String) -> FileId {
        let id = self.files.add(source_name.clone(), s);
        self.file_paths.insert(id, source_name.clone());
        self.file_ids.insert(
            source_name,
            NameIdEntry {
                id,
                source: SourceKind::Memory,
            },
        );
        id
    }

    /// Load a new source as a string, replacing any existing source with the same name.
    ///
    /// If there was a previous source with the same name, its `FileId` is reused and the
    /// cached term is deleted.
    ///
    /// Used to store intermediate short-lived generated snippets that needs to have a
    /// corresponding `FileId`, such as when querying or reporting errors.
    pub fn replace_string(&mut self, source_name: SourcePath, s: String) -> FileId {
        if let Some(file_id) = self.id_of(&source_name) {
            self.files.update(file_id, s);
            self.terms.remove(&file_id);
            file_id
        } else {
            let file_id = self.files.add(source_name.clone(), s);
            self.file_paths.insert(file_id, source_name.clone());
            self.file_ids.insert(
                source_name,
                NameIdEntry {
                    id: file_id,
                    source: SourceKind::Memory,
                },
            );
            file_id
        }
    }

    /// Parse a source and populate the corresponding entry in the cache, or do
    /// nothing if the entry has already been parsed. Support multiple formats.
    /// This function is always error tolerant, independently from `self.error_tolerant`.
    fn parse_lax(
        &mut self,
        file_id: FileId,
        format: InputFormat,
    ) -> Result<CacheOp<ParseErrors>, ParseError> {
        if let Some(TermEntry { parse_errs, .. }) = self.terms.get(&file_id) {
            Ok(CacheOp::Cached(parse_errs.clone()))
        } else {
            let (term, parse_errs) = self.parse_nocache_multi(file_id, format)?;
            self.terms.insert(
                file_id,
                TermEntry {
                    term,
                    state: EntryState::Parsed,
                    parse_errs: parse_errs.clone(),
                },
            );
            Ok(CacheOp::Done(parse_errs))
        }
    }

    /// Parse a source and populate the corresponding entry in the cache, or do
    /// nothing if the entry has already been parsed. Support multiple formats.
    /// This function is error tolerant if `self.error_tolerant` is `true`.
    pub fn parse(
        &mut self,
        file_id: FileId,
        format: InputFormat,
    ) -> Result<CacheOp<ParseErrors>, ParseErrors> {
        let result = self.parse_lax(file_id, format);

        match self.error_tolerance {
            ErrorTolerance::Tolerant => result.map_err(|err| err.into()),
            ErrorTolerance::Strict => match result? {
                CacheOp::Done(e) | CacheOp::Cached(e) if !e.no_errors() => Err(e),
                CacheOp::Done(_) => Ok(CacheOp::Done(ParseErrors::none())),
                CacheOp::Cached(_) => Ok(CacheOp::Cached(ParseErrors::none())),
            },
        }
    }

    /// Parse a source without querying nor populating the cache.
    pub fn parse_nocache(&self, file_id: FileId) -> Result<(RichTerm, ParseErrors), ParseError> {
        self.parse_nocache_multi(file_id, InputFormat::default())
    }

    /// Parse a source without querying nor populating the cache. Support multiple formats.
    pub fn parse_nocache_multi(
        &self,
        file_id: FileId,
        format: InputFormat,
    ) -> Result<(RichTerm, ParseErrors), ParseError> {
        let attach_pos = |t: RichTerm| -> RichTerm {
            let pos: TermPos =
                crate::position::RawSpan::from_codespan(file_id, self.files.source_span(file_id))
                    .into();
            t.with_pos(pos)
        };

        let buf = self.files.source(file_id);

        match format {
            InputFormat::Nickel => {
                let (t, parse_errs) =
                    // TODO: Should this really be parse_term if self.error_tolerant = false?
                    parser::grammar::TermParser::new().parse_tolerant(file_id, Lexer::new(buf))?;

                Ok((t, parse_errs))
            }
            InputFormat::Json => serde_json::from_str(self.files.source(file_id))
                .map(|t| (attach_pos(t), ParseErrors::default()))
                .map_err(|err| ParseError::from_serde_json(err, file_id, &self.files)),
            InputFormat::Yaml => {
                // YAML files can contain multiple documents. If there is only
                // one we transparently deserialize it. If there are multiple,
                // we deserialize the file as an array.
                let de = serde_yaml::Deserializer::from_str(self.files.source(file_id));
                let mut terms = de
                    .map(|de| {
                        RichTerm::deserialize(de)
                            .map(attach_pos)
                            .map_err(|err| (ParseError::from_serde_yaml(err, file_id)))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                if terms.is_empty() {
                    unreachable!(
                        "serde always produces at least one document, \
                        the empty string turns into `null`"
                    )
                } else if terms.len() == 1 {
                    Ok((
                        terms.pop().expect("we just checked the length"),
                        ParseErrors::default(),
                    ))
                } else {
                    Ok((
                        attach_pos(
                            Term::Array(
                                Array::new(Rc::from(terms.into_boxed_slice())),
                                Default::default(),
                            )
                            .into(),
                        ),
                        ParseErrors::default(),
                    ))
                }
            }
            InputFormat::Toml => toml::from_str(self.files.source(file_id))
                .map(|t| (attach_pos(t), ParseErrors::default()))
                .map_err(|err| (ParseError::from_toml(err, file_id))),
            #[cfg(feature = "nix-experimental")]
            InputFormat::Nix => {
                let json = nix_ffi::eval_to_json(self.files.source(file_id))
                    .map_err(|e| ParseError::from_nix(e.what(), file_id))?;
                serde_json::from_str(&json)
                    .map(|t| (attach_pos(t), ParseErrors::default()))
                    .map_err(|err| ParseError::from_serde_json(err, file_id, &self.files))
            }
            InputFormat::Raw => Ok((
                attach_pos(Term::Str(self.files.source(file_id).into()).into()),
                ParseErrors::default(),
            )),
        }
    }

    /// Typecheck an entry of the cache and update its state accordingly, or do nothing if the
    /// entry has already been typechecked. Require that the corresponding source has been parsed.
    /// If the source contains imports, recursively typecheck on the imports too.
    pub fn typecheck(
        &mut self,
        file_id: FileId,
        initial_ctxt: &typecheck::Context,
    ) -> Result<CacheOp<()>, CacheError<TypecheckError>> {
        match self.terms.get(&file_id) {
            Some(TermEntry { state, .. }) if *state >= EntryState::Typechecked => {
                Ok(CacheOp::Cached(()))
            }
            Some(TermEntry { term, state, .. }) if *state >= EntryState::Parsed => {
                if *state < EntryState::Typechecking {
                    let wildcards = type_check(term, initial_ctxt.clone(), self)?;
                    self.update_state(file_id, EntryState::Typechecking);
                    self.wildcards.insert(file_id, wildcards);

                    if let Some(imports) = self.imports.get(&file_id).cloned() {
                        for f in imports.into_iter() {
                            self.typecheck(f, initial_ctxt)?;
                        }
                    }

                    self.update_state(file_id, EntryState::Typechecked);
                }
                // The else case correponds to `EntryState::Typechecking`. There is nothing to do:
                // cf (grep for) [transitory_entry_state]
                Ok(CacheOp::Done(()))
            }
            _ => Err(CacheError::NotParsed),
        }
    }

    /// Apply program transformations to an entry of the cache, and update its state accordingly,
    /// or do nothing if the entry has already been transformed. Require that the corresponding
    /// source has been parsed.
    /// If the source contains imports, recursively perform transformations on the imports too.
    pub fn transform(
        &mut self,
        file_id: FileId,
    ) -> Result<CacheOp<()>, CacheError<UnboundTypeVariableError>> {
        match self.entry_state(file_id) {
            Some(state) if state >= EntryState::Transformed => Ok(CacheOp::Cached(())),
            Some(state) if state >= EntryState::Parsed => {
                if state < EntryState::Transforming {
                    let cached_term = self.terms.remove(&file_id).unwrap();
                    let term =
                        transform::transform(cached_term.term, self.wildcards.get(&file_id))?;
                    self.terms.insert(
                        file_id,
                        TermEntry {
                            term,
                            state: EntryState::Transforming,
                            ..cached_term
                        },
                    );

                    if let Some(imports) = self.imports.get(&file_id).cloned() {
                        for f in imports.into_iter() {
                            self.transform(f)?;
                        }
                    }
                    self.update_state(file_id, EntryState::Transformed);
                }
                Ok(CacheOp::Done(()))
            }
            _ => Err(CacheError::NotParsed),
        }
    }

    /// Apply program transformations to all the fields of a record.
    ///
    /// Used to transform stdlib modules and other records loaded in the environment, when using
    /// e.g. the `load` command of the REPL. If one just uses [Self::transform], the share normal
    /// form transformation would add let bindings to a record entry `{ ... }`, turning it into
    /// `let %0 = ... in ... in { ... }`. But stdlib entries are required to be syntactically
    /// records.
    ///
    /// Note that this requirement may be relaxed in the future by e.g. evaluating stdlib entries
    /// before adding their fields to the initial environment.
    ///
    /// # Preconditions
    ///
    /// - the entry must syntactically be a record (`Record` or `RecRecord`). Otherwise, this
    /// function panics
    pub fn transform_inner(
        &mut self,
        file_id: FileId,
    ) -> Result<CacheOp<()>, CacheError<UnboundTypeVariableError>> {
        match self.entry_state(file_id) {
            Some(state) if state >= EntryState::Transformed => Ok(CacheOp::Cached(())),
            Some(_) => {
                let TermEntry {
                    mut term,
                    state,
                    parse_errs,
                } = self.terms.remove(&file_id).unwrap();
                let wildcards = self.wildcards.get(&file_id);

                if state < EntryState::Transforming {
                    match SharedTerm::make_mut(&mut term.term) {
                        Term::Record(RecordData { ref mut fields, .. }) => {
                            let map_res: Result<_, UnboundTypeVariableError> =
                                std::mem::take(fields)
                                    .into_iter()
                                    .map(|(id, field)| {
                                        Ok((
                                            id,
                                            field.try_map_value(|v| {
                                                transform::transform(v, wildcards)
                                            })?,
                                        ))
                                    })
                                    .collect();
                            *fields = map_res.map_err(CacheError::Error)?;
                        }
                        Term::RecRecord(ref mut record, ref mut dyn_fields, ..) => {
                            let map_res: Result<_, UnboundTypeVariableError> =
                                std::mem::take(&mut record.fields)
                                    .into_iter()
                                    .map(|(id, field)| {
                                        Ok((
                                            id,
                                            field.try_map_value(|v| {
                                                transform::transform(v, wildcards)
                                            })?,
                                        ))
                                    })
                                    .collect();

                            let dyn_fields_res: Result<_, UnboundTypeVariableError> =
                                std::mem::take(dyn_fields)
                                    .into_iter()
                                    .map(|(id_t, mut field)| {
                                        let value = field
                                            .value
                                            .take()
                                            .map(|v| transform::transform(v, wildcards))
                                            .transpose()?;

                                        Ok((
                                            transform::transform(id_t, wildcards)?,
                                            Field { value, ..field },
                                        ))
                                    })
                                    .collect();

                            record.fields = map_res.map_err(CacheError::Error)?;
                            *dyn_fields = dyn_fields_res.map_err(CacheError::Error)?;
                        }
                        _ => panic!("cache::transform_inner(): not a record"),
                    }

                    self.terms.insert(
                        file_id,
                        TermEntry {
                            term,
                            state: EntryState::Transforming,
                            parse_errs,
                        },
                    );

                    if let Some(imports) = self.imports.get(&file_id).cloned() {
                        for f in imports.into_iter() {
                            self.transform(f).map_err(|_| CacheError::NotParsed)?;
                        }
                    }
                    self.update_state(file_id, EntryState::Transformed);
                }

                Ok(CacheOp::Done(()))
            }
            None => Err(CacheError::NotParsed),
        }
    }

    /// Resolve every imports of an entry of the cache, and update its state accordingly, or do
    /// nothing if the imports of the entry have already been resolved. Require that the
    /// corresponding source has been parsed.
    ///
    /// If resolved imports contain imports themselves, resolve them recursively. Returns a tuple
    /// of vectors, where the first component is the imports that were transitively resolved, and
    /// the second component is the errors it encountered while resolving imports in `file_id`,
    /// respectively. Imports that were already resolved before are not included in the first
    /// component: this return value is currently used by the LSP to re-run code analysis on new
    /// files/modified files.
    ///
    /// The resolved imports are ordered by a pre-order depth-first-search. In
    /// particular, earlier elements in the returned list might import later
    /// elements but -- unless there are cyclic imports -- later elements do not
    /// import earlier elements.
    ///
    /// It only accumulates errors if the cache is in error tolerant mode, otherwise it returns an
    /// `Err(..)` containing  a `CacheError`.
    #[allow(clippy::type_complexity)]
    pub fn resolve_imports(
        &mut self,
        file_id: FileId,
    ) -> Result<CacheOp<(Vec<FileId>, Vec<ImportError>)>, CacheError<ImportError>> {
        match self.entry_state(file_id) {
            Some(EntryState::Parsed) => {
                let TermEntry { term, .. } = self.terms.get(&file_id).unwrap();
                let term = term.clone();

                let import_resolution::tolerant::ResolveResult {
                    transformed_term,
                    resolved_ids: pending,
                    import_errors,
                } = match self.error_tolerance {
                    ErrorTolerance::Tolerant => {
                        import_resolution::tolerant::resolve_imports(term, self)
                    }
                    ErrorTolerance::Strict => {
                        import_resolution::strict::resolve_imports(term, self)?.into()
                    }
                };

                // unwrap!(): we called `unwrap()` at the beginning of the enclosing if branch
                // on the result of `self.terms.get(&file_id)`. We only made recursive calls to
                // `resolve_imports` in between, which don't remove anything from `self.terms`.
                let cached_term = self.terms.get_mut(&file_id).unwrap();
                cached_term.term = transformed_term;
                cached_term.state = EntryState::ImportsResolving;

                let mut done = Vec::new();

                // Transitively resolve the imports, and accumulate the ids of the resolved
                // files along the way.
                for id in pending {
                    if let CacheOp::Done((mut done_local, _)) = self.resolve_imports(id)? {
                        done.push(id);
                        done.append(&mut done_local)
                    }
                }

                self.update_state(file_id, EntryState::ImportsResolved);

                Ok(CacheOp::Done((done, import_errors)))
            }
            // [transitory_entry_state]:
            //
            // This case is triggered by a cyclic import. The entry is already
            // being treated by an ongoing call to `resolve_import` higher up in
            // the call chain, so we don't do anything here.
            //
            // Note that in some cases, this intermediate state can be observed by an
            // external caller: if a first call to `resolve_imports` fails in the middle of
            // resolving the transitive imports, the end state of the entry is
            // `ImportsResolving`. Subsequent calls to `resolve_imports` will succeed, but
            // won't change the state to `EntryState::ImportsResolved` (and for a good
            // reason: we wouldn't even know what are the pending imports to resolve). The
            // Nickel pipeline should however fail if `resolve_imports` failed at some
            // point, anyway.
            Some(EntryState::ImportsResolving) => Ok(CacheOp::Done((Vec::new(), Vec::new()))),
            // >= EntryState::ImportsResolved
            Some(
                EntryState::ImportsResolved
                | EntryState::Typechecking
                | EntryState::Typechecked
                | EntryState::Transforming
                | EntryState::Transformed,
            ) => Ok(CacheOp::Cached((Vec::new(), Vec::new()))),
            None => Err(CacheError::NotParsed),
        }
    }

    /// Prepare a source for evaluation: parse it, resolve the imports,
    /// typecheck it and apply program transformations,
    /// if it was not already done.
    pub fn prepare(
        &mut self,
        file_id: FileId,
        initial_ctxt: &typecheck::Context,
    ) -> Result<CacheOp<()>, Error> {
        let mut result = CacheOp::Cached(());

        let format = self
            .file_paths
            .get(&file_id)
            .and_then(InputFormat::from_source_path)
            .unwrap_or_default();
        if let CacheOp::Done(_) = self.parse(file_id, format)? {
            result = CacheOp::Done(());
        }

        let import_res = self.resolve_imports(file_id).map_err(|cache_err| {
            cache_err.unwrap_error(
                "cache::prepare(): expected source to be parsed before imports resolutions",
            )
        })?;
        if let CacheOp::Done(..) = import_res {
            result = CacheOp::Done(());
        }

        let typecheck_res = self.typecheck(file_id, initial_ctxt).map_err(|cache_err| {
            cache_err
                .unwrap_error("cache::prepare(): expected source to be parsed before typechecking")
        })?;
        if typecheck_res == CacheOp::Done(()) {
            result = CacheOp::Done(());
        };

        let transform_res = self.transform(file_id).map_err(|cache_err| {
            Error::ParseErrors(
                cache_err
                    .unwrap_error(
                        "cache::prepare(): expected source to be parsed before transformations",
                    )
                    .into(),
            )
        })?;

        if transform_res == CacheOp::Done(()) {
            result = CacheOp::Done(());
        };

        Ok(result)
    }

    /// Same as [Self::prepare], but do not use nor populate the cache. Used for inputs which are
    /// known to not be reused.
    ///
    /// In this case, the caller has to process the imports themselves as needed:
    /// - typechecking
    /// - resolve imports performed inside these imports.
    /// - apply program transformations.
    pub fn prepare_nocache(
        &mut self,
        file_id: FileId,
        initial_ctxt: &typecheck::Context,
    ) -> Result<(RichTerm, Vec<FileId>), Error> {
        let (term, errs) = self.parse_nocache(file_id)?;
        if !errs.no_errors() {
            return Err(Error::ParseErrors(errs));
        }

        let import_resolution::strict::ResolveResult {
            transformed_term: term,
            resolved_ids: pending,
        } = import_resolution::strict::resolve_imports(term, self)?;

        let wildcards = type_check(&term, initial_ctxt.clone(), self)?;
        let term = transform::transform(term, Some(&wildcards))
            .map_err(|err| Error::ParseErrors(err.into()))?;
        Ok((term, pending))
    }

    /// Retrieve the name of a source given an id.
    pub fn name(&self, file_id: FileId) -> &OsStr {
        self.files.name(file_id)
    }

    /// Retrieve the id of a source given a name.
    ///
    /// Note that files added via [Self::add_file] are indexed by their full normalized path (cf
    /// [normalize_path]).
    pub fn id_of(&self, name: &SourcePath) -> Option<FileId> {
        match name {
            SourcePath::Path(p) => match self.id_or_new_timestamp_of(p).ok()? {
                SourceState::UpToDate(id) => Some(id),
                SourceState::Stale(_) => None,
            },
            name => Some(self.file_ids.get(name)?.id),
        }
    }

    /// Try to retrieve the id of a cached source.
    ///
    /// Only returns `Ok` if the source is up-to-date; if the source is stale, returns
    /// either the new timestamp of the up-to-date file or the error we encountered when
    /// trying to read it (which most likely means there was no such file).
    ///
    /// The main point of this awkward signature is to minimize I/O operations: if we accessed
    /// the timestamp, keep it around.
    fn id_or_new_timestamp_of(&self, name: &Path) -> io::Result<SourceState> {
        match self.file_ids.get(&SourcePath::Path(name.to_owned())) {
            None => Ok(SourceState::Stale(timestamp(name)?)),
            Some(NameIdEntry {
                id,
                source: SourceKind::Filesystem(ts),
            }) => {
                let new_timestamp = timestamp(name)?;
                if ts == &new_timestamp {
                    Ok(SourceState::UpToDate(*id))
                } else {
                    Ok(SourceState::Stale(new_timestamp))
                }
            }
            Some(NameIdEntry {
                id,
                source: SourceKind::Memory,
            }) => Ok(SourceState::UpToDate(*id)),
        }
    }

    /// Get a reference to the underlying files. Required by
    /// the WASM REPL error reporting code and LSP functions.
    pub fn files(&self) -> &Files<String> {
        &self.files
    }

    /// Get a mutable reference to the underlying files. Required by
    /// [crate::error::IntoDiagnostics::into_diagnostics].
    pub fn files_mut(&mut self) -> &mut Files<String> {
        &mut self.files
    }

    /// Get an immutable reference to the cached term roots
    pub fn terms(&self) -> &HashMap<FileId, TermEntry> {
        &self.terms
    }

    /// Update the state of an entry. Return the previous state.
    pub fn update_state(&mut self, file_id: FileId, new: EntryState) -> Option<EntryState> {
        self.terms
            .get_mut(&file_id)
            .map(|TermEntry { state, .. }| std::mem::replace(state, new))
    }

    /// Remove the cached term associated with this id, and any cached terms
    /// that import it.
    ///
    /// The file contents associated with this id remain, and they will be
    /// re-parsed if necessary.
    ///
    /// This invalidation scheme is probably too aggressive; there are
    /// situations where a change in one file doesn't require invalidation
    /// of other files that import it. For example, if the parse status (i.e.
    /// success/failure) of a file doesn't change, files that import it don't
    /// need to re-resolve their imports. If the checked type of a file doesn't
    /// change, files that import it don't need to be re-typechecked.
    ///
    /// Returns all the additional (i.e. not including the passed one) file ids
    /// whose caches were invalidated.
    pub fn invalidate_cache(&mut self, file_id: FileId) -> Vec<FileId> {
        fn invalidate_rec(slf: &mut Cache, acc: &mut Vec<FileId>, file_id: FileId) {
            slf.terms.remove(&file_id);
            slf.imports.remove(&file_id);
            let rev_deps = slf.rev_imports.remove(&file_id).unwrap_or_default();

            acc.extend(rev_deps.iter().copied());
            for f in &rev_deps {
                invalidate_rec(slf, acc, *f);
            }
        }

        let mut ret = vec![];
        invalidate_rec(self, &mut ret, file_id);
        ret
    }

    /// Retrieve the state of an entry. Return `None` if the entry is not in the term cache,
    /// meaning that the content of the source has been loaded but has not been parsed yet.
    pub fn entry_state(&self, file_id: FileId) -> Option<EntryState> {
        self.terms
            .get(&file_id)
            .map(|TermEntry { state, .. }| state)
            .copied()
    }

    /// Retrieve a fresh clone of a cached term.
    pub fn get_owned(&self, file_id: FileId) -> Option<RichTerm> {
        self.terms
            .get(&file_id)
            .map(|TermEntry { term, .. }| term.clone())
    }

    /// Retrieve a reference to a cached term.
    pub fn get_ref(&self, file_id: FileId) -> Option<&RichTerm> {
        self.terms.get(&file_id).map(|TermEntry { term, .. }| term)
    }

    /// Returns true if a particular file id represents a Nickel standard library file, false
    /// otherwise.
    pub fn is_stdlib_module(&self, file: FileId) -> bool {
        let Some(table) = &self.stdlib_ids else {
            return false;
        };
        table.values().any(|stdlib_file| *stdlib_file == file)
    }

    /// Retrieve the FileId for a given standard libray module.
    pub fn get_submodule_file_id(&self, module: StdlibModule) -> Option<FileId> {
        let file = self.stdlib_ids.as_ref()?.get(&module).copied()?;
        Some(file)
    }

    /// Returns the set of files that this file imports.
    pub fn get_imports(&self, file: FileId) -> impl Iterator<Item = FileId> + '_ {
        self.imports
            .get(&file)
            .into_iter()
            .flat_map(|s| s.iter())
            .copied()
    }

    /// Returns the set of files that import this file.
    pub fn get_rev_imports(&self, file: FileId) -> impl Iterator<Item = FileId> + '_ {
        self.rev_imports
            .get(&file)
            .into_iter()
            .flat_map(|s| s.iter())
            .copied()
    }

    /// Returns the set of files that transitively depend on this file.
    pub fn get_rev_imports_transitive(&self, file: FileId) -> HashSet<FileId> {
        let mut ret = HashSet::new();
        let mut stack = vec![file];

        while let Some(file) = stack.pop() {
            for f in self.get_rev_imports(file) {
                if ret.insert(f) {
                    stack.push(f);
                }
            }
        }

        ret
    }

    /// Retrieve the FileIds for all the stdlib modules
    pub fn get_all_stdlib_modules_file_id(&self) -> Option<Vec<FileId>> {
        let ids = self.stdlib_ids.as_ref()?;
        Some(ids.values().copied().collect())
    }

    /// Load and parse the standard library in the cache.
    pub fn load_stdlib(&mut self) -> Result<CacheOp<()>, Error> {
        if self.stdlib_ids.is_some() {
            return Ok(CacheOp::Cached(()));
        }

        let file_ids: HashMap<StdlibModule, FileId> = nickel_stdlib::modules()
            .into_iter()
            .map(|module| {
                let content = module.content();
                (
                    module,
                    self.add_string(SourcePath::Std(module), String::from(content)),
                )
            })
            .collect();

        for (_, file_id) in file_ids.iter() {
            self.parse(*file_id, InputFormat::Nickel)?;
        }
        self.stdlib_ids.replace(file_ids);
        Ok(CacheOp::Done(()))
    }

    /// Typecheck the standard library. Currently only used in the test suite.
    pub fn typecheck_stdlib(&mut self) -> Result<CacheOp<()>, CacheError<TypecheckError>> {
        // We have a small bootstraping problem: to typecheck the initial environment, we already
        // need an initial evaluation environment, since stdlib parts may reference each other. But
        // typechecking is performed before program transformations, so this environment is not
        // final one. We have create a temporary initial environment just for typechecking, which is
        // dropped right after. However:
        // 1. The stdlib is meant to stay relatively light.
        // 2. Typechecking the standard library ought to occur only during development. Once the
        //    stdlib is stable, we won't have typecheck it at every execution.
        let initial_env = self.mk_type_ctxt().map_err(|err| match err {
            CacheError::NotParsed => CacheError::NotParsed,
            CacheError::Error(_) => unreachable!(),
        })?;
        self.typecheck_stdlib_(&initial_env)
    }

    /// Typecheck the stdlib, provided the initial typing environment. Has to be public because
    /// it's used in benches. It probably does not have to be used for something else.
    pub fn typecheck_stdlib_(
        &mut self,
        initial_ctxt: &typecheck::Context,
    ) -> Result<CacheOp<()>, CacheError<TypecheckError>> {
        if let Some(ids) = self.stdlib_ids.as_ref().cloned() {
            ids.iter()
                .try_fold(CacheOp::Cached(()), |cache_op, (_, file_id)| {
                    match self.typecheck(*file_id, initial_ctxt)? {
                        done @ CacheOp::Done(()) => Ok(done),
                        _ => Ok(cache_op),
                    }
                })
        } else {
            Err(CacheError::NotParsed)
        }
    }

    /// Load, parse, and apply program transformations to the standard library. Do not typecheck for
    /// performance reasons: this is done in the test suite. Return an initial environment
    /// containing both the evaluation and type environments. If you only need the type environment,
    /// use `load_stdlib` then `mk_type_env` to avoid transformations and evaluation preparation.
    pub fn prepare_stdlib<EC: EvalCache>(&mut self, eval_cache: &mut EC) -> Result<Envs, Error> {
        #[cfg(debug_assertions)]
        if self.skip_stdlib {
            return Ok(Envs::new());
        }
        self.load_stdlib()?;
        let type_ctxt = self.mk_type_ctxt().unwrap();

        self.stdlib_ids
            .as_ref()
            .cloned()
            .expect("cache::prepare_stdlib(): stdlib has been loaded but stdlib_ids is None")
            .into_iter()
            // We need to handle the internals module separately. Each field
            // is bound directly in the environment without evaluating it first, so we can't
            // tolerate top-level let bindings that would be introduced by `transform`.
            .try_for_each(|(module, file_id)| {
                if let nickel_stdlib::StdlibModule::Internals = module {
                    self.transform_inner(file_id)?;
                } else {
                    self.transform(file_id)?;
                }
                Ok(())
            })
            .map_err(|cache_err: CacheError<UnboundTypeVariableError>| {
                Error::ParseErrors(
                    cache_err
                        .unwrap_error(
                            "cache::prepare_stdlib(): expected standard library to be parsed",
                        )
                        .into(),
                )
            })?;
        let eval_env = self.mk_eval_env(eval_cache).unwrap();
        Ok(Envs {
            eval_env,
            type_ctxt,
        })
    }

    /// Generate the initial typing context from the list of `file_ids` corresponding to the
    /// standard library parts.
    pub fn mk_type_ctxt(&self) -> Result<typecheck::Context, CacheError<Void>> {
        let stdlib_terms_vec: Vec<(StdlibModule, RichTerm)> =
            self.stdlib_ids
                .as_ref()
                .map_or(Err(CacheError::NotParsed), |ids| {
                    Ok(ids
                        .iter()
                        .map(|(module, file_id)| {
                            (*module, self.get_owned(*file_id).expect(
                                "cache::mk_type_env(): can't build environment, stdlib not parsed",
                            ))
                        })
                        .collect())
                })?;
        Ok(typecheck::mk_initial_ctxt(&stdlib_terms_vec).unwrap())
    }

    /// Generate the initial evaluation environment from the list of `file_ids` corresponding to the
    /// standard library parts.
    pub fn mk_eval_env<EC: EvalCache>(
        &self,
        eval_cache: &mut EC,
    ) -> Result<eval::Environment, CacheError<Void>> {
        if let Some(ids) = self.stdlib_ids.as_ref().cloned() {
            let mut eval_env = eval::Environment::new();
            ids.iter().for_each(|(module, file_id)| {
                // The internals module needs special treatment: it's required to be a record
                // literal, and its bindings are added directly to the environment
                if let nickel_stdlib::StdlibModule::Internals = module {
                    let result = eval::env_add_record(
                        eval_cache,
                        &mut eval_env,
                        Closure::atomic_closure(self.get_owned(*file_id).expect(
                            "cache::mk_eval_env(): can't build environment, stdlib not parsed",
                        )),
                    );
                    if let Err(eval::EnvBuildError::NotARecord(rt)) = result {
                        panic!(
                            "cache::load_stdlib(): \
                            expected the stdlib module {} to be a record, got {:?}",
                            self.name(*file_id).to_string_lossy().as_ref(),
                            rt
                        )
                    }
                } else {
                    eval::env_add(
                        eval_cache,
                        &mut eval_env,
                        module.name().into(),
                        self.get_owned(*file_id).expect(
                            "cache::mk_eval_env(): can't build environment, stdlib not parsed",
                        ),
                        eval::Environment::new(),
                    );
                }
            });

            Ok(eval_env)
        } else {
            Err(CacheError::NotParsed)
        }
    }
}

/// Abstract the access to imported files and the import cache. Used by the evaluator, the
/// typechecker and at the [import resolution](crate::transform::import_resolution) phase.
///
/// The standard implementation uses 2 caches, the file cache for raw contents and the term cache
/// for parsed contents, mirroring the 2 steps when resolving an import:
/// 1. When an import is encountered for the first time, the content of the corresponding file is
///    read and stored in the file cache (consisting of the file database plus a map between paths
///    and ids in the database, the name-id table). The content is parsed, stored in the term
///    cache, and queued somewhere so that it can undergo the standard
///    [transformations](crate::transform) (including import resolution) later.
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
    /// inserted back in the cache. On the other hand, if it has been resolved before, it is
    /// already transformed in the cache and do not need further processing.
    fn resolve(
        &mut self,
        path: &OsStr,
        parent: Option<FileId>,
        pos: &TermPos,
    ) -> Result<(ResolvedTerm, FileId), ImportError>;

    /// Get a resolved import from the term cache.
    fn get(&self, file_id: FileId) -> Option<RichTerm>;
    /// Return the (potentially normalized) file path corresponding to the ID of a resolved import.
    fn get_path(&self, file_id: FileId) -> Option<&OsStr>;
}

impl ImportResolver for Cache {
    fn resolve(
        &mut self,
        path: &OsStr,
        parent: Option<FileId>,
        pos: &TermPos,
    ) -> Result<(ResolvedTerm, FileId), ImportError> {
        // `parent` is the file that did the import. We first look in its containing directory.
        let mut parent_path = parent
            .and_then(|p| self.get_path(p))
            .map(PathBuf::from)
            .unwrap_or_default();
        parent_path.pop();

        let possible_parents: Vec<PathBuf> = std::iter::once(parent_path)
            .chain(self.import_paths.iter().cloned())
            .collect();

        // Try to import from all possibilities, taking the first one that succeeds.
        let (id_op, path_buf) = possible_parents
            .iter()
            .find_map(|parent| {
                let mut path_buf = parent.clone();
                path_buf.push(path);
                self.get_or_add_file(&path_buf).ok().map(|x| (x, path_buf))
            })
            .ok_or_else(|| {
                let parents = possible_parents
                    .iter()
                    .map(|p| p.to_string_lossy())
                    .collect::<Vec<_>>();
                ImportError::IOError(
                    path.to_string_lossy().into_owned(),
                    format!("could not find import (looked in [{}])", parents.join(", ")),
                    *pos,
                )
            })?;

        let format = InputFormat::from_path(&path_buf).unwrap_or_default();
        let (result, file_id) = match id_op {
            CacheOp::Cached(id) => (ResolvedTerm::FromCache, id),
            CacheOp::Done(id) => (ResolvedTerm::FromFile { path: path_buf }, id),
        };

        if let Some(parent) = parent {
            self.imports.entry(parent).or_default().insert(file_id);
            self.rev_imports.entry(file_id).or_default().insert(parent);
        }

        self.parse(file_id, format)
            .map_err(|err| ImportError::ParseErrors(err, *pos))?;

        Ok((result, file_id))
    }

    fn get(&self, file_id: FileId) -> Option<RichTerm> {
        self.terms
            .get(&file_id)
            .map(|TermEntry { term, state, .. }| {
                debug_assert!(*state >= EntryState::ImportsResolved);
                term.clone()
            })
    }

    fn get_path(&self, file_id: FileId) -> Option<&OsStr> {
        self.file_paths
            .get(&file_id)
            .and_then(|p| p.try_into().ok())
    }
}

/// Normalize the path of a file for unique identification in the cache.
///
/// The returned path will be an absolute path.
pub fn normalize_path(path: impl Into<PathBuf>) -> std::io::Result<PathBuf> {
    let mut path = path.into();
    if path.is_relative() {
        path = std::env::current_dir()?.join(path);
    }
    Ok(normalize_abs_path(&path))
}

/// Normalize the path (assumed to be absolute) of a file for unique identification in the cache.
///
/// This implementation (including the comment below) was taken from cargo-util.
///
/// CAUTION: This does not resolve symlinks (unlike
/// [`std::fs::canonicalize`]). This may cause incorrect or surprising
/// behavior at times. This should be used carefully. Unfortunately,
/// [`std::fs::canonicalize`] can be hard to use correctly, since it can often
/// fail, or on Windows returns annoying device paths. This is a problem Cargo
/// needs to improve on.
fn normalize_abs_path(path: &Path) -> PathBuf {
    use std::path::Component;

    let mut components = path.components().peekable();
    let mut ret = if let Some(c @ Component::Prefix(..)) = components.peek().cloned() {
        components.next();
        PathBuf::from(c.as_os_str())
    } else {
        PathBuf::new()
    };

    for component in components {
        match component {
            Component::Prefix(..) => unreachable!(),
            Component::RootDir => {
                ret.push(component.as_os_str());
            }
            Component::CurDir => {}
            Component::ParentDir => {
                ret.pop();
            }
            Component::Normal(c) => {
                ret.push(c);
            }
        }
    }
    ret
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
            _parent: Option<FileId>,
            _pos: &TermPos,
        ) -> Result<(ResolvedTerm, FileId), ImportError> {
            panic!("cache::resolvers: dummy resolver should not have been invoked");
        }

        fn get(&self, _file_id: FileId) -> Option<RichTerm> {
            panic!("cache::resolvers: dummy resolver should not have been invoked");
        }

        fn get_path(&self, _file_id: FileId) -> Option<&OsStr> {
            panic!("cache::resolvers: dummy resolver should not have been invoked");
        }
    }

    /// Resolve imports from a mockup file database. Used to test imports without accessing the
    /// file system. File name are stored as strings, and silently converted from/to `OsString`
    /// when needed: don't use this resolver with source code that import non UTF-8 paths.
    #[derive(Clone, Default)]
    pub struct SimpleResolver {
        files: Files<String>,
        file_cache: HashMap<String, FileId>,
        term_cache: HashMap<FileId, RichTerm>,
    }

    impl SimpleResolver {
        pub fn new() -> SimpleResolver {
            SimpleResolver::default()
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
            _parent: Option<FileId>,
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
                    .parse_strict(file_id, Lexer::new(buf))
                    .map_err(|e| ImportError::ParseErrors(e, *pos))?;
                e.insert(term);
                Ok((
                    ResolvedTerm::FromFile {
                        path: PathBuf::new(),
                    },
                    file_id,
                ))
            } else {
                Ok((ResolvedTerm::FromCache, file_id))
            }
        }

        fn get(&self, file_id: FileId) -> Option<RichTerm> {
            self.term_cache.get(&file_id).cloned()
        }

        fn get_path(&self, file_id: FileId) -> Option<&OsStr> {
            Some(self.files.name(file_id))
        }
    }
}
