//! Various caches for artifacts generated across the whole pipeline: source code, parsed
//! representations, imports data (dependencies and reverse dependencies, etc.)
//!
//! In order to manage the complexity of correctly borrowing such structures, where the arena
//! allocation of ASTs requires usage of self-borrowing structures, the main cache is split in
//! different subcaches that can be borrowed independently.
pub use ast_cache::AstCache;

use crate::{
    bytecode::ast::{
        self,
        compat::{ToAst, ToMainline},
        Ast, AstAlloc, TryConvert,
    },
    closurize::Closurize as _,
    error::{Error, ImportError, ParseError, ParseErrors, TypecheckError},
    eval::cache::Cache as EvalCache,
    eval::Closure,
    files::{FileId, Files},
    identifier::LocIdent,
    metrics::measure_runtime,
    package::PackageMap,
    parser::{lexer::Lexer, ErrorTolerantParser, ExtendedTerm},
    position::TermPos,
    program::FieldPath,
    stdlib::{self as nickel_stdlib, StdlibModule},
    term::{self, RichTerm, Term},
    transform::{import_resolution, Wildcards},
    traverse::{Traverse, TraverseOrder},
    typ::{self as mainline_typ, UnboundTypeVariableError},
    typecheck::{self, typecheck, HasApparentType, TypecheckMode},
    {eval, parser, transform},
};

#[cfg(feature = "nix-experimental")]
use crate::nix_ffi;

use std::{
    collections::{hash_map, HashMap, HashSet},
    ffi::{OsStr, OsString},
    fmt, fs,
    io::{self, Read},
    path::{Path, PathBuf},
    result::Result,
    sync::Arc,
    time::SystemTime,
};

use ouroboros::self_referencing;

/// Error when trying to add bindings to the typing context where the given term isn't a record
/// literal.
pub struct NotARecord;

/// Supported input formats.
#[derive(Default, Clone, Copy, Eq, Debug, PartialEq, Hash)]
pub enum InputFormat {
    #[default]
    Nickel,
    Json,
    Yaml,
    Toml,
    #[cfg(feature = "nix-experimental")]
    Nix,
    Text,
}

impl InputFormat {
    /// Returns an [InputFormat] based on the file extension of a path.
    pub fn from_path(path: impl AsRef<Path>) -> Option<InputFormat> {
        match path.as_ref().extension().and_then(OsStr::to_str) {
            Some("ncl") => Some(InputFormat::Nickel),
            Some("json") => Some(InputFormat::Json),
            Some("yaml") | Some("yml") => Some(InputFormat::Yaml),
            Some("toml") => Some(InputFormat::Toml),
            #[cfg(feature = "nix-experimental")]
            Some("nix") => Some(InputFormat::Nix),
            Some("txt") => Some(InputFormat::Text),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &'static str {
        match self {
            InputFormat::Nickel => "Nickel",
            InputFormat::Json => "Json",
            InputFormat::Yaml => "Yaml",
            InputFormat::Toml => "Toml",
            InputFormat::Text => "Text",
            #[cfg(feature = "nix-experimental")]
            InputFormat::Nix => "Nix",
        }
    }

    /// Extracts format embedded in SourcePath
    pub fn from_source_path(source_path: &SourcePath) -> Option<InputFormat> {
        if let SourcePath::Path(_p, fmt) = source_path {
            Some(*fmt)
        } else {
            None
        }
    }
}

impl fmt::Display for InputFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl std::str::FromStr for InputFormat {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "Json" => InputFormat::Json,
            "Nickel" => InputFormat::Nickel,
            "Text" => InputFormat::Text,
            "Yaml" => InputFormat::Yaml,
            "Toml" => InputFormat::Toml,
            #[cfg(feature = "nix-experimental")]
            "Nix" => InputFormat::Nix,
            _ => return Err(()),
        })
    }
}

/// The term cache stores the parsed values (the runtime representation) of sources.
#[derive(Debug, Clone)]
pub struct TermCache {
    /// The term table stores parsed terms corresponding to the entries of the file database.
    terms: HashMap<FileId, TermEntry>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct TermNotFound;

impl TermCache {
    pub fn new() -> Self {
        TermCache {
            terms: HashMap::new(),
        }
    }

    /// Updates the state of an entry and returns the previous state, or an error if the entry
    /// isn't in the cache.
    pub fn update_state(
        &mut self,
        file_id: FileId,
        new: TermEntryState,
    ) -> Result<TermEntryState, TermNotFound> {
        self.terms
            .get_mut(&file_id)
            .map(|TermEntry { state, .. }| std::mem::replace(state, new))
            .ok_or(TermNotFound)
    }

    /// Applies term transformation excepted import resolution, implemented in a separate phase.
    fn transform(
        &mut self,
        wildcards: &WildcardsCache,
        import_data: &ImportData,
        file_id: FileId,
    ) -> Result<CacheOp<()>, TermCacheError<UnboundTypeVariableError>> {
        match self.terms.get(&file_id).map(|entry| entry.state) {
            Some(state) if state >= TermEntryState::Transformed => Ok(CacheOp::Cached(())),
            Some(state) => {
                if state < TermEntryState::Transforming {
                    let cached_term = self.terms.remove(&file_id).unwrap();
                    let term =
                        transform::transform(cached_term.term, wildcards.wildcards.get(&file_id))?;
                    self.insert(
                        file_id,
                        TermEntry {
                            term,
                            state: TermEntryState::Transforming,
                            ..cached_term
                        },
                    );

                    let imported: Vec<_> = import_data.imports(file_id).collect();
                    for file_id in imported {
                        self.transform(wildcards, import_data, file_id)?;
                    }

                    // unwrap(): we re-inserted the entry after removal and transformation, so it
                    // should be in the cache.
                    let _ = self
                        .update_state(file_id, TermEntryState::Transformed)
                        .unwrap();
                }

                Ok(CacheOp::Done(()))
            }
            None => Err(CacheError::IncompatibleState {
                want: TermEntryState::Populated,
            }),
        }
    }

    /// Retrieves the state of an entry. Returns `None` if the entry is not in the term cache. This
    /// might happen if the file hasn't been parsed, or if the term cache hasn't be filled from the
    /// AST cache yet. The latter is supposed to happen right before program transformations.
    pub fn entry_state(&self, file_id: FileId) -> Option<TermEntryState> {
        self.terms
            .get(&file_id)
            .map(|TermEntry { state, .. }| *state)
    }

    /// Replaces a cache entry by a closurized version of itself. If it contains imports,
    /// closurize them recursively.
    ///
    /// Closurization is not required before evaluation, but it has two benefits:
    ///
    /// - the closurized term uses the evaluation cache, so if it is imported in multiple
    ///   places then they will share a cache
    /// - the eval cache's built-in mechanism for preventing infinite recursion will also
    ///   apply to recursive imports.
    ///
    /// The main disadvantage of closurization is that it makes the resulting runtime
    /// representation less useful. You wouldn't want to closurize before pretty-printing, for
    /// example. This isn't as important these days, since we also have the AST representation at
    /// hand.
    pub fn closurize<C: EvalCache>(
        &mut self,
        cache: &mut C,
        import_data: &ImportData,
        file_id: FileId,
    ) -> Result<CacheOp<()>, TermCacheError<()>> {
        match self.entry_state(file_id) {
            Some(state) if state >= TermEntryState::Closurized => Ok(CacheOp::Cached(())),
            Some(_) => {
                let cached_term = self.terms.remove(&file_id).unwrap();
                let term = cached_term.term.closurize(cache, eval::Environment::new());
                self.insert(
                    file_id,
                    TermEntry {
                        term,
                        state: TermEntryState::Closurized,
                        ..cached_term
                    },
                );

                let imported: Vec<_> = import_data.imports(file_id).collect();
                for file_id in imported {
                    self.closurize(cache, import_data, file_id)?;
                }

                Ok(CacheOp::Done(()))
            }
            None => Err(CacheError::IncompatibleState {
                want: TermEntryState::Populated,
            }),
        }
    }

    /// Returns an immutable reference to the whole term cache.
    pub fn terms(&self) -> &HashMap<FileId, TermEntry> {
        &self.terms
    }

    /// Retrieves a fresh clone of a cached term.
    pub fn get_owned(&self, file_id: FileId) -> Option<RichTerm> {
        self.terms
            .get(&file_id)
            .map(|TermEntry { term, .. }| term.clone())
    }

    /// Retrieves a reference to a cached term.
    pub fn get(&self, file_id: FileId) -> Option<&RichTerm> {
        self.terms.get(&file_id).map(|TermEntry { term, .. }| term)
    }

    /// Retrieves the whole entry for a given file id.
    pub fn get_entry(&self, file_id: FileId) -> Option<&TermEntry> {
        self.terms.get(&file_id)
    }

    /// Returns `true` if the term cache contains a term for the given file id.
    pub fn contains(&self, file_id: FileId) -> bool {
        self.terms.contains_key(&file_id)
    }

    /// Inserts a new entry in the cache. Usually, this should be handled by [CacheHub] directly,
    /// but there are some use-cases where it is useful to pre-fill the term cache (typically in
    /// NLS).
    pub fn insert(&mut self, file_id: FileId, entry: TermEntry) {
        self.terms.insert(file_id, entry);
    }
}

/// This is a temporary fix for [#2362](https://github.com/tweag/nickel/issues/2362). File paths
/// prefixed with this are treated specially: they can refer to in-memory source. To build an
/// import expression that refers to an in-memory source, append the source name to this prefix and
/// use it as the path: `format!({IN_MEMORY_SOURCE_PATH_PREFIX}{src_name})`.
pub const IN_MEMORY_SOURCE_PATH_PREFIX: &str = "%inmem_src%:";

/// The source cache handles reading textual data from the file system or other souces and storing
/// it in a [Files] instance.
///
/// While not ideal, we have to make most of the fields public to allow the LSP to perform its own
/// import resolution.
#[derive(Clone)]
pub struct SourceCache {
    /// The content of the program sources plus imports.
    pub files: Files,
    /// Reverse map from file ids to source paths.
    pub file_paths: HashMap<FileId, SourcePath>,
    /// The name-id table, holding file ids stored in the database indexed by source names.
    pub file_ids: HashMap<SourcePath, NameIdEntry>,
    /// Paths where to look for imports, as included by the user through either the CLI argument
    /// `--import-path` or the environment variable `$NICKEL_IMPORT_PATH`.
    pub import_paths: Vec<PathBuf>,
    /// A table mapping FileIds to the package that they belong to.
    ///
    /// Path dependencies have already been canonicalized to absolute paths.
    pub packages: HashMap<FileId, PathBuf>,
    /// The map used to resolve package imports.
    pub package_map: Option<PackageMap>,
}

impl SourceCache {
    pub fn new() -> Self {
        SourceCache {
            files: Files::new(),
            file_paths: HashMap::new(),
            file_ids: HashMap::new(),
            import_paths: Vec::new(),
            packages: HashMap::new(),
            package_map: None,
        }
    }

    /// Retrieves the name of a source given an id.
    pub fn name(&self, file_id: FileId) -> &OsStr {
        self.files.name(file_id)
    }

    /// Add paths to the import path list, where the resolver is looking for imported files.
    pub fn add_import_paths<P>(&mut self, paths: impl Iterator<Item = P>)
    where
        PathBuf: From<P>,
    {
        self.import_paths.extend(paths.map(PathBuf::from));
    }

    /// Sets the package map to use for package import resolution.
    pub fn set_package_map(&mut self, map: PackageMap) {
        self.package_map = Some(map);
    }

    /// Same as [Self::add_file], but assumes that the path is already normalized and takes the
    /// timestamp as a parameter.
    fn add_normalized_file(
        &mut self,
        path: PathBuf,
        format: InputFormat,
        timestamp: SystemTime,
    ) -> io::Result<FileId> {
        let contents = std::fs::read_to_string(&path)?;
        let file_id = self.files.add(&path, contents);

        self.file_paths
            .insert(file_id, SourcePath::Path(path.clone(), format));
        self.file_ids.insert(
            SourcePath::Path(path, format),
            NameIdEntry {
                id: file_id,
                source: SourceKind::Filesystem(timestamp),
            },
        );
        Ok(file_id)
    }

    /// Loads a file and adds it to the name-id table.
    ///
    /// Uses the normalized path and the *modified at* timestamp as the name-id table entry.
    /// Overrides any existing entry with the same name.
    pub fn add_file(
        &mut self,
        path: impl Into<OsString>,
        format: InputFormat,
    ) -> io::Result<FileId> {
        let path = path.into();
        let timestamp = timestamp(&path)?;
        let normalized = normalize_path(&path)?;
        self.add_normalized_file(normalized, format, timestamp)
    }

    /// Try to retrieve the id of a file from the cache.
    ///
    /// If it was not in cache, try to read it and add it as a new entry.
    ///
    /// # In memory sources
    ///
    /// As a temporary fix for [#2362](https://github.com/tweag/nickel/issues/2362), if a file path
    /// starts with [IN_MEMORY_SOURCE_PATH_PREFIX], the suffix is looked up un-normalized value
    /// first, which makes it possible to hit in-memory only sources by importing a path
    /// `"{SOURCE_PATH_PREFX}{src_name}"`. If it can't be found, it is looked up normally, so that
    /// it doesn't break strange file names that happen to contain the source path prefix.
    ///
    /// It is theoretically possible that if both the source "abc" and the file
    /// "{IN_MEMORY_SOURCE_PATH_PREFIX}abc" exist, the source is imported instead of the intended
    /// file. However, given the prefix, it just can't be accidental. As we want to give access to
    /// in-memory sources in any case, although this can be surprising, I don't see any obvious
    /// attack scenario here. This fix is also intended to be temporary. If you still need to make
    /// sure this doesn't happen, one way would be to add some randomness to the name of the
    /// sources, so that they can't be predicted beforehand.
    pub fn get_or_add_file(
        &mut self,
        path: impl Into<OsString>,
        format: InputFormat,
    ) -> io::Result<CacheOp<FileId>> {
        let path = path.into();
        let normalized = normalize_path(&path)?;

        // Try to fetch a generated source if the path starts with a hardcoded prefix
        let generated_entry = path
            .to_str()
            .and_then(|p| p.strip_prefix(IN_MEMORY_SOURCE_PATH_PREFIX))
            .and_then(|src_name| {
                self.file_ids
                    .get(&SourcePath::Path(src_name.into(), format))
            });

        if let Some(entry) = generated_entry {
            return Ok(CacheOp::Cached(entry.id));
        }

        match self.id_or_new_timestamp_of(normalized.as_ref(), format)? {
            SourceState::UpToDate(id) => Ok(CacheOp::Cached(id)),
            SourceState::Stale(timestamp) => self
                .add_normalized_file(normalized, format, timestamp)
                .map(CacheOp::Done),
        }
    }

    /// Load a source and add it to the name-id table.
    ///
    /// Do not check if a source with the same name already exists: if it is the case,
    /// [Self::add_source] will happily will override the old entry in the name-id table.
    pub fn add_source<T>(&mut self, source_name: SourcePath, mut source: T) -> io::Result<FileId>
    where
        T: Read,
    {
        let mut buffer = String::new();
        source.read_to_string(&mut buffer)?;
        Ok(self.add_string(source_name, buffer))
    }

    /// Returns the content of a file.
    ///
    /// Panics if the file id is invalid.
    pub fn source(&self, id: FileId) -> &str {
        self.files.source(id)
    }

    /// Returns a cloned `Arc` to the content of the file.
    ///
    /// The `Arc` is here for the LSP, where the background evaluation is handled by background
    /// threads and processes.
    ///
    /// Panics if the file id is invalid.
    pub fn clone_source(&self, id: FileId) -> Arc<str> {
        self.files.clone_source(id)
    }

    /// Loads a new source as a string and add it to the name-id table.
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

    /// Loads a new source as a string, replacing any existing source with the same name.
    ///
    /// As opposed to [CacheHub::replace_string], this method doesn't update the other caches. It
    /// just affects the source cache.
    pub fn replace_string(&mut self, source_name: SourcePath, s: String) -> FileId {
        if let Some(file_id) = self.id_of(&source_name) {
            // The file may have been originally loaded from the filesystem and then
            // updated by the LSP, so the SourceKind needs to be updated to Memory.
            self.file_ids.insert(
                source_name,
                NameIdEntry {
                    id: file_id,
                    source: SourceKind::Memory,
                },
            );
            self.files.update(file_id, s);
            file_id
        } else {
            // We re-use [Self::add_string] here to properly fill the file_paths and file_ids
            // tables.
            self.add_string(source_name, s)
        }
    }

    /// Closes a file that has been opened in memory and reloads it from the filesystem.
    /// Returns the file ID of the replacement file loaded from the filesystem.
    pub fn close_in_memory_file(
        &mut self,
        path: PathBuf,
        format: InputFormat,
    ) -> Result<FileCloseResult, FileCloseError> {
        let entry = self
            .file_ids
            .get_mut(&SourcePath::Path(path.clone(), format))
            .ok_or(FileCloseError::FileIdNotFound)?;
        match &entry.source {
            SourceKind::Memory => {
                let closed_id = entry.id;
                entry.source = SourceKind::MemoryClosed;
                let replacement_id = self.get_or_add_file(path, format).map(|op| op.inner());
                Ok(FileCloseResult {
                    closed_id,
                    replacement_id,
                })
            }
            _ => Err(FileCloseError::FileNotOpen),
        }
    }

    /// Retrieves the id of a source given a name.
    ///
    /// Note that files added via [Self::add_file] are indexed by their full normalized path (cf
    /// [normalize_path]).
    pub fn id_of(&self, name: &SourcePath) -> Option<FileId> {
        match name {
            SourcePath::Path(p, fmt) => match self.id_or_new_timestamp_of(p, *fmt).ok()? {
                SourceState::UpToDate(id) => Some(id),
                SourceState::Stale(_) => None,
            },
            name => Some(self.file_ids.get(name)?.id),
        }
    }

    /// Tries to retrieve the id of a cached source.
    ///
    /// Only returns `Ok` if the source is up-to-date; if the source is stale, returns
    /// either the new timestamp of the up-to-date file or the error we encountered when
    /// trying to read it (which most likely means there was no such file).
    ///
    /// The main point of this awkward signature is to minimize I/O operations: if we accessed
    /// the timestamp, keep it around.
    fn id_or_new_timestamp_of(&self, name: &Path, format: InputFormat) -> io::Result<SourceState> {
        match self
            .file_ids
            .get(&SourcePath::Path(name.to_owned(), format))
        {
            None
            | Some(NameIdEntry {
                source: SourceKind::MemoryClosed,
                ..
            }) => Ok(SourceState::Stale(timestamp(name)?)),
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

    /// Gets a reference to the underlying files. Required by the WASM REPL error reporting code
    /// and LSP functions.
    pub fn files(&self) -> &Files {
        &self.files
    }

    /// Parses a Nickel source without querying nor populating other caches.
    pub fn parse_nickel<'ast>(
        &self,
        // We take the allocator explicitly, to make sure `self.asts` is properly initialized
        // before calling this function, and won't be dropped.
        alloc: &'ast AstAlloc,
        file_id: FileId,
    ) -> Result<Ast<'ast>, ParseErrors> {
        parse_nickel(alloc, file_id, self.files.source(file_id))
    }

    /// Parses a source that isn't Nickel code without querying nor populating the other caches. Support
    /// multiple formats.
    ///
    /// The Nickel/non Nickel distinction is a bit artificial at the moment, due to the fact that
    /// parsing Nickel returns the new [crate::bytecode::ast::Ast], while parsing other formats
    /// don't go through the new AST first but directly deserialize to the legacy
    /// [crate::term::Term] for simplicity and performance reasons.
    ///
    /// Once RFC007 is fully implemented, we might clean it up.
    ///
    /// # Panic
    ///
    /// This function panics if `format` is [InputFormat::Nickel].
    pub fn parse_other(
        &self,
        file_id: FileId,
        format: InputFormat,
    ) -> Result<RichTerm, ParseError> {
        let attach_pos = |t: RichTerm| -> RichTerm {
            let pos: TermPos = self.files.source_span(file_id).into();
            t.with_pos(pos)
        };

        let source = self.files.source(file_id);

        match format {
            InputFormat::Nickel => {
                // Panicking isn't great, but we expect this to be temporary, until RFC007 is fully
                // implemented. And this case is an internal bug.
                panic!("error: trying to parse a Nickel source with parse_other_nocache")
            }
            InputFormat::Json => serde_json::from_str(source)
                .map(attach_pos)
                .map_err(|err| ParseError::from_serde_json(err, file_id, &self.files)),
            InputFormat::Yaml => crate::serialize::yaml::load_yaml_term(source, Some(file_id)),
            InputFormat::Toml => crate::serialize::toml_deser::from_str(source, file_id)
                .map(attach_pos)
                .map_err(|err| (ParseError::from_toml(err, file_id))),
            #[cfg(feature = "nix-experimental")]
            InputFormat::Nix => {
                let json = nix_ffi::eval_to_json(source, &self.get_base_dir_for_nix(file_id))
                    .map_err(|e| ParseError::from_nix(e.what(), file_id))?;
                serde_json::from_str(&json)
                    .map(attach_pos)
                    .map_err(|err| ParseError::from_serde_json(err, file_id, &self.files))
            }
            InputFormat::Text => Ok(attach_pos(Term::Str(source.into()).into())),
        }
    }

    /// Returns true if a particular file id represents a Nickel standard library file, false
    /// otherwise.
    pub fn is_stdlib_module(&self, file: FileId) -> bool {
        self.files.is_stdlib(file)
    }

    /// Retrieves the file id for a given standard libray module.
    pub fn get_submodule_file_id(&self, module: StdlibModule) -> Option<FileId> {
        self.stdlib_modules()
            .find(|(m, _id)| m == &module)
            .map(|(_, id)| id)
    }

    /// Returns the list of file ids corresponding to the standard library modules.
    pub fn stdlib_modules(&self) -> impl Iterator<Item = (StdlibModule, FileId)> {
        self.files.stdlib_modules()
    }

    /// Return the format of a given source. Returns `None` if there is no entry in the source
    /// cache for `file_id`, or if there is no well-defined input format (e.g. for REPL inputs,
    /// field assignments, etc.).
    pub fn input_format(&self, file_id: FileId) -> Option<InputFormat> {
        self.file_paths
            .get(&file_id)
            .and_then(|source| match source {
                SourcePath::Path(_, input_format) => Some(*input_format),
                SourcePath::Std(_) => Some(InputFormat::Nickel),
                SourcePath::Snippet(_)
                | SourcePath::Query
                | SourcePath::ReplInput(_)
                | SourcePath::ReplTypecheck
                | SourcePath::ReplQuery
                | SourcePath::CliFieldAssignment
                | SourcePath::Override(_)
                | SourcePath::Generated(_) => None,
            })
    }

    /// Returns the base path for Nix evaluation, which is the parent directory of the source file
    /// if any, or the current working directory, or an empty path if we couldn't find any better.
    #[cfg(feature = "nix-experimental")]
    fn get_base_dir_for_nix(&self, file_id: FileId) -> PathBuf {
        let parent_dir = self
            .file_paths
            .get(&file_id)
            .and_then(|source_path| Path::new(<&OsStr>::try_from(source_path).ok()?).parent());

        parent_dir
            .map(PathBuf::from)
            .or_else(|| std::env::current_dir().ok())
            .unwrap_or_default()
    }
}

/// Stores the mapping of each wildcard id to its inferred type, for each file in the cache.
#[derive(Default, Clone, Debug)]
pub struct WildcardsCache {
    wildcards: HashMap<FileId, Wildcards>,
}

impl WildcardsCache {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, file_id: FileId) -> Option<&Wildcards> {
        self.wildcards.get(&file_id)
    }
}

/// Metadata about an imported file.
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub struct ImportTarget {
    pub file_id: FileId,
    pub format: InputFormat,
}

/// Stores dependencies and reverse dependencies data between sources.
#[derive(Default, Clone)]
pub struct ImportData {
    /// A map containing for each FileId a list of files they import (directly).
    pub imports: HashMap<FileId, HashSet<ImportTarget>>,
    /// A map containing for each FileId a list of files importing them (directly). Note that we
    /// don't need to store the format here, as only Nickel files can import other files. We do
    /// however store the position of the first import expression (the same file can be imported
    /// many times from a given file), for error reporting purpose.
    pub rev_imports: HashMap<FileId, HashMap<FileId, TermPos>>,
}

impl ImportData {
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the set of files that this file imports.
    pub fn imports(&self, file: FileId) -> impl Iterator<Item = FileId> + '_ {
        self.imports
            .get(&file)
            .into_iter()
            .flat_map(|s| s.iter())
            .map(|tgt| tgt.file_id)
    }

    /// Returns the set of files that import this file.
    pub fn rev_imports(&self, file: FileId) -> impl Iterator<Item = FileId> + '_ {
        self.rev_imports
            .get(&file)
            .into_iter()
            .flat_map(|h| h.keys())
            .copied()
    }

    /// Returns the set of files that transitively depend on this file.
    pub fn transitive_rev_imports(&self, file: FileId) -> HashSet<FileId> {
        let mut ret = HashSet::new();
        let mut stack = vec![file];

        while let Some(file) = stack.pop() {
            for f in self.rev_imports(file) {
                if ret.insert(f) {
                    stack.push(f);
                }
            }
        }

        ret
    }

    /// Returns the set of files that this file transitively depends on.
    pub fn transitive_imports(&self, file: FileId) -> HashSet<FileId> {
        let mut ret = HashSet::new();
        let mut stack = vec![file];

        while let Some(file) = stack.pop() {
            for f in self.imports(file) {
                if ret.insert(f) {
                    stack.push(f);
                }
            }
        }

        ret
    }

    /// Returns `true` if those import data are empty.
    pub fn is_empty(&self) -> bool {
        self.imports.is_empty() && self.rev_imports.is_empty()
    }
}

/// The cache hub aggregates the various kind of source-related caches used by Nickel.
///
/// [CacheHub] handles parsing, typechecking and program transformation of sources, as well as
/// caching the corresponding artifacts (text, ASTs, state). This is the central entry point for
/// other modules.
///
/// # RFC007
///
/// As part of the migration to a new AST required by RFC007, as long as we don't have a fully
/// working bytecode virtual machine, the cache needs to keep parsed expressions both as the old
/// representation (dubbed "mainline" or the runtime representation in many places) and as the new
/// AST representation.
pub struct CacheHub {
    pub terms: TermCache,
    pub sources: SourceCache,
    pub asts: AstCache,
    pub wildcards: WildcardsCache,
    pub import_data: ImportData,
    #[cfg(debug_assertions)]
    /// Skip loading the stdlib, used for debugging purpose
    pub skip_stdlib: bool,
}

impl CacheHub {
    pub fn new() -> Self {
        CacheHub {
            terms: TermCache::new(),
            sources: SourceCache::new(),
            asts: AstCache::empty(),
            wildcards: WildcardsCache::new(),
            import_data: ImportData::new(),
            #[cfg(debug_assertions)]
            skip_stdlib: false,
        }
    }

    /// Actual implementation of [Self::parse_ast] which doesn't take `self` as a parameter, so that it
    /// can be reused from other places when we don't have a full [CacheHub] instance at hand.
    fn parse_ast_impl(
        asts: &mut AstCache,
        sources: &mut SourceCache,
        file_id: FileId,
    ) -> Result<CacheOp<()>, ParseErrors> {
        if asts.contains(file_id) {
            Ok(CacheOp::Cached(()))
        } else {
            let _ = asts.parse_nickel(file_id, sources.files.source(file_id))?;
            Ok(CacheOp::Done(()))
        }
    }

    /// Parse a REPL input and populate the corresponding entry in the cache.
    ///
    /// The first component of the tuple in the `Ok` case is the identifier of the toplevel let, if
    /// the input is a toplevel let, or `None` if the input is a standard Nickel expression.
    ///
    /// # RFC007
    ///
    /// This method populates both the ast cache and the term cache at once.
    pub fn parse_repl(
        &mut self,
        file_id: FileId,
    ) -> Result<CacheOp<Option<LocIdent>>, ParseErrors> {
        // Since we need the identifier, we always reparse the input. In any case, it doesn't
        // happen that we the same REPL input twice right now, so caching it is in fact useless.
        // It's just must simpler to reuse the cache infrastructure than to reimplement the whole
        // transformations and import dependencies tracking elsewhere.
        let extd_ast = self
            .asts
            .parse_nickel_repl(file_id, self.sources.files.source(file_id))?;

        let (id, ast) = match extd_ast {
            ExtendedTerm::Term(t) => (None, t),
            ExtendedTerm::ToplevelLet(id, t) => (Some(id), t),
        };

        let term = measure_runtime!("runtime:ast_conversion", ast.to_mainline());

        self.terms.insert(
            file_id,
            TermEntry {
                term,
                state: TermEntryState::default(),
                format: InputFormat::Nickel,
            },
        );

        Ok(CacheOp::Done(id))
    }

    /// Parses a source and populate the corresponding entry in the AST cache, or do nothing if the
    /// entry has already been parsed. External input formats are currently directly parsed to the
    /// runtime representation, without going through an AST: currently, the format is assumed to
    /// be [InputFormat::Nickel] in this method. See [Self::parse_to_term] for other formats.
    ///
    /// # RFC007
    ///
    /// This method only populates the AST cache. The term cache must be filled separately.
    pub fn parse_to_ast(&mut self, file_id: FileId) -> Result<CacheOp<()>, ParseErrors> {
        Self::parse_ast_impl(&mut self.asts, &mut self.sources, file_id)
    }

    /// Parses a source or compiles an AST into the term cache:
    ///
    /// - if the entry is already in the term cache, do nothing.
    /// - if the format is Nickel and there is a corresponding entry in the AST cache, converts the
    ///   parsed AST to a [RichTerm] and put it in the term cache.
    /// - if the format is Nickel but there is no cached AST, or if the format is not Nickel, parse
    ///   the input directly into the term cache.
    ///
    /// Mostly used during ([RichTerm]-based) import resolution.
    pub fn parse_to_term(
        &mut self,
        file_id: FileId,
        format: InputFormat,
    ) -> Result<CacheOp<()>, ParseErrors> {
        if self.terms.contains(file_id) {
            return Ok(CacheOp::Cached(()));
        }

        let term = if let InputFormat::Nickel = format {
            match self.compile(file_id) {
                Ok(cache_op) => return Ok(cache_op),
                Err(_) => {
                    let alloc = AstAlloc::new();
                    self.sources.parse_nickel(&alloc, file_id)?.to_mainline()
                }
            }
        } else {
            self.sources.parse_other(file_id, format)?
        };

        self.terms.insert(
            file_id,
            TermEntry {
                term,
                state: TermEntryState::default(),
                format,
            },
        );

        Ok(CacheOp::Done(()))
    }

    /// Typecheck an entry of the cache and update its state accordingly, or do nothing if the
    /// entry has already been typechecked. Require that the corresponding source has been parsed.
    /// If the source contains imports, [Self::typecheck] recursively typechecks the imports as
    /// well.
    ///
    /// # RFC007
    ///
    /// During the transition period between the old VM and the new bytecode VM, this method
    /// performs typechecking on the new representation [crate::bytecode::ast::Ast].
    pub fn typecheck(
        &mut self,
        file_id: FileId,
        initial_mode: TypecheckMode,
    ) -> Result<CacheOp<()>, AstCacheError<TypecheckError>> {
        let (slice, asts) = self.split_asts();
        asts.typecheck(slice, file_id, initial_mode)
    }

    /// Returns the apparent type of an entry that has been typechecked with wildcards substituted.
    pub fn type_of(
        &mut self,
        file_id: FileId,
    ) -> Result<CacheOp<mainline_typ::Type>, AstCacheError<TypecheckError>> {
        let (slice, asts) = self.split_asts();
        asts.type_of(slice, file_id)
    }

    /// Prepares a source for evaluation: parse, typecheck and apply program transformations, if it
    /// was not already done.
    pub fn prepare(&mut self, file_id: FileId) -> Result<CacheOp<()>, Error> {
        self.prepare_impl(file_id, true)
    }

    /// Prepare a file for evaluation only. Same as [Self::prepare], but doesn't typecheck the
    /// source.
    pub fn prepare_eval_only(&mut self, file_id: FileId) -> Result<CacheOp<()>, Error> {
        self.prepare_impl(file_id, false)
    }

    /// Common implementation for [Self::prepare] and [Self::prepare_eval_only], which optionally
    /// skips typechecking.
    fn prepare_impl(&mut self, file_id: FileId, typecheck: bool) -> Result<CacheOp<()>, Error> {
        let mut result = CacheOp::Cached(());

        let format = self
            .sources
            .file_paths
            .get(&file_id)
            .and_then(InputFormat::from_source_path)
            .unwrap_or_default();

        if let InputFormat::Nickel = format {
            if let CacheOp::Done(_) = self.parse_to_ast(file_id)? {
                result = CacheOp::Done(());
            }

            if typecheck {
                let (slice, asts) = self.split_asts();

                let typecheck_res = asts
                    .typecheck(slice, file_id, TypecheckMode::Walk)
                    .map_err(|cache_err| {
                        cache_err.unwrap_error(
                            "cache::prepare(): expected source to be parsed before typechecking",
                        )
                    })?;

                if typecheck_res == CacheOp::Done(()) {
                    result = CacheOp::Done(());
                };
            }
        }
        // Non-Nickel terms are currently not parsed as ASTs, but directly as the runtime
        // representation. While the imports of the main file will be parsed to terms by the
        // `compile_and_transform` automatically, we do need to ensure that the main file is in the
        // term cache if it's an external format, or `compile_and_transform` will complain.
        else if let CacheOp::Done(_) = self.parse_to_term(file_id, format)? {
            result = CacheOp::Done(());
        }

        let transform_res = self.compile_and_transform(file_id).map_err(|cache_err| {
            cache_err.unwrap_error(
                "cache::prepare(): expected source to be parsed before transformations",
            )
        })?;

        if transform_res == CacheOp::Done(()) {
            result = CacheOp::Done(());
        };

        Ok(result)
    }

    /// Prepare an REPL snippet for evaluation: parse, typecheck and apply program transformations,
    /// if it was not already done. The difference with [Self::prepare] is that this method also
    /// accept toplevel binding `let <id> = <value>`.
    ///
    /// Returns the identifier of the toplevel let, if the input is a toplevel let, or `None` if
    /// the input is a standard Nickel expression.
    pub fn prepare_repl(&mut self, file_id: FileId) -> Result<CacheOp<Option<LocIdent>>, Error> {
        let mut done = false;

        let parsed = self.parse_repl(file_id)?;

        done = done || matches!(parsed, CacheOp::Done(_));

        let id = parsed.inner();

        let (slice, asts) = self.split_asts();
        let typecheck_res = asts
            .typecheck(slice, file_id, TypecheckMode::Walk)
            .map_err(|cache_err| {
                cache_err.unwrap_error(
                    "cache::prepare_repl(): expected source to be parsed before typechecking",
                )
            })?;

        if let Some(id) = id {
            let (slice, asts) = self.split_asts();
            asts
                .add_type_binding(
                    slice,
                    id,
                    file_id,
                ).expect("cache::prepare_repl(): expected source to be parsed before augmenting the type environment");
        }

        done = done || matches!(typecheck_res, CacheOp::Done(_));

        let transform_res = self.compile_and_transform(file_id).map_err(|cache_err| {
            cache_err.unwrap_error(
                "cache::prepare(): expected source to be parsed before transformations",
            )
        })?;

        done = done || matches!(transform_res, CacheOp::Done(_));

        if done {
            Ok(CacheOp::Done(id))
        } else {
            Ok(CacheOp::Cached(id))
        }
    }

    /// Proxy for [TermCache::transform].
    fn transform(
        &mut self,
        file_id: FileId,
    ) -> Result<CacheOp<()>, TermCacheError<UnboundTypeVariableError>> {
        self.terms
            .transform(&self.wildcards, &self.import_data, file_id)
    }

    /// Loads and parse the standard library in the AST cache.
    ///
    /// # RFC007
    ///
    /// This method doesn't populate the term cache. Use [Self::compile_stdlib] afterwards.
    pub fn load_stdlib(&mut self) -> Result<CacheOp<()>, Error> {
        let mut ret = CacheOp::Cached(());

        for (_, file_id) in self.sources.stdlib_modules() {
            if let CacheOp::Done(_) = self.parse_to_ast(file_id)? {
                ret = CacheOp::Done(());
            }
        }

        Ok(ret)
    }

    /// Converts the parsed standard library to the runtime representation.
    pub fn compile_stdlib(&mut self) -> Result<CacheOp<()>, AstCacheError<()>> {
        let mut ret = CacheOp::Cached(());

        for (_, file_id) in self.sources.stdlib_modules() {
            let result = self.compile(file_id).map_err(|cache_err| {
                if let CacheError::IncompatibleState { want } = cache_err {
                    CacheError::IncompatibleState { want }
                } else {
                    unreachable!("unexpected parse error during the compilation of stdlib")
                }
            })?;

            if let CacheOp::Done(_) = result {
                ret = CacheOp::Done(());
            }
        }

        Ok(ret)
    }

    /// Typechecks the standard library. Currently only used in the test suite.
    pub fn typecheck_stdlib(&mut self) -> Result<CacheOp<()>, AstCacheError<TypecheckError>> {
        let (slice, asts) = self.split_asts();
        asts.typecheck_stdlib(slice)
    }

    /// Loads, parses, and compiles the standard library. We don't typecheck for performance
    /// reasons: this is done in the test suite.
    pub fn prepare_stdlib(&mut self) -> Result<(), Error> {
        #[cfg(debug_assertions)]
        if self.skip_stdlib {
            return Ok(());
        }

        self.load_stdlib()?;
        // unwrap(): we just loaded the stdlib, so it must be parsed in the cache.
        self.compile_stdlib().unwrap();

        self.sources
            .stdlib_modules()
            // We need to handle the internals module separately. Each field
            // is bound directly in the environment without evaluating it first, so we can't
            // tolerate top-level let bindings that would be introduced by `transform`.
            .try_for_each(|(_, file_id)| self.transform(file_id).map(|_| ()))
            .map_err(|cache_err: TermCacheError<UnboundTypeVariableError>| {
                Error::ParseErrors(
                    cache_err
                        .unwrap_error(
                            "cache::prepare_stdlib(): unexpected unbound type variable error during stdlib loading",
                        )
                        .into(),
                )
            })?;

        Ok(())
    }

    /// Applies a custom transform to an input and its imports. [CacheError::IncompatibleState] is returned
    /// if the file has not yet been typechecked.
    ///
    /// If multiple invocations of `custom_transform` are needed, you must supply `transform_id` with
    /// with a number higher than that of all previous invocations.
    pub fn custom_transform<E>(
        &mut self,
        file_id: FileId,
        transform_id: usize,
        f: &mut impl FnMut(&mut CacheHub, RichTerm) -> Result<RichTerm, E>,
    ) -> Result<(), TermCacheError<E>> {
        match self.terms.entry_state(file_id) {
            None => Err(CacheError::IncompatibleState {
                want: TermEntryState::Populated,
            }),
            Some(state) => {
                if state.needs_custom_transform(transform_id) {
                    let cached_term = self.terms.terms.remove(&file_id).unwrap();
                    let term = f(self, cached_term.term)?;
                    self.terms.insert(
                        file_id,
                        TermEntry {
                            term,
                            state: TermEntryState::CustomTransforming,
                            ..cached_term
                        },
                    );

                    let imported: Vec<_> = self.import_data.imports(file_id).collect();
                    for file_id in imported {
                        self.custom_transform(file_id, transform_id, f)?;
                    }

                    // TODO: We're setting the state back to whatever it was.
                    // unwrap(): we inserted the term just above
                    let _ = self
                        .terms
                        .update_state(file_id, TermEntryState::CustomTransformed { transform_id })
                        .unwrap();
                }

                Ok(())
            }
        }
    }

    /// Resolves every imports of a term entry of the cache, and update its state accordingly, or
    /// do nothing if the imports of the entry have already been resolved or if they aren't Nickel
    /// inputs. Require that the corresponding source has been parsed.
    ///
    /// If resolved imports contain imports themselves, resolve them recursively. Returns a tuple
    /// of vectors, where the first component is the imports that were transitively resolved, and
    /// the second component is the errors it encountered while resolving imports in `file_id`,
    /// respectively. Imports that were already resolved before are not included in the first
    /// component: this return value is currently used by the LSP to re-run code analysis on new
    /// files/modified files.
    ///
    /// The resolved imports are ordered by a pre-order depth-first-search. In particular, earlier
    /// elements in the returned list might import later elements but -- unless there are cyclic
    /// imports -- later elements do not import earlier elements.
    ///
    /// It only accumulates errors if the cache is in error tolerant mode, otherwise it returns an
    /// `Err(..)` containing  a `CacheError`.
    ///
    /// # RFC007
    ///
    /// This method is still needed only because the evaluator can't handle un-resolved import, so
    /// we need to replace them by resolved imports. However, actual import resolution (loading and
    /// parsing files for the first time) is now driven by typechecking directly.
    pub fn resolve_imports(
        &mut self,
        file_id: FileId,
    ) -> Result<CacheOp<Vec<FileId>>, TermCacheError<ImportError>> {
        let entry = self.terms.terms.get(&file_id);

        match entry {
            Some(TermEntry {
                state,
                term,
                format: InputFormat::Nickel,
            }) if *state < TermEntryState::ImportsResolving => {
                let term = term.clone();

                let import_resolution::strict::ResolveResult {
                    transformed_term,
                    resolved_ids: pending,
                } = import_resolution::strict::resolve_imports(term, self)?;

                // unwrap(): we called `unwrap()` at the beginning of the enclosing if branch
                // on the result of `self.terms.get(&file_id)`. We only made recursive calls to
                // `resolve_imports` in between, which don't remove anything from `self.terms`.
                let cached_term = self.terms.terms.get_mut(&file_id).unwrap();
                cached_term.term = transformed_term;
                cached_term.state = TermEntryState::ImportsResolving;

                let mut done = Vec::new();

                // Transitively resolve the imports, and accumulate the ids of the resolved
                // files along the way.
                for id in pending {
                    if let CacheOp::Done(mut done_local) = self.resolve_imports(id)? {
                        done.push(id);
                        done.append(&mut done_local)
                    }
                }

                // unwrap(): if we are in this branch, the term is present in the cache
                let _ = self
                    .terms
                    .update_state(file_id, TermEntryState::ImportsResolved)
                    .unwrap();

                Ok(CacheOp::Done(done))
            }
            // There's no import to resolve for non-Nickel inputs. We still update the state.
            Some(TermEntry { state, .. }) if *state < TermEntryState::ImportsResolving => {
                // unwrap(): if we are in this branch, the term is present in the cache
                let _ = self
                    .terms
                    .update_state(file_id, TermEntryState::ImportsResolved)
                    .unwrap();
                Ok(CacheOp::Cached(Vec::new()))
            }
            // [^transitory_entry_state]
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
            Some(TermEntry {
                state: TermEntryState::ImportsResolving,
                ..
            }) => Ok(CacheOp::Done(Vec::new())),
            // >= EntryState::ImportsResolved
            Some(_) => Ok(CacheOp::Cached(Vec::new())),
            None => Err(CacheError::IncompatibleState {
                want: TermEntryState::Populated,
            }),
        }
    }

    /// Generate the initial evaluation environment from the list of `file_ids` corresponding to the
    /// standard library parts.
    pub fn mk_eval_env<EC: EvalCache>(&self, eval_cache: &mut EC) -> eval::Environment {
        let mut eval_env = eval::Environment::new();

        for (module, file_id) in self.sources.stdlib_modules() {
            // The internals module needs special treatment: it's required to be a record
            // literal, and its bindings are added directly to the environment
            if let nickel_stdlib::StdlibModule::Internals = module {
                let result = eval::env_add_record(
                    eval_cache,
                    &mut eval_env,
                    Closure::atomic_closure(self.terms.get_owned(file_id).expect(
                        "cache::mk_eval_env(): can't build environment, stdlib not parsed",
                    )),
                );
                if let Err(eval::EnvBuildError::NotARecord(rt)) = result {
                    panic!(
                        "cache::Caches::mk_eval_env(): \
                            expected the stdlib module {} to be a record, got {:?}",
                        self.sources.name(file_id).to_string_lossy().as_ref(),
                        rt
                    )
                }
            } else {
                eval::env_add(
                    eval_cache,
                    &mut eval_env,
                    module.name().into(),
                    self.terms.get_owned(file_id).expect(
                        "cache::Caches::mk_eval_env(): can't build environment, stdlib not parsed",
                    ),
                    eval::Environment::new(),
                );
            }
        }

        eval_env
    }

    /// Loads a new source as a string, replacing any existing source with the same name.
    ///
    /// If there was a previous source with the same name, its `FileId` is reused and the cached
    /// term is deleted.
    ///
    /// Used to store intermediate short-lived generated snippets that needs to have a
    /// corresponding `FileId`, such as when querying or reporting errors.
    pub fn replace_string(&mut self, source_name: SourcePath, s: String) -> FileId {
        if let Some(file_id) = self.sources.id_of(&source_name) {
            self.sources.files.update(file_id, s);
            self.asts.remove(file_id);
            self.terms.terms.remove(&file_id);
            file_id
        } else {
            let file_id = self.sources.files.add(source_name.clone(), s);
            self.sources.file_paths.insert(file_id, source_name.clone());
            self.sources.file_ids.insert(
                source_name,
                NameIdEntry {
                    id: file_id,
                    source: SourceKind::Memory,
                },
            );
            file_id
        }
    }

    pub fn closurize<EC: EvalCache>(
        &mut self,
        eval_cache: &mut EC,
        file_id: FileId,
    ) -> Result<CacheOp<()>, TermCacheError<()>> {
        self.terms.closurize(eval_cache, &self.import_data, file_id)
    }

    /// Add the bindings of a record to the REPL type environment. Ignore fields whose name are
    /// defined through interpolation.
    pub fn add_repl_bindings(&mut self, term: &RichTerm) -> Result<(), NotARecord> {
        let (slice, asts) = self.split_asts();
        asts.add_type_bindings(slice, term)
    }

    /// Converts an AST and all of its transitive dependencies to the runtime representation,
    /// populating the term cache. `file_id` and any of its Nickel dependencies must be present in
    /// the AST cache, or [CacheError::IncompatibleState] is returned. However, for non-Nickel
    /// dependencies, they are instead parsed directly into the term cache,
    ///
    /// "Compile" is anticipating a bit on RFC007, although it is a lowering of the AST
    /// representation to the runtime representation.
    ///
    /// Compilation doesn't have a proper state associated, and thus should always be coupled with
    /// program transformations through [Self::compile_and_transform]. It should preferably not be
    /// observable as an atomic transition, although as far as I can tell, this shouldn't cause
    /// major troubles to do so.
    pub fn compile(&mut self, main_id: FileId) -> Result<CacheOp<()>, AstCacheError<ImportError>> {
        if self.terms.contains(main_id) {
            return Ok(CacheOp::Cached(()));
        }

        // We set the format of the main `file_id` to `Nickel`, even if it is not, to require its
        // presence in either the term cache or the ast cache.
        let mut work_stack = vec![ImportTarget {
            file_id: main_id,
            format: InputFormat::default(),
        }];

        while let Some(ImportTarget { file_id, format }) = work_stack.pop() {
            if self.terms.contains(file_id) {
                continue;
            }

            let entry = if let InputFormat::Nickel = format {
                let ast_entry =
                    self.asts
                        .get_entry(file_id)
                        .ok_or(CacheError::IncompatibleState {
                            want: AstEntryState::Parsed,
                        })?;

                TermEntry {
                    term: ast_entry.ast.to_mainline(),
                    format: ast_entry.format,
                    state: TermEntryState::default(),
                }
            } else {
                // We want to maintain the same error message as before the introduction of the two
                // distinct representations, and their processing in two stages (first Nickel files that
                // have an AST, and then others before evaluation).
                //
                // If we find a non-Nickel file here that needs to be parsed, it's because it's
                // been imported from somewhere else. The error used to be an import error, which
                // includes the location of the importing expression. We thus raise an import error
                // here, in case of failure.
                let term = self
                    .sources
                    .parse_other(file_id, format)
                    .map_err(|parse_err| {
                        CacheError::Error(ImportError::ParseErrors(
                            parse_err.into(),
                            self.import_data
                                .rev_imports
                                .get(&file_id)
                                .and_then(|map| map.get(&main_id))
                                .copied()
                                .unwrap_or_default(),
                        ))
                    })?;

                TermEntry {
                    term,
                    format,
                    state: TermEntryState::default(),
                }
            };

            self.terms.insert(file_id, entry);

            work_stack.extend(
                self.import_data
                    .imports
                    .get(&file_id)
                    .into_iter()
                    .flat_map(|set| set.iter()),
            )
        }

        Ok(CacheOp::Done(()))
    }

    /// Converts an AST entry and all of its transitive dependencies to the runtime representation
    /// (compile), populating the term cache. Applies both import resolution and other program
    /// transformations on the resulting terms.
    pub fn compile_and_transform(
        &mut self,
        file_id: FileId,
    ) -> Result<CacheOp<()>, AstCacheError<Error>> {
        let mut done = false;

        done = matches!(
            self.compile(file_id)
                .map_err(|cache_err| cache_err.map_err(Error::ImportError))?,
            CacheOp::Done(_)
        ) || done;

        let imports = self
            .resolve_imports(file_id)
            // force_cast(): since we compiled `file_id`, the term cache must be populated, and
            // thus `resolve_imports` should never throw `CacheError::IncompatibleState`.
            .map_err(|cache_err| cache_err.map_err(Error::ImportError).force_cast())?;
        done = matches!(imports, CacheOp::Done(_)) || done;

        let transform = self
            .terms
            .transform(&self.wildcards, &self.import_data, file_id)
            // force_cast(): since we compiled `file_id`, the term cache must be populated, and
            // thus `resolve_imports` should never throw `CacheError::IncompatibleState`.
            .map_err(|cache_err| {
                cache_err
                    .map_err(|uvar_err| Error::ParseErrors(ParseErrors::from(uvar_err)))
                    .force_cast()
            })?;
        done = matches!(transform, CacheOp::Done(_)) || done;

        Ok(if done {
            CacheOp::Done(())
        } else {
            CacheOp::Cached(())
        })
    }

    /// Creates a partial copy of this cache for evaluation purposes only. In particular, we don't
    /// copy anything related to arena-allocated ASTs. However, source files, imports data and
    /// terms are copied over, which is useful to make new evaluation caches cheaply, typically for
    /// NLS and benches.
    pub fn clone_for_eval(&self) -> Self {
        Self {
            terms: self.terms.clone(),
            sources: self.sources.clone(),
            asts: AstCache::empty(),
            wildcards: self.wildcards.clone(),
            import_data: self.import_data.clone(),
            #[cfg(debug_assertions)]
            skip_stdlib: self.skip_stdlib,
        }
    }

    /// Split a mutable borrow to self into a mutable borrow of the AST cache and a mutable borrow
    /// of the rest.
    pub fn split_asts(&mut self) -> (CacheHubView<'_>, &mut AstCache) {
        (
            CacheHubView {
                terms: &mut self.terms,
                sources: &mut self.sources,
                wildcards: &mut self.wildcards,
                import_data: &mut self.import_data,
                #[cfg(debug_assertions)]
                skip_stdlib: self.skip_stdlib,
            },
            &mut self.asts,
        )
    }

    /// See [SourceCache::input_format].
    pub fn input_format(&self, file_id: FileId) -> Option<InputFormat> {
        self.sources.input_format(file_id)
    }
}

/// Because ASTs are arena-allocated, the self-referential [ast_cache::AstCache] which holds both
/// the arena and references to this arena often needs special treatment, if we want to make the
/// borrow checker happy. The following structure is basically a view of "everything but the ast
/// cache" into [CacheHub], so that we can separate and pack all the rest in a single structure,
/// making the signature of many [ast_cache::AstCache] methods much lighter.
pub struct CacheHubView<'cache> {
    terms: &'cache mut TermCache,
    sources: &'cache mut SourceCache,
    wildcards: &'cache mut WildcardsCache,
    import_data: &'cache mut ImportData,
    #[cfg(debug_assertions)]
    /// Skip loading the stdlib, used for debugging purpose
    skip_stdlib: bool,
}

impl CacheHubView<'_> {
    /// Make a reborrow of this slice.
    pub fn reborrow(&mut self) -> CacheHubView<'_> {
        CacheHubView {
            terms: self.terms,
            sources: self.sources,
            wildcards: self.wildcards,
            import_data: self.import_data,
            #[cfg(debug_assertions)]
            skip_stdlib: self.skip_stdlib,
        }
    }
}

/// An entry in the term cache. Stores the parsed term together with metadata and state.
#[derive(Debug, Clone, PartialEq)]
pub struct TermEntry {
    pub term: RichTerm,
    pub state: TermEntryState,
    pub format: InputFormat,
}

/// An entry in the AST cache. Stores the parsed term together with metadata and state.
#[derive(Debug, Clone, PartialEq)]
pub struct AstEntry<'ast> {
    pub ast: &'ast Ast<'ast>,
    pub state: AstEntryState,
    pub format: InputFormat,
}

impl<'ast> AstEntry<'ast> {
    /// Creates a new entry with default metadata.
    pub fn new(ast: &'ast Ast<'ast>) -> Self {
        AstEntry {
            ast,
            state: AstEntryState::default(),
            format: InputFormat::default(),
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
///
/// An input that was open as an in-memory file may be closed, namely when the file is closed
/// or deleted from an editor using the LSP. In this case, the file will be read from the
/// filesystem again instead of using the in-memory value. Closing a file only makes sense in the
/// case that the [SourcePath] refers to a path on the filesystem. Other types of in-memory files,
/// like the standard library, cannot be closed.
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Copy, Clone)]
enum SourceKind {
    Filesystem(SystemTime),
    Memory,
    MemoryClosed,
}

/// The errors that can occur while closing an in memory file.
#[derive(Debug, Clone)]
pub enum FileCloseError {
    /// The file was not closed because no mapping of the source path to a [FileId] could be
    /// found.
    FileIdNotFound,
    /// A file with the given path was found, but it was not open in memory.
    FileNotOpen,
}

impl fmt::Display for FileCloseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            FileCloseError::FileIdNotFound => {
                write!(
                    f,
                    "No file ID could be found for the file path to be closed."
                )
            }
            FileCloseError::FileNotOpen => {
                write!(f, "Attempted to close a file that was not open in-memory.")
            }
        }
    }
}

impl std::error::Error for FileCloseError {}

/// Contains information about the closed in-memory file and its replacement from the filesystem
/// in the case that an in-memory file was closed successfully.
pub struct FileCloseResult {
    /// The [FileId] of the in-memory file that was closed.
    pub closed_id: FileId,
    /// The [FileId] of the file loaded from the filesystem with the same path as the closed
    /// file, or an error indicating why the file could not be opened.
    /// An error would be expected here in the case that the file was deleted, which would
    /// also send a close file notification to the LSP.
    pub replacement_id: Result<FileId, io::Error>,
}

/// Cache entries for sources.
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
/// Usually, when applying a procedure to a term entry (e.g. program transformations), we process
/// all of its transitive imports as well. We start by processing the entry, updating the state to
/// `XXXing` (ex: `Typechecking`) upon success. Only when all the imports have been successfully
/// processed, the state is updated to `XXXed` (ex: `Typechecked`).
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Copy, Clone, Default)]
pub enum TermEntryState {
    /// The initial state. The term is in the cache but hasn't been processed further yet.
    #[default]
    Populated,
    /// A custom transformation of the entry (through `Program::custom_transform`) is underway.
    CustomTransforming,
    /// This entry has completed custom transformations of this ID and lower.
    CustomTransformed { transform_id: usize },
    /// The imports of the entry have been resolved, and the imports of its (transitive) imports are
    /// being resolved.
    ImportsResolving,
    /// The imports of the entry and its transitive dependencies has been resolved.
    ImportsResolved,
    /// The entry have been transformed, and its (transitive) imports are being transformed.
    Transforming,
    /// The entry and its transitive imports have been transformed.
    Transformed,
    /// The entry has been closurized.
    Closurized,
}

impl TermEntryState {
    fn needs_custom_transform(&self, transform_id: usize) -> bool {
        if let TermEntryState::CustomTransformed {
            transform_id: done_transform_id,
        } = self
        {
            transform_id > *done_transform_id
        } else {
            *self < TermEntryState::CustomTransforming
        }
    }
}

/// The state of an entry in the AST cache. Equivalent of [TermEntryState] but for ASTs.
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Copy, Clone, Default)]
pub enum AstEntryState {
    /// The initial state. The AST is in the cache but hasn't been processed further yet.
    #[default]
    Parsed,
    /// The entry have been typechecked, and its (transitive) imports are being typechecked.
    Typechecking,
    /// The entry and its transitive imports have been typechecked.
    Typechecked,
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
///
/// # Type parameters
///
/// - `E`: the underlying, wrapped error type
/// - `S`: the entry state, whether [TermEntryState] or [AstEntryState] in practice.
#[derive(Eq, PartialEq, Debug, Clone)]
pub enum CacheError<E, S> {
    Error(E),
    /// The state of the entry in the cache is incompatible with the requested operation.
    IncompatibleState {
        want: S,
    },
}

pub type AstCacheError<E> = CacheError<E, AstEntryState>;
pub type TermCacheError<E> = CacheError<E, TermEntryState>;

impl<E, S> From<E> for CacheError<E, S> {
    fn from(e: E) -> Self {
        CacheError::Error(e)
    }
}

impl<E, S> CacheError<E, S> {
    #[track_caller]
    pub fn unwrap_error(self, msg: &str) -> E {
        match self {
            CacheError::Error(err) => err,
            CacheError::IncompatibleState { .. } => panic!("{}", msg),
        }
    }

    pub fn map_err<O>(self, f: impl FnOnce(E) -> O) -> CacheError<O, S> {
        match self {
            CacheError::Error(e) => CacheError::Error(f(e)),
            CacheError::IncompatibleState { want } => CacheError::IncompatibleState { want },
        }
    }

    /// Assuming that `self` is of the form `CacheError::Error(e)`, cast the error type to another
    /// arbitrary state type `T`.
    ///
    /// # Panic
    ///
    /// This method panics if `self` is [CacheError::IncompatibleState].
    #[track_caller]
    pub fn force_cast<T>(self) -> CacheError<E, T> {
        match self {
            CacheError::Error(e) => CacheError::Error(e),
            CacheError::IncompatibleState { want: _ } => panic!(),
        }
    }
}

/// Input data usually comes from files on the file system, but there are also lots of cases where
/// we want to synthesize other kinds of inputs.
///
/// Note that a [SourcePath] does not uniquely identify a cached input:
///
/// - Some functions (like [SourceCache::add_file]) add a new cached input unconditionally.
/// - [`SourceCache::get_or_add_file`] will add a new cached input at the same `SourcePath` if the file
///   on disk was updated.
///
/// The equality checking of `SourcePath` only affects [SourceCache::replace_string], which
/// overwrites any previous cached input with the same `SourcePath`.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum SourcePath {
    /// A file at the given path.
    ///
    /// Note that this does not need to be a real file on the filesystem: it could still be loaded
    /// from memory by, e.g, [`SourceCache::add_string`].
    ///
    /// This is the only `SourcePath` variant that can be resolved as the target of an import
    /// statement.
    Path(PathBuf, InputFormat),
    /// A subrange of a file at the given path.
    ///
    /// This is used by NLS to analyze small parts of files that don't fully parse. The original
    /// file path is preserved, because it's needed for resolving imports.
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
            SourcePath::Path(p, _) | SourcePath::Snippet(p) => Ok(p.as_os_str()),
            _ => Err(()),
        }
    }
}

// [`Files`] needs to have an OsString for each file, so we synthesize names even for sources that
// don't have them. They don't need to be unique; they're just used for diagnostics.
impl From<SourcePath> for OsString {
    fn from(source_path: SourcePath) -> Self {
        match source_path {
            SourcePath::Path(p, _) | SourcePath::Snippet(p) => p.into(),
            SourcePath::Std(StdlibModule::Std) => "<stdlib/std.ncl>".into(),
            SourcePath::Std(StdlibModule::Internals) => "<stdlib/internals.ncl>".into(),
            SourcePath::Query => "<query>".into(),
            SourcePath::ReplInput(idx) => format!("<repl-input-{idx}>").into(),
            SourcePath::ReplTypecheck => "<repl-typecheck>".into(),
            SourcePath::ReplQuery => "<repl-query>".into(),
            SourcePath::CliFieldAssignment => "<cli-assignment>".into(),
            SourcePath::Override(path) => format!("<override {path}>",).into(),
            SourcePath::Generated(description) => format!("<generated {description}>").into(),
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
    /// The source is stale because it came from a file on disk that has since been updated. The
    /// data is the timestamp of the new version of the file.
    Stale(SystemTime),
}

/// Abstract the access to imported files and the import cache. Used by the evaluator and at the
/// [import resolution](crate::transform::import_resolution) phase.
///
/// The standard implementation uses 2 caches, the file cache for raw contents and the term cache
/// for parsed contents, mirroring the 2 steps when resolving an import:
///
/// 1. When an import is encountered for the first time, the content of the corresponding file is
///    read and stored in the file cache (consisting of the file database plus a map between paths
///    and ids in the database, the name-id table). The content is parsed, stored in the term
///    cache, and queued somewhere so that it can undergo the standard
///    [transformations](crate::transform) (including import resolution) later.
/// 2. When it is finally processed, the term cache is updated with the transformed term.
///
/// # RFC007
///
/// Import resolution on the old representation is still needed only because of the evaluator. The
/// typechecker now uses the new AST representation with its own import resolver.
pub trait ImportResolver {
    /// Resolves an import.
    ///
    /// Reads and stores the content of an import, puts it in the file cache (or get it from there
    /// if it is cached), then parses it and returns the corresponding term and file id.
    ///
    /// The term and the path are provided only if the import is processed for the first time.
    /// Indeed, at import resolution phase, the term of an import encountered for the first time is
    /// queued to be processed (e.g. having its own imports resolved). The path is needed to
    /// resolve nested imports relatively to this parent. Only after this processing the term is
    /// inserted back in the cache. On the other hand, if it has been resolved before, it is
    /// already transformed in the cache and do not need further processing.
    fn resolve(
        &mut self,
        import: &term::Import,
        parent: Option<FileId>,
        pos: &TermPos,
    ) -> Result<(ResolvedTerm, FileId), ImportError>;

    /// Return a reference to the file database.
    fn files(&self) -> &Files;

    /// Get a resolved import from the term cache.
    fn get(&self, file_id: FileId) -> Option<RichTerm>;
    /// Return the (potentially normalized) file path corresponding to the ID of a resolved import.
    fn get_path(&self, file_id: FileId) -> Option<&OsStr>;

    /// Returns the base path for Nix evaluation, which is the parent directory of the source file
    /// if any, or the current working directory, or an empty path if we couldn't determine any of
    /// the previous two.
    ///
    /// This method need to be here because the evaluator makes use of it (when evaluating the
    /// `eval_nix` primop), but at this stage it only has access to the `ImportResolver` interface.
    /// We could give a default implementation here just using [Self::get_path], but we also need
    /// `get_base_dir_for_nix` in [SourceCache]. We reuse the latter `implementation instead of
    /// duplicating a more generic variant here.
    #[cfg(feature = "nix-experimental")]
    fn get_base_dir_for_nix(&self, file_id: FileId) -> PathBuf;
}

impl ImportResolver for CacheHub {
    fn resolve(
        &mut self,
        import: &term::Import,
        parent: Option<FileId>,
        pos: &TermPos,
    ) -> Result<(ResolvedTerm, FileId), ImportError> {
        let (possible_parents, path, pkg_id, format) = match import {
            term::Import::Path { path, format } => {
                // `parent` is the file that did the import. We first look in its containing directory, followed by
                // the directories in the import path.
                let mut parent_path = parent
                    .and_then(|p| self.get_path(p))
                    .map(PathBuf::from)
                    .unwrap_or_default();
                parent_path.pop();

                (
                    std::iter::once(parent_path)
                        .chain(self.sources.import_paths.iter().cloned())
                        .collect(),
                    Path::new(path),
                    None,
                    *format,
                )
            }
            term::Import::Package { id } => {
                let package_map = self
                    .sources
                    .package_map
                    .as_ref()
                    .ok_or(ImportError::NoPackageMap { pos: *pos })?;
                let parent_path = parent
                    .and_then(|p| self.sources.packages.get(&p))
                    .map(PathBuf::as_path);
                let pkg_path = package_map.get(parent_path, *id, *pos)?;
                (
                    vec![pkg_path.to_owned()],
                    Path::new("main.ncl"),
                    Some(pkg_path.to_owned()),
                    // Packages are always in nickel format
                    InputFormat::Nickel,
                )
            }
        };

        // Try to import from all possibilities, taking the first one that succeeds.
        let (id_op, path_buf) = possible_parents
            .iter()
            .find_map(|parent| {
                let mut path_buf = parent.clone();
                path_buf.push(path);
                self.sources
                    .get_or_add_file(&path_buf, format)
                    .ok()
                    .map(|x| (x, path_buf))
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

        let (result, file_id) = match id_op {
            CacheOp::Cached(id) => (ResolvedTerm::FromCache, id),
            CacheOp::Done(id) => (ResolvedTerm::FromFile { path: path_buf }, id),
        };

        if let Some(parent) = parent {
            self.import_data
                .imports
                .entry(parent)
                .or_default()
                .insert(ImportTarget { file_id, format });
            self.import_data
                .rev_imports
                .entry(file_id)
                .or_default()
                .entry(parent)
                .or_insert(*pos);
        }

        self.parse_to_term(file_id, format)
            .map_err(|err| ImportError::ParseErrors(err, *pos))?;

        if let Some(pkg_id) = pkg_id {
            self.sources.packages.insert(file_id, pkg_id);
        }

        Ok((result, file_id))
    }

    fn files(&self) -> &Files {
        &self.sources.files
    }

    fn get(&self, file_id: FileId) -> Option<RichTerm> {
        self.terms
            .terms
            .get(&file_id)
            .map(|TermEntry { term, .. }| term.clone())
    }

    fn get_path(&self, file_id: FileId) -> Option<&OsStr> {
        self.sources
            .file_paths
            .get(&file_id)
            .and_then(|p| p.try_into().ok())
    }

    #[cfg(feature = "nix-experimental")]
    fn get_base_dir_for_nix(&self, file_id: FileId) -> PathBuf {
        self.sources.get_base_dir_for_nix(file_id)
    }
}

/// Import resolution for new AST representation (RFC007).
pub trait AstImportResolver {
    /// Resolves an import to an AST.
    ///
    /// Reads and stores the content of an import, puts it in the file cache (or gets it from there
    /// if it is cached), then parses it and returns the corresponding term and file id.
    ///
    /// The term and the path are provided only if the import is processed for the first time.
    /// Indeed, at import resolution phase, the term of an import encountered for the first time is
    /// queued to be processed (e.g. having its own imports resolved). The path is needed to
    /// resolve nested imports relatively to this parent. Only after this processing the term is
    /// inserted back in the cache. On the other hand, if it has been resolved before, it is
    /// already transformed in the cache and do not need further processing.
    ///
    /// # Returns
    ///
    /// [Self::resolve] returns `Ok(None)` if the import is an external format, which can currently
    /// be serialized directly to he runtime representation ([crate::bytecode::value::NickelValue])
    /// without going through an AST. AST import resolution is mostly used by the typechecker, and
    /// the typechecker currently ignores external formats anyway.
    ///
    /// # Lifetimes
    ///
    /// The signature is parametrized by two different lifetimes. This is due mostly to NLS: in the
    /// normal Nickel pipeline, all the ASTs are currently allocated in the same arena, and their
    /// lifetime is the same. However, in NLS, each files needs to be managed separately. At the
    /// import boundary, we're thus not guaranteed to get an AST that lives as long as the one
    /// being currently typechecked.
    fn resolve<'ast_out>(
        &'ast_out mut self,
        import: &ast::Import<'_>,
        pos: &TermPos,
    ) -> Result<Option<&'ast_out Ast<'ast_out>>, ImportError>;
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
/// CAUTION: This does not resolve symlinks (unlike [`std::fs::canonicalize`]). This may cause
/// incorrect or surprising behavior at times. This should be used carefully. Unfortunately,
/// [`std::fs::canonicalize`] can be hard to use correctly, since it can often fail, or on Windows
/// returns annoying device paths. This is a problem Cargo needs to improve on.
pub fn normalize_abs_path(path: &Path) -> PathBuf {
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

/// Normalize a relative path, removing mid-path `..`s.
///
/// Like [`normalize_abs_path`], this works only on the path itself (i.e. not the filesystem) and
/// does not follow symlinks.
pub fn normalize_rel_path(path: &Path) -> PathBuf {
    use std::path::Component;

    let mut components = path.components().peekable();
    let mut parents = PathBuf::new();
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
                if !ret.pop() {
                    parents.push(Component::ParentDir);
                }
            }
            Component::Normal(c) => {
                ret.push(c);
            }
        }
    }
    parents.extend(ret.components());
    parents
}

/// Returns the timestamp of a file. Return `None` if an IO error occurred.
pub fn timestamp(path: impl AsRef<OsStr>) -> io::Result<SystemTime> {
    fs::metadata(path.as_ref())?.modified()
}

/// As RFC007 is being rolled out, the typechecker now needs to operate on the new AST. We need a
/// structure that implements [AstImportResolver].
///
/// For borrowing reasons, this can't be all of [CacheHub] or all of [ast_cache::AstCache], as we
/// need to split the different things that are borrowed mutably or immutably. `AstResolver` is a
/// structure that borrows some parts of the cache during its lifetime and will retrieve alredy
/// imported ASTs, or register the newly imported ones in a separate hashmap that can be added back
/// to the original cache once import resolution is done.
pub struct AstResolver<'ast, 'cache> {
    /// The AST allocator used to parse new sources.
    alloc: &'ast AstAlloc,
    /// The AST cache, which is added to as import resolution progresses.
    asts: &'cache mut HashMap<FileId, AstEntry<'ast>>,
    /// The source cache where new sources will be stored.
    sources: &'cache mut SourceCache,
    /// Direct and reverse dependencies of files (with respect to imports).
    import_data: &'cache mut ImportData,
}

impl<'ast, 'cache> AstResolver<'ast, 'cache> {
    /// Create a new `AstResolver` from an allocator, an ast cache and a cache hub slice.
    pub fn new(
        alloc: &'ast AstAlloc,
        asts: &'cache mut HashMap<FileId, AstEntry<'ast>>,
        slice: CacheHubView<'cache>,
    ) -> Self {
        Self {
            alloc,
            asts,
            sources: slice.sources,
            import_data: slice.import_data,
        }
    }
}

impl AstImportResolver for AstResolver<'_, '_> {
    fn resolve(
        &mut self,
        import: &ast::Import<'_>,
        pos: &TermPos,
    ) -> Result<Option<&Ast<'_>>, ImportError> {
        let parent_id = pos.src_id();

        let (possible_parents, path, pkg_id, format) = match import {
            ast::Import::Path { path, format } => {
                // `parent` is the file that did the import. We first look in its containing
                // directory, followed by the directories in the import path.
                let parent_path = parent_id
                    .and_then(|parent| self.sources.file_paths.get(&parent))
                    .and_then(|path| <&OsStr>::try_from(path).ok())
                    .map(PathBuf::from)
                    .map(|mut path| {
                        path.pop();
                        path
                    })
                    // If the parent isn't a proper file, we look in the current directory instead.
                    // This is useful when importing e.g. from the REPL or the CLI directly.
                    .unwrap_or_default();

                (
                    std::iter::once(parent_path)
                        .chain(self.sources.import_paths.iter().cloned())
                        .collect(),
                    Path::new(path),
                    None,
                    *format,
                )
            }
            ast::Import::Package { id } => {
                let package_map = self
                    .sources
                    .package_map
                    .as_ref()
                    .ok_or(ImportError::NoPackageMap { pos: *pos })?;
                let parent_path = parent_id
                    .and_then(|p| self.sources.packages.get(&p))
                    .map(PathBuf::as_path);
                let pkg_path = package_map.get(parent_path, *id, *pos)?;
                (
                    vec![pkg_path.to_owned()],
                    Path::new("main.ncl"),
                    Some(pkg_path.to_owned()),
                    // Packages are always in nickel format
                    InputFormat::Nickel,
                )
            }
        };

        // Try to import from all possibilities, taking the first one that succeeds.
        let id_op = possible_parents
            .iter()
            .find_map(|parent| {
                let mut path_buf = parent.clone();
                path_buf.push(path);
                self.sources.get_or_add_file(&path_buf, format).ok()
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

        let file_id = id_op.inner();

        if let Some(parent_id) = parent_id {
            self.import_data
                .imports
                .entry(parent_id)
                .or_default()
                .insert(ImportTarget { file_id, format });
            self.import_data
                .rev_imports
                .entry(file_id)
                .or_default()
                .entry(parent_id)
                .or_insert(*pos);
        }

        if let Some(pkg_id) = pkg_id {
            self.sources.packages.insert(file_id, pkg_id);
        }

        if let InputFormat::Nickel = format {
            if let Some(entry) = self.asts.get(&file_id) {
                Ok(Some(entry.ast))
            } else {
                let ast = parse_nickel(self.alloc, file_id, self.sources.files.source(file_id))
                    .map_err(|parse_err| ImportError::ParseErrors(parse_err, *pos))?;
                let ast = self.alloc.alloc(ast);
                self.asts.insert(file_id, AstEntry::new(ast));

                Ok(Some(ast))
            }
        } else {
            // Currently, non-Nickel file are just ignored during the AST file. They are parsed
            // later directly into the runtime
            Ok(None)
        }
    }
}

/// Provide mockup import resolvers for testing purpose.
pub mod resolvers {
    use super::*;
    use crate::term::Import;

    /// A dummy resolver that panics when asked to do something. Used to test code that contains no
    /// import.
    pub struct DummyResolver {}

    impl ImportResolver for DummyResolver {
        fn resolve(
            &mut self,
            _import: &Import,
            _parent: Option<FileId>,
            _pos: &TermPos,
        ) -> Result<(ResolvedTerm, FileId), ImportError> {
            panic!("cache::resolvers: dummy resolver should not have been invoked");
        }

        fn files(&self) -> &Files {
            panic!("cache::resolvers: dummy resolver should not have been invoked");
        }

        fn get(&self, _file_id: FileId) -> Option<RichTerm> {
            panic!("cache::resolvers: dummy resolver should not have been invoked");
        }

        fn get_path(&self, _file_id: FileId) -> Option<&OsStr> {
            panic!("cache::resolvers: dummy resolver should not have been invoked");
        }

        #[cfg(feature = "nix-experimental")]
        fn get_base_dir_for_nix(&self, _file_id: FileId) -> PathBuf {
            panic!("cache::resolvers: dummy resolver should not have been invoked");
        }
    }

    /// Resolve imports from a mockup file database. Used to test imports without accessing the
    /// file system. File name are stored as strings, and silently converted from/to `OsString`
    /// when needed: don't use this resolver with source code that import non UTF-8 paths.
    #[derive(Clone, Default)]
    pub struct SimpleResolver {
        files: Files,
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
            import: &Import,
            _parent: Option<FileId>,
            pos: &TermPos,
        ) -> Result<(ResolvedTerm, FileId), ImportError> {
            let Import::Path { path, .. } = import else {
                panic!("simple resolver doesn't support packages");
            };

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
                let alloc = AstAlloc::new();

                let ast = parser::grammar::TermParser::new()
                    .parse_strict(&alloc, file_id, Lexer::new(buf))
                    .map_err(|e| ImportError::ParseErrors(e, *pos))?;
                e.insert(ast.to_mainline());

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

        fn files(&self) -> &Files {
            &self.files
        }

        fn get(&self, file_id: FileId) -> Option<RichTerm> {
            self.term_cache.get(&file_id).cloned()
        }

        fn get_path(&self, file_id: FileId) -> Option<&OsStr> {
            Some(self.files.name(file_id))
        }

        #[cfg(feature = "nix-experimental")]
        fn get_base_dir_for_nix(&self, file_id: FileId) -> PathBuf {
            self.get_path(file_id)
                .and_then(|path| Path::new(path).parent())
                .map(PathBuf::from)
                .unwrap_or_default()
        }
    }
}

/// Parses a Nickel expression from a string.
fn parse_nickel<'ast>(
    alloc: &'ast AstAlloc,
    file_id: FileId,
    source: &str,
) -> Result<Ast<'ast>, ParseErrors> {
    let ast = measure_runtime!(
        "runtime:parse:nickel",
        parser::grammar::TermParser::new().parse_strict(alloc, file_id, Lexer::new(source))?
    );

    Ok(ast)
}

// Parse a Nickel REPL input. In addition to normal Nickel expressions, it can be a top-level let.
fn parse_nickel_repl<'ast>(
    alloc: &'ast AstAlloc,
    file_id: FileId,
    source: &str,
) -> Result<ExtendedTerm<Ast<'ast>>, ParseErrors> {
    let et = measure_runtime!(
        "runtime:parse:nickel",
        parser::grammar::ExtendedTermParser::new().parse_strict(
            alloc,
            file_id,
            Lexer::new(source)
        )?
    );

    Ok(et)
}

/// AST cache (for the new [crate::bytecode::ast::Ast]) that holds the owned allocator of the AST
/// nodes.
mod ast_cache {
    use super::*;
    /// The AST cache packing together the AST allocator and the cached ASTs.
    #[self_referencing]
    pub struct AstCache {
        /// The allocator hosting AST nodes.
        alloc: AstAlloc,
        /// An AST for each file we have cached.
        #[borrows(alloc)]
        #[covariant]
        asts: HashMap<FileId, AstEntry<'this>>,
        /// The initial typing context. It's morally an option (unitialized at first), but we just
        /// use an empty context as a default value.
        ///
        /// This context can be augmented through [AstCache::add_repl_binding] and
        /// [AstCache::add_repl_bindings], which is typically used in the REPL to add top-level
        /// bindings.
        #[borrows(alloc)]
        #[not_covariant]
        type_ctxt: typecheck::Context<'this>,
    }

    impl AstCache {
        /// Construct a new, empty, AST cache.
        pub fn empty() -> Self {
            AstCache::new(
                AstAlloc::new(),
                |_alloc| HashMap::new(),
                |_alloc| typecheck::Context::new(),
            )
        }

        /// Clears the allocator and the cached ASTs.
        pub fn clear(&mut self) {
            *self = Self::empty();
        }

        /// Returns `true` if the AST cache contains an entry for the given file id.
        pub fn contains(&self, file_id: FileId) -> bool {
            self.borrow_asts().contains_key(&file_id)
        }

        /// Returns the underlying allocator, which might be required to call various helpers.
        pub fn get_alloc(&self) -> &AstAlloc {
            self.borrow_alloc()
        }

        /// Returns a reference to a cached AST.
        pub fn get(&self, file_id: FileId) -> Option<&Ast<'_>> {
            self.borrow_asts().get(&file_id).map(|entry| entry.ast)
        }

        /// Returns a reference to a cached AST entry.
        pub fn get_entry(&self, file_id: FileId) -> Option<&AstEntry<'_>> {
            self.borrow_asts().get(&file_id)
        }

        /// Retrieves the state of an entry. Returns `None` if the entry is not in the AST cache.
        pub fn entry_state(&self, file_id: FileId) -> Option<AstEntryState> {
            self.borrow_asts()
                .get(&file_id)
                .map(|AstEntry { state, .. }| *state)
        }

        /// Updates the state of an entry and returns the previous state, or an error if the entry
        /// isn't in the cache.
        pub fn update_state(
            &mut self,
            file_id: FileId,
            new: AstEntryState,
        ) -> Result<AstEntryState, TermNotFound> {
            self.with_asts_mut(|asts| {
                asts.get_mut(&file_id)
                    .map(|AstEntry { state, .. }| std::mem::replace(state, new))
            })
            .ok_or(TermNotFound)
        }

        /// Parses a Nickel expression and stores the corresponding AST in the cache.
        pub fn parse_nickel<'ast>(
            &'ast mut self,
            file_id: FileId,
            source: &str,
        ) -> Result<&'ast Ast<'ast>, ParseErrors> {
            self.with_mut(|slf| {
                let ast = parse_nickel(slf.alloc, file_id, source)?;
                let ast = slf.alloc.alloc(ast);
                slf.asts.insert(file_id, AstEntry::new(ast));

                Ok(ast)
            })
        }

        /// Same as [Self::parse_nickel] but accepts the extended syntax allowed in the REPL.
        ///
        /// **Caution**: this method doesn't cache the potential id of a top-level let binding,
        /// although it does save the bound expression, which is required later for typechecking,
        /// program transformation, etc.
        pub fn parse_nickel_repl<'ast>(
            &'ast mut self,
            file_id: FileId,
            source: &str,
        ) -> Result<ExtendedTerm<Ast<'ast>>, ParseErrors> {
            self.with_mut(|slf| {
                let extd_ast = parse_nickel_repl(slf.alloc, file_id, source)?;

                let ast = match &extd_ast {
                    ExtendedTerm::Term(t) | ExtendedTerm::ToplevelLet(_, t) => {
                        slf.alloc.alloc(t.clone())
                    }
                };

                slf.asts.insert(file_id, AstEntry::new(ast));

                Ok(extd_ast)
            })
        }

        pub fn remove(&mut self, file_id: FileId) -> Option<AstEntry<'_>> {
            self.with_asts_mut(|asts| asts.remove(&file_id))
        }

        /// Typechecks an entry of the cache and updates its state accordingly, or does nothing if
        /// the entry has already been typechecked. Requires that the corresponding source has been
        /// parsed. Note that this method currently fail on a non-Nickel file, that can't have been
        /// parsed to an AST.
        ///
        /// If the source contains imports, recursively typecheck on the imports too.
        ///
        /// # RFC007
        ///
        /// During the transition period between the old VM and the new bytecode VM, this method
        /// performs typechecking on the new representation [crate::bytecode::ast::Ast], and is also
        /// responsible for then converting the term to the legacy representation and populate the
        /// corresponding term cache.
        pub fn typecheck(
            &mut self,
            mut slice: CacheHubView<'_>,
            file_id: FileId,
            initial_mode: TypecheckMode,
        ) -> Result<CacheOp<()>, AstCacheError<TypecheckError>> {
            let Some(state) = self.entry_state(file_id) else {
                return Err(CacheError::IncompatibleState {
                    want: AstEntryState::Parsed,
                });
            };

            // If we're already typechecking or we have typechecked the file, we stop right here.
            if state >= AstEntryState::Typechecking {
                return Ok(CacheOp::Cached(()));
            }

            // Protect against cycles in the import graph.
            // unwrap(): we checked at the beginning of this function that the term is in the
            // cache.
            let _ = self
                .update_state(file_id, AstEntryState::Typechecking)
                .unwrap();

            // Ensure the initial typing context is properly initialized.
            self.populate_type_ctxt(slice.sources);
            self.with_mut(|slf| -> Result<(), AstCacheError<TypecheckError>> {
                // unwrap(): we checked at the beginning of this function that the AST cache has an
                // entry for `file_id`.
                let ast = slf.asts.get(&file_id).unwrap().ast;

                let mut resolver = AstResolver::new(slf.alloc, slf.asts, slice.reborrow());
                let type_ctxt = slf.type_ctxt.clone();
                let wildcards_map = measure_runtime!(
                    "runtime:type_check",
                    typecheck(slf.alloc, ast, type_ctxt, &mut resolver, initial_mode)?
                );
                slice.wildcards.wildcards.insert(
                    file_id,
                    wildcards_map.iter().map(ToMainline::to_mainline).collect(),
                );
                Ok(())
            })?;

            // Typecheck dependencies (files imported by this file).
            if let Some(imports) = slice.import_data.imports.get(&file_id) {
                // Because we need to borrow `import_data` for typechecking, we need to release the
                // borrow by moving the content of `imports` somewhere else.
                //
                // We ignore non-Nickel imports, which aren't typechecked, and are currently not
                // even in the AST cache.
                let imports: Vec<_> = imports
                    .iter()
                    .filter_map(|tgt| {
                        if let InputFormat::Nickel = tgt.format {
                            Some(tgt.file_id)
                        } else {
                            None
                        }
                    })
                    .collect();

                for file_id in imports {
                    self.typecheck(slice.reborrow(), file_id, initial_mode)?;
                }
            }

            // unwrap(): we checked at the beginning of this function that the AST is in the
            // cache.
            let _ = self
                .update_state(file_id, AstEntryState::Typechecked)
                .unwrap();

            Ok(CacheOp::Done(()))
        }

        /// Typechecks the stdlib. This has to be public because it's used in benches. It probably
        /// does not have to be used for something else.
        pub fn typecheck_stdlib(
            &mut self,
            mut slice: CacheHubView<'_>,
        ) -> Result<CacheOp<()>, AstCacheError<TypecheckError>> {
            let mut ret = CacheOp::Cached(());
            self.populate_type_ctxt(slice.sources);

            for (_, stdlib_module_id) in slice.sources.stdlib_modules() {
                let result =
                    self.typecheck(slice.reborrow(), stdlib_module_id, TypecheckMode::Walk)?;

                if let CacheOp::Done(()) = result {
                    ret = CacheOp::Done(());
                }
            }

            Ok(ret)
        }

        /// Typechecks a file (if it wasn't already) and returns the inferred type, with type
        /// wildcards properly substituted.
        pub fn type_of(
            &mut self,
            mut slice: CacheHubView<'_>,
            file_id: FileId,
        ) -> Result<CacheOp<mainline_typ::Type>, AstCacheError<TypecheckError>> {
            self.typecheck(slice.reborrow(), file_id, TypecheckMode::Walk)?;

            let typ: Result<ast::typ::Type<'_>, AstCacheError<TypecheckError>> =
                self.with_mut(|slf| {
                    let ast = slf
                        .asts
                        .get(&file_id)
                        .ok_or(CacheError::IncompatibleState {
                            want: AstEntryState::Parsed,
                        })?
                        .ast;

                    let mut resolver = AstResolver::new(slf.alloc, slf.asts, slice.reborrow());
                    let type_ctxt = slf.type_ctxt.clone();

                    let typ = TryConvert::try_convert(
                        slf.alloc,
                        ast.apparent_type(
                            slf.alloc,
                            Some(&type_ctxt.type_env),
                            Some(&mut resolver),
                        ),
                    )
                    .unwrap_or(ast::typ::TypeF::Dyn.into());
                    Ok(typ)
                });
            let typ = typ?;

            let target: mainline_typ::Type = typ.to_mainline();

            // unwrap(): we ensured that the file is typechecked, thus its wildcards and its AST
            // must be populated
            let wildcards = slice.wildcards.get(file_id).unwrap();

            Ok(CacheOp::Done(
                target
                    .traverse(
                        &mut |ty: mainline_typ::Type| -> Result<_, std::convert::Infallible> {
                            if let mainline_typ::TypeF::Wildcard(id) = ty.typ {
                                Ok(wildcards
                                    .get(id)
                                    .cloned()
                                    .unwrap_or(mainline_typ::Type::from(mainline_typ::TypeF::Dyn)))
                            } else {
                                Ok(ty)
                            }
                        },
                        TraverseOrder::TopDown,
                    )
                    .unwrap(),
            ))
        }

        /// If the type context hasn't been created yet, generate and cache the initial typing
        /// context from the list of `file_ids` corresponding to the standard library parts.
        /// Otherwise, do nothing.
        fn populate_type_ctxt(&mut self, sources: &SourceCache) {
            self.with_mut(|slf| {
                if !slf.type_ctxt.is_empty() {
                    return;
                }
                let stdlib_terms_vec: Vec<(StdlibModule, &'_ Ast<'_>)> = sources
                    .stdlib_modules()
                    .map(|(module, file_id)| {
                        let ast = slf.asts.get(&file_id).map(|entry| entry.ast);

                        (
                            module,
                            ast.expect("cache::ast_cache::AstCache::populate_type_ctxt(): can't build environment, stdlib not parsed")
                        )
                    })
                    .collect();

                *slf.type_ctxt = typecheck::mk_initial_ctxt(slf.alloc, stdlib_terms_vec).unwrap();
            });
        }

        /// Adds a binding to the type environment. The bound term is identified by its file id
        /// `file_id`.
        pub fn add_type_binding(
            &mut self,
            mut slice: CacheHubView<'_>,
            id: LocIdent,
            file_id: FileId,
        ) -> Result<(), AstCacheError<std::convert::Infallible>> {
            self.with_mut(|slf| {
                let Some(entry) = slf.asts.get(&file_id) else {
                    return Err(CacheError::IncompatibleState {
                        want: AstEntryState::Parsed,
                    });
                };

                let ast = entry.ast;
                let mut resolver = AstResolver::new(slf.alloc, slf.asts, slice.reborrow());

                typecheck::env_add(
                    slf.alloc,
                    &mut slf.type_ctxt.type_env,
                    id,
                    ast,
                    &slf.type_ctxt.term_env,
                    &mut resolver,
                );
                //slf.asts.extend(resolver.new_asts.into_iter());

                slf.type_ctxt
                    .term_env
                    .0
                    .insert(id.ident(), (ast.clone(), slf.type_ctxt.term_env.clone()));
                Ok(())
            })?;

            Ok(())
        }

        /// Add the bindings of a record to the type environment. Ignore fields whose name are
        /// defined through interpolation.
        pub fn add_type_bindings(
            &mut self,
            mut slice: CacheHubView<'_>,
            term: &RichTerm,
        ) -> Result<(), NotARecord> {
            self.with_mut(|slf| {
                // It's sad, but for now, we have to convert the term back to an AST to insert it in
                // the type environment.
                let ast = term.to_ast(slf.alloc);
                let mut resolver = AstResolver::new(slf.alloc, slf.asts, slice.reborrow());

                let ret = typecheck::env_add_term(
                    slf.alloc,
                    &mut slf.type_ctxt.type_env,
                    ast,
                    &slf.type_ctxt.term_env,
                    &mut resolver,
                )
                .map_err(|_| NotARecord);
                ret
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;

    #[test]
    fn normalize_rel() {
        assert_eq!(
            &normalize_rel_path(Path::new("../a/../b")),
            Path::new("../b")
        );
        assert_eq!(
            &normalize_rel_path(Path::new("../../a/../b")),
            Path::new("../../b")
        );
    }

    #[test]
    fn get_cached_source_with_relative_path() {
        let mut sources = SourceCache::new();
        let root_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("nickel-test-rootdir");
        let path = SourcePath::Path(root_path.join("file.ncl"), super::InputFormat::Nickel);
        let file_id = sources.replace_string(path, "1".into());

        // This path should not exist on the host but should
        // match the in memory file that was set up in the cache
        let file = sources
            .get_or_add_file(
                root_path.join("subdir").join("..").join("file.ncl"),
                InputFormat::Nickel,
            )
            .expect("Missed cached file when pulling with relative path");
        assert_eq!(CacheOp::Cached(file_id), file);
    }

    #[test]
    fn close_file() {
        let mut sources = SourceCache::new();
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("closed.ncl");
        let source_path = SourcePath::Path(path.clone(), InputFormat::Nickel);
        sources.add_string(source_path.clone(), "1".to_string());
        sources
            .close_in_memory_file(path.clone(), InputFormat::Nickel)
            .unwrap();
        assert_eq!(
            sources
                .file_ids
                .get(&source_path)
                .map(|it| it.source)
                .unwrap(),
            SourceKind::MemoryClosed
        );

        // Since the closed file should be stale, id_or_new_timestamp_of should not return the
        // file ID for the closed file. Since in this case the file doesn't exist on the
        // filesystem, it should return an error.
        assert!(sources
            .id_or_new_timestamp_of(&path, InputFormat::Nickel)
            .is_err());
    }
}
