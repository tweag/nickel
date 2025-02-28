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
    collections::hash_map,
    collections::{HashMap, HashSet},
    ffi::{OsStr, OsString},
    fs, io,
    io::Read,
    path::{Path, PathBuf},
    result::Result,
    time::SystemTime,
};

use serde::Deserialize;

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
    pub fn from_path(path: &Path) -> Option<InputFormat> {
        match path.extension().and_then(OsStr::to_str) {
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

/// The term cache stores the parsed terms (in the old/mainline representation) of sources.
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
        new: EntryState,
    ) -> Result<EntryState, TermNotFound> {
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
    ) -> Result<CacheOp<()>, CacheError<UnboundTypeVariableError>> {
        match self.terms.get(&file_id).map(|entry| entry.state) {
            Some(state) if state >= EntryState::Transformed => Ok(CacheOp::Cached(())),
            Some(state) if state >= EntryState::Parsed => {
                if state < EntryState::Transforming {
                    let cached_term = self.terms.remove(&file_id).unwrap();
                    let term =
                        transform::transform(cached_term.term, wildcards.wildcards.get(&file_id))?;
                    self.terms.insert(
                        file_id,
                        TermEntry {
                            term,
                            state: EntryState::Transforming,
                            ..cached_term
                        },
                    );

                    if let Some(imports) = import_data.imports.get(&file_id).cloned() {
                        for file_id in imports.into_iter() {
                            self.transform(wildcards, import_data, file_id)?;
                        }
                    }

                    // unwrap(): we re-inserted the entry after removal and transformation, so it
                    // should be in the cache.
                    let _ = self.update_state(file_id, EntryState::Transformed).unwrap();
                }
                Ok(CacheOp::Done(()))
            }
            _ => Err(CacheError::NotParsed),
        }
    }

    /// Retrieves the state of an entry. Returns `None` if the entry is not in the term cache,
    /// meaning that the content of the source has been loaded but has not been parsed yet.
    pub fn entry_state(&self, file_id: FileId) -> Option<EntryState> {
        self.terms
            .get(&file_id)
            .map(|TermEntry { state, .. }| state)
            .copied()
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
    /// The main disadvantage of closurization is that it makes the AST less useful. You
    /// wouldn't want to closurize before pretty-printing, for example.
    pub fn closurize<C: EvalCache>(
        &mut self,
        cache: &mut C,
        import_data: &ImportData,
        file_id: FileId,
    ) -> Result<CacheOp<()>, CacheError<()>> {
        match self.entry_state(file_id) {
            Some(state) if state >= EntryState::Closurized => Ok(CacheOp::Cached(())),
            Some(state) if state >= EntryState::Parsed => {
                let cached_term = self.terms.remove(&file_id).unwrap();
                let term = cached_term.term.closurize(cache, eval::Environment::new());
                self.terms.insert(
                    file_id,
                    TermEntry {
                        term,
                        state: EntryState::Closurized,
                        ..cached_term
                    },
                );

                if let Some(imports) = import_data.imports.get(&file_id).cloned() {
                    for file_id in imports.into_iter() {
                        self.closurize(cache, import_data, file_id)?;
                    }
                }

                Ok(CacheOp::Done(()))
            }
            _ => Err(CacheError::NotParsed),
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
}

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
    pub fn get_or_add_file(
        &mut self,
        path: impl Into<OsString>,
        format: InputFormat,
    ) -> io::Result<CacheOp<FileId>> {
        let path = path.into();
        let normalized = normalize_path(&path)?;
        match self.id_or_new_timestamp_of(path.as_ref(), format)? {
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
            self.files.update(file_id, s);
            file_id
        } else {
            self.files.add(source_name.clone(), s)
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

    /// Gets a reference to the underlying files. Required by the WASM REPL error reporting code
    /// and LSP functions.
    pub fn files(&self) -> &Files {
        &self.files
    }

    /// Parses a Nickel source without querying nor populating other caches.
    pub fn parse_nickel<'ast>(
        &self,
        // We take the allocator explicitly, to make sure `self.asts` is properly initialized
        // before calling this function, and won't be dropped .
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
            InputFormat::Yaml => {
                // YAML files can contain multiple documents. If there is only
                // one we transparently deserialize it. If there are multiple,
                // we deserialize the file as an array.
                let de = serde_yaml::Deserializer::from_str(source);
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
                    Ok(terms.pop().expect("we just checked the length"))
                } else {
                    Ok(attach_pos(
                        Term::Array(terms.into_iter().collect(), Default::default()).into(),
                    ))
                }
            }
            InputFormat::Toml => crate::serialize::toml_deser::from_str(source, file_id)
                .map(attach_pos)
                .map_err(|err| (ParseError::from_toml(err, file_id))),
            #[cfg(feature = "nix-experimental")]
            InputFormat::Nix => {
                let json = nix_ffi::eval_to_json(source)
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

/// Stores dependencies and reverse dependencies data between sources.
#[derive(Default, Clone)]
pub struct ImportData {
    /// Map containing for each FileId a list of files they import (directly).
    pub imports: HashMap<FileId, HashSet<FileId>>,
    /// Map containing for each FileId a list of files importing them (directly).
    pub rev_imports: HashMap<FileId, HashSet<FileId>>,
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
            .copied()
    }

    /// Returns the set of files that import this file.
    pub fn rev_imports(&self, file: FileId) -> impl Iterator<Item = FileId> + '_ {
        self.rev_imports
            .get(&file)
            .into_iter()
            .flat_map(|s| s.iter())
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
/// representation (dubbed "mainline" in many places) and as the new representation.
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
            asts: AstCache::new(),
            wildcards: WildcardsCache::new(),
            import_data: ImportData::new(),
            #[cfg(debug_assertions)]
            skip_stdlib: false,
        }
    }

    /// Actual implementation of [Self::parse] which doesn't take `self` as a parameter, so that it
    /// can be reused from other places when we don't have a full [CacheHub] instance at hand.
    fn parse_impl(
        terms: &mut TermCache,
        asts: &mut AstCache,
        sources: &mut SourceCache,
        file_id: FileId,
        format: InputFormat,
    ) -> Result<CacheOp<()>, ParseErrors> {
        if terms.contains(file_id) {
            Ok(CacheOp::Cached(()))
        } else if let InputFormat::Nickel = format {
            let ast = asts.parse_nickel(file_id, sources.files.source(file_id))?;
            let term = measure_runtime!("runtime:ast_conversion", ast.to_mainline());

            terms.terms.insert(
                file_id,
                TermEntry {
                    term,
                    state: EntryState::Parsed,
                    format,
                },
            );

            Ok(CacheOp::Done(()))
        } else {
            let term = sources.parse_other(file_id, format)?;

            terms.terms.insert(
                file_id,
                TermEntry {
                    term,
                    state: EntryState::Parsed,
                    format,
                },
            );

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

        self.terms.terms.insert(
            file_id,
            TermEntry {
                term,
                state: EntryState::Parsed,
                format: InputFormat::Nickel,
            },
        );

        Ok(CacheOp::Done(id))
    }

    /// Parse a source and populate the corresponding entry in the cache, or do
    /// nothing if the entry has already been parsed. Support multiple formats.
    ///
    /// # RFC007
    ///
    /// This method populates both the ast cache and the term cache at once.
    pub fn parse(
        &mut self,
        file_id: FileId,
        format: InputFormat,
    ) -> Result<CacheOp<()>, ParseErrors> {
        Self::parse_impl(
            &mut self.terms,
            &mut self.asts,
            &mut self.sources,
            file_id,
            format,
        )
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
    ) -> Result<CacheOp<()>, CacheError<TypecheckError>> {
        let (slice, asts) = self.split_asts();
        asts.typecheck(slice, file_id, initial_mode)
    }

    /// Returns the apparent type of an entry that has been typechecked with wildcards substituted.
    pub fn type_of(
        &mut self,
        file_id: FileId,
    ) -> Result<CacheOp<mainline_typ::Type>, CacheError<TypecheckError>> {
        let (slice, asts) = self.split_asts();
        asts.type_of(slice, file_id)
    }

    /// Prepares a source for evaluation: parse, typecheck and apply program transformations, if it
    /// was not already done.
    pub fn prepare(&mut self, file_id: FileId) -> Result<CacheOp<()>, Error> {
        let mut result = CacheOp::Cached(());

        let format = self
            .sources
            .file_paths
            .get(&file_id)
            .and_then(InputFormat::from_source_path)
            .unwrap_or_default();

        if let CacheOp::Done(_) = self.parse(file_id, format)? {
            result = CacheOp::Done(());
        }

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

        let transform_res = self.apply_all_transforms(file_id).map_err(|cache_err| {
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

        let transform_res = self.apply_all_transforms(file_id).map_err(|cache_err| {
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
    ) -> Result<CacheOp<()>, CacheError<UnboundTypeVariableError>> {
        self.terms
            .transform(&self.wildcards, &self.import_data, file_id)
    }

    /// Loads and parse the standard library in the cache.
    ///
    /// # RFC007
    ///
    /// This populates both the ast cache and the term cache at once.
    pub fn load_stdlib(&mut self) -> Result<CacheOp<()>, Error> {
        let mut ret = CacheOp::Cached(());

        for (_, file_id) in self.sources.stdlib_modules() {
            if let CacheOp::Done(_) = self.parse(file_id, InputFormat::Nickel)? {
                ret = CacheOp::Done(());
            }
        }

        Ok(ret)
    }

    /// Typechecks the standard library. Currently only used in the test suite.
    pub fn typecheck_stdlib(&mut self) -> Result<CacheOp<()>, CacheError<TypecheckError>> {
        let (slice, asts) = self.split_asts();
        asts.typecheck_stdlib(slice)
    }

    /// Loads, parses, and applies program transformations to the standard library. We don't
    /// typecheck for performance reasons: this is done in the test suite.
    pub fn prepare_stdlib(&mut self) -> Result<(), Error> {
        #[cfg(debug_assertions)]
        if self.skip_stdlib {
            return Ok(());
        }

        self.load_stdlib()?;

        self.sources
            .stdlib_modules()
            // We need to handle the internals module separately. Each field
            // is bound directly in the environment without evaluating it first, so we can't
            // tolerate top-level let bindings that would be introduced by `transform`.
            .try_for_each(|(_, file_id)| self.transform(file_id).map(|_| ()))
            .map_err(|cache_err: CacheError<UnboundTypeVariableError>| {
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

    /// Applies a custom transform to an input and its imports, leaving them in the same state as
    /// before. Requires that the input has been parsed. In order for the transform to apply to
    /// imports, they need to have been resolved.
    pub fn custom_transform<E>(
        &mut self,
        file_id: FileId,
        f: &mut impl FnMut(&mut CacheHub, RichTerm) -> Result<RichTerm, E>,
    ) -> Result<(), CacheError<E>> {
        match self.terms.entry_state(file_id) {
            Some(state) if state >= EntryState::Parsed => {
                if state < EntryState::Transforming {
                    let cached_term = self.terms.terms.remove(&file_id).unwrap();
                    let term = f(self, cached_term.term)?;
                    self.terms.terms.insert(
                        file_id,
                        TermEntry {
                            term,
                            state: EntryState::Transforming,
                            ..cached_term
                        },
                    );

                    if let Some(imports) = self.import_data.imports.get(&file_id).cloned() {
                        for file_id in imports.into_iter() {
                            self.custom_transform(file_id, f)?;
                        }
                    }
                    // TODO: We're setting the state back to whatever it was.
                    // unwrap(): we inserted the term just above
                    let _ = self.terms.update_state(file_id, state).unwrap();
                }
                Ok(())
            }
            _ => Err(CacheError::NotParsed),
        }
    }

    /// Resolves every imports of an entry of the cache, and update its state accordingly, or do
    /// nothing if the imports of the entry have already been resolved or if they aren't Nickel
    /// inputs. Require that the corresponding source has been parsed.
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
    ///
    /// # RFC007
    ///
    /// This method is still needed only because the evaluator can't handle un-resolved import, so
    /// we need to replace them by resolved imports. However, actual import resolution (loading
    /// and parsing files for the first time) is now driven by typechecking directly.
    #[allow(clippy::type_complexity)]
    pub fn resolve_imports(
        &mut self,
        file_id: FileId,
    ) -> Result<CacheOp<Vec<FileId>>, CacheError<ImportError>> {
        eprintln!("Resolving imports for {file_id:?}");

        let entry = self.terms.terms.get(&file_id);

        eprintln!("State of this entry: {:?}", entry);

        match entry {
            Some(TermEntry {
                state,
                term,
                format: InputFormat::Nickel,
            }) if state < &EntryState::ImportsResolving => {
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
                cached_term.state = EntryState::ImportsResolving;

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
                    .update_state(file_id, EntryState::ImportsResolved)
                    .unwrap();

                Ok(CacheOp::Done(done))
            }
            // We don't have anything to do for non-Nickel entries.
            Some(TermEntry {
                format: InputFormat::Nickel,
                ..
            }) => {
                // unwrap(): if we are in this branch, the term is present in the cache
                let _ = self
                    .terms
                    .update_state(file_id, EntryState::ImportsResolved)
                    .unwrap();
                Ok(CacheOp::Cached(Vec::new()))
            }
            // [^transitory_entry_state]:
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
                state: EntryState::ImportsResolving,
                ..
            }) => Ok(CacheOp::Done(Vec::new())),
            // >= EntryState::ImportsResolved
            Some(_) => Ok(CacheOp::Cached(Vec::new())),
            None => Err(CacheError::NotParsed),
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
    ) -> Result<CacheOp<()>, CacheError<()>> {
        self.terms.closurize(eval_cache, &self.import_data, file_id)
    }

    /// Add the bindings of a record to the REPL type environment. Ignore fields whose name are
    /// defined through interpolation.
    pub fn add_repl_bindings(&mut self, term: &RichTerm) -> Result<(), NotARecord> {
        let (slice, asts) = self.split_asts();
        asts.add_type_bindings(slice, term)
    }

    /// Applies both import resolution and other program transformations on the given term.
    fn apply_all_transforms(&mut self, file_id: FileId) -> Result<CacheOp<()>, CacheError<Error>> {
        let mut done = false;

        let imports = self
            .resolve_imports(file_id)
            .map_err(|cache_err| cache_err.map_err(Error::ImportError))?;
        done = matches!(imports, CacheOp::Done(_)) || done;

        let transform = self
            .terms
            .transform(&self.wildcards, &self.import_data, file_id)
            .map_err(|cache_err| {
                cache_err.map_err(|uvar_err| Error::ParseErrors(ParseErrors::from(uvar_err)))
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
    /// benches.
    pub fn clone_for_eval(&self) -> Self {
        Self {
            terms: self.terms.clone(),
            sources: self.sources.clone(),
            asts: AstCache::new(),
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
}

/// Because ASTs are arena-allocated, the self-referential [ast_cache::AstCache] which holds both
/// the arena and references to this arena often needs special treatment, if we want to make the
/// borrow checker happy. The following structure is basically a view of "everything but the ast
/// cache" into [CacheHub], so that we can separate and pack all the rest in a single
/// structure, making the signature of many [ast_cache::AstCache] methods much lighter.
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
    pub state: EntryState,
    pub format: InputFormat,
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
/// Usually, when applying a procedure to an entry (typechecking, transformation, ...), we process
/// all of its transitive imports as well. We start by processing the entry, updating the state to
/// `XXXing` (ex: `Typechecking`) upon success. Only when all the imports have been successfully
/// processed, the state is updated to `XXXed` (ex: `Typechecked`).
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Copy, Clone)]
pub enum EntryState {
    /// The term have just been parsed.
    Parsed,
    /// The entry have been typechecked, and its (transitive) imports are being typechecked.
    Typechecking,
    /// The entry and its transitive imports have been typechecked.
    Typechecked,
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

    pub fn map_err<O>(self, f: impl FnOnce(E) -> O) -> CacheError<O> {
        match self {
            CacheError::Error(e) => CacheError::Error(f(e)),
            CacheError::NotParsed => CacheError::NotParsed,
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
                .insert(file_id);
            self.import_data
                .rev_imports
                .entry(file_id)
                .or_default()
                .insert(parent);
        }

        self.parse(file_id, format)
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
            .map(|TermEntry { term, state, .. }| {
                debug_assert!(*state >= EntryState::Parsed);
                term.clone()
            })
    }

    fn get_path(&self, file_id: FileId) -> Option<&OsStr> {
        self.sources
            .file_paths
            .get(&file_id)
            .and_then(|p| p.try_into().ok())
    }
}

/// Import resolution for new AST representation (RFC007).
pub trait AstImportResolver {
    /// Resolves an import.
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
    /// The AST cache before the start of import resolution. During typechecking, the AST of the
    /// term being typechecked borrows from  of technicalities of the
    /// self-referential [super::AstCache], we can only take it as an immutable reference. Newly
    /// imported ASTs are put in [Self::new_asts].
    asts: &'cache mut HashMap<FileId, &'ast Ast<'ast>>,
    /// The term cache.
    terms: &'cache mut TermCache,
    /// The source cache where new sources will be stored.
    sources: &'cache mut SourceCache,
    /// Direct and reverse dependencies of files (with respect to imports).
    import_data: &'cache mut ImportData,
}

impl<'ast, 'cache> AstResolver<'ast, 'cache> {
    /// Create a new `AstResolver` from an allocator, an ast cache and a cache hub slice.
    pub fn new(
        alloc: &'ast AstAlloc,
        asts: &'cache mut HashMap<FileId, &'ast Ast<'ast>>,
        slice: CacheHubView<'cache>,
    ) -> Self {
        Self {
            alloc,
            asts,
            terms: slice.terms,
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
                .insert(file_id);
            self.import_data
                .rev_imports
                .entry(file_id)
                .or_default()
                .insert(parent_id);
        }

        if let Some(pkg_id) = pkg_id {
            self.sources.packages.insert(file_id, pkg_id);
        }

        if let InputFormat::Nickel = format {
            if let Some(ast) = self.asts.get(&file_id) {
                Ok(Some(*ast))
            } else {
                let ast = parse_nickel(self.alloc, file_id, self.sources.files.source(file_id))
                    .map_err(|parse_err| ImportError::ParseErrors(parse_err, *pos))?;
                let ast = self.alloc.alloc(ast);
                self.asts.insert(file_id, ast);

                let term = measure_runtime!("runtime:ast_conversion", ast.to_mainline());

                self.terms.terms.insert(
                    file_id,
                    TermEntry {
                        term,
                        state: EntryState::Parsed,
                        format,
                    },
                );

                Ok(Some(ast))
            }
        } else {
            let term = self
                .sources
                .parse_other(file_id, format)
                .map_err(|parse_err| ImportError::ParseErrors(parse_err.into(), *pos))?;
            self.terms.terms.insert(
                file_id,
                TermEntry {
                    term,
                    state: EntryState::Parsed,
                    format,
                },
            );

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
///
/// [ast_cache::AstCache] has a self-referential flavour and requires unsafe code (which is why
/// it's been put in its own module). Please do not mess with its implementation unless you know
/// what you're doing.
mod ast_cache {
    use super::*;

    /// ASTs are stored with the `'static` lifetime. We try as much as possible to *not*
    /// manipulate them in this state, but convert them back to a safe and local lifetime
    /// associated with `self`.
    ///
    /// This internal function does precisely that: it borrows the allocator and returns a
    /// cached AST with an associated lifetime. This keeps the allocator borrowed as long as
    /// the resulting AST is alive.
    ///
    /// Note that we need to split `alloc` and `asts` to make this helper flexible enough; we
    /// can't just take `self` as a whole.
    ///
    /// ## Soft Safety
    ///
    /// The following is not part of the safety contract per se for this free-standing
    /// function, but is a safety invariant to maintain when this function is used in the
    /// context of the AST cache.
    ///
    /// The caller must ensure that the asts stored in `asts` have been allocated with `_alloc`
    /// (or any allocator that will live as long as `_alloc`).
    fn borrow_ast<'ast>(
        _alloc: &'ast AstAlloc,
        asts: &HashMap<FileId, &'static Ast<'static>>,
        file_id: &FileId,
    ) -> Option<&'ast Ast<'ast>> {
        let ast: Option<&'ast Ast<'ast>> = asts.get(file_id).copied();
        ast
    }

    /// Same as [Self::borrow], but for the whole map.
    ///
    /// # Safety
    ///
    /// The caller must ensure that any new AST inserted must've been allocated with `_alloc`,
    /// or at least be guaranteed to live as long as `_alloc`.
    ///
    /// ## Soft Safety
    ///
    /// The following is not part of the safety contract per se for this free-standing
    /// function, but is a safety invariant to maintain when this function is used in the
    /// context of the AST cache.
    ///
    /// The caller must ensure that the asts stored in `asts` have been allocated with `_alloc`
    /// (or any allocator that will live as long as `_alloc`).
    unsafe fn borrow_asts_mut<'ast, 'borrow>(
        _alloc: &'ast AstAlloc,
        asts: &'borrow mut HashMap<FileId, &'static Ast<'static>>,
    ) -> &'borrow mut HashMap<FileId, &'ast Ast<'ast>>
    where
        'ast: 'borrow,
    {
        std::mem::transmute::<
            &'borrow mut HashMap<FileId, &'static Ast<'static>>,
            &'borrow mut HashMap<FileId, &'ast Ast<'ast>>,
        >(asts)
    }

    /// Same as [Self::borrow_ast], but for the typing context. Since contexts are cheaply
    /// cloneable and needs to be provided as owned variant, [Self::borrow_clone_context]
    /// returns a clone.
    ///
    /// # Soft Safety
    ///
    /// The following is not part of the safety contract per se for this free-standing
    /// function, but is a safety invariant to maintain when this function is used in the
    /// context of the AST cache.
    ///
    /// The caller must ensure that the types stored in `type_ctxt` have been allocated with
    /// `_alloc` (or any allocator that will live as long as `_alloc`).
    unsafe fn borrow_type_ctxt_copy<'ast>(
        _alloc: &'ast AstAlloc,
        type_ctxt: &typecheck::Context<'static>,
    ) -> typecheck::Context<'ast> {
        std::mem::transmute::<typecheck::Context<'static>, typecheck::Context<'ast>>(
            type_ctxt.clone(),
        )
    }

    /// Same as [Self::borrow_type_ctxt_copy] but returns a mutable reference.
    ///
    /// # Safety
    ///
    /// The caller must ensure that any new type or AST component inserted must've been
    /// allocated with `_alloc`, or at least be guaranteed to live as long as `_alloc`.
    ///
    /// ## Soft Safety
    ///
    /// The following is not part of the safety contract per se for this free-standing
    /// function, but is a safety invariant to maintain when this function is used in the
    /// context of the AST cache.
    ///
    /// The caller must ensure that the types stored in `type_ctxt` have been allocated with
    /// `_alloc` (or any allocator that will live as long as `_alloc`).
    unsafe fn borrow_context_mut<'ast, 'borrow>(
        _alloc: &'ast AstAlloc,
        type_ctxt: &'borrow mut typecheck::Context<'static>,
    ) -> &'borrow mut typecheck::Context<'ast>
    where
        'ast: 'borrow,
    {
        std::mem::transmute::<
            &'borrow mut typecheck::Context<'static>,
            &'borrow mut typecheck::Context<'ast>,
        >(type_ctxt)
    }

    /// Safely re-borrow the AST cache with a lifetime tied to the cache's allocator.
    ///
    /// This macro is safe as long as the invariant of [AstCache] holds (the elements of
    /// `self.asts` have been allocated from `self.alloc`).
    macro_rules! borrow_ast {
        ($self:ident, $file_id:expr) => {
            crate::cache::ast_cache::borrow_ast(&$self.alloc, &$self.asts, $file_id)
        };
    }

    /// Borrows the AST cache with a lifetime tied to the cache's allocator.
    ///
    /// # Safety
    ///
    /// This macro isn't sufficient to ensure safe usage: in addition to the invariant of
    /// [AstCache] that the elements of `self.type_ctxt` have been allocated from `self.alloc`, the
    /// caller must also ensure that the ASTs inserted in the cache will live at least as long as
    /// the `self.alloc`.
    macro_rules! unsafe_borrow_asts_mut {
        ($self:ident) => {
            unsafe { crate::cache::ast_cache::borrow_asts_mut(&$self.alloc, &mut $self.asts) }
        };
    }

    /// Borrows the type context with a lifetime tied to the cache's allocator.
    ///
    /// This macro is safe as long as the invariant of [AstCache] holds (the elements of
    /// `self.type_ctxt` have been allocated from `self.alloc`).
    macro_rules! borrow_type_ctxt_copy {
        ($self:ident) => {
            unsafe {
                crate::cache::ast_cache::borrow_type_ctxt_copy(&$self.alloc, &$self.type_ctxt)
            }
        };
    }

    /// Mutably borrows the type context with a lifetime tied to the cache's allocator.
    ///
    /// # Safety
    ///
    /// This macro isn't sufficient to ensure safe usage: in addition to the invariant of
    /// [AstCache] that the elements of `self.type_ctxt` have been allocated from `self.alloc`, the
    /// caller must also ensure that the ASTs inserted in the cache will live at least as long as
    /// the `self.alloc`.
    macro_rules! unsafe_borrow_type_ctxt_mut {
        ($self:ident) => {
            unsafe {
                crate::cache::ast_cache::borrow_context_mut(&$self.alloc, &mut $self.type_ctxt)
            }
        };
    }

    /// The AST cache packing together the AST allocator and the cached ASTs.
    #[derive(Debug)]
    pub struct AstCache {
        /// # Safety
        ///
        /// The ASTs stored here are surely _not_ static, they are pointing to inside the memory
        /// manager `alloc`. We just use `'static` as a placeholder. Indeed, we can't currently
        /// express such a self-referential structure in safe Rust, where the lifetime of `Ast` is
        /// tied to `self`.
        ///
        /// Note that we don't actually refer to stuff within [Self::alloc] memory _directly_, but
        /// to heap-allocated memory that is guaranteed by [crate::bytecode::ast::alloc::AstAlloc]
        /// not to move. We thus don't need to consider moves and pinning.
        asts: HashMap<FileId, &'static Ast<'static>>,
        /// The initial typing context. It's morally an option (unitialized at first), but we just
        /// use an empty context as a default value.
        ///
        /// This context can be augmented through [AstCache::add_repl_binding] and
        /// [AstCache::add_repl_bindings], which is typically used in the REPL to add top-level
        /// bindings.
        ///
        /// # Safety
        ///
        /// as for [Self::asts], we have to use a `'static` lifetime here as a placeholder, but it
        /// is actually tied to `alloc`. As for [Self::asts], we don't have a direct
        /// self-reference, so we don't need to care about moving and pinning.
        type_ctxt: typecheck::Context<'static>,
        /// The allocator hosting AST nodes.
        ///
        /// **Important**: keep this field last in the struct. Rust guarantees that fields are
        /// dropped in declaration order, meaning that `asts` and `type_ctxt` will properly be
        /// dropped before the memory they borrow from. Otherwise, we might create dangling
        /// references, even for a short lapse of time.
        alloc: AstAlloc,
    }

    impl AstCache {
        pub fn new() -> Self {
            AstCache {
                alloc: AstAlloc::new(),
                asts: HashMap::new(),
                type_ctxt: typecheck::Context::new(),
            }
        }

        /// Clears the allocator and the cached ASTs.
        pub fn clear(&mut self) {
            // We release the memory previously used by the allocator.
            //
            // **SAFETY**: To avoid any undefined behavior (values cached in one form or another
            // that would borrow from the allocator and survive it), we make sure to clear all and
            // every fields of the cache at once by reinitializing it entirely. change with care.
            *self = Self::new();
        }

        /// Returns the underlying allocator, which might be required to call various helpers.
        pub fn get_alloc(&self) -> &AstAlloc {
            &self.alloc
        }

        /// Parses a Nickel expression and stores the corresponding AST in the cache.
        pub fn parse_nickel<'ast>(
            &'ast mut self,
            file_id: FileId,
            source: &str,
        ) -> Result<&'ast Ast<'ast>, ParseErrors> {
            let ast = parse_nickel(&self.alloc, file_id, source)?;
            let ast = self.alloc.alloc(ast);
            // Safety: we are transmuting the lifetime of the AST from `'ast` to `'static`. This is
            // unsafe in general, but we never use or leak any `'static` reference. It's just a
            // placeholder. We only store such `Ast<'static>` in `asts`, and return them as `'a`
            // references where `self: 'a` in the various `borrow_xxx` methods and macros.
            //
            // Thus, the `'static` lifetime isn't observable from outsideof `AstCache`, and asts
            // are never used as `Ast<'static>` from within the implementation of `AstCache`. They
            // are always tied to the lifetime of `self.alloc` through borrow helpers.
            let promoted_ast =
                unsafe { std::mem::transmute::<&'_ Ast<'_>, &'static Ast<'static>>(ast) };
            self.asts.insert(file_id, promoted_ast);

            Ok(ast)
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
            let extd_ast = parse_nickel_repl(&self.alloc, file_id, source)?;

            let ast = match &extd_ast {
                ExtendedTerm::Term(t) | ExtendedTerm::ToplevelLet(_, t) => {
                    self.alloc.alloc(t.clone())
                }
            };

            // Safety: we are transmuting the lifetime of the AST from `'ast` to `'static`. This is
            // unsafe in general, but we never use or leak any `'static` reference. It's just a
            // placeholder. We only store such `Ast<'static>` in `asts`, and return them as `'a`
            // references where `self: 'a` in the various `borrow_xxx` methods and macros.
            //
            // Thus, the `'static` lifetime isn't observable from outsideof `AstCache`, and asts
            // are never used as `Ast<'static>` from within the implementation of `AstCache`. They
            // are always tied to the lifetime of `self.alloc` through borrow helpers.
            let promoted_ast =
                unsafe { std::mem::transmute::<&'_ Ast<'_>, &'static Ast<'static>>(ast) };
            self.asts.insert(file_id, promoted_ast);

            Ok(extd_ast)
        }

        pub fn remove(&mut self, file_id: FileId) -> Option<&Ast<'_>> {
            self.asts.remove(&file_id)
        }

        /// Typechecks an entry of the cache and updates its state accordingly, or does nothing if
        /// the entry has already been typechecked. Requires that the corresponding source has been
        /// parsed.
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
        ) -> Result<CacheOp<()>, CacheError<TypecheckError>> {
            eprintln!("typechecking file {file_id:?}");

            let Some(TermEntry { state, format, .. }) = slice.terms.get_entry(file_id) else {
                return Err(CacheError::NotParsed);
            };

            let state = *state;
            let format = *format;

            // If we're already typechecking or we have typechecked the file, we stop right here.
            if state >= EntryState::Typechecking {
                return Ok(CacheOp::Cached(()));
            }

            // If the file isn't a Nickel file, we don't have to typecheck it either.
            if format != InputFormat::Nickel {
                if state < EntryState::Typechecked {
                    // unwrap(): we checked at the beginning of this function that the term is in
                    // the cache.
                    let _ = slice
                        .terms
                        .update_state(file_id, EntryState::Typechecked)
                        .unwrap();
                }

                return Ok(CacheOp::Cached(()));
            }

            // Protect against cycles in the import graph.
            // unwrap(): we checked at the beginning of this function that the term is in the
            // cache.
            let _ = slice
                .terms
                .update_state(file_id, EntryState::Typechecking)
                .unwrap();

            // Ensure the initial typing context is properly initialized.
            self.populate_type_ctxt(slice.sources);

            let ast = borrow_ast!(self, &file_id);
            let Some(ast) = ast else {
                return Err(CacheError::NotParsed);
            };

            // Safety: we only provide `asts` to the `AstResolver`, where the lifetime of the ASTs
            // inside is tied to the lifetime of `self.alloc`. This is the `'ast` parameter
            // lifetime in the definition of `AstResolver`. Thus, `AstResolver` can  only insert
            // ASTs that don't outlive `self.alloc`.
            let asts = unsafe_borrow_asts_mut!(self);

            let mut resolver = AstResolver::new(&self.alloc, asts, slice.reborrow());
            let type_ctxt = borrow_type_ctxt_copy!(self);

            let wildcards_map = measure_runtime!(
                "runtime:type_check",
                typecheck(&self.alloc, ast, type_ctxt, &mut resolver, initial_mode)?
            );

            slice.wildcards.wildcards.insert(
                file_id,
                wildcards_map.iter().map(ToMainline::to_mainline).collect(),
            );

            // Typecheck reverse dependencies (files imported by this file).
            if let Some(imports) = slice.import_data.imports.get(&file_id) {
                // Because we need to borrow `import_data` for typechecking, we need to release the
                // borrow by moving the content of `imports` somewhere else.
                let imports: Vec<_> = imports.iter().copied().collect();

                for import_id in imports {
                    eprintln!("Typechecking reverse dependency {import_id:?}");

                    self.typecheck(slice.reborrow(), import_id, initial_mode)?;
                }
            }

            // unwrap(): we checked at the beginning of this function that the term is in the
            // cache.
            let _ = slice
                .terms
                .update_state(file_id, EntryState::Typechecked)
                .unwrap();

            Ok(CacheOp::Done(()))
        }

        /// Typechecks the stdlib. This has to be public because it's used in benches. It probably
        /// does not have to be used for something else.
        pub fn typecheck_stdlib(
            &mut self,
            mut slice: CacheHubView<'_>,
        ) -> Result<CacheOp<()>, CacheError<TypecheckError>> {
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
        ) -> Result<CacheOp<mainline_typ::Type>, CacheError<TypecheckError>> {
            self.typecheck(slice.reborrow(), file_id, TypecheckMode::Walk)?;

            let ast = borrow_ast!(self, &file_id).ok_or(CacheError::NotParsed)?;
            // Safety: The import resolver only inserts new term parsed from its `alloc` field,
            // which is initialized to be `self.alloc`.
            let asts = unsafe_borrow_asts_mut!(self);
            let mut resolver = AstResolver::new(&self.alloc, asts, slice.reborrow());
            let type_ctxt = borrow_type_ctxt_copy!(self);

            let typ: ast::typ::Type<'_> = TryConvert::try_convert(
                &self.alloc,
                ast.apparent_type(&self.alloc, Some(&type_ctxt.type_env), Some(&mut resolver)),
            )
            .unwrap_or(ast::typ::TypeF::Dyn.into());

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
            if !self.type_ctxt.is_empty() {
                return;
            }

            let stdlib_terms_vec: Vec<(StdlibModule, &'_ Ast<'_>)> = sources
                .stdlib_modules()
                .map(|(module, file_id)| {
                    let ast = borrow_ast!(self, &file_id);

                    (
                        module,
                        ast.expect("cache::ast_cache::AstCache::populate_type_ctxt(): can't build environment, stdlib not parsed")
                    )
                })
                .collect();

            let ctxt = typecheck::mk_initial_ctxt(&self.alloc, &stdlib_terms_vec).unwrap();

            // Safety: as for asts, we "forget" the lifetime of the context but it's tied to the
            // allocator stored in `self`. It is also correctly reset when the allocator is reset
            // (upon [Self::clear]).
            self.type_ctxt = unsafe {
                std::mem::transmute::<typecheck::Context<'_>, typecheck::Context<'static>>(ctxt)
            };
        }

        /// Adds a binding to the type environment. The bound term is identified by its file id
        /// `file_id`.
        pub fn add_type_binding(
            &mut self,
            mut slice: CacheHubView<'_>,
            id: LocIdent,
            file_id: FileId,
        ) -> Result<(), CacheError<std::convert::Infallible>> {
            let ast = borrow_ast!(self, &file_id);
            let Some(ast) = ast else {
                return Err(CacheError::NotParsed);
            };

            // Safety: AstResolver only inserts new terms allocated from its `alloc` field, which
            // is equal to `self.alloc`.
            let asts = unsafe_borrow_asts_mut!(self);
            let mut resolver = AstResolver::new(&self.alloc, asts, slice.reborrow());

            // Safety: `type_ctxt` is given to `env_add`. `env_add` can only insert elements
            // allocated from `self.alloc`, because it is generic in the `'ast` lifetime: the only
            // way for `env_add` to safely generate objects of lifetime `'ast` is to use the
            // provided allocator, which is `self.alloc`, or by casting a `'static` lifetime, which
            // is fine (parametricity).
            let type_ctxt = unsafe_borrow_type_ctxt_mut!(self);

            typecheck::env_add(
                &self.alloc,
                &mut type_ctxt.type_env,
                id,
                ast,
                &type_ctxt.term_env,
                &mut resolver,
            );

            type_ctxt
                .term_env
                .0
                .insert(id.ident(), (ast.clone(), type_ctxt.term_env.clone()));

            Ok(())
        }

        /// Add the bindings of a record to the type environment. Ignore fields whose name are
        /// defined through interpolation.
        pub fn add_type_bindings(
            &mut self,
            mut slice: CacheHubView<'_>,
            term: &RichTerm,
        ) -> Result<(), NotARecord> {
            // It's sad, but for now, we have to convert the term back to an AST to insert it in
            // the type environment.
            let ast = term.to_ast(&self.alloc);
            // Safety: we only insert terms coming from import resolution, which are allocated
            // using `self.alloc` which is provided to `AstResolver` below.
            let asts = unsafe_borrow_asts_mut!(self);
            let mut resolver = AstResolver::new(&self.alloc, asts, slice.reborrow());
            // Safety: We only insert types derived from `ast`, which is itself allocated from
            // `self.alloc`.
            let type_ctxt = unsafe_borrow_type_ctxt_mut!(self);

            typecheck::env_add_term(
                &self.alloc,
                &mut type_ctxt.type_env,
                ast,
                &type_ctxt.term_env,
                &mut resolver,
            )
            .map_err(|_| NotARecord)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::normalize_rel_path;
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
}
