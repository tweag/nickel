#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unreachable_code)]
//! A cache for Nickel sources and terms
//!
//! At its most basic, a [Cache] is simply mapping [CacheKey]s to [CacheEntry]s. Each [CacheEntry]
//! describes a Nickel source file and its state along the evaluation pipeline.
//!
//! In order to properly share imports of the same file from different places, we also need to
//! maintain a mapping from [SourcePath]s to [CacheKey]s. With this, when we encounter an `import`
//! statement, we normalize the path provided to it, look it up in the [Cache] and only if there is
//! no existing [CacheKey] do we add it as a new file.

use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    ffi::{OsStr, OsString},
    fs, io,
    num::NonZeroUsize,
    path::{Path, PathBuf},
    time::SystemTime,
};

use codespan::{ByteIndex, FileId, Files, LineIndex, Location, Span};
use codespan_reporting::files::{Error as CodespanError, Files as ReportingFiles};

use crate::{
    error::{ImportError, ParseErrors},
    position::TermPos,
    source::{Source, SourcePath},
    stdlib::StdlibModule,
    term::RichTerm,
    typecheck::Wildcards,
};

// Be very careful about cloning a cache! If the two copies end up containing different entries,
// [CacheKey]s from one will refer to diferent things in the other. This may lead to panics or even
// unexpected results, if the key is valid for both but points to different contents.
#[derive(Clone)]
pub struct SourceCache {
    sources: Files<Source>,
    by_path: HashMap<SourcePath, CacheKey>,
    entries: Vec<CacheEntry>,
    // We store parse errors by CacheKey separately, to facilitate retrieving errors after the fact
    // in error tolerant mode.
    parse_errors: HashMap<CacheKey, ParseErrors>,

    // We cache the imports discovered in each source file and the source files importing a given
    // one for the LSP.
    // TODO(vkleen): Maybe this data should be maintained by the LSP itself?
    imports: HashMap<CacheKey, HashSet<CacheKey>>,
    rev_imports: HashMap<CacheKey, HashSet<CacheKey>>,

    // This counter is used to facilitate unique names for generated source files using
    // [SourcePath::GeneratedByEvaluation]
    next_generated: usize,

    // The set of [CacheKey]s referring to loaded stdlib modules. Any module with soruce path
    // matching [SourcePath::Std(_)] is rcorded here.
    stdlib_modules: Vec<(StdlibModule, CacheKey)>,

    import_paths: Vec<PathBuf>,
}

// Intentionally don't derive `serde::Deserialize` since we want a `CacheKey` to always refer to a
// valid entry in the `SourceCache`
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, serde::Serialize)]
pub struct CacheKey(NonZeroUsize);

impl CacheKey {
    /// Use this only for parsers tests that don't actually try to use the [CacheKey]
    pub fn dummy() -> CacheKey {
        CacheKey(NonZeroUsize::new(1).unwrap())
    }
}

/// The state of an entry of the term cache.
///
/// # Imports
///
/// Usually, when applying a procedure to an entry (typechecking, transformation, ...), we process
/// all of its transitive imports as well. We start by processing the entry, updating the state to
/// `XXXing` (ex: `Typechecking`) upon success. Only when all the imports have been successfully
/// processed, the state is updated to `XXXed` (ex: `Typechecked`).
#[derive(Debug, Clone)]
pub enum SourceState {
    /// The cache entry has been created and the source has been loaded, but nothing else has
    /// happened, yet
    Added,
    /// The source has been parsed, potentially with parse errors.
    Parsed(ParsedEntry),
}

#[derive(Debug, Clone)]
pub struct ParsedEntry {
    pub term: RichTerm,
    pub state: ParsedState,
}

/// The state of an entry after having been parsed.
#[derive(Debug, Clone)]
pub enum ParsedState {
    /// The entry has only been parsed, nothing else has happened, yet.
    Parsed,
    /// The imports of the entry and its transitive dependencies have been resolved.
    ImportsResolved,
    /// The entry and its transitive imports have been typechecked.
    Typechecked { wildcards: Wildcards },
    /// The entry and its transitive imports have been transformed.
    Transformed,
}

impl ParsedState {
    pub fn typechecked(&self) -> bool {
        match self {
            ParsedState::Parsed | ParsedState::ImportsResolved => false,
            ParsedState::Typechecked { .. } | ParsedState::Transformed => true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CacheEntry {
    state: SourceState,

    path: SourcePath,
    // TODO(vkleen): I'd rather store the [Source] directly in the CacheEntry, but reusing [codespan::Files] is much easier this way.
    source: FileId,

    // When typechecking, transforming or resolving imports in a source file, we mark the current
    // entry as `transitory` to avoid circular dependencies. Only when all transitive imports have
    // been processed to we reset this flag.
    transitory: bool,
}

impl Default for SourceCache {
    fn default() -> Self {
        Self::new()
    }
}

impl SourceCache {
    pub fn new() -> Self {
        SourceCache {
            sources: Files::new(),
            by_path: HashMap::new(),
            entries: Vec::new(),
            parse_errors: HashMap::new(),
            next_generated: 0,
            stdlib_modules: Vec::new(),
            import_paths: Vec::new(),
            imports: HashMap::new(),
            rev_imports: HashMap::new(),
        }
    }

    /// Find the [`CacheKey`] corresponding to a known [`SourcePath`]
    pub fn find(&self, path: &SourcePath) -> Option<CacheKey> {
        self.by_path.get(path).copied()
    }

    /// Insert or replace a [`Source`] for the given [`SourcePath`].
    pub fn insert(&mut self, path: SourcePath, source: Source) -> CacheKey {
        let stdlib_module = match &path {
            SourcePath::Std(m) => Some(*m),
            _ => None,
        };
        match self.by_path.entry(path) {
            Entry::Occupied(e) => {
                let cache_key = *e.get();
                let entry = self
                    .entries
                    .get_mut(cache_key.0.get() - 1)
                    .expect("CacheKeys can only be constructed by us");
                self.sources.update(entry.source, source);
                entry.state = SourceState::Added;
                cache_key
            }
            Entry::Vacant(e) => {
                // We can pass a dummy name for the source here to make `codespan::Files` happy,
                // because we don't use its name rtrieval mechanism for error reporting.
                let file_id = self.sources.add(OsString::from(""), source);
                self.entries.push(CacheEntry {
                    state: SourceState::Added,
                    path: e.key().clone(),
                    source: file_id,
                    transitory: false,
                });
                let cache_key = CacheKey(
                    NonZeroUsize::new(self.entries.len())
                        .expect("We inserted something into `self.entries` above"),
                );
                e.insert(cache_key);
                if let Some(m) = stdlib_module {
                    self.stdlib_modules.push((m, cache_key));
                }
                cache_key
            }
        }
    }

    /// Insert a [`Source`] under a freshly generated [`SourcePath::GeneratedByEvaluation`].
    pub fn insert_generated(&mut self, source: Source) -> CacheKey {
        let path = SourcePath::GeneratedByEvaluation(self.next_generated);
        self.next_generated += 1;
        self.insert(path, source)
    }

    fn entry(&self, key: CacheKey) -> &CacheEntry {
        self.entries
            .get(key.0.get() - 1)
            .expect("CacheKeys can only be constructed by us")
    }

    fn entry_mut(&mut self, key: CacheKey) -> &mut CacheEntry {
        self.entries
            .get_mut(key.0.get() - 1)
            .expect("CacheKeys can only be constructed by us")
    }

    pub fn get(&self, key: CacheKey) -> &SourceState {
        &self.entry(key).state
    }

    /// Get a mutable reference to the [SourceState] of an entry identified by a [CacheKey]
    pub fn get_mut(&mut self, key: CacheKey) -> &mut SourceState {
        &mut self.entry_mut(key).state
    }

    pub fn set_parsed(&mut self, key: CacheKey, entry: ParsedEntry) {
        self.entry_mut(key).state = SourceState::Parsed(entry);
    }

    /// Set the parsed state of an entry. It the entry has not actually been parsed before, panic.
    // TODO(vkleen): having this panic feels dirty, maybe we should distinguish parsed and unparsed
    // entries in the type of CacheKey?
    pub fn set_parsed_state(&mut self, key: CacheKey, new_state: ParsedState) {
        match self.entry_mut(key).state {
            SourceState::Added => panic!("Attempted to set the parsed state of an unparsed entry"),
            SourceState::Parsed(ParsedEntry { ref mut state, .. }) => *state = new_state,
        }
    }

    pub fn get_parsed(&self, key: CacheKey) -> Option<&ParsedEntry> {
        match self.entry(key).state {
            SourceState::Added => None,
            SourceState::Parsed(ref e) => Some(e),
        }
    }

    pub fn reset(&mut self, key: CacheKey) {
        self.entry_mut(key).state = SourceState::Added;
    }

    /// Get a refrernce to the currently stored source string for a [CacheKey]
    pub fn source(&self, key: CacheKey) -> &str {
        self.sources.source(self.entry(key).source).as_ref()
    }

    pub fn load_from_filesystem(&mut self, path: impl Into<OsString>) -> io::Result<CacheKey> {
        todo!()
    }

    /// Resolve an import.
    ///
    /// Read and store the content of an import, put it in the file cache (or get it from there if
    /// it is cached) and return the corresponding cache key.
    ///
    /// TODO(vkleen) check this description
    /// The term and the path are provided only if the import is processed for the first time.
    /// Indeed, at import resolution phase, the term of an import encountered for the first time is
    /// queued to be processed (e.g. having its own imports resolved). The path is needed to
    /// resolve nested imports relatively to this parent. Only after this processing the term is
    /// inserted back in the cache. On the other hand, if it has been resolved before, it is
    /// already transformed in the cache and does not need further processing.
    pub fn resolve_import(
        &mut self,
        path: impl Into<OsString>,
        parent: Option<CacheKey>,
        pos: &TermPos,
    ) -> Result<CacheKey, ImportError> {
        todo!()
    }

    pub fn source_path(&self, key: CacheKey) -> &SourcePath {
        &self.entry(key).path
    }

    pub fn source_span(&self, key: CacheKey) -> codespan::Span {
        self.sources.source_span(self.entry(key).source)
    }

    pub fn source_slice(
        &self,
        key: CacheKey,
        span: impl Into<Span>,
    ) -> Result<&str, CodespanError> {
        self.sources.source_slice(self.entry(key).source, span)
    }

    pub fn line_span(
        &self,
        key: CacheKey,
        line_index: impl Into<LineIndex>,
    ) -> Result<Span, CodespanError> {
        self.sources.line_span(self.entry(key).source, line_index)
    }

    pub fn location(
        &self,
        key: CacheKey,
        byte_index: impl Into<ByteIndex>,
    ) -> Result<Location, CodespanError> {
        self.sources.location(self.entry(key).source, byte_index)
    }

    // TODO(vkleen) This shouldn't need to allocate, but I can't be bothered to figure out the
    // lifetimes properly right now
    pub fn stdlib_keys(&self) -> Vec<CacheKey> {
        self.stdlib_modules()
            .iter()
            .map(|(_, k)| k)
            .copied()
            .collect()
    }

    pub fn stdlib_modules(&self) -> &[(StdlibModule, CacheKey)] {
        self.stdlib_modules.as_slice()
    }

    pub fn add_import_paths<P>(&mut self, paths: impl Iterator<Item = P>)
    where
        PathBuf: From<P>,
    {
        self.import_paths.extend(paths.map(PathBuf::from));
    }

    pub fn term(&self, cache_key: CacheKey) -> Option<&RichTerm> {
        match self.get(cache_key) {
            SourceState::Added => None,
            SourceState::Parsed(ParsedEntry { term, .. }) => Some(term),
        }
    }

    pub fn term_owned(&self, cache_key: CacheKey) -> Option<RichTerm> {
        match self.get(cache_key) {
            SourceState::Added => None,
            SourceState::Parsed(ParsedEntry { term, .. }) => Some(term.clone()),
        }
    }

    pub fn record_parse_errors(&mut self, cache_key: CacheKey, errors: ParseErrors) {
        self.parse_errors.insert(cache_key, errors);
    }

    pub fn get_parse_errors(&self, cache_key: CacheKey) -> Option<&ParseErrors> {
        self.parse_errors.get(&cache_key)
    }

    /// Returns the set of files that this file imports.
    pub fn get_imports(&self, file: CacheKey) -> impl Iterator<Item = CacheKey> + '_ {
        self.imports
            .get(&file)
            .into_iter()
            .flat_map(|s| s.iter())
            .copied()
    }

    /// Returns the set of files that import this file.
    pub fn get_rev_imports(&self, file: CacheKey) -> impl Iterator<Item = CacheKey> + '_ {
        self.rev_imports
            .get(&file)
            .into_iter()
            .flat_map(|s| s.iter())
            .copied()
    }

    /// Returns the set of files that transitively depend on this file.
    pub fn get_rev_imports_transitive(&self, file: CacheKey) -> HashSet<CacheKey> {
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
}

impl<'a> ReportingFiles<'a> for SourceCache {
    type FileId = CacheKey;

    type Name = &'a SourcePath;

    type Source = &'a str;

    fn name(&'a self, key: Self::FileId) -> Result<Self::Name, CodespanError> {
        Ok(self.source_path(key))
    }

    fn source(&'a self, key: Self::FileId) -> Result<Self::Source, CodespanError> {
        Ok(self.source(key))
    }

    fn line_index(&'a self, key: Self::FileId, byte_index: usize) -> Result<usize, CodespanError> {
        ReportingFiles::line_index(&self.sources, self.entry(key).source, byte_index)
    }

    fn line_range(
        &'a self,
        key: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, CodespanError> {
        self.sources.line_range(self.entry(key).source, line_index)
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

#[cfg(test)]
pub mod tests {
    use super::*;
    use assert_matches::assert_matches;

    #[test]
    fn insert_and_find() {
        let mut cache = SourceCache::new();
        let path = SourcePath::Generated("by me".into());
        let source = Source::Memory {
            source: "Hello, world!".into(),
        };
        let key = cache.insert(path.clone(), source);
        assert_eq!(cache.find(&path), Some(key));
        assert_eq!(*cache.source_path(key), path);
        assert_matches!(cache.get(key), SourceState::Added)
    }
}
