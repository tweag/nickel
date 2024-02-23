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
    collections::{hash_map::Entry, HashMap},
    ffi::OsString,
    io,
    num::NonZeroUsize,
};

use codespan::{ByteIndex, FileId, Files, LineIndex, Location, Span};
use codespan_reporting::files::{Error as CodespanError, Files as ReportingFiles};

use crate::{
    error::ParseErrors,
    source::{Source, SourcePath},
    term::RichTerm,
    typecheck::Wildcards,
};

pub struct SourceCache {
    sources: Files<Source>,
    by_path: HashMap<SourcePath, CacheKey>,
    entries: Vec<CacheEntry>,
    // We store parse errors by CacheKey separately, to facilitate retrieving errors after the fact
    // in error tolerant mode.
    parse_errors: HashMap<CacheKey, ParseErrors>,
    next_generated: usize,
}

// Intentionally don't derive `serde::Deserialize` since we want a `CacheKey` to always refer to a
// valid entry in the `SourceCache`
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, serde::Serialize)]
pub struct CacheKey(NonZeroUsize);

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
    /// The imports of the entry have been resolved, and the imports of its (transitive) imports are
    /// being resolved.
    ImportsResolving(ResolvedEntry),
    /// The imports of the entry and its transitive dependencies has been resolved.
    ImportsResolved(ResolvedEntry),
    /// The entry have been typechecked, and its (transitive) imports are being typechecked.
    Typechecking(TypecheckedEntry),
    /// The entry and its transitive imports have been typechecked.
    Typechecked(TypecheckedEntry),
    /// The entry have been transformed, and its (transitive) imports are being transformed.
    Transforming(TransformedEntry),
    /// The entry and its transitive imports have been transformed.
    Transformed(TransformedEntry),
}

#[derive(Debug, Clone)]
pub struct ParsedEntry {
    pub term: RichTerm,
}

#[derive(Debug, Clone)]
pub struct ResolvedEntry {
    pub term: RichTerm,
}

#[derive(Debug, Clone)]
pub struct TypecheckedEntry {
    pub term: RichTerm,
    pub wildcards: Wildcards,
}

#[derive(Debug, Clone)]
pub struct TransformedEntry {
    pub term: RichTerm,
}

#[derive(Debug, Clone)]
pub struct CacheEntry {
    state: SourceState,

    path: SourcePath,
    // TODO(vkleen): I'd rather store the [Source] directly in the CacheEntry, but to satisfy
    // codespan, we'll reference the global `Files<String>` table.
    source: FileId,
}

impl SourceCache {
    pub fn new() -> Self {
        SourceCache {
            sources: Files::new(),
            by_path: HashMap::new(),
            entries: Vec::new(),
            parse_errors: HashMap::new(),
            next_generated: 0,
        }
    }

    /// Find the [`CacheKey`] corresponding to a known [`SourcePath`]
    pub fn find(&self, path: &SourcePath) -> Option<CacheKey> {
        self.by_path.get(path).copied()
    }

    /// Insert or replace a [`Source`] for the given [`SourcePath`].
    pub fn insert(&mut self, path: SourcePath, source: Source) -> CacheKey {
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
                });
                let cache_key = CacheKey(
                    NonZeroUsize::new(self.entries.len())
                        .expect("We inserted something into `self.entries` above"),
                );
                e.insert(cache_key);
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

    fn entry_mut(&mut self, key: CacheKey) -> &mut CacheEntry {
        self.entries
            .get_mut(key.0.get() - 1)
            .expect("CacheKeys can only be constructed by us")
    }

    pub fn get(&self, key: CacheKey) -> &CacheEntry {
        self.entries
            .get(key.0.get() - 1)
            .expect("CacheKeys can only be constructed by us")
    }

    /// Get a mutable reference to the [SourceState] of an entry identified by a [CacheKey]
    pub fn get_mut(&mut self, key: CacheKey) -> &mut SourceState {
        &mut self.entry_mut(key).state
    }

    /// Get a refrernce to the currently stored source string for a [CacheKey]
    pub fn source(&self, key: CacheKey) -> &str {
        self.sources.source(self.get(key).source).as_ref()
    }

    pub fn from_filesystem(&mut self, path: impl Into<OsString>) -> io::Result<CacheKey> {
        todo!()
    }

    pub fn source_path(&self, key: CacheKey) -> &SourcePath {
        &self.get(key).path
    }

    pub fn source_span(&self, key: CacheKey) -> codespan::Span {
        self.sources.source_span(self.get(key).source)
    }

    pub fn source_slice(
        &self,
        key: CacheKey,
        span: impl Into<Span>,
    ) -> Result<&str, CodespanError> {
        self.sources.source_slice(self.get(key).source, span)
    }

    pub fn line_span(
        &self,
        key: CacheKey,
        line_index: impl Into<LineIndex>,
    ) -> Result<Span, CodespanError> {
        self.sources.line_span(self.get(key).source, line_index)
    }

    pub fn location(
        &self,
        key: CacheKey,
        byte_index: impl Into<ByteIndex>,
    ) -> Result<Location, CodespanError> {
        self.sources.location(self.get(key).source, byte_index)
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
        ReportingFiles::line_index(&self.sources, self.get(key).source, byte_index)
    }

    fn line_range(
        &'a self,
        key: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, CodespanError> {
        self.sources.line_range(self.get(key).source, line_index)
    }
}

#[cfg(test)]
mod tests {
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
        assert_eq!(cache.get(key).path, path);
        assert_matches!(cache.get(key).state, SourceState::Added)
    }
}
