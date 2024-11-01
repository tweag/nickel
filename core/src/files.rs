//! This module provides [`Files`], a cheaply-clonable, persistent, `codespan`-compatible collection of files.

use std::{
    ffi::{OsStr, OsString},
    path::PathBuf,
    rc::Rc,
};

use codespan::ByteIndex;
use codespan_reporting::files::Error;
use nickel_lang_vector::Vector;

use crate::{position::RawSpan, stdlib::StdlibModule};

/// A file identifier, which can be used to access a file in a [`Files`].
///
/// Note that there is no protection against using a `FileId` for the wrong
/// instance of `Files`.
#[derive(
    Copy, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct FileId(u32);

#[derive(Debug, Clone)]
struct File {
    /// The name of the file.
    name: OsString,
    /// The source code of the file.
    source: Rc<str>,
    /// The byte index of the start of each line. The first element of this array is always 0.
    line_starts: Rc<[ByteIndex]>,
}

impl File {
    fn new(name: impl Into<OsString>, source: impl Into<Rc<str>>) -> Self {
        let source = source.into();
        let line_starts: Vec<_> = std::iter::once(ByteIndex(0))
            .chain(
                source
                    .match_indices('\n')
                    .map(|(i, _)| ByteIndex(i as u32 + 1)),
            )
            .collect();

        File {
            name: name.into(),
            line_starts: line_starts.into(),
            source,
        }
    }

    fn line_index(&self, byte_index: ByteIndex) -> usize {
        match self.line_starts.binary_search(&byte_index) {
            Ok(line) => line,
            // unwrap: we always start off the `line_starts` array with a zero,
            // so next_line must be at least 1.
            Err(next_line) => next_line.checked_sub(1).unwrap(),
        }
    }
}

/// A cheaply-clonable, persistent, `codespan`-compatible collection of files.
///
/// `Files` knows about the nickel standard library, and automatically loads it on creation
/// (but it doesn't do parsing, or anything particularly expensive).
///
/// Cloning a `Files` is cheap, and the underlying file data will be shared between clones
/// until one of them wants to modify a file. In that case, only the modified file(s) will
/// be duplicated.
#[derive(Debug, Clone)]
pub struct Files {
    files: Vector<File, 8>,
    first_non_stdlib: usize,
}

impl Files {
    /// Creates a new `Files`, initialized with the nickel standard library.
    pub fn new() -> Self {
        let files: Vector<_, 8> = crate::stdlib::modules()
            .iter()
            .map(|m| File::new(m.file_name().to_owned(), m.content()))
            .collect();

        Files {
            first_non_stdlib: files.len(),
            files,
        }
    }

    /// Does this file id point to a standard library file?
    pub fn is_stdlib(&self, id: FileId) -> bool {
        (id.0 as usize) < self.first_non_stdlib
    }

    /// Returns the list of all standard library modules and their file ids.
    pub fn stdlib_modules(&self) -> impl Iterator<Item = (StdlibModule, FileId)> {
        crate::stdlib::modules()
            .into_iter()
            .zip(0..)
            .map(|(m, id)| (m, FileId(id)))
    }

    /// Adds a file to this collection, creating and returning a new file id.
    ///
    /// The name does not need to be unique, and this method does not affect any other files
    /// with the same name.
    pub fn add(&mut self, name: impl Into<OsString>, source: impl Into<Rc<str>>) -> FileId {
        let file_id = FileId(self.files.len() as u32);
        self.files.push(File::new(name, source));
        file_id
    }

    /// Updates a source file in place.
    ///
    /// Panics if `file_id` is invalid.
    pub fn update(&mut self, file_id: FileId, source: impl Into<Rc<str>>) {
        // This implementation would be a little nicer if `Vector` supported mutable access.
        // unwrap: we're allowed to panic if file_id is invalid
        let mut old = self.get(file_id).unwrap().clone();
        old.source = source.into();
        self.files.set(file_id.0 as usize, old);
    }

    /// Returns a span containing all of a source.
    ///
    /// Panics if `file_id` is invalid.
    pub fn source_span(&self, file_id: FileId) -> RawSpan {
        // unwrap: we're allowed to panic if file_id is invalid
        let len = self.get(file_id).unwrap().source.len();

        RawSpan {
            src_id: file_id,
            start: ByteIndex(0),
            end: ByteIndex(len as u32),
        }
    }

    /// Returns the source's name.
    ///
    /// Panics if `file_id` is invalid.
    pub fn name(&self, id: FileId) -> &OsStr {
        &self.get(id).unwrap().name
    }

    /// Returns a source's contents.
    pub fn source(&self, id: FileId) -> &str {
        self.get(id).unwrap().source.as_ref()
    }

    /// Returns a slice of the source's contents.
    pub fn source_slice(&self, span: RawSpan) -> &str {
        let start: usize = span.start.into();
        let end: usize = span.end.into();
        &self.source(span.src_id)[start..end]
    }

    /// Returns the `codespan::Location` (basically: line + col) corresponding
    /// to a byte index.
    ///
    /// Returns an error if the byte index is out of bounds or fails to point to
    /// a UTF-8 char boundary.
    pub fn location(
        &self,
        id: FileId,
        byte_index: impl Into<ByteIndex>,
    ) -> Result<codespan::Location, Error> {
        let file = self.get(id)?;
        let byte_index = byte_index.into();
        let idx = byte_index.to_usize();

        if idx >= file.source.len() {
            return Err(Error::IndexTooLarge {
                given: idx,
                max: file.source.len() - 1,
            });
        }

        let line_idx = file.line_index(byte_index);
        let line_start_idx = file.line_starts[line_idx];
        let line = file
            .source
            .get(line_start_idx.to_usize()..idx)
            .ok_or(Error::InvalidCharBoundary { given: idx })?;

        Ok(codespan::Location {
            line: codespan::LineIndex::from(line_idx as u32),
            column: codespan::ColumnIndex::from(line.chars().count() as u32),
        })
    }

    fn get(&self, id: FileId) -> Result<&File, Error> {
        self.files.get(id.0 as usize).ok_or(Error::FileMissing)
    }
}

impl Default for Files {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> codespan_reporting::files::Files<'a> for Files {
    type FileId = FileId;
    type Name = String;
    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<String, Error> {
        Ok(PathBuf::from(&self.get(id)?.name).display().to_string())
    }

    fn source(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        Ok(self.get(id)?.source.as_ref())
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        let file = self.get(id)?;
        Ok(file.line_index(ByteIndex(byte_index as u32)))
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        let file = self.get(id)?;
        let starts = &file.line_starts;
        let end = starts
            .get(line_index + 1)
            .copied()
            .unwrap_or(ByteIndex(file.source.len() as u32));
        Ok(starts[line_index].into()..end.into())
    }
}
