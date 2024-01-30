// Utilities for translating from codespan types into Language Server Protocol (LSP) types
//
// This file was copied (and modified) from the original in the codespan-lsp crate, which appears
// to be abandoned. The latest upstream version is available at the link below, copyright Markus Westerlind,
// released under the Apache-2.0 license.
//
// https://github.com/brendanzab/codespan/blob/ce6d0bb2ed29a27782acd21017d6a7524d36ebba/codespan-lsp/src/lib.rs

use codespan_reporting::files::{Error, Files};
use lsp_types::{Position as LspPosition, Range as LspRange};
use std::ops::Range;

fn location_to_position(
    line_str: &str,
    line: usize,
    column: usize,
    byte_index: usize,
) -> Result<LspPosition, Error> {
    if column > line_str.len() {
        let max = line_str.len();
        let given = column;

        Err(Error::ColumnTooLarge { given, max })
    } else if !line_str.is_char_boundary(column) {
        let given = byte_index;

        Err(Error::InvalidCharBoundary { given })
    } else {
        let line_utf16 = line_str[..column].encode_utf16();
        let character = line_utf16.count() as u32;
        let line = line as u32;

        Ok(LspPosition { line, character })
    }
}

pub fn byte_index_to_position<'a, F>(
    files: &'a F,
    file_id: F::FileId,
    byte_index: usize,
) -> Result<LspPosition, Error>
where
    F: Files<'a> + ?Sized,
{
    let source = files.source(file_id)?;
    let source = source.as_ref();

    let line_index = files.line_index(file_id, byte_index)?;
    let line_span = files.line_range(file_id, line_index).unwrap();

    // https://github.com/rust-lang/rust-clippy/issues/8522
    #[allow(clippy::unnecessary_lazy_evaluations)]
    let line_str = source
        .get(line_span.clone())
        .ok_or_else(|| Error::IndexTooLarge {
            given: if line_span.start >= source.len() {
                line_span.start
            } else {
                line_span.end
            },
            max: source.len() - 1,
        })?;
    let column = byte_index - line_span.start;

    location_to_position(line_str, line_index, column, byte_index)
}

pub fn byte_span_to_range<'a, F>(
    files: &'a F,
    file_id: F::FileId,
    span: Range<usize>,
) -> Result<LspRange, Error>
where
    F: Files<'a> + ?Sized,
{
    Ok(LspRange {
        start: byte_index_to_position(files, file_id, span.start)?,
        end: byte_index_to_position(files, file_id, span.end)?,
    })
}

fn character_to_line_offset(line: &str, character: u32) -> Result<usize, Error> {
    let line_len = line.len();
    let mut character_offset = 0;

    let mut chars = line.chars();
    while let Some(ch) = chars.next() {
        if character_offset == character {
            let chars_off = chars.as_str().len();
            let ch_off = ch.len_utf8();

            return Ok(line_len - chars_off - ch_off);
        }

        character_offset += ch.len_utf16() as u32;
    }

    // Handle positions after the last character on the line
    if character_offset == character {
        Ok(line_len)
    } else {
        Err(Error::ColumnTooLarge {
            given: character_offset as usize,
            max: line.len(),
        })
    }
}

pub fn position_to_byte_index<'a, F>(
    files: &'a F,
    file_id: F::FileId,
    position: &LspPosition,
) -> Result<usize, Error>
where
    F: Files<'a> + ?Sized,
{
    let source = files.source(file_id)?;
    let source = source.as_ref();

    let line_span = files.line_range(file_id, position.line as usize).unwrap();
    let line_str = source.get(line_span.clone()).unwrap();

    let byte_offset = character_to_line_offset(line_str, position.character)?;

    Ok(line_span.start + byte_offset)
}

#[cfg(test)]
mod tests {
    use codespan_reporting::files::{Location, SimpleFiles};

    use super::*;

    #[test]
    fn position() {
        let text = r#"
let test = 2
let test1 = ""
test
"#;
        let mut files = SimpleFiles::new();
        let file_id = files.add("test", text);
        let pos = position_to_byte_index(
            &files,
            file_id,
            &LspPosition {
                line: 3,
                character: 2,
            },
        )
        .unwrap();
        assert_eq!(
            Location {
                // One-based
                line_number: 3 + 1,
                column_number: 2 + 1,
            },
            files.location(file_id, pos).unwrap()
        );
    }

    // The protocol specifies that each `character` in position is a UTF-16 character.
    // This means that `å` and `ä` here counts as 1 while `𐐀` counts as 2.
    const UNICODE: &str = "åä t𐐀b";

    #[test]
    fn unicode_get_byte_index() {
        let mut files = SimpleFiles::new();
        let file_id = files.add("unicode", UNICODE);

        let result = position_to_byte_index(
            &files,
            file_id,
            &LspPosition {
                line: 0,
                character: 3,
            },
        );
        assert_eq!(result.unwrap(), 5);

        let result = position_to_byte_index(
            &files,
            file_id,
            &LspPosition {
                line: 0,
                character: 6,
            },
        );
        assert_eq!(result.unwrap(), 10);
    }

    #[test]
    fn unicode_get_position() {
        let mut files = SimpleFiles::new();
        let file_id = files.add("unicode", UNICODE.to_string());
        let file_id2 = files.add("unicode newline", "\n".to_string() + UNICODE);

        let result = byte_index_to_position(&files, file_id, 5);
        assert_eq!(
            result.unwrap(),
            LspPosition {
                line: 0,
                character: 3,
            }
        );

        let result = byte_index_to_position(&files, file_id, 10);
        assert_eq!(
            result.unwrap(),
            LspPosition {
                line: 0,
                character: 6,
            }
        );

        let result = byte_index_to_position(&files, file_id2, 11);
        assert_eq!(
            result.unwrap(),
            LspPosition {
                line: 1,
                character: 6,
            }
        );
    }
}
