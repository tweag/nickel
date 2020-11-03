/// A few helpers to generate position spans and labels easily during parsing
use crate::label::Label;
use crate::position::RawSpan;
use crate::term::{RichTerm, StrChunk};
use crate::types::Types;
use codespan::FileId;

/// Distinguish between the standard string separators `"`/`"` and the multi-line string separators
/// `m#"`/`"#m` in the parser.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum StringKind {
    Standard,
    Multiline,
}

/// Make a span from parser byte offsets.
pub fn mk_span(src_id: FileId, l: usize, r: usize) -> RawSpan {
    RawSpan {
        src_id,
        start: (l as u32).into(),
        end: (r as u32).into(),
    }
}

/// Same as `mk_span`, but for labels.
pub fn mk_label(types: Types, src_id: FileId, l: usize, r: usize) -> Label {
    Label {
        types,
        tag: String::new(),
        span: mk_span(src_id, l, r),
        polarity: true,
        path: Vec::new(),
    }
}

/// Determine the minimal level of indentation of a multi-line string.
///
/// The result is determined by computing the minimum indentation level among all lines, where the
/// indentation level of a line is the number of consecutive whitespace characters, which are
/// either a space or a, counted from the beginning of the line. If a line is empty or consist only
/// of whitespace characters, it is ignored.
pub fn min_indent(chunks: &Vec<StrChunk<RichTerm>>) -> u32 {
    let mut min: u32 = std::u32::MAX;
    let mut current = 0;
    let mut start_line = true;

    for chunk in chunks.iter() {
        match chunk {
            StrChunk::Expr(_) if start_line => {
                if current < min {
                    min = current;
                }
                start_line = false;
            }
            StrChunk::Expr(_) => (),
            StrChunk::Literal(s) => {
                for c in s.chars() {
                    match c {
                        ' ' | '\t' if start_line => current += 1,
                        '\n' => {
                            current = 0;
                            start_line = true;
                        }
                        _ if start_line => {
                            if current < min {
                                min = current;
                            }
                            start_line = false;
                        }
                        _ => (),
                    }
                }
            }
        }
    }

    min
}

/// Strip the common indentation prefix from a multi-line string.
///
/// Determine the minimum indentation level of a multi-line string via
/// [`min_indent`](./fn.min_indent.html), and strip an equal number of whitespace characters (` `
/// or `\t`) from the beginning of each line. If the last line is empty or consist only of
/// whitespace characters, it is filtered out.
pub fn strip_indent(mut chunks: Vec<StrChunk<RichTerm>>) -> Vec<StrChunk<RichTerm>> {
    if chunks.is_empty() {
        return chunks;
    }

    let min = min_indent(&chunks);
    let mut current = 0;
    let mut start_line = true;

    let chunks_len = chunks.len();
    for (index, chunk) in chunks.iter_mut().enumerate() {
        match chunk {
            StrChunk::Literal(ref mut s) => {
                let mut buffer = String::new();
                for c in s.chars() {
                    match c {
                        ' ' | '\t' if start_line && current < min => current += 1,
                        '\n' => {
                            current = 0;
                            start_line = true;
                            buffer.push(c);
                        }
                        c if start_line => {
                            start_line = false;
                            buffer.push(c);
                        }

                        c => buffer.push(c),
                    }
                }

                // Strip the first line, if it is only whitespace characters
                if index == 0 {
                    println!("First chunk");
                    if let Some(first_index) = buffer.find('\n') {
                        println!("Found newline char: {}", first_index);
                        println!("substring tested: {}", &buffer[0..(first_index + 1)]);
                        if first_index == 0
                           || buffer.as_bytes()[..first_index]
                                .iter()
                                .all(|c| *c == b' ' || *c == b'\t')
                        {
                            buffer = String::from(&buffer[(first_index + 1)..]);
                        }
                    }
                }

                // Strip the last line, if it is only whitespace characters.
                if index == chunks_len-1 {
                    if let Some(last_index) = buffer.rfind('\n') {
                        if last_index == buffer.len() - 1
                            || buffer.as_bytes()[(last_index + 1)..]
                                .iter()
                                .all(|c| *c == b' ' || *c == b'\t')
                        {
                            buffer.truncate(last_index);
                        }
                    }
                }

                std::mem::replace(s, buffer);
            }
            StrChunk::Expr(_) => (),
        }
    }

    chunks
}
