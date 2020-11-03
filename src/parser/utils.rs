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
/// either a space or a tab, counted from the beginning of the line. If a line is empty or consist
/// only of whitespace characters, it is ignored.
pub fn min_indent(chunks: &Vec<StrChunk<RichTerm>>) -> usize {
    let mut min: usize = std::usize::MAX;
    let mut current = 0;
    let mut start_line = true;

    for chunk in chunks.iter() {
        match chunk {
            StrChunk::Expr(_, _) if start_line => {
                if current < min {
                    min = current;
                }
                start_line = false;
            }
            StrChunk::Expr(_, _) => (),
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
///
/// The indentation of interpolated expressions in a multi-line string follow the rules:
/// - if an interpolated expression is alone on a line with whitespaces, its indentation -- minus
///   the common minimal indentation -- is stored and when the expression will be evaluated, each new
///   line will be prepended with this indentation level.
/// - if there are other non whitespace characters or interpolated expressions on the line, then it
///   is just replaced by its content. The common indentation is still stripped before the start of
///   this expression, but newlines inside it won't be affected..
///
/// Examples:
///
/// ```
/// let x = "I\nam\nindented" in
/// m#"
///   baseline
///     ${x}
///   end
/// "#m
/// ```
/// gives
/// ```
///"baseline
///  I
///  am
///  indented
/// end"
/// ```.
///
/// while
/// ```
/// let x = "I\nam\nnot" in
/// m#"
///   baseline
///     ${x} sth
///   end
/// "#m
/// ```
/// gives
/// ```
///"baseline
///  I
///am
///not sth
/// end"
/// ```.
pub fn strip_indent(mut chunks: Vec<StrChunk<RichTerm>>) -> Vec<StrChunk<RichTerm>> {
    if chunks.is_empty() {
        return chunks;
    }

    let min = min_indent(&chunks);
    let mut current = 0;
    let mut start_line = true;
    let chunks_len = chunks.len();

    // When processing a line with an indented interpolated expression, as in:
    //
    // ```
    // m#"
    //  some
    //    ${x} ${y}
    //    ${x}
    //  string
    // "#m
    // ```
    //
    // We don't know at the time we process the expression `${x}` if it wil have to be re-indented,
    // as it depends on the rest of the line being only whitespace or not, according to the
    // indentation rule. Here, the first occurrence should not, while the second one should. We can
    // only know this once we process the next chunks, here when arriving at `${y}`. To handle
    // this, we set all indentation levels as if expressions were alone on their line during the
    // main loop, but also store the index of such chunks which indentation level must be revisited
    // once the information becomes available. Then, their indentation level is erased in a last
    // pass.
    let mut unindent: Vec<usize> = Vec::new();
    let mut expr_on_line: Option<usize> = None;

    for (index, chunk) in chunks.iter_mut().enumerate() {
        match chunk {
            StrChunk::Literal(ref mut s) => {
                let mut buffer = String::new();
                for c in s.chars() {
                    match c {
                        ' ' | '\t' if start_line && current < min => current += 1,
                        ' ' | '\t' if start_line => {
                            current += 1;
                            buffer.push(c);
                        }
                        '\n' => {
                            current = 0;
                            start_line = true;
                            expr_on_line = None;
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
                    if let Some(first_index) = buffer.find('\n') {
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
                if index == chunks_len - 1 {
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
            StrChunk::Expr(_, ref mut indent) => {
                if start_line {
                    debug_assert!(current >= min);
                    debug_assert!(expr_on_line.is_none());
                    *indent = current - min;
                    start_line = false;
                    expr_on_line = Some(index);
                } else if let Some(expr_index) = expr_on_line.take() {
                    unindent.push(expr_index);
                }
            }
        }
    }

    for index in unindent.into_iter() {
        match chunks.get_mut(index) {
            Some(StrChunk::Expr(_, ref mut indent)) => *indent = 0,
            _ => panic!(),
        }
    }

    chunks
}
