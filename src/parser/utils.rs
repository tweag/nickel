//! Various helpers and companion code for the parser are put here to keep the grammar definition
//! uncluttered.
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;

use codespan::FileId;

use crate::{
    identifier::Ident,
    label::Label,
    mk_app, mk_fun,
    parser::error::ParseError,
    position::{RawSpan, TermPos},
    term::{make as mk_term, BinaryOp, RecordAttrs, RichTerm, StrChunk, Term, UnaryOp},
    types::{AbsType, Types},
};

/// Distinguish between the standard string separators `"`/`"` and the multi-line string separators
/// `m#"`/`"#m` in the parser.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum StringKind {
    Standard,
    Multiline,
}

/// Distinguish between a normal case `id => exp` and a default case `_ => exp`.
#[derive(Clone, Debug)]
pub enum SwitchCase {
    Normal(Ident, RichTerm),
    Default(RichTerm),
}

/// Left hand side of a record field declaration.
#[derive(Clone, Debug)]
pub enum FieldPathElem {
    /// A static field declaration: `{ foo = .. }`
    Ident(Ident),
    /// A quoted field declaration: `{ "#{protocol}" = .. }`
    ///
    /// In practice, the argument must always be `StrChunks`, but since we also need to keep track
    /// of the associated span it's handier to just use a `RichTerm`.
    Expr(RichTerm),
}

/// A string chunk literal atom, being either a string or a single char.
///
/// Because of the way the lexer handles escaping and interpolation, a contiguous static string
/// `"Some \\ \#{escaped} string"` will be lexed as a sequence of such atoms.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ChunkLiteralPart<'input> {
    Str(&'input str),
    Char(char),
}

/// The last field of a record, that can either be a normal field declaration or an ellipsis.
#[derive(Clone, Debug)]
pub enum RecordLastField {
    Field((FieldPathElem, RichTerm)),
    Ellipsis,
}

/// An infix operator that is not applied. Used for the curried operator syntax (e.g `(==)`)
pub enum InfixOp {
    Unary(UnaryOp),
    Binary(BinaryOp),
}

impl From<UnaryOp> for InfixOp {
    fn from(op: UnaryOp) -> Self {
        InfixOp::Unary(op)
    }
}

impl From<BinaryOp> for InfixOp {
    fn from(op: BinaryOp) -> Self {
        InfixOp::Binary(op)
    }
}

impl InfixOp {
    /// Eta-expand an operator. This wraps an operator, for example `==`, as a function `fun x1 x2
    /// => x1 == x2`. Propagate the given position to the function body, for better error
    /// reporting.
    pub fn eta_expand(self, pos: TermPos) -> RichTerm {
        let pos = pos.into_inherited();
        match self {
            InfixOp::Unary(op) => mk_fun!("x", mk_term::op1(op, mk_term::var("x")).with_pos(pos)),
            InfixOp::Binary(op) => mk_fun!(
                "x1",
                "x2",
                mk_term::op2(op, mk_term::var("x1"), mk_term::var("x2")).with_pos(pos)
            ),
        }
    }
}

/// Turn record destructors using literal chunks only into static accesses
pub fn mk_access(access: RichTerm, root: RichTerm) -> RichTerm {
    let label = match *access.term {
        Term::StrChunks(ref chunks) => {
            chunks
                .iter()
                .fold(Some(String::new()), |acc, next| match (acc, next) {
                    (Some(mut acc), StrChunk::Literal(lit)) => {
                        acc.push_str(lit);
                        Some(acc)
                    }
                    _ => None,
                })
        }
        _ => None,
    };

    if let Some(label) = label {
        mk_term::op1(
            UnaryOp::StaticAccess(Ident {
                label,
                pos: access.pos,
            }),
            root,
        )
    } else {
        mk_term::op2(BinaryOp::DynAccess(), access, root)
    }
}

/// Elaborate a record field definition specified as a path, like `a.b.c = foo`, into a regular
/// flat definition `a = {b = {c = foo}}`.
///
/// # Preconditions
/// - /!\ path must be **non-empty**, otherwise this function panics
pub fn elaborate_field_path(
    path: Vec<FieldPathElem>,
    content: RichTerm,
) -> (FieldPathElem, RichTerm) {
    let mut it = path.into_iter();
    let fst = it.next().unwrap();

    let content = it.rev().fold(content, |acc, path_elem| match path_elem {
        FieldPathElem::Ident(id) => {
            let mut map = HashMap::new();
            map.insert(id, acc);
            Term::Record(map, Default::default()).into()
        }
        FieldPathElem::Expr(exp) => {
            let static_access = match exp.term.as_ref() {
                Term::StrChunks(chunks) => {
                    chunks
                        .iter()
                        .fold(Some(String::new()), |acc, next| match (acc, next) {
                            (Some(mut acc), StrChunk::Literal(lit)) => {
                                acc.push_str(lit);
                                Some(acc)
                            }
                            _ => None,
                        })
                }
                _ => None,
            };

            if let Some(static_access) = static_access {
                let id = Ident {
                    label: static_access,
                    pos: exp.pos,
                };

                let mut map = HashMap::new();
                map.insert(id, acc);
                Term::Record(map, Default::default()).into()
            } else {
                let empty = Term::Record(HashMap::new(), Default::default());
                mk_app!(mk_term::op2(BinaryOp::DynExtend(), exp, empty), acc)
            }
        }
    });

    (fst, content)
}

/// Build a record from a list of field definitions. If a field is defined several times, the
/// different definitions are merged.
pub fn build_record<I>(fields: I, attrs: RecordAttrs) -> Term
where
    I: IntoIterator<Item = (FieldPathElem, RichTerm)> + Debug,
{
    let mut static_map = HashMap::new();
    let mut dynamic_fields = Vec::new();

    fn insert_static_field(static_map: &mut HashMap<Ident, RichTerm>, id: Ident, t: RichTerm) {
        match static_map.entry(id) {
            Entry::Occupied(mut occpd) => {
                // temporary putting null in the entry to take the previous value.
                let prev = occpd.insert(Term::Null.into());
                occpd.insert(mk_term::op2(BinaryOp::Merge(), prev, t));
            }
            Entry::Vacant(vac) => {
                vac.insert(t);
            }
        }
    }

    fields.into_iter().for_each(|field| match field {
        (FieldPathElem::Ident(id), t) => insert_static_field(&mut static_map, id, t),
        (FieldPathElem::Expr(e), t) => {
            // Dynamic fields (whose name is defined by an interpolated string) have a different
            // semantics than fields whose name can be determined statically. However, static
            // fields with special characters are also parsed as an `Expr(e)`:
            //
            // ```
            // let x = "dynamic" in {"I#am.static" = false, "#{x}" = true}
            // ```
            //
            // Here, both fields are parsed as `Expr(e)`, but the first field is actually a static
            // one, just with special characters. The following code determines which fields are
            // actually static or not, and inserts them in the right location.
            match e.term.as_ref() {
                Term::StrChunks(chunks) => {
                    let mut buffer = String::new();

                    let is_static = chunks.iter().try_for_each(|chunk| match chunk {
                        StrChunk::Literal(s) => {
                            buffer.push_str(s);
                            Ok(())
                        }
                        StrChunk::Expr(..) => Err(()),
                    });

                    if is_static.is_ok() {
                        insert_static_field(
                            &mut static_map,
                            Ident {
                                label: buffer,
                                pos: e.pos,
                            },
                            t,
                        )
                    } else {
                        dynamic_fields.push((e, t));
                    }
                }
                // Currently `e` can only be string chunks, and this case should be unreachable,
                // but let's be future-proof
                _ => dynamic_fields.push((e, t)),
            }
        }
    });

    Term::RecRecord(static_map, dynamic_fields, attrs)
}

/// Make a span from parser byte offsets.
pub fn mk_span(src_id: FileId, l: usize, r: usize) -> RawSpan {
    RawSpan {
        src_id,
        start: (l as u32).into(),
        end: (r as u32).into(),
    }
}

pub fn mk_pos(src_id: FileId, l: usize, r: usize) -> TermPos {
    TermPos::Original(mk_span(src_id, l, r))
}

/// Same as `mk_span`, but for labels.
pub fn mk_label(types: Types, src_id: FileId, l: usize, r: usize) -> Label {
    Label {
        types,
        tag: String::new(),
        span: mk_span(src_id, l, r),
        arg_thunk: None,
        arg_pos: TermPos::None,
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
pub fn min_indent(chunks: &[StrChunk<RichTerm>]) -> usize {
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
/// ```text
/// let x = "I\nam\nindented" in
/// m#"
///   baseline
///     ${x}
///   end
/// "#m
/// ```
///
/// gives
///
/// ```text
///"baseline
///  I
///  am
///  indented
/// end"
/// ```
///
/// While
///
/// ```text
/// let x = "I\nam\nnot" in
/// m#"
///   baseline
///     ${x} sth
///   end
/// "#m
/// ```
///
/// gives
///
/// ```text
///"baseline
///  I
///am
///not sth
/// end"
/// ```
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

                *s = buffer;
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

/// Strip the indentation of a documentation metavalue. Wrap it as a literal string chunk and call
/// [`strip_indent`](./fn.strip_indent.html).
pub fn strip_indent_doc(doc: String) -> String {
    let chunk = vec![StrChunk::Literal(doc)];
    strip_indent(chunk)
        .into_iter()
        .map(|chunk| match chunk {
            StrChunk::Literal(s) => s,
            _ => panic!("expected literal string after indentation of documentation"),
        })
        .next()
        .expect("expected non-empty chunks after indentation of documentation")
}

/// Recursively checks for unbound type variables in a type
pub fn check_unbound(types: &Types, span: RawSpan) -> Result<(), ParseError> {
    // heavy lifting function, recurses into a type expression and returns a set of unbound vars
    fn find_unbound_vars(types: &Types, unbound_set: &mut HashSet<Ident>) {
        match &types.0 {
            AbsType::Var(ident) => {
                unbound_set.insert(ident.clone());
            }
            AbsType::Forall(ident, ty) => {
                // forall needs a "scoped" set for the variables in its nodes
                let mut forall_unbound_vars = HashSet::new();
                find_unbound_vars(ty, &mut forall_unbound_vars);

                forall_unbound_vars.remove(ident);

                // once the forall vars are recursed into and analyzed, the parent set and
                // the forall set are merged
                unbound_set.extend(forall_unbound_vars);
            }
            AbsType::Arrow(s, t) => {
                find_unbound_vars(s, unbound_set);
                find_unbound_vars(t, unbound_set);
            }
            AbsType::DynRecord(ty)
            | AbsType::StaticRecord(ty)
            | AbsType::List(ty)
            | AbsType::Enum(ty) => {
                find_unbound_vars(ty, unbound_set);
            }
            AbsType::RowExtend(_, opt_ty, ty) => {
                if let Some(ty) = opt_ty {
                    find_unbound_vars(ty, unbound_set);
                }

                find_unbound_vars(ty, unbound_set);
            }
            AbsType::Dyn()
            | AbsType::Bool()
            | AbsType::Num()
            | AbsType::Str()
            | AbsType::Sym()
            | AbsType::Flat(_)
            | AbsType::RowEmpty() => {}
        }
    }

    let mut unbound_set: HashSet<Ident> = HashSet::new();

    // recurse into type and find unbound type vars
    find_unbound_vars(types, &mut unbound_set);

    if !unbound_set.is_empty() {
        return Err(ParseError::UnboundTypeVariables(
            unbound_set.into_iter().collect(),
            span,
        ));
    }

    Ok(())
}
