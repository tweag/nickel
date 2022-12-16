//! Various helpers and companion code for the parser are put here to keep the grammar definition
//! uncluttered.
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

use codespan::FileId;

use super::error::ParseError;

use crate::{
    destruct::Destruct,
    eval::operation::RecPriority,
    identifier::Ident,
    label::Label,
    mk_fun,
    position::{RawSpan, TermPos},
    term::{
        make as mk_term,
        record::{Field, FieldMetadata, RecordAttrs, RecordData},
        BinaryOp, LabeledType, RichTerm, StrChunk, Term, TypeAnnotation, UnaryOp,
    },
    types::{TypeF, Types},
};

/// Distinguish between the standard string opening delimiter `"`, the multi-line string
/// opening delimter `m%"`, and the symbolic string opening delimiter `s%"`.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum StringStartDelimiter<'input> {
    Standard,
    Multiline,
    Symbolic(&'input str),
}

impl StringStartDelimiter<'_> {
    pub fn is_closed_by(&self, close: &StringEndDelimiter) -> bool {
        matches!(
            (self, close),
            (StringStartDelimiter::Standard, StringEndDelimiter::Standard)
                | (StringStartDelimiter::Multiline, StringEndDelimiter::Special)
                | (
                    StringStartDelimiter::Symbolic(_),
                    StringEndDelimiter::Special
                )
        )
    }

    pub fn needs_strip_indent(&self) -> bool {
        match self {
            StringStartDelimiter::Standard => false,
            StringStartDelimiter::Multiline | StringStartDelimiter::Symbolic(_) => true,
        }
    }
}

/// Distinguish between the standard string closing delimiter `"` and the "special" string
/// closing delimiter `"%`.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum StringEndDelimiter {
    Standard,
    Special,
}

/// Distinguish between a normal case `id => exp` and a default case `_ => exp`.
#[derive(Clone, Debug)]
pub enum MatchCase {
    Normal(Ident, RichTerm),
    Default(RichTerm),
}

/// Left hand side of a record field declaration.
#[derive(Clone, Debug)]
pub enum FieldPathElem {
    /// A static field declaration: `{ foo = .. }`
    Ident(Ident),
    /// A quoted field declaration: `{ "%{protocol}" = .. }`
    ///
    /// In practice, the argument must always be `StrChunks`, but since we also need to keep track
    /// of the associated span it's handier to just use a `RichTerm`.
    Expr(RichTerm),
}

pub type FieldPath = Vec<FieldPathElem>;

/// A string chunk literal atom, being either a string or a single char.
///
/// Because of the way the lexer handles escaping and interpolation, a contiguous static string
/// `"Some \\ \%{escaped} string"` will be lexed as a sequence of such atoms.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ChunkLiteralPart<'input> {
    Str(&'input str),
    Char(char),
}

#[derive(Clone, Debug)]
pub struct FieldDef {
    pub path: FieldPath,
    pub field: Field,
    pub pos: TermPos,
}

impl FieldDef {
    /// Elaborate a record field definition specified as a path, like `a.b.c = foo`, into a regular
    /// flat definition `a = {b = {c = foo}}`.
    ///
    /// # Preconditions
    /// - /!\ path must be **non-empty**, otherwise this function panics
    pub fn elaborate(self) -> (FieldPathElem, Field) {
        let mut it = self.path.into_iter();
        let fst = it.next().unwrap();

        let content = it.rev().fold(self.field, |acc, path_elem| {
            // We first compute a position for the intermediate generated records (it's useful in
            // particular for the LSP). The position starts at the subpath corresponding to the
            // intermediate record and ends at the final value.
            //
            // unwrap is safe here becuase the initial content has a position,
            // and we make sure we assign a position for the next field.
            let pos = match path_elem {
                FieldPathElem::Ident(id) => id.pos,
                FieldPathElem::Expr(ref expr) => expr.pos,
            };
            // unwrap is safe here because every id should have a non-`TermPos::None` position
            let id_span = pos.unwrap();
            let acc_span = acc.value.map(|value| value.pos.unwrap()).unwrap_or(id_span);

            // `RawSpan::fuse` only returns `None` when the two spans are in different files.
            // A record field and its value *must* be in the same file, so this is safe.
            let pos = TermPos::Original(RawSpan::fuse(id_span, acc_span).unwrap());

            match path_elem {
                FieldPathElem::Ident(id) => {
                    let mut fields = HashMap::new();
                    fields.insert(id, acc);
                    Field::from(RichTerm::new(Term::Record(RecordData { fields, ..Default::default() }), pos))
                }
                FieldPathElem::Expr(exp) => {
                    let static_access = exp.term.as_ref().try_str_chunk_as_static_str();

                    if let Some(static_access) = static_access {
                        let id = Ident::new_with_pos(static_access, exp.pos);
                        let mut fields = HashMap::new();
                        fields.insert(id, acc);
                        Field::from(RichTerm::new(Term::Record(RecordData { fields, ..Default::default() }), pos))
                    } else {
                        // The record we create isn't recursive, because it is only comprised of one
                        // dynamic field. It's just simpler to use the infrastructure of `RecRecord` to
                        // handle dynamic fields at evaluation time rather than right here
                        Field::from(RichTerm::new(Term::RecRecord(RecordData::empty(), vec![(exp, acc)], None), pos))
                    }
                }
            }
        });

        (fst, content)
    }
}

/// The last field of a record, that can either be a normal field declaration or an ellipsis.
#[derive(Clone, Debug)]
pub enum RecordLastField {
    Field(FieldDef),
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

/// General interface for structures representing a series of annotations.
pub trait Annot: Default + From<FieldMetadata> {
    /// Attach the annotation to a term.
    fn attach_value(self, value: RichTerm) -> Field;
    /// Combine two annotations.
    fn combine(outer: Self, inner: Self) -> Self;
}

/// Consistency of naming inside the parser module.
pub type MetaAnnot = FieldMetadata;

impl Annot for FieldMetadata {
    fn attach_value(self, value: RichTerm) -> Field {
        Field {
            value: Some(value),
            metadata: self,
        }
    }

    fn combine(outer: Self, inner: Self) -> Self {
        Self::flatten(outer, inner)
    }
}

impl Annot for TypeAnnotation {
    fn attach_value(self, value: RichTerm) -> Field {
        Field {
            value: Some(value),
            metadata: FieldMetadata {
                annotation: self,
                ..Default::default()
            }
        }
    }

    /// Combine two type annotations. If both have `types` set, the final type is the one of outer,
    /// while inner's type is put inside the final `contracts`.
    fn combine(outer: Self, inner: Self) -> Self {
        let (types, leftover) = match (inner.types, outer.types) {
            (outer_ty @ Some(_), inner_ty @ Some(_)) => (outer_ty, inner_ty),
            (outer_ty, inner_ty) => (outer_ty.or(inner_ty), None)
        };

        let contracts = inner.contracts.into_iter().chain(leftover.into_iter()).chain(outer.contracts.into_iter()).collect();

        TypeAnnotation { types, contracts }
    }

}

impl From<FieldMetadata> for TypeAnnotation {
    fn from(metadata: FieldMetadata) -> Self {
        metadata.annotation
    }
}

/// Used to combine annotations in a pattern. If at least one annotation is not `None`, then this
/// just calls [`Annot::combine`] and substitute a potential `None` by the default value.
///
/// If both arguments are `None`, we still need a label to report useful error diagnostics. In this
/// case, `combine_match_annots` returns a value with a dummy `Dyn` contract and a label with the
/// position set to `span`.
pub fn combine_match_annots(
    anns: Option<MetaAnnot>,
    default: Option<MetaAnnot>,
    span: RawSpan,
) -> MetaAnnot {
    match (anns, default) {
        (anns @ Some(_), default) | (anns, default @ Some(_)) => {
            Annot::combine(anns.unwrap_or_default(), default.unwrap_or_default())
        }
        (None, None) => FieldMetadata {
            annotation: TypeAnnotation {
                contracts: vec![LabeledType {
                    types: Types(TypeF::Dyn),
                    label: Label {
                        span,
                        ..Default::default()
                    },
                }],
                ..Default::default()
            },
            ..Default::default()
        },
    }
}

/// Some constructs are introduced with the metadata pipe operator `|`, but aren't metadata per se
/// (ex: `rec force`/`rec default`). Those are collected in this extended annotation and then
/// desugared into a standard metavalue.
#[derive(Clone, Debug, Default)]
pub struct FieldAnnot {
    /// Standard metadata.
    pub metadata: FieldMetadata,
    /// Presence of an annotation `push force`
    pub rec_force: bool,
    /// Presence of an annotation `push default`
    pub rec_default: bool,
}

impl FieldAnnot {
    pub fn new() -> Self {
        Default::default()
    }
}

impl Annot for FieldAnnot {
    fn attach_value(mut self, value: RichTerm) -> Field {
        let value = if self.rec_force || self.rec_default {
            let rec_prio = if self.rec_force {
                RecPriority::Top
            } else {
                RecPriority::Bottom
            };

            Some(rec_prio.apply_rec_prio_op(value).with_pos(value.pos))
        } else {
            Some(value)
        };

        Field {
            value,
            metadata: self.metadata,
        }
    }

    fn combine(outer: Self, inner: Self) -> Self {
        let metadata = FieldMetadata::flatten(outer.metadata, inner.metadata);
        let rec_force = outer.rec_force || inner.rec_force;
        let rec_default = outer.rec_default || inner.rec_default;

        FieldAnnot {
            metadata,
            rec_force,
            rec_default,
        }
    }
}

impl From<FieldMetadata> for FieldAnnot {
    fn from(metadata: FieldMetadata) -> Self {
        FieldAnnot {
            metadata,
            ..Default::default()
        }
    }
}

/// Turn dynamic accesses using literal chunks only into static accesses
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
            UnaryOp::StaticAccess(Ident::new_with_pos(label, access.pos)),
            root,
        )
    } else {
        mk_term::op2(BinaryOp::DynAccess(), access, root)
    }
}

/// Build a record from a list of field definitions. If a field is defined several times, the
/// different definitions are merged.
pub fn build_record<I>(fields: I, attrs: RecordAttrs) -> Term
where
    I: IntoIterator<Item = (FieldPathElem, Field)> + Debug,
{
    let mut static_fields = HashMap::new();
    let mut dynamic_fields = Vec::new();

    fn insert_static_field(static_fields: &mut HashMap<Ident, Field>, id: Ident, field: Field) {
        match static_fields.entry(id) {
            Entry::Occupied(mut occpd) => {
                // temporarily putting an empty field in the entry to take the previous value.
                let prev = occpd.insert(Field::default());

                // A field of a record without metadata AND without value is impossible
                occpd.insert(merge_fields(prev, field));
            }
            Entry::Vacant(vac) => {
                vac.insert(field);
            }
        }
    }

    fields.into_iter().for_each(|field| match field {
        (FieldPathElem::Ident(id), t) => insert_static_field(&mut static_fields, id, t),
        (FieldPathElem::Expr(e), t) => {
            // Dynamic fields (whose name is defined by an interpolated string) have a different
            // semantics than fields whose name can be determined statically. However, static
            // fields with special characters are also parsed as string chunks:
            //
            // ```
            // let x = "dynamic" in {"I%am.static" = false, "%{x}" = true}
            // ```
            //
            // Here, both fields are parsed as `StrChunks`, but the first field is actually a
            // static one, just with special characters. The following code determines which fields
            // are actually static or not, and inserts them in the right location.
            match e.term.as_ref() {
                Term::StrChunks(chunks) => {
                    let mut buffer = String::new();

                    let is_static = chunks
                        .iter()
                        .try_for_each(|chunk| match chunk {
                            StrChunk::Literal(s) => {
                                buffer.push_str(s);
                                Ok(())
                            }
                            StrChunk::Expr(..) => Err(()),
                        })
                        .is_ok();

                    if is_static {
                        insert_static_field(
                            &mut static_fields,
                            Ident::new_with_pos(buffer, e.pos),
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

    Term::RecRecord(
        RecordData::new(
            static_fields
                .into_iter()
                .map(|(id, value)| (id, value))
                .collect(),
            attrs,
            None,
        ),
        dynamic_fields,
        None,
    )
}

/// Merge two fields by performing the merge of both their value and MetaValue if any.
fn merge_fields(field1: Field, field2: Field) -> Field {
    let value = match (field1.value, field2.value) {
        (Some(t1), Some(t2)) => Some(mk_term::op2(BinaryOp::Merge(), t1, t2)),
        (Some(t), None) | (None, Some(t)) => Some(t),
        (None, None) => None,
    };

    let metadata = FieldMetadata::flatten(field1.metadata, field2.metadata);

    Field { value, metadata }
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
        types: Rc::new(types),
        tag: String::new(),
        span: mk_span(src_id, l, r),
        arg_idx: None,
        arg_pos: TermPos::None,
        polarity: true,
        path: Vec::new(),
    }
}

/// Generate a `Let` or a `LetPattern` (depending on `pat` being empty or not) from a the parsing
/// of a let definition. This function fails if the definition has both a non-empty pattern and
/// is recursive (`pat != Destruct::Empty && rec`), because recursive let-patterns are currently
/// not supported.
pub fn mk_let(
    rec: bool,
    id: Option<Ident>,
    pat: Destruct,
    t1: RichTerm,
    t2: RichTerm,
    span: RawSpan,
) -> Result<RichTerm, ParseError> {
    let result = match pat {
        d @ (Destruct::Record { .. } | Destruct::Array { .. }) => {
            if rec {
                return Err(ParseError::RecursiveLetPattern(span));
            }
            mk_term::let_pat(id, d, t1, t2)
        }
        Destruct::Empty => {
            if let Some(id) = id {
                if rec {
                    mk_term::let_rec_in(id, t1, t2)
                } else {
                    mk_term::let_in(id, t1, t2)
                }
            } else {
                panic!("unexpected let-binding without pattern or identifier")
            }
        }
    };

    Ok(result)
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
/// Determine the minimum indentation level of a multi-line string via [`min_indent`], and strip an
/// equal number of whitespace characters (` ` or `\t`) from the beginning of each line. If the
/// last line is empty or consist only of whitespace characters, it is filtered out.
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
/// m%"
///   baseline
///     ${x}
///   end
/// "%
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
/// m%"
///   baseline
///     ${x} sth
///   end
/// "%
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
    // m%"
    //  some
    //    ${x} ${y}
    //    ${x}
    //  string
    // "%
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
/// [`strip_indent`].
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
