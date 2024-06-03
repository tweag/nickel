//! Various helpers and companion code for the parser are put here to keep the grammar definition
//! uncluttered.
use indexmap::map::Entry;
use std::fmt::Debug;
use std::rc::Rc;

use codespan::FileId;

use super::error::ParseError;

use crate::{
    combine::Combine,
    eval::{
        merge::{merge_doc, split},
        operation::RecPriority,
    },
    identifier::LocIdent,
    label::{Label, MergeKind, MergeLabel},
    mk_app, mk_fun,
    position::{RawSpan, TermPos},
    term::pattern::{Pattern, PatternData},
    term::{
        make as mk_term,
        record::{Field, FieldMetadata, RecordAttrs, RecordData},
        *,
    },
    typ::Type,
};

use malachite::num::conversion::traits::{FromSciString, FromStringBase};

pub struct ParseNumberError;

pub fn parse_number_sci(slice: &str) -> Result<Rational, ParseNumberError> {
    Rational::from_sci_string(slice).ok_or(ParseNumberError)
}

pub fn parse_number_base(base: u8, slice: &str) -> Result<Rational, ParseNumberError> {
    Ok(Rational::from(
        Integer::from_string_base(base, slice).ok_or(ParseNumberError)?,
    ))
}

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

/// Left hand side of a record field declaration.
#[derive(Clone, Debug)]
pub enum FieldPathElem {
    /// A static field declaration: `{ foo = .. }`
    Ident(LocIdent),
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
pub enum ChunkLiteralPart {
    Str(String),
    Char(char),
}

/// A field definition atom. A field is defined by a path, a potential value, and associated
/// metadata.
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
        let last_ident = self.path.last().and_then(|elem| match elem {
            FieldPathElem::Ident(id) => Some(*id),
            FieldPathElem::Expr(_) => None,
        });

        let mut it = self.path.into_iter();
        let fst = it.next().unwrap();

        let content = it
            .rev()
            .fold(self.field.with_name(last_ident), |acc, path_elem| {
                // We first compute a position for the intermediate generated records (it's useful
                // in particular for the LSP). The position starts at the subpath corresponding to
                // the intermediate record and ends at the final value.
                //
                // unwrap is safe here becuase the initial content has a position, and we make sure
                // we assign a position for the next field.
                let pos = match path_elem {
                    FieldPathElem::Ident(id) => id.pos,
                    FieldPathElem::Expr(ref expr) => expr.pos,
                };
                // unwrap is safe here because every id should have a non-`TermPos::None` position
                let id_span = pos.unwrap();
                let acc_span = acc
                    .value
                    .as_ref()
                    .map(|value| value.pos.unwrap())
                    .unwrap_or(id_span);

                // `RawSpan::fuse` only returns `None` when the two spans are in different files.
                // A record field and its value *must* be in the same file, so this is safe.
                let pos = TermPos::Original(id_span.fuse(acc_span).unwrap());

                match path_elem {
                    FieldPathElem::Ident(id) => {
                        let mut fields = IndexMap::new();
                        fields.insert(id, acc);
                        Field::from(RichTerm::new(
                            Term::Record(RecordData {
                                fields,
                                ..Default::default()
                            }),
                            pos,
                        ))
                    }
                    FieldPathElem::Expr(exp) => {
                        let static_access = exp.term.as_ref().try_str_chunk_as_static_str();

                        if let Some(static_access) = static_access {
                            let id = LocIdent::new_with_pos(static_access, exp.pos);
                            let mut fields = IndexMap::new();
                            fields.insert(id, acc);
                            Field::from(RichTerm::new(
                                Term::Record(RecordData {
                                    fields,
                                    ..Default::default()
                                }),
                                pos,
                            ))
                        } else {
                            // The record we create isn't recursive, because it is only comprised of
                            // one dynamic field. It's just simpler to use the infrastructure of
                            // `RecRecord` to handle dynamic fields at evaluation time rather than
                            // right here
                            Field::from(RichTerm::new(
                                Term::RecRecord(RecordData::empty(), vec![(exp, acc)], None),
                                pos,
                            ))
                        }
                    }
                }
            });

        (fst, content)
    }

    /// Returns the identifier corresponding to this definition if the path is composed of exactly
    /// one element which is a static identifier. Returns `None` otherwise.
    pub fn path_as_ident(&self) -> Option<LocIdent> {
        if self.path.len() > 1 {
            return None;
        }

        self.path.first().and_then(|path_elem| match path_elem {
            FieldPathElem::Expr(_) => None,
            FieldPathElem::Ident(ident) => Some(*ident),
        })
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
            // We treat `UnaryOp::BoolAnd` and `UnaryOp::BoolOr` separately.
            // They should morally be binary operators, but we represent them as unary
            // operators internally so that their second argument is evaluated lazily.
            InfixOp::Unary(op @ UnaryOp::BoolAnd) | InfixOp::Unary(op @ UnaryOp::BoolOr) => {
                mk_fun!(
                    "x1",
                    "x2",
                    mk_app!(mk_term::op1(op, mk_term::var("x1")), mk_term::var("x2")).with_pos(pos)
                )
            }
            InfixOp::Unary(op) => mk_fun!("x", mk_term::op1(op, mk_term::var("x")).with_pos(pos)),
            InfixOp::Binary(op) => mk_fun!(
                "x1",
                "x2",
                mk_term::op2(op, mk_term::var("x1"), mk_term::var("x2")).with_pos(pos)
            ),
        }
    }
}

/// Trait for structures representing annotations which can be combined with a term to build
/// another term, or another structure holding a term, such as a field. `T` is the said target
/// structure.
pub trait AttachTerm<T> {
    fn attach_term(self, rt: RichTerm) -> T;
}

impl<T: Combine> Combine for Option<T> {
    fn combine(left: Self, right: Self) -> Self {
        match (left, right) {
            (None, None) => None,
            (None, Some(x)) | (Some(x), None) => Some(x),
            (Some(left), Some(right)) => Some(Combine::combine(left, right)),
        }
    }
}

impl Combine for FieldMetadata {
    /// Combine two field metadata into one. If data that can't be combined (typically, the
    /// documentation or the type annotation) are set by both, the left one's are kept.
    ///
    /// Note that no environment management operation such as closurization of contracts takes
    /// place, because this function is expected to be used on the AST before the evaluation (in
    /// the parser or during program transformation).
    fn combine(left: Self, right: Self) -> Self {
        let priority = match (left.priority, right.priority) {
            // Neutral corresponds to the case where no priority was specified. In that case, the
            // other priority takes precedence.
            (MergePriority::Neutral, p) | (p, MergePriority::Neutral) => p,
            // Otherwise, we keep the maximum of both priorities, as we would do when merging
            // values.
            (p1, p2) => std::cmp::max(p1, p2),
        };

        FieldMetadata {
            doc: merge_doc(left.doc, right.doc),
            annotation: Combine::combine(left.annotation, right.annotation),
            opt: left.opt || right.opt,
            // The resulting field will be suppressed from serialization if either of the fields to be merged is.
            not_exported: left.not_exported || right.not_exported,
            priority,
        }
    }
}

impl AttachTerm<Field> for FieldMetadata {
    fn attach_term(self, rt: RichTerm) -> Field {
        Field {
            value: Some(rt),
            metadata: self,
            pending_contracts: Default::default(),
        }
    }
}

impl Combine for LetMetadata {
    // Combine two let metadata into one. If `doc` is set by both, the left one's documentation
    // is kept.
    fn combine(left: Self, right: Self) -> Self {
        LetMetadata {
            doc: left.doc.or(right.doc),
            annotation: Combine::combine(left.annotation, right.annotation),
        }
    }
}

impl Combine for TypeAnnotation {
    /// Combine two type annotations. If both have `types` set, the final type
    /// is the one of the left annotation, while the right one's type is put
    /// inside the final `contracts`.
    ///
    /// Contracts are combined from left to right; the left one's are put first,
    /// then maybe the right one's type annotation and then the right one's
    /// contracts.
    fn combine(left: Self, right: Self) -> Self {
        let (typ, leftover) = match (left.typ, right.typ) {
            (left_ty @ Some(_), right_ty @ Some(_)) => (left_ty, right_ty),
            (left_ty, right_ty) => (left_ty.or(right_ty), None),
        };

        let contracts = left
            .contracts
            .into_iter()
            .chain(leftover)
            .chain(right.contracts)
            .collect();

        TypeAnnotation { typ, contracts }
    }
}

impl AttachTerm<RichTerm> for TypeAnnotation {
    fn attach_term(self, rt: RichTerm) -> RichTerm {
        if self.is_empty() {
            return rt;
        }

        let pos = rt.pos;
        RichTerm::new(Term::Annotated(self, rt), pos)
    }
}

/// Some constructs are introduced with the metadata pipe operator `|`, but aren't metadata per se
/// (ex: `rec force`/`rec default`). Those are collected in this extended annotation and then
/// desugared into standard metadata.
#[derive(Clone, Debug, Default)]
pub struct FieldExtAnnot {
    /// Standard metadata.
    pub metadata: FieldMetadata,
    /// Presence of an annotation `push force`
    pub rec_force: bool,
    /// Presence of an annotation `push default`
    pub rec_default: bool,
}

impl FieldExtAnnot {
    pub fn new() -> Self {
        Default::default()
    }
}

impl AttachTerm<Field> for FieldExtAnnot {
    fn attach_term(self, value: RichTerm) -> Field {
        let value = if self.rec_force || self.rec_default {
            let rec_prio = if self.rec_force {
                RecPriority::Top
            } else {
                RecPriority::Bottom
            };

            let pos = value.pos;
            Some(rec_prio.apply_rec_prio_op(value).with_pos(pos))
        } else {
            Some(value)
        };

        Field {
            value,
            metadata: self.metadata,
            pending_contracts: Default::default(),
        }
    }
}

impl Combine for FieldExtAnnot {
    fn combine(left: Self, right: Self) -> Self {
        let metadata = FieldMetadata::combine(left.metadata, right.metadata);
        let rec_force = left.rec_force || right.rec_force;
        let rec_default = left.rec_default || right.rec_default;

        FieldExtAnnot {
            metadata,
            rec_force,
            rec_default,
        }
    }
}

impl From<FieldMetadata> for FieldExtAnnot {
    fn from(metadata: FieldMetadata) -> Self {
        FieldExtAnnot {
            metadata,
            ..Default::default()
        }
    }
}

/// Turn dynamic accesses using literal chunks only into static accesses
pub fn mk_access(access: RichTerm, root: RichTerm) -> RichTerm {
    if let Some(label) = access.as_ref().try_str_chunk_as_static_str() {
        mk_term::op1(
            UnaryOp::RecordAccess(LocIdent::new_with_pos(label, access.pos)),
            root,
        )
    } else {
        mk_term::op2(BinaryOp::RecordGet, access, root)
    }
}

/// Build a record from a list of field definitions. If a field is defined several times, the
/// different definitions are merged.
pub fn build_record<I>(fields: I, attrs: RecordAttrs) -> Term
where
    I: IntoIterator<Item = (FieldPathElem, Field)> + Debug,
{
    let mut static_fields = IndexMap::new();
    let mut dynamic_fields = Vec::new();

    fn insert_static_field(
        static_fields: &mut IndexMap<LocIdent, Field>,
        id: LocIdent,
        field: Field,
    ) {
        match static_fields.entry(id) {
            Entry::Occupied(mut occpd) => {
                // temporarily putting an empty field in the entry to take the previous value.
                let prev = occpd.insert(Field::default());

                // unwrap(): the field's identifier must have a position during parsing.
                occpd.insert(merge_fields(id.pos.unwrap(), prev, field));
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
            let static_access = e.term.as_ref().try_str_chunk_as_static_str();

            if let Some(static_access) = static_access {
                insert_static_field(
                    &mut static_fields,
                    LocIdent::new_with_pos(static_access, e.pos),
                    t,
                )
            } else {
                dynamic_fields.push((e, t));
            }
        }
    });

    Term::RecRecord(
        RecordData::new(static_fields, attrs, None),
        dynamic_fields,
        None,
    )
}

/// Merge two fields by performing the merge of both their value (dynamically if
/// necessary, by introducing a merge operator) and their metadata (statically).
///
/// If the values of both fields are static records ([`Term::Record`]s), their
/// merge is computed statically. This prevents building terms whose depth is
/// linear in the number of fields if partial definitions are involved. This
/// manifested in https://github.com/tweag/nickel/issues/1427.
fn merge_fields(id_span: RawSpan, field1: Field, field2: Field) -> Field {
    // FIXME: We're duplicating a lot of the logic in
    // [`eval::merge::merge_fields`] but not quite enough to actually factor
    // it out
    fn merge_values(id_span: RawSpan, t1: RichTerm, t2: RichTerm) -> RichTerm {
        let RichTerm {
            term: t1,
            pos: pos1,
        } = t1;
        let RichTerm {
            term: t2,
            pos: pos2,
        } = t2;
        match (t1.into_owned(), t2.into_owned()) {
            (Term::Record(rd1), Term::Record(rd2)) => {
                let split::SplitResult {
                    left,
                    center,
                    right,
                } = split::split(rd1.fields, rd2.fields);
                let mut fields = IndexMap::with_capacity(left.len() + center.len() + right.len());
                fields.extend(left);
                fields.extend(right);
                for (id, (field1, field2)) in center.into_iter() {
                    fields.insert(id, merge_fields(id_span, field1, field2));
                }
                Term::Record(RecordData::new(
                    fields,
                    RecordAttrs::combine(rd1.attrs, rd2.attrs),
                    None,
                ))
                .into()
            }
            (t1, t2) => mk_term::op2(
                BinaryOp::Merge(MergeLabel {
                    span: id_span,
                    kind: MergeKind::PiecewiseDef,
                }),
                RichTerm::new(t1, pos1),
                RichTerm::new(t2, pos2),
            ),
        }
    }

    let (value, priority) = match (field1.value, field2.value) {
        (Some(t1), Some(t2)) if field1.metadata.priority == field2.metadata.priority => (
            Some(merge_values(id_span, t1, t2)),
            field1.metadata.priority,
        ),
        (Some(t), _) if field1.metadata.priority > field2.metadata.priority => {
            (Some(t), field1.metadata.priority)
        }
        (_, Some(t)) if field1.metadata.priority < field2.metadata.priority => {
            (Some(t), field2.metadata.priority)
        }
        (Some(t), None) => (Some(t), field1.metadata.priority),
        (None, Some(t)) => (Some(t), field2.metadata.priority),
        (None, None) => (None, Default::default()),
        _ => unreachable!(),
    };

    // At this stage, pending contracts aren't filled nor meaningful, and should all be empty.
    debug_assert!(field1.pending_contracts.is_empty() && field2.pending_contracts.is_empty());
    Field {
        value,
        // [`FieldMetadata::combine`] produces subtly different behaviour from
        // the runtime merging code, which is what we need to replicate here
        metadata: FieldMetadata {
            doc: merge_doc(field1.metadata.doc, field2.metadata.doc),
            annotation: Combine::combine(field1.metadata.annotation, field2.metadata.annotation),
            opt: field1.metadata.opt && field2.metadata.opt,
            not_exported: field1.metadata.not_exported || field2.metadata.not_exported,
            priority,
        },
        pending_contracts: Vec::new(),
    }
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
pub fn mk_label(typ: Type, src_id: FileId, l: usize, r: usize) -> Label {
    Label {
        typ: Rc::new(typ),
        span: mk_span(src_id, l, r),
        ..Default::default()
    }
}

/// Same as `mk_span`, but for merge labels. The kind is set to the default one
/// (`MergeKind::Standard`).
pub fn mk_merge_label(src_id: FileId, l: usize, r: usize) -> MergeLabel {
    MergeLabel {
        span: mk_span(src_id, l, r),
        kind: Default::default(),
    }
}

/// Generate a `Let` or a `LetPattern` (depending on whether `assgn` has a record pattern) from
/// the parsing of a let definition. This function fails if the definition has both a pattern
/// and is recursive because recursive let-patterns are currently not supported.
pub fn mk_let(
    rec: bool,
    pat: Pattern,
    t1: RichTerm,
    t2: RichTerm,
    span: RawSpan,
) -> Result<RichTerm, ParseError> {
    match pat.data {
        PatternData::Any(id) if rec => Ok(mk_term::let_rec_in(id, t1, t2)),
        PatternData::Any(id) => Ok(mk_term::let_in(id, t1, t2)),
        _ if rec => Err(ParseError::RecursiveLetPattern(span)),
        _ => Ok(mk_term::let_pat(pat, t1, t2)),
    }
}

/// Generate a `Fun` (when the pattern is trivial) or a `FunPattern` from the parsing of a function
/// definition. This function panics if the definition somehow has neither an `Ident` nor a
/// non-`Empty` `Destruct` pattern.
pub fn mk_fun(pat: Pattern, body: RichTerm) -> Term {
    match pat.data {
        PatternData::Any(id) => Term::Fun(id, body),
        _ => Term::FunPattern(pat, body),
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
/// Determine the minimum indentation level of a multi-line string via [`min_indent`], and strip an
/// equal number of whitespace characters (` ` or `\t`) from the beginning of each line. If the last
/// line is empty or consist only of whitespace characters, it is filtered out.
///
/// The indentation of interpolated expressions in a multi-line string follow the rules:
/// - if an interpolated expression is alone on a line with whitespaces, its indentation -- minus
///   the common minimal indentation -- is stored and when the expression will be evaluated, each
///   new line will be prepended with this indentation level.
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

#[cfg(test)]
mod tests {
    use crate::typ::TypeF;

    use super::*;

    #[test]
    fn contract_annotation_order() {
        let ty1 = LabeledType {
            typ: TypeF::Number.into(),
            label: Label::dummy(),
        };
        let annot1 = TypeAnnotation {
            typ: None,
            contracts: vec![ty1.clone()],
        };

        let ty2 = LabeledType {
            typ: TypeF::Bool.into(),
            label: Label::dummy(),
        };
        let annot2 = TypeAnnotation {
            typ: None,
            contracts: vec![ty2.clone()],
        };

        assert_eq!(Combine::combine(annot1, annot2).contracts, vec![ty1, ty2])
    }

    /// Regression test for issue [#548](https://github.com/tweag/nickel/issues/548)
    #[test]
    fn type_annotation_combine() {
        let inner = TypeAnnotation {
            typ: Some(LabeledType {
                typ: Type::from(TypeF::Number),
                label: Label::dummy(),
            }),
            ..Default::default()
        };
        let outer = TypeAnnotation::default();
        let res = TypeAnnotation::combine(outer, inner);
        assert_ne!(res.typ, None);
    }
}
