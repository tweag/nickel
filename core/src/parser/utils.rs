//! Various helpers and companion code for the parser are put here to keep the grammar definition
//! uncluttered.
use std::{
    ffi::OsString,
    iter,
    {collections::HashSet, fmt::Debug},
};

use super::error::ParseError;

use crate::{
    app,
    bytecode::ast::{
        pattern::bindings::Bindings as _,
        record::{FieldDef, FieldMetadata},
        *,
    },
    cache::InputFormat,
    combine::CombineAlloc,
    eval::merge::merge_doc,
    files::FileId,
    fun,
    identifier::LocIdent,
    position::{RawSpan, TermPos},
    primop_app,
};

use malachite::{
    base::num::conversion::traits::{FromSciString, FromStringBase},
    Integer,
};

pub struct ParseNumberError;

pub fn parse_number_sci(slice: &str) -> Result<Number, ParseNumberError> {
    Number::from_sci_string(slice).ok_or(ParseNumberError)
}

pub fn parse_number_base(base: u8, slice: &str) -> Result<Number, ParseNumberError> {
    Ok(Number::from(
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

/// A string chunk literal atom, being either a string or a single char.
///
/// Because of the way the lexer handles escaping and interpolation, a contiguous static string
/// `"Some \\ \%{escaped} string"` will be lexed as a sequence of such atoms.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ChunkLiteralPart {
    Str(String),
    Char(char),
}

/// The last field of a record, that can either be a normal field declaration or an ellipsis.
#[derive(Clone, Debug)]
pub enum LastField<'ast> {
    FieldDecl(FieldDecl<'ast>),
    Ellipsis,
}

/// A record field declaration, that can be either a standard field definition or an `include`
/// expression.
#[derive(Clone, Debug)]
pub enum FieldDecl<'ast> {
    Def(FieldDef<'ast>),
    Include(LocIdent),
    IncludeList(Vec<LocIdent>),
}

/// The last match in a data structure pattern. This can either be a normal match, or an ellipsis
/// which can capture the rest of the data structure. The type parameter `P` is the type of the
/// pattern of the data structure (ellipsis are supported for both array and record patterns).
///
/// # Example
///
/// - In `{foo={}, bar}`, the last match is an normal match.
/// - In `{foo={}, bar, ..}`, the last match is a non-capturing ellipsis.
/// - In `{foo={}, bar, ..rest}`, the last match is a capturing ellipsis.
#[derive(Debug, PartialEq, Clone)]
pub enum LastPattern<P> {
    /// The last field is a normal match. In this case the pattern is "closed" so every record
    /// fields should be matched.
    Normal(P),
    /// The pattern is "open" `, ..}`. Optionally you can bind a record containing the remaining
    /// fields to an `Identifier` using the syntax `, ..y}`.
    Ellipsis(Option<LocIdent>),
}

/// Trait for operators that can be eta-expanded to a function.
pub(super) trait EtaExpand {
    /// Eta-expand an operator. This wraps an operator, for example `==`, as a function `fun x1 x2
    /// => x1 == x2`. Propagate the position of the curried operator to the generated primop apps
    /// for better error reporting.
    fn eta_expand(self, alloc: &AstAlloc, pos: TermPos) -> Node<'_>;
}

/// An infix operator that is not applied. Used for the curried operator syntax (e.g `(==)`)
pub(super) struct InfixOp(pub(super) primop::PrimOp);

impl EtaExpand for InfixOp {
    fn eta_expand(self, alloc: &AstAlloc, pos: TermPos) -> Node<'_> {
        // We could use `LocIdent::fresh` for the  newly introduced function parameters. However,
        // it has the issue that pretty printing them doesn't result in valid Nickel anymore. This
        // is why we prefer normal identifier like `x` or `y`.
        match self {
            // We treat `UnaryOp::BoolAnd` and `UnaryOp::BoolOr` separately.
            //
            // They are unary operators taking a second lazy argument, but the current mainine
            // evaluator expects that they are always fully applied (including to their argument).
            // That is, Nickel currently doesn't support a partial application like `%bool_or%
            // <arg1>` (which is fine, because the latter isn't actually representable in the
            // source language: `BoolOr` is only expressible through the infix syntax `<arg1> ||
            // <arg2>`). Thus, instead of eta-expanding to `fun x => <op> x` as we would for other
            // unary operators, we eta-expand to `fun x1 x2 => <op> x1 x2`.
            InfixOp(op @ primop::PrimOp::BoolAnd) | InfixOp(op @ primop::PrimOp::BoolOr) => {
                let fst_arg = LocIdent::from("x");
                let snd_arg = LocIdent::from("y");

                fun!(
                    alloc,
                    fst_arg,
                    snd_arg,
                    app!(
                        alloc,
                        primop_app!(alloc, op, builder::var(fst_arg)),
                        builder::var(snd_arg),
                    )
                    .with_pos(pos),
                )
                .node
            }
            // `RecordGet field record` corresponds to `record."%{field}"`. Using the curried
            // version `(.)` has thus reversed argument corresponding to the `RecordGet` primop, so
            // we need to flip them.
            InfixOp(op @ primop::PrimOp::RecordGet) => {
                let fst_arg = LocIdent::new("x");
                let snd_arg = LocIdent::new("y");

                fun!(
                    alloc,
                    fst_arg,
                    snd_arg,
                    primop_app!(alloc, op, builder::var(snd_arg), builder::var(fst_arg))
                        .with_pos(pos),
                )
                .node
            }
            InfixOp(op) => {
                let vars: Vec<_> = (0..op.arity())
                    .map(|i| LocIdent::from(format!("x{i}")))
                    .collect();
                let fun_args: Vec<_> = vars.iter().map(|arg| pattern::Pattern::any(*arg)).collect();
                let args: Vec<_> = vars.into_iter().map(builder::var).collect();

                alloc.fun(fun_args, alloc.prim_op(op, args).spanned(pos))
            }
        }
    }
}

/// Additional infix operators that aren't proper primitive operations in the Nickel AST but are
/// still available in the surface syntax (and desugared at parsing time). They can still be used
/// in a curried form so they need a wrapper and an `EtaExpand` implementation.
pub(super) enum ExtendedInfixOp {
    /// The reverse application operation or pipe operator `|>`.
    ReverseApp,
    /// The inequality operator `!=`.
    NotEqual,
}

impl EtaExpand for ExtendedInfixOp {
    fn eta_expand(self, alloc: &AstAlloc, pos: TermPos) -> Node<'_> {
        match self {
            ExtendedInfixOp::ReverseApp => {
                let fst_arg = LocIdent::from("x");
                let snd_arg = LocIdent::from("y");

                fun!(
                    alloc,
                    fst_arg,
                    snd_arg,
                    app!(alloc, builder::var(snd_arg), builder::var(fst_arg)).with_pos(pos),
                )
                .node
            }
            ExtendedInfixOp::NotEqual => {
                let fst_arg = LocIdent::from("x");
                let snd_arg = LocIdent::from("y");

                fun!(
                    alloc,
                    fst_arg,
                    snd_arg,
                    primop_app!(
                        alloc,
                        primop::PrimOp::BoolNot,
                        primop_app!(
                            alloc,
                            primop::PrimOp::Eq,
                            builder::var(fst_arg),
                            builder::var(snd_arg),
                        )
                        .with_pos(pos),
                    )
                    .with_pos(pos),
                )
                .node
            }
        }
    }
}

/// Trait for structures representing annotations which can be combined with a term to build
/// another term, or another structure holding a term, such as a field. `T` is the said target
/// structure.
pub trait AttachToAst<'ast, T> {
    fn attach_to_ast(self, alloc: &'ast AstAlloc, ast: Ast<'ast>) -> T;
}

impl<'ast> CombineAlloc<'ast> for FieldMetadata<'ast> {
    /// Combine two field metadata into one. If data that can't be combined (typically, the
    /// documentation or the type annotation) are set by both, the left one's are kept.
    fn combine(alloc: &'ast AstAlloc, left: Self, right: Self) -> Self {
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
            annotation: CombineAlloc::combine(alloc, left.annotation, right.annotation),
            opt: left.opt || right.opt,
            // The resulting field will be suppressed from serialization if either of the fields to be merged is.
            not_exported: left.not_exported || right.not_exported,
            priority,
        }
    }
}

impl<'ast> CombineAlloc<'ast> for LetMetadata<'ast> {
    /// Combine two let metadata into one. Same as `FieldMetadata::combine` but restricted to the
    /// metadata that can be associated to a let block.
    fn combine(alloc: &'ast AstAlloc, left: Self, right: Self) -> Self {
        LetMetadata {
            doc: merge_doc(left.doc, right.doc),
            annotation: CombineAlloc::combine(alloc, left.annotation, right.annotation),
        }
    }
}

impl<'ast> CombineAlloc<'ast> for Annotation<'ast> {
    /// Combine two annotations. If both have `types` set, the final type
    /// is the one of the left annotation, while the right one's type is put
    /// inside the final `contracts`.
    ///
    /// Contracts are combined from left to right; the left one's are put first,
    /// then maybe the right one's type annotation and then the right one's
    /// contracts.
    fn combine(alloc: &'ast AstAlloc, left: Self, right: Self) -> Self {
        let (typ, leftover) = match (left.typ, right.typ) {
            (left_ty @ Some(_), right_ty @ Some(_)) => (left_ty, right_ty),
            (left_ty, right_ty) => (left_ty.or(right_ty), None),
        };

        let contracts: Vec<_> = left
            .contracts
            .iter()
            .cloned()
            .chain(leftover)
            .chain(right.contracts.iter().cloned())
            .collect();

        alloc.annotation(typ, contracts)
    }
}

impl<'ast> AttachToAst<'ast, Ast<'ast>> for Annotation<'ast> {
    fn attach_to_ast(self, alloc: &'ast AstAlloc, ast: Ast<'ast>) -> Ast<'ast> {
        if self.is_empty() {
            return ast;
        }

        let pos = ast.pos;
        Ast {
            node: alloc.annotated(self, ast),
            pos,
        }
    }
}

/// Takes a record access written as `foo."<access>"`, and either turn it into a static access
/// whenever possible (when `<access>` is a static string without interpolation), or into a dynamic
/// `%record/get%` access otherwise.
pub fn mk_access<'ast>(alloc: &'ast AstAlloc, access: Ast<'ast>, root: Ast<'ast>) -> Node<'ast> {
    if let Some(label) = access.node.try_str_chunk_as_static_str() {
        alloc.prim_op(
            primop::PrimOp::RecordStatAccess(LocIdent::new_with_pos(label, access.pos)),
            iter::once(root),
        )
    } else {
        alloc.prim_op(primop::PrimOp::RecordGet, [access, root])
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

/// Checks that there are no duplicate bindings in a let block (when bindins are simple, that is
/// they aren't pattern), and builds the corresponding let block node if the check passes.
pub fn mk_let<'ast>(
    alloc: &'ast AstAlloc,
    rec: bool,
    bindings: Vec<LetBinding<'ast>>,
    body: Ast<'ast>,
) -> Result<Node<'ast>, ParseError> {
    // Check for duplicate names across the different bindings. We
    // don't check for duplicate names within a single binding because
    // there are backwards-compatibility constraints (e.g., see
    // `RecordPattern::check_dup`).
    let mut seen_bindings: HashSet<LocIdent> = HashSet::new();

    for b in &bindings {
        let new_bindings = b.pattern.bindings();
        for binding in &new_bindings {
            if let Some(old) = seen_bindings.get(&binding.id) {
                return Err(ParseError::DuplicateIdentInLetBlock {
                    ident: binding.id,
                    prev_ident: *old,
                });
            }
        }

        seen_bindings.extend(new_bindings.into_iter().map(|binding| binding.id));
    }

    Ok(alloc.let_block(bindings, body, rec))
}

pub fn mk_import_based_on_filename(
    alloc: &AstAlloc,
    path: String,
    _span: RawSpan,
) -> Result<Node<'_>, ParseError> {
    let path = OsString::from(path);
    let format: Option<InputFormat> =
        InputFormat::from_path(std::path::Path::new(path.as_os_str()));

    // Fall back to InputFormat::Nickel in case of unknown filename extension for backwards compatiblilty.
    let format = format.unwrap_or_default();

    Ok(alloc.import_path(path, format))
}

pub fn mk_import_explicit(
    alloc: &AstAlloc,
    path: String,
    format: LocIdent,
    span: RawSpan,
) -> Result<Node<'_>, ParseError> {
    let path = OsString::from(path);
    let Ok(format) = format.label().parse::<InputFormat>() else {
        return Err(ParseError::InvalidImportFormat { span });
    };

    Ok(alloc.import_path(path, format))
}

/// Determine the minimal level of indentation of a multi-line string.
///
/// The result is determined by computing the minimum indentation level among all lines, where the
/// indentation level of a line is the number of consecutive whitespace characters, which are
/// either a space or a tab, counted from the beginning of the line. If a line is empty or consist
/// only of whitespace characters, it is ignored.
pub fn min_indent(chunks: &[StringChunk<Ast<'_>>]) -> usize {
    let mut min: usize = usize::MAX;
    let mut current = 0;
    let mut start_line = true;

    for chunk in chunks.iter() {
        match chunk {
            StringChunk::Expr(_, _) if start_line => {
                if current < min {
                    min = current;
                }
                start_line = false;
            }
            StringChunk::Expr(_, _) => (),
            StringChunk::Literal(s) => {
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
pub fn strip_indent(chunks: &mut [StringChunk<Ast<'_>>]) {
    if chunks.is_empty() {
        return;
    }

    let min = min_indent(chunks);
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
            StringChunk::Literal(ref mut s) => {
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
            StringChunk::Expr(_, ref mut indent) => {
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
            Some(StringChunk::Expr(_, ref mut indent)) => *indent = 0,
            _ => unreachable!(
                "all elements in `unindent` should be expressions, but found a literal"
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        combine::Combine,
        label::Label,
        term::{LabeledType, TypeAnnotation},
        typ::{Type, TypeF},
    };

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
