//! Pretty-printing of the Nickel AST.
use std::cell::Cell;
use std::fmt;

use crate::{
    bytecode::ast::{
        pattern::{
            ArrayPattern, ConstantPattern, ConstantPatternData, EnumPattern, OrPattern, Pattern,
            PatternData, RecordPattern, TailPattern,
        },
        primop::{OpPos, PrimOp},
        record::{FieldDef, FieldMetadata, FieldPathElem, MergePriority, Record},
        typ::{iter::RecordRowsItem, EnumRow, EnumRows, RecordRows, Type},
        Annotation, Ast, Import, LetBinding, MatchBranch, Node, Number, StringChunk,
    },
    cache::InputFormat,
    identifier::{Ident, LocIdent},
    parser::lexer::KEYWORDS,
    typ::{DictTypeFlavour, EnumRowsF, RecordRowF, RecordRowsF, TypeF},
};

use malachite::base::num::{basic::traits::Zero, conversion::traits::ToSci};
use once_cell::sync::Lazy;
use pretty::docs;
pub use pretty::{DocAllocator, DocBuilder, Pretty};
use regex::Regex;

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum StringRenderStyle {
    /// Never allow rendering as a multiline string
    ForceMonoline,
    /// Render as a multiline string if the string contains a newline
    Multiline,
}

pub trait IsAtom {
    /// Determine if an expression is an atom of the surface syntax. Atoms are basic elements of
    /// the syntax that can freely substituted without being parenthesized.
    fn is_atom(&self) -> bool;
}

impl IsAtom for Node<'_> {
    fn is_atom(&self) -> bool {
        match self {
        Node::Null
        | Node::Bool(..)
        | Node::String(..)
        | Node::StringChunks(..)
        | Node::EnumVariant { tag: _, arg: None }
        | Node::Record(_)
        | Node::Array(_)
        | Node::Var(_)
        | Node::PrimOpApp { op: PrimOp::RecordStatAccess(_), args: _ }
        | Node::PrimOpApp { op: PrimOp::RecordGet, args: _ }
        // Those special cases aren't really atoms, but mustn't be parenthesized because they
        // are really functions taking additional non-strict arguments and printed as "partial"
        // infix operators.
        //
        // For example, `Op1(BoolOr, Var("x"))` is currently printed as `x ||`. Such operators
        // must never be parenthesized, such as in `(x ||)`.
        //
        // We might want a more robust mechanism for pretty printing such operators.
        | Node::PrimOpApp { op: PrimOp::BoolAnd, args: _ }
        | Node::PrimOpApp { op: PrimOp::BoolOr, args: _ } => true,
        // A number with a minus sign as a prefix isn't a proper atom
        Node::Number(n) => **n >= 0,
        Node::Type(typ) => typ.is_atom(),
        Node::Let {..}
        | Node::IfThenElse {..}
        | Node::EnumVariant {..}
        | Node::Match { .. }
        | Node::Fun{ .. }
        | Node::App{ .. }
        | Node::PrimOpApp {.. }
        | Node::Annotated{ .. }
        | Node::Import(_)
        | Node::ParseError(_) => false,
    }
    }
}

impl IsAtom for Type<'_> {
    fn is_atom(&self) -> bool {
        match &self.typ {
            TypeF::Dyn
            | TypeF::Number
            | TypeF::Bool
            | TypeF::String
            | TypeF::Var(_)
            | TypeF::Record(_)
            | TypeF::Enum(_) => true,
            TypeF::Contract(ast) => ast.node.is_atom(),
            _ => false,
        }
    }
}

/// Helper to find the min number of `%` sign needed to interpolate a string containing this chunk.
fn min_interpolate_sign(text: &str) -> usize {
    let reg = Regex::new(r#"([%]+\{)|("[%]+)"#).unwrap();
    reg.find_iter(text)
        .map(|m| {
            // We iterate over all sequences `%+{` and `"%+`, which could clash with the
            // interpolation syntax, and return the maximum number of `%` insead each sequence.
            //
            // For the case of a closing delimiter `"%`, we could actually be slightly smarter as we
            // don't necessarily need more `%`, but just a different number of `%`. For example, if
            // the string contains only one `"%%`, then single `%` delimiters like `m%"` and `"%`
            // would be fine. But picking the maximum results in a simpler algorithm for now, which
            // we can update later if necessary.
            m.end() - m.start()
        })
        .max()
        .unwrap_or(1)
}

/// Escape a string to make it suitable for placing between quotes in Nickel
fn escape(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace("%{", "\\%{")
        .replace('\"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
}

static QUOTING_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new("^_*[a-zA-Z][_a-zA-Z0-9-]*$").unwrap());

/// Return the string representation of an identifier, and add enclosing double quotes if the
/// label isn't a valid identifier according to the parser, for example if it contains a
/// special character like a space.
pub fn ident_quoted(ident: impl Into<Ident>) -> String {
    let ident = ident.into();
    let label = ident.label();
    if QUOTING_REGEX.is_match(label) && !KEYWORDS.contains(&label) {
        String::from(label)
    } else {
        format!("\"{}\"", escape(label))
    }
}

/// Return a string representation of an identifier, adding enclosing double quotes if
/// the label isn't valid for an enum tag. This is like `ident_quoted` except that keywords
/// aren't wrapped in quotes (because `'if` is a valid enum tag, for example).
pub fn enum_tag_quoted(ident: impl Into<Ident>) -> String {
    let ident = ident.into();
    let label = ident.label();
    if QUOTING_REGEX.is_match(label) {
        String::from(label)
    } else {
        format!("\"{}\"", escape(label))
    }
}

/// Does a sequence of `StringChunk`s contain a literal newline?
fn contains_newline<T>(chunks: &[StringChunk<T>]) -> bool {
    chunks.iter().any(|chunk| match chunk {
        StringChunk::Literal(str) => str.contains('\n'),
        StringChunk::Expr(_, _) => false,
    })
}

/// Does a sequence of `StringChunk`s contain a carriage return? Lone carriage
/// returns are forbidden in Nickel's surface syntax.
fn contains_carriage_return<T>(chunks: &[StringChunk<T>]) -> bool {
    chunks.iter().any(|chunk| match chunk {
        StringChunk::Literal(str) => str.contains('\r'),
        StringChunk::Expr(_, _) => false,
    })
}

/// Determines if a type to be printed in type position needs additional parentheses.
///
/// Terms in type position don't need to be atoms: for example, we should pretty print `foo |
/// Contract arg1 arg2` without parentheses instead of `foo | (Contract arg1 arg2)`.
///
/// However, some terms (i.e. contracts) in type position still need parentheses in some cases, for
/// example when said term is a function function. This function precisely determines if the given
/// type is such a term.
fn needs_parens_in_type_pos(typ: &Type) -> bool {
    if let TypeF::Contract(ast) = &typ.typ {
        matches!(
            &ast.node,
            Node::Fun { .. } | Node::Let { .. } | Node::IfThenElse { .. } | Node::Import { .. }
        )
    } else {
        false
    }
}

pub fn fmt_pretty<T>(value: &T, f: &mut fmt::Formatter) -> fmt::Result
where
    T: for<'a> Pretty<'a, Allocator, ()> + Clone,
{
    let allocator = Allocator::default();
    let doc: DocBuilder<_, ()> = value.clone().pretty(&allocator);
    doc.render_fmt(80, f)
}

#[derive(Clone, Copy, Debug, Default)]
struct SizeBound {
    depth: usize,
    size: usize,
}

/// A pretty-printing allocator that supports rough bounds on the
/// size of the output.
///
/// When a pretty-printed object is too large, it will be abbreviated.
/// For example, a record will be abbreviated as "{…}".
///
/// The bounds are "rough" in that the depth bound only (currently; this might
/// be extended in the future) constrains the number of nested records: you can
/// still have deeply nested terms of other kinds. The size bound only constrains
/// the number of children of nested records. As such, neither constraint gives
/// precise control over the size of the output.
pub struct Allocator {
    inner: pretty::BoxAllocator,
    bound: Option<Cell<SizeBound>>,
}

/// The default `BoundedAllocator` imposes no constraints.
impl Default for Allocator {
    fn default() -> Self {
        Self {
            inner: pretty::BoxAllocator,
            bound: None,
        }
    }
}

impl Allocator {
    /// Creates a `BoundedAllocator` with constraints.
    pub fn bounded(max_depth: usize, max_size: usize) -> Self {
        Self {
            inner: pretty::BoxAllocator,
            bound: Some(Cell::new(SizeBound {
                depth: max_depth,
                size: max_size,
            })),
        }
    }

    /// Runs a callback with a "smaller" allocator.
    fn shrunken<'a, F: FnOnce(&'a Allocator) -> DocBuilder<'a, Self>>(
        &'a self,
        child_size: usize,
        f: F,
    ) -> DocBuilder<'a, Self> {
        if let Some(bound) = self.bound.as_ref() {
            let old = bound.get();
            bound.set(SizeBound {
                depth: old.depth.saturating_sub(1),
                size: child_size,
            });

            let ret = f(self);

            bound.set(old);

            ret
        } else {
            f(self)
        }
    }

    fn depth_constraint(&self) -> usize {
        self.bound.as_ref().map_or(usize::MAX, |b| b.get().depth)
    }

    fn size_constraint(&self) -> usize {
        self.bound.as_ref().map_or(usize::MAX, |b| b.get().size)
    }
}

impl<'a> DocAllocator<'a> for Allocator {
    type Doc = pretty::BoxDoc<'a>;

    fn alloc(&'a self, doc: pretty::Doc<'a, Self::Doc>) -> Self::Doc {
        self.inner.alloc(doc)
    }

    fn alloc_column_fn(
        &'a self,
        f: impl Fn(usize) -> Self::Doc + 'a,
    ) -> <Self::Doc as pretty::DocPtr<'a, ()>>::ColumnFn {
        self.inner.alloc_column_fn(f)
    }

    fn alloc_width_fn(
        &'a self,
        f: impl Fn(isize) -> Self::Doc + 'a,
    ) -> <Self::Doc as pretty::DocPtr<'a, ()>>::WidthFn {
        self.inner.alloc_width_fn(f)
    }
}

impl Allocator {
    fn record<'a>(&'a self, record: &Record) -> DocBuilder<'a, Self> {
        let size_per_child = self.size_constraint() / record.field_defs.len().max(1);
        if record.field_defs.is_empty() && !record.open {
            self.text("{}")
        } else if record.field_defs.is_empty() {
            "{..}".pretty(self)
        } else if size_per_child == 0 || self.depth_constraint() == 0 {
            "{…}".pretty(self)
        } else {
            self.shrunken(size_per_child, |alloc| {
                docs![
                    alloc,
                    alloc.line(),
                    alloc.intersperse(
                        record
                            .includes
                            .iter()
                            // For now we don't need to escape the included id, as it must be a
                            // valid variable name, and thus can't contain non-identifier
                            // characters such as spaces.
                            .map(|include| {
                                docs![
                                    alloc,
                                    "include",
                                    alloc.space(),
                                    include.ident.to_string(),
                                    self.field_metadata(&include.metadata, true)
                                ]
                            }),
                        docs![alloc, ",", alloc.line()]
                    ),
                    if !record.includes.is_empty() {
                        docs![alloc, ",", alloc.line()]
                    } else {
                        alloc.nil()
                    },
                    alloc.intersperse(record.field_defs.iter(), docs![alloc, ",", alloc.line()]),
                    if record.open {
                        docs![alloc, ",", alloc.line(), ".."]
                    } else {
                        alloc.nil()
                    }
                ]
                .nest(2)
                .append(self.line())
                .braces()
                .group()
            })
        }
    }

    fn record_type<'a>(&'a self, rows: &RecordRows) -> DocBuilder<'a, Self> {
        let child_count = rows.iter().count().max(1);
        let size_per_child = self.size_constraint() / child_count.max(1);
        if size_per_child == 0 || self.depth_constraint() == 0 {
            "{…}".pretty(self)
        } else {
            self.shrunken(size_per_child, |alloc| {
                let tail = match rows.iter().last() {
                    Some(RecordRowsItem::TailDyn) => docs![alloc, ";", alloc.line(), "Dyn"],
                    Some(RecordRowsItem::TailVar(id)) => {
                        docs![alloc, ";", alloc.line(), id.to_string()]
                    }
                    _ => alloc.nil(),
                };

                let rows = rows.iter().filter_map(|r| match r {
                    RecordRowsItem::Row(r) => Some(r),
                    _ => None,
                });

                docs![
                    alloc,
                    alloc.line(),
                    alloc.intersperse(rows, docs![alloc, ",", alloc.line()]),
                    tail
                ]
                .nest(2)
                .append(alloc.line())
                .braces()
                .group()
            })
        }
    }

    /// Escape the special characters in a string, including the newline character, so that it can
    /// be enclosed by double quotes a be a valid Nickel string.
    fn escaped_string<'a>(&'a self, s: &str) -> DocBuilder<'a, Self> {
        self.text(escape(s))
    }

    /// Print string chunks, either in the single line or multiline style.
    fn chunks<'a>(
        &'a self,
        chunks: &[StringChunk<Ast>],
        string_style: StringRenderStyle,
    ) -> DocBuilder<'a, Self> {
        let multiline = string_style == StringRenderStyle::Multiline
            && contains_newline(chunks)
            && !contains_carriage_return(chunks);

        let nb_perc = if multiline {
            chunks
                .iter()
                .map(
                    |c| {
                        if let StringChunk::Literal(s) = c {
                            min_interpolate_sign(s)
                        } else {
                            1
                        }
                    }, // be sure we have at least 1 `%` sign when an interpolation is present
                )
                .max()
                .unwrap_or(1)
        } else {
            1
        };

        let interp: String = "%".repeat(nb_perc);

        let line_maybe = if multiline {
            self.hardline()
        } else {
            self.nil()
        };

        let start_delimiter = if multiline {
            format!("m{interp}")
        } else {
            String::new()
        };

        let end_delimiter = if multiline {
            interp.clone()
        } else {
            String::new()
        };

        line_maybe
            .clone()
            .append(self.concat(chunks.iter().map(|c| {
                match c {
                    StringChunk::Literal(s) => {
                        if multiline {
                            self.concat(
                                // We do this manually instead of using
                                // `str::lines` because we need to be careful
                                // about whether a trailing newline appears at
                                // the end of the last line.
                                s.split_inclusive('\n').map(|line| {
                                    if let Some(s) = line.strip_suffix('\n') {
                                        self.text(s.to_owned()).append(self.hardline())
                                    } else {
                                        self.text(line.to_owned())
                                    }
                                }),
                            )
                        } else {
                            self.escaped_string(s)
                        }
                    }
                    StringChunk::Expr(e, _i) => docs![self, interp.clone(), "{", e, "}"],
                }
            })))
            .nest(if multiline { 2 } else { 0 })
            .append(line_maybe)
            .double_quotes()
            .enclose(start_delimiter, end_delimiter)
    }

    fn field_metadata<'a>(
        &'a self,
        metadata: &FieldMetadata,
        with_doc: bool,
    ) -> DocBuilder<'a, Self> {
        docs![
            self,
            &metadata.annotation,
            if with_doc {
                metadata
                    .doc
                    .map(|doc| {
                        docs![
                            self,
                            self.line(),
                            "| doc ",
                            self.chunks(
                                &[StringChunk::Literal(doc.to_owned())],
                                StringRenderStyle::Multiline
                            ),
                        ]
                    })
                    .unwrap_or_else(|| self.nil())
            } else {
                self.nil()
            },
            if metadata.opt {
                docs![self, self.line(), "| optional"]
            } else {
                self.nil()
            },
            match &metadata.priority {
                MergePriority::Bottom => docs![self, self.line(), "| default"],
                MergePriority::Neutral => self.nil(),
                MergePriority::Numeral(p) =>
                    docs![self, self.line(), "| priority ", p.to_sci().to_string()],
                MergePriority::Top => docs![self, self.line(), "| force"],
            }
        ]
    }

    fn atom<'a>(&'a self, ast: &Ast) -> DocBuilder<'a, Self> {
        ast.pretty(self).parens_if(!ast.node.is_atom())
    }

    /// Almost identical to calling `typ.pretty(self)`, but adds parentheses when the type is
    /// actually a contract that has a top-level form that needs parentheses (let-binding,
    /// if-then-else, etc.).
    ///
    /// Although terms can appear in type position as contracts, the parenthesis rules are slightly
    /// more restrictive than for a generic term: for example, `{foo | let x = Contract in x}` is
    /// not valid Nickel. It must be parenthesised as `{foo | (let x = Contract in x)}`.
    ///
    /// This method must be used whenever a type is rendered either as component of another type or
    /// in the position of an annotation. Rendering stand-alone types (for example as part of error
    /// messages) can avoid those parentheses and directly call to `typ.pretty(allocator)` instead.
    fn type_part<'a>(&'a self, typ: &Type) -> DocBuilder<'a, Self> {
        typ.pretty(self).parens_if(needs_parens_in_type_pos(typ))
    }

    /// Pretty printing of a restricted patterns that requires enum variant patterns and
    /// or-patterns to be parenthesized (typically function pattern arguments). The only difference
    /// with a general pattern is that for a function, a top-level enum variant pattern with an
    /// enum tag as an argument such as `'Foo 'Bar` must be parenthesized, because `fun 'Foo 'Bar
    /// => ...` is parsed as a function of two arguments, which are bare enum tags `'Foo` and
    /// `'Bar`. We must print `fun ('Foo 'Bar) => ..` instead.
    fn pat_with_parens<'a>(&'a self, pattern: &Pattern) -> DocBuilder<'a, Self> {
        pattern.pretty(self).parens_if(matches!(
            pattern.data,
            PatternData::Enum(EnumPattern {
                pattern: Some(_),
                ..
            }) | PatternData::Or(_)
        ))
    }

    /// Uniform handling of application-like syntax.
    fn application<'a, 'b, I, T, U>(&'a self, head: T, args: I) -> DocBuilder<'a, Self>
    where
        I: Iterator<Item = U>,
        T: for<'c> Pretty<'c, Self, ()> + Clone,
        U: for<'c> Pretty<'c, Self, ()> + Clone,
    {
        docs![
            self,
            head,
            self.concat(args.map(|arg| docs![self, self.line(), arg]))
                .nest(2)
        ]
        .group()
    }
}

trait NickelDocBuilderExt {
    /// Call `self.parens()` but only if `parens` is `true`.
    fn parens_if(self, parens: bool) -> Self;
}

impl NickelDocBuilderExt for DocBuilder<'_, Allocator> {
    fn parens_if(self, parens: bool) -> Self {
        if parens {
            self.parens()
        } else {
            self
        }
    }
}

/// A wrapper around an `Ast` that ensures it will be wrapped in parentheses if required.
#[derive(Copy, Clone)]
struct Atom<'ast> {
    inner: &'ast Ast<'ast>,
}

impl<'ast> Atom<'ast> {
    fn new(ast: &'ast Ast<'ast>) -> Self {
        Self { inner: ast }
    }
}

impl<'a> Pretty<'a, Allocator> for Atom<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator, ()> {
        allocator.atom(self.inner)
    }
}

impl<'a> Pretty<'a, Allocator> for LocIdent {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        allocator.text(self.into_label())
    }
}

impl<'a> Pretty<'a, Allocator> for &Annotation<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        docs![
            allocator,
            if let Some(typ) = &self.typ {
                docs![allocator, allocator.line(), ": ", allocator.type_part(typ)]
            } else {
                allocator.nil()
            },
            if !self.contracts.is_empty() {
                allocator.line()
            } else {
                allocator.nil()
            },
            allocator.intersperse(
                self.contracts
                    .iter()
                    .map(|t| { docs![allocator, "| ", allocator.type_part(t)] }),
                allocator.line(),
            )
        ]
    }
}

impl<'a> Pretty<'a, Allocator> for &PrimOp {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        match self {
            PrimOp::BoolNot => allocator.text("!"),
            PrimOp::BoolAnd | PrimOp::BoolOr | PrimOp::RecordStatAccess(_) => {
                unreachable!(
                    "These are handled specially since they are actually encodings \
                    of binary operators (`BoolAnd` and `BoolOr`) or need special \
                    formatting (`StaticAccess`). This currently happens in the `App` \
                    branch of `Term::pretty`"
                )
            }
            PrimOp::EnumEmbed(id) => docs![
                allocator,
                "%enum/embed%",
                docs![allocator, allocator.line(), id.to_string()].nest(2)
            ],
            PrimOp::Plus => allocator.text("+"),
            PrimOp::Sub => allocator.text("-"),

            PrimOp::Mult => allocator.text("*"),
            PrimOp::Div => allocator.text("/"),
            PrimOp::Modulo => allocator.text("%"),

            PrimOp::Eq => allocator.text("=="),
            PrimOp::LessThan => allocator.text("<"),
            PrimOp::GreaterThan => allocator.text(">"),
            PrimOp::GreaterOrEq => allocator.text(">="),
            PrimOp::LessOrEq => allocator.text("<="),

            PrimOp::Merge(_) => allocator.text("&"),

            PrimOp::StringConcat => allocator.text("++"),
            PrimOp::ArrayConcat => allocator.text("@"),

            PrimOp::RecordGet => allocator.text("."),

            op => allocator.text(format!("%{op}%")),
        }
    }
}

impl<'a> Pretty<'a, Allocator> for &Pattern<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        let alias_prefix = if let Some(alias) = self.alias {
            docs![
                allocator,
                alias.to_string(),
                allocator.space(),
                "@",
                allocator.space()
            ]
        } else {
            allocator.nil()
        };

        docs![allocator, alias_prefix, &self.data]
    }
}

impl<'a> Pretty<'a, Allocator> for &PatternData<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        match self {
            PatternData::Wildcard => allocator.text("_"),
            PatternData::Any(id) => allocator.as_string(id),
            PatternData::Record(rp) => rp.pretty(allocator),
            PatternData::Array(ap) => ap.pretty(allocator),
            PatternData::Enum(evp) => evp.pretty(allocator),
            PatternData::Constant(cp) => cp.pretty(allocator),
            PatternData::Or(op) => op.pretty(allocator),
        }
    }
}

impl<'a> Pretty<'a, Allocator> for &ConstantPattern<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        self.data.pretty(allocator)
    }
}

impl<'a> Pretty<'a, Allocator> for &ConstantPatternData<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        match self {
            ConstantPatternData::Bool(b) => allocator.as_string(b),
            ConstantPatternData::Number(n) => allocator.as_string(format!("{}", n.to_sci())),
            ConstantPatternData::String(s) => allocator.escaped_string(s).double_quotes(),
            ConstantPatternData::Null => allocator.text("null"),
        }
    }
}

impl<'a> Pretty<'a, Allocator> for &EnumPattern<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        docs![
            allocator,
            "'",
            enum_tag_quoted(&self.tag),
            if let Some(ref arg_pat) = self.pattern {
                docs![
                    allocator,
                    allocator.line(),
                    allocator.pat_with_parens(arg_pat)
                ]
            } else {
                allocator.nil()
            }
        ]
    }
}

impl<'a> Pretty<'a, Allocator> for &RecordPattern<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        let RecordPattern {
            patterns: matches,
            tail,
            ..
        } = self;
        docs![
            allocator,
            allocator.line(),
            allocator.intersperse(
                matches.iter().map(|field_pat| {
                    docs![
                        allocator,
                        field_pat.matched_id.to_string(),
                        allocator.field_metadata(
                            &FieldMetadata {
                                annotation: field_pat.annotation.clone(),
                                ..Default::default()
                            },
                            false
                        ),
                        if let Some(default) = field_pat.default.as_ref() {
                            docs![allocator, allocator.line(), "? ", allocator.atom(default),]
                        } else {
                            allocator.nil()
                        },
                        match &field_pat.pattern.data {
                            PatternData::Any(id) if *id == field_pat.matched_id => allocator.nil(),
                            _ => docs![allocator, allocator.line(), "= ", &field_pat.pattern],
                        },
                        ","
                    ]
                    .nest(2)
                }),
                allocator.line()
            ),
            match tail {
                TailPattern::Empty => allocator.nil(),
                TailPattern::Open => docs![allocator, allocator.line(), ".."],
                TailPattern::Capture(id) =>
                    docs![allocator, allocator.line(), "..", id.ident().to_string()],
            },
        ]
        .nest(2)
        .append(allocator.line())
        .braces()
        .group()
    }
}

impl<'a> Pretty<'a, Allocator> for &ArrayPattern<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        docs![
            allocator,
            allocator.intersperse(
                self.patterns.iter(),
                docs![allocator, ",", allocator.line()],
            ),
            if !self.patterns.is_empty() && self.is_open() {
                docs![allocator, ",", allocator.line()]
            } else {
                allocator.nil()
            },
            match self.tail {
                TailPattern::Empty => allocator.nil(),
                TailPattern::Open => allocator.text(".."),
                TailPattern::Capture(id) => docs![allocator, "..", id.ident().to_string()],
            },
        ]
        .nest(2)
        .brackets()
        .group()
    }
}

impl<'a> Pretty<'a, Allocator> for &OrPattern<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        docs![
            allocator,
            allocator.intersperse(
                self.patterns
                    .iter()
                    .map(|pat| allocator.pat_with_parens(pat)),
                docs![allocator, allocator.line(), "or", allocator.space()],
            ),
        ]
        .group()
    }
}

impl<'a> Pretty<'a, Allocator> for &Ast<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        self.node.pretty(allocator)
    }
}

impl<'a> Pretty<'a, Allocator> for &Node<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        match self {
            Node::Null => allocator.text("null"),
            Node::Bool(v) => allocator.as_string(v),
            Node::Number(n) => allocator.as_string(format!("{}", n.to_sci())),
            Node::String(v) => allocator.escaped_string(v).double_quotes(),
            Node::StringChunks(chunks) => allocator.chunks(chunks, StringRenderStyle::Multiline),
            Node::IfThenElse {
                cond,
                then_branch,
                else_branch,
            } => docs![
                allocator,
                "if ",
                *cond,
                " then",
                docs![allocator, allocator.line(), *then_branch].nest(2),
                allocator.line(),
                "else",
                docs![allocator, allocator.line(), *else_branch].nest(2)
            ]
            .group(),
            Node::Fun { args, body } => docs![
                allocator,
                "fun",
                docs![
                    allocator,
                    allocator.concat(args.iter().map(|pat| docs![
                        allocator,
                        allocator.line(),
                        allocator.pat_with_parens(pat)
                    ])),
                    allocator.line(),
                    "=>"
                ]
                .nest(2),
                docs![allocator, allocator.line(), body.pretty(allocator)].nest(2),
            ]
            .group(),
            Node::Let {
                bindings,
                body,
                rec,
            } => docs![
                allocator,
                "let",
                allocator.space(),
                if *rec {
                    docs![allocator, "rec", allocator.space()]
                } else {
                    allocator.nil()
                },
                allocator.intersperse(bindings.iter(), docs![allocator, ",", allocator.line()]),
                allocator.line(),
                "in",
            ]
            .nest(2)
            .group()
            .append(allocator.line())
            .append(body.pretty(allocator).nest(2))
            .group(),
            Node::App { head, args } => match &head.node {
                Node::PrimOpApp {
                    op: op @ (PrimOp::BoolAnd | PrimOp::BoolOr),
                    args: [fst],
                } => {
                    // There must be precisely one (lazy) argument here.
                    let [snd] = args else {
                        panic!("pretty-printer: ill-formed `&&` or `||` with more than 2 arguments")
                    };

                    docs![
                        allocator,
                        allocator.atom(fst),
                        allocator.line(),
                        match op {
                            PrimOp::BoolAnd => "&& ",
                            PrimOp::BoolOr => "|| ",
                            _ => unreachable!(),
                        },
                        allocator.atom(snd)
                    ]
                }
                _ => allocator.application(Atom::new(head), args.iter().map(Atom::new)),
            }
            .group(),
            Node::Var(id) => allocator.as_string(id),
            Node::EnumVariant { tag, arg } => docs![
                allocator,
                "'",
                allocator.text(enum_tag_quoted(tag)),
                if let Some(arg) = arg {
                    docs![allocator, allocator.line(), allocator.atom(arg)].nest(2)
                } else {
                    allocator.nil()
                }
            ]
            .group(),
            Node::Record(record_data) => allocator.record(record_data),
            Node::Match(data) => docs![
                allocator,
                "match ",
                docs![
                    allocator,
                    allocator.line(),
                    allocator.concat(data.branches.iter().map(|b| docs![
                        allocator,
                        b,
                        ",",
                        allocator.line()
                    ]))
                ]
                .nest(2)
                .braces()
            ]
            .group(),
            Node::Array(elts) => docs![
                allocator,
                allocator.line(),
                allocator.intersperse(elts.iter(), allocator.text(",").append(allocator.line()),),
            ]
            .nest(2)
            .append(allocator.line())
            .brackets()
            .group(),
            Node::PrimOpApp {
                op: PrimOp::RecordStatAccess(id),
                args: [arg],
            } => {
                docs![allocator, allocator.atom(arg), ".", ident_quoted(id)]
            }
            Node::PrimOpApp {
                op: PrimOp::BoolNot,
                args: [arg],
            } => docs![allocator, "!", allocator.atom(arg)],
            Node::PrimOpApp {
                op: PrimOp::BoolAnd,
                args: [arg],
            } => docs![allocator, "(&&)", allocator.line(), allocator.atom(arg)].group(),
            Node::PrimOpApp {
                op: PrimOp::BoolOr,
                args: [arg],
            } => docs![allocator, "(||)", allocator.line(), allocator.atom(arg)].group(),
            Node::PrimOpApp {
                op: PrimOp::RecordGet,
                args: [field, record],
            } => docs![allocator, record, ".", field],
            Node::PrimOpApp {
                op: PrimOp::Sub,
                args: [left, right],
            } if matches!(left.node, Node::Number(&Number::ZERO)) => {
                docs![allocator, allocator.text("-"), allocator.atom(right)]
            }
            Node::PrimOpApp { op, args } => match op.positioning() {
                OpPos::Postfix => docs![
                    allocator,
                    allocator
                        .intersperse(args.iter().map(|arg| allocator.atom(arg)), allocator.line()),
                    allocator.line(),
                    *op,
                ]
                .group(),
                OpPos::Infix if args.len() == 2 => docs![
                    allocator,
                    allocator.atom(&args[0]),
                    allocator.line(),
                    *op,
                    allocator.space(),
                    allocator.atom(&args[1]),
                ]
                .group(),
                // Infix for more than 2 arguments isn't really well defined, so we consider that
                // prefix.
                OpPos::Prefix | OpPos::Infix => {
                    allocator.application(*op, args.iter().map(Atom::new))
                }
            },
            Node::Annotated { annot, inner } => {
                allocator.atom(inner).append(annot.pretty(allocator))
            }
            Node::Import(Import::Path { path, format }) => {
                docs![
                    allocator,
                    "import",
                    allocator.space(),
                    allocator
                        .escaped_string(path.to_string_lossy().as_ref())
                        .double_quotes(),
                    if Some(*format) != InputFormat::from_path(path) {
                        docs![
                            allocator,
                            allocator.space(),
                            "as",
                            allocator.space(),
                            "'",
                            format.to_str()
                        ]
                    } else {
                        allocator.nil()
                    },
                ]
            }
            Node::Import(Import::Package { id }) => {
                allocator.text("import ").append(id.to_string())
            }
            // This type is in term position, so we don't need to add parentheses.
            Node::Type(typ) => typ.pretty(allocator),
            Node::ParseError(_) => allocator.text("%<parse error>"),
        }
    }
}

impl<'a> Pretty<'a, Allocator> for &FieldDef<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        docs![
            allocator,
            allocator.intersperse(self.path.iter(), allocator.text(".")),
            docs![
                allocator,
                allocator.field_metadata(&self.metadata, true),
                if let Some(value) = &self.value {
                    docs![
                        allocator,
                        if self.metadata.is_empty() {
                            docs![allocator, allocator.space(), "=", allocator.line()]
                        } else {
                            docs![allocator, allocator.line(), "=", allocator.space()]
                        },
                        value.pretty(allocator).nest(2)
                    ]
                    .group()
                } else {
                    allocator.nil()
                },
            ]
            .nest(2),
        ]
        .group()
    }
}

impl<'a> Pretty<'a, Allocator> for &FieldPathElem<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        match self {
            FieldPathElem::Ident(id) => allocator.text(ident_quoted(id)),
            FieldPathElem::Expr(ast) => match &ast.node {
                Node::StringChunks(chunks) => {
                    allocator.chunks(chunks, StringRenderStyle::ForceMonoline)
                }
                Node::ParseError(_) => allocator.text("%<parse error>"),
                _ => {
                    panic!(
                        "pretty printer: unexpected content of field path element (was not chunks or parse error)"
                    );
                }
            },
        }
    }
}

impl<'a> Pretty<'a, Allocator> for &LetBinding<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        docs![
            allocator,
            &self.pattern,
            allocator.field_metadata(&self.metadata.clone().into(), true),
            allocator.line(),
            "=",
            allocator.space(),
            &self.value,
        ]
    }
}

impl<'a> Pretty<'a, Allocator> for &EnumRows<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        match &self.0 {
            EnumRowsF::Empty => allocator.nil(),
            EnumRowsF::TailVar(id) => docs![allocator, ";", allocator.line(), id.to_string()],
            EnumRowsF::Extend { row, tail } => {
                let mut result = row.pretty(allocator);

                if let EnumRowsF::Extend { .. } = tail.0 {
                    result = result.append(allocator.text(",").append(allocator.line()));
                }

                result.append(*tail)
            }
        }
    }
}

impl<'a> Pretty<'a, Allocator> for &EnumRow<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        let mut result = allocator
            .text("'")
            .append(allocator.text(enum_tag_quoted(&self.id)));

        if let Some(typ) = self.typ.as_ref() {
            let ty_parenthesized = if typ.is_atom() {
                typ.pretty(allocator)
            } else {
                allocator
                    .text("(")
                    .append(allocator.line_())
                    .append(typ.pretty(allocator))
                    .append(allocator.line_())
                    .append(")")
            };

            result = result.append(allocator.text(" ")).append(ty_parenthesized);
        }

        result
    }
}

impl<'a> Pretty<'a, Allocator> for &RecordRows<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        // TODO: move some of this to NickelAllocatorExt so we can impose size limits
        match &self.0 {
            RecordRowsF::Empty => allocator.nil(),
            RecordRowsF::TailDyn => docs![allocator, ";", allocator.line(), "Dyn"],
            RecordRowsF::TailVar(id) => docs![allocator, ";", allocator.line(), id.to_string()],
            RecordRowsF::Extend { row, tail } => docs![
                allocator,
                row,
                if let RecordRowsF::Extend { .. } = tail.0 {
                    docs![allocator, ",", allocator.line()]
                } else {
                    allocator.nil()
                },
                *tail
            ],
        }
    }
}

impl<'a, 'ast, Ty> Pretty<'a, Allocator> for &RecordRowF<Ty>
where
    Ty: std::ops::Deref<Target = Type<'ast>>,
{
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        docs![
            allocator,
            ident_quoted(&self.id),
            " : ",
            allocator.type_part(self.typ.deref()),
        ]
    }
}

impl<'a> Pretty<'a, Allocator> for RecordRowF<&Type<'_>> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        (&self).pretty(allocator)
    }
}

impl<'a> Pretty<'a, Allocator> for &Type<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        use TypeF::*;
        match &self.typ {
            Dyn => allocator.text("Dyn"),
            Number => allocator.text("Number"),
            Bool => allocator.text("Bool"),
            String => allocator.text("String"),
            Array(ty) => if ty.is_atom() {
                docs![allocator, "Array", allocator.line(), *ty].nest(2)
            } else {
                docs![
                    allocator,
                    "Array (",
                    docs![allocator, allocator.line_(), *ty].nest(2),
                    allocator.line_(),
                    ")"
                ]
            }
            .group(),
            ForeignId => allocator.text("ForeignId"),
            Symbol => allocator.text("Symbol"),
            Contract(t) => t.pretty(allocator),
            Var(var) => allocator.as_string(var),
            Forall { var, body, .. } => {
                let mut curr = *body;
                let mut foralls = vec![var];
                while let Type {
                    typ: Forall { var, body, .. },
                    ..
                } = curr
                {
                    foralls.push(var);
                    curr = *body;
                }
                docs![
                    allocator,
                    "forall",
                    allocator.line(),
                    allocator.intersperse(
                        foralls.iter().map(|i| allocator.as_string(i)),
                        allocator.line(),
                    ),
                    ".",
                    allocator.line(),
                    allocator.type_part(curr)
                ]
                .nest(2)
                .group()
            }
            Enum(erows) => docs![allocator, allocator.line(), erows]
                .nest(2)
                .append(allocator.line())
                .enclose("[|", "|]")
                .group(),
            Record(rrows) => allocator.record_type(rrows),
            Dict {
                type_fields: ty,
                flavour: attrs,
            } => docs![
                allocator,
                allocator.line(),
                "_ ",
                match attrs {
                    DictTypeFlavour::Type => ":",
                    DictTypeFlavour::Contract => "|",
                },
                " ",
                allocator.type_part(ty),
            ]
            .nest(2)
            .append(allocator.line())
            .braces()
            .group(),
            Arrow(dom, codom) => docs![
                allocator,
                allocator
                    .type_part(dom)
                    .parens_if(matches!(dom.typ, Arrow(..) | Forall { .. }))
                    .nest(2),
                allocator.line(),
                "-> ",
                allocator
                    .type_part(codom)
                    .parens_if(matches!(codom.typ, Forall { .. }))
            ]
            .group(),
            Wildcard(_) => allocator.text("_"),
        }
    }
}

impl<'a> Pretty<'a, Allocator> for &MatchBranch<'_> {
    fn pretty(self, allocator: &'a Allocator) -> DocBuilder<'a, Allocator> {
        let guard = if let Some(guard) = &self.guard {
            docs![allocator, allocator.line(), "if", allocator.space(), guard]
        } else {
            allocator.nil()
        };

        docs![
            allocator,
            &self.pattern,
            guard,
            allocator.space(),
            "=>",
            docs![allocator, allocator.line(), self.body.pretty(allocator),].nest(2),
        ]
    }
}

/// Generate an implementation of `fmt::Display` for types that implement `Pretty`.
#[macro_export]
macro_rules! impl_display_from_bytecode_pretty {
    ($ty:ty) => {
        impl std::fmt::Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                $crate::bytecode::pretty::fmt_pretty(&self, f)
            }
        }
    };
}

/// Provide a method to pretty-print a long term, type, etc. (anything that implements `ToString`,
/// really) capped to a maximum length.
pub trait PrettyPrintCap: ToString {
    /// Pretty print an object capped to a given max length (in characters). Useful to limit the
    /// size of terms reported e.g. in typechecking errors. If the output of pretty printing is
    /// greater than the bound, the string is truncated to `max_width` and the last character after
    /// truncate is replaced by the ellipsis unicode character U+2026.
    fn pretty_print_cap(&self, max_width: usize) -> String {
        let output = self.to_string();

        if output.len() <= max_width {
            output
        } else {
            let (end, _) = output.char_indices().nth(max_width).unwrap();
            let mut truncated = String::from(&output[..end]);

            if max_width >= 2 {
                truncated.pop();
                truncated.push('\u{2026}');
            }

            truncated
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        bytecode::ast::AstAlloc,
        files::Files,
        parser::{
            grammar::{FixedTypeParser, TermParser},
            lexer::Lexer,
            ErrorTolerantParser,
        },
    };
    use pretty::Doc;

    use super::*;
    use indoc::indoc;

    /// Parse a type represented as a string.
    fn parse_type<'ast>(ast_alloc: &'ast AstAlloc, s: &str) -> Type<'ast> {
        let id = Files::new().add("<test>", s);

        FixedTypeParser::new()
            .parse_strict(ast_alloc, id, Lexer::new(s))
            .unwrap()
    }

    /// Parse a term represented as a string.
    fn parse_term<'ast>(ast_alloc: &'ast AstAlloc, s: &str) -> Ast<'ast> {
        let id = Files::new().add("<test>", s);

        TermParser::new()
            .parse_strict(ast_alloc, id, Lexer::new(s))
            .unwrap()
    }

    /// Parse a string representation `long` of a type, and assert that
    /// formatting it gives back `long`, if the line length is set to `80`, or
    /// alternatively results in `short`, if the line length is set to `0`
    #[track_caller]
    fn assert_long_short_type(long: &str, short: &str) {
        let ast_alloc = AstAlloc::new();
        let ty = parse_type(&ast_alloc, long);
        let alloc = Allocator::default();
        let doc: DocBuilder<'_, _, ()> = ty.pretty(&alloc);

        let mut long_lines = String::new();
        doc.render_fmt(usize::MAX, &mut long_lines).unwrap();

        let mut short_lines = String::new();
        doc.render_fmt(0, &mut short_lines).unwrap();

        assert_eq!(long_lines, long);
        assert_eq!(short_lines, short);
    }

    /// Parse a string representation `long` of a Nickel term, and assert that
    /// formatting it gives back `long`, if the line length is set to `80`, or
    /// alternatively results in `short`, if the line length is set to `0`
    #[track_caller]
    fn assert_long_short_term(long: &str, short: &str) {
        let ast_alloc = AstAlloc::new();
        let term = parse_term(&ast_alloc, long);
        let alloc = Allocator::default();
        let doc: DocBuilder<'_, _, ()> = term.pretty(&alloc);

        let mut long_lines = String::new();
        doc.render_fmt(160, &mut long_lines).unwrap();

        let mut short_lines = String::new();
        doc.render_fmt(0, &mut short_lines).unwrap();

        assert_eq!(long_lines, long);
        assert_eq!(short_lines, short);
    }

    #[test]
    fn pretty_array_type() {
        assert_long_short_type("Array String", "Array\n  String");
        assert_long_short_type(
            "Array (Number -> Array Dyn)",
            indoc! {"
                Array (
                  Number
                  -> Array
                    Dyn
                )"
            },
        );
    }

    #[test]
    fn pretty_arrow_type() {
        assert_long_short_type("Number -> Number", "Number\n-> Number");
        assert_long_short_type(
            "(Number -> Number -> Dyn) -> Number",
            indoc! {"
                (Number
                  -> Number
                  -> Dyn)
                -> Number"
            },
        );
    }

    #[test]
    fn pretty_dict_type() {
        assert_long_short_type(
            "{ _ : Number }",
            indoc! {"
                {
                  _ : Number
                }"
            },
        );
        assert_long_short_type(
            "{ _ : { x : Number, y : String } }",
            indoc! {"
                {
                  _ : {
                    x : Number,
                    y : String
                  }
                }"
            },
        );
    }

    #[test]
    fn pretty_record_type() {
        assert_long_short_type(
            "{ x : Number, y : String; Dyn }",
            indoc! {"
                {
                  x : Number,
                  y : String;
                  Dyn
                }"
            },
        );
    }

    #[test]
    fn pretty_enum_type() {
        assert_long_short_type(
            "forall r. [| 'tag1, 'tag2, 'tag3; r |]",
            indoc! {"
                forall
                  r.
                  [|
                    'tag1,
                    'tag2,
                    'tag3;
                    r
                  |]"
            },
        )
    }

    #[test]
    fn pretty_forall_type() {
        assert_long_short_type(
            "forall a r. a -> { foo : a; r }",
            indoc! {"
                forall
                  a
                  r.
                  a
                  -> {
                    foo : a;
                    r
                  }"
            },
        );
    }

    #[test]
    fn pretty_opn() {
        assert_long_short_term(
            "%string/replace% string pattern replace",
            indoc! {"
                %string/replace%
                  string
                  pattern
                  replace"
            },
        );
    }

    #[test]
    fn pretty_binop() {
        assert_long_short_term(
            "a + b",
            indoc! {"
                a
                + b"
            },
        );
        assert_long_short_term(
            "%string/split% string sep",
            indoc! {"
                %string/split%
                  string
                  sep"
            },
        );
        assert_long_short_term("-5", "-5");
        assert_long_short_term(
            "a - (-b)",
            indoc! {"
                a
                - (-b)"
            },
        );
    }

    #[test]
    fn pretty_unop() {
        assert_long_short_term("!xyz", "!xyz");
        assert_long_short_term(
            "a && b",
            indoc! {"
                a
                && b"
            },
        );
        assert_long_short_term(
            "(a && b) && c",
            indoc! {"
                (a
                && b)
                && c"
            },
        );
        assert_long_short_term(
            "a || b",
            indoc! {"
                a
                || b"
            },
        );
        assert_long_short_term(
            "if true then false else not",
            indoc! {"
                if true then
                  false
                else
                  not"
            },
        );
        assert_long_short_term(
            "%enum/embed% foo bar",
            indoc! {"
                %enum/embed%
                  foo
                  bar"
            },
        );
    }

    #[test]
    fn pretty_arrays() {
        assert_long_short_term(
            "[ 1, 2, 3, 4 ]",
            indoc! {"
                [
                  1,
                  2,
                  3,
                  4
                ]"
            },
        );
    }

    #[test]
    fn pretty_match() {
        assert_long_short_term(
            "match { 'A => a, 'B => b, 'C => c, }",
            indoc! {"
                match {
                  'A =>
                    a,
                  'B =>
                    b,
                  'C =>
                    c,
                }"
            },
        );
    }

    #[test]
    fn pretty_record() {
        assert_long_short_term("{}", "{}");
        assert_long_short_term(
            "{ a = b, c = d }",
            indoc! {"
                {
                  a =
                    b,
                  c =
                    d
                }"
            },
        );
        assert_long_short_term(
            r#"{ a | String | force = b, c | Number | doc "" = d }"#,
            indoc! {r#"
                {
                  a
                    | String
                    | force
                    = b,
                  c
                    | Number
                    | doc ""
                    = d
                }"#
            },
        );
        assert_long_short_term(
            "{ a = b, .. }",
            indoc! {"
                {
                  a =
                    b,
                  ..
                }"
            },
        );
        assert_long_short_term(
            r#"{ a = b, "%{a}" = c, .. }"#,
            indoc! {r#"
                {
                  a =
                    b,
                  "%{a}" =
                    c,
                  ..
                }"#
            },
        );
        assert_long_short_term(
            r#"{ "=" = a }"#,
            indoc! {r#"
                {
                  "=" =
                    a
                }"#
            },
        );
    }

    #[test]
    fn pretty_let() {
        assert_long_short_term(
            "let rec foo | String = c in {}",
            indoc! {"
                let rec foo
                  | String
                  = c
                  in
                {}"
            },
        );
        assert_long_short_term(
            "let foo = c bar in {}",
            indoc! {"
                let foo
                  = c
                    bar
                  in
                {}"
            },
        );
        assert_long_short_term(
            "let foo | String = c bar in {}",
            indoc! {"
                let foo
                  | String
                  = c
                    bar
                  in
                {}"
            },
        );
    }

    #[test]
    fn pretty_multiline_strings() {
        let ast_alloc = AstAlloc::new();
        // The string `"\n1."` contains a newline, so it will be pretty-printed using Nickel's
        // multiline string syntax. The result looks like:
        // ```
        // m%"
        //
        //   1.
        // "%
        // ```
        // The newline after `m%"` and the newline before `"%` are removed by the parser, as is the
        // indentation. Unfortunately, we can't use `indoc!` in this test because `pretty.rs`
        // insists on putting two spaces after every newline (but the last one), even if the line
        // is otherwise empty.
        // But `indoc!` would rightfully strip those empty spaces.
        let ast: Ast<'_> = ast_alloc
            .string_chunks(vec![StringChunk::Literal("\n1.".to_owned())])
            .into();
        assert_eq!(format!("{ast}"), "m%\"\n  \n  1.\n\"%");

        let ast: Ast<'_> = ast_alloc
            .string_chunks(vec![StringChunk::Literal(
                "a multiline string\n\n\n\n".to_owned(),
            )])
            .into();
        assert_eq!(
            format!("{ast}"),
            "m%\"\n  a multiline string\n  \n  \n  \n\n\"%"
        );
    }

    #[test]
    fn pretty_let_pattern() {
        assert_long_short_term(
            "let foo @ { a | Bool ? true = a', b ? false, } = c in {}",
            indoc! {"
                let foo @ {
                    a
                      | Bool
                      ? true
                      = a',
                    b
                      ? false,
                  }
                  = c
                  in
                {}"
            },
        );
        assert_long_short_term(
            "let foo @ { a = a', b = e @ { foo, .. }, } = c in {}",
            indoc! {"
                let foo @ {
                    a
                      = a',
                    b
                      = e @ {
                        foo,
                        ..
                      },
                  }
                  = c
                  in
                {}"
            },
        );
        assert_long_short_term(
            "let foo @ { a = a', b, } | String = c in {}",
            indoc! {"
                let foo @ {
                    a
                      = a',
                    b,
                  }
                  | String
                  = c
                  in
                {}"
            },
        );
    }

    #[test]
    fn pretty_fun() {
        assert_long_short_term(
            "fun x y z => x y z",
            indoc! {"
                fun
                  x
                  y
                  z
                  =>
                  x
                    y
                    z"
            },
        );
        assert_long_short_term(
            "fun x @ { foo, bar ? true, } y @ { baz, } => x y z",
            indoc! {"
                fun
                  x @ {
                    foo,
                    bar
                      ? true,
                  }
                  y @ {
                    baz,
                  }
                  =>
                  x
                    y
                    z"
            },
        );
    }

    #[test]
    fn pretty_app() {
        assert_long_short_term(
            "x y z",
            indoc! {"
                x
                  y
                  z"
            },
        );
    }

    /// Take a string representation of a type, parse it, and assert that formatting it gives the
    /// same string as the original argument.
    ///
    /// Note that there are infinitely many string representations of the same type since, for
    /// example, spaces are ignored: for the outcome of this function to be meaningful, the
    /// original type must be written in the same way as types are formatted.
    #[track_caller]
    fn assert_format_eq(s: &str) {
        let ast_alloc = AstAlloc::new();
        let ty = parse_type(&ast_alloc, s);
        assert_eq!(s, &format!("{ty}"));
    }

    #[test]
    fn types_pretty_printing() {
        assert_format_eq("Number");
        assert_format_eq("Number -> Number");
        assert_format_eq("(Number -> Number) -> (Number -> Number) -> Number -> Number");
        assert_format_eq("((Number -> Number) -> Number) -> Number");
        assert_format_eq("Number -> (forall a. a -> String) -> String");

        assert_format_eq("{ _ : String }");
        assert_format_eq("{ _ : (String -> String) -> String }");
        assert_format_eq("{ _ | String }");
        assert_format_eq("{ _ | (String -> String) -> String }");

        assert_format_eq("{ x : (Bool -> Bool) -> Bool, y : Bool }");
        assert_format_eq("forall r. { x : Bool, y : Bool, z : Bool; r }");
        assert_format_eq("{ x : Bool, y : Bool, z : Bool }");

        assert_format_eq("[| 'a, 'b, 'c, 'd |]");
        assert_format_eq("forall r. [| 'tag1, 'tag2, 'tag3; r |]");

        assert_format_eq("Array Number");
        assert_format_eq("Array (Array Number)");
        assert_format_eq("Number -> Array (Array String) -> Number");
        assert_format_eq("Array (Number -> Number)");
        assert_format_eq("Array (Array (Array Dyn) -> Number)");

        assert_format_eq("_");
        assert_format_eq("_ -> _");
        assert_format_eq("{ x : _, y : Bool }");
        assert_format_eq("{ _ : _ }");
    }

    fn format_short_term(input: &str, depth: usize, size: usize) -> String {
        let ast_alloc = AstAlloc::new();
        let term = parse_term(&ast_alloc, input);
        let allocator = Allocator::bounded(depth, size);
        let doc: DocBuilder<_, ()> = term.pretty(&allocator);
        Doc::pretty(&doc, 1000).to_string()
    }

    #[test]
    fn bounded_pretty_printing() {
        assert_eq!("{ hello = 1 }", &format_short_term("{hello = 1}", 1, 1));
        assert_eq!("{…}", &format_short_term("{hello = 1, bye = 2}", 1, 1));
        assert_eq!(
            "{ hello = 1, inner = { bye = 2 } }",
            &format_short_term("{hello = 1, inner = { bye = 2 }}", 2, 2)
        );
        assert_eq!(
            "{ hello = 1, inner = {…} }",
            &format_short_term("{hello = 1, inner = { bye = 2 }}", 1, 100)
        );
        assert_eq!(
            "{ hello = 1, inner = {…} }",
            &format_short_term("{hello = 1, inner = { bye = 2, other = 3 }}", 100, 2)
        );
    }
}
