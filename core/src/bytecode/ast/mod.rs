//! The Nickel AST, as ingested by the bytecode compiler.
//!
//! Since the AST is built once for each Nickel expression and is then compiled away to bytecode,
//! the number nodes ever allocated should be reasonably bounded by the input program size. Thus,
//! for performance reasons, we allocate notes using an arena and keep them alive until the end of
//! compilation. In return, we get fast allocation and de-allocation, and we can easily reference
//! other nodes and data structures using native references.
//!
//! The corresponding lifetime of all the nodes - and thus of the arena as well - is consistently
//! called `'ast`.

use std::ffi::OsString;

use pattern::Pattern;
use record::Record;

use crate::{
    cache::InputFormat,
    error::ParseError,
    identifier::LocIdent,
    position::TermPos,
    term::{self, Number, StrChunk},
    typ::Type,
};

use bumpalo::Bump;

pub mod pattern;
pub mod record;

use pattern::*;

#[derive(Clone, Copy, Debug, PartialEq)]
/// A Nickel AST. Contains a root node and a span. Both are references so that `Ast` is cheap to
/// clone - it's in fact `Copy` - and to store.
pub struct Ast<'ast> {
    node: &'ast Node<'ast>,
    pos: &'ast TermPos,
}

/// A node of the Nickel AST.
///
/// Nodes are built by the parser and then mostly traversed immutably. Thus, they are immutable and
/// optimized for sharing and fast cloning. In particular, the data aren't owned and are mostly
/// references that are in practice pointing to one global arena.
///
/// Using an arena has another advantage: the data is allocated in the same order as the AST is
/// built. This means that even if there are reference indirections, the children of a node are
/// most likely close to the node itself in memory, which should be good for cache locality.
#[derive(Clone, Debug, PartialEq, Default)]
pub enum Node<'ast> {
    /// The null value.
    #[default]
    Null,

    /// A boolean value.
    Bool(bool),

    /// A number.
    ///
    /// A number is an arbitrary-precision rational in Nickel. It's not small and thus we put it
    /// behind a reference to avoid size bloat.
    Number(&'ast Number),

    /// A string literal.
    String(&'ast str),

    /// A string containing interpolated expressions, represented as a list of either literals or
    /// expressions.
    ///
    /// As opposed to [crate::term::Term::StrChunks], the chunks are stored in the original order:
    /// `"hello%{var}"` will give `["hello", var]`.
    StrChunks(&'ast [StrChunk<Ast<'ast>>]),

    /// A function.
    Fun(&'ast Pattern<'ast>, Ast<'ast>),

    /// A let-binding.
    Let {
        bindings: &'ast [(Pattern<'ast>, Ast<'ast>)],
        body: Ast<'ast>,
        rec: bool,
    },

    /// An application.
    App { fun: Ast<'ast>, arg: Ast<'ast> },

    /// A variable.
    Var(LocIdent),

    /// An enum variant (an algebraic datatype). Variants have at most once argument: variants with
    /// no arguments are often called simply enum tags. Note that one can just use a record as an
    /// argument to emulate variants with multiple arguments.
    EnumVariant {
        tag: LocIdent,
        arg: Option<Ast<'ast>>,
    },

    /// A record.
    Record(&'ast Record<'ast>),

    /// A match expression. This expression is still to be applied to an argument to match on.
    Match(Match<'ast>),

    /// An array.
    Array(&'ast [Ast<'ast>]),

    /// A primitive operator application.
    PrimOp { op: PrimOp, args: &'ast [Ast<'ast>] },

    /// A term with a type and/or contract annotation.
    Annotated {
        annot: Annotation<'ast>,
        inner: Ast<'ast>,
    },

    /// An import.
    Import { path: OsString, format: InputFormat },

    /// A type in term position, such as in `let my_contract = Number -> Number in ...`.
    ///
    /// During evaluation, this will get turned into a contract.
    Type(&'ast Type),

    /// A term that couldn't be parsed properly. Used by the LSP to handle partially valid
    /// programs.
    ParseError(&'ast ParseError),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PrimOp {}

/// A branch of a match expression.
#[derive(Debug, PartialEq, Clone)]
pub struct MatchBranch<'ast> {
    /// The pattern on the left hand side of `=>`.
    pub pattern: Pattern<'ast>,
    /// A potential guard, which is an additional side-condition defined as `if cond`. The value
    /// stored in this field is the boolean condition itself.
    pub guard: Option<Ast<'ast>>,
    /// The body of the branch, on the right hand side of `=>`.
    pub body: Ast<'ast>,
}

/// Content of a match expression.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Match<'ast> {
    /// Branches of the match expression, where the first component is the pattern on the left hand
    /// side of `=>` and the second component is the body of the branch.
    pub branches: &'ast [MatchBranch<'ast>],
}

/// A type and/or contract annotation.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Annotation<'ast> {
    /// The type annotation (using `:`).
    pub typ: Option<&'ast Type>,

    /// The contracts annotation (using `|`).
    pub contracts: &'ast [Type],
}

impl<'ast> Annotation<'ast> {
    /// Return the main annotation, which is either the type annotation if any, or the first
    /// contract annotation.
    pub fn first(&self) -> Option<&'ast Type> {
        self.typ.or(self.contracts.iter().next())
    }

    /// Iterate over the annotations, starting by the type and followed by the contracts.
    pub fn iter(&self) -> impl Iterator<Item = &'ast Type> + '_ {
        self.typ
            .iter()
            .map(|type_ref| *type_ref)
            .chain(self.contracts.iter())
    }

    /// Return a string representation of the contracts (without the static type annotation) as a
    /// comma-separated list.
    pub fn contracts_to_string(&self) -> Option<String> {
        (!self.contracts.is_empty()).then(|| {
            self.contracts
                .iter()
                .map(|typ| format!("{typ}"))
                .collect::<Vec<_>>()
                .join(",")
        })
    }

    /// Return `true` if this annotation is empty, i.e. hold neither a type annotation nor
    /// contracts annotations.
    pub fn is_empty(&self) -> bool {
        self.typ.is_none() && self.contracts.is_empty()
    }
}

/// Own the arenas required to allocate new AST nodes and provide builder methods to create them.
///
/// # Drop and arena allocation
///
/// The most popular choice for arena is the `bumpalo` crate, which is a fast bump allocator that
/// can handle heterogeneous data. However, it doesn't support destructors, which is a problem
/// because some of the nodes in the AST owns heap allocated data and needs to be de-allocated
/// (`Number`, `Type` and errors for now).
///
/// Another choice is `typed-arena` and derivatives, which does run destructors, but can only store
/// one type of values. As the number of types that need to be dropped is relatively small, we use
/// a general `bumpalo` arena by default, and specialized typed arenas for stuff that need to be
/// dropped.
pub struct AstBuilder {
    generic_arena: Bump,
    type_arena: typed_arena::Arena<Type>,
    number_arena: typed_arena::Arena<Number>,
    error_arena: typed_arena::Arena<ParseError>,
}

impl AstBuilder {
    /// Create a new `AstBuilder`.
    pub fn new() -> Self {
        Self {
            generic_arena: Bump::new(),
            type_arena: typed_arena::Arena::new(),
            number_arena: typed_arena::Arena::new(),
            error_arena: typed_arena::Arena::new(),
        }
    }

    pub fn null(&self) -> &Node<'_> {
        self.generic_arena.alloc(Node::Null)
    }

    pub fn bool(&self, value: bool) -> &Node<'_> {
        self.generic_arena.alloc(Node::Bool(value))
    }

    pub fn number(&self, number: Number) -> &Node<'_> {
        let number = self.number_arena.alloc(number);
        self.generic_arena.alloc(Node::Number(number))
    }

    pub fn string<'ast, 'b>(&'ast self, s: &'b str) -> &'ast Node<'ast> {
        let s = self.generic_arena.alloc_str(s);
        self.generic_arena.alloc(Node::String(s))
    }

    pub fn str_chunks_iter<'ast, I>(&'ast self, chunks: I) -> &'ast Node<'ast>
    where
        I: IntoIterator<Item = StrChunk<Ast<'ast>>>,
        I::IntoIter: ExactSizeIterator,
    {
        let chunks = self.generic_arena.alloc_slice_fill_iter(chunks);
        self.generic_arena.alloc(Node::StrChunks(chunks))
    }

    pub fn fun<'ast>(&'ast self, pat: &'ast Pattern<'ast>, body: Ast<'ast>) -> &'ast Node<'ast> {
        self.generic_arena.alloc(Node::Fun(pat, body))
    }

    pub fn let_binding<'ast, I>(
        &'ast self,
        bindings: I,
        body: Ast<'ast>,
        rec: bool,
    ) -> &'ast Node<'ast>
    where
        I: IntoIterator<Item = (Pattern<'ast>, Ast<'ast>)>,
        I::IntoIter: ExactSizeIterator,
    {
        let bindings = self.generic_arena.alloc_slice_fill_iter(bindings);
        self.generic_arena.alloc(Node::Let {
            bindings,
            body,
            rec,
        })
    }

    pub fn app<'ast>(&'ast self, fun: Ast<'ast>, arg: Ast<'ast>) -> &'ast Node<'ast> {
        self.generic_arena.alloc(Node::App { fun, arg })
    }

    pub fn var<'ast>(&'ast self, ident: LocIdent) -> &'ast Node<'ast> {
        self.generic_arena.alloc(Node::Var(ident))
    }

    pub fn enum_variant<'ast>(
        &'ast self,
        tag: LocIdent,
        arg: Option<Ast<'ast>>,
    ) -> &'ast Node<'ast> {
        self.generic_arena.alloc(Node::EnumVariant { tag, arg })
    }

    pub fn record<'ast>(&'ast self, record: Record<'ast>) -> &'ast Node<'ast> {
        let record = self.generic_arena.alloc(record);
        self.generic_arena.alloc(Node::Record(record))
    }

    pub fn record_data<'ast, Ss, Ds>(
        &'ast self,
        stat_fields: Ss,
        dyn_fields: Ds,
        open: bool,
    ) -> &'ast Record<'ast>
    where
        Ss: IntoIterator<Item = (LocIdent, Ast<'ast>)>,
        Ds: IntoIterator<Item = (Ast<'ast>, Ast<'ast>)>,
        Ss::IntoIter: ExactSizeIterator,
        Ds::IntoIter: ExactSizeIterator,
    {
        let stat_fields = self.generic_arena.alloc_slice_fill_iter(stat_fields);
        let dyn_fields = self.generic_arena.alloc_slice_fill_iter(dyn_fields);
        self.generic_arena.alloc(Record {
            stat_fields,
            dyn_fields,
            open,
        })
    }

    pub fn match_expr<'ast, I>(&'ast self, branches: I) -> &'ast Node<'ast>
    where
        I: IntoIterator<Item = MatchBranch<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let branches = self.generic_arena.alloc_slice_fill_iter(branches);
        self.generic_arena.alloc(Node::Match(Match { branches }))
    }

    pub fn array<'ast, I>(&'ast self, elts: I) -> &'ast Node<'ast>
    where
        I: IntoIterator<Item = Ast<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let elts = self.generic_arena.alloc_slice_fill_iter(elts);
        self.generic_arena.alloc(Node::Array(elts))
    }

    pub fn prim_op<'ast, I>(&'ast self, op: PrimOp, args: I) -> &'ast Node<'ast>
    where
        I: IntoIterator<Item = Ast<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let args = self.generic_arena.alloc_slice_fill_iter(args);
        self.generic_arena.alloc(Node::PrimOp { op, args })
    }

    pub fn annotated<'ast>(
        &'ast self,
        annot: Annotation<'ast>,
        inner: Ast<'ast>,
    ) -> &'ast Node<'ast> {
        self.generic_arena.alloc(Node::Annotated { annot, inner })
    }

    pub fn import(&self, path: OsString, format: InputFormat) -> &Node<'_> {
        self.generic_arena.alloc(Node::Import { path, format })
    }

    pub fn typ<'ast>(&'ast self, typ: Type) -> &'ast Node<'ast> {
        let typ = self.type_arena.alloc(typ);
        self.generic_arena.alloc(Node::Type(typ))
    }

    pub fn parse_error<'ast>(&'ast self, error: ParseError) -> &'ast Node<'ast> {
        let error = self.error_arena.alloc(error);
        self.generic_arena.alloc(Node::ParseError(error))
    }

    pub fn ast<'ast>(&'ast self, node: &'ast Node<'ast>, pos: TermPos) -> Ast<'ast> {
        let pos = self.generic_arena.alloc(pos);
        Ast { node, pos }
    }

    pub fn pattern<'ast>(
        &'ast self,
        data: PatternData<'ast>,
        alias: Option<LocIdent>,
        pos: TermPos,
    ) -> &'ast Pattern<'ast> {
        self.generic_arena.alloc(Pattern { data, alias, pos })
    }

    pub fn enum_pattern<'ast>(
        &'ast self,
        tag: LocIdent,
        pattern: Option<Pattern<'ast>>,
        pos: TermPos,
    ) -> &'ast EnumPattern<'ast> {
        self.generic_arena.alloc(EnumPattern { tag, pattern, pos })
    }

    pub fn field_pattern<'ast>(
        &'ast self,
        matched_id: LocIdent,
        annotation: Annotation<'ast>,
        default: Option<Ast<'ast>>,
        pattern: Pattern<'ast>,
        pos: TermPos,
    ) -> &'ast FieldPattern<'ast> {
        self.generic_arena.alloc(FieldPattern {
            matched_id,
            annotation,
            default,
            pattern,
            pos,
        })
    }

    pub fn record_pattern<'ast, I>(
        &'ast self,
        patterns: I,
        tail: TailPattern,
        pos: TermPos,
    ) -> &'ast RecordPattern<'ast>
    where
        I: IntoIterator<Item = FieldPattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let patterns = self.generic_arena.alloc_slice_fill_iter(patterns);

        self.generic_arena.alloc(RecordPattern {
            patterns,
            tail,
            pos,
        })
    }

    pub fn array_pattern<'ast, I>(
        &'ast self,
        patterns: I,
        tail: TailPattern,
        pos: TermPos,
    ) -> &'ast ArrayPattern<'ast>
    where
        I: IntoIterator<Item = Pattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let patterns = self.generic_arena.alloc_slice_fill_iter(patterns);

        self.generic_arena.alloc(ArrayPattern {
            patterns,
            tail,
            pos,
        })
    }

    pub fn constant_pattern<'ast>(
        &'ast self,
        data: ConstantPatternData<'ast>,
        pos: TermPos,
    ) -> &'ast ConstantPattern<'ast> {
        self.generic_arena.alloc(ConstantPattern { data, pos })
    }

    pub fn or_pattern<'ast, I>(&'ast self, patterns: I, pos: TermPos) -> &'ast OrPattern<'ast>
    where
        I: IntoIterator<Item = Pattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let patterns = self.generic_arena.alloc_slice_fill_iter(patterns);

        self.generic_arena.alloc(OrPattern { patterns, pos })
    }

    pub fn from_pattern<'ast>(&'ast self, pattern: &term::pattern::Pattern) -> &'ast Pattern<'ast> {
        todo!()
    }

    pub fn from_pattern_owned<'ast>(&'ast self, pattern: &term::pattern::Pattern) -> Pattern<'ast> {
        todo!()
    }

    pub fn from_term<'ast>(&'ast self, term: &term::Term) -> &'ast Node<'ast> {
        use term::Term;

        match term {
            Term::Null => self.null(),
            Term::Bool(b) => self.bool(*b),
            Term::Num(n) => self.number(n.clone()),
            Term::Str(s) => self.string(s),
            Term::StrChunks(chunks) => {
                self.str_chunks_iter(chunks.iter().map(|chunk| match chunk {
                    term::StrChunk::Literal(s) => StrChunk::Literal(s.clone()),
                    term::StrChunk::Expr(e, indent) => {
                        StrChunk::Expr(self.from_rich_term(e), *indent)
                    }
                }))
            }
            Term::Fun(id, body) => self.fun(
                self.generic_arena.alloc(Pattern::any(*id)),
                self.from_rich_term(body),
            ),
            Term::FunPattern(pat, body) => {
                self.fun(self.from_pattern(pat), self.from_rich_term(body))
            }
            Term::Let(bindings, body, attrs) => self.let_binding(
                bindings
                    .iter()
                    .map(|(id, term)| (Pattern::any(*id), self.from_rich_term(term))),
                self.from_rich_term(body),
                attrs.rec,
            ),
            Term::LetPattern(bindings, body, attrs) => self.let_binding(
                bindings
                    .iter()
                    .map(|(pat, term)| (self.from_pattern_owned(pat), self.from_rich_term(term))),
                self.from_rich_term(body),
                attrs.rec,
            ),
            Term::App(fun, arg) => self.app(self.from_rich_term(fun), self.from_rich_term(arg)),
            Term::Var(id) => self.var(*id),
            Term::Enum(id) => self.enum_variant(*id, None),
            Term::EnumVariant { tag, arg, attrs: _ } => {
                self.enum_variant(*tag, Some(self.from_rich_term(arg)))
            }
            Term::Record(data) => {
                todo!()
            }
            Term::Match(data) => todo!(),
            Term::Array(data, _) => {
                // We should probably make array's iterator an ExactSizeIterator. But for now, we
                // don't care about the translation's performance so it's simpler to just collect
                // them in a vec locally.
                let elts = data
                    .iter()
                    .map(|term| self.from_rich_term(term))
                    .collect::<Vec<_>>();
                self.array(elts)
            }
            _ => unimplemented!(),
        }
    }

    pub fn from_rich_term<'ast>(&'ast self, rterm: &term::RichTerm) -> Ast<'ast> {
        self.ast(self.from_term(rterm.as_ref()), rterm.pos)
    }
}
