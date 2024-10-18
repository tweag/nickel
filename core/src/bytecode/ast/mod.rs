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

impl From<&term::UnaryOp> for PrimOp {
    fn from(op: &term::UnaryOp) -> Self {
        todo!()
    }
}

impl From<&term::BinaryOp> for PrimOp {
    fn from(op: &term::BinaryOp) -> Self {
        todo!()
    }
}

impl From<&term::NAryOp> for PrimOp {
    fn from(op: &term::NAryOp) -> Self {
        todo!()
    }
}

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
    ///
    /// We can't allocate a `Type` directly here, as they live in a different arena, so we need to
    /// allocate each type individually and then a slice of references to those types.
    pub contracts: &'ast [&'ast Type],
}

impl<'ast> Annotation<'ast> {
    /// Return the main annotation, which is either the type annotation if any, or the first
    /// contract annotation.
    pub fn first(&self) -> Option<&'ast Type> {
        self.typ.or(self.contracts.iter().map(|t| *t).next())
    }

    /// Iterate over the annotations, starting by the type and followed by the contracts.
    pub fn iter(&self) -> impl Iterator<Item = &'ast Type> + '_ {
        self.typ
            .iter()
            .map(|type_ref| *type_ref)
            .chain(self.contracts.iter().map(|t| *t))
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
        self.generic_arena.alloc(self.from_pattern_owned(pattern))
    }

    pub fn from_pattern_owned<'ast>(&'ast self, pattern: &term::pattern::Pattern) -> Pattern<'ast> {
        Pattern {
            data: self.from_pattern_data(&pattern.data),
            alias: pattern.alias,
            pos: pattern.pos,
        }
    }

    pub fn from_pattern_data<'ast>(
        &'ast self,
        data: &term::pattern::PatternData,
    ) -> PatternData<'ast> {
        match data {
            term::pattern::PatternData::Wildcard => PatternData::Wildcard,
            term::pattern::PatternData::Any(id) => PatternData::Any(*id),
            term::pattern::PatternData::Record(record_pattern) => {
                self.from_record_pattern(record_pattern)
            }
            term::pattern::PatternData::Array(array_pattern) => {
                self.from_array_pattern(array_pattern)
            }
            term::pattern::PatternData::Enum(enum_pattern) => self.from_enum_pattern(enum_pattern),
            term::pattern::PatternData::Constant(constant_pattern) => {
                self.from_constant_pattern(constant_pattern)
            }
            term::pattern::PatternData::Or(or_pattern) => self.from_or_pattern(or_pattern),
        }
    }

    pub fn from_record_pattern<'ast>(
        &'ast self,
        pattern: &term::pattern::RecordPattern,
    ) -> PatternData<'ast> {
        let patterns = pattern
            .patterns
            .iter()
            .map(|field_pattern| self.from_field_pattern(field_pattern));

        let tail = match pattern.tail {
            term::pattern::TailPattern::Empty => TailPattern::Empty,
            term::pattern::TailPattern::Open => TailPattern::Open,
            term::pattern::TailPattern::Capture(id) => TailPattern::Capture(id),
        };

        PatternData::Record(self.record_pattern(patterns, tail, pattern.pos))
    }

    pub fn from_field_pattern<'ast>(
        &'ast self,
        field_pat: &term::pattern::FieldPattern,
    ) -> FieldPattern<'ast> {
        let pattern = self.from_pattern_owned(&field_pat.pattern);

        let default = field_pat
            .default
            .as_ref()
            .map(|term| self.from_rich_term(term));
        let annotation = self.from_annotation(&field_pat.annotation);

        FieldPattern {
            matched_id: field_pat.matched_id,
            annotation,
            default,
            pattern,
            pos: field_pat.pos,
        }
    }

    pub fn from_array_pattern<'ast>(
        &'ast self,
        array_pat: &term::pattern::ArrayPattern,
    ) -> PatternData<'ast> {
        let patterns = array_pat
            .patterns
            .iter()
            .map(|pat| self.from_pattern_owned(pat));

        let tail = match array_pat.tail {
            term::pattern::TailPattern::Empty => TailPattern::Empty,
            term::pattern::TailPattern::Open => TailPattern::Open,
            term::pattern::TailPattern::Capture(id) => TailPattern::Capture(id),
        };

        PatternData::Array(self.array_pattern(patterns, tail, array_pat.pos))
    }

    pub fn from_enum_pattern<'ast>(
        &'ast self,
        enum_pat: &term::pattern::EnumPattern,
    ) -> PatternData<'ast> {
        let pattern = enum_pat.pattern.as_ref().map(|pat| self.from_pattern_owned(pat));
        PatternData::Enum(self.enum_pattern(enum_pat.tag, pattern, enum_pat.pos))
    }

    pub fn from_constant_pattern<'ast>(
        &'ast self,
        pattern: &'ast term::pattern::ConstantPattern,
    ) -> PatternData<'ast> {
        let data = match &pattern.data {
            term::pattern::ConstantPatternData::Bool(b) => ConstantPatternData::Bool(*b),
            term::pattern::ConstantPatternData::Number(n) => {
                ConstantPatternData::Number(self.generic_arena.alloc(n.clone()))
            }
            term::pattern::ConstantPatternData::String(s) => ConstantPatternData::String(s),
            term::pattern::ConstantPatternData::Null => ConstantPatternData::Null,
        };
        PatternData::Constant(self.constant_pattern(data, pattern.pos))
    }

    pub fn from_or_pattern<'ast>(
        &'ast self,
        pattern: &term::pattern::OrPattern,
    ) -> PatternData<'ast> {
        let patterns = pattern
            .patterns
            .iter()
            .map(|pat| self.from_pattern(pat))
            .collect::<Vec<_>>();
        PatternData::Or(self.or_pattern(patterns, pattern.pos))
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
            Term::Op1(op, arg) => {
                self.prim_op(PrimOp::from(op), std::iter::once(self.from_rich_term(arg)))
            }
            Term::Op2(op, arg1, arg2) => self.prim_op(
                PrimOp::from(op),
                [arg1, arg2].iter().map(|arg| self.from_rich_term(arg)),
            ),
            Term::OpN(op, args) => self.prim_op(
                PrimOp::from(op),
                args.iter().map(|arg| self.from_rich_term(arg)),
            ),
            Term::SealingKey(_) => panic!("didn't expect a sealing key at the first stage"),
            Term::Sealed(..) => panic!("didn't expect a sealed term at the first stage"),
            Term::Annotated(annot, term) => {
                self.annotated(self.from_annotation(annot), self.from_rich_term(term))
            }
            Term::Import { path, format } => self.import(path.clone(), *format),
            Term::ResolvedImport(_) => panic!("didn't expect a resolved import at parsing stage"),
            Term::Type { typ, .. } => self.typ(typ.clone()),
            Term::CustomContract(_) => panic!("didn't expect a custom contract at parsing stage"),
            Term::ParseError(error) => self.parse_error(error.clone()),
            Term::RuntimeError(_) => panic!("didn't expect a runtime error at parsing stage"),
            Term::Closure(_) => panic!("didn't expect a closure at parsing stage"),
            Term::ForeignId(_) => panic!("didn't expect a foreign id at parsing stage"),
            _ => unimplemented!(),
        }
    }

    pub fn from_rich_term<'ast>(&'ast self, rterm: &term::RichTerm) -> Ast<'ast> {
        self.ast(self.from_term(rterm.as_ref()), rterm.pos)
    }

    pub fn from_annotation<'ast>(&'ast self, annot: &term::TypeAnnotation) -> Annotation<'ast> {
        let typ = annot
            .typ
            .as_ref()
            .map(|typ| &*self.type_arena.alloc(typ.typ.clone()));
        let contracts = self.generic_arena.alloc_slice_fill_iter(
            annot
                .contracts
                .iter()
                .map(|contract| &*self.type_arena.alloc(contract.typ.clone())),
        );

        Annotation { typ, contracts }
    }
}
