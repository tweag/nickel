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

use std::{ffi::OsString, rc};

use pattern::Pattern;
use record::Record;

use crate::{cache::InputFormat, error::ParseError, identifier::LocIdent, position::TermPos};

// For now, we reuse those types from the term module.
pub use crate::term::{Number, StrChunk};

use bumpalo::Bump;

pub mod compat;
pub mod pattern;
pub mod primop;
pub mod record;
pub mod typ;

use pattern::*;
use primop::PrimOp;
use typ::*;

/// A Nickel AST. Contains a root node and a span.
///
//TODO: we don't expect to access the span much on the happy path. Should we add an indirection
//through a reference?
#[derive(Clone, Debug, PartialEq)]
pub struct Ast<'ast> {
    node: Node<'ast>,
    pos: TermPos,
}

/// A node of the Nickel AST.
///
/// Nodes are built by the parser and then mostly traversed immutably. Such nodes are optimized for
/// sharing (hence immutability) and for size, as the size of an enum can grow quite quickly in
/// Rust. In particular, any data that is bigger than a few words isn't usually owned but rather a
/// reference to some arena-allocated data
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
    Fun {
        arg: &'ast Pattern<'ast>,
        body: &'ast Ast<'ast>,
    },

    /// A let-binding.
    Let {
        bindings: &'ast [(Pattern<'ast>, Ast<'ast>)],
        body: &'ast Ast<'ast>,
        rec: bool,
    },

    /// An application to one or more arguments.
    App {
        fun: &'ast Ast<'ast>,
        args: &'ast [Ast<'ast>],
    },

    /// A variable.
    Var(LocIdent),

    /// An enum variant (an algebraic datatype).
    ///
    /// Variants have at most one argument: variants with no arguments are often called simply
    /// enum tags. Note that one can just use a record as an argument to emulate variants with
    /// multiple arguments.
    EnumVariant {
        tag: LocIdent,
        arg: Option<&'ast Ast<'ast>>,
    },

    /// A record.
    Record(&'ast Record<'ast>),

    /// An if-then-else expression.
    IfThenElse {
        cond: &'ast Ast<'ast>,
        then_branch: &'ast Ast<'ast>,
        else_branch: &'ast Ast<'ast>,
    },

    /// A match expression. This expression is still to be applied to an argument to match on.
    Match(Match<'ast>),

    /// An array.
    Array(&'ast [Ast<'ast>]),

    /// An n-ary primitive operation application. As opposed to a traditional function application:
    ///
    /// 1. The function part is necessarily a primitive operation.
    /// 2. The arguments are forced before entering the
    PrimOpApp {
        op: &'ast PrimOp,
        args: &'ast [Ast<'ast>],
    },

    /// A term with a type and/or contract annotation.
    Annotated {
        annot: &'ast Annotation<'ast>,
        inner: &'ast Ast<'ast>,
    },

    /// An import.
    Import {
        path: &'ast OsString,
        format: InputFormat,
    },

    /// A type in term position, such as in `let my_contract = Number -> Number in ...`.
    ///
    /// During evaluation, this will get turned into a contract.
    Type(&'ast Type<'ast>),

    /// A term that couldn't be parsed properly. Used by the LSP to handle partially valid
    /// programs.
    ParseError(&'ast ParseError),
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
    pub typ: Option<Type<'ast>>,

    /// The contract annotations (using `|`).
    pub contracts: &'ast [Type<'ast>],
}

impl<'ast> Annotation<'ast> {
    /// Returns the main annotation, which is either the type annotation if any, or the first
    /// contract annotation.
    pub fn first(&'ast self) -> Option<&'ast Type> {
        self.typ.as_ref().or(self.contracts.iter().next())
    }

    /// Iterates over the annotations, starting by the type and followed by the contracts.
    pub fn iter(&'ast self) -> impl Iterator<Item = &'ast Type> {
        self.typ.iter().chain(self.contracts.iter())
    }

    /// Returns a string representation of the contracts (without the static type annotation) as a
    /// comma-separated list.
    pub fn contracts_to_string(&self) -> Option<String> {
        todo!("requires pretty printing first")
        //(!self.contracts.is_empty()).then(|| {
        //    self.contracts
        //        .iter()
        //        .map(|typ| format!("{typ}"))
        //        .collect::<Vec<_>>()
        //        .join(",")
        //})
    }

    /// Returns `true` if this annotation is empty, i.e. hold neither a type annotation nor
    /// contracts annotations.
    pub fn is_empty(&self) -> bool {
        self.typ.is_none() && self.contracts.is_empty()
    }
}

/// Owns the arenas required to allocate new AST nodes and provide builder methods to create them.
///
/// # Drop and arena allocation
///
/// The most popular choice for arena is the `bumpalo` crate, which is a fast bump allocator that
/// can handle heterogeneous data. However, it doesn't support destructors, which is a problem
/// because some of the nodes in the AST owns heap allocated data and needs to be de-allocated
/// (numbers and parse errors currently).
///
/// Another choice is `typed-arena` and derivatives, which do run destructors, but can only store
/// one type of values. As the number of types that need to be dropped is relatively small, we use
/// a general `bumpalo` arena by default, and specialized typed arenas for stuff that need to be
/// dropped.
pub struct AstAlloc {
    generic_arena: Bump,
    number_arena: typed_arena::Arena<Number>,
    error_arena: typed_arena::Arena<ParseError>,
}

impl AstAlloc {
    /// Create a new ast allocator.
    pub fn new() -> Self {
        Self {
            generic_arena: Bump::new(),
            number_arena: typed_arena::Arena::new(),
            error_arena: typed_arena::Arena::new(),
        }
    }

    pub fn node<'ast>(&'ast self, node: Node<'ast>) -> &'ast Node<'ast> {
        self.generic_arena.alloc(node)
    }

    pub fn number<'ast>(&'ast self, number: Number) -> Node<'ast> {
        Node::Number(self.number_arena.alloc(number))
    }

    pub fn string<'ast>(&'ast self, s: &str) -> Node<'ast> {
        Node::String(self.generic_arena.alloc_str(s))
    }

    pub fn str_chunks<'ast, I>(&'ast self, chunks: I) -> Node<'ast>
    where
        I: IntoIterator<Item = StrChunk<Ast<'ast>>>,
        I::IntoIter: ExactSizeIterator,
    {
        Node::StrChunks(self.generic_arena.alloc_slice_fill_iter(chunks))
    }

    pub fn fun<'ast>(&'ast self, pat: Pattern<'ast>, body: Ast<'ast>) -> Node<'ast> {
        let arg = self.generic_arena.alloc(pat);
        let body = self.generic_arena.alloc(body);
        Node::Fun { arg, body }
    }

    pub fn let_binding<'ast, I>(&'ast self, bindings: I, body: Ast<'ast>, rec: bool) -> Node<'ast>
    where
        I: IntoIterator<Item = (Pattern<'ast>, Ast<'ast>)>,
        I::IntoIter: ExactSizeIterator,
    {
        let bindings = self.generic_arena.alloc_slice_fill_iter(bindings);
        let body = self.generic_arena.alloc(body);

        Node::Let {
            bindings,
            body,
            rec,
        }
    }

    pub fn app<'ast, I>(&'ast self, fun: Ast<'ast>, args: I) -> Node<'ast>
    where
        I: IntoIterator<Item = Ast<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        Node::App {
            fun: self.generic_arena.alloc(fun),
            args: self.generic_arena.alloc_slice_fill_iter(args),
        }
    }

    pub fn enum_variant<'ast>(&'ast self, tag: LocIdent, arg: Option<Ast<'ast>>) -> Node<'ast> {
        Node::EnumVariant {
            tag,
            arg: arg.map(|arg| &*self.generic_arena.alloc(arg)),
        }
    }

    pub fn record<'ast>(&'ast self, record: Record<'ast>) -> Node<'ast> {
        let record = self.generic_arena.alloc(record);
        Node::Record(record)
    }

    pub fn record_data<'ast, Ss, Ds>(
        &'ast self,
        stat_fields: Ss,
        dyn_fields: Ds,
        open: bool,
    ) -> &'ast Record<'ast>
    where
        Ss: IntoIterator<Item = (LocIdent, record::Field<'ast>)>,
        Ds: IntoIterator<Item = (Ast<'ast>, record::Field<'ast>)>,
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

    pub fn if_then_else<'ast>(
        &'ast self,
        cond: Ast<'ast>,
        then_branch: Ast<'ast>,
        else_branch: Ast<'ast>,
    ) -> Node<'ast> {
        Node::IfThenElse {
            cond: self.generic_arena.alloc(cond),
            then_branch: self.generic_arena.alloc(then_branch),
            else_branch: self.generic_arena.alloc(else_branch),
        }
    }

    pub fn match_expr<'ast, I>(&'ast self, branches: I) -> Node<'ast>
    where
        I: IntoIterator<Item = MatchBranch<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        Node::Match(Match {
            branches: self.generic_arena.alloc_slice_fill_iter(branches),
        })
    }

    pub fn array<'ast, I>(&'ast self, elts: I) -> Node<'ast>
    where
        I: IntoIterator<Item = Ast<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        Node::Array(self.generic_arena.alloc_slice_fill_iter(elts))
    }

    pub fn prim_op<'ast, I>(&'ast self, op: PrimOp, args: I) -> Node<'ast>
    where
        I: IntoIterator<Item = Ast<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let op = self.generic_arena.alloc(op);
        let args = self.generic_arena.alloc_slice_fill_iter(args);
        Node::PrimOpApp { op, args }
    }

    pub fn annotated<'ast>(&'ast self, annot: Annotation<'ast>, inner: Ast<'ast>) -> Node<'ast> {
        Node::Annotated {
            annot: self.generic_arena.alloc(annot),
            inner: self.generic_arena.alloc(inner),
        }
    }

    pub fn import(&self, path: OsString, format: InputFormat) -> Node<'_> {
        Node::Import {
            path: self.generic_arena.alloc(path),
            format,
        }
    }

    /// As opposed to [Self::typ], this method takes an already constructed type and move it into
    /// the arena, instead of taking each constituent separately.
    pub fn typ<'ast>(&'ast self, typ: Type<'ast>) -> Node<'ast> {
        Node::Type(self.generic_arena.alloc(typ))
    }

    pub fn typ_from_unr<'ast>(&'ast self, typ: TypeUnr<'ast>, pos: TermPos) -> Node<'ast> {
        Node::Type(self.generic_arena.alloc(Type { typ, pos }))
    }

    pub fn types<'ast, I>(&'ast self, types: I) -> &'ast [Type<'ast>]
    where
        I: IntoIterator<Item = Type<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        self.generic_arena.alloc_slice_fill_iter(types)
    }

    pub fn enum_rows<'ast>(&'ast self, erows: EnumRowsUnr<'ast>) -> &'ast EnumRows<'ast> {
        self.generic_arena.alloc(EnumRows(erows))
    }

    pub fn record_rows<'ast>(&'ast self, rrows: RecordRowsUnr<'ast>) -> &'ast RecordRows<'ast> {
        self.generic_arena.alloc(RecordRows(rrows))
    }

    pub fn parse_error(&self, error: ParseError) -> Node<'_> {
        Node::ParseError(self.error_arena.alloc(error))
    }

    pub fn ast<'ast>(&'ast self, ast: Ast<'ast>) -> &'ast Ast<'ast> {
        self.generic_arena.alloc(ast)
    }

    pub fn pattern<'ast>(&'ast self, pattern: Pattern<'ast>) -> &'ast Pattern<'ast> {
        self.generic_arena.alloc(pattern)
    }

    pub fn enum_pattern<'ast>(
        &'ast self,
        enum_pattern: EnumPattern<'ast>,
    ) -> &'ast EnumPattern<'ast> {
        self.generic_arena.alloc(enum_pattern)
    }

    pub fn field_pattern<'ast>(
        &'ast self,
        field_pat: FieldPattern<'ast>,
    ) -> &'ast FieldPattern<'ast> {
        self.generic_arena.alloc(field_pat)
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
        cst_pat: ConstantPattern<'ast>,
    ) -> &'ast ConstantPattern<'ast> {
        self.generic_arena.alloc(cst_pat)
    }

    pub fn or_pattern<'ast, I>(&'ast self, patterns: I, pos: TermPos) -> &'ast OrPattern<'ast>
    where
        I: IntoIterator<Item = Pattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        let patterns = self.generic_arena.alloc_slice_fill_iter(patterns);

        self.generic_arena.alloc(OrPattern { patterns, pos })
    }
}
