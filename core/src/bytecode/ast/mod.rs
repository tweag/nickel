//! The Nickel AST, as ingested by the (future) bytecode compiler.
//!
//! Since the AST is built once for each Nickel expression and is then compiled away to bytecode,
//! the total number of allocated nodes is reasonably bounded by the input program size. Thus, for
//! performance reasons, we allocate notes using an arena and keep them alive until the end of
//! compilation. In return, we get fast allocation and de-allocation, and we can easily reference
//! other nodes and data structures using native references.
//!
//! The corresponding lifetime of all the nodes - and thus of the arena as well - is consistently
//! called `'ast`.

use std::{
    ffi::{OsStr, OsString},
    fmt::Debug,
    rc,
};

use pattern::Pattern;
use record::{FieldDef, Record};

use crate::{
    cache::InputFormat,
    error::ParseError,
    identifier::{Ident, LocIdent},
    position::TermPos,
};

// For now, we reuse those types from the term module.
pub use crate::term::{MergePriority, Number, StrChunk as StringChunk};

use bumpalo::Bump;

pub mod builder;
pub mod combine;
pub mod compat;
pub mod pattern;
pub mod primop;
pub mod record;
pub mod typ;

use pattern::*;
use primop::PrimOp;
use typ::*;

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
    StringChunks(&'ast [StringChunk<Ast<'ast>>]),

    /// A function.
    Fun {
        arg: &'ast Pattern<'ast>,
        body: &'ast Ast<'ast>,
    },

    /// A let block.
    Let {
        bindings: &'ast [LetBinding<'ast>],
        body: &'ast Ast<'ast>,
        rec: bool,
    },

    /// An application to one or more arguments.
    App {
        head: &'ast Ast<'ast>,
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
    Import(Import<'ast>),

    /// A type in term position, such as in `let my_contract = Number -> Number in ...`.
    ///
    /// During evaluation, this will get turned into a contract.
    Type(&'ast Type<'ast>),

    /// A term that couldn't be parsed properly. Used by the LSP to handle partially valid
    /// programs.
    ParseError(&'ast ParseError),
}

/// An individual binding in a let block.
#[derive(Debug, Clone, PartialEq)]
pub struct LetBinding<'ast> {
    pub pattern: Pattern<'ast>,
    pub metadata: LetMetadata<'ast>,
    pub value: Ast<'ast>,
}

/// The metadata that can be attached to a let. It's a subset of [record::FieldMetadata].
#[derive(Debug, Default, Clone, PartialEq)]
pub struct LetMetadata<'ast> {
    pub doc: Option<rc::Rc<str>>,
    pub annotation: Annotation<'ast>,
}

impl<'ast> From<LetMetadata<'ast>> for record::FieldMetadata<'ast> {
    fn from(let_metadata: LetMetadata<'ast>) -> Self {
        record::FieldMetadata {
            annotation: let_metadata.annotation,
            doc: let_metadata.doc,
            ..Default::default()
        }
    }
}

impl<'ast> TryFrom<record::FieldMetadata<'ast>> for LetMetadata<'ast> {
    type Error = ();

    fn try_from(field_metadata: record::FieldMetadata<'ast>) -> Result<Self, Self::Error> {
        if let record::FieldMetadata {
            doc,
            annotation,
            opt: false,
            not_exported: false,
            priority: MergePriority::Neutral,
        } = field_metadata
        {
            Ok(LetMetadata { doc, annotation })
        } else {
            Err(())
        }
    }
}

impl<'ast> Node<'ast> {
    /// Tries to extract a static literal from string chunks.
    ///
    /// This methods returns a `Some(..)` when the term is a [Node::StringChunks] and all the
    /// chunks are [StringChunk::Literal]
    pub fn try_str_chunk_as_static_str(&self) -> Option<String> {
        match self {
            Node::StringChunks(chunks) => StringChunk::try_chunks_as_static_str(*chunks),
            _ => None,
        }
    }

    /// Attaches a position to this node turning it into an [Ast].
    pub fn spanned(self, pos: TermPos) -> Ast<'ast> {
        Ast { node: self, pos }
    }
}

/// A Nickel AST. Contains a root node and a span.
///
//TODO: we don't expect to access the span much on the happy path. Should we add an indirection
//through a reference?
#[derive(Clone, Debug, PartialEq)]
pub struct Ast<'ast> {
    pub node: Node<'ast>,
    pub pos: TermPos,
}

impl Ast<'_> {
    /// Sets a new position for this AST node.
    pub fn with_pos(self, pos: TermPos) -> Self {
        Ast { pos, ..self }
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
    pub typ: Option<Type<'ast>>,

    /// The contract annotations (using `|`).
    pub contracts: &'ast [Type<'ast>],
}

impl<'ast> Annotation<'ast> {
    /// Returns the main annotation, which is either the type annotation if any, or the first
    /// contract annotation.
    pub fn first(&'ast self) -> Option<&'ast Type<'ast>> {
        self.typ.as_ref().or(self.contracts.iter().next())
    }

    /// Iterates over the annotations, starting by the type and followed by the contracts.
    pub fn iter(&'ast self) -> impl Iterator<Item = &'ast Type<'ast>> {
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

#[derive(Clone, Debug, PartialEq, Eq)]
/// Specifies where something should be imported from.
pub enum Import<'ast> {
    Path {
        path: &'ast OsStr,
        format: InputFormat,
    },
    /// Importing packges requires a [`crate::package::PackageMap`] to translate the location
    /// to a path. The format is always Nickel.
    Package { id: Ident },
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

    /// Allocate an AST element in the arena.
    ///
    /// [Self] never guarantees that all destructors are going to be run when using such a generic
    /// allocation function. We don't want to allocate values that need to be dropped through this
    /// method, typically because they own heap-allocated data, such as numbers or parse errors.
    /// That's why we use a marker trait to specify which types can be allocated freely. Types that
    /// need to be dropped have a dedicated method for allocation.
    pub fn alloc<T>(&self, value: T) -> &T {
        self.generic_arena.alloc(value)
    }

    /// Allocate a sequence of AST elements in the arena.
    ///
    /// See [Self::alloc].
    pub fn alloc_iter<T, I>(&self, iter: I) -> &[T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        self.generic_arena.alloc_slice_fill_iter(iter)
    }

    pub fn alloc_str<'ast>(&'ast self, s: &str) -> &'ast str {
        self.generic_arena.alloc_str(s)
    }

    pub fn node<'ast>(&'ast self, node: Node<'ast>) -> &'ast Node<'ast> {
        self.generic_arena.alloc(node)
    }

    pub fn number(&self, number: Number) -> Node<'_> {
        Node::Number(self.number_arena.alloc(number))
    }

    pub fn number_move(&self, number: Number) -> &'_ Number {
        self.number_arena.alloc(number)
    }

    pub fn string<'ast>(&'ast self, s: &str) -> Node<'ast> {
        Node::String(self.generic_arena.alloc_str(s))
    }

    pub fn string_chunks<'ast, I>(&'ast self, chunks: I) -> Node<'ast>
    where
        I: IntoIterator<Item = StringChunk<Ast<'ast>>>,
        I::IntoIter: ExactSizeIterator,
    {
        Node::StringChunks(self.generic_arena.alloc_slice_fill_iter(chunks))
    }

    pub fn fun<'ast>(&'ast self, pat: Pattern<'ast>, body: Ast<'ast>) -> Node<'ast> {
        let arg = self.generic_arena.alloc(pat);
        let body = self.generic_arena.alloc(body);
        Node::Fun { arg, body }
    }

    pub fn nary_fun<'ast, I>(&'ast self, args: I, body: Ast<'ast>) -> Node<'ast>
    where
        I: IntoIterator<Item = Pattern<'ast>>,
        I::IntoIter: DoubleEndedIterator,
    {
        args.into_iter()
            .rev()
            .fold(body, |body, arg| Ast {
                node: self.fun(arg, body),
                pos: TermPos::None,
            })
            .node
    }

    pub fn let_block<'ast, I>(&'ast self, bindings: I, body: Ast<'ast>, rec: bool) -> Node<'ast>
    where
        I: IntoIterator<Item = LetBinding<'ast>>,
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

    pub fn app<'ast, I>(&'ast self, head: Ast<'ast>, args: I) -> Node<'ast>
    where
        I: IntoIterator<Item = Ast<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        Node::App {
            head: self.generic_arena.alloc(head),
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

    pub fn record_data<'ast, Ss, Ds>(&'ast self, field_defs: Ds, open: bool) -> &'ast Record<'ast>
    where
        Ds: IntoIterator<Item = FieldDef<'ast>>,
        Ds::IntoIter: ExactSizeIterator,
    {
        self.generic_arena.alloc(Record {
            field_defs: self.generic_arena.alloc_slice_fill_iter(field_defs),
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

    pub fn annotation<'ast, I>(
        &'ast self,
        typ: Option<Type<'ast>>,
        contracts: I,
    ) -> Annotation<'ast>
    where
        I: IntoIterator<Item = Type<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        Annotation {
            typ,
            contracts: self.generic_arena.alloc_slice_fill_iter(contracts),
        }
    }

    pub fn import_path(&self, path: OsString, format: InputFormat) -> Node<'_> {
        Node::Import(Import::Path {
            path: self.generic_arena.alloc(path),
            format,
        })
    }

    pub fn import_package(&self, id: Ident) -> Node<'_> {
        Node::Import(Import::Package { id })
    }

    pub fn typ<'ast>(&'ast self, typ: Type<'ast>) -> Node<'ast> {
        Node::Type(self.generic_arena.alloc(typ))
    }

    pub fn type_from_unr<'ast>(&'ast self, typ: TypeUnr<'ast>, pos: TermPos) -> Node<'ast> {
        Node::Type(self.type_move(Type { typ, pos }))
    }

    pub fn type_data<'ast>(&'ast self, typ: TypeUnr<'ast>, pos: TermPos) -> &'ast Type<'ast> {
        self.type_move(Type { typ, pos })
    }

    pub fn type_move<'ast>(&'ast self, typ: Type<'ast>) -> &'ast Type<'ast> {
        self.generic_arena.alloc(typ)
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

    pub fn enum_rows_move<'ast>(&'ast self, erows: EnumRows<'ast>) -> &'ast EnumRows<'ast> {
        self.generic_arena.alloc(erows)
    }

    pub fn record_rows<'ast>(&'ast self, rrows: RecordRowsUnr<'ast>) -> &'ast RecordRows<'ast> {
        self.generic_arena.alloc(RecordRows(rrows))
    }

    pub fn record_rows_move<'ast>(&'ast self, rrows: RecordRows<'ast>) -> &'ast RecordRows<'ast> {
        self.generic_arena.alloc(rrows)
    }

    pub fn record_row<'ast>(&'ast self, id: LocIdent, typ: Type<'ast>) -> &'ast RecordRow<'ast> {
        self.generic_arena.alloc(RecordRow {
            id,
            typ: self.generic_arena.alloc(typ),
        })
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

    pub fn patterns<'ast, I>(&'ast self, patterns: I) -> &'ast [Pattern<'ast>]
    where
        I: IntoIterator<Item = Pattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        self.generic_arena.alloc_slice_fill_iter(patterns)
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

    pub fn field_patterns<'ast, I>(&'ast self, field_pats: I) -> &'ast [FieldPattern<'ast>]
    where
        I: IntoIterator<Item = FieldPattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        self.generic_arena.alloc_slice_fill_iter(field_pats)
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
        self.generic_arena.alloc(RecordPattern {
            patterns: self.field_patterns(patterns),
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
        self.generic_arena.alloc(ArrayPattern {
            patterns: self.patterns(patterns),
            tail,
            pos,
        })
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

// Phony implementation of `Debug` so that we can still derive the trait for structure that holds
// onto an allocator.
impl Debug for AstAlloc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AstAlloc")
    }
}

impl<'ast> From<Node<'ast>> for Ast<'ast> {
    fn from(node: Node<'ast>) -> Self {
        Ast {
            node,
            pos: TermPos::None,
        }
    }
}
