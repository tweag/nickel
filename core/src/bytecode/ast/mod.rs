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
    label::Label,
    position::TermPos,
    term::{Number, StrChunk, TypeAnnotation},
    typ::Type,
};

pub mod pattern;
pub mod record;

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

    /// A standard function.
    Fun(LocIdent, Ast<'ast>),

    /// A destructuring function.
    FunPattern(&'ast Pattern<'ast>, Ast<'ast>),

    /// A blame label.
    Label(Label),

    /// A let-binding.
    Let {
        pattern: &'ast [(Pattern<'ast>, Ast<'ast>)],
        body: Ast<'ast>,
        rec: bool,
    },

    /// An application.
    App(Ast<'ast>, Ast<'ast>),

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
        // TODO: needs to run destructor?
        annot: TypeAnnotation,
        inner: Ast<'ast>,
    },

    /// An import.
    Import { path: OsString, format: InputFormat },

    /// A type in term position, such as in `let my_contract = Number -> Number in ...`.
    ///
    /// During evaluation, this will get turned into a contract.
    // TODO: needs to run destructor?
    Type(&'ast Type),

    /// A term that couldn't be parsed properly. Used by the LSP to handle partially valid
    /// programs.
    // TODO: needs to run destructor?
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
