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

use record::Record;

use crate::{
    identifier::{Ident, LocIdent},
    label::Label,
    term::{Number, StrChunk, MatchData, pattern::Pattern, TypeAnnotation},
    cache::InputFormat,
    typ::Type,
    error::ParseError,
};


pub mod record;
// pub mod pattern;

#[derive(Clone, Debug, PartialEq)]
pub struct Ast<'ast> {
    node: &'ast Node<'ast>,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub enum Node<'ast> {
    /// The null value.
    #[default]
    Null,

    /// A boolean value.
    Bool(bool),

    /// A number.
    Number(Number),

    /// A string literal.
    String(String),

    /// A string containing interpolated expressions, represented as a list of either literals or
    /// expressions.
    ///
    /// As opposed to [crate::term::Term::StrChunks], the chunks are stored in the original order:
    /// `"hello%{var}"` will give `["hello", var]`.
    StrChunks(&'ast [StrChunk<Ast<'ast>>]),

    /// A standard function.
    Fun(LocIdent, Ast<'ast>),

    /// A destructuring function.
    FunPattern(&'ast Pattern, Ast<'ast>),

    /// A blame label.
    Label(Label),

    /// A let-binding.
    Let {
        pattern: &'ast [(Pattern, Ast<'ast>)],
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
    Record(Record<'ast>),

    /// A match expression. This expression is still to be applied to an argument to match on.
    Match(MatchData),

    /// An array.
    Array(&'ast [Ast<'ast>]),

    /// A primitive operator application.
    PrimOp { op: PrimOp, args: &'ast [Ast<'ast>] },

    /// A term with a type and/or contract annotation.
    Annotated {
        annot: TypeAnnotation,
        inner: Ast<'ast>,
    },

    /// An import.
    Import { path: OsString, format: InputFormat },

    /// A type in term position, such as in `let my_contract = Number -> Number in ...`.
    ///
    /// During evaluation, this will get turned into a contract.
    Type(Type),

    /// A term that couldn't be parsed properly. Used by the LSP to handle partially valid
    /// programs.
    ParseError(ParseError),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PrimOp { }
