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
    iter, rc,
};

use pattern::Pattern;
use record::{FieldDef, Record};

use crate::{
    cache::InputFormat,
    error::ParseError,
    identifier::{Ident, LocIdent},
    position::TermPos,
    traverse::*,
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
#[derive(Clone, Debug, PartialEq, Eq, Default)]
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
        args: &'ast [Pattern<'ast>],
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetBinding<'ast> {
    pub pattern: Pattern<'ast>,
    pub metadata: LetMetadata<'ast>,
    pub value: Ast<'ast>,
}

/// The metadata that can be attached to a let. It's a subset of [record::FieldMetadata].
#[derive(Debug, Default, Clone, PartialEq, Eq)]
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
#[derive(Clone, Debug, PartialEq, Eq)]
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
#[derive(Debug, PartialEq, Eq, Clone)]
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
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Match<'ast> {
    /// Branches of the match expression, where the first component is the pattern on the left hand
    /// side of `=>` and the second component is the body of the branch.
    pub branches: &'ast [MatchBranch<'ast>],
}

/// A type and/or contract annotation.
#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Annotation<'ast> {
    /// The type annotation (using `:`).
    pub typ: Option<Type<'ast>>,

    /// The contract annotations (using `|`).
    pub contracts: &'ast [Type<'ast>],
}

impl<'ast> Annotation<'ast> {
    /// Returns the main annotation, which is either the type annotation if any, or the first
    /// contract annotation.
    pub fn first<'a>(&'a self) -> Option<&'a Type<'ast>> {
        self.typ.as_ref().or(self.contracts.iter().next())
    }

    /// Iterates over the annotations, starting by the type and followed by the contracts.
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = &'a Type<'ast>> {
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

impl<'ast> TraverseAlloc<'ast, Ast<'ast>> for Ast<'ast> {
    /// Traverse through all [Ast] in the tree.
    ///
    /// This also recurses into the terms that are contained in [typ::Type] subtrees.
    fn traverse<F, E>(
        self,
        alloc: &'ast AstAlloc,
        f: &mut F,
        order: TraverseOrder,
    ) -> Result<Ast<'ast>, E>
    where
        F: FnMut(Ast<'ast>) -> Result<Ast<'ast>, E>,
    {
        let ast = match order {
            TraverseOrder::TopDown => f(self)?,
            TraverseOrder::BottomUp => self,
        };
        let pos = ast.pos;

        let result = match &ast.node {
            Node::Fun { args, body } => {
                let args = traverse_alloc_many(alloc, args.iter().cloned(), f, order)?;
                let body = alloc.alloc((*body).clone().traverse(alloc, f, order)?);

                Ast {
                    node: Node::Fun { args, body },
                    pos,
                }
            }
            Node::Let {
                bindings,
                body,
                rec,
            } => {
                let bindings = traverse_alloc_many(alloc, bindings.iter().cloned(), f, order)?;
                let body = alloc.alloc((*body).clone().traverse(alloc, f, order)?);

                Ast {
                    node: Node::Let {
                        bindings,
                        body,
                        rec: *rec,
                    },
                    pos,
                }
            }
            Node::App { head, args } => {
                let head = alloc.alloc((*head).clone().traverse(alloc, f, order)?);
                let args = traverse_alloc_many(alloc, args.iter().cloned(), f, order)?;

                Ast {
                    node: Node::App { head, args },
                    pos,
                }
            }
            Node::Match(data) => {
                let branches = traverse_alloc_many(alloc, data.branches.iter().cloned(), f, order)?;

                Ast {
                    node: Node::Match(Match { branches }),
                    pos,
                }
            }
            Node::PrimOpApp { op, args } => {
                let args = traverse_alloc_many(alloc, args.iter().cloned(), f, order)?;

                Ast {
                    node: Node::PrimOpApp { op, args },
                    pos,
                }
            }
            Node::Record(record) => {
                let field_defs =
                    traverse_alloc_many(alloc, record.field_defs.iter().cloned(), f, order)?;

                Ast {
                    node: Node::Record(alloc.alloc(record::Record {
                        field_defs,
                        open: record.open,
                    })),
                    pos,
                }
            }
            Node::Array(elts) => {
                let elts = traverse_alloc_many(alloc, elts.iter().cloned(), f, order)?;

                Ast {
                    node: Node::Array(elts),
                    pos,
                }
            }
            Node::StringChunks(chunks) => {
                let chunks_res: Result<Vec<StringChunk<Ast<'ast>>>, E> = chunks
                    .iter()
                    .cloned()
                    .map(|chunk| match chunk {
                        chunk @ StringChunk::Literal(_) => Ok(chunk),
                        StringChunk::Expr(ast, indent) => {
                            Ok(StringChunk::Expr(ast.traverse(alloc, f, order)?, indent))
                        }
                    })
                    .collect();

                Ast {
                    node: Node::StringChunks(alloc.alloc_many(chunks_res?)),
                    pos,
                }
            }
            Node::Annotated { annot, inner } => {
                let annot = alloc.alloc((*annot).clone().traverse(alloc, f, order)?);
                let inner = alloc.alloc((*inner).clone().traverse(alloc, f, order)?);

                Ast {
                    node: Node::Annotated { annot, inner },
                    pos,
                }
            }
            Node::Type(typ) => {
                let typ = alloc.alloc((*typ).clone().traverse(alloc, f, order)?);

                Ast {
                    node: Node::Type(typ),
                    pos,
                }
            }
            _ => ast,
        };

        match order {
            TraverseOrder::TopDown => Ok(result),
            TraverseOrder::BottomUp => f(result),
        }
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&Ast<'ast>, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        let child_state = match f(self, state) {
            TraverseControl::Continue => None,
            TraverseControl::ContinueWithScope(s) => Some(s),
            TraverseControl::SkipBranch => {
                return None;
            }
            TraverseControl::Return(ret) => {
                return Some(ret);
            }
        };
        let state = child_state.as_ref().unwrap_or(state);

        match self.node {
            Node::Null
            | Node::Bool(_)
            | Node::Number(_)
            | Node::String(_)
            | Node::Var(_)
            | Node::Import(_)
            | Node::ParseError(_) => None,
            Node::IfThenElse {
                cond,
                then_branch,
                else_branch,
            } => cond
                .traverse_ref(f, state)
                .or_else(|| then_branch.traverse_ref(f, state))
                .or_else(|| else_branch.traverse_ref(f, state)),
            Node::EnumVariant { tag: _, arg } => arg?.traverse_ref(f, state),
            Node::StringChunks(chunks) => chunks.iter().find_map(|chk| {
                if let StringChunk::Expr(term, _) = chk {
                    term.traverse_ref(f, state)
                } else {
                    None
                }
            }),
            Node::Fun { args, body } => args
                .iter()
                .find_map(|arg| arg.traverse_ref(f, state))
                .or_else(|| body.traverse_ref(f, state)),
            Node::PrimOpApp { op: _, args } => {
                args.iter().find_map(|arg| arg.traverse_ref(f, state))
            }
            Node::Let {
                bindings,
                body,
                rec: _,
            } => bindings
                .iter()
                .find_map(|binding| binding.traverse_ref(f, state))
                .or_else(|| body.traverse_ref(f, state)),
            Node::App { head, args } => head
                .traverse_ref(f, state)
                .or_else(|| args.iter().find_map(|arg| arg.traverse_ref(f, state))),
            Node::Record(data) => data
                .field_defs
                .iter()
                .find_map(|field_def| field_def.traverse_ref(f, state)),
            Node::Match(data) => data.branches.iter().find_map(
                |MatchBranch {
                     pattern,
                     guard,
                     body,
                 }| {
                    pattern
                        .traverse_ref(f, state)
                        .or_else(|| {
                            if let Some(cond) = guard.as_ref() {
                                cond.traverse_ref(f, state)
                            } else {
                                None
                            }
                        })
                        .or_else(|| body.traverse_ref(f, state))
                },
            ),
            Node::Array(elts) => elts.iter().find_map(|t| t.traverse_ref(f, state)),
            Node::Annotated { annot, inner } => annot
                .traverse_ref(f, state)
                .or_else(|| inner.traverse_ref(f, state)),
            Node::Type(typ) => typ.traverse_ref(f, state),
        }
    }
}

impl<'ast> TraverseAlloc<'ast, Type<'ast>> for Ast<'ast> {
    fn traverse<F, E>(
        self,
        alloc: &'ast AstAlloc,
        f: &mut F,
        order: TraverseOrder,
    ) -> Result<Ast<'ast>, E>
    where
        F: FnMut(Type<'ast>) -> Result<Type<'ast>, E>,
    {
        self.traverse(
            alloc,
            &mut |ast: Ast<'ast>| match &ast.node {
                Node::Type(typ) => {
                    let typ = alloc.alloc((*typ).clone().traverse(alloc, f, order)?);
                    Ok(Ast {
                        node: Node::Type(typ),
                        pos: ast.pos,
                    })
                }
                _ => Ok(ast),
            },
            order,
        )
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&Type<'ast>, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        self.traverse_ref(
            &mut |ast: &Ast<'ast>, state: &S| match &ast.node {
                Node::Type(typ) => typ.traverse_ref(f, state).into(),
                _ => TraverseControl::Continue,
            },
            state,
        )
    }
}

impl<'ast> TraverseAlloc<'ast, Ast<'ast>> for Annotation<'ast> {
    fn traverse<F, E>(
        self,
        alloc: &'ast AstAlloc,
        f: &mut F,
        order: TraverseOrder,
    ) -> Result<Self, E>
    where
        F: FnMut(Ast<'ast>) -> Result<Ast<'ast>, E>,
    {
        let typ = self
            .typ
            .map(|typ| typ.traverse(alloc, f, order))
            .transpose()?;
        let contracts = traverse_alloc_many(alloc, self.contracts.iter().cloned(), f, order)?;

        Ok(Annotation { typ, contracts })
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&Ast<'ast>, &S) -> TraverseControl<S, U>,
        scope: &S,
    ) -> Option<U> {
        self.typ
            .iter()
            .chain(self.contracts.iter())
            .find_map(|c| c.traverse_ref(f, scope))
    }
}

impl<'ast> TraverseAlloc<'ast, Ast<'ast>> for LetBinding<'ast> {
    fn traverse<F, E>(
        self,
        alloc: &'ast AstAlloc,
        f: &mut F,
        order: TraverseOrder,
    ) -> Result<Self, E>
    where
        F: FnMut(Ast<'ast>) -> Result<Ast<'ast>, E>,
    {
        let pattern = self.pattern.traverse(alloc, f, order)?;

        let metadata = LetMetadata {
            annotation: self.metadata.annotation.traverse(alloc, f, order)?,
            doc: self.metadata.doc,
        };

        let value = self.value.traverse(alloc, f, order)?;

        Ok(LetBinding {
            pattern,
            metadata,
            value,
        })
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&Ast<'ast>, &S) -> TraverseControl<S, U>,
        scope: &S,
    ) -> Option<U> {
        self.metadata
            .annotation
            .traverse_ref(f, scope)
            .or_else(|| self.value.traverse_ref(f, scope))
    }
}

impl<'ast> TraverseAlloc<'ast, Ast<'ast>> for MatchBranch<'ast> {
    fn traverse<F, E>(
        self,
        alloc: &'ast AstAlloc,
        f: &mut F,
        order: TraverseOrder,
    ) -> Result<Self, E>
    where
        F: FnMut(Ast<'ast>) -> Result<Ast<'ast>, E>,
    {
        let pattern = self.pattern.traverse(alloc, f, order)?;
        let body = self.body.traverse(alloc, f, order)?;
        let guard = self
            .guard
            .map(|guard| guard.traverse(alloc, f, order))
            .transpose()?;

        Ok(MatchBranch {
            pattern,
            guard,
            body,
        })
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&Ast<'ast>, &S) -> TraverseControl<S, U>,
        scope: &S,
    ) -> Option<U> {
        self.pattern
            .traverse_ref(f, scope)
            .or_else(|| self.body.traverse_ref(f, scope))
            .or_else(|| {
                self.guard
                    .as_ref()
                    .and_then(|guard| guard.traverse_ref(f, scope))
            })
    }
}

/// Marker trait for AST nodes that don't need to be dropped (in practice, it's often equivalent to
/// not owning any heap allocated data) and can be used with [allocator][AstAlloc::alloc]. The
/// current exceptions are [Number] and [crate::error::ParseError], which must be allocated through
/// specialized method in [AstAlloc].
pub trait Allocable {}

impl Allocable for Ast<'_> {}
impl<T: Allocable> Allocable for StringChunk<T> {}
impl Allocable for LetBinding<'_> {}
impl Allocable for PrimOp {}
impl Allocable for Annotation<'_> {}
impl Allocable for MatchBranch<'_> {}

impl Allocable for Record<'_> {}
impl Allocable for record::FieldPathElem<'_> {}
impl Allocable for FieldDef<'_> {}

impl Allocable for Pattern<'_> {}
impl Allocable for EnumPattern<'_> {}
impl Allocable for FieldPattern<'_> {}
impl Allocable for RecordPattern<'_> {}
impl Allocable for ArrayPattern<'_> {}
impl Allocable for OrPattern<'_> {}
impl Allocable for ConstantPattern<'_> {}

impl Allocable for Type<'_> {}
impl Allocable for typ::RecordRows<'_> {}
impl Allocable for typ::EnumRows<'_> {}
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
    /// Creates a new ast allocator.
    pub fn new() -> Self {
        Self {
            generic_arena: Bump::new(),
            number_arena: typed_arena::Arena::new(),
            error_arena: typed_arena::Arena::new(),
        }
    }

    /// Allocates an AST component in the arena.
    ///
    /// [Self] never guarantees that all destructors are going to be run when using such a generic
    /// allocation function. We don't want to allocate values that need to be dropped through this
    /// method, typically because they own heap-allocated data, such as numbers or parse errors.
    /// That's why we use a marker trait to specify which types can be allocated freely. Types that
    /// need to be dropped don't implement [Allocable] and have a dedicated method for allocation.
    pub fn alloc<T: Allocable>(&self, value: T) -> &T {
        self.generic_arena.alloc(value)
    }

    /// Allocates a sequence of AST components in the arena.
    ///
    /// See [Self::alloc].
    pub fn alloc_many<T: Allocable, I>(&self, iter: I) -> &[T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        self.generic_arena.alloc_slice_fill_iter(iter)
    }

    /// Allocates an array with exactly one element in the arena.
    pub fn alloc_singleton<T: Allocable>(&self, value: T) -> &[T] {
        self.generic_arena.alloc_slice_fill_iter(iter::once(value))
    }

    /// Allocates a string in the arena.
    pub fn alloc_str<'ast>(&'ast self, s: &str) -> &'ast str {
        self.generic_arena.alloc_str(s)
    }

    pub fn number(&self, number: Number) -> Node<'_> {
        Node::Number(self.number_arena.alloc(number))
    }

    pub fn alloc_number(&self, number: Number) -> &'_ Number {
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

    pub fn fun<'ast, I>(&'ast self, args: I, body: Ast<'ast>) -> Node<'ast>
    where
        I: IntoIterator<Item = Pattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        Node::Fun {
            args: self.generic_arena.alloc_slice_fill_iter(args),
            body: self.generic_arena.alloc(body),
        }
    }

    pub fn unary_fun<'ast>(&'ast self, arg: Pattern<'ast>, body: Ast<'ast>) -> Node<'ast> {
        Node::Fun {
            args: self.generic_arena.alloc_slice_fill_iter(iter::once(arg)),
            body: self.generic_arena.alloc(body),
        }
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

    pub fn type_data<'ast>(&'ast self, typ: TypeUnr<'ast>, pos: TermPos) -> &'ast Type<'ast> {
        self.generic_arena.alloc(Type { typ, pos })
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
            patterns: self.generic_arena.alloc_slice_fill_iter(patterns),
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
            patterns: self.generic_arena.alloc_slice_fill_iter(patterns),
            tail,
            pos,
        })
    }

    pub fn or_pattern<'ast, I>(&'ast self, patterns: I, pos: TermPos) -> &'ast OrPattern<'ast>
    where
        I: IntoIterator<Item = Pattern<'ast>>,
        I::IntoIter: ExactSizeIterator,
    {
        self.generic_arena.alloc(OrPattern {
            patterns: self.generic_arena.alloc_slice_fill_iter(patterns),
            pos,
        })
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

/// Similar to `TryFrom`, but takes an additional allocator for conversion from and to
/// [crate::bytecode::ast::Ast] that requires to thread an explicit allocator.
///
/// We chose a different name than `try_from` for the method - although it has a different
/// signature from the standard `TryFrom` (two arguments vs one) - to avoid confusing the compiler
/// which would otherwise have difficulties disambiguating calls like `Ast::try_from`.
pub(crate) trait TryConvert<'ast, T>
where
    Self: Sized,
{
    type Error;

    fn try_convert(alloc: &'ast AstAlloc, from: T) -> Result<Self, Self::Error>;
}
