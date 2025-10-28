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

use std::ffi::OsStr;

use crate::{
    cache::InputFormat,
    error::ParseError,
    identifier::{Ident, LocIdent},
    impl_display_from_bytecode_pretty,
    position::TermPos,
    traverse::*,
};

// For now, we reuse those types from the term module.
pub use crate::term::{MergePriority, Number, StrChunk as StringChunk};

pub mod alloc;
pub mod builder;
pub mod combine;
pub mod compat;
pub mod pattern;
pub mod primop;
pub mod record;
pub mod typ;

pub use alloc::AstAlloc;
use pattern::*;
use primop::PrimOp;
use record::*;
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
    pub doc: Option<&'ast str>,
    pub annotation: Annotation<'ast>,
}

impl<'ast> From<LetMetadata<'ast>> for FieldMetadata<'ast> {
    fn from(let_metadata: LetMetadata<'ast>) -> Self {
        FieldMetadata {
            annotation: let_metadata.annotation,
            doc: let_metadata.doc,
            ..Default::default()
        }
    }
}

impl<'ast> TryFrom<FieldMetadata<'ast>> for LetMetadata<'ast> {
    type Error = ();

    fn try_from(field_metadata: FieldMetadata<'ast>) -> Result<Self, Self::Error> {
        if let FieldMetadata {
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

impl Default for Ast<'_> {
    fn default() -> Self {
        Ast {
            node: Node::Null,
            pos: TermPos::None,
        }
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

impl Annotation<'_> {
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

/// Specifies where something should be imported from.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Import<'ast> {
    Path {
        path: &'ast OsStr,
        format: InputFormat,
    },
    /// Importing packages requires a [`crate::package::PackageMap`] to translate the location
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
                    node: Node::Record(alloc.alloc(Record {
                        field_defs,
                        includes: record.includes,
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
        &'ast self,
        f: &mut dyn FnMut(&'ast Ast<'ast>, &S) -> TraverseControl<S, U>,
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
        &'ast self,
        f: &mut dyn FnMut(&'ast Type<'ast>, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        self.traverse_ref(
            &mut |ast: &'ast Ast<'ast>, state: &S| match &ast.node {
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
        &'ast self,
        f: &mut dyn FnMut(&'ast Ast<'ast>, &S) -> TraverseControl<S, U>,
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
        &'ast self,
        f: &mut dyn FnMut(&'ast Ast<'ast>, &S) -> TraverseControl<S, U>,
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
        &'ast self,
        f: &mut dyn FnMut(&'ast Ast<'ast>, &S) -> TraverseControl<S, U>,
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

impl_display_from_bytecode_pretty!(Node<'_>);
impl_display_from_bytecode_pretty!(Ast<'_>);
