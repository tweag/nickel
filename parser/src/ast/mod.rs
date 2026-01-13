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

use std::{cmp::Ordering, ffi::OsStr, fmt, path::Path};

use malachite::base::num::basic::traits::Zero;

use crate::{
    error::ParseError,
    identifier::{Ident, LocIdent},
    impl_display_from_bytecode_pretty,
    position::TermPos,
    traverse::*,
};

/// The underlying type representing Nickel numbers. Currently, numbers are arbitrary precision
/// rationals.
///
/// Basic arithmetic operations are exact, without loss of precision (within the limits of available
/// memory).
///
/// Raising to a power that doesn't fit in a signed 64bits number will lead to converting both
/// operands to 64-bits floats, performing the floating-point power operation, and converting back
/// to rationals, which can incur a loss of precision.
///
/// [^number-serialization]: Conversion to string and serialization try to first convert the
///     rational as an exact signed or usigned 64-bits integer. If this succeeds, such operations
///     don't lose precision. Otherwise, the number is converted to the nearest 64bit float and then
///     serialized/printed, which can incur a loss of information.
pub type Number = malachite::rational::Rational;

pub mod alloc;
pub mod builder;
pub mod combine;
pub mod pattern;
pub mod pretty;
pub mod primop;
pub mod record;
pub mod typ;

pub use alloc::AstAlloc;
use pattern::*;
use primop::PrimOp;
use record::*;
use serde::{Serialize, Serializer};
use typ::*;

/// Supported input formats.
#[derive(Default, Clone, Copy, Eq, Debug, PartialEq, Hash, rkyv::Archive)]
#[cfg_attr(feature = "clap", derive(clap::ValueEnum))]
pub enum InputFormat {
    #[default]
    Nickel,
    Json,
    Yaml,
    Toml,
    #[cfg(feature = "nix-experimental")]
    Nix,
    Text,
}

impl InputFormat {
    /// Returns an [InputFormat] based on the file extension of a path.
    pub fn from_path(path: impl AsRef<Path>) -> Option<InputFormat> {
        match path.as_ref().extension().and_then(OsStr::to_str) {
            Some("ncl") => Some(InputFormat::Nickel),
            Some("json") => Some(InputFormat::Json),
            Some("yaml") | Some("yml") => Some(InputFormat::Yaml),
            Some("toml") => Some(InputFormat::Toml),
            #[cfg(feature = "nix-experimental")]
            Some("nix") => Some(InputFormat::Nix),
            Some("txt") => Some(InputFormat::Text),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &'static str {
        match self {
            InputFormat::Nickel => "Nickel",
            InputFormat::Json => "Json",
            InputFormat::Yaml => "Yaml",
            InputFormat::Toml => "Toml",
            InputFormat::Text => "Text",
            #[cfg(feature = "nix-experimental")]
            InputFormat::Nix => "Nix",
        }
    }
}

impl fmt::Display for InputFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl std::str::FromStr for InputFormat {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "Json" => InputFormat::Json,
            "Nickel" => InputFormat::Nickel,
            "Text" => InputFormat::Text,
            "Yaml" => InputFormat::Yaml,
            "Toml" => InputFormat::Toml,
            #[cfg(feature = "nix-experimental")]
            "Nix" => InputFormat::Nix,
            _ => return Err(()),
        })
    }
}

/// A chunk of a string with interpolated expressions inside. Can be either a string literal or an
/// interpolated expression.
#[derive(Debug, PartialEq, Eq, Clone, rkyv::Archive)]
pub enum StringChunk<E> {
    /// A string literal.
    Literal(String),

    /// An interpolated expression.
    Expr(
        E,     /* the expression */
        usize, /* the indentation level (see parser::utils::strip_indent) */
    ),
}

impl<E> StringChunk<E> {
    // This is only for use in tests, but because some tests are in other crates
    // it can't be cfg(test).
    #[doc(hidden)]
    pub fn expr(e: E) -> Self {
        StringChunk::Expr(e, 0)
    }

    pub fn try_chunks_as_static_str<'a, I>(chunks: I) -> Option<String>
    where
        I: IntoIterator<Item = &'a StringChunk<E>>,
        E: 'a,
    {
        chunks
            .into_iter()
            .try_fold(String::new(), |mut acc, next| match next {
                StringChunk::Literal(lit) => {
                    acc.push_str(lit);
                    Some(acc)
                }
                _ => None,
            })
    }
}

#[derive(Debug, Clone, Default, rkyv::Archive)]
pub enum MergePriority {
    /// The priority of default values that are overridden by everything else.
    Bottom,

    /// The priority by default, when no priority annotation (`default`, `force`, `priority`) is
    /// provided.
    ///
    /// Act as the value `MergePriority::Numeral(0)` with respect to ordering and equality
    /// testing. The only way to discriminate this variant is to pattern match on it.
    #[default]
    Neutral,

    /// A numeral priority.
    Numeral(#[rkyv(with = crate::stash::NumberStash)] Number),

    /// The priority of values that override everything else and can't be overridden.
    Top,
}

impl PartialOrd for MergePriority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for MergePriority {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (MergePriority::Bottom, MergePriority::Bottom)
            | (MergePriority::Neutral, MergePriority::Neutral)
            | (MergePriority::Top, MergePriority::Top) => true,
            (MergePriority::Numeral(p1), MergePriority::Numeral(p2)) => p1 == p2,
            (MergePriority::Neutral, MergePriority::Numeral(p))
            | (MergePriority::Numeral(p), MergePriority::Neutral)
                if p == &Number::ZERO =>
            {
                true
            }
            _ => false,
        }
    }
}

impl Eq for MergePriority {}

impl Ord for MergePriority {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            // Equalities
            (MergePriority::Bottom, MergePriority::Bottom)
            | (MergePriority::Top, MergePriority::Top)
            | (MergePriority::Neutral, MergePriority::Neutral) => Ordering::Equal,
            (MergePriority::Numeral(p1), MergePriority::Numeral(p2)) => p1.cmp(p2),

            // Top and bottom.
            (MergePriority::Bottom, _) | (_, MergePriority::Top) => Ordering::Less,
            (MergePriority::Top, _) | (_, MergePriority::Bottom) => Ordering::Greater,

            // Neutral and numeral.
            (MergePriority::Neutral, MergePriority::Numeral(n)) => Number::ZERO.cmp(n),
            (MergePriority::Numeral(n), MergePriority::Neutral) => n.cmp(&Number::ZERO),
        }
    }
}

impl fmt::Display for MergePriority {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MergePriority::Bottom => write!(f, "default"),
            MergePriority::Neutral => write!(f, "{}", Number::ZERO),
            MergePriority::Numeral(p) => write!(f, "{p}"),
            MergePriority::Top => write!(f, "force"),
        }
    }
}

impl Serialize for MergePriority {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

/// Possible origins of a merge operation.
#[derive(Clone, Copy, Eq, PartialEq, Debug, Default, rkyv::Archive)]
pub enum MergeKind {
    /// A standard, user-written merge operation (or a merge operation descending from a
    /// user-written merge operation).
    #[default]
    Standard,
    /// A merge generated by the parser when reconstructing piecewise definition, for example:
    ///
    /// ```nickel
    /// { foo = def1, foo = def2 }
    /// ```
    PiecewiseDef,
}

/// A flavor for record operations. By design, we want empty optional values to be transparent for
/// record operations, because they would otherwise make many operations fail spuriously (e.g.
/// trying to map over such an empty value). So they are most of the time silently ignored.
///
/// However, it's sometimes useful and even necessary to take them into account. This behavior is
/// controlled by [RecordOpKind].
#[derive(Clone, Debug, PartialEq, Eq, Copy, Default, rkyv::Archive)]
pub enum RecordOpKind {
    #[default]
    IgnoreEmptyOpt,
    ConsiderAllFields,
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
    /// As opposed to `nickel_lang_core::term::Term::StrChunks`, the chunks are stored in the original order:
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

impl<'ast> Ast<'ast> {
    /// Sets a new position for this AST node.
    pub fn with_pos(self, pos: TermPos) -> Self {
        Ast { pos, ..self }
    }

    /// Removes the position from this AST node and (recursively) child nodes.
    ///
    /// This is mainly useful for tests, where we often want to compare syntax
    /// trees without their locations.
    #[cfg(test)]
    pub fn without_pos(self, alloc: &'ast AstAlloc) -> Self {
        self.traverse(
            alloc,
            &mut |t: Type| -> Result<_, std::convert::Infallible> {
                Ok(Type {
                    pos: TermPos::None,
                    ..t
                })
            },
            TraverseOrder::BottomUp,
        )
        .unwrap()
        .traverse(
            alloc,
            &mut |t: Ast<'_>| -> Result<_, std::convert::Infallible> {
                let node = match t.node {
                    Node::Record(r) => Node::Record(alloc.alloc(Record {
                        field_defs: alloc.alloc_many(r.field_defs.iter().map(|fd| FieldDef {
                            pos: TermPos::None,
                            ..fd.clone()
                        })),
                        ..r.clone()
                    })),
                    n => n,
                };
                Ok(Ast {
                    pos: TermPos::None,
                    node,
                })
            },
            TraverseOrder::BottomUp,
        )
        .unwrap()
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
    /// Importing packges requires a `PackageMap` to translate the location
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
            Node::IfThenElse {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond = alloc.alloc((*cond).clone().traverse(alloc, f, order)?);
                let then_branch = alloc.alloc((*then_branch).clone().traverse(alloc, f, order)?);
                let else_branch = alloc.alloc((*else_branch).clone().traverse(alloc, f, order)?);

                Ast {
                    node: Node::IfThenElse {
                        cond,
                        then_branch,
                        else_branch,
                    },
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
/// [crate::ast::Ast] that requires to thread an explicit allocator.
///
/// We chose a different name than `try_from` for the method - although it has a different
/// signature from the standard `TryFrom` (two arguments vs one) - to avoid confusing the compiler
/// which would otherwise have difficulties disambiguating calls like `Ast::try_from`.
pub trait TryConvert<'ast, T>
where
    Self: Sized,
{
    type Error;

    fn try_convert(alloc: &'ast AstAlloc, from: T) -> Result<Self, Self::Error>;
}

impl_display_from_bytecode_pretty!(Node<'_>);
impl_display_from_bytecode_pretty!(Ast<'_>);
