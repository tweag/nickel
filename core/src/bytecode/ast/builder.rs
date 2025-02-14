//! A builder interface for producing Nickel values.
//!
//! Using this module, you can define Nickel values using a builder style. For example:
//!
//! ```rust
//! # use nickel_lang_core::bytecode::ast::{record::MergePriority, Ast, Node, builder::Record, AstAlloc};
//!
//! let alloc = AstAlloc::new();
//!
//! let record = Record::new()
//!     .field("foo")
//!     .priority(MergePriority::Bottom)
//!     .doc("foo?")
//!     .not_exported(true)
//!     .value(&alloc, alloc.string("foo"));
//!
//! let record = record
//!     .field("bar")
//!     .value(&alloc, alloc.number(42.into()))
//!     .build(&alloc);
//!
//! // Gives {foo | doc "foo?" | not_exported | default = "foo", bar = 42}
//! ```
//!
//! This modules also offers a simpler interface for basic values where all arguments can be
//! provided at once, to avoid useless intermediate allocations.
use crate::identifier::Ident;

use super::{
    record::{FieldMetadata, FieldPathElem, MergePriority},
    typ::Type,
    *,
};

use std::rc::Rc;

type StaticPath = Vec<Ident>;

/// Typestate style tag for `Field`s that are not yet completely specified
pub struct Incomplete();

/// Typestate style tag for `Field`s that have been finalized
pub struct Complete<'ast>(Option<Ast<'ast>>);

/// A Nickel record field being constructed
#[derive(Debug)]
pub struct Field<'ast, State> {
    /// This field stores different piece of information depending on the state of this field:
    ///
    /// - It's empty for an incomplete field (`()`).
    /// - When a value (or `None`) has been set, `State` becomes [Complete] and stores the
    ///   optional value of this field.
    /// - Internally, [Self] is also used temporarily with `State` set to `Record<'ast>>` inside
    ///   [Self::attach], before updating a record with a completed field (and thus consuming said
    ///   field). During this transitory phase, `state` stores the parent record.
    state: State,
    path: StaticPath,
    metadata: FieldMetadata<'ast>,
    /// We need to store the documentation separately, because the metadata only accepts a string
    /// that has been allocated in the AST allocator. We could require to thread the allocator for
    /// calls to [Self::doc] or [Self::some_doc], but it's more ergonomic to keep the phases of
    /// building a field and finalizing it separate. We thus store the documentation here
    /// temporarily.
    doc: Option<String>,
    /// As we might build contract element by element, we can't rely on
    /// `metadata.annotation.contracts`, which is a fixed array.
    ///
    /// We accumulate them in this field instead and keep `metadata.annotation.contracts` empty
    /// until finalization, where we finally allocate it.
    contracts: Vec<Type<'ast>>,
}

impl<'ast, A> Field<'ast, A> {
    /// Attach documentation metadata to the field
    pub fn doc(self, doc: &impl ToString) -> Self {
        self.some_doc(Some(doc))
    }

    /// Attach documentation metadata to the field, optionally
    pub fn some_doc(mut self, some_doc: Option<&impl ToString>) -> Self {
        self.doc = some_doc.map(ToString::to_string);
        self
    }

    /// Mark the field as optional or not, depending on `opt`
    pub fn optional(mut self, opt: bool) -> Self {
        self.metadata.opt = opt;
        self
    }

    /// Mark the field as `not_exported` or not, depending on the argument
    pub fn not_exported(mut self, not_exported: bool) -> Self {
        self.metadata.not_exported = not_exported;
        self
    }

    /// Attach a contract to the field
    pub fn contract(mut self, contract: impl Into<Type<'ast>>) -> Self {
        self.contracts.push(contract.into());
        self
    }

    /// Attach multiple contracts to the field
    pub fn contracts<I>(mut self, contracts: I) -> Self
    where
        I: IntoIterator<Item = Type<'ast>>,
    {
        self.contracts.extend(contracts);
        self
    }

    /// Attach a type annotation to the field
    pub fn types(mut self, typ: impl Into<Type<'ast>>) -> Self {
        self.metadata.annotation.typ = Some(typ.into());
        self
    }

    /// Set the field's merge priority
    pub fn priority(mut self, priority: MergePriority) -> Self {
        self.metadata.priority = priority;
        self
    }

    /// Set the field's metadata all at once
    pub fn metadata(mut self, metadata: FieldMetadata<'ast>) -> Self {
        self.metadata = metadata;
        self
    }
}

impl<'ast> Field<'ast, Incomplete> {
    /// Construct an incomplete [`Field`] at a given path
    pub fn path<I, It>(path: It) -> Self
    where
        I: AsRef<str>,
        It: IntoIterator<Item = I>,
    {
        Field {
            state: Incomplete(),
            path: path.into_iter().map(|e| e.as_ref().into()).collect(),
            metadata: Default::default(),
            doc: None,
            contracts: Vec::new(),
        }
    }

    /// Construct an incomplete [`Field`] with a given name
    pub fn name(name: impl AsRef<str>) -> Self {
        Self::path([name])
    }

    /// Finalize the [`Field`] without setting a value
    pub fn no_value(self) -> Field<'ast, Complete<'ast>> {
        Field {
            state: Complete(None),
            path: self.path,
            metadata: self.metadata,
            doc: self.doc,
            contracts: self.contracts,
        }
    }

    /// Finalize the [`Field`] by setting its value
    pub fn value(self, value: impl Into<Ast<'ast>>) -> Field<'ast, Complete<'ast>> {
        Field {
            state: Complete(Some(value.into())),
            path: self.path,
            metadata: self.metadata,
            doc: self.doc,
            contracts: self.contracts,
        }
    }
}

impl<'ast> Field<'ast, Complete<'ast>> {
    /// Attach a finalized [`Field`] to a [`Record`]
    pub fn attach(self, alloc: &'ast AstAlloc, record: Record<'ast>) -> Record<'ast> {
        let value = self.state;

        let field = Field {
            state: record,
            path: self.path,
            metadata: self.metadata,
            doc: self.doc,
            contracts: self.contracts,
        };
        match value {
            Complete(Some(v)) => field.value(alloc, v),
            Complete(None) => field.no_value(alloc),
        }
    }
}

impl<'ast> Field<'ast, Record<'ast>> {
    /// Finalize the [`Field`] without setting a value
    pub fn no_value(mut self, alloc: &'ast AstAlloc) -> Record<'ast> {
        self.finalize_contracts(alloc);
        self.metadata.doc = self.doc.map(|s| alloc.alloc_str(&s));

        self.state.field_defs.push(record::FieldDef {
            path: alloc.alloc_many(
                self.path
                    .into_iter()
                    .map(|id| FieldPathElem::Ident(id.into())),
            ),
            metadata: self.metadata,
            value: None,
            pos: TermPos::None,
        });
        self.state
    }

    /// Finalize the [`Field`] by setting its a value
    pub fn value(mut self, alloc: &'ast AstAlloc, value: impl Into<Ast<'ast>>) -> Record<'ast> {
        self.finalize_contracts(alloc);
        self.metadata.doc = self.doc.map(|s| alloc.alloc_str(&s));

        self.state.field_defs.push(record::FieldDef {
            path: alloc.alloc_many(
                self.path
                    .into_iter()
                    .map(|id| FieldPathElem::Ident(id.into())),
            ),
            metadata: self.metadata,
            value: Some(value.into()),
            pos: TermPos::None,
        });

        self.state
    }

    /// Finalize contracts by consuming the vector `self.contracts`, allocating a fixed array and
    /// filling the field `self.metadata.annotation.contracts` with it. After this call,
    /// `self.contracts` is empty and shouldn't be used anymore (it will have no effect).
    fn finalize_contracts(&mut self, alloc: &'ast AstAlloc) {
        self.metadata.annotation.contracts = alloc.alloc_many(self.contracts.drain(..));
    }
}

/// A Nickel record being constructed
#[derive(Debug, Default)]
pub struct Record<'ast> {
    field_defs: Vec<record::FieldDef<'ast>>,
    open: bool,
}

impl<'ast> Record<'ast> {
    /// Make a new, empty record builder
    pub fn new() -> Self {
        Record::default()
    }

    /// Start constructing a field with the given name
    pub fn field(self, name: impl AsRef<str>) -> Field<'ast, Record<'ast>> {
        Field {
            state: self,
            path: vec![Ident::new(name)],
            metadata: Default::default(),
            doc: None,
            contracts: Vec::new(),
        }
    }

    /// Attach possibly multiple fields to this record
    pub fn fields<I, It>(mut self, alloc: &'ast AstAlloc, fields: It) -> Self
    where
        I: Into<Field<'ast, Complete<'ast>>>,
        It: IntoIterator<Item = I>,
    {
        for f in fields {
            self = f.into().attach(alloc, self)
        }
        self
    }

    /// Start constructing a field at the given path
    pub fn path<It, I>(self, path: It) -> Field<'ast, Record<'ast>>
    where
        I: AsRef<str>,
        It: IntoIterator<Item = I>,
    {
        Field {
            state: self,
            path: path.into_iter().map(|e| Ident::new(e)).collect(),
            metadata: Default::default(),
            doc: None,
            contracts: Vec::new(),
        }
    }

    /// Mark this record as "open" when used as a contract
    pub fn open(mut self) -> Self {
        self.open = true;
        self
    }

    /// Mark this record as "open" when used as a contract, depending on the value of `open`
    pub fn set_open(mut self, open: bool) -> Self {
        self.open = open;
        self
    }

    /// Finalize the record and turn it into a [`crate::term::RichTerm`]
    pub fn build(self, alloc: &'ast AstAlloc) -> Ast<'ast> {
        alloc
            .record(record::Record {
                field_defs: alloc.alloc_many(self.field_defs),
                open: self.open,
            })
            .into()
    }

    /// Creates a record from an iterator of finalized fields.
    ///
    /// We can't implement `FromIterator` for `Record` because we need to provide an additional
    /// allocator.
    pub fn from_iterator<I, It>(alloc: &'ast AstAlloc, fields: It) -> Self
    where
        I: Into<Field<'ast, Complete<'ast>>>,
        It: IntoIterator<Item = I>,
    {
        Record::new().fields(alloc, fields)
    }
}

/// Multi-ary application for types implementing `Into<Ast>`.
#[macro_export]
macro_rules! app {
    // We avoid a vec allocation for unary applications, which are relatively common.
    ( $alloc:expr, $f:expr , $arg:expr $(,)?) => {
            $crate::bytecode::ast::Ast::from(
                $alloc.app(
                    $crate::bytecode::ast::Ast::from($f),
                    std::iter::once($crate::bytecode::ast::Ast::from($arg))
                )
            )
    };
    ( $alloc:expr, $f:expr, $arg1:expr $(, $args:expr )+ $(,)?) => {
        {
            let args = vec![
                $crate::bytecode::ast::Ast::from($arg1)
                $(, $crate::bytecode::ast::Ast::from($args) )+
            ];

            $crate::bytecode::ast::Ast::from($alloc.app($crate::bytecode::ast::Ast::from($f), args))
        }
    };
}

#[macro_export]
/// Multi-ary application for types implementing `Into<RichTerm>`.
macro_rules! primop_app {
    // We avoid a vec allocation for unary primop applications, which are relatively common.
    ( $alloc:expr, $op:expr , $arg:expr $(,)?) => {
            $crate::bytecode::ast::Ast::from(
                $alloc.prim_op(
                    $op,
                    std::iter::once($crate::bytecode::ast::Ast::from($arg))
                )
            )
    };
    ( $alloc:expr, $op:expr, $arg1:expr $(, $args:expr )+ $(,)?) => {
        {
            let args = vec![
                $crate::bytecode::ast::Ast::from($arg1)
                $(, $crate::bytecode::ast::Ast::from($args) )+
            ];
            $crate::bytecode::ast::Ast::from($alloc.prim_op($op, args))
        }
    };
}

#[macro_export]
/// Multi argument function for types implementing `Into<Ident>` (for the identifiers), and
/// `Into<RichTerm>` for the body.
macro_rules! fun {
    // Auxiliary form for `fun!` where the arguments are clearly delimited syntactically by
    // brackets. The issue with the general interface of `fun!` is that it must extract its last
    // argument, the body of the function, with an arbitrary number of macro arguments before it.
    // This isn't easy to do because the macro parser can't backtrack. The bracketed form uses
    // recursion and a separate syntax for arguments to make it work.
    //
    // For example, calling `fun!(alloc, args=[], arg1, arg2, body)` will expand to `fun!(alloc,
    // args=[arg1, arg2], body)`, which is the base case that can be turned into a function. The
    // bracket part is used to accumulate the arguments before the body.
    ($alloc:expr, args=[ $( $args:expr, )+ ], $body:expr) => {
        {
            let args = vec![
                $($crate::bytecode::ast::pattern::Pattern::any($crate::identifier::LocIdent::from($args)), )+
            ];

            $crate::bytecode::ast::Ast::from(
                $alloc.fun(args, $crate::bytecode::ast::Ast::from($body))
            )
        }
    };
    // Recursive case for the bracketed form.
    ($alloc:expr, args=[ $( $args:expr, )* ], $next_arg:expr $(, $rest:expr )+) => {
        fun!($alloc, args=[ $( $args, )* $next_arg, ] $(, $rest )+)
    };
    // We avoid a vec allocation for unary functions, which are relatively common.
    ( $alloc:expr, $arg:expr, $body:expr $(,)?) => {
        $crate::bytecode::ast::Ast::from(
            $alloc.fun(
                std::iter::once($crate::bytecode::ast::pattern::Pattern::any($crate::identifier::LocIdent::from($arg))),
                $crate::bytecode::ast::Ast::from($body)
            )
        )
    };
    ( $alloc:expr, $arg1:expr, $arg2:expr $(, $rest:expr )+ $(,)?) => {
        fun!($alloc, args=[ $arg1, $arg2, ] $(, $rest )+)
    };
}

pub fn var<'ast>(id: impl Into<LocIdent>) -> Ast<'ast> {
    Ast::from(Node::Var(id.into()))
}

pub fn enum_tag<'ast>(tag: impl Into<LocIdent>) -> Ast<'ast> {
    Ast::from(Node::EnumVariant {
        tag: tag.into(),
        arg: None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::iter;

    fn build_simple_record<'ast, I, Id>(alloc: &'ast AstAlloc, fields: I, open: bool) -> Ast<'ast>
    where
        Id: Into<LocIdent>,
        I: IntoIterator<Item = (Id, Node<'ast>)>,
        I::IntoIter: ExactSizeIterator,
    {
        build_record(
            alloc,
            fields
                .into_iter()
                .map(|(id, node)| (id, Default::default(), Some(node))),
            open,
        )
    }

    fn build_record<'ast, I, Id>(alloc: &'ast AstAlloc, fields: I, open: bool) -> Ast<'ast>
    where
        Id: Into<LocIdent>,
        I: IntoIterator<Item = (Id, FieldMetadata<'ast>, Option<Node<'ast>>)>,
        I::IntoIter: ExactSizeIterator,
    {
        alloc
            .record(record::Record {
                field_defs: alloc.alloc_many(fields.into_iter().map(|(id, metadata, node)| {
                    record::FieldDef {
                        path: FieldPathElem::single_ident_path(alloc, id.into()),
                        value: node.map(Ast::from),
                        metadata,
                        pos: TermPos::None,
                    }
                })),
                open,
            })
            .into()
    }

    #[test]
    fn trivial() {
        let alloc = AstAlloc::new();

        let ast: Ast = Record::new()
            .field("foo")
            .value(&alloc, alloc.string("bar"))
            .build(&alloc);

        assert_eq!(
            ast,
            build_simple_record(&alloc, vec![("foo", alloc.string("bar"))], false)
        );
    }

    #[test]
    fn from_iter() {
        let alloc = AstAlloc::new();

        let ast: Ast = Record::from_iterator(
            &alloc,
            [
                Field::name("foo").value(Node::Null),
                Field::name("bar").value(Node::Null),
            ],
        )
        .build(&alloc);

        assert_eq!(
            ast,
            build_simple_record(
                &alloc,
                vec![("foo", Node::Null), ("bar", Node::Null),],
                false
            )
        );
    }

    #[test]
    fn some_doc() {
        let alloc = AstAlloc::new();

        let ast: Ast = Record::from_iterator(
            &alloc,
            [
                Field::name("foo").some_doc(Some("foo")).no_value(),
                Field::name("bar").some_doc(None as Option<&str>).no_value(),
                Field::name("baz").doc("baz").no_value(),
            ],
        )
        .build(&alloc);

        assert_eq!(
            ast,
            build_record(
                &alloc,
                vec![
                    (
                        "foo",
                        FieldMetadata {
                            doc: Some(Rc::from("foo")),
                            ..Default::default()
                        },
                        None
                    ),
                    ("bar", Default::default(), None),
                    (
                        "baz",
                        FieldMetadata {
                            doc: Some(Rc::from("baz")),
                            ..Default::default()
                        },
                        None,
                    )
                ],
                false,
            )
        );
    }

    #[test]
    fn fields() {
        let alloc = AstAlloc::new();

        let ast: Ast = Record::new()
            .fields(
                &alloc,
                [
                    Field::name("foo").value(alloc.string("foo")),
                    Field::name("bar").value(alloc.string("bar")),
                ],
            )
            .build(&alloc);

        assert_eq!(
            ast,
            build_simple_record(
                &alloc,
                vec![("foo", alloc.string("foo")), ("bar", alloc.string("bar")),],
                false,
            )
        );
    }

    #[test]
    fn fields_metadata() {
        let alloc = AstAlloc::new();

        let ast: Ast = Record::new()
            .fields(
                &alloc,
                [
                    Field::name("foo").optional(true).no_value(),
                    Field::name("bar").optional(true).no_value(),
                ],
            )
            .build(&alloc);

        assert_eq!(
            ast,
            build_record(
                &alloc,
                vec![
                    (
                        "foo",
                        FieldMetadata {
                            opt: true,
                            ..Default::default()
                        },
                        None,
                    ),
                    (
                        "bar",
                        FieldMetadata {
                            opt: true,
                            ..Default::default()
                        },
                        None,
                    ),
                ],
                false,
            )
        );
    }

    #[test]
    fn overriding() {
        let alloc = AstAlloc::new();

        let ast: Ast = Record::new()
            .path(vec!["terraform", "required_providers"])
            .value(
                &alloc,
                Record::from_iterator(
                    &alloc,
                    [
                        Field::name("foo").value(Node::Null),
                        Field::name("bar").value(Node::Null),
                    ],
                )
                .build(&alloc),
            )
            .path(vec!["terraform", "required_providers", "foo"])
            .value(&alloc, alloc.string("hello world!"))
            .build(&alloc);

        eprintln!("{:?}", ast);

        assert_eq!(
            ast,
            alloc
                .record(record::Record {
                    field_defs: alloc.alloc_many(vec![
                        record::FieldDef {
                            path: alloc.alloc_many(vec![
                                FieldPathElem::Ident("terraform".into()),
                                FieldPathElem::Ident("required_providers".into())
                            ]),
                            metadata: Default::default(),
                            value: Some(
                                alloc
                                    .record(record::Record {
                                        field_defs: alloc.alloc_many(vec![
                                            record::FieldDef {
                                                path: FieldPathElem::single_ident_path(
                                                    &alloc,
                                                    "foo".into()
                                                ),
                                                metadata: Default::default(),
                                                value: Some(Node::Null.into()),
                                                pos: TermPos::None,
                                            },
                                            record::FieldDef {
                                                path: FieldPathElem::single_ident_path(
                                                    &alloc,
                                                    "bar".into()
                                                ),
                                                metadata: Default::default(),
                                                value: Some(Node::Null.into()),
                                                pos: TermPos::None,
                                            },
                                        ]),
                                        open: false,
                                    })
                                    .into()
                            ),
                            pos: TermPos::None,
                        },
                        record::FieldDef {
                            path: alloc.alloc_many(vec![
                                FieldPathElem::Ident("terraform".into()),
                                FieldPathElem::Ident("required_providers".into()),
                                FieldPathElem::Ident("foo".into())
                            ]),
                            metadata: Default::default(),
                            value: Some(alloc.string("hello world!").into()),
                            pos: TermPos::None,
                        }
                    ]),
                    open: false,
                })
                .into()
        );
    }

    #[test]
    fn open_record() {
        let alloc = AstAlloc::new();

        let ast: Ast = Record::new().open().build(&alloc);

        assert_eq!(ast, alloc.record(record::Record::empty().open()).into());
    }

    #[test]
    fn prio_metadata() {
        let alloc = AstAlloc::new();

        let ast: Ast = Record::new()
            .field("foo")
            .priority(MergePriority::Top)
            .no_value(&alloc)
            .build(&alloc);

        assert_eq!(
            ast,
            build_record(
                &alloc,
                iter::once((
                    "foo",
                    FieldMetadata {
                        priority: MergePriority::Top,
                        ..Default::default()
                    },
                    None,
                )),
                false
            )
        );
    }

    #[test]
    fn contract() {
        let alloc = AstAlloc::new();

        let ast: Ast = Record::new()
            .field("foo")
            .contract(TypeF::String)
            .no_value(&alloc)
            .build(&alloc);

        assert_eq!(
            ast,
            build_record(
                &alloc,
                iter::once((
                    "foo",
                    record::FieldMetadata::from(Annotation {
                        contracts: alloc.alloc_singleton(Type {
                            typ: TypeF::String,
                            pos: TermPos::None
                        }),
                        ..Default::default()
                    }),
                    None
                )),
                false
            )
        );
    }

    #[test]
    fn exercise_metadata() {
        let alloc = AstAlloc::new();

        let ast: Ast = Record::new()
            .field("foo")
            .priority(MergePriority::Bottom)
            .doc("foo?")
            .contract(TypeF::String)
            .types(TypeF::Number)
            .optional(true)
            .not_exported(true)
            .no_value(&alloc)
            .build(&alloc);

        assert_eq!(
            ast,
            build_record(
                &alloc,
                iter::once((
                    "foo",
                    FieldMetadata {
                        doc: Some(Rc::from("foo?")),
                        opt: true,
                        priority: MergePriority::Bottom,
                        not_exported: true,
                        annotation: Annotation {
                            typ: Some(Type {
                                typ: TypeF::Number,
                                pos: TermPos::None,
                            }),
                            contracts: alloc.alloc_singleton(Type {
                                typ: TypeF::String,
                                pos: TermPos::None
                            }),
                        },
                    },
                    None,
                )),
                Default::default()
            )
        );
    }
}
