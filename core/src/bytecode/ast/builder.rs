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
//!     .not_exported()
//!     .value(alloc.string("foo"));
//!
//! let record = record
//!     .field("bar")
//!     .value(Node::Number(42.into()))
//!     .build(&alloc);
//!
//! // Gives {foo | doc "foo?" | not_exported | defaut = "foo", bar = 42}
//! ```
//!
//! This modules also offers a simpler interface for basic values where all arguments can be
//! provided at once, to avoid useless intermediate allocations.
use indexmap::IndexMap;

use crate::identifier::{Ident, LocIdent};

use super::{
    combine::Combine,
    record::{FieldMetadata, MergePriority},
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
    /// This field store different piece of information depending on the state of this field:
    ///
    /// - It's empty for an incomplete field (`()`).
    /// - When a value (or `None`) has ben set, `State` becomes [Complete] and `state` store the
    ///   optional value of this field.
    /// - Finally, when the field has been attached to a record, `state` store the parent record.
    state: State,
    path: StaticPath,
    metadata: FieldMetadata<'ast>,
    /// As we might build contract element by element, we can't rely on
    /// `metadata.annotation.contracts`, which is a fixed array.
    ///
    /// We accumulate them in this field instead and keep `metadata.annotation.contracts` empty
    /// until finalization, where we finally allocate it.
    contracts: Vec<Type<'ast>>,
}

impl<'ast, A> Field<'ast, A> {
    /// Attach documentation metadata to the field
    pub fn doc(self, doc: impl AsRef<str>) -> Self {
        self.some_doc(Some(doc))
    }

    /// Attach documentation metadata to the field, optionally
    pub fn some_doc(mut self, some_doc: Option<impl AsRef<str>>) -> Self {
        self.metadata.doc = some_doc.map(|doc| Rc::from(doc.as_ref()));
        self
    }

    /// Mark the field as optional
    pub fn optional(self) -> Self {
        self.set_optional(true)
    }

    /// Mark the field as optional or not, depending on `opt`
    pub fn set_optional(mut self, opt: bool) -> Self {
        self.metadata.opt = opt;
        self
    }

    /// Mark the field as `not_exported`
    pub fn not_exported(self) -> Self {
        self.set_not_exported(true)
    }

    /// Mark the field as `not_exported` or not, depending on the argument
    pub fn set_not_exported(mut self, not_exported: bool) -> Self {
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
            contracts: self.contracts,
        }
    }

    /// Finalize the [`Field`] by setting its value
    pub fn value(self, value: impl Into<Ast<'ast>>) -> Field<'ast, Complete<'ast>> {
        Field {
            state: Complete(Some(value.into())),
            path: self.path,
            metadata: self.metadata,
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

        self.state.fields.push((
            self.path,
            record::Field {
                metadata: self.metadata,
                ..Default::default()
            },
        ));
        self.state
    }

    /// Finalize the [`Field`] by setting its a value
    pub fn value(mut self, alloc: &'ast AstAlloc, value: impl Into<Ast<'ast>>) -> Record<'ast> {
        self.finalize_contracts(alloc);

        self.state.fields.push((
            self.path,
            record::Field {
                value: Some(value.into()),
                metadata: self.metadata,
            },
        ));
        self.state
    }

    /// Finalize contracts by consuming the vector `self.contracts`, allocating a fixed array and
    /// filling the field `self.metadata.annotation.contracts` with it. After this call,
    /// `self.contracts` is empty and shouldn't be used anymore (it will have no effect).
    fn finalize_contracts(&mut self, alloc: &'ast AstAlloc) {
        self.metadata.annotation.contracts = alloc.types(self.contracts.drain(..));
    }
}

/// A Nickel record being constructed
#[derive(Debug, Default)]
pub struct Record<'ast> {
    fields: Vec<(StaticPath, record::Field<'ast>)>,
    open: bool,
}

fn elaborate_field_path<'ast, I>(
    alloc: &'ast AstAlloc,
    path: I,
    content: record::Field<'ast>,
) -> (LocIdent, record::Field<'ast>)
where
    I: IntoIterator<Item = Ident>,
    I::IntoIter: DoubleEndedIterator,
{
    let mut it = path.into_iter();
    let fst = it.next().unwrap();

    let content = it.rev().fold(content, |acc, id| {
        let record = alloc.record_data(
            std::iter::once((LocIdent::from(id), acc)),
            std::iter::empty(),
            false,
        );

        record::Field::from(Ast {
            node: Node::Record(record),
            pos: TermPos::None,
        })
    });

    (fst.into(), content)
}

fn build_record<'ast, I>(alloc: &'ast AstAlloc, fields: I, open: bool) -> Node<'ast>
where
    I: IntoIterator<Item = (LocIdent, record::Field<'ast>)>,
{
    fn merge_fields<'ast>(
        alloc: &'ast AstAlloc,
        field1: record::Field<'ast>,
        field2: record::Field<'ast>,
    ) -> record::Field<'ast> {
        let value = match (field1.value, field2.value) {
            (Some(t1), Some(t2)) => Some(
                alloc
                    .prim_op(primop::PrimOp::Merge(Default::default()), [t1, t2])
                    .into(),
            ),
            (Some(t), None) | (None, Some(t)) => Some(t),
            (None, None) => None,
        };
        let metadata = Combine::combine(alloc, field1.metadata, field2.metadata);
        record::Field { value, metadata }
    }

    let mut static_fields = IndexMap::new();
    for (id, t) in fields {
        match static_fields.entry(id) {
            indexmap::map::Entry::Occupied(mut occupied) => {
                let prev = occupied.insert(record::Field::default());
                occupied.insert(merge_fields(alloc, prev, t));
            }
            indexmap::map::Entry::Vacant(vacant) => {
                vacant.insert(t);
            }
        }
    }

    Node::Record(alloc.record_data(static_fields, std::iter::empty(), open))
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
        let elaborated = self
            .fields
            .into_iter()
            .map(|(path, field)| elaborate_field_path(alloc, path, field));

        build_record(alloc, elaborated, self.open).into()
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

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::iter;

    fn node_as_field(node: Node<'_>) -> record::Field<'_> {
        record::Field::from(Ast {
            node,
            pos: TermPos::None,
        })
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
            build_record(
                &alloc,
                vec![("foo".into(), node_as_field(alloc.string("bar")))],
                false,
            )
            .into()
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
            build_record(
                &alloc,
                vec![
                    ("foo".into(), node_as_field(Node::Null)),
                    ("bar".into(), node_as_field(Node::Null)),
                ],
                false
            )
            .into()
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
                        "foo".into(),
                        record::Field::from(FieldMetadata {
                            doc: Some(Rc::from("foo")),
                            ..Default::default()
                        })
                    ),
                    ("bar".into(), Default::default()),
                    (
                        "baz".into(),
                        record::Field::from(FieldMetadata {
                            doc: Some(Rc::from("baz")),
                            ..Default::default()
                        })
                    )
                ],
                false,
            )
            .into()
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
            build_record(
                &alloc,
                vec![
                    ("foo".into(), node_as_field(alloc.string("foo"))),
                    ("bar".into(), node_as_field(alloc.string("bar"))),
                ],
                false,
            )
            .into()
        );
    }

    #[test]
    fn fields_metadata() {
        let alloc = AstAlloc::new();

        let ast: Ast = Record::new()
            .fields(
                &alloc,
                [
                    Field::name("foo").optional().no_value(),
                    Field::name("bar").optional().no_value(),
                ],
            )
            .build(&alloc);

        assert_eq!(
            ast,
            build_record(
                &alloc,
                vec![
                    (
                        "foo".into(),
                        record::Field::from(FieldMetadata {
                            opt: true,
                            ..Default::default()
                        })
                    ),
                    (
                        "bar".into(),
                        record::Field::from(FieldMetadata {
                            opt: true,
                            ..Default::default()
                        }),
                    ),
                ],
                false,
            )
            .into()
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
            build_record(
                &alloc,
                vec![
                    elaborate_field_path(
                        &alloc,
                        vec!["terraform".into(), "required_providers".into()],
                        node_as_field(build_record(
                            &alloc,
                            vec![
                                ("foo".into(), node_as_field(Node::Null)),
                                ("bar".into(), node_as_field(Node::Null))
                            ],
                            false
                        ))
                    ),
                    elaborate_field_path(
                        &alloc,
                        vec![
                            "terraform".into(),
                            "required_providers".into(),
                            "foo".into(),
                        ],
                        node_as_field(alloc.string("hello world!"))
                    )
                ],
                false
            )
            .into()
        );
    }

    #[test]
    fn open_record() {
        let alloc = AstAlloc::new();

        let ast: Ast = Record::new().open().build(&alloc);

        assert_eq!(ast, build_record(&alloc, iter::empty(), true).into());
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
                    "foo".into(),
                    record::Field::from(FieldMetadata {
                        priority: MergePriority::Top,
                        ..Default::default()
                    })
                )),
                false
            )
            .into()
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
                    "foo".into(),
                    record::Field::from(Annotation {
                        contracts: alloc.types(std::iter::once(Type {
                            typ: TypeF::String,
                            pos: TermPos::None
                        })),
                        ..Default::default()
                    })
                )),
                false
            )
            .into()
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
            .optional()
            .not_exported()
            .no_value(&alloc)
            .build(&alloc);

        assert_eq!(
            ast,
            build_record(
                &alloc,
                iter::once((
                    "foo".into(),
                    record::Field::from(FieldMetadata {
                        doc: Some(Rc::from("foo?")),
                        opt: true,
                        priority: MergePriority::Bottom,
                        not_exported: true,
                        annotation: Annotation {
                            typ: Some(Type {
                                typ: TypeF::Number,
                                pos: TermPos::None,
                            }),
                            contracts: alloc.types(iter::once(Type {
                                typ: TypeF::String,
                                pos: TermPos::None
                            })),
                        },
                    }),
                )),
                Default::default()
            )
            .into()
        );
    }
}
