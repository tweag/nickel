//! A builder interface for producing Nickel records.
//!
//! Using this module, you can define Nickel records using a builder style. For example:
//!
//! ```rust
//! # use nickel_lang_core::term::{MergePriority, Term, RichTerm, make::builder::Record};
//! let b = Record::new()
//!     .field("foo")
//!     .priority(MergePriority::Bottom)
//!     .doc("foo?")
//!     .not_exported()
//!     .value(Term::Str("foo".into()));
//! let t : RichTerm = b
//!     .field("bar")
//!     .value(Term::Num(42.into()))
//!     .into();
//! ```
//!
use indexmap::IndexMap;

use crate::{
    combine::Combine,
    identifier::{Ident, LocIdent},
    label::Label,
    term::{
        make::op2,
        record::{self, FieldMetadata, RecordAttrs, RecordData},
        BinaryOp, LabeledType, MergePriority, RichTerm, Term,
    },
    typ::Type,
};

type StaticPath = Vec<Ident>;

/// Typestate style tag for `Field`s that are not yet completely specified
pub struct Incomplete();

/// Typestate style tag for `Field`s that have been finalized
pub struct Complete(Option<RichTerm>);

/// A Nickel record field being constructed
#[derive(Debug)]
pub struct Field<RB> {
    record: RB,
    path: StaticPath,
    metadata: FieldMetadata,
}

impl<A> Field<A> {
    /// Attach documentation metadata to the field
    pub fn doc(self, doc: impl AsRef<str>) -> Self {
        self.some_doc(Some(doc))
    }

    /// Attach documentation metadata to the field, optionally
    pub fn some_doc(mut self, some_doc: Option<impl AsRef<str>>) -> Self {
        self.metadata.doc = some_doc.map(|d| d.as_ref().to_owned());
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
    pub fn contract(mut self, contract: impl Into<Type>) -> Self {
        self.metadata.annotation.contracts.push(LabeledType {
            typ: contract.into(),
            label: Default::default(),
        });
        self
    }

    /// Attach multiple contracts to the field
    pub fn contracts<I>(mut self, contracts: I) -> Self
    where
        I: IntoIterator<Item = Type>,
    {
        self.metadata
            .annotation
            .contracts
            .extend(contracts.into_iter().map(|c| LabeledType {
                typ: c,
                label: Default::default(),
            }));
        self
    }

    /// Attach a type annotation to the field
    pub fn types(mut self, t: impl Into<Type>) -> Self {
        self.metadata.annotation.typ = Some(LabeledType {
            typ: t.into(),
            label: Default::default(),
        });
        self
    }

    /// Set the field's merge priority
    pub fn priority(mut self, priority: MergePriority) -> Self {
        self.metadata.priority = priority;
        self
    }

    /// Set the field's metadata all at once
    pub fn metadata(mut self, metadata: FieldMetadata) -> Self {
        self.metadata = metadata;
        self
    }
}

impl Field<Incomplete> {
    /// Construct an incomplete [`Field`] at a given path
    pub fn path<I, It>(path: It) -> Self
    where
        I: AsRef<str>,
        It: IntoIterator<Item = I>,
    {
        Field {
            record: Incomplete(),
            path: path.into_iter().map(|e| e.as_ref().into()).collect(),
            metadata: Default::default(),
        }
    }

    /// Construct an incomplete [`Field`] with a given name
    pub fn name(name: impl AsRef<str>) -> Self {
        Self::path([name])
    }

    /// Finalize the [`Field`] without setting a value
    pub fn no_value(self) -> Field<Complete> {
        Field {
            record: Complete(None),
            path: self.path,
            metadata: self.metadata,
        }
    }

    /// Finalize the [`Field`] by setting its value
    pub fn value(self, value: impl Into<RichTerm>) -> Field<Complete> {
        Field {
            record: Complete(Some(value.into())),
            path: self.path,
            metadata: self.metadata,
        }
    }
}

impl Field<Complete> {
    /// Attach a finalized [`Field`] to a [`Record`]
    pub fn with_record(self, r: Record) -> Record {
        let v = self.record;
        let f = Field {
            record: r,
            path: self.path,
            metadata: self.metadata,
        };
        match v {
            Complete(Some(v)) => f.value(v),
            Complete(None) => f.no_value(),
        }
    }
}

impl Field<Record> {
    /// Finalize the [`Field`] without setting a value
    pub fn no_value(mut self) -> Record {
        self.record.fields.push((
            self.path,
            record::Field {
                metadata: self.metadata,
                ..Default::default()
            },
        ));
        self.record
    }

    /// Finalize the [`Field`] by setting its a value
    pub fn value(mut self, value: impl Into<RichTerm>) -> Record {
        self.record.fields.push((
            self.path,
            record::Field {
                value: Some(value.into()),
                metadata: self.metadata,
                ..Default::default()
            },
        ));
        self.record
    }
}

/// A Nickel record being constructed
#[derive(Debug)]
pub struct Record {
    fields: Vec<(StaticPath, record::Field)>,
    attrs: RecordAttrs,
}

fn elaborate_field_path<I>(path: I, content: record::Field) -> (LocIdent, record::Field)
where
    I: IntoIterator<Item = Ident>,
    I::IntoIter: DoubleEndedIterator,
{
    let mut it = path.into_iter();
    let fst = it.next().unwrap();

    let content = it.rev().fold(content, |acc, id| {
        record::Field::from(RichTerm::from(Term::Record(RecordData {
            fields: [(LocIdent::from(id), acc)].into(),
            ..Default::default()
        })))
    });

    (fst.into(), content)
}

fn build_record<I>(fields: I, attrs: RecordAttrs) -> Term
where
    I: IntoIterator<Item = (LocIdent, record::Field)>,
{
    fn merge_fields(field1: record::Field, field2: record::Field) -> record::Field {
        let value = match (field1.value, field2.value) {
            (Some(t1), Some(t2)) => Some(op2(BinaryOp::Merge(Label::default().into()), t1, t2)),
            (Some(t), None) | (None, Some(t)) => Some(t),
            (None, None) => None,
        };
        let metadata = Combine::combine(field1.metadata, field2.metadata);
        record::Field {
            value,
            metadata,
            pending_contracts: vec![],
        }
    }

    let mut static_fields = IndexMap::new();
    for (id, t) in fields {
        match static_fields.entry(id) {
            indexmap::map::Entry::Occupied(mut occupied) => {
                let prev = occupied.insert(record::Field::default());
                occupied.insert(merge_fields(prev, t));
            }
            indexmap::map::Entry::Vacant(vacant) => {
                vacant.insert(t);
            }
        }
    }
    Term::Record(RecordData::new(static_fields, attrs, None))
}

impl Record {
    /// Make a new, empty record builder
    pub fn new() -> Self {
        Record {
            fields: vec![],
            attrs: Default::default(),
        }
    }

    /// Start constructing a field with the given name
    pub fn field(self, name: impl AsRef<str>) -> Field<Record> {
        Field {
            record: self,
            path: vec![Ident::new(name)],
            metadata: Default::default(),
        }
    }

    /// Attach possibly multiple fields to this record
    pub fn fields<I, It>(mut self, fields: It) -> Self
    where
        I: Into<Field<Complete>>,
        It: IntoIterator<Item = I>,
    {
        for f in fields {
            self = f.into().with_record(self)
        }
        self
    }

    /// Start constructing a field at the given path
    pub fn path<It, I>(self, path: It) -> Field<Record>
    where
        I: AsRef<str>,
        It: IntoIterator<Item = I>,
    {
        Field {
            record: self,
            path: path.into_iter().map(|e| Ident::new(e)).collect(),
            metadata: Default::default(),
        }
    }

    /// Set all record attributes at once
    pub fn attrs(mut self, attrs: RecordAttrs) -> Self {
        self.attrs = attrs;
        self
    }

    // Clippy correctly observes that `open` is the only field in `RecordAttrs`.
    // Nevertheless, we want to do the record update. That way we can be
    // somewhat futureproof in case new fields are added to `RecordAttrs` in
    // Nickel.
    #[allow(clippy::needless_update)]
    /// Mark this record as "open" when used as a contract
    pub fn open(mut self) -> Self {
        self.attrs = RecordAttrs {
            open: true,
            ..self.attrs
        };
        self
    }

    // See `open` for comments on the clippy directive
    #[allow(clippy::needless_update)]
    /// Mark this record as "open" when used as a contract, depending on the value of `open`
    pub fn set_open(mut self, open: bool) -> Self {
        self.attrs = RecordAttrs { open, ..self.attrs };
        self
    }

    /// Finalize the record and turn it into a [`crate::term::RichTerm`]
    pub fn build(self) -> RichTerm {
        let elaborated = self
            .fields
            .into_iter()
            .map(|(path, rt)| elaborate_field_path(path, rt));
        build_record(elaborated, self.attrs).into()
    }
}

impl Default for Record {
    fn default() -> Self {
        Self::new()
    }
}

impl<I, It> From<It> for Record
where
    I: Into<Field<Complete>>,
    It: IntoIterator<Item = I>,
{
    fn from(f: It) -> Self {
        Record::new().fields(f)
    }
}

impl From<Record> for RichTerm {
    fn from(val: Record) -> Self {
        val.build()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        position::TermPos,
        term::{RichTerm, TypeAnnotation},
        typ::{Type, TypeF},
    };

    use pretty_assertions::assert_eq;

    use super::*;

    fn term(t: Term) -> record::Field {
        record::Field::from(RichTerm::from(t))
    }

    #[test]
    fn trivial() {
        let t: RichTerm = Record::new()
            .field("foo")
            .value(Term::Str("bar".into()))
            .into();
        assert_eq!(
            t,
            build_record(
                vec![("foo".into(), term(Term::Str("bar".to_owned().into())))],
                Default::default()
            )
            .into()
        );
    }

    #[test]
    fn from_iter() {
        let t: RichTerm = Record::from([
            Field::name("foo").value(Term::Null),
            Field::name("bar").value(Term::Null),
        ])
        .into();
        assert_eq!(
            t,
            build_record(
                vec![
                    ("foo".into(), term(Term::Null)),
                    ("bar".into(), term(Term::Null)),
                ],
                Default::default()
            )
            .into()
        );
    }

    #[test]
    fn some_doc() {
        let t: RichTerm = Record::from([
            Field::name("foo").some_doc(Some("foo")).no_value(),
            Field::name("bar").some_doc(None as Option<&str>).no_value(),
            Field::name("baz").doc("baz").no_value(),
        ])
        .into();
        assert_eq!(
            t,
            build_record(
                vec![
                    (
                        "foo".into(),
                        record::Field::from(FieldMetadata {
                            doc: Some("foo".into()),
                            ..Default::default()
                        })
                    ),
                    ("bar".into(), Default::default()),
                    (
                        "baz".into(),
                        record::Field::from(FieldMetadata {
                            doc: Some("baz".into()),
                            ..Default::default()
                        })
                    )
                ],
                Default::default()
            )
            .into()
        );
    }

    #[test]
    fn fields() {
        let t: RichTerm = Record::new()
            .fields([
                Field::name("foo").value(Term::Str("foo".into())),
                Field::name("bar").value(Term::Str("bar".into())),
            ])
            .into();
        assert_eq!(
            t,
            build_record(
                vec![
                    ("foo".into(), term(Term::Str("foo".into()))),
                    ("bar".into(), term(Term::Str("bar".into()))),
                ],
                Default::default()
            )
            .into()
        );
    }

    #[test]
    fn fields_metadata() {
        let t: RichTerm = Record::new()
            .fields([
                Field::name("foo").optional().no_value(),
                Field::name("bar").optional().no_value(),
            ])
            .into();
        assert_eq!(
            t,
            build_record(
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
                Default::default()
            )
            .into()
        );
    }

    #[test]
    fn overriding() {
        let t: RichTerm = Record::new()
            .path(vec!["terraform", "required_providers"])
            .value(Record::from([
                Field::name("foo").value(Term::Null),
                Field::name("bar").value(Term::Null),
            ]))
            .path(vec!["terraform", "required_providers", "foo"])
            .value(Term::Str("hello world!".into()))
            .into();
        eprintln!("{:?}", t);
        assert_eq!(
            t,
            build_record(
                vec![
                    elaborate_field_path(
                        vec!["terraform".into(), "required_providers".into()],
                        term(build_record(
                            vec![
                                ("foo".into(), term(Term::Null)),
                                ("bar".into(), term(Term::Null))
                            ],
                            Default::default()
                        ))
                    ),
                    elaborate_field_path(
                        vec![
                            "terraform".into(),
                            "required_providers".into(),
                            "foo".into(),
                        ],
                        term(Term::Str("hello world!".into()))
                    )
                ],
                Default::default()
            )
            .into()
        );
    }

    #[test]
    fn open_record() {
        let t: RichTerm = Record::new().open().into();
        assert_eq!(
            t,
            build_record(
                vec![],
                RecordAttrs {
                    open: true,
                    ..Default::default()
                }
            )
            .into()
        );
    }

    #[test]
    fn prio_metadata() {
        let t: RichTerm = Record::new()
            .field("foo")
            .priority(MergePriority::Top)
            .no_value()
            .into();
        assert_eq!(
            t,
            build_record(
                vec![(
                    "foo".into(),
                    record::Field::from(FieldMetadata {
                        priority: MergePriority::Top,
                        ..Default::default()
                    })
                )],
                Default::default()
            )
            .into()
        );
    }

    #[test]
    fn contract() {
        let t: RichTerm = Record::new()
            .field("foo")
            .contract(TypeF::String)
            .no_value()
            .into();
        assert_eq!(
            t,
            build_record(
                vec![(
                    "foo".into(),
                    record::Field::from(TypeAnnotation {
                        contracts: vec![LabeledType {
                            typ: Type {
                                typ: TypeF::String,
                                pos: TermPos::None
                            },
                            label: Default::default()
                        }],
                        ..Default::default()
                    })
                )],
                Default::default()
            )
            .into()
        );
    }

    #[test]
    fn exercise_metadata() {
        let t: RichTerm = Record::new()
            .field("foo")
            .priority(MergePriority::Bottom)
            .doc("foo?")
            .contract(TypeF::String)
            .types(TypeF::Number)
            .optional()
            .not_exported()
            .no_value()
            .into();
        assert_eq!(
            t,
            build_record(
                vec![(
                    "foo".into(),
                    record::Field::from(FieldMetadata {
                        doc: Some("foo?".into()),
                        opt: true,
                        priority: MergePriority::Bottom,
                        not_exported: true,
                        annotation: TypeAnnotation {
                            typ: Some(LabeledType {
                                typ: Type {
                                    typ: TypeF::Number,
                                    pos: TermPos::None
                                },
                                label: Default::default()
                            }),
                            contracts: vec![LabeledType {
                                typ: Type {
                                    typ: TypeF::String,
                                    pos: TermPos::None
                                },
                                label: Default::default()
                            }],
                        },
                    }),
                )],
                Default::default()
            )
            .into()
        );
    }
}
