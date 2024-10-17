use super::{Annotation, Ast};

use crate::{
    combine::Combine,
    identifier::LocIdent,
    term::MergePriority,
};

use std::rc::Rc;

/// Additional attributes for record.
#[derive(Debug, Default, Eq, PartialEq, Copy, Clone)]
pub struct RecordAttrs {
    /// If the record is an open record, ie ending with `..`. Open records have a different
    /// behavior when used as a record contract: they allow additional fields to be present.
    pub open: bool,
}

impl Combine for RecordAttrs {
    fn combine(left: Self, right: Self) -> Self {
        RecordAttrs {
            open: left.open || right.open,
        }
    }
}

/// The metadata attached to record fields.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct FieldMetadata<'ast> {
    /// The documentation of the field. This is allocated once and for all and shared through a
    /// reference-counted pointer.
    pub doc: Option<Rc<str>>,
    /// Potential type and contract annotations.
    pub annotation: Annotation<'ast>,
    /// If the field is optional.
    pub opt: bool,
    /// If the field should be skipped during serialization.
    pub not_exported: bool,
    /// The merge priority.
    pub priority: MergePriority,
}

impl<'ast> FieldMetadata<'ast> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn is_empty(&self) -> bool {
        self.doc.is_none()
            && self.annotation.is_empty()
            && !self.opt
            && !self.not_exported
            && matches!(self.priority, MergePriority::Neutral)
    }
}

impl<'ast> From<Annotation<'ast>> for FieldMetadata<'ast> {
    fn from(annotation: Annotation<'ast>) -> Self {
        FieldMetadata {
            annotation,
            ..Default::default()
        }
    }
}

/// A record field with its metadata.
#[derive(Clone, Default, PartialEq, Debug)]
pub struct Field<'ast> {
    /// The value is optional because record field may not have a definition (e.g. optional fields).
    pub value: Option<Ast<'ast>>,
    /// The metadata attached to the field.
    pub metadata: FieldMetadata<'ast>,
}

impl<'ast> From<Ast<'ast>> for Field<'ast> {
    fn from(ast: Ast<'ast>) -> Self {
        Field {
            value: Some(ast),
            ..Default::default()
        }
    }
}

impl<'ast> From<Annotation<'ast>> for Field<'ast> {
    fn from(ann: Annotation<'ast>) -> Self {
        Field::from(FieldMetadata::from(ann))
    }
}

impl<'ast> From<FieldMetadata<'ast>> for Field<'ast> {
    fn from(metadata: FieldMetadata<'ast>) -> Self {
        Field {
            metadata,
            ..Default::default()
        }
    }
}

/// The base structure of a Nickel record.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Record<'ast> {
    /// Fields whose names are known statically.
    pub stat_fields: &'ast [(LocIdent, Field<'ast>)],
    /// Fields whose names is a Nickel expression computed at runtime.
    pub dyn_fields: &'ast [(Ast<'ast>, Field<'ast>)],
    /// If the record is open, i.e. if it ended with `..`.
    pub open: bool,
}

impl<'ast> Record<'ast> {
    /// A record with no fields and the default set of attributes.
    pub fn empty() -> Self {
        Default::default()
    }
}
