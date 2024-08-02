use super::*;
use crate::{
    combine::Combine,
    error::EvalError,
    identifier::{Ident, LocIdent},
    label::Label,
};
use std::{collections::HashSet, rc::Rc};

/// Additional attributes for record.
#[derive(Debug, Default, Eq, PartialEq, Copy, Clone)]
pub struct RecordAttrs {
    /// If the record is an open record, ie ending with `..`. Open records have a different
    /// behavior when used as a record contract: they allow additional fields to be present.
    pub open: bool,
    /// A record is closurized when each element is a [crate::term::Term::Closure] or a constant.
    /// Note that closurization is _required_ for evaluated records that are passed to e.g.
    /// [crate::eval::merge::merge] or other primitive operators. Non-closurized record are mostly
    /// produced by the parser or when building Nickel terms programmatically. When encountered by
    /// the main eval loop, they are closurized and the flag is set accordingly.
    ///
    /// Ideally, we would have a different AST representation for evaluation, where records would
    /// be closurized by construction. In the meantime, while we need to cope with a unique AST
    /// across the whole pipeline, we use this flag.
    pub closurized: bool,
}

impl RecordAttrs {
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the `closurized` flag to true and return the updated attributes.
    pub fn closurized(mut self) -> Self {
        self.closurized = true;
        self
    }
}

impl Combine for RecordAttrs {
    fn combine(left: Self, right: Self) -> Self {
        RecordAttrs {
            open: left.open || right.open,
            closurized: left.closurized && right.closurized,
        }
    }
}

/// Dependencies of a field or a cache element over the other recursive fields of a recursive
/// record.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FieldDeps {
    /// The set of dependencies is fixed and has been computed. When attached to an element, an
    /// empty set of dependency means that the element isn't revertible, but standard.
    Known(Rc<HashSet<Ident>>),

    /// The element is revertible, but the set of dependencies hasn't been computed. In that case,
    /// the interpreter should be conservative and assume that any recursive references can appear
    /// in the content of the corresponding element.
    Unknown,
}

impl FieldDeps {
    /// Compute the union of two cache elements dependencies. [`FieldDeps::Unknown`] can be see as
    /// the top element, meaning that if one of the two set of dependencies is
    /// [`FieldDeps::Unknown`], so is the result.
    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            // If one of the field has unknown dependencies (understand: may depend on all the other
            // fields), then the resulting fields has unknown dependencies as well
            (FieldDeps::Unknown, _) | (_, FieldDeps::Unknown) => FieldDeps::Unknown,
            (FieldDeps::Known(deps1), FieldDeps::Known(deps2)) => {
                let union: HashSet<Ident> = deps1.union(&*deps2).cloned().collect();
                FieldDeps::Known(Rc::new(union))
            }
        }
    }

    /// Return an empty set of dependencies
    pub fn empty() -> Self {
        FieldDeps::Known(Rc::new(HashSet::new()))
    }

    /// Return `true` if the dependencies are known and are empty, or `false` otherwise.
    pub fn is_empty(&self) -> bool {
        matches!(self, FieldDeps::Known(deps) if deps.is_empty())
    }
}

impl From<HashSet<Ident>> for FieldDeps {
    fn from(set: HashSet<Ident>) -> Self {
        FieldDeps::Known(Rc::new(set))
    }
}

/// Store field interdependencies in a recursive record. Map each static and dynamic field to the
/// set of recursive fields that syntactically appears in their definition as free variables.
#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct RecordDeps {
    /// Must have exactly the same keys as the static fields map of the recursive record.
    pub stat_fields: IndexMap<Ident, FieldDeps>,
    /// Must have exactly the same length as the dynamic fields list of the recursive record.
    pub dyn_fields: Vec<FieldDeps>,
}

/// The metadata attached to record fields.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct FieldMetadata {
    pub doc: Option<String>,
    pub annotation: TypeAnnotation,
    /// If the field is optional.
    pub opt: bool,
    /// If the field is serialized.
    pub not_exported: bool,
    pub priority: MergePriority,
}

impl FieldMetadata {
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

impl From<TypeAnnotation> for FieldMetadata {
    fn from(annotation: TypeAnnotation) -> Self {
        FieldMetadata {
            annotation,
            ..Default::default()
        }
    }
}

/// A record field with its metadata.
#[derive(Clone, Default, PartialEq, Debug)]
pub struct Field {
    /// The value is optional because record field may not have a definition (e.g. optional fields).
    pub value: Option<RichTerm>,
    pub metadata: FieldMetadata,
    /// List of contracts yet to be applied.
    /// These are only observed when data enter or leave the record.
    pub pending_contracts: Vec<RuntimeContract>,
}

impl From<RichTerm> for Field {
    fn from(rt: RichTerm) -> Self {
        Field {
            value: Some(rt),
            ..Default::default()
        }
    }
}

impl From<TypeAnnotation> for Field {
    fn from(annotation: TypeAnnotation) -> Self {
        Field::from(FieldMetadata::from(annotation))
    }
}

impl From<FieldMetadata> for Field {
    fn from(metadata: FieldMetadata) -> Self {
        Field {
            metadata,
            ..Default::default()
        }
    }
}

impl Field {
    /// Map a function over the value of the field, if any.
    pub fn map_value(self, f: impl FnOnce(RichTerm) -> RichTerm) -> Self {
        Field {
            value: self.value.map(f),
            ..self
        }
    }

    /// Map a fallible function over the value of the field, if any.
    pub fn try_map_value<E>(
        self,
        f: impl FnOnce(RichTerm) -> Result<RichTerm, E>,
    ) -> Result<Self, E> {
        Ok(Field {
            value: self.value.map(f).transpose()?,
            ..self
        })
    }

    /// Determine if a field is optional and without a defined value. In that case, it is usually
    /// silently ignored by most record operations (`has_field`, `values`, etc.).
    pub fn is_empty_optional(&self) -> bool {
        self.value.is_none() && self.metadata.opt
    }

    /// Required by the dynamic extension operator to know if the field being treated has a defined
    /// value that must be obtained from the stack or not.
    pub fn extension_kind(&self) -> RecordExtKind {
        if self.value.is_some() {
            RecordExtKind::WithValue
        } else {
            RecordExtKind::WithoutValue
        }
    }

    pub fn with_name(self, field_name: Option<LocIdent>) -> Self {
        Field {
            metadata: FieldMetadata {
                annotation: self.metadata.annotation.with_field_name(field_name),
                ..self.metadata
            },
            ..self
        }
    }
}

impl Traverse<RichTerm> for Field {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Field, E>
    where
        F: FnMut(RichTerm) -> Result<RichTerm, E>,
    {
        let annotation = self.metadata.annotation.traverse(f, order)?;
        let value = self.value.map(|v| v.traverse(f, order)).transpose()?;

        let metadata = FieldMetadata {
            annotation,
            ..self.metadata
        };

        let pending_contracts = self
            .pending_contracts
            .into_iter()
            .map(|pending_contract| pending_contract.traverse(f, order))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Field {
            metadata,
            value,
            pending_contracts,
        })
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&RichTerm, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        self.metadata
            .annotation
            .traverse_ref(f, state)
            .or_else(|| self.value.as_ref().and_then(|v| v.traverse_ref(f, state)))
            .or_else(|| {
                self.pending_contracts
                    .iter()
                    .find_map(|c| c.traverse_ref(f, state))
            })
    }
}

/// The base structure of a Nickel record.
///
/// Used to group together fields common to both the [super::Term::Record] and
/// [super::Term::RecRecord] terms.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct RecordData {
    /// Fields whose names are known statically.
    pub fields: IndexMap<LocIdent, Field>,
    /// Attributes which may be applied to a record.
    pub attrs: RecordAttrs,
    /// The hidden part of a record under a polymorphic contract.
    pub sealed_tail: Option<SealedTail>,
}

/// Error raised by [RecordData] methods when trying to access a field that doesn't have a
/// definition and isn't optional.
#[derive(Clone, Debug)]
pub struct MissingFieldDefError {
    pub id: LocIdent,
    pub metadata: FieldMetadata,
}

impl MissingFieldDefError {
    pub fn into_eval_err(self, pos_record: TermPos, pos_access: TermPos) -> EvalError {
        EvalError::MissingFieldDef {
            id: self.id,
            metadata: self.metadata,
            pos_record,
            pos_access,
        }
    }
}

impl RecordData {
    pub fn new(
        fields: IndexMap<LocIdent, Field>,
        attrs: RecordAttrs,
        sealed_tail: Option<SealedTail>,
    ) -> Self {
        RecordData {
            fields,
            attrs,
            sealed_tail,
        }
    }

    /// A record with no fields and the default set of attributes.
    pub fn empty() -> Self {
        Default::default()
    }

    /// A record with the provided fields and the default set of attributes.
    pub fn with_field_values(field_values: impl IntoIterator<Item = (LocIdent, RichTerm)>) -> Self {
        let fields = field_values
            .into_iter()
            .map(|(id, value)| (id, Field::from(value)))
            .collect();

        RecordData {
            fields,
            ..Default::default()
        }
    }

    /// Returns the record resulting from applying the provided function
    /// to each field.
    ///
    /// Note that `f` is taken as `mut` in order to allow it to mutate
    /// external state while iterating.
    pub fn map_values<F>(self, mut f: F) -> Self
    where
        F: FnMut(LocIdent, Option<RichTerm>) -> Option<RichTerm>,
    {
        let fields = self
            .fields
            .into_iter()
            .map(|(id, field)| {
                (
                    id,
                    Field {
                        value: f(id, field.value),
                        ..field
                    },
                )
            })
            .collect();
        RecordData { fields, ..self }
    }

    /// Returns the record resulting from applying the provided function to each field with a
    /// defined value. Fields without a value are left unchanged.
    pub fn map_defined_values<F>(self, mut f: F) -> Self
    where
        F: FnMut(LocIdent, RichTerm) -> RichTerm,
    {
        self.map_values(|id, value| value.map(|v| f(id, v)))
    }

    /// Turn the record into an iterator over the fields' values, ignoring optional fields without
    /// definition.
    ///
    /// The returned iterator applies pending contracts to each value.
    ///
    /// Fields that aren't optional but yet don't have a definition are mapped to the
    /// error `MissingFieldDefError`.
    pub fn into_iter_without_opts(
        self,
    ) -> impl Iterator<Item = Result<(Ident, RichTerm), MissingFieldDefError>> {
        self.fields
            .into_iter()
            .filter_map(|(id, field)| match field.value {
                Some(v) => {
                    let pos = v.pos;
                    Some(Ok((
                        id.ident(),
                        RuntimeContract::apply_all(v, field.pending_contracts.into_iter(), pos),
                    )))
                }
                None if !field.metadata.opt => Some(Err(MissingFieldDefError {
                    id,
                    metadata: field.metadata,
                })),
                None => None,
            })
    }

    /// Return an iterator over the fields' values, ignoring optional fields
    /// without definition and fields marked as not_exported. Fields that
    /// aren't optional but yet don't have a definition are mapped to the error
    /// `MissingFieldDefError`.
    pub fn iter_serializable(
        &self,
    ) -> impl Iterator<Item = Result<(Ident, &RichTerm), MissingFieldDefError>> {
        self.fields.iter().filter_map(|(id, field)| {
            debug_assert!(field.pending_contracts.is_empty());
            match field.value {
                Some(ref v) if !field.metadata.not_exported => Some(Ok((id.ident(), v))),
                None if !field.metadata.opt && !field.metadata.not_exported => {
                    Some(Err(MissingFieldDefError {
                        id: *id,
                        metadata: field.metadata.clone(),
                    }))
                }
                _ => None,
            }
        })
    }

    /// Get the value of a field. Ignore optional fields without value: trying to get their value
    /// returns `None`, as if they weren't present at all. Trying to extract a field without value
    /// which is non optional return an error.
    ///
    /// This method automatically applies the potential pending contracts
    pub fn get_value_with_ctrs(
        &self,
        id: &LocIdent,
    ) -> Result<Option<RichTerm>, MissingFieldDefError> {
        match self.fields.get(id) {
            Some(Field {
                value: None,
                metadata: metadata @ FieldMetadata { opt: false, .. },
                ..
            }) => Err(MissingFieldDefError {
                id: *id,
                metadata: metadata.clone(),
            }),
            Some(Field {
                value: Some(value),
                pending_contracts,
                ..
            }) => {
                let pos = value.pos;
                Ok(Some(RuntimeContract::apply_all(
                    value.clone(),
                    pending_contracts.iter().cloned(),
                    pos,
                )))
            }
            _ => Ok(None),
        }
    }

    /// Return a vector of all the fields' names of this record sorted alphabetically.
    ///
    /// # Parameters
    ///
    /// - `op_kind` controls if we should ignore or include empty optional fields
    pub fn field_names(&self, op_kind: RecordOpKind) -> Vec<LocIdent> {
        let mut fields: Vec<LocIdent> = self
            .fields
            .iter()
            // Ignore optional fields without definitions.
            .filter(|(_, field)| {
                matches!(op_kind, RecordOpKind::ConsiderAllFields) || !field.is_empty_optional()
            })
            .map(|(id, _)| *id)
            .collect();

        fields.sort_by(|id1, id2| id1.label().cmp(id2.label()));
        fields
    }
}

/// The sealed tail of a Nickel record under a polymorphic contract.
///
/// Note that access to the enclosed term must only be allowed when a matching sealing key is
/// provided. If this is not enforced it will lead to parametricity violations.
#[derive(Clone, Debug, PartialEq)]
pub struct SealedTail {
    /// The key with which the tail is sealed.
    sealing_key: SealingKey,
    /// The label to which blame will be attributed if code tries to
    /// interact with the sealed tail in any way.
    pub label: Label,
    /// The term which is sealed.
    term: RichTerm,
    /// The field names of the sealed fields.
    // You may find yourself wondering why this is a `Vec` rather than a
    // `HashSet` given we only ever do containment checks against it.
    // In brief: we'd need to use a `HashSet<String>`, which would mean
    // allocating `fields.len()` `String`s in a fairly hot codepath.
    // Since we only ever check whether the tail contains a specific field
    // when we already know we're going to raise an error, it's not really
    // an issue to have a linear lookup there, so we do that instead.
    fields: Vec<Ident>,
}

impl SealedTail {
    pub fn new(sealing_key: SealingKey, label: Label, term: RichTerm, fields: Vec<Ident>) -> Self {
        Self {
            sealing_key,
            label,
            term,
            fields,
        }
    }

    /// Returns the sealed term if the key matches, otherwise returns None.
    pub fn unseal(&self, key: &SealingKey) -> Option<&RichTerm> {
        if key == &self.sealing_key {
            Some(&self.term)
        } else {
            None
        }
    }

    pub fn has_field(&self, field: &Ident) -> bool {
        self.fields.contains(field)
    }

    pub fn has_dyn_field(&self, field: &str) -> bool {
        self.fields.iter().any(|i| i.label() == field)
    }
}
