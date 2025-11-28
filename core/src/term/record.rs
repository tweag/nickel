use super::*;
use crate::{
    combine::Combine,
    error::EvalErrorKind,
    identifier::{Ident, LocIdent},
    label::Label,
    position::PosIdx,
};
use std::{collections::HashSet, rc::Rc};

/// Additional attributes for record.
#[derive(Debug, Default, Eq, PartialEq, Copy, Clone)]
pub struct RecordAttrs {
    /// If the record is an open record, ie ending with `..`. Open records have a different
    /// behavior when used as a record contract: they allow additional fields to be present.
    pub open: bool,
    /// If the record has been frozen.
    ///
    /// A recursive record is frozen when all the lazy contracts are applied to their corresponding
    /// fields and flushed from the lazy contracts list. The values of the fields are computed but
    /// all dependencies are erased. That is, we turn a recursive, overridable record into a static
    /// dictionary. The information about field dependencies is lost and future overriding won't
    /// update reverse dependencies.
    ///
    /// We store this information for performance reason: freezing is expensive (linear in the
    /// number of fields of the record), and we might need to do it on every dictionary operation
    /// such as `insert`, `remove`, etc. (see
    /// [#1877](https://github.com/tweag/nickel/issues/1877)). This flags avoid repeated, useless
    /// freezing.
    pub frozen: bool,
}

impl RecordAttrs {
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the `frozen` flag to true and return the updated attributes.
    pub fn frozen(mut self) -> Self {
        self.frozen = true;
        self
    }
}

impl Combine for RecordAttrs {
    fn combine(left: Self, right: Self) -> Self {
        RecordAttrs {
            open: left.open || right.open,
            frozen: left.frozen && right.frozen,
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

/// Store field interdependencies in a recursive record. Map each static, dynamic and included
/// field to the set of recursive fields that syntactically appear in their definition as free
/// variables.
#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct RecordDeps {
    /// Must have exactly the same keys as the static fields map of the recursive record and the
    /// include expressions. Static fields and include expressions are combined because at the time
    /// the evaluator uses the dependencies, include expressions don't exist anymore: they have
    /// already been elaborated to static fields and inserted.
    pub stat_fields: IndexMap<Ident, FieldDeps>,
    /// Must have exactly the same length as the dynamic fields list of the recursive record.
    pub dyn_fields: Vec<FieldDeps>,
}

#[derive(Clone, Debug, PartialEq)]
/// An include expression (see [crate::ast::record::Include]).
pub struct Include {
    /// The included identifier.
    pub ident: LocIdent,
    /// The field metadata.
    pub metadata: FieldMetadata,
}

/// The metadata attached to record fields.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct FieldMetadata {
    pub doc: Option<Rc<str>>,
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

    /// Set the `field_name` attribute of the labels of the type and contracts annotations.
    pub fn with_field_name(mut self, name: Option<LocIdent>) -> Self {
        self.annotation = self.annotation.with_field_name(name);
        self
    }
}

impl Combine for FieldMetadata {
    fn combine(left: Self, right: Self) -> Self {
        let priority = match (left.priority, right.priority) {
            // Neutral corresponds to the case where no priority was specified. In that case, the
            // other priority takes precedence.
            (MergePriority::Neutral, p) | (p, MergePriority::Neutral) => p,
            // Otherwise, we keep the maximum of both priorities, as we would do when merging
            // values.
            (p1, p2) => std::cmp::max(p1, p2),
        };

        FieldMetadata {
            doc: crate::eval::merge::merge_doc(left.doc, right.doc),
            annotation: Combine::combine(left.annotation, right.annotation),
            opt: left.opt || right.opt,
            // The resulting field will be suppressed from serialization if either of the fields to be merged is.
            not_exported: left.not_exported || right.not_exported,
            priority,
        }
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
    pub value: Option<NickelValue>,
    pub metadata: Rc<FieldMetadata>,
    /// List of contracts yet to be applied.
    /// These are only observed when data enter or leave the record.
    pub pending_contracts: Vec<RuntimeContract>,
}

impl From<NickelValue> for Field {
    fn from(value: NickelValue) -> Self {
        Field {
            value: Some(value),
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
        Field::from(Rc::new(metadata))
    }
}

impl From<Rc<FieldMetadata>> for Field {
    fn from(metadata: Rc<FieldMetadata>) -> Self {
        Field {
            metadata,
            ..Default::default()
        }
    }
}

impl Field {
    /// Map a function over the value of the field, if any.
    pub fn map_value(self, f: impl FnOnce(NickelValue) -> NickelValue) -> Self {
        Field {
            value: self.value.map(f),
            ..self
        }
    }

    /// Map a fallible function over the value of the field, if any.
    pub fn try_map_value<E>(
        self,
        f: impl FnOnce(NickelValue) -> Result<NickelValue, E>,
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
}

impl Traverse<NickelValue> for Field {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Field, E>
    where
        F: FnMut(NickelValue) -> Result<NickelValue, E>,
    {
        let metadata = Rc::unwrap_or_clone(self.metadata);
        let annotation = metadata.annotation.traverse(f, order)?;
        let value = self.value.map(|v| v.traverse(f, order)).transpose()?;

        let metadata = FieldMetadata {
            annotation,
            ..metadata
        };

        let pending_contracts = self
            .pending_contracts
            .into_iter()
            .map(|pending_contract| pending_contract.traverse(f, order))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Field {
            metadata: Rc::new(metadata),
            value,
            pending_contracts,
        })
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&NickelValue, &S) -> TraverseControl<S, U>,
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
/// Used to group together fields common to both the [NickelValue] evaluated record and the
/// [super::Term::RecRecord] term.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct RecordData {
    /// Fields whose names are known statically.
    pub fields: IndexMap<LocIdent, Field>,
    /// Attributes which may be applied to a record.
    pub attrs: RecordAttrs,
    /// The hidden part of a record under a polymorphic contract.
    pub sealed_tail: Option<Rc<SealedTail>>,
}

/// Error raised by [RecordData] methods when trying to access a field that doesn't have a
/// definition and isn't optional.
#[derive(Clone, Debug)]
pub struct MissingFieldDefErrorData {
    pub id: LocIdent,
    pub metadata: FieldMetadata,
}

pub type MissingFieldDefError = Box<MissingFieldDefErrorData>;

impl MissingFieldDefErrorData {
    pub fn into_eval_err(self, pos_record: PosIdx, pos_access: PosIdx) -> EvalErrorKind {
        EvalErrorKind::MissingFieldDef {
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
            sealed_tail: sealed_tail.map(Rc::new),
        }
    }

    /// A record with no fields and the default set of attributes.
    pub fn empty() -> Self {
        Default::default()
    }

    /// A record with the provided fields and the default set of attributes.
    pub fn with_field_values(
        field_values: impl IntoIterator<Item = (LocIdent, NickelValue)>,
    ) -> Self {
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
        F: FnMut(LocIdent, Option<NickelValue>) -> Option<NickelValue>,
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
        F: FnMut(LocIdent, NickelValue) -> NickelValue,
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
    pub fn iter_without_opts(
        &self,
    ) -> impl Iterator<Item = Result<(Ident, NickelValue), MissingFieldDefError>> {
        self.fields
            .iter()
            .filter_map(|(id, field)| match &field.value {
                Some(v) => {
                    let pos = v.pos_idx();
                    Some(Ok((
                        id.ident(),
                        RuntimeContract::apply_all(
                            v.clone(),
                            field.pending_contracts.iter().cloned(),
                            pos,
                        ),
                    )))
                }
                None if !field.metadata.opt => Some(Err(Box::new(MissingFieldDefErrorData {
                    id: *id,
                    metadata: (*field.metadata).clone(),
                }))),
                None => None,
            })
    }

    /// Return an iterator over the fields' values, ignoring optional fields
    /// without definition and fields marked as not_exported. Fields that
    /// aren't optional but yet don't have a definition are mapped to the error
    /// `MissingFieldDefError`.
    pub fn iter_serializable(
        &self,
    ) -> impl Iterator<Item = Result<(Ident, &NickelValue), MissingFieldDefError>> {
        self.fields.iter().filter_map(|(id, field)| {
            debug_assert!(field.pending_contracts.is_empty());
            match field.value {
                Some(ref v) if !field.metadata.not_exported => Some(Ok((id.ident(), v))),
                None if !field.metadata.opt && !field.metadata.not_exported => {
                    Some(Err(Box::new(MissingFieldDefErrorData {
                        id: *id,
                        metadata: (*field.metadata).clone(),
                    })))
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
    ) -> Result<Option<NickelValue>, MissingFieldDefError> {
        match self.fields.get(id) {
            Some(Field {
                value: None,
                metadata,
                ..
            }) if !metadata.opt => Err(Box::new(MissingFieldDefErrorData {
                id: *id,
                metadata: (**metadata).clone(),
            })),
            Some(Field {
                value: Some(value),
                pending_contracts,
                ..
            }) => {
                let pos = value.pos_idx();
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

    /// Checks if this record is empty (including the sealed tail). Whether the record is open or
    /// not doesn't impact emptiness: `{..}` is considered empty.
    pub fn is_empty(&self) -> bool {
        self.fields.is_empty() && self.sealed_tail.is_none()
    }

    /// Checks if this record is empty (including the sealed tail), or if it is composed only of
    /// empty optional fields. [Self::is_empty] implies [Self::has_only_empty_opts], but the
    /// converse is not true, typically for `{foo | optional}`, for example.
    pub fn has_only_empty_opts(&self) -> bool {
        self.fields.values().all(Field::is_empty_optional) && self.sealed_tail.is_none()
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
    term: NickelValue,
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
    pub fn new(
        sealing_key: SealingKey,
        label: Label,
        term: NickelValue,
        fields: Vec<Ident>,
    ) -> Self {
        Self {
            sealing_key,
            label,
            term,
            fields,
        }
    }

    /// Returns the sealed term if the key matches, otherwise returns None.
    pub fn unseal(&self, key: &SealingKey) -> Option<&NickelValue> {
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
