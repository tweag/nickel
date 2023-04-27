use super::*;
use crate::{error::EvalError, identifier::Ident, label::Label};
use std::{collections::HashSet, rc::Rc};

/// Additional attributes for record.
#[derive(Debug, Default, Eq, PartialEq, Copy, Clone)]
pub struct RecordAttrs {
    /// If the record is an open record, ie ending with `..`. Open records have a different
    /// behavior when used as a record contract: they allow additional fields to be present.
    pub open: bool,
}

impl RecordAttrs {
    pub fn merge(attrs1: RecordAttrs, attrs2: RecordAttrs) -> RecordAttrs {
        RecordAttrs {
            open: attrs1.open || attrs2.open,
        }
    }
}

/// Dependencies of a field or a cache element over the other recursive fields of a recursive record.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FieldDeps {
    /// The set of dependencies is fixed and has been computed. When attached to an element, an empty
    /// set of dependency means that the element isn't revertible, but standard.
    Known(Rc<HashSet<Ident>>),
    /// The element is revertible, but the set of dependencies hasn't been computed. In that case,
    /// the interpreter should be conservative and assume that any recursive references can appear
    /// in the content of the corresponding element.
    Unknown,
}

impl FieldDeps {
    /// Compute the union of two cache elements dependencies. [`FieldDeps::Unknown`] can be see as the top
    /// element, meaning that if one of the two set of dependencies is [`FieldDeps::Unknown`], so
    /// is the result.
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

    /// Flatten two nested metadata into one. If data that can't be combined (typically, the
    /// documentation or the type annotation) are set by both, the outer's one are kept.
    ///
    /// Note that no environment management operation such as closurization of contracts takes
    /// place, because this function is expected to be used on the AST before the evaluation (in
    /// the parser or during program transformation).
    pub fn flatten(mut outer: FieldMetadata, mut inner: FieldMetadata) -> FieldMetadata {
        // Keep the outermost value for non-mergeable information, such as documentation, type annotation,
        // and so on, which is the one that is accessible from the outside anyway (by queries, by the typechecker, and
        // so on).
        // Keep the inner value.

        if outer.annotation.types.is_some() {
            // If both have type annotations, the result will have the outer one as a type annotation.
            // However we still need to enforce the corresponding contract to preserve the operational
            // semantics. Thus, the inner type annotation is derelicted to a contract.
            if let Some(ctr) = inner.annotation.types.take() {
                outer.annotation.contracts.push(ctr)
            }
        }

        outer
            .annotation
            .contracts
            .extend(inner.annotation.contracts.into_iter());

        let priority = match (outer.priority, inner.priority) {
            // Neutral corresponds to the case where no priority was specified. In that case, the
            // other priority takes precedence.
            (MergePriority::Neutral, p) | (p, MergePriority::Neutral) => p,
            // Otherwise, we keep the maximum of both priorities, as we would do when merging
            // values.
            (p1, p2) => std::cmp::max(p1, p2),
        };

        FieldMetadata {
            doc: outer.doc.or(inner.doc),
            annotation: TypeAnnotation {
                types: outer.annotation.types.or(inner.annotation.types),
                contracts: outer.annotation.contracts,
            },
            opt: outer.opt || inner.opt,
            not_exported: outer.not_exported || inner.not_exported,
            priority,
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

    pub fn with_name(self, field_name: Option<Ident>) -> Self {
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
    fn traverse<F, S, E>(self, f: &F, state: &mut S, order: TraverseOrder) -> Result<Field, E>
    where
        F: Fn(RichTerm, &mut S) -> Result<RichTerm, E>,
    {
        let annotation = self.metadata.annotation.traverse(f, state, order)?;
        let value = self
            .value
            .map(|v| v.traverse(f, state, order))
            .transpose()?;

        let metadata = FieldMetadata {
            annotation,
            ..self.metadata
        };

        let pending_contracts = self
            .pending_contracts
            .into_iter()
            .map(|pending_contract| pending_contract.traverse(f, state, order))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Field {
            metadata,
            value,
            pending_contracts,
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
    pub fields: IndexMap<Ident, Field>,
    /// Attributes which may be applied to a record.
    pub attrs: RecordAttrs,
    /// The hidden part of a record under a polymorphic contract.
    pub sealed_tail: Option<SealedTail>,
}

/// Error raised by [RecordData] methods when trying to access a field that doesn't have a
/// definition and isn't optional.
#[derive(Clone, Debug)]
pub struct MissingFieldDefError {
    pub id: Ident,
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
        fields: IndexMap<Ident, Field>,
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
    pub fn with_field_values(field_values: IndexMap<Ident, RichTerm>) -> Self {
        let fields = field_values
            .into_iter()
            .map(|(id, value)| {
                (
                    id,
                    Field {
                        value: Some(value),
                        ..Default::default()
                    },
                )
            })
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
        F: FnMut(Ident, Option<RichTerm>) -> Option<RichTerm>,
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
        F: FnMut(Ident, RichTerm) -> RichTerm,
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
                        id,
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
    ) -> impl Iterator<Item = Result<(&Ident, &RichTerm), MissingFieldDefError>> {
        self.fields.iter().filter_map(|(id, field)| {
            debug_assert!(field.pending_contracts.is_empty());
            match field.value {
                Some(ref v) if !field.metadata.not_exported => Some(Ok((id, v))),
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
        id: &Ident,
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
}

impl SealedTail {
    pub fn new(sealing_key: SealingKey, label: Label, term: RichTerm) -> Self {
        Self {
            sealing_key,
            label,
            term,
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
}
