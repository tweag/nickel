use super::*;
use crate::{error::EvalError, identifier::Ident, label::Label};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

#[derive(Debug, Default, Eq, PartialEq, Copy, Clone)]
pub struct RecordAttrs {
    pub open: bool,
}

impl RecordAttrs {
    pub fn merge(attrs1: RecordAttrs, attrs2: RecordAttrs) -> RecordAttrs {
        RecordAttrs {
            open: attrs1.open || attrs2.open,
        }
    }
}

/// Dependencies of a field or a thunk over the other recursive fields of a recursive record.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FieldDeps {
    /// The set of dependencies is fixed and has been computed. When attached to a thunk, an empty
    /// set of dependency means that the thunk isn't revertible, but standard.
    Known(Rc<HashSet<Ident>>),
    /// The thunk is revertible, but the set of dependencies hasn't been computed. In that case,
    /// the interpreter should be conservative and assume that any recursive references can appear
    /// in the content of the corresponding thunk.
    Unknown,
}

impl FieldDeps {
    /// Compute the union of two thunk dependencies. [`FieldDeps::Unknown`] can be see as the top
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
    pub stat_fields: HashMap<Ident, FieldDeps>,
    /// Must have exactly the same length as the dynamic fields list of the recursive record.
    pub dyn_fields: Vec<FieldDeps>,
}

/// A type or a contract together with its corresponding label.
// #[derive(Debug, PartialEq, Clone)]
// pub struct LabeledType {
//     pub types: Types,
//     pub label: Label,
// }

/// The metadata attached to record fields.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct FieldMetadata {
    pub doc: Option<String>,
    pub annotation: TypeAnnotation,
    /// If the field is optional.
    pub opt: bool,
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
            priority,
        }
    }
}

/// A record field with meta
#[derive(Clone, Default, PartialEq, Debug)]
pub struct Field {
    /// The value is optional because record field may not have a definition (e.g. optional fields).
    pub value: Option<RichTerm>,
    pub metadata: FieldMetadata,
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
    pub fn map_value(self, f: impl FnOnce(RichTerm) -> RichTerm) -> Self {
        Field {
            metadata: self.metadata,
            value: self.value.map(f),
        }
    }

    pub fn try_map_value<E>(
        self,
        f: impl FnOnce(RichTerm) -> Result<RichTerm, E>,
    ) -> Result<Self, E> {
        Ok(Field {
            metadata: self.metadata,
            value: self.value.map(f).transpose()?,
        })
    }

    pub fn is_empty_optional(&self) -> bool {
        self.value.is_none() && self.metadata.opt
    }

    pub fn extension_kind(&self) -> RecordExtKind {
        if self.value.is_some() {
            RecordExtKind::WithValue
        } else {
            RecordExtKind::WithoutValue
        }
    }
}

/// The base structure of a Nickel record.
///
/// Used to group together fields common to both the [super::Term::Record] and
/// [super::Term::RecRecord] terms.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct RecordData {
    /// Fields whose names are known statically.
    pub fields: HashMap<Ident, Field>,
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
        fields: HashMap<Ident, Field>,
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
    pub fn with_field_values(field_values: HashMap<Ident, RichTerm>) -> Self {
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
    pub fn map_fields<F>(self, mut f: F) -> Self
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
                        metadata: field.metadata,
                        value: f(id, field.value),
                    },
                )
            })
            .collect();
        RecordData { fields, ..self }
    }

    /// Returns the record resulting from applying the provided function to each field with a
    /// defined value. Fields without a value are left unchanged.
    pub fn map_fields_with_value<F>(self, mut f: F) -> Self
    where
        F: FnMut(Ident, RichTerm) -> RichTerm,
    {
        self.map_fields(|id, value| value.map(|v| f(id, v)))
    }

    /// Turn the record into an iterator over the fields' values, ignoring optional fields without
    /// definition. Fields that aren't optional but yet don't have a definition are mapped to the
    /// error `MissingFieldDefError`.
    pub fn into_iter_without_opts(
        self,
    ) -> impl Iterator<Item = Result<(Ident, RichTerm), MissingFieldDefError>> {
        self.fields
            .into_iter()
            .filter_map(|(id, field)| match field.value {
                Some(v) => Some(Ok((id, v))),
                None if !field.metadata.opt => Some(Err(MissingFieldDefError {
                    id,
                    metadata: field.metadata,
                })),
                None => None,
            })
    }

    /// Return an iterator over the fields' values, ignoring optional fields without
    /// definition. Fields that aren't optional but yet don't have a definition are mapped to the
    /// error `MissingFieldDefError`.
    pub fn iter_without_opts(
        &self,
    ) -> impl Iterator<Item = Result<(&Ident, &RichTerm), MissingFieldDefError>> {
        self.fields
            .iter()
            .filter_map(|(id, field)| match field.value {
                Some(ref v) => Some(Ok((id, v))),
                None if !field.metadata.opt => Some(Err(MissingFieldDefError {
                    id: *id,
                    metadata: field.metadata.clone(),
                })),
                None => None,
            })
    }

    /// Get the value of a field. Ignore optional fields without value: trying to get their value
    /// returns `None`, as if they weren't present at all. Trying to extract a field without value
    /// which is non optional return an error.
    pub fn get_value(&self, id: &Ident) -> Result<Option<&RichTerm>, MissingFieldDefError> {
        match self.fields.get(id) {
            Some(Field {
                value: None,
                metadata: metadata @ FieldMetadata { opt: false, .. },
                ..
            }) => Err(MissingFieldDefError {
                id: *id,
                metadata: metadata.clone(),
            }),
            field_opt => Ok(field_opt.and_then(|field| field.value.as_ref())),
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
