//! Substitute wildcard types with their inferred type.
//!
//! During type checking, wildcard types are treated as type variables that may end up unified
//! with a concrete type.  If this is the case, this pass will substitute these wildcards with
//! the type inferred during type checking.  Otherwise, wildcards will be substituted with
//! `Dyn`.
use std::{convert::Infallible, rc::Rc};

use crate::{
    label::Label,
    match_sharedterm,
    term::{
        record::{Field, FieldMetadata, RecordData},
        LabeledType, RichTerm, Term, Traverse, TraverseOrder, TypeAnnotation,
    },
    typecheck::Wildcards,
    types::{TypeF, Types},
};

/// If the top-level node of the AST is a meta-value with a wildcard type annotation, replace
/// both the type annotation and the label's type with the inferred type.
pub fn transform_one(rt: RichTerm, wildcards: &Wildcards) -> RichTerm {
    let pos = rt.pos;
    match_sharedterm! {rt.term,
        with {
            Term::Annotated(
                annot @ TypeAnnotation {
                    types: Some(_),
                    ..
                },
                inner
            ) => {
                RichTerm::new(Term::Annotated(annot.subst_wildcards(wildcards), inner), pos)
            },
            Term::RecRecord(record_data, dyn_fields, deps) => {
                let record_data = record_data.subst_wildcards(wildcards);
                let dyn_fields = dyn_fields
                    .into_iter()
                    .map(|(id_t, field)| (id_t, field.subst_wildcards(wildcards)))
                    .collect();

                RichTerm::new(Term::RecRecord(record_data, dyn_fields, deps), pos)
            },
            Term::Record(record_data) => {
                RichTerm::new(
                    Term::Record(record_data.subst_wildcards(wildcards)),
                    pos,
                )
            },
        } else rt
    }
}

/// Get the inferred type for a wildcard, or `Dyn` if no type was inferred.
fn get_wildcard_type(wildcards: &Wildcards, id: usize) -> Types {
    wildcards.get(id).cloned().unwrap_or(Types(TypeF::Dyn))
}

trait SubstWildcard {
    /// Recursively substitutes wildcards for their inferred type inside a given type.
    fn subst_wildcards(self, wildcards: &Wildcards) -> Self;
}

impl SubstWildcard for Types {
    fn subst_wildcards(self, wildcards: &Wildcards) -> Types {
        self.traverse::<_, _, Infallible>(
            &|ty, _| {
                if let Types(TypeF::Wildcard(id)) = ty {
                    Ok(get_wildcard_type(wildcards, id))
                } else {
                    Ok(ty)
                }
            },
            &mut (),
            TraverseOrder::TopDown,
        )
        .unwrap()
    }
}

impl SubstWildcard for LabeledType {
    fn subst_wildcards(self, wildcards: &Wildcards) -> LabeledType {
        LabeledType {
            types: self.types.subst_wildcards(wildcards),
            label: self.label.subst_wildcards(wildcards),
        }
    }
}

impl SubstWildcard for Label {
    fn subst_wildcards(self, wildcards: &Wildcards) -> Label {
        Label {
            types: Rc::new((*self.types).clone().subst_wildcards(wildcards)),
            ..self
        }
    }
}

impl SubstWildcard for TypeAnnotation {
    fn subst_wildcards(self, wildcards: &Wildcards) -> TypeAnnotation {
        let types = self
            .types
            .map(|labeled_ty| labeled_ty.subst_wildcards(wildcards));
        let contracts = self
            .contracts
            .into_iter()
            .map(|labeled_ty| labeled_ty.subst_wildcards(wildcards))
            .collect();

        TypeAnnotation { types, contracts }
    }
}

impl SubstWildcard for FieldMetadata {
    fn subst_wildcards(self, wildcards: &Wildcards) -> FieldMetadata {
        FieldMetadata {
            annotation: self.annotation.subst_wildcards(wildcards),
            ..self
        }
    }
}

impl SubstWildcard for Field {
    fn subst_wildcards(self, wildcards: &Wildcards) -> Field {
        let metadata = self.metadata.subst_wildcards(wildcards);
        Field { metadata, ..self }
    }
}

impl SubstWildcard for RecordData {
    fn subst_wildcards(self, wildcards: &Wildcards) -> RecordData {
        RecordData {
            fields: self
                .fields
                .into_iter()
                .map(|(id, field)| (id, field.subst_wildcards(wildcards)))
                .collect(),
            ..self
        }
    }
}
