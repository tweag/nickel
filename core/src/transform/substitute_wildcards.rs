//! Substitute wildcard types with their inferred type.
//!
//! During type checking, wildcard types are treated as type variables that may end up unified
//! with a concrete type.  If this is the case, this pass will substitute these wildcards with
//! the type inferred during type checking.  Otherwise, wildcards will be substituted with
//! `Dyn`.
use std::{convert::Infallible, rc::Rc};

use super::Wildcards;

use crate::{
    eval::value::{NickelValue, ValueContent, lens::TermContent},
    label::Label,
    term::{
        LabeledType, Term, TypeAnnotation,
        record::{Field, FieldMetadata, RecordData},
    },
    traverse::{Traverse, TraverseOrder},
    typ::{Type, TypeF},
};

/// If the top-level node of the AST is a meta-value with a wildcard type annotation, replace
/// both the type annotation and the label's type with the inferred type.
pub fn transform_one(value: NickelValue, wildcards: &Wildcards) -> NickelValue {
    let pos_idx = value.pos_idx();

    match value.content() {
        ValueContent::Term(term_lens) => {
            if let Term::Annotated(TypeAnnotation { typ: Some(_), .. }, _) = term_lens.term() {
                let TermContent::Annotated(value_lens) = term_lens else {
                    unreachable!("TermContent::Annotated expected")
                };

                let (annot, inner) = value_lens.take();

                NickelValue::term(
                    Term::Annotated(annot.subst_wildcards(wildcards), inner),
                    pos_idx,
                )
            } else if let TermContent::RecRecord(record_lens) = term_lens {
                let (record_data, includes, dyn_fields, deps, closurized) = record_lens.take();

                let record_data = record_data.subst_wildcards(wildcards);
                let dyn_fields = dyn_fields
                    .into_iter()
                    .map(|(id_t, field)| (id_t, field.subst_wildcards(wildcards)))
                    .collect();

                NickelValue::term(
                    Term::RecRecord(record_data, includes, dyn_fields, deps, closurized),
                    pos_idx,
                )
            } else {
                term_lens.restore()
            }
        }
        ValueContent::Record(lens) if lens.peek().is_inline_empty_record() => lens.restore(),
        ValueContent::Record(record_lens) => {
            // unwrap(): we treated the inline empty record case above
            let record_data = record_lens.take().into_opt().unwrap();
            NickelValue::record(record_data.subst_wildcards(wildcards), pos_idx)
        }
        lens => lens.restore(),
    }
}

/// Get the inferred type for a wildcard, or `Dyn` if no type was inferred.
fn get_wildcard_type(wildcards: &Wildcards, id: usize) -> Type {
    wildcards.get(id).cloned().unwrap_or(Type::from(TypeF::Dyn))
}

trait SubstWildcard {
    /// Recursively substitutes wildcards for their inferred type inside a given type.
    fn subst_wildcards(self, wildcards: &Wildcards) -> Self;
}

impl SubstWildcard for Type {
    fn subst_wildcards(self, wildcards: &Wildcards) -> Type {
        self.traverse(
            &mut |ty: Type| -> Result<_, Infallible> {
                if let TypeF::Wildcard(id) = ty.typ {
                    Ok(get_wildcard_type(wildcards, id))
                } else {
                    Ok(ty)
                }
            },
            TraverseOrder::TopDown,
        )
        .unwrap()
    }
}

impl SubstWildcard for LabeledType {
    fn subst_wildcards(self, wildcards: &Wildcards) -> LabeledType {
        LabeledType {
            typ: self.typ.subst_wildcards(wildcards),
            label: self.label.subst_wildcards(wildcards),
        }
    }
}

impl SubstWildcard for Label {
    fn subst_wildcards(self, wildcards: &Wildcards) -> Label {
        Label {
            typ: Rc::new((*self.typ).clone().subst_wildcards(wildcards)),
            ..self
        }
    }
}

impl SubstWildcard for TypeAnnotation {
    fn subst_wildcards(self, wildcards: &Wildcards) -> TypeAnnotation {
        let typ = self
            .typ
            .map(|labeled_ty| labeled_ty.subst_wildcards(wildcards));
        let contracts = self
            .contracts
            .into_iter()
            .map(|labeled_ty| labeled_ty.subst_wildcards(wildcards))
            .collect();

        TypeAnnotation { typ, contracts }
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
