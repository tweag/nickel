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
    term::{LabeledType, MetaValue, RichTerm, Term, Traverse, TraverseOrder, TypeAnnotation},
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
                // the followign line is due to a limitation of the macro `match_sharedterm`: we
                // can't use `mut` directly inside patterns. See the associated documentation.
                let mut annot = annot;
                let LabeledType { types, label } = annot.types.take().unwrap();  // Safe, we know there's a type

                // Replace the type annotation and the blame label
                let types = substitute_wildcards_recursively(types, wildcards);
                let label_types = substitute_wildcards_recursively(label.types.as_ref().clone(), wildcards);
                let label = Label {
                    types: Rc::new(label_types),
                    ..label
                };

                annot.types = Some(LabeledType { types, label });

                // TODO: should we also replace wildcards for contract annotation? It may be useful
                // to use them, for example by annotationg a subexpression inside a typed block
                // `exp | _`, saying "whatever the typechecker thinks the type of this expression should
                // be, assume that it is".

                RichTerm::new(Term::Annotated(annot, inner), pos)
            },
            Term::MetaValue(meta @ MetaValue {
                types: Some(_),
                ..
            }) => {
                let mut meta = meta;
                let LabeledType { types, label } = meta.types.take().unwrap();  // Safe, we know there's a type

                // Replace the type annotation and the blame label
                let types = substitute_wildcards_recursively(types, wildcards);
                let label_types = substitute_wildcards_recursively(label.types.as_ref().clone(), wildcards);

                let label = Label {
                    types: Rc::new(label_types),
                    ..label
                };
                meta.types = Some(LabeledType { types, label });

                RichTerm::new(Term::MetaValue(meta), pos)
            }
        } else rt
    }
}

/// Get the inferred type for a wildcard, or Dyn if no type was inferred.
fn get_wildcard_type(wildcards: &Wildcards, id: usize) -> Types {
    wildcards.get(id).cloned().unwrap_or(Types(TypeF::Dyn))
}

/// Recursively substitutes wildcards for their inferred type inside a given type.
fn substitute_wildcards_recursively(ty: Types, wildcards: &Wildcards) -> Types {
    ty.traverse::<_, _, Infallible>(
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
