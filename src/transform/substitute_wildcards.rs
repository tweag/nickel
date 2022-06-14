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
    term::{Contract, MetaValue, RichTerm, Term, TraverseOrder},
    typecheck::Wildcards,
    types::{AbsType, Types},
};

/// If the top-level node of the AST is a meta-value with a wildcard type annotation, replace
/// both the type annotation and the label's type with the inferred type.
pub fn transform_one(rt: RichTerm, wildcards: &Wildcards) -> RichTerm {
    let pos = rt.pos;
    match_sharedterm! {rt.term,
        with {
            Term::MetaValue(meta @ MetaValue {
                types: Some(_),
                ..
            }) => {
                let mut meta = meta;
                let Contract { types, label } = meta.types.take().unwrap();  // Safe, we know there's a type

                // Replace the type annotation and the blame label
                let types = substitute_wildcards_recursively(types, wildcards);
                let label_types = substitute_wildcards_recursively(label.types.as_ref().clone(), wildcards);

                let label = Label {
                    types: Rc::new(label_types),
                    ..label
                };
                meta.types = Some(Contract { types, label });

                RichTerm::new(Term::MetaValue(meta), pos)
            }
        } else rt
    }
}

/// Get the inferred type for a wildcard, or Dyn if no type was inferred.
fn get_wildcard_type(wildcards: &Wildcards, id: usize) -> Types {
    wildcards.get(id).cloned().unwrap_or(Types(AbsType::Dyn()))
}

/// Recursively substitutes wildcards for their inferred type inside a given type.
fn substitute_wildcards_recursively(ty: Types, wildcards: &Wildcards) -> Types {
    ty.traverse::<_, _, Infallible>(
        &mut |ty, _| {
            if let Types(AbsType::Wildcard(id)) = ty {
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
