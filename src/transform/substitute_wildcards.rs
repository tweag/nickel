//! Substitute wildcard types with their inferred type.
//!
//! During type checking, wildcard types are treated as type variables that may end up unified
//! with a concrete type.  If this is the case, this pass will substitute these wildcards with
//! the type inferred during type checking.  Otherwise, wildcards will be substituted with
//! `Dyn`.
use std::rc::Rc;

use crate::{
    label::Label,
    match_sharedterm,
    term::{Contract, MetaValue, RichTerm, Term},
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
                types:
                    Some(Contract {
                        types: Types(AbsType::Wildcard(_)),
                        ..
                    }),
                ..
            }) => {
                let mut meta = meta;
                let Contract { types, label } = meta.types.take().unwrap();  // Safe, we know there's a type

                // Replace the type annotation
                let types = if let Types(AbsType::Wildcard(id)) = types {
                    get_wildcard_type(wildcards, id)
                } else {
                    types
                };

                // Replace the blame label
                let label = Label {
                    types: if let Types(AbsType::Wildcard(id)) = label.types.as_ref() {
                        Rc::new(get_wildcard_type(wildcards, *id))
                    } else {
                        label.types
                    },
                    ..label
                };

                meta.types.replace(Contract { types, label });

                RichTerm::new(Term::MetaValue(meta), pos)
            }
        } else rt
    }
}

/// Get the inferred type for a wildcard, or Dyn if no type was inferred.
fn get_wildcard_type(wildcards: &Wildcards, id: usize) -> Types {
    wildcards.get(&id).cloned().unwrap_or(Types(AbsType::Dyn()))
}
