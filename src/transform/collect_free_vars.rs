use std::collections::HashSet;
use std::mem;

use crate::destruct::{Destruct, Match};
use crate::term::{SharedTerm, TermType};
use crate::{
    identifier::Ident,
    term::{RichTerm, Term},
};

// `collect_free_vars` is transformation pass that caches a terms free variables.
// This is used for optimizing memory consumption
//
// `free_vars` is the set of free variables in this term.
//
// `fields_free_vars` is a mapping between position of a field in a RecRecord`,
//  and the free variables used in the body of the field.
pub fn collect_free_vars(
    rt: &mut RichTerm,
    parent_type: TermType,
    free_vars: &mut HashSet<Ident>,
    fields_free_vars: &mut Vec<HashSet<Ident>>,
) {
    match parent_type {
        TermType::RecRecord => {
            dbg!(&free_vars);
            dbg!(&fields_free_vars);
            dbg!(parent_type);
            let mut ffv = HashSet::new();
            mem::swap(&mut ffv, free_vars);
            fields_free_vars.push(ffv);
        }
        TermType::Let(id) => {
            free_vars.remove(id);
        }

        TermType::LetPattern(id, destruct) => {}
        TermType::Any => {}
    }

    match SharedTerm::make_mut(&mut rt.term) {
        Term::Var(id) => {
            free_vars.insert(id.clone());
        }
        Term::Fun(id, _) => {
        }
        Term::LetPattern(id, d, _, _) | Term::FunPattern(id, d, _) => {
            id.as_ref().map(|id| free_vars.remove(id));
            remove_destruct(d, free_vars);
        }

        Term::RecRecord(map, dyn_fields, _, ffv) => {
            for i in fields_free_vars.iter().flat_map(|fv| fv.iter()) {
                free_vars.insert(i.clone());
            }

            for f in dyn_fields.iter_mut() {
                f.2 = Some(fields_free_vars.pop().unwrap());
            }

            dbg!(&fields_free_vars);
            *ffv = Some(
                map.iter()
                    .zip(fields_free_vars.drain(0..map.len()))
                    .map(|((id, _), fv)| (id.clone(), fv))
                    .collect(),
            );

            map.iter().for_each(|(id, _)| {
                free_vars.remove(id);
            });

            dbg!(&free_vars);
            dbg!(&fields_free_vars);
            dbg!(parent_type);
        }
        _ => {}
    }
}

// Making this recursive should be fine unless we expect massive destructuring patterns.
fn remove_destruct(d: &Destruct, free_vars: &mut HashSet<Ident>) {
    match d {
        Destruct::Record(_, _, id) => {
            id.as_ref().map(|id| free_vars.remove(id));
        }
        Destruct::List(ms) => ms.iter().for_each(|m| match m {
            Match::Assign(id, _, (m_id, d)) => {
                free_vars.remove(id);
                m_id.as_ref().map(|id| free_vars.remove(id));
                remove_destruct(d, free_vars)
            }
            Match::Simple(id, _) => {
                free_vars.remove(id);
            }
        }),
        Destruct::Empty => {}
    }
}
