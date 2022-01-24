use std::collections::{HashSet, VecDeque};

use crate::destruct::{Destruct, Match};
use crate::term::{SharedTerm, TermType};
use crate::{
    identifier::Ident,
    term::{RichTerm, Term},
};

pub fn collect_free_vars(
    rt: &mut RichTerm,
    parent_rec_record: TermType,
    free_vars: &mut HashSet<Ident>,
    fields_free_vars: &mut VecDeque<HashSet<Ident>>,
) {

    if parent_rec_record == TermType::RecRecord {
        fields_free_vars.push_back(free_vars.clone());
    }

    match SharedTerm::make_mut(&mut rt.term) {
        Term::Var(id) => {
            free_vars.insert(id.clone());
        }
        Term::Let(id, _, _, _) | Term::Fun(id, _) => {
            free_vars.remove(id);
        }
        Term::LetPattern(id, d, _, _) | Term::FunPattern(id, d, _) => {
            id.as_ref().map(|id| free_vars.remove(id));
            remove_destruct(d, free_vars);
        }

        Term::RecRecord(map, dyn_fields, _, ffv) => {
            for f in dyn_fields.iter_mut() {
                f.2 = Some(fields_free_vars.pop_back().unwrap());
            }

            *ffv = Some(
                map.iter()
                    .zip(fields_free_vars.drain(0..map.len()))
                    .map(|((id, _), fv)| (id.clone(), fv))
                    .collect(),
            );
            for (fv, (_, _, dfv)) in fields_free_vars.drain(0..).zip(dyn_fields.iter_mut()) {
                *dfv = Some(fv);
            }

            map.iter().for_each(|(id, _)| {
                free_vars.remove(id);
            });
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
