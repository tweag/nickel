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

        Term::RecRecord(map, _dyn_fields, _, ffv) => {
            *ffv = Some(
                map.iter()
                    .map(|(id, _)| (id.clone(), fields_free_vars.pop_front().unwrap()))
                    .collect(),
            );
            let dyn_fields_free_vars =
                fields_free_vars
                    .drain(0..)
                    .fold(HashSet::new(), |mut acc, fr| {
                        fr.into_iter().for_each(|id| {
                            acc.insert(id);
                        });
                        acc
                    });
            // All Dynamic field's free vars are combined into one set with an empty Ident.
            ffv.as_mut()
                .unwrap()
                .insert(Ident::from(String::new()), dyn_fields_free_vars);

            map.iter().for_each(|(id, _)| {
                free_vars.remove(id);
            });
        }
        _ => {}
    }

    if parent_rec_record == TermType::RecRecord {
        fields_free_vars.push_back(free_vars.clone());
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
