use std::collections::{HashSet, VecDeque};
use std::mem::swap;

use crate::term::SharedTerm;
use crate::{
    identifier::Ident,
    term::{RichTerm, Term},
};

pub fn collect_free_vars(
    rt: &mut RichTerm,
    parent_rec_record: bool,
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
        Term::LetPattern(id, _, _, _) => {
            // We can ignore the `Destruct`, since all values lhs are in the rhs.
            id.as_ref().map(|id| free_vars.remove(id));
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
        Term::Switch(..) => {
            // TODO switch should be revisted.
        }

        _ => {}
    }

    if parent_rec_record {
        fields_free_vars.push_back(free_vars.clone());
    }
}
