use std::collections::{HashMap, HashSet};

use crate::term::{BindingType, SharedTerm};
use crate::{
    identifier::Ident,
    term::{RichTerm, Term},
};

pub fn collect_free_vars(
    rt: &mut RichTerm,
    free_vars: &mut HashSet<Ident>,
    fields_free_vars: HashMap<Ident, HashSet<Ident>>,
) {
    match SharedTerm::make_mut(&mut rt.term) {
        Term::Var(id) => {
            free_vars.insert(id.clone());
        }
        Term::Fun(id, _) => {
            free_vars.remove(id);
        }
        Term::Let(id, _, _, btype) => {
            if *btype == BindingType::Revertible && free_vars.contains(id) {
                *btype = BindingType::Normal;
            }
            free_vars.remove(id);
        }
        Term::LetPattern(id, _, _, _) => {
            // We can ignore the `Destruct`, since all values lhs are in the rhs.
            id.as_ref().map(|id| free_vars.remove(id));
        }

        Term::RecRecord(map, _dyn_fields, _, ffv) => {
            *ffv = Some(fields_free_vars);
            map.iter().for_each(|(id, _)| {
                free_vars.remove(id);
            });
        }
        Term::Switch(..) => {
            // TODO switch should be revisted.
        }

        _ => {}
    }
}
