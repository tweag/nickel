use std::collections::{hash_map::Entry, HashMap};

use nickel_lang_core::{
    identifier::Ident,
    position::TermPos,
    term::{RichTerm, Term},
};

use crate::{linearization::completed::Completed, server::Server};

#[derive(Clone, Debug)]
pub struct Def {
    location: TermPos,
    // FIXME: this is the wrong recursive definition, because it forces us to fully resolve
    // the tree of fields before we can query a single path. Do something lazier.
    fields: HashMap<Ident, Vec<Def>>,
}

impl Def {
    pub fn build(rt: &RichTerm, linearization: &Completed, server: &Server) -> Def {
        let fields = match rt.term.as_ref() {
            Term::Record(data) | Term::RecRecord(data, ..) => data
                .fields
                .iter()
                .map(|(&ident, field)| {
                    let field_def = match &field.value {
                        Some(val) => vec![Def::build(val, linearization, server)],
                        None => vec![],
                    };
                    // TODO: also take into account a type annotation
                    (ident, field_def)
                })
                .collect(),
            _ => Default::default(),
        };

        Def {
            location: rt.pos,
            fields,
        }
    }

    pub fn follow<'a>(&'a self, path: &'a [Ident]) -> impl Iterator<Item = Ident> + 'a {
        match path.split_first() {
            Some((id, tail)) => {
                let fields = self.fields.get(id);
                if let Some(fields) = fields {
                    Box::new(fields.iter().flat_map(|def| def.follow(tail)))
                        as Box<dyn Iterator<Item = Ident>>
                } else {
                    Box::new(std::iter::empty())
                }
            }
            None => Box::new(self.fields.keys().copied()) as Box<dyn Iterator<Item = Ident>>,
        }
    }

    fn merge_from(mut self, other: Def) -> Def {
        for (ident, defs) in other.fields {
            match self.fields.entry(ident) {
                Entry::Occupied(oc) => {
                    oc.into_mut().extend_from_slice(&defs);
                }
                Entry::Vacant(vac) => {
                    vac.insert(defs);
                }
            }
        }
        self
    }
}
