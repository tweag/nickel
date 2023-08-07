use std::collections::{hash_map::Entry, HashMap};

use nickel_lang_core::{
    identifier::Ident,
    position::TermPos,
    term::{RichTerm, Term},
};

use crate::{linearization::completed::Completed, server::Server};

/// The position at which a something is defined.
#[derive(Clone, Debug)]
pub struct Def {
    /// The location of the definition.
    ///
    /// This is not necessarily the same as the position
    /// of any `RichTerm`. For example, the location of `x`'s definition in `let x = 1` is
    /// just the location of the "x" itself.
    pub location: TermPos,
    /// The value assigned by the definition, if there is one.
    pub value: Option<RichTerm>,
}

#[derive(Clone, Debug)]
pub struct FieldDefs {
    // The key to this map is really a Symbol rather than an Ident. Since the interner is not
    // public, we use an Ident that has had its location removed.
    fields: HashMap<Ident, Vec<Def>>,
}

/// Resolve a record path iteratively, returning the names of all the fields defined on the final path element.
pub fn resolve_path<'a>(
    rt: &'a RichTerm,
    mut path: &'a [Ident],
    linearization: &Completed,
    server: &Server,
) -> impl Iterator<Item = Ident> {
    let mut fields = FieldDefs::resolve(rt, linearization, server);

    while let Some((id, tail)) = path.split_first() {
        path = tail;
        let defs = fields.fields.remove(&id.without_pos()).unwrap_or_default();
        fields.fields.clear();

        for rt in defs.into_iter().filter_map(|d| d.value) {
            fields = fields.merge_from(FieldDefs::resolve(&rt, linearization, server));
        }
    }

    fields
        .fields
        .into_iter()
        .flat_map(|(id, defs)| defs.into_iter().map(move |d| id.with_pos(d.location)))
}

impl FieldDefs {
    pub fn resolve(rt: &RichTerm, linearization: &Completed, server: &Server) -> FieldDefs {
        let fields = match rt.term.as_ref() {
            Term::Record(data) | Term::RecRecord(data, ..) => data
                .fields
                .iter()
                .map(|(&ident, field)| {
                    (
                        ident.without_pos(),
                        vec![Def {
                            location: ident.pos,
                            value: field.value.clone(),
                        }],
                    )
                })
                .collect(),
            Term::Var(_) => {
                if let Some(def) = linearization.lookup_usage(rt) {
                    FieldDefs::resolve(&def, linearization, server).fields
                } else {
                    Default::default()
                }
            }
            Term::ResolvedImport(file_id) => {
                if let Some(term) = server.cache.get_ref(*file_id) {
                    FieldDefs::resolve(term, linearization, server).fields
                } else {
                    Default::default()
                }
            }
            _ => Default::default(),
        };

        FieldDefs { fields }
    }

    fn merge_from(mut self, other: FieldDefs) -> FieldDefs {
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
