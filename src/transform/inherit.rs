use crate::match_sharedterm;
use crate::term::{make, RichTerm, Term, UnaryOp};
use crate::identifier::Ident;

pub fn transform_one(rt: RichTerm) -> RichTerm {
    match_sharedterm! {rt.term, with {
    Term::RecRecord(record, dyn_fields, deps, inh) => {
        let mut fields = record.clone();
        let renaming = inh.iter().map(|(ids, rt)| {
            if let Some(rt) = rt {
                // need a fresh var only for the record. a static access will be
                // performed on it.
                (ids, vec![Ident::fresh()], Some(rt))
            } else {
                (ids, ids.iter().map(|_| Ident::fresh()).collect(), None)
            }
        });

        fields.fields.extend(
            renaming
                .map(|(ids, vars, rt)| {
                    ids.iter().zip(vars).map(move |(id, var)| {
                        if rt.is_some() {
                            (
                                id.clone(),
                                make::op1(UnaryOp::StaticAccess(id.clone()), make::var(var)),
                            )
                        } else {
                            (id.clone(), make::var(var))
                        }
                    })
                })
                .flatten(),
        );
        RichTerm::new(Term::RecRecord(fields, dyn_fields, deps, vec![]), rt.pos)
    },
    } else  rt
    }
}
