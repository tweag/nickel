use crate::identifier::Ident;
use crate::match_sharedterm;
use crate::term::{make, RichTerm, Term, UnaryOp};

pub fn transform_one(rt: RichTerm) -> RichTerm {
    match_sharedterm! {rt.term, with {
    Term::RecRecord(record, dyn_fields, deps, inh) if !inh.is_empty() => {
        let mut fields = record;
        println!("inherit len: {}", inh.len());
        let renaming: Vec<(Vec<_>, Vec<_>, Option<RichTerm>)> = inh.into_iter().map(|(ids, rt)| {
            if rt.is_some() {
                // need a fresh var only for the record. a static access will be
                // performed on it.
                let var  = Ident::fresh();
                (ids.clone(), ids.iter().map(|_| var).collect(), rt)
            } else {
                (ids.clone(), ids.iter().map(|_| Ident::fresh()).collect(), None)
            }
        }).collect();

        fields.fields.extend(
            renaming
            .iter()
                .flat_map(|(ids, vars, rt)| {
                    ids.iter().zip(vars).map(move |(id, var)| {
                        if rt.is_some() {
                            (
                                *id,
                                make::op1(UnaryOp::StaticAccess(*id), make::var(*var)),
                            )
                        } else {
                            (*id, make::var(*var))
                        }
                    })
                }),
        );
        let rec = RichTerm::new(Term::RecRecord(fields, dyn_fields, deps, vec![]), rt.pos);
        renaming.iter().fold(rec, |rec, (ids, vars, rt)|
                      if let Some(rt) = rt {
                          make::let_in(vars[0], rt.clone(), rec)
                      } else {
                          ids.iter().zip(vars).fold(rec, |rec, (id, var)| make::let_in(*var, make::var(*id), rec))
                      })

    },
    } else  rt
    }
}
