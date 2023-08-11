use std::collections::HashMap;

use nickel_lang_core::{
    environment::Environment,
    identifier::{Ident, LocIdent},
    term::{RichTerm, Term, Traverse, TraverseControl},
};

use crate::{
    field_walker::{Def, DefValue},
    term::RichTermPtr,
};

#[derive(Clone, Debug, Default)]
pub struct UsageLookup {
    def_table: HashMap<RichTermPtr, Def>,
    usage_table: HashMap<Ident, RichTerm>,
}

impl UsageLookup {
    pub fn new(rt: &RichTerm) -> Self {
        let mut table = Self::default();
        table.fill(rt, &Environment::new());
        table
    }

    fn fill(&mut self, rt: &RichTerm, env: &Environment<Ident, Def>) {
        // Static accesses count as usage (but probably not for us?)
        // Resolved imports count as usage
        // Let and Fun count as declarations
        // LetPattern and FunPattern count as declarations
        rt.traverse_ref(&mut |term: &RichTerm| match term.term.as_ref() {
            Term::Fun(id, body) => {
                let mut new_env = env.clone();
                new_env.insert(
                    id.symbol(),
                    Def {
                        ident: *id,
                        value: None,
                        metadata: None,
                    },
                );

                self.fill(body, &new_env);
                TraverseControl::SkipBranch
            }
            Term::Let(id, val, body, attrs) => {
                let mut new_env = env.clone();
                new_env.insert(
                    id.symbol(),
                    Def {
                        ident: *id,
                        value: Some(val.clone().into()),
                        metadata: None,
                    },
                );

                self.fill(val, if attrs.rec { &new_env } else { env });
                self.fill(body, &new_env);

                TraverseControl::SkipBranch
            }
            Term::LetPattern(maybe_id, pat, val, body) => {
                let mut new_env = env.clone();
                if let Some(id) = maybe_id {
                    new_env.insert(
                        id.symbol(),
                        Def {
                            ident: *id,
                            value: Some(val.clone().into()),
                            metadata: None,
                        },
                    );
                }

                for m in &pat.matches {
                    for (path, id, field) in m.to_flattened_bindings() {
                        let path = path.iter().map(LocIdent::symbol).collect();
                        new_env.insert(
                            id.symbol(),
                            Def {
                                ident: id,
                                value: Some(DefValue {
                                    term: val.clone(),
                                    path,
                                }),
                                metadata: Some(field.metadata),
                            },
                        );
                    }
                }
                self.fill(body, &new_env);
                TraverseControl::SkipBranch
            }
            Term::Var(id) => {
                if let Some(def) = env.get(&id.symbol()) {
                    self.def_table.insert(RichTermPtr(rt.clone()), def.clone());
                    self.usage_table.insert(def.ident.symbol(), rt.clone());
                }
                TraverseControl::Continue
            }
            _ => TraverseControl::<()>::Continue,
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::{position::tests::parse, usage::UsageLookup};

    #[test]
    fn let_and_var() {
        let rt = parse("let x = 1 in x");
        let table = UsageLookup::new(&rt);
        dbg!(table);
    }
}
