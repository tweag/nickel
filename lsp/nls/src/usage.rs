use std::collections::HashMap;

use nickel_lang_core::{
    environment::Environment as GenericEnvironment,
    identifier::Ident,
    term::{record::FieldMetadata, RichTerm, Term, Traverse, TraverseControl},
};

use crate::{
    field_walker::{DefWithPath, TermAtPath},
    identifier::LocIdent,
};

type Environment = GenericEnvironment<Ident, DefWithPath>;

trait EnvExt {
    fn def(
        &mut self,
        id: impl Into<LocIdent>,
        val: Option<impl Into<TermAtPath>>,
        meta: Option<FieldMetadata>,
    );

    fn def_noval(&mut self, id: impl Into<LocIdent>, meta: Option<FieldMetadata>) {
        self.def(id, None::<TermAtPath>, meta);
    }
}

impl EnvExt for Environment {
    fn def(
        &mut self,
        id: impl Into<LocIdent>,
        val: Option<impl Into<TermAtPath>>,
        meta: Option<FieldMetadata>,
    ) {
        let ident = id.into();
        self.insert(
            ident.symbol,
            DefWithPath {
                ident,
                value: val.map(Into::into),
                metadata: meta,
            },
        );
    }
}

#[derive(Clone, Debug, Default)]
pub struct UsageLookup {
    def_table: HashMap<LocIdent, DefWithPath>,
    usage_table: HashMap<LocIdent, Vec<LocIdent>>,
}

impl UsageLookup {
    pub fn new(rt: &RichTerm) -> Self {
        let mut table = Self::default();
        table.fill(rt, &Environment::new());
        table
    }

    pub fn usages(&self, ident: &LocIdent) -> impl Iterator<Item = &LocIdent> {
        self.usage_table
            .get(ident)
            .map(|v| v.iter())
            .unwrap_or([].iter())
    }

    pub fn def(&self, ident: &LocIdent) -> Option<&DefWithPath> {
        self.def_table.get(ident)
    }

    fn fill(&mut self, rt: &RichTerm, env: &Environment) {
        // Static accesses count as usage (but probably not for us?)
        // Resolved imports count as usage
        // Let and Fun count as declarations
        // LetPattern and FunPattern count as declarations
        rt.traverse_ref(&mut |term: &RichTerm| match term.term.as_ref() {
            Term::Fun(id, body) => {
                let mut new_env = env.clone();
                new_env.def_noval(*id, None);
                self.fill(body, &new_env);
                TraverseControl::SkipBranch
            }
            Term::FunPattern(maybe_id, pat, body) => {
                let mut new_env = env.clone();
                if let Some(id) = maybe_id {
                    new_env.def_noval(*id, None);
                }

                for m in &pat.matches {
                    for (_path, id, field) in m.to_flattened_bindings() {
                        new_env.def_noval(id, Some(field.metadata));
                    }
                }
                self.fill(body, &new_env);
                TraverseControl::SkipBranch
            }
            Term::Let(id, val, body, attrs) => {
                let mut new_env = env.clone();
                new_env.def(*id, Some(val.clone()), None);

                self.fill(val, if attrs.rec { &new_env } else { env });
                self.fill(body, &new_env);

                TraverseControl::SkipBranch
            }
            Term::LetPattern(maybe_id, pat, val, body) => {
                let mut new_env = env.clone();
                if let Some(id) = maybe_id {
                    new_env.def(*id, Some(val.clone()), None);
                }

                for m in &pat.matches {
                    for (path, id, field) in m.to_flattened_bindings() {
                        let path = path.iter().map(|i| i.symbol()).rev().collect();
                        let term = TermAtPath {
                            term: val.clone(),
                            path,
                        };
                        new_env.def(id, Some(term), Some(field.metadata));
                    }
                }
                self.fill(body, &new_env);
                TraverseControl::SkipBranch
            }
            Term::Var(id) => {
                let id = LocIdent::from(*id);
                if let Some(def) = env.get(&id.symbol) {
                    self.def_table.insert(id, def.clone());
                    self.usage_table.entry(def.ident).or_default().push(id);
                }
                TraverseControl::Continue
            }
            _ => TraverseControl::<()>::Continue,
        });
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use codespan::FileId;
    use nickel_lang_core::{identifier::Ident, position::RawSpan, term::Term};

    use crate::{identifier::LocIdent, position::tests::parse, usage::UsageLookup};

    fn locced(ident: impl Into<Ident>, src_id: FileId, range: std::ops::Range<u32>) -> LocIdent {
        LocIdent {
            symbol: ident.into(),
            pos: RawSpan {
                src_id,
                start: range.start.into(),
                end: range.end.into(),
            }
            .into(),
        }
    }

    #[test]
    fn let_and_var() {
        let (file, rt) = parse("let x = 1 in x + x");
        let x = Ident::new("x");
        let x0 = locced(x, file, 4..5);
        let x1 = locced(x, file, 13..14);
        let x2 = locced(x, file, 17..18);
        let table = UsageLookup::new(&rt);

        assert_eq!(table.usages(&x0).cloned().collect::<Vec<_>>(), vec![x1, x2]);
        assert_eq!(table.def(&x1), table.def(&x2));
        assert_eq!(table.def(&x0), None);

        let def = table.def(&x1).unwrap();
        assert_eq!(def.ident, x0);
        assert_matches!(def.value.as_ref().unwrap().term.as_ref(), &Term::Num(_));
    }

    #[test]
    fn pattern_path() {
        let (file, rt) = parse("let x@{ foo = a@{ bar = baz } } = 'Undefined in x + a + baz");
        let x0 = locced("x", file, 4..5);
        let x1 = locced("x", file, 48..49);
        let a0 = locced("a", file, 14..15);
        let a1 = locced("a", file, 52..53);
        let baz0 = locced("baz", file, 24..27);
        let baz1 = locced("baz", file, 56..59);
        let table = UsageLookup::new(&rt);

        assert_eq!(table.usages(&x0).cloned().collect::<Vec<_>>(), vec![x1]);
        assert_eq!(table.usages(&a0).cloned().collect::<Vec<_>>(), vec![a1]);
        assert_eq!(table.usages(&baz0).cloned().collect::<Vec<_>>(), vec![baz1]);

        let x_def = table.def(&x1).unwrap();
        assert_eq!(x_def.ident, x0);
        assert!(x_def.value.as_ref().unwrap().path.is_empty());

        let a_def = table.def(&a1).unwrap();
        assert_eq!(a_def.ident, a0);
        assert_eq!(a_def.value.as_ref().unwrap().path, vec!["foo".into()]);

        let baz_def = table.def(&baz1).unwrap();
        assert_eq!(baz_def.ident, baz0);
        assert_eq!(
            baz_def.value.as_ref().unwrap().path,
            vec!["foo".into(), "bar".into()]
        );
    }
}
