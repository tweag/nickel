use std::collections::HashMap;

use nickel_lang_core::{
    environment::Environment as GenericEnvironment,
    identifier::Ident,
    position::RawSpan,
    term::{record::FieldMetadata, RichTerm, Term, Traverse, TraverseControl},
};

use crate::{field_walker::DefWithPath, identifier::LocIdent};

/// A term and a path.
///
/// This is morally equivalent to (but a more convenient representation than)
/// `Op1(StaticAccess("field2"), Op1(StaticAccess("field1"), term))`.
#[derive(Clone, Debug, PartialEq)]
pub struct TermAtPath {
    pub term: RichTerm,
    /// A path of identifiers, in left-to-right order.
    ///
    /// So, for `term.x.y.z`, this will be `vec!["x", "y", "z"]`.
    pub path: Vec<Ident>,
}

impl From<RichTerm> for TermAtPath {
    fn from(term: RichTerm) -> Self {
        Self {
            term,
            path: Vec::new(),
        }
    }
}

pub type Environment = GenericEnvironment<Ident, DefWithPath>;

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
        let (term, path) = val
            .map(Into::into)
            .map(|term_at_path| (term_at_path.term, term_at_path.path))
            .unzip();
        self.insert(
            ident.ident,
            DefWithPath {
                ident,
                value: term.map(Into::into),
                metadata: meta,
                path: path.unwrap_or_default(),
            },
        );
    }
}

/// A lookup table for finding variable usages and variable definitions.
///
/// Variable usages come from `Term::Var`, while variable definitions come
/// from `let` bindings, function definitions, and bindings in records.
///
/// Note that this is not (on its own) an implementation of the "goto definition"
/// functionality, as it only resolves variable references. For example, going
/// to the definition of `foo` in `x.foo` involves two steps: finding the definition
/// of the variable `x`, and then looking for the definition of its "foo" field.
/// This lookup table is for the first step.
#[derive(Clone, Debug, Default)]
pub struct UsageLookup {
    // Maps from spans (of terms) to the environments that those spans belong to.
    // This could be made more general (we might want to map arbitrary positions to
    // environments) but its enough for us now.
    def_table: HashMap<RawSpan, Environment>,
    usage_table: HashMap<LocIdent, Vec<LocIdent>>,
    // The list of all the symbols (and their locations) in the document.
    //
    // Currently, variables bound in `let` bindings and record fields count as symbols.
    syms: HashMap<LocIdent, DefWithPath>,
}

impl UsageLookup {
    /// Create a new lookup table by looking for definitions and usages in the tree rooted at `rt`.
    pub fn new(rt: &RichTerm, env: &Environment) -> Self {
        let mut table = Self::default();
        table.fill(rt, env);
        table
    }

    /// Return all the usages of `ident`.
    pub fn usages(&self, ident: &LocIdent) -> impl Iterator<Item = &LocIdent> {
        self.usage_table
            .get(ident)
            .map(|v| v.iter())
            .unwrap_or([].iter())
    }

    /// Return the definition site of `ident`.
    pub fn def(&self, ident: &LocIdent) -> Option<&DefWithPath> {
        // First try to look up the definition in our symbols table. If that fails,
        // find the active environment and look up the ident in it.
        //
        // We check the symbols table first so that we can retrieve the definition
        // in the case that the ident is already pointing straight at it. For example, in
        // `let x = 3`, the environment containing `x` doesn't define `x` but we still
        // want `def(x)` to return this definition.
        self.syms.get(ident).or_else(|| {
            ident
                .pos
                .as_opt_ref()
                .and_then(|span| self.def_table.get(span))
                .and_then(|env| env.get(&ident.ident))
        })
    }

    /// Return the enviroment that a term belongs to.
    pub fn env(&self, term: &RichTerm) -> Option<&Environment> {
        term.pos
            .as_opt_ref()
            .and_then(|span| self.def_table.get(span))
    }

    /// Return the list of symbols in the document.
    pub fn symbols(&self) -> impl Iterator<Item = LocIdent> + '_ {
        self.syms.keys().cloned()
    }

    fn add_sym(
        &mut self,
        id: impl Into<LocIdent>,
        val: Option<impl Into<TermAtPath>>,
        meta: Option<FieldMetadata>,
    ) {
        let ident = id.into();
        let (term, path) = val
            .map(Into::into)
            .map(|term_at_path| (term_at_path.term, term_at_path.path))
            .unzip();

        self.syms.insert(
            ident,
            DefWithPath {
                ident,
                value: term.map(Into::into),
                metadata: meta,
                path: path.unwrap_or_default(),
            },
        );
    }

    fn fill(&mut self, rt: &RichTerm, env: &Environment) {
        rt.traverse_ref(
            &mut |term: &RichTerm, env: &Environment| {
                if let Some(span) = term.pos.as_opt_ref() {
                    self.def_table.insert(*span, env.clone());
                }

                match term.term.as_ref() {
                    Term::Fun(id, _body) => {
                        let mut new_env = env.clone();
                        new_env.def_noval(*id, None);
                        TraverseControl::ContinueWithScope(new_env)
                    }
                    Term::FunPattern(maybe_id, pat, _body) => {
                        let mut new_env = env.clone();
                        if let Some(id) = maybe_id {
                            new_env.def_noval(*id, None);
                        }

                        for m in &pat.matches {
                            for (_path, id, field) in m.to_flattened_bindings() {
                                new_env.def_noval(id, Some(field.metadata));
                            }
                        }
                        TraverseControl::ContinueWithScope(new_env)
                    }
                    Term::Let(id, val, body, attrs) => {
                        let mut new_env = env.clone();
                        new_env.def(*id, Some(val.clone()), None);
                        self.add_sym(*id, Some(val.clone()), None);

                        self.fill(val, if attrs.rec { &new_env } else { env });
                        self.fill(body, &new_env);

                        TraverseControl::SkipBranch
                    }
                    Term::LetPattern(maybe_id, pat, val, _body) => {
                        let mut new_env = env.clone();
                        if let Some(id) = maybe_id {
                            new_env.def(*id, Some(val.clone()), None);
                            self.add_sym(*id, Some(val.clone()), None);
                        }

                        for m in &pat.matches {
                            for (path, id, field) in m.to_flattened_bindings() {
                                let path = path.iter().map(|i| i.ident()).rev().collect();
                                let term = TermAtPath {
                                    term: val.clone(),
                                    path,
                                };
                                new_env.def(id, Some(term.clone()), Some(field.metadata));
                                self.add_sym(id, Some(val.clone()), None);
                            }
                        }
                        TraverseControl::ContinueWithScope(new_env)
                    }
                    Term::RecRecord(data, _interp_fields, _deps) => {
                        let mut new_env = env.clone();

                        // Records are recursive and the order of fields is unimportant, so define
                        // all the fields in the environment and then recurse into their values.
                        for (id, field) in &data.fields {
                            new_env.def(*id, field.value.clone(), Some(field.metadata.clone()));
                            self.add_sym(*id, field.value.clone(), Some(field.metadata.clone()));
                        }

                        TraverseControl::ContinueWithScope(new_env)
                    }
                    Term::Var(id) => {
                        let id = LocIdent::from(*id);
                        if let Some(def) = env.get(&id.ident) {
                            self.usage_table.entry(def.ident).or_default().push(id);
                        }
                        TraverseControl::Continue
                    }
                    _ => TraverseControl::<_, ()>::Continue,
                }
            },
            env,
        );
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use assert_matches::assert_matches;
    use codespan::FileId;
    use nickel_lang_core::{identifier::Ident, position::RawSpan, term::Term};

    use crate::{
        identifier::LocIdent,
        position::tests::parse,
        usage::{Environment, UsageLookup},
    };

    pub(crate) fn locced(
        ident: impl Into<Ident>,
        src_id: FileId,
        range: std::ops::Range<u32>,
    ) -> LocIdent {
        LocIdent {
            ident: ident.into(),
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
        let table = UsageLookup::new(&rt, &Environment::new());

        assert_eq!(table.usages(&x0).cloned().collect::<Vec<_>>(), vec![x1, x2]);
        assert_eq!(table.def(&x1), table.def(&x2));
        assert_eq!(table.def(&x0), table.def(&x1));

        let def = table.def(&x1).unwrap();
        assert_eq!(def.ident, x0);
        assert_matches!(def.value().unwrap().term.as_ref(), Term::Num(_));
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
        let table = UsageLookup::new(&rt, &Environment::new());

        assert_eq!(table.usages(&x0).cloned().collect::<Vec<_>>(), vec![x1]);
        assert_eq!(table.usages(&a0).cloned().collect::<Vec<_>>(), vec![a1]);
        assert_eq!(table.usages(&baz0).cloned().collect::<Vec<_>>(), vec![baz1]);

        let x_def = table.def(&x1).unwrap();
        assert_eq!(x_def.ident, x0);
        assert!(x_def.path().is_empty());

        let a_def = table.def(&a1).unwrap();
        assert_eq!(a_def.ident, a0);
        assert_eq!(a_def.path(), &["foo".into()]);

        let baz_def = table.def(&baz1).unwrap();
        assert_eq!(baz_def.ident, baz0);
        assert_eq!(baz_def.path(), vec!["foo".into(), "bar".into()]);
    }

    #[test]
    fn record_bindings() {
        let (file, rt) =
            parse("{ foo = 1, sub.field = 2, bar = foo, baz = sub.field, child = { bar = foo } }");
        let foo0 = locced("foo", file, 2..5);
        let foo1 = locced("foo", file, 32..35);
        let foo2 = locced("foo", file, 70..73);
        let sub0 = locced("sub", file, 11..14);
        let sub1 = locced("sub", file, 43..46);
        let field1 = locced("field", file, 47..52);
        let table = UsageLookup::new(&rt, &Environment::new());

        assert_eq!(table.def(&foo1).unwrap().ident, foo0);
        assert_eq!(table.def(&foo2).unwrap().ident, foo0);
        assert_eq!(table.def(&sub1).unwrap().ident, sub0);

        // We don't see "baz = sub.field" as a "usage" of field, because it's
        // a static access and not a var.
        assert!(table.def(&field1).is_none());
    }
}
