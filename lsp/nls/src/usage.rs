use std::collections::HashMap;

use nickel_lang_core::{
    bytecode::ast::{pattern::bindings::Bindings as _, Ast, AstAlloc, Match, Node},
    environment::Environment as GenericEnvironment,
    identifier::Ident,
    position::RawSpan,
    traverse::{TraverseAlloc, TraverseControl},
};

use crate::{field_walker::Def, identifier::LocIdent};

pub type Environment<'ast> = GenericEnvironment<Ident, Def<'ast>>;

trait EnvExt<'ast> {
    fn insert_def(&mut self, def: Def<'ast>);
}

impl<'ast> EnvExt<'ast> for Environment<'ast> {
    fn insert_def(&mut self, def: Def<'ast>) {
        self.insert(def.ident(), def);
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
pub struct UsageLookup<'ast> {
    // Maps from spans (of terms) to the environments that those spans belong to.
    // This could be made more general (we might want to map arbitrary positions to
    // environments) but its enough for us now.
    def_table: HashMap<RawSpan, Environment<'ast>>,
    // Maps from spans of idents to the places where they are referenced.
    usage_table: HashMap<RawSpan, Vec<LocIdent>>,
    // The list of all the symbols (and their locations) in the document.
    //
    // Currently, variables bound in `let` bindings and record fields count as symbols.
    syms: HashMap<LocIdent, Def<'ast>>,
}

impl<'ast> UsageLookup<'ast> {
    /// Create a new lookup table by looking for definitions and usages in the tree rooted at `rt`.
    pub fn new(alloc: &'ast AstAlloc, ast: &'ast Ast<'ast>, env: &Environment<'ast>) -> Self {
        let mut table = Self::default();
        table.fill(alloc, ast, env);
        table
    }

    /// Return all the usages of `ident`.
    pub fn usages(&self, span: &RawSpan) -> impl Iterator<Item = &LocIdent> {
        self.usage_table
            .get(span)
            .map(|v| v.iter())
            .unwrap_or([].iter())
    }

    /// Return the definition site of `ident`.
    pub fn def(&self, ident: &LocIdent) -> Option<&Def<'ast>> {
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
    pub fn env(&self, ast: &'ast Ast<'ast>) -> Option<&Environment<'ast>> {
        ast.pos
            .as_opt_ref()
            .and_then(|span| self.def_table.get(span))
    }

    fn add_sym(&mut self, def: Def<'ast>) {
        self.syms.insert(def.loc_ident(), def);
    }

    // In general, a match is like a function in that it needs to be applied before we
    // know what's being matched on. So for example, in
    // ```
    // match { x => x.ba }
    // ```
    // we can't do much to auto-complete "ba". But it's very common in practice for the match to
    // be applied immediately, like
    // ```
    // y |> match { x => x.ba }
    // ```
    // and in this case we can look into `y` to extract completions for "ba".
    //
    // This is a long-winded way of saying that we can treat pattern bindings like we treat
    // function bindings (if we don't know where the application is) or like we treat let bindings
    // (if we know what the match gets applied to). Here, `value` is the value that the match
    // is applied to, if we know it.
    fn fill_match(
        &mut self,
        alloc: &'ast AstAlloc,
        env: &Environment<'ast>,
        data: &Match<'ast>,
        value: Option<&'ast Ast<'ast>>,
    ) {
        for branch in data.branches.iter() {
            let mut new_env = env.clone();
            // for (path, ident, _field) in branch.pattern.bindings() {
            for pat_binding in branch.pattern.bindings() {
                let def = Def::MatchBinding {
                    ident: pat_binding.id.into(),
                    value,
                    path: pat_binding.path.into_iter().map(|x| x.ident()).collect(),
                    metadata: alloc.alloc(pat_binding.metadata),
                };
                new_env.insert_def(def.clone());
                self.add_sym(def);
            }

            self.fill(alloc, &branch.body, &new_env);

            if let Some(guard) = &branch.guard {
                self.fill(alloc, guard, &new_env);
            }
        }
    }

    fn fill(&mut self, alloc: &'ast AstAlloc, ast: &'ast Ast<'ast>, env: &Environment<'ast>) {
        ast.traverse_ref(
            &mut |ast: &'ast Ast<'ast>, env: &Environment<'ast>| {
                if let Some(span) = ast.pos.as_opt_ref() {
                    self.def_table.insert(*span, env.clone());
                }

                match &ast.node {
                    Node::Fun { args, .. } => {
                        let mut new_env = env.clone();

                        for pat_bdg in args.iter().flat_map(|arg| arg.bindings()) {
                            new_env.insert_def(Def::Fn {
                                ident: LocIdent::from(pat_bdg.id),
                            });
                        }

                        TraverseControl::ContinueWithScope(new_env)
                    }
                    // Term::Let(bindings, body, attrs) => {
                    //     let mut new_env = env.clone();
                    //     for (id, val) in bindings {
                    //         let def = Def::Let {
                    //             ident: LocIdent::from(*id),
                    //             value: val.clone(),
                    //             path: Vec::new(),
                    //         };
                    //         new_env.insert_def(def.clone());
                    //         self.add_sym(def);
                    //     }
                    //
                    //     for (_, val) in bindings {
                    //         self.fill(val, if attrs.rec { &new_env } else { env });
                    //     }
                    //     self.fill(body, &new_env);
                    //
                    //     TraverseControl::SkipBranch
                    // }
                    Node::Let {
                        bindings,
                        body,
                        rec,
                    } => {
                        let mut new_env = env.clone();

                        for bdg in bindings.iter() {
                            for pat_bdg in bdg.pattern.bindings() {
                                let path = pat_bdg.path.iter().map(|i| i.ident()).collect();
                                let def = Def::Let {
                                    ident: LocIdent::from(pat_bdg.id),
                                    value: &bdg.value,
                                    metadata: &bdg.metadata,
                                    path,
                                };
                                new_env.insert_def(def.clone());
                                self.add_sym(def);
                            }
                        }

                        for bdg in bindings.iter() {
                            self.fill(alloc, &bdg.value, if *rec { &new_env } else { env });
                        }

                        self.fill(alloc, body, &new_env);

                        TraverseControl::SkipBranch
                    }
                    Node::Record(record) => {
                        // Term::RecRecord(data, ..) | Term::Record(data) => {
                        let mut new_env = env.clone();

                        // Records are recursive and the order of fields is unimportant, so define
                        // all the fields in the environment and then recurse into their values.
                        for (ident, defs) in record.group_by_field_id().into_iter() {
                            let def = Def::Field {
                                ident,
                                pieces: todo!("we probably need to recursively descend in the record"),
                                record: ast,
                            };
                            new_env.insert_def(def.clone());
                            self.add_sym(def);
                        }

                        TraverseControl::ContinueWithScope(new_env)
                    }
                    Node::App { head, args } => {
                        if let Node::Match(data) = &head.node {
                            // panicking indexing: an application has always one argument, and the
                            // first one is the one being matched on, if the head is a match
                            // expressoin.
                            self.fill_match(alloc, env, data, Some(&args[0]));

                            // We've already traversed the branch bodies. We don't want to continue
                            // traversal because that will traverse them again. But we need to traverse
                            // the arguments of the application.
                            for arg in args.iter() {
                                self.fill(alloc, arg, env);
                            }

                            TraverseControl::SkipBranch
                        } else {
                            TraverseControl::Continue
                        }
                    }
                    Node::Match(data) => {
                        self.fill_match(alloc, env, data, None);
                        TraverseControl::SkipBranch
                    }
                    Node::Var(id) => {
                        let id = LocIdent::from(*id);

                        if let Some(def) = env.get(&id.ident) {
                            if let Some(span) = def.loc_ident().pos.into_opt() {
                                self.usage_table.entry(span).or_default().push(id);
                            }
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
    use nickel_lang_core::{
        bytecode::ast::{AstAlloc, Node},
        files::FileId,
        identifier::Ident,
        position::RawSpan,
        term::Term,
    };

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
        let alloc = AstAlloc::new();

        let (file, ast) = parse(&alloc, "let x = 1 in x + x");
        let x = Ident::new("x");
        let x0 = locced(x, file, 4..5);
        let x1 = locced(x, file, 13..14);
        let x2 = locced(x, file, 17..18);
        let table = UsageLookup::new(&alloc, &ast, &Environment::new());

        assert_eq!(
            table.usages(&x0.pos.unwrap()).cloned().collect::<Vec<_>>(),
            vec![x1, x2]
        );
        assert_eq!(table.def(&x1), table.def(&x2));
        assert_eq!(table.def(&x0), table.def(&x1));

        let def = table.def(&x1).unwrap();
        assert_eq!(def.loc_ident(), x0);
        assert_eq!(def.values().len(), 1);
        assert_matches!(&def.values().first().unwrap().node, Node::Number(_));
    }

    #[test]
    fn pattern_path() {
        let alloc = AstAlloc::new();

        let (file, ast) = parse(
            &alloc,
            "let x@{ foo = a@{ bar = baz } } = 'Undefined in x + a + baz",
        );
        let x0 = locced("x", file, 4..5);
        let x1 = locced("x", file, 48..49);
        let a0 = locced("a", file, 14..15);
        let a1 = locced("a", file, 52..53);
        let baz0 = locced("baz", file, 24..27);
        let baz1 = locced("baz", file, 56..59);
        let table = UsageLookup::new(&alloc, &ast, &Environment::new());

        assert_eq!(
            table.usages(&x0.pos.unwrap()).cloned().collect::<Vec<_>>(),
            vec![x1]
        );
        assert_eq!(
            table.usages(&a0.pos.unwrap()).cloned().collect::<Vec<_>>(),
            vec![a1]
        );
        assert_eq!(
            table
                .usages(&baz0.pos.unwrap())
                .cloned()
                .collect::<Vec<_>>(),
            vec![baz1]
        );

        let x_def = table.def(&x1).unwrap();
        assert_eq!(x_def.loc_ident(), x0);
        assert!(x_def.path().is_empty());

        let a_def = table.def(&a1).unwrap();
        assert_eq!(a_def.loc_ident(), a0);
        assert_eq!(a_def.path(), &["foo".into()]);

        let baz_def = table.def(&baz1).unwrap();
        assert_eq!(baz_def.loc_ident(), baz0);
        assert_eq!(baz_def.path(), vec!["foo".into(), "bar".into()]);
    }

    #[test]
    fn record_bindings() {
        let alloc = AstAlloc::new();

        let (file, ast) = parse(
            &alloc,
            "{ foo = 1, sub.field = 2, bar = foo, baz = sub.field, child = { bar = foo } }",
        );

        let foo0 = locced("foo", file, 2..5);
        let foo1 = locced("foo", file, 32..35);
        let foo2 = locced("foo", file, 70..73);
        let sub0 = locced("sub", file, 11..14);
        let sub1 = locced("sub", file, 43..46);
        let field1 = locced("field", file, 47..52);
        let table = UsageLookup::new(&alloc, &ast, &Environment::new());

        assert_eq!(table.def(&foo1).unwrap().loc_ident(), foo0);
        assert_eq!(table.def(&foo2).unwrap().loc_ident(), foo0);
        assert_eq!(table.def(&sub1).unwrap().loc_ident(), sub0);

        // We don't see "baz = sub.field" as a "usage" of field, because it's
        // a static access and not a var.
        assert!(table.def(&field1).is_none());
    }
}
