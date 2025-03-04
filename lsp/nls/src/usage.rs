use std::collections::hash_map::{Entry, HashMap};

use nickel_lang_core::{
    bytecode::ast::{
        pattern::bindings::Bindings as _, record::FieldPathElem, Ast, AstAlloc, Match, Node,
    },
    environment::Environment as GenericEnvironment,
    identifier::Ident,
    position::RawSpan,
    traverse::{TraverseAlloc, TraverseControl},
    typecheck::AnnotSeqRef,
};

use crate::{
    field_walker::{Def, FieldDefPiece},
    identifier::LocIdent,
};

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

/// Data structure that is used to aggregate the data of piecewise field definitions.
///
/// For example, in the record `{ foo.bar.baz = 1, foo.qux = 2 }`, the aggregated defs are:
///
/// - `def` is a definition of `foo` that refers to the pieces `bar.baz = 1`, `qux = 2`.
/// - `subdefs` is the map `{ bar => .., qux => ... }`, where `bar` and `qux` are themselves
///   aggregate definitions.
struct AggregatedDef<'ast> {
    /// The pieces of this definition.
    pieces: Vec<FieldDefPiece<'ast>>,
    /// Potential subdefinitions.
    subdefs: HashMap<Ident, AggregatedDef<'ast>>,
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
        // self.syms.get(ident).or_else(|| {
        //     ident
        //         .pos
        //         .as_opt_ref()
        //         .and_then(|span| self.def_table.get(span))
        //         .and_then(|env| env.get(&ident.ident))
        // })

        eprintln!("Looking for the def of {}", ident.ident);

        let as_sym = self.syms.get(ident);
        if as_sym.is_none() {
            eprintln!("Not found in symbols table");
        }
        let id_pos = ident.pos.as_opt_ref();
        if id_pos.is_none() {
            eprintln!("The ident has no position...");
        }
        let as_def_table = id_pos.clone().and_then(|span| self.def_table.get(span));
        if as_def_table.is_none() {
            eprintln!("Data not found in def table for this span");
        }
        let from_env = as_def_table.and_then(|env| env.get(&ident.ident));
        if from_env.is_none() {
            eprintln!("Id not found in environment");
            if as_def_table.is_some() {
                eprintln!(
                    "Available: {:?}",
                    as_def_table
                        .unwrap()
                        .iter()
                        .map(|(k, _)| k.to_string())
                        .collect::<Vec<String>>()
                );
            }
        }

        as_sym.or(from_env)
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
                    eprintln!("Inserting in def table @ {span:?}");
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
                        // When traversing a record with potentially piece-wise definitions, we
                        // need to make a first pass to collect all the field definitions. For
                        // example:
                        //
                        // `{foo.bar.baz = 1, foo.bar.qux = 2, one.two = "a", one.three = "b"}`
                        //
                        // We want to collect the following definitions:
                        //
                        // - `foo` (defined by two pieces, `bar.baz = 1` and `bar.qux = 2`)
                        // - `foo.bar` (defined by two pieces, `baz = 1` and `qux = 2`)
                        // - `foo.bar.baz` and `for.bar.qux`, which are leaf definitions
                        // - `one` (defined by twodefinition pieces, `two = "a"` and `three = "b"`)
                        // - `one.two` and `one.three`, which are leaf definitions
                        //
                        // In a second step, we build the proper environment for each field
                        // definition and visit it.
                        let mut agg_defs = HashMap::new();

                        for field_def in record.field_defs.iter() {
                            let mut index = 0;
                            let mut cursor = &mut agg_defs;

                            // For a definition of the form `x.y.z = <value>`, we create or update
                            // the corresponding aggregated and nested definitions.
                            //
                            // If we encounter any dynamically defined field, as in `x."%{y}".z =
                            // <value>`, we stop. Note that we will still need to add `z` to the
                            // environment of `<value>` when filling usage information, but this
                            // definition doesn't need to be aggregated with the rest of the record
                            // - there is no way to statically pair "%{y}" and anything that
                            // follows with other parts of the current record. Such dynamic fields
                            // and the ones following them are handled on-the-fly in the second
                            // loop below, when recursing in the values of each definition.
                            while let Some(ident) = field_def
                                .path
                                .get(index)
                                .and_then(FieldPathElem::try_as_ident)
                            {
                                let def_piece = FieldDefPiece { index, field_def };

                                match cursor.entry(ident.ident()) {
                                    Entry::Vacant(entry) => {
                                        entry.insert(AggregatedDef {
                                            pieces: vec![def_piece],
                                            subdefs: HashMap::new(),
                                        });
                                    }
                                    Entry::Occupied(mut entry) => {
                                        entry.get_mut().pieces.push(def_piece);
                                    }
                                }

                                // It's unfortunate to use `get` again, but if we try to re-use
                                // `entry` directly to update `cursor`, we run into borrowing
                                // issues.
                                cursor = &mut cursor.get_mut(&ident.ident()).unwrap().subdefs;
                                index += 1;
                            }
                        }

                        // We can now build the recursive environment common to all fields.
                        let mut rec_env = env.clone();

                        eprintln!("Creating the recursive environment");
                        for id in agg_defs.keys() {
                            let def = Def::Field {
                                ident: id.clone(),
                                pieces: agg_defs[id].pieces.clone(),
                                record: ast,
                            };

                            rec_env.insert_def(def.clone());
                            // Note that with piecewise field definitions, a `Def` might not have
                            // one well-defined `LocIdent` anymore. While `def.loc_ident()` returns
                            // an arbitrary `LocIdent` (the first), if we want to be able to refer
                            // back to a definition from any of the defining piece site, we need to
                            // use a different key for each piece. Thus, we defer the insertion
                            // into the symbol table filling to the next loop below.
                        }

                        // Now that we've aggregated the definitions, we actually recurse into each
                        // definition.
                        for field_def in record.field_defs.iter() {
                            let mut local_env = rec_env.clone();
                            let mut cursor = Some(&agg_defs);

                            for (index, elt) in field_def.path.iter().enumerate() {
                                eprintln!("Processing field path element {index}: {:?}", elt);

                                match elt {
                                    // The first element of the path is already handled by the
                                    // recursive environment, but we still need to fill the symbol
                                    // table with the right `LocIdent` - see the building of the
                                    // recursive enviroment above.
                                    FieldPathElem::Ident(id) if index == 0 => {
                                        // unwrap(): we should have an aggregate definition for
                                        // each top-level field, and we put them in the recursive
                                        // environment, therefore it must be there.
                                        self.syms.insert((*id).into(), rec_env.get(&id.ident()).unwrap().clone());
                                    }
                                    FieldPathElem::Ident(id) => {
                                        let def = if let Some(agg_def) = cursor
                                            .take()
                                            .and_then(|agg_defs| agg_defs.get(&id.ident()))
                                        {
                                            eprintln!("Found aggregate definition");

                                            let def = Def::Field {
                                                ident: id.ident(),
                                                pieces: agg_def.pieces.clone(),
                                                record: ast,
                                            };
                                            cursor = Some(&agg_def.subdefs);

                                            def
                                        }
                                        // Otherwise, we had a dynamic field earlier in the path,
                                        // and we need to refer to aggregate definitions.
                                        else {
                                            eprintln!("No aggregate definition found - we had a dynamic field before, or sth is wrong");

                                            Def::Field {
                                                ident: id.ident(),
                                                pieces: vec![FieldDefPiece { index, field_def }],
                                                record: ast,
                                            }
                                        };

                                        local_env.insert_def(def.clone());
                                        self.syms.insert((*id).into(), def);
                                    }
                                    FieldPathElem::Expr(expr) => {
                                        // After a dynamic field we stop looking into aggregate
                                        // definitions.
                                        eprintln!("Dynamic field, stopping aggregation");
                                        self.fill(alloc, expr, &local_env);
                                        cursor = None;
                                    }
                                }
                            }

                            for typ in field_def.metadata.annotation.iter() {
                                eprintln!("Traversing type annotations");

                                typ.traverse_ref::<_, ()>(
                                    &mut |ast: &'ast Ast<'ast>, env: &Environment<'ast>| {
                                        self.fill(alloc, ast, env);

                                        TraverseControl::Continue
                                    },
                                    &local_env,
                                );
                            }

                            if let Some(value) = field_def.value.as_ref() {
                                eprintln!("Processing value");

                                self.fill(alloc, value, &local_env);
                            }
                        }

                        TraverseControl::SkipBranch
                    }
                    Node::App { head, args } => {
                        if let Node::Match(data) = &head.node {
                            // panicking indexing: an application has always one argument, and the
                            // first one is the one being matched on, if the head is a match
                            // expression.
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
                            eprintln!("Found def for {} usage", id.ident);

                            if let Some(span) = def.loc_ident().pos.into_opt() {
                                self.usage_table.entry(span).or_default().push(id);
                            }
                        }
                        else {
                            eprintln!("No def found for {} usage!", id.ident);
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
