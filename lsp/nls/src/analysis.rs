use std::collections::HashMap;

use nickel_lang_core::{
    bytecode::ast::{primop::PrimOp, typ::Type, Ast, AstAlloc, Node},
    cache::{AstImportResolver, ImportData, SourceCache},
    error::{ParseError, ParseErrors, TypecheckError},
    files::FileId,
    identifier::Ident,
    parser::{self, lexer::Lexer, ErrorTolerantParser},
    position::RawSpan,
    traverse::{TraverseAlloc, TraverseControl},
    typ::TypeF,
    typecheck::{
        mk_initial_ctxt,
        reporting::{NameReg, ToType},
        typecheck_visit, Context as TypeContext, TypeTables, TypecheckMode, TypecheckVisitor,
        UnifType,
    },
};

use crate::{
    field_walker::{Def, EltId},
    identifier::LocIdent,
    position::PositionLookup,
    term::AstPtr,
    usage::{Environment, UsageLookup},
    world::{ImportTargets, WorldImportResolver},
};

#[derive(Clone, Debug)]
pub struct Parent<'ast> {
    pub ast: &'ast Ast<'ast>,
    pub child_name: Option<EltId>,
}

impl<'ast> From<&'ast Ast<'ast>> for Parent<'ast> {
    fn from(ast: &'ast Ast<'ast>) -> Self {
        Parent {
            ast,
            child_name: None,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ParentLookup<'ast> {
    table: HashMap<AstPtr<'ast>, Parent<'ast>>,
}

impl<'ast> ParentLookup<'ast> {
    pub fn new(ast: &'ast Ast<'ast>) -> Self {
        let mut table = HashMap::new();

        fn traversal<'ast>(
            ast: &'ast Ast<'ast>,
            parent: &Option<Parent<'ast>>,
            acc: &mut HashMap<AstPtr<'ast>, Parent<'ast>>,
        ) -> TraverseControl<Option<Parent<'ast>>, ()> {
            if let Some(parent) = parent {
                acc.insert(AstPtr(ast), parent.clone());
            }

            match ast.node {
                Node::Record(data) => {
                    todo!();
                    // for (name, field) in &data.fields {
                    //     if let Some(child) = &field.value {
                    //         let parent = Parent {
                    //             ast: ast.clone(),
                    //             child_name: Some(name.ident().into()),
                    //         };
                    //         child.traverse_ref(
                    //             &mut |rt, parent| traversal(rt, parent, acc),
                    //             &Some(parent),
                    //         );
                    //     }
                    // }

                    // if let Term::RecRecord(_, dynamic, _) = ast.as_ref() {
                    //     let parent = Parent {
                    //         ast: ast.clone(),
                    //         child_name: None,
                    //     };
                    //     for (name_term, field) in dynamic {
                    //         name_term.traverse_ref(
                    //             &mut |rt, parent| traversal(rt, parent, acc),
                    //             &Some(parent.clone()),
                    //         );
                    //         if let Some(child) = &field.value {
                    //             child.traverse_ref(
                    //                 &mut |rt, parent| traversal(rt, parent, acc),
                    //                 &Some(parent.clone()),
                    //             );
                    //         }
                    //     }
                    // }
                    TraverseControl::SkipBranch
                }
                Node::Array(elts) => {
                    for elt in elts.iter() {
                        let parent = Parent {
                            ast,
                            child_name: Some(EltId::ArrayElt),
                        };
                        elt.traverse_ref(
                            &mut |rt, parent| traversal(rt, parent, acc),
                            &Some(parent),
                        );
                    }
                    TraverseControl::SkipBranch
                }
                _ => TraverseControl::ContinueWithScope(Some(ast.into())),
            }
        }

        ast.traverse_ref(&mut |ast, parent| traversal(ast, parent, &mut table), &None);

        ParentLookup { table }
    }

    pub fn parent(&self, ast: &'ast Ast<'ast>) -> Option<&Parent<'ast>> {
        self.table.get(&AstPtr(ast))
    }

    pub fn parent_chain(&self, ast: &'ast Ast<'ast>) -> ParentChainIter<'ast, '_> {
        let next = self.parent(ast).cloned();
        ParentChainIter {
            table: self,
            path: Some(Vec::new()),
            next,
        }
    }

    /// Is this parent lookup table empty?
    pub fn is_empty(&self) -> bool {
        self.table.is_empty()
    }
}

fn find_static_accesses<'ast>(ast: &'ast Ast<'ast>) -> HashMap<Ident, Vec<&'ast Ast<'ast>>> {
    let mut map: HashMap<Ident, Vec<&'ast Ast<'ast>>> = HashMap::new();

    ast.traverse_ref(
        &mut |ast: &'ast Ast<'ast>, _scope: &()| {
            if let Node::PrimOpApp {
                op: PrimOp::RecordStatAccess(id),
                ..
            } = &ast.node
            {
                map.entry(id.ident()).or_default().push(ast);
            }
            TraverseControl::Continue::<_, ()>
        },
        &(),
    );

    map
}

/// Essentially an iterator over pairs of `(ancestor, reversed_path_to_the_original)`.
///
/// For example, if we are iterating over the AST of `foo.bar.baz`, the iterator
/// should return
/// - ancestor `foo.bar`, path \[`baz`\]; and then
/// - ancestor `foo`, path [`baz`, `bar`].
///
/// If, during our iteration, we encounter an ancestor that isn't a record then the
/// path will be none from then on. For example, if we traverse `(some_fn foo.bar.baz).quux`
/// starting from the `baz` AST node then the first couple of terms will have paths like
/// the previous example, but after that we'll get
/// - ancestor `(some_fn foo.bar.baz)`, path `None`; and then
/// - ancestor `(some_fn foo.bar.baz).quux`, path `None`.
///
/// This is a "streaming iterator" in the sense that the returned data borrows from
/// our state. Since streaming iterators are not (yet) in rust's stdlib, we don't
/// implement any traits here, but just do it by hand.
///
/// For borrowck reasons, the iteration is done in two parts: `next` advances the iterator
/// and returns just the term part. `path` retrieves the path corresponding to the previous
/// `next` call.
pub struct ParentChainIter<'ast, 'a> {
    table: &'a ParentLookup<'ast>,
    path: Option<Vec<EltId>>,
    next: Option<Parent<'ast>>,
}

impl<'ast> ParentChainIter<'ast, '_> {
    pub fn next(&mut self) -> Option<&'ast Ast<'ast>> {
        if let Some(next) = self.next.take() {
            if let Some((ident, path)) = next.child_name.zip(self.path.as_mut()) {
                path.push(ident);
            }

            if !matches!(
                &next.ast.node,
                Node::Record(_) | Node::Annotated { .. } | Node::Array(..)
            ) && !matches!(&next.ast.node, Node::PrimOpApp { op, ..} if op.arity() == 2)
            {
                self.path = None;
            }
            self.next = self.table.parent(next.ast).cloned();

            Some(next.ast)
        } else {
            None
        }
    }

    /// Like `next`, but skips over everything except for merges, annotations, and records.
    pub fn next_merge(&mut self) -> Option<&'ast Ast<'ast>> {
        let is_fieldy_term = |ast: &Ast<'ast>| {
            matches!(
                &ast.node,
                Node::PrimOpApp {
                    op: PrimOp::Merge(_),
                    ..
                } | Node::Annotated { .. }
                    | Node::Record(_)
            )
        };

        let is_merge_term = |ast: &Ast<'ast>| {
            matches!(
                &ast.node,
                Node::PrimOpApp {
                    op: PrimOp::Merge(_),
                    ..
                } | Node::Annotated { .. }
            )
        };

        while let Some(p) = self.next() {
            // If a parent and a grandparent were both merges, we can skip the parent
            // because the grandparent will have a superset of its fields. This prevents
            // quadratic behavior on long chains of merges.
            if let Some(gp) = self.peek_gp() {
                if is_merge_term(gp) {
                    continue;
                }
            }

            if is_fieldy_term(&p) {
                return Some(p);
            }
        }
        None
    }

    pub fn path(&self) -> Option<&[EltId]> {
        self.path.as_deref()
    }

    /// Peek at the grandparent.
    pub fn peek_gp(&self) -> Option<&'ast Ast<'ast>> {
        if let Some(Parent { ast, .. }) = &self.next {
            self.table.parent(ast).map(|gp| gp.ast)
        } else {
            None
        }
    }
}

/// The initial analysis that we collect for a file.
///
/// This analysis is re-collected from scratch each time the file is updated.
#[derive(Default, Debug)]
pub struct Analysis<'ast> {
    pub position_lookup: PositionLookup<'ast>,
    pub usage_lookup: UsageLookup<'ast>,
    pub parent_lookup: ParentLookup<'ast>,
    pub type_lookup: CollectedTypes<'ast, Type<'ast>>,
    /// A lookup table for static accesses, for looking up all occurrences of,
    /// say, `.foo` in a file.
    pub static_accesses: HashMap<Ident, Vec<&'ast Ast<'ast>>>,
}

impl<'ast> Analysis<'ast> {
    pub fn new(
        alloc: &'ast AstAlloc,
        ast: &'ast Ast<'ast>,
        type_lookup: CollectedTypes<'ast, Type<'ast>>,
        initial_env: &Environment<'ast>,
    ) -> Self {
        Self {
            position_lookup: PositionLookup::new(ast),
            usage_lookup: UsageLookup::new(alloc, ast, initial_env),
            parent_lookup: ParentLookup::new(ast),
            static_accesses: find_static_accesses(ast),
            type_lookup,
        }
    }

    /// Is this analysis filled yet, or is it empty?
    pub fn is_empty(&self) -> bool {
        self.position_lookup.is_empty()
            && self.usage_lookup.is_empty()
            && self.parent_lookup.is_empty()
            && self.type_lookup.is_empty()
            && self.static_accesses.is_empty()
    }
}

/// The collection of analyses for every file that we know about.
#[derive(Debug)]
pub struct AnalysisRegistry {
    /// Map of the analyses of each live file.
    ///
    /// Most of the fields of `Analysis` are themselves hash tables. Having a table of tables
    /// requires more lookups than necessary, but it makes it easy to invalidate a whole file.
    pub analyses: HashMap<FileId, PackedAnalysis>,
    /// The analysis corresponding to the `std` module of the stdlib. It's separate because we want
    /// to guarantee that it's live for the whole duration of [Self]. Having it there alone isn't
    /// sufficient to enforce this invariant, but it makes it harder to remove it from
    /// [Self::analysis_reg] by accident. Also, this field isn't public.
    ///
    /// # Safety
    ///
    /// **This analysis must not be dropped or taken out before [Self] is dropped, or Undefined
    /// Behavior will ensue.**
    stdlib_analysis: PackedAnalysis,
    /// The initial environment, analyzed from the stdlib. Its content is allocated into
    /// [Self::stdlib_analysis], which must thus be guaranteed to live as long as `self`. Thus,
    /// once we've initialized the stdlib, we shouldn't touch its analysis anymore.
    ///
    /// # Safety
    ///
    /// This environment isn't actually `'static`: it's an internal placeholder because the
    /// lifetime is self-referential (morally, it's `'self`). Do not use this field directly; use
    /// [Self] helpers instead.
    initial_term_env: Environment<'static>,
    /// The initial typing environment, created from the stdlib.
    ///
    /// Its content is allocated into [Self::stdlib_analysis], which must thus be guaranteed to
    /// live as long as `self`. Thus, once we've initialized the stdlib, we shouldn't touch its
    /// analysis anymore.
    ///
    /// # Safety
    ///
    /// This environment isn't actually `'static`: it's an internal placeholder because the
    /// lifetime is self-referential (morally, it's `'self`). Do not use this field directly; use
    /// [Self] helpers instead.
    initial_type_ctxt: TypeContext<'static>,
}

/// This block gathers the analysis for a single `FileId`, together with the corresponding
/// allocator.
///
/// # Memory management
///
/// Files are individually modified many times, at which point we need to ditch the previous
/// analysis, parse and typecheck again etc. Thus, as opposed to the standard Nickel pipeline, we
/// can't afford to allocate all ASTs and related objects in one central arena. The latter would
/// need to live forever in the LSP, which would quickly leak memory.
///
/// However, the analysis of one file is a single unit of objects that are created and dropped
/// together. We thus use one arena per file analysis. This is the `PackedAnalysis` struct.
#[derive(Debug)]
pub(crate) struct PackedAnalysis {
    alloc: AstAlloc,
    /// The id of the analyzed file.
    file_id: FileId,
    /// The corresponding parsed AST. It is initialized with a static reference to a `null` value,
    /// and properly set after the file is parsed.
    ///
    /// We need the usual trick to cope with such a referential struct (where the lifetime of the
    /// analysis is tied to the lifetime of `self`): we use a `'static` lifetime as a place holder
    /// and implement a few safe methods to borrow from it.
    ast: Ast<'static>,
    /// The non-fatal parse errors that occurred while parsing [Self::ast], if any.
    parse_errors: ParseErrors,
    /// The analysis of the current file.
    analysis: Analysis<'static>,
}

impl PackedAnalysis {
    /// Create a new packed analysis with a fresh arena and an empty analysis.
    pub(crate) fn new(file_id: FileId) -> Self {
        Self {
            alloc: AstAlloc::new(),
            file_id,
            ast: Default::default(),
            parse_errors: Default::default(),
            analysis: Default::default(),
        }
    }

    pub(crate) fn analysis<'ast>(&'ast self) -> &'ast Analysis<'ast> {
        // Safety: We know that the `'static` lifetime is actually the lifetime of `self`.
        unsafe {
            std::mem::transmute::<&'ast Analysis<'static>, &'ast Analysis<'ast>>(&self.analysis)
        }
    }

    pub(crate) fn analysis_mut<'ast>(&'ast mut self) -> &'ast mut Analysis<'ast> {
        // Safety: We know that the `'static` lifetime is actually the lifetime of `self`.
        unsafe {
            std::mem::transmute::<&'ast mut Analysis<'static>, &'ast mut Analysis<'ast>>(
                &mut self.analysis,
            )
        }
    }

    pub(crate) fn ast<'ast>(&'ast self) -> &'ast Ast<'ast> {
        &self.ast
    }

    pub(crate) fn ast_mut<'ast>(&'ast mut self) -> &'ast mut Ast<'ast> {
        // Safety: We know that the `'static` lifetime is actually the lifetime of `self`.
        unsafe { std::mem::transmute::<&'ast Ast<'static>, &'ast mut Ast<'ast>>(&self.ast) }
    }

    pub(crate) fn alloc<'ast>(&'ast self) -> &'ast AstAlloc {
        &self.alloc
    }

    pub(crate) fn file_id(&self) -> FileId {
        self.file_id
    }

    pub(crate) fn parse_errors(&self) -> &ParseErrors {
        &self.parse_errors
    }

    /// Parse the corresonding file_id and fill [Self::ast] with the result. Stores non-fatal parse
    /// errors in [Self::parse_errors], or fail on fatal errors.
    pub(crate) fn parse(&mut self, sources: &SourceCache) -> Result<(), ParseError> {
        let source = sources.source(self.file_id);
        let (ast, errors) = parser::grammar::TermParser::new().parse_tolerant(
            &self.alloc,
            self.file_id,
            Lexer::new(source),
        )?;

        // Safety: `'static` is a placeholder for `'self`. Since we allocate the ast with
        // `self.alloc`, and that we never drop the allocator before the whole [Self] is dropped,
        // `ast` will live long enough.
        self.ast = unsafe { std::mem::transmute::<Ast<'_>, Ast<'static>>(ast) };
        self.parse_errors = errors;

        Ok(())
    }

    /// Typecheck and analyze the given file, storing the result in this packed analysis.
    ///
    /// `reg` might be `None` during the initialization of the stdlib, because we need to fill the
    /// analysis of `std` first before initializing the registry.
    pub(crate) fn fill_analysis<'a>(
        &'a mut self,
        sources: &mut SourceCache,
        import_data: &mut ImportData,
        import_targets: &mut ImportTargets,
        reg: Option<&'a AnalysisRegistry>,
    ) -> Result<Vec<PackedAnalysis>, Vec<TypecheckError>> {
        // if let Ok(CacheOp::Done((ids, errors))) = self.resolve_imports(file_id) {
        //     import_errors = errors;
        //     // Reverse the imports, so we try to typecheck the leaf dependencies first.
        //     for &id in ids.iter().rev() {
        //         let _ = self.typecheck_with_analysis(id, initial_term_env, registry);
        //     }
        // }

        // for id in self.import_data.get_imports(file_id) {
        //     // If we have typechecked a file correctly, its imports should be
        //     // in the `registry`. The imports that are not in `registry`
        //     // were not typechecked correctly.
        //     if !registry.analyses.contains_key(&id) {
        //         typecheck_import_diagnostics.push(id);
        //     }
        // }

        let mut collector = TypeCollector::default();

        let alloc = &self.alloc;
        let ast = Self::borrow_ast(&self.ast, alloc);

        let mut resolver = WorldImportResolver {
            reg,
            new_imports: Vec::new(),
            sources,
            import_data,
            import_targets,
        };

        let type_tables = typecheck_visit(
            alloc,
            ast,
            reg.map(AnalysisRegistry::initial_type_ctxt)
                .unwrap_or_default(),
            &mut resolver,
            &mut collector,
            TypecheckMode::Walk,
        )
        .map_err(|err| vec![err])?;

        let new_imports = std::mem::take(&mut resolver.new_imports);
        let type_lookups = collector.complete(alloc, type_tables);

        // Safety: everything that we store in the current analysis is borrowed from
        // `self.ast`/`self.alloc`, or from `reg.initial_term_env` which is guaranteed to live as
        // long as `self`.
        self.analysis = unsafe {
            std::mem::transmute::<Analysis<'_>, Analysis<'static>>(Analysis::new(
                alloc,
                ast,
                type_lookups,
                &reg.map(AnalysisRegistry::initial_term_env).unwrap_or_default(),
            ))
        };

        Ok(new_imports)
    }

    /// Only generates usage analysis for the current file.
    ///
    /// This is useful for temporary little pieces of input (like parts extracted from incomplete
    /// input) that need variable resolution but not the full analysis.
    pub fn fill_usage<'ast>(
        &'ast mut self,
        initial_env: &Environment<'ast>,
    ) {
        let alloc = &self.alloc;
        let ast = Self::borrow_ast(&self.ast, alloc);

        let new_analysis = Analysis {
            usage_lookup: UsageLookup::new(&self.alloc, ast, initial_env),
            ..Default::default() 
        };
        
        //TODO: is it safe to use Environment here? It's allocated in the parent file, so we have
        //no guarantee that this will stay alive long enough. Maybe we should 
        self.analysis = unsafe {
            std::mem::transmute::<Analysis<'_>, Analysis<'static>>(new_analysis)
        };
    }

    fn borrow_ast<'ast>(ast: &'ast Ast<'static>, _alloc: &'ast AstAlloc) -> &'ast Ast<'ast> {
        // Safety: We know that the `'static` lifetime is actually the lifetime of `self`.
        unsafe { std::mem::transmute::<&Ast<'static>, &'ast Ast<'ast>>(ast) }
    }
}

impl AnalysisRegistry {
    pub fn new(stdlib_analysis: PackedAnalysis, initial_term_env: Environment<'static>) -> Self {
        let std_env = vec![(
            nickel_lang_core::stdlib::StdlibModule::Std,
            stdlib_analysis.ast(),
        )];
        let initial_type_ctxt = unsafe {
            std::mem::transmute::<TypeContext<'_>, TypeContext<'static>>(
                mk_initial_ctxt(stdlib_analysis.alloc(), &std_env)
                    .expect("failed to make an initial typing context ouf of the stdlib"),
            )
        };

        Self {
            analyses: HashMap::new(),
            stdlib_analysis,
            initial_term_env,
            initial_type_ctxt,
        }
    }

    pub fn initial_type_ctxt<'ast>(&'ast self) -> TypeContext<'ast> {
        unsafe {
            std::mem::transmute::<TypeContext<'static>, TypeContext<'ast>>(
                self.initial_type_ctxt.clone(),
            )
        }
    }

    pub fn initial_term_env<'ast>(&'ast self) -> Environment<'ast> {
        unsafe {
            std::mem::transmute::<Environment<'static>, Environment<'ast>>(
                self.initial_term_env.clone(),
            )
        }
    }

    pub fn set_stdlib(
        &mut self,
        stdlib_analysis: PackedAnalysis,
        initial_term_env: Environment<'static>,
    ) {
        self.stdlib_analysis = stdlib_analysis;
        self.initial_term_env = initial_term_env;
    }

    pub fn insert(&mut self, analysis: PackedAnalysis) {
        if analysis.file_id() == self.stdlib_analysis.file_id() {
            // Panicking there is a bit exaggerated, but it's a bug to re-analyse the stdlib
            // several times. At least we'll catch it.
            panic!("tried to insert the stdlib analysis into the registry, but was already there");
        }

        self.analyses.insert(analysis.file_id, analysis);
    }

    /// Inserts a new file into the analysis, but only generates usage analysis for it.
    ///
    /// This is useful for temporary little pieces of input (like parts extracted from incomplete input)
    /// that need variable resolution but not the full analysis.
    // pub fn insert_usage<'ast>(
    //     &'ast mut self,
    //     file_id: FileId,
    //     term: &'ast Ast<'ast>,
    // ) {
    //     self.analyses.insert(
    //         file_id,
    //         Analysis {
    //             usage_lookup: UsageLookup::new(term, initial_env),
    //             ..Default::default()
    //         },
    //     );
    // }

    pub fn get(&self, file_id: FileId) -> Option<&PackedAnalysis> {
        if file_id == self.stdlib_analysis.file_id() {
            Some(&self.stdlib_analysis)
        } else {
            self.analyses.get(&file_id)
        }
    }

    pub fn remove(&mut self, file_id: FileId) -> Option<PackedAnalysis> {
        self.analyses.remove(&file_id)
    }

    pub fn get_def(&self, ident: &LocIdent) -> Option<&Def> {
        let file = ident.pos.as_opt_ref()?.src_id;
        self.analyses.get(&file)?.analysis().usage_lookup.def(ident)
    }

    pub fn get_usages(&self, span: &RawSpan) -> impl Iterator<Item = &LocIdent> {
        fn inner<'a>(
            slf: &'a AnalysisRegistry,
            span: &RawSpan,
        ) -> Option<impl Iterator<Item = &'a LocIdent>> {
            let file = span.src_id;
            Some(slf.analyses.get(&file)?.analysis.usage_lookup.usages(span))
        }

        inner(self, span).into_iter().flatten()
    }

    pub fn get_env<'ast>(&'ast self, ast: &'ast Ast<'ast>) -> Option<&'ast Environment<'ast>> {
        let file = ast.pos.as_opt_ref()?.src_id;
        self.analyses.get(&file)?.analysis().usage_lookup.env(ast)
    }

    pub fn get_type<'ast>(&'ast self, ast: &'ast Ast<'ast>) -> Option<&'ast Type<'ast>> {
        let file = ast.pos.as_opt_ref()?.src_id;
        self.analyses
            .get(&file)?
            .analysis()
            .type_lookup
            .terms
            .get(&AstPtr(ast))
    }

    pub fn get_type_for_ident(&self, id: &LocIdent) -> Option<&Type> {
        let file = id.pos.as_opt_ref()?.src_id;
        self.analyses
            .get(&file)?
            .analysis()
            .type_lookup
            .idents
            .get(id)
    }

    pub fn get_parent_chain<'ast>(
        &'ast self,
        ast: &'ast Ast<'ast>,
    ) -> Option<ParentChainIter<'ast, 'ast>> {
        let file = ast.pos.as_opt_ref()?.src_id;
        Some(
            self.analyses
                .get(&file)?
                .analysis()
                .parent_lookup
                .parent_chain(ast),
        )
    }

    pub fn get_parent<'ast>(&'ast self, ast: &'ast Ast<'ast>) -> Option<&'ast Parent<'ast>> {
        let file = ast.pos.as_opt_ref()?.src_id;
        self.analyses
            .get(&file)?
            .analysis()
            .parent_lookup
            .parent(ast)
    }

    pub fn get_static_accesses<'ast>(&'ast self, id: Ident) -> Vec<&'ast Ast<'ast>> {
        self.analyses
            .values()
            .filter_map(|a| a.analysis().static_accesses.get(&id))
            .flatten()
            .cloned()
            .collect()
    }
}

#[derive(Debug, Default)]
pub struct TypeCollector<'ast> {
    tables: CollectedTypes<'ast, UnifType<'ast>>,
}

#[derive(Clone, Debug)]
pub struct CollectedTypes<'ast, Ty> {
    pub terms: HashMap<AstPtr<'ast>, Ty>,
    pub idents: HashMap<LocIdent, Ty>,
}

impl<Ty> CollectedTypes<'_, Ty> {
    /// Is this type collection empty?
    pub fn is_empty(&self) -> bool {
        self.terms.is_empty() && self.idents.is_empty()
    }
}

impl<'ast, Ty> Default for CollectedTypes<'ast, Ty> {
    fn default() -> Self {
        Self {
            terms: Default::default(),
            idents: Default::default(),
        }
    }
}

impl<'ast> TypecheckVisitor<'ast> for TypeCollector<'ast> {
    fn visit_term(&mut self, ast: &'ast Ast<'ast>, ty: UnifType<'ast>) {
        self.tables.terms.insert(AstPtr(ast), ty);
    }

    fn visit_ident(
        &mut self,
        ident: &nickel_lang_core::identifier::LocIdent,
        new_type: UnifType<'ast>,
    ) {
        self.tables.idents.insert((*ident).into(), new_type);
    }
}

impl<'ast> TypeCollector<'ast> {
    pub fn complete(
        self,
        alloc: &'ast AstAlloc,
        type_tables: TypeTables<'ast>,
    ) -> CollectedTypes<'ast, Type<'ast>> {
        let mut name_reg = NameReg::new(type_tables.names.clone());

        let mut transform_type = |uty: UnifType<'ast>| -> Type<'ast> {
            let ty = uty.to_type(alloc, &mut name_reg, &type_tables.table);
            match ty.typ {
                TypeF::Wildcard(i) => type_tables.wildcards.get(i).unwrap_or(&ty).clone(),
                _ => ty,
            }
        };

        // See [^disable-clippy-mutable-key-type]
        #[allow(clippy::mutable_key_type)]
        let terms = self
            .tables
            .terms
            .into_iter()
            .map(|(ast_ptr, uty)| (ast_ptr, transform_type(uty)))
            .collect();
        let idents = self
            .tables
            .idents
            .into_iter()
            .map(|(id, uty)| (id, transform_type(uty)))
            .collect();
        CollectedTypes { terms, idents }
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use codespan::ByteIndex;
    use nickel_lang_core::{
        bytecode::ast::{AstAlloc, Node},
        files::Files,
        identifier::Ident,
        parser::{grammar, lexer, ErrorTolerantParser as _},
    };

    use crate::{
        field_walker::EltId,
        position::{tests::parse, PositionLookup},
        usage::{tests::locced, Environment, UsageLookup},
    };

    use super::ParentLookup;

    #[test]
    fn parent_chain() {
        let alloc = AstAlloc::new();

        let (file, rt) = parse(&alloc, "{ foo = [{ bar = 1 }] }");
        let bar_id = Ident::new("bar");
        let bar = locced(bar_id, file, 11..14);

        let parent = ParentLookup::new(&rt);
        let usages = UsageLookup::new(&alloc, &rt, &Environment::new());
        let values = usages.def(&bar).unwrap().values();

        assert_eq!(values.len(), 1);
        let bar_val = values.into_iter().next().unwrap();

        let p = parent.parent(bar_val).unwrap();
        assert_eq!(p.child_name, Some(EltId::Ident(bar_id)));
        assert_matches!(&p.ast.node, Node::Record(_));

        let gp = parent.parent(&p.ast).unwrap();
        assert_eq!(gp.child_name, Some(EltId::ArrayElt));
        assert_matches!(&gp.ast.node, Node::Array { .. });

        let ggp = parent.parent(&gp.ast).unwrap();
        assert_matches!(ggp.child_name, Some(EltId::Ident(_)));
        assert_matches!(&ggp.ast.node, Node::Record(_));
    }

    #[test]
    fn parse_error_parent() {
        let alloc = AstAlloc::new();

        // The field that fails to parse should have a record as its parent.
        let s = "{ field. }";
        let file = Files::new().add("<test>", s.to_owned());

        let (ast, _errors) = grammar::TermParser::new()
            .parse_tolerant(&alloc, file, lexer::Lexer::new(s))
            .unwrap();

        let parent = ParentLookup::new(&ast);
        let positions = PositionLookup::new(&ast);
        let err = positions.get(ByteIndex(5)).unwrap();

        dbg!(&ast, err);

        let p = parent.parent(err).unwrap();
        assert!(p.child_name.is_none());
        assert_matches!(&p.ast.node, Node::Record(_));
    }
}
