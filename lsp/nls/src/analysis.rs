use std::collections::HashMap;

use lsp_server::{ErrorCode, ResponseError};
use ouroboros::self_referencing;

use nickel_lang_core::{
    bytecode::ast::{primop::PrimOp, record::FieldPathElem, typ::Type, Ast, AstAlloc, Node},
    cache::{ImportData, SourceCache},
    error::{ParseErrors, TypecheckError},
    files::FileId,
    identifier::Ident,
    parser::{
        self,
        lexer::{Lexer, OffsetLexer},
        FullyErrorTolerantParser as _,
    },
    position::{RawPos, RawSpan, TermPos},
    stdlib::StdlibModule,
    traverse::{TraverseAlloc, TraverseControl},
    typ::TypeF,
    typecheck::{
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
    world::{ImportTargets, StdlibResolver, WorldImportResolver},
};

/// The parent of an AST node.
#[derive(Clone, Debug)]
pub struct Parent<'ast> {
    /// The parent.
    pub ast: &'ast Ast<'ast>,
    /// The path from the parent to the child. This is a vector because of field paths: in
    /// `{foo.bar.baz = 1}`, the path of `1` is a sequence of identifiers - there are no explicit
    /// intermediate ASTs. The path might be empty as well, for example an incomplete field definition
    /// which fails to parse in a record such as `{ field. }` or a dynamic field definition at the
    /// top-level such as `{ "%{<expr>}" = null }`.
    pub child_path: Vec<EltId>,
}

impl<'ast> From<&'ast Ast<'ast>> for Parent<'ast> {
    fn from(ast: &'ast Ast<'ast>) -> Self {
        Parent {
            ast,
            child_path: Vec::new(),
        }
    }
}

fn child_path_from_field_path(field_path: &[FieldPathElem<'_>]) -> Vec<EltId> {
    field_path
        .iter()
        .map(|elem| {
            if let Some(id) = elem.try_as_ident() {
                EltId::Ident(id.ident())
            } else {
                EltId::DynIdent
            }
        })
        .collect()
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
                    for def in data.field_defs.iter() {
                        let parent = Parent {
                            ast,
                            child_path: child_path_from_field_path(def.path),
                        };
                        def.traverse_ref(
                            &mut |ast, parent| traversal(ast, parent, acc),
                            &Some(parent),
                        );

                        // We rely on `FieldDef::traverse_ref` to avoid re-implementing the field
                        // definition traversal logic, but there is a small catch: the child path
                        // for dynamic field names in the field path and for the rest of the field
                        // definition are different.
                        //
                        // We look for dynamic field definitions in the field path and adjust their
                        // field paths. Those also include parse errors.
                        for (index, path_elem) in def.path.iter().enumerate() {
                            if let Some(expr) = path_elem.try_as_dyn_expr() {
                                // unwrap(): the traversal of a field definition visit the elements
                                // of the field path, so we must have seen this ast in the
                                // `def.traverse_ref` call above.
                                acc.get_mut(&AstPtr(expr))
                                    .unwrap()
                                    .child_path
                                    .truncate(index);
                            }
                        }
                    }

                    TraverseControl::SkipBranch
                }
                Node::Array(elts) => {
                    for elt in elts.iter() {
                        let parent = Parent {
                            ast,
                            child_path: vec![EltId::ArrayElt],
                        };

                        elt.traverse_ref(
                            &mut |ast, parent| traversal(ast, parent, acc),
                            &Some(parent),
                        );
                    }

                    TraverseControl::SkipBranch
                }
                Node::EnumVariant {
                    tag,
                    arg: Some(elt),
                } => {
                    let parent = Parent {
                        ast,
                        child_path: vec![EltId::Tag(tag.ident())],
                    };
                    elt.traverse_ref(
                        &mut |ast, parent| traversal(ast, parent, acc),
                        &Some(parent),
                    );
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
            rev_path: Some(Vec::new()),
            next,
        }
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
/// For example, if we are iterating over the AST `baz` within `foo.bar.baz`, the iterator should
/// return
///
/// - ancestor `foo.bar`, path \[`baz`\]; and then
/// - ancestor `foo`, path [`baz`, `bar`].
///
/// If, during our iteration, we encounter an ancestor that isn't a record then the
/// path will be none from then on. For example, if we traverse `(some_fn foo.bar.baz).quux`
/// starting from the `baz` AST node then the first couple of terms will have paths like
/// the previous example, but after that we'll get
///
/// - ancestor `(some_fn foo.bar.baz)`, path `None`; and then
/// - ancestor `(some_fn foo.bar.baz).quux`, path `None`.
///
/// This is a "streaming iterator" in the sense that the returned data borrows from
/// our state. Since streaming iterators are not (yet) in rust's stdlib, we don't
/// implement any traits here, but just do it by hand.
///
/// For borrowing reasons, the iteration is done in two parts: `next` advances the iterator and
/// returns just the term part. `path` retrieves the path corresponding to the previous `next`
/// call.
pub struct ParentChainIter<'ast, 'a> {
    table: &'a ParentLookup<'ast>,
    rev_path: Option<Vec<EltId>>,
    next: Option<Parent<'ast>>,
}

impl<'ast> ParentChainIter<'ast, '_> {
    pub fn next(&mut self) -> Option<&'ast Ast<'ast>> {
        if let Some(next) = self.next.take() {
            if let Some(path) = self.rev_path.as_mut() {
                path.extend(next.child_path.iter().cloned());
            }

            if !matches!(
                &next.ast.node,
                Node::Record(_)
                    | Node::Annotated { .. }
                    | Node::Array(..)
                    | Node::EnumVariant { .. }
            ) && !matches!(&next.ast.node, Node::PrimOpApp { op, ..} if op.arity() == 2)
            {
                self.rev_path = None;
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
                    op: PrimOp::Merge(_) | PrimOp::MergeContract,
                    ..
                } | Node::Annotated { .. }
                    | Node::Record(_)
            )
        };

        let is_merge_term = |ast: &Ast<'ast>| {
            matches!(
                &ast.node,
                Node::PrimOpApp {
                    op: PrimOp::Merge(_) | PrimOp::MergeContract,
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

            if is_fieldy_term(p) {
                return Some(p);
            }
        }
        None
    }

    pub fn rev_path(&self) -> Option<&[EltId]> {
        self.rev_path.as_deref()
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

/// The initial code analysis that we collect for a file.
///
/// This analysis is re-collected from scratch each time the file is updated.
#[derive(Default, Debug)]
pub struct Analysis<'ast> {
    pub position_lookup: PositionLookup<'ast>,
    pub usage_lookup: UsageLookup<'ast>,
    pub parent_lookup: ParentLookup<'ast>,
    pub type_lookup: CollectedTypes<'ast, Type<'ast>>,
    /// A lookup table for static accesses, for looking up all occurrences of, say, `.foo` in a
    /// file.
    pub static_accesses: HashMap<Ident, Vec<&'ast Ast<'ast>>>,
    /// Store the last AST that has been refined from a parse error, if any.
    ///
    /// This is used during completion, and is a temporary value. We need to store this value here
    /// because of borrowing shenanigans. If we would return it directly from
    /// [PackedAnalysis::reparse_range], we would keep a mutable borrow on the whole analysis,
    /// which isn't practical. Instead, we store it here and the caller can retrieve it immutably
    /// later.
    last_reparsed_ast: Option<&'ast Ast<'ast>>,
}

//type Test<'a> = nickel_lang_core::environment::Environment<String, &'a ()>;
fn is_covariant<'a, 'b: 'a>(a: Analysis<'b>) -> Analysis<'a> {
    a
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
            last_reparsed_ast: None,
        }
    }
}

#[derive(Copy, Clone)]
pub struct AnalysisRegistryRef<'a, 'std_ast> {
    stdlib_analysis: &'a PackedAnalysis<'static>,
    analyses: &'a HashMap<FileId, PackedAnalysis<'std_ast>>,
    pub init_term_env: &'a Environment<'std_ast>,
    pub init_type_ctxt: &'a TypeContext<'std_ast>,
}

impl<'a, 'std_ast> AnalysisRegistryRef<'a, 'std_ast> {
    pub fn get(&self, file_id: FileId) -> Option<&PackedAnalysis<'std_ast>> {
        if file_id == self.stdlib_analysis.file_id() {
            Some(self.stdlib_analysis)
        } else {
            self.analyses.get(&file_id)
        }
    }
}

/// The collection of analyses for every file that we know about.
#[self_referencing]
pub struct AnalysisRegistry {
    /// The analysis corresponding to the `std` module of the stdlib. It's separate because we want
    /// to guarantee that it's live for the whole duration of [Self]. Having it there alone isn't
    /// sufficient to enforce this invariant, but it makes it harder to remove it from
    /// [Self::analyses] by accident. Also, this field isn't public.
    ///
    /// # Safety
    ///
    /// This analysis must not be dropped or taken out before [Self] is dropped, or Undefined
    /// Behavior will ensue.
    ///
    /// **Important**: keep this field last in the struct. Rust guarantees that fields are dropped
    /// in declaration order, meaning that the other analyses that borrow from the stdlib analysis'
    /// initial environment will be dropped first. Otherwise, we might create dangling references,
    /// even for a short lapse of time.
    stdlib_analysis: PackedAnalysis<'static>,
    /// The initial environment, analyzed from the stdlib. Its content is allocated into
    /// [Self::stdlib_analysis], which must thus be guaranteed to live as long as `self`. Thus,
    /// once we've initialized the stdlib, we shouldn't touch its analysis anymore.
    ///
    /// # Safety
    ///
    /// This environment isn't actually `'static`: it's an internal placeholder because the
    /// lifetime is self-referential (morally, it's `'self`). Do not use this field directly; use
    /// [Self] helpers instead.
    #[borrows(stdlib_analysis)]
    #[covariant]
    initial_term_env: Environment<'this>,
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
    #[borrows(stdlib_analysis)]
    #[covariant]
    initial_type_ctxt: TypeContext<'this>,
    /// Map of the analyses of each live file.
    ///
    /// Most of the fields of `Analysis` are themselves hash tables. Having a table of tables
    /// requires more lookups than necessary, but it makes it easy to invalidate a whole file.
    #[borrows(stdlib_analysis, initial_term_env, initial_type_ctxt)]
    #[covariant]
    pub analyses: HashMap<FileId, PackedAnalysis<'this>>,
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
#[self_referencing]
pub(crate) struct PackedAnalysis<'std> {
    /// The allocator hosting AST nodes.
    alloc: AstAlloc,
    init_term_env: Environment<'std>,
    init_type_ctxt: TypeContext<'std>,
    /// The id of the analyzed file.
    file_id: FileId,
    /// The corresponding parsed AST. It is initialized with a static reference to a `null` value,
    /// and properly set after the file is parsed.
    #[borrows(alloc)]
    #[covariant]
    ast: &'this Ast<'this>,
    /// The non-fatal parse errors that occurred while parsing [Self::ast], if any.
    parse_errors: ParseErrors,
    /// The type context, from type-checking the AST. This is only actually used
    /// when this `PackedAnalysis` is the analysis of the stdlib, but because of
    /// the various borrowing inter-dependencies, it's easier to just store it in
    /// here always.
    #[borrows(alloc, ast)]
    #[covariant]
    type_ctxt: TypeContext<'this>,
    /// The analysis of the current file.
    #[borrows(alloc, init_term_env, init_type_ctxt)]
    #[covariant]
    analysis: Analysis<'this>,
}

impl<'std> PackedAnalysis<'std> {
    /// FIXME: docs
    pub(crate) fn parsed(
        file_id: FileId,
        sources: &SourceCache,
        init_term_env: Environment<'std>,
        init_type_env: TypeContext<'std>,
    ) -> Self {
        let alloc = AstAlloc::new();

        let mut parse_errors = ParseErrors::default();
        let mut ret = PackedAnalysis::new(
            alloc,
            init_term_env,
            init_type_env,
            file_id,
            |alloc| {
                let source = sources.source(file_id);
                let (ast, errors) = parser::grammar::TermParser::new().parse_fully_tolerant(
                    alloc,
                    file_id,
                    Lexer::new(source),
                    sources.files.source_span(file_id),
                );
                parse_errors = errors;
                alloc.alloc(ast)
            },
            ParseErrors::default(),
            |_, _| TypeContext::new(),
            |_, _, _| Analysis::default(),
        );
        ret.with_parse_errors_mut(|errors| {
            *errors = parse_errors;
        });
        ret
    }

    pub(crate) fn analysis<'ast>(&'ast self) -> &'ast Analysis<'ast> {
        self.borrow_analysis()
    }

    pub(crate) fn ast(&self) -> &Ast<'_> {
        self.borrow_ast()
    }

    pub(crate) fn alloc(&self) -> &AstAlloc {
        self.borrow_alloc()
    }

    pub(crate) fn file_id(&self) -> FileId {
        *self.borrow_file_id()
    }

    pub(crate) fn parse_errors(&self) -> &ParseErrors {
        self.borrow_parse_errors()
    }

    pub(crate) fn last_reparsed_ast(&self) -> Option<&Ast<'_>> {
        self.borrow_analysis().last_reparsed_ast
    }

    /// Reset the last re-parsed AST to `None`, prior to a new reparsing tentative.
    pub(crate) fn clear_last_reparsed_ast(&mut self) {
        self.with_analysis_mut(|slf| {
            slf.last_reparsed_ast = None;
        })
    }

    /// Tries to parse a selected substring of a parse error in the current file. If the parsing
    /// succeeds, which is defined by the fact that there's no fatal error and the result isn't
    /// [nickel_lang_core::bytecode::ast::Node::ParseError] again, then:
    ///
    /// - the new AST is typechecked (in particular if it contains imports, they are resolved)
    /// - usage analysis is updated with the new information
    /// - the new AST is stored in a special field of the analysis.
    ///
    /// The result can be retrieved later through [Self::last_reparsed_ast].
    ///
    /// Returns `true` upon success (parsing), that is if the top-level node of the re-parsed AST
    /// is NOT [nickel_lang_core::bytecode::ast::Node::ParseError]. In this case
    /// `self.last_reparsed_ast()`, will return `Some(_)`. Otherwise, `false` is returned and
    /// `self.last_reparsed_ast()` is left unchanged.
    ///
    /// For example, a subterm `foo.bar.` will be a parse error at first, but if the user triggers
    /// completion, the completion handler will try to parse `foo.bar` instead, which is indeed a
    /// proper expression. In that case, we'll add this new subexpression to the usage analysis.
    ///
    /// Despite the name, this function doesn't load any new data from disk or from the client. It
    /// shouldn't be used when the file's content is changing, but only to refine existing partial
    /// information (typically incomplete input).
    ///
    /// # Panics
    ///
    /// This method panics if the given range for the source is out of bounds.
    pub(crate) fn reparse_range(
        &mut self,
        sources: &SourceCache,
        range_err: RawSpan,
        subrange: RawSpan,
    ) -> bool {
        let file_id = self.file_id();
        self.with_mut(|slf| {
            let to_parse = &sources.source(file_id)[subrange.to_range()];
            let alloc = &slf.alloc;

            let (ast, _errors) = parser::grammar::TermParser::new().parse_fully_tolerant(
                alloc,
                file_id,
                OffsetLexer::new(to_parse, subrange.start.0 as usize),
                subrange,
            );

            if let Node::ParseError(_) = ast.node {
                return false;
            }

            let ast = alloc.alloc(ast);
            slf.analysis
                .usage_lookup
                .amend_parse_error(alloc, ast, range_err);

            slf.analysis.last_reparsed_ast = Some(ast);
            true
        })
    }

    /// Typecheck and analyze the given file, storing the result in this packed analysis.
    ///
    /// # Arguments
    ///
    /// `reg` might be `None` during the initialization of the stdlib, because we need to fill the
    /// analysis of `std` first before initializing the registry.
    ///
    /// # Returns
    ///
    /// Return a list of fresh analyses corresponding to the transitive dependencies of this file
    /// that weren't already in the registry. The parsed AST of those analyses is populated, but
    /// not the code analysis itself (it's not typechecked yet).
    pub(crate) fn fill_analysis<'world, 'std_ast>(
        &mut self,
        sources: &'world mut SourceCache,
        import_data: &'world mut ImportData,
        import_targets: &'world mut ImportTargets,
        reg: AnalysisRegistryRef<'world, 'std_ast>,
    ) -> Result<Vec<PackedAnalysis<'std_ast>>, Vec<TypecheckError>> {
        self.with_mut(move |slf| {
            let mut collector = TypeCollector::default();
            let alloc = slf.alloc;

            let mut resolver = WorldImportResolver {
                reg,
                new_imports: Vec::new(),
                sources,
                import_data,
                import_targets,
            };

            let type_tables = typecheck_visit(
                alloc,
                slf.ast,
                (*slf.init_type_ctxt).clone(),
                &mut resolver,
                &mut collector,
                TypecheckMode::Walk,
            )
            .map_err(|err| vec![err])?;

            let new_imports = std::mem::take(&mut resolver.new_imports);
            let type_lookups = collector.complete(alloc, type_tables);

            *slf.analysis = Analysis::new(alloc, slf.ast, type_lookups, slf.init_term_env);

            Ok(new_imports)
        })
    }

    /// Fill the analysis of the `std` module. Similar to [Self::fill_analysis], but doesn't
    /// require an analysis registry (which isn't initialized yet) and create the initial typing
    /// environment from `self.ast`. Returns the initial typing environment.
    ///
    /// This method panics on error.
    pub(crate) fn fill_stdlib_analysis<'ast>(&'ast mut self) {
        use nickel_lang_core::{stdlib::StdlibModule, typecheck::mk_initial_ctxt};

        self.with_mut(|slf| {
            let mut collector = TypeCollector::default();
            let alloc = slf.alloc;

            let initial_type_ctxt =
                mk_initial_ctxt(alloc, std::iter::once((StdlibModule::Std, *slf.ast)))
                    .expect("fail to create the initial typing environment");

            let type_tables = typecheck_visit(
                alloc,
                slf.ast,
                initial_type_ctxt.clone(),
                &mut StdlibResolver,
                &mut collector,
                TypecheckMode::Walk,
            )
            .expect("failed to typecheck `std`");

            let type_lookups = collector.complete(alloc, type_tables);

            *slf.type_ctxt = initial_type_ctxt;
            *slf.analysis =
                Analysis::new(alloc, slf.ast, type_lookups, &Environment::<'static>::new());
        })
    }

    pub(crate) fn stdlib_type_context(&self) -> &TypeContext {
        self.borrow_type_ctxt()
    }
}

impl AnalysisRegistry {
    pub fn with_std(stdlib_analysis: PackedAnalysis<'static>) -> Self {
        Self::new(
            stdlib_analysis,
            |stdlib_analysis| {
                let name = StdlibModule::Std.name().into();

                let def = Def::Let {
                    ident: crate::identifier::LocIdent {
                        ident: name,
                        pos: TermPos::None,
                    },
                    metadata: stdlib_analysis.alloc().alloc(Default::default()),
                    value: stdlib_analysis.ast(),
                    path: Vec::new(),
                };
                let mut initial_env = Environment::default();
                initial_env.insert(name, def);
                initial_env
            },
            |analysis| analysis.stdlib_type_context().clone(),
            |_, _, _| HashMap::new(),
        )
    }

    pub fn initial_type_ctxt<'ast>(&'ast self) -> TypeContext<'ast> {
        self.borrow_initial_type_ctxt().clone()
    }

    pub fn initial_term_env<'ast>(&'ast self) -> Environment<'ast> {
        self.borrow_initial_term_env().clone()
    }

    /// Inserts a new analysis. If an analysis was already there for the given file id, return the
    /// overridden analysis, or `None` otherwise.
    // TODO: we can make a modify also?
    pub fn insert_with<'a, F>(&'a mut self, callback: F)
    where
        F: for<'std_ast> FnOnce(
            &'std_ast PackedAnalysis<'static>,
            &Environment<'std_ast>,
            &TypeContext<'std_ast>,
        ) -> PackedAnalysis<'std_ast>,
    {
        self.with_mut(|slf| {
            let analysis = callback(
                slf.stdlib_analysis,
                slf.initial_term_env,
                slf.initial_type_ctxt,
            );
            if analysis.file_id() == slf.stdlib_analysis.file_id() {
                // Panicking there is a bit exaggerated, but it's a bug to re-analyse the stdlib
                // several times. At least we'll catch it.
                panic!(
                    "tried to insert the stdlib analysis into the registry, but was already there"
                );
            }

            slf.analyses.insert(analysis.file_id(), analysis);
        })
    }

    pub fn get(&self, file_id: FileId) -> Option<&PackedAnalysis> {
        if file_id == self.borrow_stdlib_analysis().file_id() {
            Some(self.borrow_stdlib_analysis())
        } else {
            self.borrow_analyses().get(&file_id)
        }
    }

    pub fn modify<T, F>(&mut self, file_id: FileId, callback: F) -> Option<T>
    where
        F: for<'std_ast> FnOnce(
            AnalysisRegistryRef<'_, 'std_ast>,
            &mut PackedAnalysis<'std_ast>,
        ) -> T,
    {
        self.modify_and_insert(file_id, |reg, analysis| {
            Ok::<_, ()>((Vec::new(), callback(reg, analysis)))
        })
        // unwrap: modify_and_insert only fails when the inner callback does,
        // and this inner callback never fails
        .unwrap()
    }

    pub fn modify_and_insert<F, T, E>(
        &mut self,
        file_id: FileId,
        callback: F,
    ) -> Result<Option<T>, E>
    where
        F: for<'std_ast> FnOnce(
            AnalysisRegistryRef<'_, 'std_ast>,
            &mut PackedAnalysis<'std_ast>,
        ) -> Result<(Vec<PackedAnalysis<'std_ast>>, T), E>,
    {
        if file_id == self.borrow_stdlib_analysis().file_id() {
            panic!("can't modify the stdlib analysis!");
        } else {
            self.with_mut(|slf| {
                if let Some(mut analysis) = slf.analyses.remove(&file_id) {
                    let reg_ref = AnalysisRegistryRef {
                        stdlib_analysis: slf.stdlib_analysis,
                        analyses: slf.analyses,
                        init_term_env: slf.initial_term_env,
                        init_type_ctxt: slf.initial_type_ctxt,
                    };
                    let result = callback(reg_ref, &mut analysis);
                    slf.analyses.insert(analysis.file_id(), analysis);

                    let (new_analyses, ret) = result?;
                    for a in new_analyses {
                        slf.analyses.insert(a.file_id(), a);
                    }
                    Ok(Some(ret))
                } else {
                    Ok(None)
                }
            })
        }
    }

    pub fn remove(&mut self, file_id: FileId) -> Option<PackedAnalysis> {
        self.with_analyses_mut(|analyses| analyses.remove(&file_id))
    }

    pub fn get_def(&self, ident: &LocIdent) -> Option<&Def> {
        let file = ident.pos.as_opt_ref()?.src_id;
        self.get(file)?
            .analysis()
            .usage_lookup
            .def(ident)
            // This special case probably only has an effect when referring to
            // the name "std" from within the standard library. It's here because
            // the standard library analysis is constructed from an empty initial
            // env, meaning that it doesn't know about the definition of "std".
            //
            // Every other file other than the standard library is analyzed
            // starting from the environment `self.initial_term_env`, so if the
            // previous lookup failed then this should fail also.
            .or_else(|| self.borrow_initial_term_env().get(&ident.ident))
    }

    pub fn get_usages(&self, span: &RawSpan) -> impl Iterator<Item = &LocIdent> {
        fn inner<'a>(
            slf: &'a AnalysisRegistry,
            span: &RawSpan,
        ) -> Option<impl Iterator<Item = &'a LocIdent>> {
            let file = span.src_id;
            Some(
                slf.borrow_analyses()
                    .get(&file)?
                    .analysis()
                    .usage_lookup
                    .usages(span),
            )
        }

        inner(self, span).into_iter().flatten()
    }

    pub fn get_env<'ast>(&'ast self, ast: &'ast Ast<'ast>) -> Option<&'ast Environment<'ast>> {
        let file = ast.pos.as_opt_ref()?.src_id;
        self.borrow_analyses()
            .get(&file)?
            .analysis()
            .usage_lookup
            .env(ast)
    }

    pub fn get_type<'ast>(&'ast self, ast: &'ast Ast<'ast>) -> Option<&'ast Type<'ast>> {
        let file = ast.pos.as_opt_ref()?.src_id;
        self.borrow_analyses()
            .get(&file)?
            .analysis()
            .type_lookup
            .terms
            .get(&AstPtr(ast))
    }

    pub fn get_ident_type<'ast>(&'ast self, id: &LocIdent) -> Option<&'ast Type<'ast>> {
        let file = id.pos.as_opt_ref()?.src_id;
        self.borrow_analyses()
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
            self.borrow_analyses()
                .get(&file)?
                .analysis()
                .parent_lookup
                .parent_chain(ast),
        )
    }

    pub fn get_static_accesses(&self, id: Ident) -> Vec<&Ast<'_>> {
        self.borrow_analyses()
            .values()
            .filter_map(|a| a.analysis().static_accesses.get(&id))
            .flatten()
            .cloned()
            .collect()
    }

    /// Same as [Self::get], but produce a proper LSP error if the file is not in the registry.
    pub fn get_or_err(&self, file: FileId) -> Result<&PackedAnalysis, ResponseError> {
        self.get(file).ok_or_else(|| ResponseError {
            data: None,
            message: "File has not yet been parsed or cached.".to_owned(),
            code: ErrorCode::ParseError as i32,
        })
    }

    /// Takes a position, finds the corresponding file in the registry and retrieve the smaller AST
    /// whose span contains that position, if any.
    pub fn ast_at(&self, pos: RawPos) -> Result<Option<&Ast<'_>>, ResponseError> {
        Ok(self
            .get_or_err(pos.src_id)?
            .analysis()
            .position_lookup
            .at(pos.index))
    }

    /// Takes a position, finds the corresponding file in the registry and retrieve the identifier
    /// at that position, if any.
    pub fn ident_at(
        &self,
        pos: RawPos,
    ) -> Result<Option<crate::identifier::LocIdent>, ResponseError> {
        Ok(self.ident_data_at(pos)?.map(|id| id.ident))
    }

    /// Takes a position, finds the corresponding file in the registry and retrieve the identifier
    /// at that position and the field definition it is part of, if any.
    pub fn ident_data_at(
        &self,
        pos: RawPos,
    ) -> Result<Option<crate::position::IdentData>, ResponseError> {
        Ok(self
            .get_or_err(pos.src_id)?
            .analysis()
            .position_lookup
            .ident_data_at(pos.index))
    }

    pub(crate) fn stdlib_analysis(&self) -> &PackedAnalysis {
        self.borrow_stdlib_analysis()
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

impl<Ty> Default for CollectedTypes<'_, Ty> {
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
        parser::{grammar, lexer, FullyErrorTolerantParser as _},
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

        let (file, ast) = parse(&alloc, "{ foo = [{ bar = 1 }] }");
        let bar_id = Ident::new("bar");
        let bar = locced(bar_id, file, 11..14);

        let parent = ParentLookup::new(&ast);
        let usages = UsageLookup::new(&alloc, &ast, &Environment::new());
        let values = usages.def(&bar).unwrap().values();

        assert_eq!(values.len(), 1);
        let bar_val = values.into_iter().next().unwrap();

        let p = parent.parent(bar_val).unwrap();
        assert_eq!(p.child_path, vec![EltId::Ident(bar_id)]);
        assert_matches!(&p.ast.node, Node::Record(_));

        let gp = parent.parent(p.ast).unwrap();
        assert_eq!(gp.child_path, vec![EltId::ArrayElt]);
        assert_matches!(&gp.ast.node, Node::Array { .. });

        let ggp = parent.parent(gp.ast).unwrap();
        assert_matches!(ggp.child_path.as_slice(), &[EltId::Ident(_)]);
        assert_matches!(&ggp.ast.node, Node::Record(_));
    }

    #[test]
    fn parse_error_parent() {
        use nickel_lang_core::position::RawSpan;

        let alloc = AstAlloc::new();

        // The field that fails to parse should have a record as its parent.
        let s = "{ field. }";
        let file = Files::new().add("<test>", s.to_owned());

        let (ast, _errors) = grammar::TermParser::new().parse_fully_tolerant(
            &alloc,
            file,
            lexer::Lexer::new(s),
            RawSpan::from_range(file, 0..s.len()),
        );

        let parent = ParentLookup::new(&ast);
        let positions = PositionLookup::new(&ast);
        let err = positions.at(ByteIndex(5)).unwrap();

        let p = parent.parent(err).unwrap();
        assert!(p.child_path.is_empty());
        assert_matches!(&p.ast.node, Node::Record(_));
    }
}
