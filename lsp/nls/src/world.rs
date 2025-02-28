use std::{
    collections::{HashMap, HashSet},
    ffi::OsString,
    path::{Path, PathBuf},
};

use codespan::ByteIndex;
use log::warn;
use lsp_server::ResponseError;
use lsp_types::{TextDocumentPositionParams, Url};

use nickel_lang_core::{
    bytecode::ast::{pattern::bindings::Bindings as _, primop::PrimOp, Ast, Import, Node},
    cache::{AstImportResolver, CacheHub, ImportData, InputFormat, SourceCache, SourcePath},
    environment::Environment,
    error::{ImportError, IntoDiagnostics},
    eval::{cache::CacheImpl, VirtualMachine},
    files::FileId,
    position::{RawPos, RawSpan, TermPos},
    stdlib::StdlibModule,
    traverse::TraverseAlloc,
};

use crate::{
    analysis::{AnalysisRegistry, PackedAnalysis},
    diagnostic::SerializableDiagnostic,
    error::WarningReporter,
    field_walker::{Def, FieldResolver},
    files::uri_to_path,
    identifier::LocIdent,
};

pub type ImportTargets = HashMap<FileId, HashMap<RawSpan, FileId>>;

/// All the state associated with the files we know about.
///
/// Includes cached analyses, cached parse trees, etc.
pub struct World {
    /// The original string content of each source we have read. As we don't use
    /// [nickel_lang_core::cache::Caches], we need to manage sources ourselves.
    pub sources: SourceCache,
    /// In order to return diagnostics, we store the URL of each file we know about.
    pub file_uris: HashMap<FileId, Url>,
    /// A map of all live file anlyses, minus the one of the stdlib, which is stored separately
    /// (see [Self::stdlib_analysis]).
    pub analysis_reg: AnalysisRegistry,
    /// A map of import dependencies and reverse import dependencies between Nickel source files.
    pub(crate) import_data: ImportData,
    /// A map of import location to file ids, corresponding to the id of the target. This is needed
    /// to go to definition when hovering an import.
    ///
    /// This cache is segregated by file id to avoid easy quick invalidation.
    import_targets: ImportTargets,
    /// A map associating imported files with failed imports. This allows us to
    /// invalidate the cached version of a file when one of its imports becomes available.
    ///
    /// The keys in this map are the filenames (just the basename; no directory) of the
    /// files that failed to import, and the values in this map are the file ids that tried
    /// to import it.
    pub failed_imports: HashMap<OsString, HashSet<FileId>>,
    /// This is a cached version of the stdlib in the old AST representation. It is used to cheaply
    /// derive [nickel_lang_core::cache::CacheHub] instances for background evaluation while
    /// avoiding work duplication as much as possible. In particular, the instances will use this
    /// parsed and converted representation of the stdlib and will share the same source cache.
    compiled_stdlib: nickel_lang_core::term::RichTerm,
}

/// Initialize the standard library and thus the analysis registry.
fn initialize_stdlib(sources: &mut SourceCache) -> AnalysisRegistry {
    let mut initial_env = Environment::default();
    // We need to pass import data around for resolution, but we expect the stdlid to not import
    // anything in the current setup.
    let mut import_data = ImportData::new();
    let mut import_targets = ImportTargets::new();
    // The analysis of the `std` module.
    let mut stdlib_analysis = None;
    // The analysis of other internal modules.
    let mut other_analyses = Vec::new();

    for (module, file_id) in sources.stdlib_modules() {
        let mut analysis = PackedAnalysis::new(file_id);
        let errors = analysis.parse(sources);
        // We don't recover from failing to load the stdlib
        assert!(
            errors.is_ok_and(|()| analysis.parse_errors().errors.is_empty()),
            "failed to parse the stdlib"
        );

        let imported = analysis
            .fill_analysis(sources, &mut import_data, &mut import_targets, None)
            .expect("failed to typecheck the stdlib");
        debug_assert!(
            imported.is_empty() && import_data.is_empty() && import_targets.is_empty(),
            "the stdlib should not import anything else"
        );

        // Add the std module to the environment (but not `internals`, because those symbols
        // don't get their own namespace, and we don't want to use them for completion anyway).
        if module == StdlibModule::Std {
            let name = module.name().into();

            let def = Def::Let {
                ident: crate::identifier::LocIdent {
                    ident: name,
                    pos: TermPos::None,
                },
                metadata: analysis.alloc().alloc(Default::default()),
                value: analysis.ast(),
                path: Vec::new(),
            };

            // Safety: the safety is guaranteed by the invariant, maintained through the code base,
            // that the packed analysis for the stdlib module is never evicted from the registry
            // during the lifetime of `World`.
            let def = unsafe { std::mem::transmute::<Def<'_>, Def<'static>>(def) };
            initial_env.insert(name, def);
            stdlib_analysis = Some(analysis);
        } else {
            other_analyses.push(analysis);
        }
    }

    let mut reg = AnalysisRegistry::new(
        stdlib_analysis.expect("internal error: unexpectedly missing `std` module in the stdlib"),
        initial_env,
    );

    for analysis in other_analyses {
        reg.analyses.insert(analysis.file_id(), analysis);
    }

    reg
}

impl World {
    pub fn new() -> Self {
        use nickel_lang_core::bytecode::ast::compat::ToMainline;

        let mut sources = SourceCache::new();

        if let Ok(nickel_path) = std::env::var("NICKEL_IMPORT_PATH") {
            sources.add_import_paths(nickel_path.split(':'));
        }

        let analysis_reg = initialize_stdlib(&mut sources);
        let compiled_stdlib = analysis_reg.stdlib_analysis().ast().to_mainline();

        Self {
            sources,
            analysis_reg,
            import_data: ImportData::new(),
            import_targets: HashMap::new(),
            file_uris: HashMap::new(),
            failed_imports: HashMap::new(),
            compiled_stdlib,
        }
    }

    /// Adds a new file to our world.
    ///
    /// Returns a list of files that were invalidated by this addition. Even though this is a new
    /// file, it can still invalidate existing files if they were already importing this file
    /// directly from the filesystem instead of through the editor.
    pub fn add_file(
        &mut self,
        uri: Url,
        contents: String,
    ) -> anyhow::Result<(FileId, HashSet<FileId>)> {
        let path = uri_to_path(&uri)?;

        // Invalidate the cache of every file that tried, but failed, to import a file
        // with a name like this.
        let failed_to_import = path
            .file_name()
            .and_then(|name| self.failed_imports.remove(name))
            .unwrap_or_default();

        // Replace the path (as opposed to adding it): we may already have this file in the
        // cache if it was imported by an already-open file.
        let file_id = self
            .sources
            .replace_string(SourcePath::Path(path, InputFormat::Nickel), contents);

        // The cache automatically invalidates reverse-dependencies; we also need to track them, so
        // that we can clear our own analysis.
        let mut invalid = failed_to_import.clone();
        invalid.extend(self.invalidate(file_id));

        for f in failed_to_import {
            invalid.extend(self.invalidate(f));
        }

        for rev_dep in &invalid {
            self.analysis_reg.remove(*rev_dep);
        }

        self.file_uris.insert(file_id, uri);

        Ok((file_id, invalid))
    }

    /// Updates a file's contents.
    ///
    /// Returns a list of files that were invalidated by this change.
    pub fn update_file(
        &mut self,
        uri: Url,
        contents: String,
    ) -> anyhow::Result<(FileId, Vec<FileId>)> {
        let path = uri_to_path(&uri)?;
        let file_id = self
            .sources
            .replace_string(SourcePath::Path(path, InputFormat::Nickel), contents);

        let invalid = self.invalidate(file_id);

        for f in &invalid {
            self.analysis_reg.remove(*f);
        }

        Ok((file_id, invalid))
    }

    pub fn lsp_diagnostics(
        &mut self,
        file_id: FileId,
        err: impl IntoDiagnostics,
    ) -> Vec<SerializableDiagnostic> {
        SerializableDiagnostic::from(err, &mut self.sources.files().clone(), file_id)
    }

    // Make a record of I/O errors in imports so that we can retry them when appropriate.
    fn associate_failed_import(&mut self, err: &nickel_lang_core::error::TypecheckError) {
        if let nickel_lang_core::error::TypecheckError::ImportError(ImportError::IOError(
            name,
            _,
            pos,
        )) = &err
        {
            if let Some((filename, pos)) = PathBuf::from(name).file_name().zip(pos.into_opt()) {
                self.failed_imports
                    .entry(filename.to_owned())
                    .or_default()
                    .insert(pos.src_id);
            }
        }
    }

    /// Returns `Ok` for recoverable (or no) errors, or `Err` for fatal errors.
    pub fn parse(
        &mut self,
        file_id: FileId,
    ) -> Result<Vec<SerializableDiagnostic>, Vec<SerializableDiagnostic>> {
        //TODO[RFC007]: should we re-parse if the analysis is already there? It's the safest, but
        //might be wasteful.
        let analysis = self
            .analysis_reg
            .analyses
            .entry(file_id)
            .or_insert_with(|| PackedAnalysis::new(file_id));

        let result = analysis.parse(&self.sources);
        let errs = analysis.parse_errors().clone();

        result
            .map(|()| self.lsp_diagnostics(file_id, errs))
            .map_err(|fatal| self.lsp_diagnostics(file_id, fatal))
    }

    /// Typechecks a file, returning diagnostics on error.
    ///
    /// This won't parse the file by default (the analysed term will then be a `null` value without
    /// position). Use [Self::parse_and_typecheck] if you want to do both.
    pub fn typecheck(&mut self, file_id: FileId) -> Result<(), Vec<SerializableDiagnostic>> {
        // It's a bit annoying, but we need to take the analysis out of the hashmap to avoid
        // borrowing issues, as typechecking and import resolution will need to both access the registry
        // and the packed analysis.
        let mut analysis = self.analysis_reg.remove(file_id).unwrap();

        let new_imports = analysis
            .fill_analysis(
                &mut self.sources,
                &mut self.import_data,
                &mut self.import_targets,
                Some(&self.analysis_reg),
            )
            .map_err(|errors| {
                errors
                    .into_iter()
                    .flat_map(|err| {
                        self.associate_failed_import(&err);
                        self.lsp_diagnostics(file_id, err)
                    })
                    .collect::<Vec<_>>()
            })?;

        let new_ids = new_imports
            .iter()
            .map(|analysis| analysis.file_id())
            .collect::<Vec<_>>();

        // We need to first populate the registry, and then take each analysis out one by one to
        // typecheck it. The reason is the following: say A imports B and C, and B imports C. After
        // typechecking A, we get [B, C] as new imports. If we first typecheck B, it won't find C
        // in the registry and will thus consider it as a new import: C will be parsed again and an
        // empty analysis will be allocated. If we're doing this really naively, we might even
        // typecheck and analyse C two times as well.
        //
        // Instead, we put everything in the registry first so that it's up to date, and only then
        // typecheck the files one by one.
        for analysis in new_imports {
            self.analysis_reg.insert(analysis);
        }

        let mut typecheck_import_diagnostics: Vec<FileId> = Vec::new();

        for id in new_ids {
            if self.typecheck(id).is_err() {
                // Add the correct position to typecheck import errors and then transform them to
                // normal import errors.
                typecheck_import_diagnostics.push(id);
            }
        }

        if !typecheck_import_diagnostics.is_empty() {
            let typecheck_import_diagnostics = typecheck_import_diagnostics
                .into_iter()
                .flat_map(|id| {
                    let message = "This import could not be resolved \
                    because its content has failed to typecheck correctly.";

                    // Find a position (one is enough) where the import came from.
                    let pos = analysis
                        .ast()
                        .find_map(|ast: &Ast<'_>| match &ast.node {
                            Node::Import { .. }
                                if self
                                    .import_targets
                                    .get(&ast.pos.src_id()?)?
                                    .get(&ast.pos.into_opt()?)?
                                    == &id =>
                            {
                                Some(ast.pos)
                            }
                            _ => None,
                        })
                        .unwrap_or_default();

                    let name: String = self.sources.name(id).to_str().unwrap().into();
                    self.lsp_diagnostics(
                        file_id,
                        ImportError::IOError(name, String::from(message), pos),
                    )
                })
                .collect();

            Err(typecheck_import_diagnostics)
        } else {
            Ok(())
        }
    }

    pub fn parse_and_typecheck(&mut self, file_id: FileId) -> Vec<SerializableDiagnostic> {
        match self.parse(file_id) {
            Ok(mut nonfatal) => {
                if let Err(e) = self.typecheck(file_id) {
                    nonfatal.extend(e);
                }
                nonfatal
            }
            Err(fatal) => fatal,
        }
    }

    pub fn eval_diagnostics(
        &mut self,
        file_id: FileId,
        recursion_limit: usize,
    ) -> Vec<SerializableDiagnostic> {
        //TODO[RFC007]: do that as part of the CacheHub instead.
        let mut diags = self.parse_and_typecheck(file_id);
        if diags.is_empty() {
            let (reporter, warnings) = WarningReporter::new();
            let mut vm = VirtualMachine::<_, CacheImpl>::new(
                self.cache_hub_for_eval(file_id),
                std::io::stderr(),
                reporter,
            );
            // unwrap: we don't expect an error here, since we already typechecked above.
            let rt = vm.prepare_eval(file_id).unwrap();

            let errors = vm.eval_permissive(rt, recursion_limit);
            // Get a possibly-updated files from the vm instead of relying on the one
            // in `world`.
            let mut files = vm.import_resolver().sources.files().clone();

            diags.extend(
                errors
                    .into_iter()
                    .filter(|e| {
                        !matches!(
                            e,
                            nickel_lang_core::error::EvalError::MissingFieldDef { .. }
                        )
                    })
                    .flat_map(|e| SerializableDiagnostic::from(e, &mut files, file_id)),
            );
            diags.extend(warnings.try_iter().flat_map(|(warning, mut files)| {
                SerializableDiagnostic::from(warning, &mut files, file_id)
            }));
        }

        diags.sort();
        diags.dedup();
        diags
    }

    pub fn file_analysis(&self, file: FileId) -> Result<&PackedAnalysis, ResponseError> {
        self.analysis_reg.get_or_err(file)
    }

    pub fn lookup_ast_by_position<'ast>(
        &'ast self,
        pos: RawPos,
    ) -> Result<Option<&'ast Ast<'ast>>, ResponseError> {
        self.analysis_reg.lookup_ast_by_position(pos)
    }

    pub fn lookup_ident_by_position(
        &self,
        pos: RawPos,
    ) -> Result<Option<crate::identifier::LocIdent>, ResponseError> {
        self.analysis_reg.lookup_ident_by_position(pos)
    }

    /// Finds all the locations at which a term (or possibly an ident within a term) is "defined".
    ///
    /// "Ident within a term" applies when the term is a record or a pattern binding, so that we
    /// can refer to fields in a record, or specific idents in a pattern binding.
    ///
    /// The return value contains all the spans of all the definition locations. It's a span instead
    /// of a `LocIdent` because when `term` is an import, the definition location is the whole
    /// included file. In every other case, the definition location will be the span of a LocIdent.
    pub fn get_defs<'ast>(
        &'ast self,
        ast: &'ast Ast<'ast>,
        ident: Option<LocIdent>,
    ) -> Vec<RawSpan> {
        // The inner function returning Option is just for ?-early-return convenience.
        fn inner<'ast>(
            world: &'ast World,
            ast: &'ast Ast<'ast>,
            ident: Option<LocIdent>,
        ) -> Option<Vec<RawSpan>> {
            let resolver = FieldResolver::new(world);

            let ret = match (&ast.node, ident) {
                (Node::Var(id), _) => {
                    let id = LocIdent::from(*id);
                    let def = world.analysis_reg.get_def(&id)?;
                    let cousins = resolver.cousin_defs(def);
                    if cousins.is_empty() {
                        vec![def.loc_ident().pos.unwrap()]
                    } else {
                        cousins
                            .into_iter()
                            .filter_map(|piece| piece.ident().map(|id| id.pos.into_opt()).flatten())
                            .collect()
                    }
                }
                (
                    Node::PrimOpApp {
                        op: PrimOp::RecordStatAccess(id),
                        args: [parent],
                    },
                    _,
                ) => {
                    let parents = resolver.resolve_record(parent);
                    parents
                        .iter()
                        .flat_map(|parent| parent.field_locs(id.ident()))
                        .filter_map(|id| id.pos.into_opt())
                        .collect()
                }
                (Node::Let { bindings, .. }, Some(hovered_id)) => {
                    let mut spans = Vec::new();

                    for binding in bindings.iter() {
                        if let Some(pat_binding) = binding
                            .pattern
                            .bindings()
                            .into_iter()
                            .find(|pat_binding| pat_binding.id.ident() == hovered_id.ident)
                        {
                            //TODO[RFC007]: why don't we use `last` anymore?
                            let (last, path) = pat_binding.path.split_last()?;
                            let path: Vec<_> = path.iter().map(|id| id.ident()).collect();
                            let parents =
                                resolver.resolve_path(&binding.value, path.iter().copied());
                            spans = parents
                                .iter()
                                .flat_map(|parent| parent.field_locs(pat_binding.id.ident()))
                                .filter_map(|id| id.pos.into_opt())
                                .collect();
                            break;
                        }
                    }
                    spans
                }
                (Node::Import(_), _) => {
                    let target = world
                        .import_targets
                        .get(&ast.pos.src_id()?)?
                        .get(&ast.pos.into_opt()?)?;
                    let pos = world.analysis_reg.get(*target)?.ast().pos.into_opt()?;
                    vec![pos]
                }
                (Node::Record(..), Some(id)) => {
                    let def = Def::Field {
                        ident: id.ident,
                        // value: None,
                        // TODO[RFC007]: what to use as pieces? Value was `None` before.
                        pieces: Vec::new(),
                        record: ast,
                    };
                    let cousins = resolver.cousin_defs(&def);
                    cousins
                        .into_iter()
                        .filter_map(|piece| piece.ident().and_then(|id| id.pos.into_opt()))
                        .collect()
                }
                _ => {
                    return None;
                }
            };
            Some(ret)
        }

        inner(self, ast, ident).unwrap_or_default()
    }

    /// If `span` is pointing at the identifier binding a record field, returns
    /// all the places that the record field is referenced.
    ///
    /// This is a sort of inverse of `get_defs`, at least when the argument to `get_defs`
    /// is a static access: the spans returned by this function are exactly the static accesses
    /// that, when passed to `get_defs`, return `span`.
    ///
    /// This function can be expensive, because it calls `get_defs` on every static access
    /// that could potentially be referencing this field.
    pub fn get_field_refs(&self, span: RawSpan) -> Vec<RawSpan> {
        // The inner function returning Option is just for ?-early-return convenience.
        fn inner(world: &World, span: RawSpan) -> Option<Vec<RawSpan>> {
            let ident = world.lookup_ident_by_position(span.start_pos()).ok()??;
            let ast = world.lookup_ast_by_position(span.start_pos()).ok()??;

            if let Node::Record(_) = &ast.node {
                let accesses = world.analysis_reg.get_static_accesses(ident.ident);

                Some(
                    accesses
                        .into_iter()
                        .filter_map(|access| {
                            let Node::PrimOpApp {
                                op: PrimOp::RecordStatAccess(id),
                                args: _,
                            } = &access.node
                            else {
                                return None;
                            };

                            if world.get_defs(&access, None).contains(&span) {
                                id.pos.into_opt()
                            } else {
                                None
                            }
                        })
                        .collect(),
                )
            } else {
                None
            }
        }
        inner(self, span).unwrap_or_default()
    }

    pub fn uris(&self, ids: impl IntoIterator<Item = FileId>) -> impl Iterator<Item = &Url> {
        ids.into_iter().filter_map(|id| {
            self.file_uris.get(&id).or_else(|| {
                warn!("no uri for {id:?}");
                None
            })
        })
    }

    pub fn invalidate(&mut self, file_id: FileId) -> Vec<FileId> {
        fn invalidate_rec(world: &mut World, acc: &mut Vec<FileId>, file_id: FileId) {
            world.import_data.imports.remove(&file_id);

            let rev_deps = world
                .import_data
                .rev_imports
                .remove(&file_id)
                .unwrap_or_default();

            acc.extend(rev_deps.iter().copied());

            for file_id in &rev_deps {
                invalidate_rec(world, acc, *file_id);
            }
        }

        let mut acc = Vec::new();
        invalidate_rec(self, &mut acc, file_id);
        acc
    }

    pub fn position(
        &self,
        lsp_pos: &TextDocumentPositionParams,
    ) -> Result<RawPos, crate::error::Error> {
        let uri = &lsp_pos.text_document.uri;
        let file_id = self
            .file_id(uri)?
            .ok_or_else(|| crate::error::Error::FileNotFound(uri.clone()))?;
        let pos = lsp_pos.position;
        let idx = crate::codespan_lsp::position_to_byte_index(self.sources.files(), file_id, &pos)
            .map_err(|_| crate::error::Error::InvalidPosition {
                pos,
                file: uri.clone(),
            })?;

        Ok(RawPos::new(file_id, ByteIndex(idx as u32)))
    }

    pub fn file_id(&self, uri: &Url) -> Result<Option<FileId>, crate::error::Error> {
        let path = uri
            .to_file_path()
            .map_err(|_| crate::error::Error::FileNotFound(uri.clone()))?;
        Ok(self
            .sources
            .id_of(&SourcePath::Path(path, InputFormat::Nickel)))
    }

    /// Returns a [nickel_lang_core::cache::CacheHub] instance derived from this world and targeted
    /// toward the evaluation of a given file. NLS handles files on its own, but we want to avoid
    /// duplicating work when doing background evaluation: loading the stdlib, parsing, etc.
    ///
    /// This method spins up a new cache, and pre-fill the old AST representation for the stdlib,
    /// the given file and the transitive closure of its imports.
    pub fn cache_hub_for_eval(&self, file_id: FileId) -> CacheHub {
        use nickel_lang_core::{
            bytecode::ast::compat::ToMainline,
            cache::{EntryState, TermEntry},
        };

        let mut cache = CacheHub::new();
        cache.sources = self.sources.clone();
        cache.import_data = self.import_data.clone();

        cache.terms.insert(
            self.analysis_reg.stdlib_analysis().file_id(),
            TermEntry {
                term: self.compiled_stdlib.clone(),
                state: EntryState::Typechecked,
                format: InputFormat::Nickel,
            },
        );

        cache.terms.insert(
            file_id,
            TermEntry {
                term: self.analysis_reg.get(file_id).unwrap().ast().to_mainline(),
                state: EntryState::Parsed,
                format: InputFormat::Nickel,
            },
        );

        let mut work_stack: Vec<FileId> = self
            .import_data
            .imports
            .get(&file_id)
            .map(|imports| imports.iter().copied().collect())
            .unwrap_or_default();

        // Imports can be cyclic, so we need to keep track of what we've already done.
        let mut visited = HashSet::new();
        visited.insert(file_id);

        while let Some(next) = work_stack.pop() {
            if !visited.insert(next) {
                continue;
            }

            // It's fine to not pre-fill the cache: we could start from an empty cache and things
            // will still work out. We just try to fill as many things as we can to avoid
            // duplicating work.
            let Some(analysis) = self.analysis_reg.get(next) else {
                continue;
            };

            cache.terms.insert(
                next,
                TermEntry {
                    term: analysis.ast().to_mainline(),
                    state: EntryState::Typechecked,
                    format: InputFormat::Nickel,
                },
            );

            work_stack.extend(
                self.import_data
                    .imports
                    .get(&next)
                    .cloned()
                    .unwrap_or_default(),
            );
        }

        cache
    }

    pub fn get_import_target(&self, pos: TermPos) -> Option<FileId> {
        let pos = pos.into_opt()?;
        self.import_targets.get(&pos.src_id)?.get(&pos).copied()
    }
}

/// The import resolver used by [World]. It borrows from the analysis registry, from the source
/// cache and from the import data that are updated as new file are parsed.
///
/// TODO: why do we use `new_imports`
pub(crate) struct WorldImportResolver<'a> {
    pub(crate) reg: Option<&'a AnalysisRegistry>,
    pub(crate) new_imports: Vec<PackedAnalysis>,
    pub(crate) sources: &'a mut SourceCache,
    pub(crate) import_data: &'a mut ImportData,
    pub(crate) import_targets: &'a mut ImportTargets,
}

impl AstImportResolver for WorldImportResolver<'_> {
    fn resolve<'ast_in, 'ast_out>(
        &'ast_out mut self,
        import: &Import<'ast_in>,
        pos: &TermPos,
    ) -> Result<Option<&'ast_out Ast<'ast_out>>, ImportError> {
        use nickel_lang_core::bytecode::ast::Import;
        use std::ffi::OsStr;

        let parent_id = pos.src_id();

        let (possible_parents, path, pkg_id, format) = match import {
            Import::Path { path, format } => {
                // `parent` is the file that did the import. We first look in its containing
                // directory, followed by the directories in the import path.
                let parent_path = parent_id
                    .clone()
                    .and_then(|parent| self.sources.file_paths.get(&parent))
                    .and_then(|path| <&OsStr>::try_from(path).ok())
                    .map(PathBuf::from)
                    .map(|mut path| {
                        path.pop();
                        path
                    })
                    // If the parent isn't a proper file, we look in the current directory instead.
                    // This is useful when importing e.g. from the REPL or the CLI directly.
                    .unwrap_or_default();

                (
                    std::iter::once(parent_path)
                        .chain(self.sources.import_paths.iter().cloned())
                        .collect(),
                    Path::new(path),
                    None,
                    *format,
                )
            }
            Import::Package { id } => {
                let package_map = self
                    .sources
                    .package_map
                    .as_ref()
                    .ok_or(ImportError::NoPackageMap { pos: *pos })?;
                let parent_path = parent_id
                    .clone()
                    .and_then(|p| self.sources.packages.get(&p))
                    .map(PathBuf::as_path);
                let pkg_path = package_map.get(parent_path, *id, *pos)?;
                (
                    vec![pkg_path.to_owned()],
                    Path::new("main.ncl"),
                    Some(pkg_path.to_owned()),
                    // Packages are always in nickel format
                    InputFormat::Nickel,
                )
            }
        };

        // Try to import from all possibilities, taking the first one that succeeds.
        let (id_op, path_buf) = possible_parents
            .iter()
            .find_map(|parent| {
                let mut path_buf = parent.clone();
                path_buf.push(path);
                self.sources
                    .get_or_add_file(&path_buf, format)
                    .ok()
                    .map(|x| (x, path_buf))
            })
            .ok_or_else(|| {
                let parents = possible_parents
                    .iter()
                    .map(|p| p.to_string_lossy())
                    .collect::<Vec<_>>();
                ImportError::IOError(
                    path.to_string_lossy().into_owned(),
                    format!("could not find import (looked in [{}])", parents.join(", ")),
                    *pos,
                )
            })?;

        let file_id = id_op.inner();

        if let Some(parent_id) = parent_id {
            eprintln!("Parent id : {parent_id:?}. Inserting corresponding import data");

            // unwrap(): if `parent_id = pos.src_id()` is defined, then `pos` must be defined.
            self.import_targets
                .entry(parent_id)
                .or_default()
                .insert(pos.unwrap(), file_id);

            self.import_data
                .imports
                .entry(parent_id)
                .or_default()
                .insert(file_id);
            self.import_data
                .rev_imports
                .entry(file_id)
                .or_default()
                .insert(parent_id);
        }

        if let Some(pkg_id) = pkg_id {
            self.sources.packages.insert(file_id, pkg_id);
        }

        if let InputFormat::Nickel = format {
            if let Some(analysis) = self.reg.and_then(|reg| reg.get(file_id)) {
                eprintln!("Import hitting cache - associating to file id {file_id:?}");
                Ok(Some(analysis.ast()))
            } else {
                eprintln!(
                    "Import resolution: first time parsing {} - associating to file id {file_id:?}",
                    path_buf.display()
                );

                let mut analysis = PackedAnalysis::new(file_id);
                analysis
                    .parse(self.sources)
                    .map_err(|parse_err| ImportError::ParseErrors(parse_err.into(), *pos))?;

                // Since `new_imports` owns the packed anlysis, we need to push the analysis here
                // first and then re-borrow it from `new_imports`.
                self.new_imports.push(analysis);
                Ok(Some(self.new_imports.last().unwrap().ast()))
            }
        } else {
            // For a non-nickel file, we don't do anything currently, as they aren't converted to
            // the new AST. At some point we might want to do this, to allow to jump into a
            // definition or to provide completions for JSON files, for example.
            Ok(None)
        }
    }
}
