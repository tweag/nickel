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
    error::{ImportError, IntoDiagnostics, TypecheckError},
    eval::{cache::CacheImpl, VirtualMachine},
    files::FileId,
    position::{RawPos, RawSpan, TermPos},
    traverse::TraverseAlloc,
};

use crate::{
    analysis::{AnalysisRegistry, AnalysisRegistryRef, PackedAnalysis},
    diagnostic::SerializableDiagnostic,
    error::WarningReporter,
    field_walker::FieldResolver,
    files::uri_to_path,
    identifier::LocIdent,
    position::IdentData,
};

pub type ImportTargets = HashMap<FileId, HashMap<RawSpan, FileId>>;

/// All the state associated with the files we know about.
///
/// Includes cached analyses, cached parse trees, etc.
pub struct World {
    /// The original string content of each source we have read. As we don't use
    /// [nickel_lang_core::cache::CacheHub], we need to manage sources ourselves.
    pub sources: SourceCache,
    /// In order to return diagnostics, we store the URL of each file we know about.
    pub file_uris: HashMap<FileId, Url>,
    /// A map of all live file anlyses.
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

impl World {
    pub fn new() -> Self {
        use nickel_lang_core::bytecode::ast::compat::ToMainline;

        let mut sources = SourceCache::new();

        if let Ok(nickel_path) = std::env::var("NICKEL_IMPORT_PATH") {
            sources.add_import_paths(nickel_path.split(':'));
        }

        let analysis_reg = AnalysisRegistry::with_std(&mut sources);
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

        // We invalidate our reverse dependencies.
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
        &self,
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

    /// Returns all of the diagnostics encountered during parsing.
    pub fn parse(&mut self, file_id: FileId) -> Vec<SerializableDiagnostic> {
        let mut errs = nickel_lang_core::error::ParseErrors::default();
        self.analysis_reg
            .insert_with(|init_term_env, init_type_ctxt| {
                let analysis = PackedAnalysis::parsed(
                    file_id,
                    &self.sources,
                    init_term_env.clone(),
                    init_type_ctxt.clone(),
                );
                errs = analysis.parse_errors().clone();
                analysis
            });

        self.lsp_diagnostics(file_id, errs)
    }

    /// Typechecks a file, returning diagnostics on error.
    ///
    /// This won't parse the file by default (the analysed term will then be a `null` value without
    /// position). Use [Self::parse_and_typecheck] if you want to do both.
    ///
    /// # Panics
    ///
    /// Panics if `file_id` is unknown
    pub fn typecheck(&mut self, file_id: FileId) -> Result<(), Vec<SerializableDiagnostic>> {
        let mut typecheck_diagnostics: Vec<_> = self
            .analysis_reg
            .modify_and_insert(file_id, |reg, analysis| -> Result<_, Vec<TypecheckError>> {
                let imports = analysis.fill_analysis(
                    &mut self.sources,
                    &mut self.import_data,
                    &mut self.import_targets,
                    reg,
                )?;

                Ok((imports, ()))
            })
            .map_err(|errors| {
                errors
                    .into_iter()
                    .flat_map(|err| {
                        self.associate_failed_import(&err);
                        self.lsp_diagnostics(file_id, err)
                    })
                    .collect()
            })
            .err()
            .unwrap_or_default();

        enum FailedPhase {
            Parsing,
            Typechecking,
        }

        let imported_file_ids: Vec<_> = self.import_data.imports(file_id).collect();
        let failed_imports: Vec<(FileId, FailedPhase)> = imported_file_ids
            .into_iter()
            .filter_map(|id| {
                // Any imported files should have been added to the analysis
                // registry when fill_analysis was run if they weren't
                // there already, so the parsing errors can be retrieved
                // from cache. I'm not sure the backup of running parse is
                // even necessary.
                let has_parsing_errors = self
                    .analysis_reg
                    .get(id)
                    .map(|analysis| !analysis.parse_errors().errors.is_empty())
                    .unwrap_or_else(|| !self.parse(id).is_empty());

                if has_parsing_errors {
                    Some((id, FailedPhase::Parsing))
                } else if self.typecheck(id).is_err() {
                    Some((id, FailedPhase::Typechecking))
                } else {
                    None
                }
            })
            .collect();

        // unwrap(): we're allowed to panic if `file_id` is missing. We didn't remove it, so if it's missing now
        // then it was missing when this method was called.
        let analysis = self.analysis_reg.get(file_id).unwrap();

        let mut import_diagnostics = failed_imports
            .into_iter()
            .flat_map(|(id, phase)| {
                let message = match phase {
                    FailedPhase::Parsing => {
                        "This import could not be resolved \
                                because its content could not be parsed."
                    }
                    FailedPhase::Typechecking => {
                        "This import could not be resolved \
                                because its content has failed to typecheck correctly."
                    }
                };

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

        typecheck_diagnostics.append(&mut import_diagnostics);

        if typecheck_diagnostics.is_empty() {
            Ok(())
        } else {
            Err(typecheck_diagnostics)
        }
    }

    pub fn parse_and_typecheck(&mut self, file_id: FileId) -> Vec<SerializableDiagnostic> {
        let mut diags = self.parse(file_id);
        diags.extend(self.typecheck(file_id).err().unwrap_or_default());
        diags
    }

    /// Calls [PackedAnalysis::reparse_range] on the corresponding analysis, if any. If the
    /// reparsed AST import new files, they are parsed, typechecked and analysed as well, to
    /// provide the original completion request any needed information. Diagnostics are ignored: if
    /// an imported file fails to typecheck, the completion request will just ignore it.
    pub fn reparse_range(&mut self, range_err: RawSpan, reparse_range: RawSpan) -> bool {
        use nickel_lang_core::typecheck::{typecheck_visit, TypecheckMode};

        let new_ids = self
            .analysis_reg
            .modify_and_insert(range_err.src_id, |reg, analysis| {
                if !analysis.reparse_range(&self.sources, range_err, reparse_range) {
                    return Err(());
                }

                let Some(ast) = analysis.last_reparsed_ast() else {
                    unreachable!("reparse_range returned true, but last_reparsed_ast is None");
                };

                let alloc = &analysis.alloc();

                let mut resolver = WorldImportResolver {
                    reg,
                    new_imports: Vec::new(),
                    sources: &mut self.sources,
                    import_data: &mut self.import_data,
                    import_targets: &mut self.import_targets,
                };

                let _ = typecheck_visit(
                    alloc,
                    ast,
                    reg.init_type_ctxt.clone(),
                    &mut resolver,
                    &mut (),
                    TypecheckMode::Walk,
                );

                let new_imports = std::mem::take(&mut resolver.new_imports);
                let new_ids: Vec<_> = new_imports
                    .iter()
                    .map(|analysis| analysis.file_id())
                    .collect();

                Ok((new_imports, new_ids))
            });

        if let Ok(Some(new_ids)) = new_ids {
            for id in new_ids {
                let _ = self.typecheck(id);
            }
            true
        } else {
            false
        }
    }

    pub fn eval_diagnostics(
        &mut self,
        file_id: FileId,
        recursion_limit: usize,
    ) -> Vec<SerializableDiagnostic> {
        let mut diags = self.parse_and_typecheck(file_id);

        if diags.is_empty() {
            let (reporter, warnings) = WarningReporter::new();
            let mut vm = VirtualMachine::<_, CacheImpl>::new(
                self.cache_hub_for_eval(file_id),
                std::io::stderr(),
                reporter,
            );
            // unwrap: we don't expect an error here, since we already typechecked above.
            let rt = vm.prepare_eval_only(file_id).unwrap();

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

    pub fn ast_at(&self, pos: RawPos) -> Result<Option<&Ast<'_>>, ResponseError> {
        self.analysis_reg.ast_at(pos)
    }

    pub fn ident_at(
        &self,
        pos: RawPos,
    ) -> Result<Option<crate::identifier::LocIdent>, ResponseError> {
        self.analysis_reg.ident_at(pos)
    }

    pub fn ident_data_at(
        &self,
        pos: RawPos,
    ) -> Result<Option<crate::position::IdentData>, ResponseError> {
        self.analysis_reg.ident_data_at(pos)
    }

    /// Finds all the locations at which a term (or possibly an ident within a term) is "defined".
    ///
    /// "Ident within a term" applies when the term is a record or a pattern binding, so that we
    /// can refer to fields in a record, including in the middle of a field path in a piecewise
    /// field definition, or specific idents in a pattern binding.
    ///
    /// The return value contains all the spans of all the definition locations. It's a span instead
    /// of a `LocIdent` because when `term` is an import, the definition location is the whole
    /// included file. In every other case, the definition location will be the span of a LocIdent.
    pub fn get_defs<'ast>(
        &'ast self,
        ast: &'ast Ast<'ast>,
        ident_data: Option<&IdentData>,
    ) -> Vec<RawSpan> {
        // The inner function returning Option is just for ?-early-return convenience.
        fn inner<'ast>(
            world: &'ast World,
            ast: &'ast Ast<'ast>,
            ident_data: Option<&IdentData>,
        ) -> Option<Vec<RawSpan>> {
            let resolver = FieldResolver::new(world);

            let ret = match (&ast.node, ident_data) {
                (Node::Var(id), _) => {
                    let id = LocIdent::from(*id);
                    let def = world.analysis_reg.get_def(&id)?;
                    let cousins = resolver.cousin_defs(def);

                    if cousins.is_empty() {
                        vec![def.loc_ident().pos.unwrap()]
                    } else {
                        cousins
                            .into_iter()
                            // [^cousin-def-backtracking]: In `{ a = x } & { a = y }`, looking for
                            // the definition of the first occurrence of `a` we look at the cousin
                            // `{a = y}` and retrieve the definition of `a` there, which will be a
                            // field definition piece of `= y` (with `def_piece.index == 1`). We
                            // backtrack to include the original `a` in the span, recovering the
                            // full `a = y`.
                            //
                            // unwrap(): if we found this definition by indexing in a container,
                            // then `backtrack` should succeed and the previous element in the path
                            // must be an ident.
                            .filter_map(|piece| {
                                piece.backtrack().unwrap().ident().unwrap().pos.into_opt()
                            })
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
                        if let Some(pat_binding) =
                            binding.pattern.bindings().into_iter().find(|pat_binding| {
                                pat_binding.id.ident() == hovered_id.ident.ident
                            })
                        {
                            let (last, path) = pat_binding.path.split_last()?;
                            let path: Vec<_> = path.iter().map(|id| id.ident()).collect();
                            let parents =
                                resolver.resolve_path(&binding.value, path.iter().copied());
                            spans = parents
                                .iter()
                                .flat_map(|parent| parent.field_locs(last.ident()))
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
                (Node::Record(_), Some(ident_data)) => {
                    let def_pieces = if let Some(def_piece) = ident_data.field_def {
                        resolver.cousin_defs_at_path(
                            ast,
                            def_piece.path_in_parent()?,
                            ident_data.ident.ident,
                        )
                    } else {
                        resolver.cousin_defs_at(ast, ident_data.ident.ident)
                    };

                    def_pieces
                        .into_iter()
                        // See [^cousin-def-backtracking]
                        .filter_map(|piece| {
                            piece
                                .backtrack()
                                .unwrap()
                                .ident()
                                .and_then(|id| id.pos.into_opt())
                        })
                        .collect()
                }
                _ => {
                    return None;
                }
            };
            Some(ret)
        }

        inner(self, ast, ident_data).unwrap_or_default()
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
            let ident = world.ident_at(span.start_pos()).ok()??;
            let ast = world.ast_at(span.start_pos()).ok()??;

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

                            if world.get_defs(access, None).contains(&span) {
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
    /// at the evaluation of a given file. NLS handles files on its own, but we want to avoid
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
            // would still work out. We just try to fill as many things as we can to avoid
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
pub(crate) struct WorldImportResolver<'a, 'std> {
    pub(crate) reg: AnalysisRegistryRef<'a, 'std>,
    pub(crate) new_imports: Vec<PackedAnalysis<'std>>,
    pub(crate) sources: &'a mut SourceCache,
    pub(crate) import_data: &'a mut ImportData,
    pub(crate) import_targets: &'a mut ImportTargets,
}

impl AstImportResolver for WorldImportResolver<'_, '_> {
    fn resolve<'ast_out>(
        &'ast_out mut self,
        import: &Import<'_>,
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
        let file_id = possible_parents
            .iter()
            .find_map(|parent| {
                let mut path_buf = parent.clone();
                path_buf.push(path);
                self.sources.get_or_add_file(&path_buf, format).ok()
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
            })?
            .inner();

        if let Some(parent_id) = parent_id {
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
            if let Some(analysis) = self.reg.get(file_id) {
                Ok(Some(analysis.ast()))
            } else {
                let analysis = PackedAnalysis::parsed(
                    file_id,
                    self.sources,
                    self.reg.init_term_env.clone(),
                    self.reg.init_type_ctxt.clone(),
                );
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

/// We need to provide an import resolver when typechecking the stdlib, but the stdlib doesn't
/// import from any other file. [StdlibResolver] is a dummy resolver that panics whenever it is
/// called.
pub(crate) struct StdlibResolver;

impl AstImportResolver for StdlibResolver {
    fn resolve<'ast_out>(
        &'ast_out mut self,
        _import: &nickel_lang_core::bytecode::ast::Import<'_>,
        _pos: &TermPos,
    ) -> Result<Option<&'ast_out Ast<'ast_out>>, ImportError> {
        panic!("unexpected import from the `std` module")
    }
}

#[cfg(test)]
mod tests {
    use lsp_types::Position;

    use super::*;

    /// Makes a path as a Url for a test file that works on any OS.
    /// Filenames are created in the same base directory so they can
    /// import each other by relative path.
    /// Panics if the filename is not valid in a Url.
    fn make_file_url(filename: &str) -> Url {
        let base_path: PathBuf = env!("CARGO_MANIFEST_DIR").into();
        Url::from_file_path(base_path.join(filename)).unwrap()
    }

    #[test]
    fn test_successful_typecheck() {
        let mut world = World::new();

        let (parent, _) = world
            .add_file(make_file_url("file.ncl"), "1 : Number".to_string())
            .unwrap();
        // The parse phase needs to run before typechecking
        world.parse(parent);
        assert!(world.typecheck(parent).is_ok());
    }

    #[test]
    fn test_failed_typecheck() {
        let mut world = World::new();

        let (parent, _) = world
            .add_file(make_file_url("file.ncl"), "1 : String".to_string())
            .unwrap();
        world.parse(parent);

        assert!(world.typecheck(parent).is_err());
    }

    #[test]
    fn typechecking_fails_on_import_typecheck_error() {
        let mut world = World::new();
        let (child, _) = world
            .add_file(make_file_url("child.ncl"), "2 : String".to_string())
            .unwrap();
        world.parse(child);

        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "import \"child.ncl\"".to_string(),
            )
            .unwrap();
        world.parse(parent);

        assert!(world.typecheck(parent).is_err());
    }

    #[test]
    fn typechecking_fails_on_import_parse_error() {
        let mut world = World::new();

        let (child, _) = world
            .add_file(
                make_file_url("child.ncl"),
                "[1,2".to_string(), // this is intentionally unparseable
            )
            .unwrap();
        world.parse(child);

        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "import \"child.ncl\"".to_string(),
            )
            .unwrap();
        world.parse(parent);

        assert!(world.typecheck(parent).is_err());
    }

    #[test]
    fn typechecking_succeeds_on_valid_import() {
        let mut world = World::new();
        let (child, _) = world
            .add_file(make_file_url("child.ncl"), "2 : Number".to_string())
            .unwrap();
        world.parse(child);

        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "(import \"child.ncl\") : Number".to_string(),
            )
            .unwrap();
        world.parse(parent);

        assert!(world.typecheck(parent).is_ok());
    }

    #[test]
    fn multiple_import_failures_issue_multiple_diagnostics() {
        let mut world = World::new();
        let (child1, _) = world
            .add_file(make_file_url("child1.ncl"), "2 : String".to_string())
            .unwrap();
        world.parse(child1);

        let (child2, _) = world
            .add_file(make_file_url("child2.ncl"), "[1,2".to_string())
            .unwrap();
        world.parse(child2);

        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "let child1 = import \"child1.ncl\" in\
                    let child2 = import \"child2.ncl\" in \
                    { include [child1, child2] }
                    "
                .to_string(),
            )
            .unwrap();
        world.parse(parent);

        assert_eq!(world.typecheck(parent).err().unwrap().len(), 2);
    }

    #[test]
    fn import_diagnostics_are_issued_when_overall_typechecking_fails() {
        /* the typechecking phase can emit diagnostics from either the
        file being typechecked or files being imported. This test makes
        sure that if there's a typechecking error in the primary file, that
        diagnostics for imported files can still be emitted. This is
        important in the case where a parsing error makes it impossible to
        infer the type of the imported file. The inferred type of an unparseable
        file is Dyn, which can cause problems typechecking the rest of the file.

        So given these two files:

        # child.ncl
        [1, 2

        # parent.ncl
        let x = import "child.ncl" in x : Array Number

        x inferred to be Dyn which causes "x: Array Number" to fail typechecking.
        So if that failure shortcuts the typechecking and causes import diagnostics
        not to be issued, it would miss the actual cause of the failure, which
        is that "child.ncl" is unparseable.
        */
        let mut world = World::new();

        let (child, _) = world
            .add_file(make_file_url("child.ncl"), "[1,2".to_string())
            .unwrap();
        world.parse(child);

        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "let x = import \"child.ncl\"\nin x : Array Number".to_string(),
            )
            .unwrap();
        world.parse(parent);

        let diagnostics = world.typecheck(parent).unwrap_err();

        // the result should have a diagnostic whose range begins
        // at the import expression.
        let has_diagnostic_at_import = diagnostics.iter().any(|d| {
            d.range.0.start
                == Position {
                    line: 0,
                    character: 8,
                }
        });

        assert!(has_diagnostic_at_import);
    }
}
