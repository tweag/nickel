use std::{
    collections::{HashMap, HashSet},
    convert::Infallible,
    ffi::OsString,
    path::{Path, PathBuf},
};

use codespan::ByteIndex;
use log::warn;
use lsp_server::ResponseError;
use lsp_types::{TextDocumentPositionParams, Url};

use nickel_lang_core::{
    bytecode::ast::{Ast, Import, Node, pattern::bindings::Bindings as _, primop::PrimOp},
    cache::{
        AstImportResolver, CacheHub, ImportData, ImportTarget, InputFormat, SourceCache, SourcePath,
    },
    error::{ImportError, IntoDiagnostics, ParseErrors},
    eval::{VirtualMachine, VmContext, cache::CacheImpl, value::NickelValue},
    files::FileId,
    position::{PosTable, RawPos, RawSpan, TermPos},
    traverse::TraverseAlloc,
    typ::TypeF,
};

use crate::{
    analysis::{
        AltFormatErrors, AnalysisRegistry, AnalysisRegistryRef, AnalysisState, AnalysisTarget,
        PackedAnalysis,
    },
    config::LspConfig,
    diagnostic::{DiagnosticCompat as _, SerializableDiagnostic},
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
    compiled_stdlib: NickelValue,
    contract_configs: crate::contracts::ContractConfigsWatcher,
    pub config: LspConfig,
    /// Since we hold a compiled version of the stdlib, we also need the corresponding position
    /// table. The pos table is also used for analysis of non-Nickel format (e.g. JSON or YAML)
    /// which are currently only representable in a "compiled" form.
    pos_table: PosTable,
}

impl World {
    pub fn new(config: LspConfig) -> Self {
        Self::new_with_contract_configs(config, crate::contracts::ContractConfigsWatcher::new())
    }

    /// Creates a new `World` that doesn't start a file-watcher for contract configurations.
    pub fn new_without_contract_configs(config: LspConfig) -> Self {
        Self::new_with_contract_configs(config, crate::contracts::ContractConfigsWatcher::dummy())
    }

    fn new_with_contract_configs(
        config: LspConfig,
        cfgs: crate::contracts::ContractConfigsWatcher,
    ) -> Self {
        use nickel_lang_core::bytecode::ast::compat::ToMainline;

        let mut sources = SourceCache::new();

        if let Ok(nickel_path) = std::env::var("NICKEL_IMPORT_PATH") {
            sources.add_import_paths(nickel_path.split(':'));
        }

        let analysis_reg = AnalysisRegistry::with_std(&mut sources);
        let mut pos_table = PosTable::new();
        let compiled_stdlib = analysis_reg
            .stdlib_analysis()
            .ast()
            .to_mainline(&mut pos_table);

        Self {
            sources,
            analysis_reg,
            import_data: ImportData::new(),
            import_targets: HashMap::new(),
            file_uris: HashMap::new(),
            failed_imports: HashMap::new(),
            compiled_stdlib,
            contract_configs: cfgs,
            config,
            pos_table,
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
        self.contract_configs.watch_configs_for(&uri);

        // Invalidate the cache of every file that tried, but failed, to import a file
        // with a name like this.
        let failed_to_import = path
            .file_name()
            .and_then(|name| self.failed_imports.remove(name))
            .unwrap_or_default();

        // Replace the path (as opposed to adding it): we may already have this file in the
        // cache if it was imported by an already-open file.
        let format = InputFormat::from_path(&path).unwrap_or_default();
        let file_id = self
            .sources
            .replace_string(SourcePath::Path(path, format), contents);

        // We invalidate our reverse dependencies.
        let mut invalid = failed_to_import.clone();
        invalid.extend(self.invalidate(file_id));

        for f in failed_to_import {
            invalid.extend(self.invalidate(f));
        }

        for rev_dep in &invalid {
            self.analysis_reg.remove(*rev_dep);
        }

        self.file_uris.insert(file_id, uri.clone());

        Ok((file_id, invalid))
    }

    fn file_format(&self, file_id: FileId) -> Option<InputFormat> {
        if let Some(SourcePath::Path(_, format)) = self.sources.file_paths.get(&file_id) {
            Some(*format)
        } else {
            None
        }
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
        let format = InputFormat::from_path(&path).unwrap_or_default();
        let file_id = self
            .sources
            .replace_string(SourcePath::Path(path, format), contents);

        let invalid = self.invalidate(file_id);

        for f in &invalid {
            self.analysis_reg.remove(*f);
        }

        Ok((file_id, invalid))
    }

    /// Closes a file that has been opened in-memory so that it can be reloaded from the
    /// filesystem.
    /// Returns the file ID of the file from the filesystem replacing the closed in-memory file,
    /// or None if no file with the same path exists. Also returns the list of invalidated files.
    pub fn close_file(&mut self, uri: Url) -> anyhow::Result<(Option<FileId>, Vec<FileId>)> {
        let path = uri_to_path(&uri)?;

        let format = InputFormat::from_path(&path).unwrap_or_default();
        let result = self.sources.close_in_memory_file(path, format)?;
        self.analysis_reg.remove(result.closed_id);

        let invalid = self.invalidate(result.closed_id);
        for f in &invalid {
            self.analysis_reg.remove(*f);
        }

        if let Ok(id) = result.replacement_id {
            self.file_uris.insert(id, uri);
        }

        Ok((result.replacement_id.ok(), invalid))
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
        if let nickel_lang_core::error::TypecheckErrorData::ImportError(ImportError::IOError(
            name,
            _,
            pos,
        )) = err.borrow_error()
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
    ///
    /// # Panics
    ///
    /// Panics if the `file_id` is invalid.
    pub fn parse(&mut self, file_id: FileId) -> Vec<SerializableDiagnostic> {
        let errs = match self.file_format(file_id).unwrap() {
            InputFormat::Nickel => {
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
                errs
            }
            InputFormat::Yaml => {
                let mut errs = nickel_lang_core::error::ParseErrors::default();
                self.analysis_reg
                    .insert_with(|_init_term_env, _init_type_ctxt| {
                        let analysis = PackedAnalysis::parsed_yaml(file_id, &self.sources);
                        errs = analysis.parse_errors().clone();
                        analysis
                    });
                errs
            }
            format => {
                let errors = self
                    .sources
                    .parse_other(&mut self.pos_table, file_id, format)
                    .err()
                    .map(|e| ParseErrors::new(vec![e]))
                    .unwrap_or_else(|| ParseErrors::new(vec![]));
                eprintln!("parsed {file_id:?}, errors {errors:?}");
                errors
            }
        };

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
        // unwrap: if `file_id` is known, `file_format` will be `Some`. If `file_id` is unknown,
        // we're allowed to panic
        let format = self.file_format(file_id).unwrap();
        if !matches!(format, InputFormat::Nickel) {
            // We don't need to avoid import loops for non-Nickel files, because we don't
            // call `typecheck` on an imported non-Nickel file.
            return self.check_non_nickel(file_id, format);
        }

        let analysis = self.analysis_reg.get(file_id);
        match analysis.map(PackedAnalysis::state) {
            // If an analysis is already being typechecked or has been typechecked, return
            // the cached diagnostics. In the case that the file is still being typechecked,
            // these diagnostics may be incomplete. However, it's still necessary to stop
            // typechecking short and return the incomplete value, since otherwise there
            // could be an infinite loop in the case of mutually imported files.
            Some(AnalysisState::Typechecking) | Some(AnalysisState::Typechecked) => {
                let diagnostics = analysis
                    .unwrap() // unwrap(): A state was found so the entry exists
                    .typecheck_diagnostics();
                if diagnostics.is_empty() {
                    Ok(())
                } else {
                    Err(diagnostics.to_vec())
                }
            }
            _ => self.typecheck_uncached(file_id),
        }
    }

    /// Checks the validity of a non-Nickel file, if there's a Nickel contract configured
    /// for it.
    fn check_non_nickel(
        &mut self,
        file_id: FileId,
        format: InputFormat,
    ) -> Result<(), Vec<SerializableDiagnostic>> {
        use codespan_reporting::diagnostic::{Diagnostic, Label};
        use nickel_lang_core::{
            cache::{TermEntry, TermEntryState},
            term::{LabeledType, Term, TypeAnnotation},
        };

        let Some(uri) = self.file_uris.get(&file_id) else {
            return Ok(());
        };
        let Some(cfg) = self.contract_configs.config_for(uri) else {
            return Ok(());
        };

        let Some(contract_id) = self
            .sources
            .get_or_add_file(&cfg.contract_path, InputFormat::Nickel)
            .ok()
            .map(|id| id.inner())
        else {
            return Ok(());
        };

        // If the contract hasn't already been loaded, load it.
        if self.analysis_reg.get(contract_id).is_none() {
            let contract_diagnostics = self.parse_and_typecheck(contract_id);
            if !contract_diagnostics.is_empty() {
                // We don't issue diagnostics on the contract (it might not
                // even be loaded by the editor, and if it is then we should
                // be issuing them elsewhere anyway). We issue a diagnostic
                // at the beginning of the data file, just because otherwise
                // it might be surprising that the contract is not doing
                // anything.

                return Err(SerializableDiagnostic::from_codespan(
                    file_id,
                    Diagnostic {
                        severity: codespan_reporting::diagnostic::Severity::Warning,
                        code: None,
                        message: format!(
                            "the configured contract at {} failed to load",
                            cfg.contract_path.display()
                        ),
                        labels: vec![Label::primary(file_id, 0..1)],
                        notes: Vec::new(),
                    },
                    &self.sources.files,
                ));
            }
        }

        // The data file doesn't really *import* the contract, but we'll pretend it does
        // because it fits with our dependency tracking: if the contract changes then we
        // want to invalidate and re-check the data file.
        self.import_data
            .imports
            .entry(file_id)
            .or_default()
            .insert(ImportTarget {
                file_id: contract_id,
                format: InputFormat::Nickel,
            });

        self.import_data
            .rev_imports
            .entry(contract_id)
            .or_default()
            .insert(file_id, TermPos::None);

        let (reporter, warnings) = WarningReporter::new();
        let (cache_hub, pos_table) = self.cache_hub_for_eval(contract_id);
        let mut vm_ctxt =
            VmContext::new_with_pos_table(cache_hub, pos_table, std::io::stderr(), reporter);

        let value = match self
            .sources
            .parse_other(&mut vm_ctxt.pos_table, file_id, format)
        {
            Ok(t) => t,
            Err(e) => {
                return Err(self.lsp_diagnostics(file_id, ParseErrors::new(vec![e])));
            }
        };

        vm_ctxt.import_resolver.terms.insert(
            file_id,
            TermEntry {
                value,
                state: TermEntryState::Populated,
                format,
            },
        );

        // unwrap: we don't expect an error here, since we already typechecked above.
        let contract_value = vm_ctxt.prepare_eval_only(contract_id).unwrap();

        let value = NickelValue::term_posless(Term::Annotated(
            TypeAnnotation {
                typ: None,
                contracts: vec![LabeledType {
                    typ: TypeF::Contract(contract_value).into(),
                    label: Default::default(),
                }],
            },
            NickelValue::term_posless(Term::ResolvedImport(file_id)),
        ));

        let mut vm = VirtualMachine::<_, CacheImpl>::new(&mut vm_ctxt);
        let errors = vm.eval_permissive(value, self.config.eval_config.eval_limits.recursion_limit);
        let mut files = vm.import_resolver().sources.files().clone();
        let mut diags: Vec<_> = errors
            .into_iter()
            .flat_map(|e| SerializableDiagnostic::from(e, &mut files, file_id))
            .collect();

        diags.extend(warnings.try_iter().flat_map(|(warning, mut files)| {
            SerializableDiagnostic::from(warning, &mut files, file_id)
        }));

        if diags.is_empty() { Ok(()) } else { Err(diags) }
    }

    /// Performs typechecking without checking for a cached value of the typechecking
    /// result.
    fn typecheck_uncached(&mut self, file_id: FileId) -> Result<(), Vec<SerializableDiagnostic>> {
        let typecheck_result = self
            .analysis_reg
            .modify_and_insert(file_id, |reg, analysis| -> Result<_, Infallible> {
                Ok(analysis.fill_analysis(
                    &mut self.sources,
                    &mut self.import_data,
                    &mut self.import_targets,
                    &mut self.file_uris,
                    reg,
                ))
            })
            // unwrap(): The callback we provided can't fail so the result here is always Ok
            .unwrap()
            // unwrap(): None is only returned when the file ID is unknown. This is a documented
            // situation in which this function may panic.
            .unwrap();

        let mut typecheck_diagnostics: Vec<_> = typecheck_result
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

        // Cache the diagnostics
        for diag in &typecheck_diagnostics {
            self.analysis_reg
                .modify(file_id, |_, a| a.add_typecheck_diagnostic(diag.clone()));
        }

        enum FailedPhase {
            Parsing(InputFormat),
            Typechecking,
        }

        let imported_file_ids: Vec<_> = self.import_data.imports(file_id).collect();
        let failed_imports: Vec<(FileId, FailedPhase)> = imported_file_ids
            .into_iter()
            .filter_map(|id| {
                match self.file_format(id) {
                    Some(InputFormat::Nickel) => {
                        // Any imported files should have been added to the analysis
                        // registry when fill_analysis was run if they weren't
                        // there already, so the parsing errors can be retrieved
                        // from cache.
                        let has_parsing_errors = self
                            .analysis_reg
                            .get(id)
                            .map(|analysis| !analysis.parse_errors().errors.is_empty());

                        // Ideally, it should not be possible to get this far in
                        // typechecking without the analysis for all imported files getting
                        // cached in the analysis registry, and this unwrap should be safe even
                        // in release builds. However, I'm not yet confident that this invariant
                        // holds, so for now release builds will fall back to reparsing the file
                        // while it unwraps in debug builds so it'll get caught as a bug if this
                        // occurs during testing.
                        #[cfg(debug_assertions)]
                        let has_parsing_errors = has_parsing_errors.unwrap();
                        #[cfg(not(debug_assertions))]
                        let has_parsing_errors = has_parsing_errors.unwrap_or_else(|| {
                            warn!(
                                "Analysis for file {:?} imported by {:?} was not cached",
                                id, file_id
                            );
                            !self.parse(id).is_empty()
                        });

                        if has_parsing_errors {
                            Some((id, FailedPhase::Parsing(InputFormat::Nickel)))
                        } else if self.typecheck(id).is_err() {
                            Some((id, FailedPhase::Typechecking))
                        } else {
                            None
                        }
                    }
                    Some(_) => {
                        // It's possible in some cases for the same file to get imported
                        // using different parsers, like if someone changes the line
                        //
                        //   import "x" as 'Json
                        // to
                        //   import "x" as 'Yaml
                        //
                        // This could raise some concern that cached parsing errors could
                        // become out of date due to changes in how a file is being parsed.
                        // However, this turns out not to be an issue, because SourceCache
                        // uses the InputFormat as part of the key to look up files from the
                        // cache, and if the format changes it'll generate a new file ID.
                        // This means that we can safely assume that the file format when
                        // the file was originally parsed is the same as the format it is now.
                        let file_data = self.analysis_reg.get_alt_format_errors(id);
                        let has_parsing_errors = file_data
                            .map(|it| !it.parse_errors.no_errors())
                            .unwrap_or(false);
                        if has_parsing_errors {
                            Some((id, FailedPhase::Parsing(file_data.unwrap().format)))
                        } else {
                            None
                        }
                    }
                    None => None,
                }
            })
            .collect();

        // unwrap(): we're allowed to panic if `file_id` is missing. We didn't remove it, so if it's missing now
        // then it was missing when this method was called.
        let analysis = self.analysis_reg.get(file_id).unwrap();

        let mut import_diagnostics: Vec<SerializableDiagnostic> = failed_imports
            .into_iter()
            .flat_map(|(id, phase)| {
                let message = match phase {
                    FailedPhase::Parsing(format) => &format!(
                        "This import could not be resolved \
                         because its content could not be parsed as {format}."
                    ),
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

        // Cache the diagnostics found while typechecking imports
        for diag in &import_diagnostics {
            self.analysis_reg
                .modify(file_id, |_, a| a.add_typecheck_diagnostic(diag.clone()));
        }
        self.analysis_reg
            .modify(file_id, |_, a| a.complete_typechecking());

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
        use nickel_lang_core::typecheck::{TypecheckMode, typecheck_visit};

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
                    file_uris: &mut self.file_uris,
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
        use nickel_lang_core::error::{EvalError, EvalErrorData};

        let mut diags = self.parse_and_typecheck(file_id);

        if diags.is_empty() {
            let (reporter, warnings) = WarningReporter::new();
            let (cache_hub, pos_table) = self.cache_hub_for_eval(file_id);
            let mut vm_ctxt: VmContext<_, CacheImpl> =
                VmContext::new_with_pos_table(cache_hub, pos_table, std::io::stderr(), reporter);

            // unwrap: we don't expect an error here, since we already typechecked above.
            let rt = vm_ctxt.prepare_eval_only(file_id).unwrap();

            let errors = VirtualMachine::new(&mut vm_ctxt).eval_permissive(rt, recursion_limit);
            // Get a possibly-updated files from the vm instead of relying on the one
            // in `world`.
            let mut files = vm_ctxt.import_resolver.sources.files().clone();

            diags.extend(
                errors
                    .into_iter()
                    .filter(|e| {
                        !matches!(
                            e,
                            EvalError {
                                error: EvalErrorData::MissingFieldDef { .. },
                                ctxt: _
                            }
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
            world.analysis_reg.remove(file_id);

            let rev_deps = world
                .import_data
                .rev_imports
                .remove(&file_id)
                .unwrap_or_default();

            acc.extend(rev_deps.keys());

            for file_id in rev_deps.keys() {
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
        let format = InputFormat::from_path(&path).unwrap_or_default();
        Ok(self.sources.id_of(&SourcePath::Path(path, format)))
    }

    /// Returns a [nickel_lang_core::cache::CacheHub] instance and an associated position table
    /// derived from this world and targeted at the evaluation of a given file. NLS handles files
    /// on its own, but we want to avoid duplicating work when doing background evaluation: loading
    /// the stdlib, parsing, etc.
    ///
    /// This method spins up a new cache, and pre-fill the old AST representation for the stdlib,
    /// the given file and the transitive closure of its imports.
    pub fn cache_hub_for_eval(&self, file_id: FileId) -> (CacheHub, PosTable) {
        use nickel_lang_core::{
            bytecode::ast::compat::ToMainline,
            cache::{TermEntry, TermEntryState},
        };

        let mut cache = CacheHub::new();
        let mut pos_table = self.pos_table.clone();
        cache.sources = self.sources.clone();
        cache.import_data = self.import_data.clone();

        cache.terms.insert(
            self.analysis_reg.stdlib_analysis().file_id(),
            TermEntry {
                value: self.compiled_stdlib.clone(),
                state: TermEntryState::default(),
                format: InputFormat::Nickel,
            },
        );

        cache.terms.insert(
            file_id,
            TermEntry {
                value: self
                    .analysis_reg
                    .get(file_id)
                    .unwrap()
                    .ast()
                    .to_mainline(&mut pos_table),
                state: TermEntryState::default(),
                format: InputFormat::Nickel,
            },
        );

        let mut work_stack: Vec<FileId> = self.import_data.imports(file_id).collect();
        // Imports can be cyclic, so we need to keep track of what we've already done.
        let mut visited = HashSet::from([file_id]);

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
                    value: analysis.ast().to_mainline(&mut pos_table),
                    state: TermEntryState::default(),
                    format: InputFormat::Nickel,
                },
            );

            work_stack.extend(self.import_data.imports(next));
        }

        (cache, pos_table)
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
    pub(crate) new_imports: Vec<AnalysisTarget<'std>>,
    pub(crate) sources: &'a mut SourceCache,
    pub(crate) import_data: &'a mut ImportData,
    pub(crate) import_targets: &'a mut ImportTargets,
    pub(crate) file_uris: &'a mut HashMap<FileId, Url>,
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
        let (import_path, file_id) = possible_parents
            .iter()
            .find_map(|parent| {
                let mut path_buf = parent.clone();
                path_buf.push(path);
                self.sources
                    .get_or_add_file(&path_buf, format)
                    .ok()
                    .map(|id| (path_buf, id.inner()))
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

        let url = Url::from_file_path(import_path).unwrap();
        self.file_uris.insert(file_id, url.clone());

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
                .insert(ImportTarget { file_id, format });
            self.import_data
                .rev_imports
                .entry(file_id)
                .or_default()
                .entry(parent_id)
                .or_insert(*pos);
        }

        if let Some(pkg_id) = pkg_id {
            self.sources.packages.insert(file_id, pkg_id);
        }

        match format {
            InputFormat::Nickel => {
                if let Some(analysis) = self.reg.get(file_id) {
                    Ok(Some(analysis.ast()))
                } else {
                    let analysis = PackedAnalysis::parsed(
                        file_id,
                        self.sources,
                        self.reg.init_term_env.clone(),
                        self.reg.init_type_ctxt.clone(),
                    );
                    // Since `new_imports` owns the packed analysis, we need to push the analysis here
                    // first and then re-borrow it from `new_imports`.
                    self.new_imports.push(AnalysisTarget::Analysis(analysis));
                    if let AnalysisTarget::Analysis(a) = self.new_imports.last().unwrap() {
                        Ok(Some(a.ast()))
                    } else {
                        // We just pushed the Nickel variant to new_imports so this can't
                        // be anything else.
                        unreachable!()
                    }
                }
            }
            InputFormat::Yaml => {
                if let Some(analysis) = self.reg.get(file_id) {
                    Ok(Some(analysis.ast()))
                } else {
                    let analysis = PackedAnalysis::parsed_yaml(file_id, self.sources);
                    // Since `new_imports` owns the packed analysis, we need to push the analysis here
                    // first and then re-borrow it from `new_imports`.
                    self.new_imports.push(AnalysisTarget::Analysis(analysis));
                    if let AnalysisTarget::Analysis(a) = self.new_imports.last().unwrap() {
                        Ok(Some(a.ast()))
                    } else {
                        // We just pushed the Nickel variant to new_imports so this can't
                        // be anything else.
                        unreachable!()
                    }
                }
            }
            format => {
                // Gather parsing errors to display in diagnostics. A fuller
                // analysis isn't possible because other formats don't get
                // parsed into the Nickel AST. At some point we might want to
                // do this, to allow to jump into a definition or to provide
                // completions for JSON files, for example. It's been done for YAML
                // above.
                let parse_errors = self
                    .sources
                    // For now, we only care about parse error diagnostics, which doesn't need to
                    // preserve the position table.
                    .parse_other(&mut PosTable::new(), file_id, format)
                    .err()
                    .map(|e| ParseErrors::new(vec![e]))
                    .unwrap_or_else(|| ParseErrors::new(vec![]));
                self.new_imports
                    .push(AnalysisTarget::Other(AltFormatErrors {
                        file_id,
                        parse_errors,
                        format,
                    }));
                Ok(None)
            }
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

    use lsp_types::{Position, Range};
    use tempfile::TempDir;

    use crate::diagnostic::OrdRange;

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
    fn imported_file_analysis_is_cached_on_parent_typecheck_success() {
        let mut world = World::new(LspConfig::default());

        let (child, _) = world
            .add_file(make_file_url("child.ncl"), "2".to_string())
            .unwrap();
        // The child hasn't been parsed yet so it should not yet be in the registry.
        assert!(world.analysis_reg.get(child).is_none());

        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "(import \"child.ncl\") : Number".to_string(),
            )
            .unwrap();
        world.parse(parent);
        world.typecheck(parent).unwrap();

        assert!(world.analysis_reg.get(child).is_some())
    }

    #[test]
    fn imported_file_analysis_is_cached_on_parent_typecheck_failure() {
        let mut world = World::new(LspConfig::default());

        let (child, _) = world
            .add_file(make_file_url("child.ncl"), "2".to_string())
            .unwrap();

        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "(import \"child.ncl\") : String".to_string(),
            )
            .unwrap();
        world.parse(parent);
        assert!(world.typecheck(parent).is_err());

        assert!(world.analysis_reg.get(child).is_some())
    }

    #[test]
    fn imported_file_analysis_is_cached_on_child_parse_failure() {
        let mut world = World::new(LspConfig::default());

        let (child, _) = world
            .add_file(make_file_url("child.ncl"), "[1,2".to_string())
            .unwrap();

        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "import \"child.ncl\"".to_string(),
            )
            .unwrap();
        world.parse(parent);
        let _ = world.typecheck(parent);

        assert!(
            !world
                .analysis_reg
                .get(child)
                .unwrap()
                .parse_errors()
                .no_errors()
        )
    }

    #[test]
    fn imported_file_analysis_is_cached_on_partial_import_failure() {
        let mut world = World::new(LspConfig::default());

        let (child, _) = world
            .add_file(make_file_url("child.ncl"), "2".to_string())
            .unwrap();
        // The child hasn't been parsed yet so it should not yet be in the registry.
        assert!(world.analysis_reg.get(child).is_none());

        // This imports two files. The first import should be successful,
        // while the second file does not exist and will fail to resolve.
        // The first file should get cached anyway.
        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "let a = import \"child.ncl\" in \
                    let b = import \"missing_file.ncl\" \
                    in [a, b]"
                    .to_string(),
            )
            .unwrap();
        world.parse(parent);
        let _ = world.typecheck(parent);

        assert!(world.analysis_reg.get(child).is_some())
    }

    #[test]
    fn typechecking_succeeds_with_valid_json_import() {
        let mut world = World::new(LspConfig::default());
        world.sources.add_string(
            SourcePath::Path(
                make_file_url("child.json").to_file_path().unwrap(),
                InputFormat::Json,
            ),
            "{ \"a\": 1 }".to_string(),
        );

        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "import \"child.json\"".to_string(),
            )
            .unwrap();
        world.parse(parent);

        assert!(world.typecheck(parent).is_ok());
    }

    #[test]
    fn typechecking_fails_with_invalid_json_import() {
        let mut world = World::new(LspConfig::default());
        let child_path = make_file_url("child.json").to_file_path().unwrap();
        world.sources.add_string(
            SourcePath::Path(child_path.clone(), InputFormat::Json),
            "\"a\": 1 }".to_string(),
        );

        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "let x = import \"child.json\"\nin x".to_string(),
            )
            .unwrap();
        world.parse(parent);

        let diagnostics = world.typecheck(parent).err().unwrap();
        let diagnostic = diagnostics.first().unwrap();
        assert_eq!(
            diagnostic.message,
            format!(
                "import of {} failed: This import could not be resolved because its content \
            could not be parsed as Json.",
                child_path.to_str().unwrap()
            )
        );
        assert_eq!(
            diagnostic.range,
            OrdRange(Range {
                start: Position {
                    line: 0,
                    character: 8
                },
                end: Position {
                    line: 0,
                    character: 27
                }
            })
        )
    }

    #[test]
    fn imported_json_parse_errors_are_cached() {
        let mut world = World::new(LspConfig::default());
        let child = world.sources.add_string(
            SourcePath::Path(
                make_file_url("child.json").to_file_path().unwrap(),
                InputFormat::Json,
            ),
            "\"a\": 1 }".to_string(),
        );

        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "import \"child.json\"".to_string(),
            )
            .unwrap();
        world.parse(parent);
        assert!(world.typecheck(parent).is_err());

        assert!(world.analysis_reg.get_alt_format_errors(child).is_some());
    }

    #[test]
    fn typechecking_succeeds_after_fixing_import_type() {
        let mut world = World::new(LspConfig::default());

        let url = make_file_url("parent.ncl");
        let (parent, _) = world
            .add_file(
                url.clone(),
                "import \"tests/unit_test_resources/imported_yaml.txt\" as 'Json".to_string(),
            )
            .unwrap();
        world.parse(parent);
        assert!(world.typecheck(parent).is_err());

        world
            .update_file(
                url,
                "import \"tests/unit_test_resources/imported_yaml.txt\" as 'Yaml".to_string(),
            )
            .unwrap();
        world.parse(parent);
        world.typecheck(parent).unwrap();
    }

    #[test]
    fn typechecking_errors_are_cached() {
        let mut world = World::new(LspConfig::default());

        let (file_id, _) = world
            .add_file(make_file_url("file.ncl"), "1 : String".to_string())
            .unwrap();
        world.parse(file_id);
        let diagnostics = world.typecheck(file_id).err().unwrap();
        assert_eq!(
            &diagnostics,
            world
                .analysis_reg
                .get(file_id)
                .unwrap()
                .typecheck_diagnostics()
        );
    }

    #[test]
    fn import_errors_are_cached() {
        let mut world = World::new(LspConfig::default());

        world
            .add_file(make_file_url("child.ncl"), "1 : String".to_string())
            .unwrap();

        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "import \"child.ncl\"".to_string(),
            )
            .unwrap();
        world.parse(parent);

        let diagnostics = world.typecheck(parent).err().unwrap();
        assert_eq!(
            &diagnostics,
            world
                .analysis_reg
                .get(parent)
                .unwrap()
                .typecheck_diagnostics()
        );
    }

    #[test]
    fn typecheck_cache_is_invalidated_on_dependency_update() {
        let mut world = World::new(LspConfig::default());

        let url = make_file_url("child.ncl");
        world
            .add_file(url.clone(), "1 : String".to_string())
            .unwrap();

        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "import \"child.ncl\"".to_string(),
            )
            .unwrap();
        world.parse(parent);
        assert!(world.typecheck(parent).is_err());

        world.update_file(url, "1 : Number".to_string()).unwrap();
        world.parse(parent);
        world.typecheck(parent).unwrap();
    }

    #[test]
    fn typecheck_cache_is_invalidated_on_transitive_dependency_update() {
        let mut world = World::new(LspConfig::default());

        let url = make_file_url("a.ncl");
        world
            .add_file(url.clone(), "1 : String".to_string())
            .unwrap();

        world
            .add_file(make_file_url("b.ncl"), "import \"a.ncl\"".to_string())
            .unwrap();

        let (parent, _) = world
            .add_file(make_file_url("c.ncl"), "import \"b.ncl\"".to_string())
            .unwrap();
        world.parse(parent);
        assert!(world.typecheck(parent).is_err());

        world.update_file(url, "1 : Number".to_string()).unwrap();

        world.parse(parent);
        world.typecheck(parent).unwrap();
    }

    #[test]
    fn close_deleted_file() {
        let mut world = World::new(LspConfig::default());

        let url = make_file_url("nonexistent.ncl");
        let (file, _) = world.add_file(url.clone(), "1".to_string()).unwrap();
        world.parse(file);
        world.typecheck(file).unwrap();

        let (new_id, _) = world.close_file(url.clone()).unwrap();
        assert_eq!(new_id, None);

        // Since the in memory file was closed and no matching path was found on the filesystem,
        // no file should be returned when we look up this url.
        assert_eq!(world.file_id(&url).unwrap(), None);
    }

    #[test]
    fn close_existing_file() {
        let mut world = World::new(LspConfig::default());

        let url = make_file_url("tests/unit_test_resources/closed_file.ncl");
        let (file, _) = world
            .add_file(url.clone(), "1 : Number".to_string())
            .unwrap();
        world.parse(file);
        world.typecheck(file).unwrap();

        let (new_id, _) = world.close_file(url.clone()).unwrap();
        // This path exists on the filesystem, so a new ID should have been found.
        assert!(new_id.is_some());
    }

    #[test]
    fn closing_file_invalidates_reverse_deps() {
        let mut world = World::new(LspConfig::default());

        let (parent, _) = world
            .add_file(
                make_file_url("parent.ncl"),
                "import \"tests/unit_test_resources/closed_file.ncl\"".to_string(),
            )
            .unwrap();
        world.parse(parent);

        let url = make_file_url("tests/unit_test_resources/closed_file.ncl");
        let (file, _) = world
            .add_file(url.clone(), "1 : String".to_string())
            .unwrap();
        world.parse(file);

        // The imported file has a type error at this stage, so typechecking the parent
        // should fail.
        assert!(world.typecheck(parent).is_err());

        let (new_id, invalidated) = world.close_file(url.clone()).unwrap();
        assert_eq!(invalidated, vec![parent]);

        world.parse(new_id.unwrap());

        // Since the in memory file was closed, the parent should now be importing the correctly
        // typed file from the filesystem, and should no longer return any diagnostics.
        assert_eq!(world.parse_and_typecheck(parent), vec![]);
    }

    #[test]
    fn contract_changes_reload() {
        let _ = env_logger::try_init();

        let mut world = World::new(LspConfig::default());
        let dir = TempDir::new().unwrap();

        std::fs::write(
            dir.path().join(crate::contracts::CONFIG_FILE_NAME),
            r#"[{ glob = "*.yaml", contract_path = "C.ncl" }]"#,
        )
        .unwrap();

        std::fs::write(dir.path().join("C.ncl"), r#"{ foo | String }"#).unwrap();
        let url = Url::from_file_path(dir.path().join("input.yaml")).unwrap();
        let file_id = world.add_file(url, r#"foo: 1"#.to_owned()).unwrap().0;
        assert!(!world.parse_and_typecheck(file_id).is_empty());

        // Rewriting the contract file on disk should trigger a reload.
        std::fs::write(dir.path().join("C.ncl"), r#"{ foo | Number }"#).unwrap();
        assert!(world.parse_and_typecheck(file_id).is_empty());

        // Opening the contract file in the editor should trigger a reload.
        let contract_url = Url::from_file_path(dir.path().join("C.ncl")).unwrap();
        world
            .add_file(contract_url, r#"{ foo | String }"#.to_owned())
            .unwrap();
        assert!(!world.parse_and_typecheck(file_id).is_empty());
    }
}
