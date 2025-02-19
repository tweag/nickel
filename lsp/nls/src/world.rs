use std::{
    collections::{HashMap, HashSet},
    ffi::OsString,
    path::PathBuf,
};

use log::warn;
use lsp_server::{ErrorCode, ResponseError};
use lsp_types::Url;
use nickel_lang_core::{
    bytecode::ast::{primop::PrimOp, Ast, AstAlloc, Node},
    cache::{CacheError, Caches, ErrorTolerance, ImportData, InputFormat, SourceCache, SourcePath},
    environment::Environment,
    error::{Error, ImportError, IntoDiagnostics, ParseError, ParseErrors},
    files::FileId,
    identifier::Ident,
    parser::{self, lexer::Lexer, ErrorTolerantParser},
    position::{RawPos, RawSpan, TermPos},
    stdlib::StdlibModule,
};

use crate::{
    analysis::{Analysis, AnalysisRegistry, PackedAnalysis},
    cache::CachesExt as _,
    diagnostic::SerializableDiagnostic,
    field_walker::{Def, FieldResolver},
    files::uri_to_path,
    identifier::LocIdent,
};

/// All the state associated with the files we know about.
///
/// Includes cached analyses, cached parse trees, etc.
pub struct World {
    /// The original string content of each source we have read. As we don't use
    /// [nickel_lang_core::cache::Caches], we need to manage sources ourselves.
    pub sources: SourceCache,
    /// In order to return diagnostics, we store the URL of each file we know about.
    pub file_uris: HashMap<FileId, Url>,
    pub analysis_reg: AnalysisRegistry,
    /// The initial environment, analyzed from the stdlib. Its content is allocated into the
    /// corresponding [PackedAnalysis] of the registry, which must thus be guaranteed to live as
    /// long as `self`. Thus, once we've initialized the stdlib, we shouldn't touch its analysis
    /// anymore.
    initial_term_env: crate::usage::Environment<'static>,
    /// A map of import dependencies and reverse import dependencies between Nickel source files.
    import_data: ImportData,
    /// A map associating imported files with failed imports. This allows us to
    /// invalidate the cached version of a file when one of its imports becomes available.
    ///
    /// The keys in this map are the filenames (just the basename; no directory) of the
    /// files that failed to import, and the values in this map are the file ids that tried
    /// to import it.
    pub failed_imports: HashMap<OsString, HashSet<FileId>>,
}

// fn initialize_stdlib(sources: &mut SourceCache, analysis: &mut AnalysisRegistry) -> Environment<Ident, Def> {
unsafe fn initialize_stdlib(
    sources: &mut SourceCache,
    reg: &mut AnalysisRegistry,
) -> Environment<Ident, Def<'static>> {
    let mut initial_env = Environment::default();

    for (module, file_id) in sources.stdlib_modules() {
        let mut analysis = PackedAnalysis::new();
        // We don't recover from failing to load the stdlib
        analysis
            .parse(sources, file_id)
            .expect("internal error: failed to parse the stdlib");

        //typecheck with analysis

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
        }

        reg.analyses.insert(file_id, analysis);
    }

    initial_env
}

impl Default for World {
    fn default() -> Self {
        let mut sources = SourceCache::new();

        if let Ok(nickel_path) = std::env::var("NICKEL_IMPORT_PATH") {
            sources.add_import_paths(nickel_path.split(':'));
        }

        let mut analysis_reg = AnalysisRegistry::new();
        // safety: initialize_stdlib is unsafe because it returns an environment with `'static`
        // lifetime which isn't actually static. We store it in the private field
        // `initial_term_env` and then always expose it through helper with a lifetime bound to
        // `self`, which is ok, thanks to the invariant maintained throughout NLS that the analysis
        // of the `std` module is never evicted from the cache.
        let initial_term_env = unsafe { initialize_stdlib(&mut sources, &mut analysis_reg) };

        Self {
            sources,
            analysis_reg,
            initial_term_env,
            import_data: ImportData::new(),
            file_uris: HashMap::new(),
            failed_imports: HashMap::new(),
        }
    }
}

impl World {
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
            .replace_string_nocache(SourcePath::Path(path, InputFormat::Nickel), contents);

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
            .replace_string_nocache(SourcePath::Path(path, InputFormat::Nickel), contents);

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
    fn associate_failed_import(&mut self, err: &nickel_lang_core::error::Error) {
        if let nickel_lang_core::error::Error::ImportError(ImportError::IOError(name, _, pos)) =
            &err
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
        let analysis = self.analysis_reg.analyses.entry(file_id).or_default();

        analysis
            .parse(&self.sources, file_id)
            .map(|nonfatal| self.lsp_diagnostics(file_id, nonfatal))
            .map_err(|fatal| self.lsp_diagnostics(file_id, fatal))
    }

    /// Typechecks a file, returning diagnostics on error.
    ///
    /// Panics if the file has not yet been parsed. (Use [`World::parse_and_typecheck`] if you
    /// want to do both.)
    pub fn typecheck(&mut self, file_id: FileId) -> Result<(), Vec<SerializableDiagnostic>> {
        self.cache
            .typecheck_with_analysis(file_id, &mut self.analysis_reg)
            .map_err(|error| match error {
                CacheError::Error(tc_error) => tc_error
                    .into_iter()
                    .flat_map(|err| {
                        self.associate_failed_import(&err);
                        self.lsp_diagnostics(file_id, err)
                    })
                    .collect::<Vec<_>>(),
                CacheError::NotParsed => panic!("must parse first!"),
            })?;

        Ok(())
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

    pub fn file_analysis<'ast>(
        &'ast self,
        file: FileId,
    ) -> Result<&'ast Analysis<'ast>, ResponseError> {
        self.analysis_reg
            .analyses
            .get(&file)
            .map(PackedAnalysis::analysis)
            .ok_or_else(|| ResponseError {
                data: None,
                message: "File has not yet been parsed or cached.".to_owned(),
                code: ErrorCode::ParseError as i32,
            })
    }

    pub fn lookup_term_by_position<'ast>(
        &'ast self,
        pos: RawPos,
    ) -> Result<Option<&'ast Ast<'ast>>, ResponseError> {
        Ok(self
            .file_analysis(pos.src_id)?
            .position_lookup
            .get(pos.index))
    }

    pub fn lookup_ident_by_position(
        &self,
        pos: RawPos,
    ) -> Result<Option<crate::identifier::LocIdent>, ResponseError> {
        Ok(self
            .file_analysis(pos.src_id)?
            .position_lookup
            .get_ident(pos.index))
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
                        vec![def.ident().pos.unwrap()]
                    } else {
                        cousins
                            .into_iter()
                            .filter_map(|(loc, _)| loc.pos.into_opt())
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
                        .filter_map(|parent| {
                            parent
                                .field_loc(id.ident())
                                .and_then(|def| def.pos.into_opt())
                        })
                        .collect()
                }
                (Node::Let { bindings, .. }, Some(hovered_id)) => {
                    let mut spans = Vec::new();
                    for (pat, value) in bindings {
                        if let Some((path, _, _)) = pat
                            .bindings()
                            .into_iter()
                            .find(|(_path, bound_id, _)| bound_id.ident() == hovered_id.ident)
                        {
                            let (last, path) = path.split_last()?;
                            let path: Vec<_> = path.iter().map(|id| id.ident()).collect();
                            let parents = resolver.resolve_path(value, path.iter().copied());
                            spans = parents
                                .iter()
                                .filter_map(|parent| {
                                    parent
                                        .field_loc(last.ident())
                                        .and_then(|def| def.pos.into_opt())
                                })
                                .collect();
                            break;
                        }
                    }
                    spans
                }
                (Node::Import(import), _) => {
                    //TODO[RFC007]: maintain a mapping from imports to file id?
                    let pos = world.cache.terms().get(file)?.term.pos;
                    vec![pos.into_opt()?]
                }
                (Node::Record(..), Some(id)) => {
                    let def = Def::Field {
                        ident: id,
                        value: None,
                        record: ast,
                        //TODO[RFC007]: second time that we need `default()` metadata but in the
                        //current implementation we need to allocate them somewhere...
                        metadata: FieldMetadata::default(),
                    };
                    let cousins = resolver.cousin_defs(&def);
                    cousins
                        .into_iter()
                        .filter_map(|(loc, _)| loc.pos.into_opt())
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
            let ast = world.lookup_term_by_position(span.start_pos()).ok()??;

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
}

pub(crate) struct AstImportResolver<'a> {
    analysis: &'a mut Analysis,
}
