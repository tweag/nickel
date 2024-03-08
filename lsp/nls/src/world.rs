use std::{
    collections::{HashMap, HashSet},
    ffi::OsString,
    path::PathBuf,
};

use codespan::FileId;
use lsp_server::{ErrorCode, ResponseError};
use lsp_types::Url;
use nickel_lang_core::{
    cache::{Cache, CacheError, ErrorTolerance, SourcePath},
    error::{ImportError, IntoDiagnostics},
    position::{RawPos, RawSpan},
    term::{record::FieldMetadata, RichTerm, Term, UnaryOp},
    typecheck::Context,
};

use crate::{
    analysis::{Analysis, AnalysisRegistry},
    cache::CacheExt as _,
    diagnostic::{DiagnosticCompat, SerializableDiagnostic},
    field_walker::{Def, FieldResolver},
    files::uri_to_path,
    identifier::LocIdent,
    pattern::Bindings as _,
};

/// All the state associated with the files we know about.
///
/// Includes cached analyses, cached parse trees, etc.
pub struct World {
    pub cache: Cache,
    /// In order to return diagnostics, we store the URL of each file we know about.
    pub file_uris: HashMap<FileId, Url>,
    pub analysis: AnalysisRegistry,
    pub initial_ctxt: Context,
    pub initial_term_env: crate::usage::Environment,

    /// A map associating imported files with failed imports. This allows us to
    /// invalidate the cached version of a file when one of its imports becomes available.
    ///
    /// The keys in this map are the filenames (just the basename; no directory) of the
    /// files that failed to import, and the values in this map are the file ids that tried
    /// to import it.
    pub failed_imports: HashMap<OsString, HashSet<FileId>>,
}

impl Default for World {
    fn default() -> Self {
        let mut cache = Cache::new(ErrorTolerance::Tolerant);
        if let Ok(nickel_path) = std::env::var("NICKEL_IMPORT_PATH") {
            cache.add_import_paths(nickel_path.split(':'));
        }
        // We don't recover from failing to load the stdlib for now.
        cache.load_stdlib().unwrap();
        let initial_ctxt = cache.mk_type_ctxt().unwrap();

        let mut analysis = AnalysisRegistry::default();
        let initial_term_env = crate::utils::initialize_stdlib(&mut cache, &mut analysis);

        Self {
            cache,
            initial_ctxt,
            analysis,
            initial_term_env,
            file_uris: HashMap::default(),
            failed_imports: HashMap::default(),
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
        let mut invalid = path
            .file_name()
            .and_then(|name| self.failed_imports.remove(name))
            .unwrap_or_default();

        // Replace the path (as opposed to adding it): we may already have this file in the
        // cache if it was imported by an already-open file.
        let file_id = self.cache.replace_string(SourcePath::Path(path), contents);

        // Invalidate any cached inputs that imported the newly-opened file, so that any
        // cross-file references are updated.
        invalid.extend(self.cache.get_rev_imports_transitive(file_id));

        for rev_dep in &invalid {
            self.analysis.remove(*rev_dep);
            // Reset the cached state (Parsed is the earliest one) so that it will
            // re-resolve its imports.
            self.cache
                .update_state(*rev_dep, nickel_lang_core::cache::EntryState::Parsed);
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
    ) -> anyhow::Result<(FileId, HashSet<FileId>)> {
        let path = uri_to_path(&uri)?;
        let file_id = self.cache.replace_string(SourcePath::Path(path), contents);

        let invalid = self.cache.get_rev_imports_transitive(file_id);
        for f in &invalid {
            self.analysis.remove(*f);
        }
        Ok((file_id, invalid))
    }

    pub fn lsp_diagnostics(
        &mut self,
        file_id: FileId,
        err: impl IntoDiagnostics<FileId>,
    ) -> Vec<SerializableDiagnostic> {
        let stdlib_ids = self.cache.get_all_stdlib_modules_file_id();
        err.into_diagnostics(self.cache.files_mut(), stdlib_ids.as_ref())
            .into_iter()
            .flat_map(|d| SerializableDiagnostic::from_codespan(file_id, d, self.cache.files_mut()))
            .collect()
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
        self.cache
            .parse(file_id)
            .map(|nonfatal| self.lsp_diagnostics(file_id, nonfatal.inner()))
            .map_err(|fatal| self.lsp_diagnostics(file_id, fatal))
    }

    /// Typechecks a file, returning diagnostics on error.
    ///
    /// Panics if the file has not yet been parsed. (Use [`World::parse_and_typecheck`] if you
    /// want to do both.)
    pub fn typecheck(&mut self, file_id: FileId) -> Result<(), Vec<SerializableDiagnostic>> {
        self.cache
            .typecheck_with_analysis(
                file_id,
                &self.initial_ctxt,
                &self.initial_term_env,
                &mut self.analysis,
            )
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

    pub fn file_analysis(&self, file: FileId) -> Result<&Analysis, ResponseError> {
        self.analysis
            .analysis
            .get(&file)
            .ok_or_else(|| ResponseError {
                data: None,
                message: "File has not yet been parsed or cached.".to_owned(),
                code: ErrorCode::ParseError as i32,
            })
    }

    pub fn lookup_term_by_position(&self, pos: RawPos) -> Result<Option<&RichTerm>, ResponseError> {
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
    pub fn get_defs(&self, term: &RichTerm, ident: Option<LocIdent>) -> Vec<RawSpan> {
        // The inner function returning Option is just for ?-early-return convenience.
        fn inner(world: &World, term: &RichTerm, ident: Option<LocIdent>) -> Option<Vec<RawSpan>> {
            let resolver = FieldResolver::new(world);
            let ret = match (term.as_ref(), ident) {
                (Term::Var(id), _) => {
                    let id = LocIdent::from(*id);
                    let def = world.analysis.get_def(&id)?;
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
                (Term::Op1(UnaryOp::StaticAccess(id), parent), _) => {
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
                (Term::LetPattern(pat, value, _), Some(hovered_id)) => {
                    let (path, _, _) = pat
                        .bindings()
                        .into_iter()
                        .find(|(_path, bound_id, _)| bound_id.ident() == hovered_id.ident)?;

                    let (last, path) = path.split_last()?;
                    let path: Vec<_> = path.iter().map(|id| id.ident()).collect();
                    let parents = resolver.resolve_path(value, path.iter().copied());
                    parents
                        .iter()
                        .filter_map(|parent| {
                            parent
                                .field_loc(last.ident())
                                .and_then(|def| def.pos.into_opt())
                        })
                        .collect()
                }
                (Term::ResolvedImport(file), _) => {
                    let pos = world.cache.terms().get(file)?.term.pos;
                    vec![pos.into_opt()?]
                }
                (Term::RecRecord(..) | Term::Record(_), Some(id)) => {
                    let def = Def::Field {
                        ident: id,
                        value: None,
                        record: term.clone(),
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

        inner(self, term, ident).unwrap_or_default()
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
            let term = world.lookup_term_by_position(span.start_pos()).ok()??;

            if let Term::RecRecord(..) | Term::Record(_) = term.as_ref() {
                let accesses = world.analysis.get_static_accesses(ident.ident);
                Some(
                    accesses
                        .into_iter()
                        .filter_map(|access| {
                            let Term::Op1(UnaryOp::StaticAccess(id), _) = access.as_ref() else {
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
}
