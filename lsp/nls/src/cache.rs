use codespan::ByteIndex;
use lsp_types::{TextDocumentPositionParams, Url};
use nickel_lang_core::{
    bytecode::ast::{Ast, Node},
    cache::{CacheError, CacheOp, Caches, EntryState, InputFormat, SourcePath, TermEntry},
    error::{Error, ImportError},
    files::FileId,
    position::RawPos,
    traverse::TraverseAlloc,
    typecheck::{self},
};

use crate::analysis::{AnalysisRegistry, TypeCollector};

pub struct PackedAst {
    alloc: AstAlloc,
    root: &'ast Ast<'static>,
}

impl PackedAst {
    pub fn borrow<'ast>(&'ast self) -> &'ast Ast<'ast> {
        self.root
    }

    pub fn borrow_mut() {}
}

pub struct Cache {
    asts: HashMap<FileId, PackedAst>,
}

pub trait CachesExt {
    fn typecheck_with_analysis<'ast>(
        &'ast mut self,
        file_id: FileId,
        initial_term_env: &crate::usage::Environment<'ast>,
        registry: &mut AnalysisRegistry<'ast>,
    ) -> Result<CacheOp<()>, CacheError<Vec<Error>>>;

    fn position(&self, lsp_pos: &TextDocumentPositionParams)
        -> Result<RawPos, crate::error::Error>;

    fn file_id(&self, uri: &Url) -> Result<Option<FileId>, crate::error::Error>;
}

impl CachesExt for Caches {
    fn typecheck_with_analysis<'ast>(
        &'ast mut self,
        file_id: FileId,
        initial_term_env: &crate::usage::Environment<'ast>,
        registry: &mut AnalysisRegistry<'ast>,
    ) -> Result<CacheOp<()>, CacheError<Vec<Error>>> {
        if !self.terms.contains(file_id) {
            return Err(CacheError::NotParsed);
        }

        let mut import_errors = Vec::new();
        let mut typecheck_import_diagnostics: Vec<FileId> = Vec::new();

        if let Ok(CacheOp::Done((ids, errors))) = self.resolve_imports(file_id) {
            import_errors = errors;
            // Reverse the imports, so we try to typecheck the leaf dependencies first.
            for &id in ids.iter().rev() {
                let _ = self.typecheck_with_analysis(id, initial_term_env, registry);
            }
        }

        for id in self.import_data.get_imports(file_id) {
            // If we have typechecked a file correctly, its imports should be
            // in the `registry`. The imports that are not in `registry`
            // were not typechecked correctly.
            if !registry.analysis.contains_key(&id) {
                typecheck_import_diagnostics.push(id);
            }
        }

        // unwrap(): We check at the very beginning of the method that the term has been parsed.
        let state = self.terms.entry_state(file_id).unwrap();

        let result = if state > EntryState::Typechecked && registry.analysis.contains_key(&file_id)
        {
            Ok(CacheOp::Cached(()))
        } else {
            let mut collector = TypeCollector::default();

            let type_tables = self
                .asts
                .typecheck_visit_one(
                    &mut self.sources,
                    &mut self.terms,
                    &mut self.import_data,
                    self.error_tolerance,
                    file_id,
                    &mut collector,
                    typecheck::TypecheckMode::Walk,
                )
                // unwrap(): We check at the very beginning of the method that the term has been parsed.
                .map_err(|err| {
                    vec![Error::TypecheckError(err.unwrap_error(
                        "nls: already checked that the file was properly parsed",
                    ))]
                })?;

            // unwrap(): We check at the very beginning of the method that the term has been parsed.
            let ast = self.asts.get(&file_id).unwrap();
            let type_lookups = collector.complete(self.asts.get_alloc(), type_tables);
            registry.insert(file_id, type_lookups, ast, initial_term_env);
            self.terms.update_state(file_id, EntryState::Typechecked);
            Ok(CacheOp::Done(()))
        };

        if import_errors.is_empty() && typecheck_import_diagnostics.is_empty() {
            result
        } else {
            // Add the correct position to typecheck import errors and then
            // transform them to normal import errors.
            let typecheck_import_diagnostics = typecheck_import_diagnostics.into_iter().map(|id| {
                let message = "This import could not be resolved \
                    because its content has failed to typecheck correctly.";
                // Find a position (one is enough) where the import came from.
                let pos = self
                    .asts
                    .get(&file_id)
                    .unwrap()
                    .find_map(|ast: &'ast Ast<'ast>| match &ast.node {
                        // TODO: we need to find the right import, not just the first one. But since
                        // the RFC007 migration, it's not as obvious as it was to map an import to a
                        // file id. Also this bug was present before the migration, so leaving it
                        // this way temporarily.
                        Node::Import { .. } => Some(ast.pos),
                        _ => None,
                    })
                    .unwrap_or_default();
                let name: String = self.sources.name(id).to_str().unwrap().into();
                ImportError::IOError(name, String::from(message), pos)
            });
            import_errors.extend(typecheck_import_diagnostics);

            Err(CacheError::Error(
                import_errors.into_iter().map(Error::ImportError).collect(),
            ))
        }
    }

    fn file_id(&self, uri: &Url) -> Result<Option<FileId>, crate::error::Error> {
        let path = uri
            .to_file_path()
            .map_err(|_| crate::error::Error::FileNotFound(uri.clone()))?;
        Ok(self
            .sources
            .id_of(&SourcePath::Path(path, InputFormat::Nickel)))
    }

    fn position(
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
}
