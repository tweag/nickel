use codespan::ByteIndex;
use lsp_types::{TextDocumentPositionParams, Url};
use nickel_lang_core::cache_new::{CacheKey, ParsedEntry, ParsedState, SourceCache};
use nickel_lang_core::driver;
use nickel_lang_core::source::SourcePath;
use nickel_lang_core::term::{RichTerm, Term, Traverse};
use nickel_lang_core::transform::import_resolution::tolerant::ResolveResult;
use nickel_lang_core::{
    error::{Error, ImportError},
    position::RawPos,
    typecheck::{self},
};

use crate::analysis::{AnalysisRegistry, TypeCollector};

pub trait CacheExt {
    fn typecheck_with_analysis(
        &mut self,
        file_id: CacheKey,
        initial_ctxt: &typecheck::Context,
        initial_term_env: &crate::usage::Environment,
        registry: &mut AnalysisRegistry,
    ) -> Result<(), Vec<Error>>;

    fn position(&self, lsp_pos: &TextDocumentPositionParams)
        -> Result<RawPos, crate::error::Error>;

    fn find_url(&self, uri: &Url) -> Result<Option<CacheKey>, crate::error::Error>;
}

impl CacheExt for SourceCache {
    fn typecheck_with_analysis<'a>(
        &mut self,
        file_id: CacheKey,
        initial_ctxt: &typecheck::Context,
        initial_term_env: &crate::usage::Environment,
        registry: &mut AnalysisRegistry,
    ) -> Result<(), Vec<Error>> {
        let mut typecheck_import_diagnostics: Vec<CacheKey> = Vec::new();
        let ResolveResult {
            resolved_keys: ids,
            mut import_errors,
            ..
        } = driver::resolve_imports(self, file_id);
        // Reverse the imports, so we try to typecheck the leaf dependencies first.
        for &id in ids.iter().rev() {
            let _ = self.typecheck_with_analysis(id, initial_ctxt, initial_term_env, registry);
        }

        for id in self.get_imports(file_id) {
            // If we have typechecked a file correctly, its imports should be
            // in the `registry`. The imports that are not in `registry`
            // were not typechecked correctly.
            if !registry.analysis.contains_key(&id) {
                typecheck_import_diagnostics.push(id);
            }
        }

        // After self.parse(), the cache must be populated
        let ParsedEntry { term, state } = self.get_parsed(file_id).unwrap().clone();

        let result = if state.typechecked() && registry.analysis.contains_key(&file_id) {
            Ok(())
        } else {
            let mut collector = TypeCollector::default();
            let type_tables = typecheck::type_check_with_visitor(
                &term,
                initial_ctxt.clone(),
                self,
                &mut collector,
            )
            .map_err(|err| vec![Error::TypecheckError(err)])?;
            let wildcards = type_tables.wildcards.clone();

            let type_lookups = collector.complete(type_tables);
            registry.insert(file_id, type_lookups, &term, initial_term_env);
            self.set_parsed_state(file_id, ParsedState::Typechecked { wildcards });
            Ok(())
        };
        if import_errors.is_empty() && typecheck_import_diagnostics.is_empty() {
            result
        } else {
            // Add the correct position to typecheck import errors and then
            // transform them to normal import errors.
            let typecheck_import_diagnostics = typecheck_import_diagnostics.into_iter().map(|id| {
                let message = "This import could not be resolved \
                    because its content has failed to typecheck correctly.";
                // Find a position (one is enough) that the import came from.
                let pos = term
                    .find_map(|rt: &RichTerm| match rt.as_ref() {
                        Term::ResolvedImport(_) => Some(rt.pos),
                        _ => None,
                    })
                    .unwrap_or_default();
                let name: String = format!("{}", self.source_path(id));
                ImportError::IOError(name, String::from(message), pos)
            });
            import_errors.extend(typecheck_import_diagnostics);

            Err(import_errors.into_iter().map(Error::ImportError).collect())
        }
    }

    fn find_url(&self, uri: &Url) -> Result<Option<CacheKey>, crate::error::Error> {
        let path = uri
            .to_file_path()
            .map_err(|_| crate::error::Error::FileNotFound(uri.clone()))?;
        Ok(self.find(&SourcePath::Path(path)))
    }

    fn position(
        &self,
        lsp_pos: &TextDocumentPositionParams,
    ) -> Result<RawPos, crate::error::Error> {
        let uri = &lsp_pos.text_document.uri;
        let file_id = self
            .find_url(uri)?
            .ok_or_else(|| crate::error::Error::FileNotFound(uri.clone()))?;
        let pos = lsp_pos.position;
        let idx =
            crate::codespan_lsp::position_to_byte_index(self, file_id, &pos).map_err(|_| {
                crate::error::Error::InvalidPosition {
                    pos,
                    file: uri.clone(),
                }
            })?;

        Ok(RawPos::new(file_id, ByteIndex(idx as u32)))
    }
}
