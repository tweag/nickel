use codespan::{ByteIndex, FileId};
use lsp_types::TextDocumentPositionParams;
use nickel_lang_core::term::{RichTerm, Term, Traverse};
use nickel_lang_core::{
    cache::{Cache, CacheError, CacheOp, EntryState, SourcePath, TermEntry},
    error::{Error, ImportError},
    position::RawPos,
    typecheck::{self},
};

use crate::linearization::LinRegistry;
use crate::linearization::{CollectedTypes, TypeCollector};

pub trait CacheExt {
    fn typecheck_with_analysis(
        &mut self,
        file_id: FileId,
        initial_ctxt: &typecheck::Context,
        initial_term_env: &crate::usage::Environment,
        lin_registry: &mut LinRegistry,
    ) -> Result<CacheOp<()>, CacheError<Vec<Error>>>;

    fn position(&self, lsp_pos: &TextDocumentPositionParams)
        -> Result<RawPos, crate::error::Error>;
}

impl CacheExt for Cache {
    fn typecheck_with_analysis<'a>(
        &mut self,
        file_id: FileId,
        initial_ctxt: &typecheck::Context,
        initial_term_env: &crate::usage::Environment,
        lin_registry: &mut LinRegistry,
    ) -> Result<CacheOp<()>, CacheError<Vec<Error>>> {
        if !self.terms().contains_key(&file_id) {
            return Err(CacheError::NotParsed);
        }

        let mut import_errors = Vec::new();
        let mut typecheck_import_diagnostics: Vec<FileId> = Vec::new();
        if let Ok(CacheOp::Done((ids, errors))) = self.resolve_imports(file_id) {
            import_errors = errors;
            // Reverse the imports, so we try to typecheck the leaf dependencies first.
            for &id in ids.iter().rev() {
                let _ =
                    self.typecheck_with_analysis(id, initial_ctxt, initial_term_env, lin_registry);
            }
        }

        for id in self.get_imports(file_id) {
            // If we have typechecked a file correctly, its imports should be
            // in the `lin_registry`. The imports that are not in `lin_registry`
            // were not typechecked correctly.
            if !lin_registry.type_lookups.contains_key(&id) {
                typecheck_import_diagnostics.push(id);
            }
        }

        // After self.parse(), the cache must be populated
        let TermEntry { term, state, .. } = self.terms().get(&file_id).unwrap().clone();

        let result = if state > EntryState::Typechecked
            && lin_registry.type_lookups.contains_key(&file_id)
        {
            Ok(CacheOp::Cached(()))
        } else if state >= EntryState::Parsed {
            let types = TypeCollector::default();
            let (_, type_lookups) = typecheck::type_check_linearize(
                &term,
                initial_ctxt.clone(),
                self,
                types,
                CollectedTypes::default(),
            )
            .map_err(|err| vec![Error::TypecheckError(err)])?;

            lin_registry.insert(file_id, type_lookups, &term, initial_term_env);
            self.update_state(file_id, EntryState::Typechecked);
            Ok(CacheOp::Done(()))
        } else {
            // This is unreachable because `EntryState::Parsed` is the first item of the enum, and
            // we have the case `if *state >= EntryState::Parsed` just before this branch.
            // It it actually unreachable unless the order of the enum `EntryState` is changed.
            unreachable!()
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
                let name: String = self.name(id).to_str().unwrap().into();
                ImportError::IOError(name, String::from(message), pos)
            });
            import_errors.extend(typecheck_import_diagnostics);

            Err(CacheError::Error(
                import_errors.into_iter().map(Error::ImportError).collect(),
            ))
        }
    }

    fn position(
        &self,
        lsp_pos: &TextDocumentPositionParams,
    ) -> Result<RawPos, crate::error::Error> {
        let uri = &lsp_pos.text_document.uri;
        let path = uri
            .to_file_path()
            .map_err(|_| crate::error::Error::FileNotFound(uri.clone()))?;
        let file_id = self
            .id_of(&SourcePath::Path(path))
            .ok_or_else(|| crate::error::Error::FileNotFound(uri.clone()))?;

        let pos = lsp_pos.position;
        let idx =
            codespan_lsp::position_to_byte_index(self.files(), file_id, &pos).map_err(|_| {
                crate::error::Error::InvalidPosition {
                    pos,
                    file: uri.clone(),
                }
            })?;

        Ok(RawPos::new(file_id, ByteIndex(idx as u32)))
    }
}
