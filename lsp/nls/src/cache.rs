use std::collections::HashMap;

use codespan::FileId;
use nickel_lang_core::{
    cache::{Cache, CacheError, CacheOp, EntryState, TermEntry},
    error::{Error, ImportError},
    typecheck::{self, linearization::Linearization},
};

use crate::linearization::{building::Building, AnalysisHost, Environment, LinRegistry};

pub trait CacheExt {
    fn typecheck_with_analysis(
        &mut self,
        file_id: FileId,
        initial_ctxt: &typecheck::Context,
        initial_env: &Environment,
        lin_registry: &mut LinRegistry,
    ) -> Result<CacheOp<()>, CacheError<Vec<Error>>>;
}

impl CacheExt for Cache {
    fn typecheck_with_analysis<'a>(
        &mut self,
        file_id: FileId,
        initial_ctxt: &typecheck::Context,
        initial_env: &Environment,
        lin_registry: &mut LinRegistry,
    ) -> Result<CacheOp<()>, CacheError<Vec<Error>>> {
        if !self.terms().contains_key(&file_id) {
            return Err(CacheError::NotParsed);
        }

        let mut import_errors = Vec::new();
        let mut typecheck_import_diagnostics: Vec<FileId> = Vec::new();
        if let Ok(CacheOp::Done((ids, errors))) = self.resolve_imports(file_id) {
            import_errors = errors;
            for id in ids {
                let _ = self.typecheck_with_analysis(id, initial_ctxt, initial_env, lin_registry);
            }
        }

        if let Some(ids) = self.get_imports(&file_id) {
            for id in ids {
                // If we have typechecked a file correctly, its imports should be
                // in the `lin_registry`. The imports that are not in `lin_registry`
                // were not typechecked correctly.
                if !lin_registry.map.contains_key(&id) {
                    typecheck_import_diagnostics.push(id);
                }
            }
        }

        // After self.parse(), the cache must be populated
        let TermEntry { term, state, .. } = self.terms().get(&file_id).unwrap();

        let result = if *state > EntryState::Typechecked && lin_registry.map.contains_key(&file_id)
        {
            Ok(CacheOp::Cached(()))
        } else if *state >= EntryState::Parsed {
            let host = AnalysisHost::new(file_id, initial_env.clone());
            let building = Linearization::new(Building {
                lin_registry,
                linearization: Vec::new(),
                import_locations: HashMap::new(),
                cache: self,
            });
            let (_, linearized) =
                typecheck::type_check_linearize(term, initial_ctxt.clone(), self, host, building)
                    .map_err(|err| vec![Error::TypecheckError(err)])?;
            self.update_state(file_id, EntryState::Typechecked);
            lin_registry.map.insert(file_id, linearized);
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
            let typecheck_import_diagnostics = typecheck_import_diagnostics
            .into_iter()
            .map(|id| {
                let message = "This import could not be resolved because its content has failed to typecheck correctly.";
                // The unwrap is safe here because (1) we have linearized `file_id` and it must be
                // in the `lin_registry` and (2) every resolved import has a corresponding position in
                // the linearization of the file that imports it.
                let pos = lin_registry.map.get(&file_id).and_then(|lin| lin.import_locations.get(&id)).unwrap();
                let name: String = self.name(id).to_str().unwrap().into();
                ImportError::IOError(name, String::from(message), *pos)
            });
            import_errors.extend(typecheck_import_diagnostics);

            Err(CacheError::Error(
                import_errors.into_iter().map(Error::ImportError).collect(),
            ))
        }
    }
}
