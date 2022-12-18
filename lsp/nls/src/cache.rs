use std::{collections::HashMap, ffi::OsString, io};

use codespan::FileId;
use nickel_lang::{
    cache::{Cache, CacheError, CacheOp, CachedTerm, EntryState},
    error::TypecheckError,
    typecheck::{self, linearization::Linearization},
};

use crate::linearization::{building::Building, completed::Completed, AnalysisHost, Environment};

pub trait CacheExt {
    fn update_content(&mut self, path: impl Into<OsString>, s: String) -> io::Result<FileId>;
    fn typecheck_with_analysis(
        &mut self,
        file_id: FileId,
        initial_ctxt: &typecheck::Context,
        initial_env: &Environment,
        lin_cache: &mut HashMap<FileId, Completed>,
    ) -> Result<CacheOp<()>, CacheError<TypecheckError>>;
}

impl CacheExt for Cache {
    fn update_content(&mut self, path: impl Into<OsString>, source: String) -> io::Result<FileId> {
        let path: OsString = path.into();
        if let Some(file_id) = self.id_of(path.clone()) {
            self.files_mut().update(file_id, source);
            // invalidate cache so the file gets parsed again
            self.terms_mut().remove(&file_id);
            Ok(file_id)
        } else {
            Ok(self.add_string(path, source))
        }
    }

    fn typecheck_with_analysis<'a>(
        &mut self,
        file_id: FileId,
        initial_ctxt: &typecheck::Context,
        initial_env: &Environment,
        lin_cache: &mut HashMap<FileId, Completed>,
    ) -> Result<CacheOp<()>, CacheError<TypecheckError>> {
        if !self.terms().contains_key(&file_id) {
            return Err(CacheError::NotParsed);
        }

        if let Ok(CacheOp::Done(ids)) = self.resolve_imports(file_id) {
            // TODO: we need a way to stop infite recursion with imports
            for id in ids {
                if id == file_id {
                    continue;
                }
                // Linearize all imports in this file
                // NOTE: This only goes down one level
                self.typecheck_with_analysis(id, initial_ctxt, initial_env, lin_cache)
                    .unwrap();
            }
        }

        // After self.parse(), the cache must be populated
        let CachedTerm { term, state, .. } = self.terms().get(&file_id).unwrap();

        if *state > EntryState::Typechecked && lin_cache.contains_key(&file_id) {
            Ok(CacheOp::Cached(()))
        } else if *state >= EntryState::Parsed {
            let host = AnalysisHost::new(file_id, initial_env.clone());
            let terms = self.terms();
            let building = Linearization::new(Building {
                lin_cache,
                linearization: Vec::new(),
                terms,
            });
            let (_, linearized) =
                typecheck::type_check_linearize(term, initial_ctxt.clone(), self, host, building)?;
            self.update_state(file_id, EntryState::Typechecked);
            lin_cache.insert(file_id, linearized);
            Ok(CacheOp::Done(()))
        } else {
            panic!()
        }
    }
}
