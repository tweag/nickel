use std::{collections::HashMap, ffi::OsString, io};

use codespan::FileId;
use nickel_lang::{
    cache::{Cache, CacheError, CacheOp, CachedTerm, EntryState},
    error::{Error, ImportError},
    term::{RichTerm, Term, Traverse, TraverseOrder},
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
    ) -> Result<CacheOp<()>, CacheError<Vec<Error>>>;
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
    ) -> Result<CacheOp<()>, CacheError<Vec<Error>>> {
        if !self.terms().contains_key(&file_id) {
            return Err(CacheError::NotParsed);
        }

        let mut import_errors = Vec::new();
        if let Ok(CacheOp::Done((ids, errors))) = self.resolve_imports(file_id) {
            import_errors = errors;
            for id in ids {
                // Ignore the results, and check for errors after resolution
                let _ = self.typecheck_with_analysis(id, initial_ctxt, initial_env, lin_cache);
            }
        }

        // After self.parse(), the cache must be populated
        let CachedTerm {
            term,
            state,
            parse_errs,
        } = self.terms_mut().remove(&file_id).unwrap();

        // We do this to get a list of the imports that have been resolved
        // but don't typecheck, and then, we give an appriopiate diagnostics
        let mut errors = Vec::new();
        let term = term.traverse::<_, _, ()>(
            // O(n) every time, where n is the size of the term
            // Alternatively, we can get the list of resolved imports of this file 
            // that do not have a corresponding entry in `lin_cache`, but with that 
            // we cannot know the correct location of the import term, which is more 
            // important for resporting diagnostics.
            &|rt, errors: &mut Vec<_>| {
                let RichTerm { ref term, pos } = rt;
                match term.as_ref() {
                    Term::ResolvedImport(id) if !lin_cache.contains_key(id) => {
                        let name: String = self.name(*id).to_str().unwrap().into();
                        errors.push((name, pos));
                        Ok(rt)
                    }
                    _ => Ok(rt),
                }
            },
            &mut errors,
            TraverseOrder::TopDown,
        )
        .unwrap();

        self.terms_mut().insert(
            file_id,
            CachedTerm {
                term,
                state,
                parse_errs,
            },
        );

        let message = "This import could not be resolved because its content have either failed to parse or typecheck correctly.";
        let errors = errors
            .into_iter()
            .map(|(name, pos)| ImportError::IOError(name, String::from(message), pos));

        import_errors.extend(errors);

        // After self.parse(), the cache must be populated
        let CachedTerm { term, state, .. } = self.terms().get(&file_id).unwrap();

        let result = if *state > EntryState::Typechecked && lin_cache.contains_key(&file_id) {
            Ok(CacheOp::Cached(()))
        } else if *state >= EntryState::Parsed {
            let host = AnalysisHost::new(file_id, initial_env.clone());
            let building = Linearization::new(Building {
                lin_cache,
                linearization: Vec::new(),
                cache: self,
            });
            let (_, linearized) =
                typecheck::type_check_linearize(term, initial_ctxt.clone(), self, host, building)
                    .map_err(|err| vec![Error::TypecheckError(err)])?;
            self.update_state(file_id, EntryState::Typechecked);
            lin_cache.insert(file_id, linearized);
            Ok(CacheOp::Done(()))
        } else {
            unreachable!()
        };
        if import_errors.is_empty() {
            result
        } else {
            Err(CacheError::Error(
                import_errors.into_iter().map(Error::ImportError).collect(),
            ))
        }
    }
}
