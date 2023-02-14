use std::{collections::HashMap, ffi::OsString, io};

use codespan::FileId;
use lsp_server::{Connection, Message};
use lsp_types::{PublishDiagnosticsParams, Url};
use nickel_lang::{
    cache::{Cache, CacheError, CacheOp, CachedTerm, EntryState},
    error::{ToDiagnostic, TypecheckError},
    typecheck::{self, linearization::Linearization},
};

use crate::{
    diagnostic::DiagnosticCompat,
    linearization::{building::Building, completed::Completed, AnalysisHost, Environment},
};

pub trait CacheExt {
    fn update_content(&mut self, path: impl Into<OsString>, s: String) -> io::Result<FileId>;
    fn typecheck_with_analysis(
        &mut self,
        url: Url,
        file_id: FileId,
        initial_ctxt: &typecheck::Context,
        initial_env: &Environment,
        server_connection: &Connection,
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
        uri: Url,
        file_id: FileId,
        initial_ctxt: &typecheck::Context,
        initial_env: &Environment,
        server_connection: &Connection,
        lin_cache: &mut HashMap<FileId, Completed>,
    ) -> Result<CacheOp<()>, CacheError<TypecheckError>> {
        if !self.terms().contains_key(&file_id) {
            return Err(CacheError::NotParsed);
        }

        if let Ok(CacheOp::Done((ids, errors))) = self.resolve_imports(file_id, true) {
            let diagnostics = errors
                .iter()
                .flat_map(|error| error.to_diagnostic(self.files_mut(), None))
                .collect::<Vec<_>>();

            let diagnostics = diagnostics
                .into_iter()
                .flat_map(|d| lsp_types::Diagnostic::from_codespan(d, self.files_mut()))
                .collect();

            let notification = lsp_server::Notification::new(
                "textDocument/publishDiagnostics".into(),
                PublishDiagnosticsParams {
                    uri: uri.clone(),
                    diagnostics,
                    version: None,
                },
            );
            server_connection
                .sender
                .send(Message::Notification(notification))
                .unwrap();

            for id in ids {
                self.typecheck_with_analysis(
                    uri.clone(),
                    id,
                    initial_ctxt,
                    initial_env,
                    server_connection,
                    lin_cache,
                )
                .unwrap();
            }
        }

        // After self.parse(), the cache must be populated
        let CachedTerm { term, state, .. } = self.terms().get(&file_id).unwrap();

        if *state > EntryState::Typechecked && lin_cache.contains_key(&file_id) {
            Ok(CacheOp::Cached(()))
        } else if *state >= EntryState::Parsed {
            let host = AnalysisHost::new(file_id, initial_env.clone());
            let building = Linearization::new(Building {
                lin_cache,
                linearization: Vec::new(),
                cache: self,
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
