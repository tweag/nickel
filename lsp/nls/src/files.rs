use anyhow::Result;
use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;
use log::trace;
use lsp_server::Notification;
use lsp_types::{DidChangeTextDocumentParams, DidOpenTextDocumentParams, PublishDiagnosticsParams};
use nickel::{
    cache::{CacheError, CacheOp},
    error::ToDiagnostic,
};

use super::cache::CacheExt;
use super::diagnostic::DiagnosticCompat;
use super::server::Server;

pub fn handle_open(server: &mut Server, params: DidOpenTextDocumentParams) -> Result<()> {
    let file_id = server
        .cache
        .add_string(params.text_document.uri.as_str(), params.text_document.text);

    let checked = server
        .cache
        .parse(file_id)
        .map_err(|parse_err| parse_err.to_diagnostic(server.cache.files_mut(), None))
        .map_err(|mut d| {
            trace!("Parsed with errors, checking types");
            let _ = typecheck(server, file_id).map_err(|mut ty_d| d.append(&mut ty_d));
            d
        })
        .and_then(|_| {
            trace!("Parsed, checking types");
            typecheck(server, file_id)
        })
        .map_err(|diagnostics| {
            trace!("Parsing or typechecking caused an error!");
            diagnostics
                .into_iter()
                .map(|d| lsp_types::Diagnostic::from_codespan(d, server.cache.files_mut()))
                .flatten()
                .collect()
        });

    match checked.map(|_| Vec::new()) {
        Ok(diagnostics) | Err(diagnostics) => {
            server.notify(Notification::new(
                "textDocument/publishDiagnostics".into(),
                PublishDiagnosticsParams {
                    uri: params.text_document.uri,

                    diagnostics,
                    version: None,
                },
            ));
        }
    }

    Ok(())
}

pub fn handle_save(server: &mut Server, params: DidChangeTextDocumentParams) -> Result<()> {
    let file_id = server.cache.update_content(
        params.text_document.uri.as_str(),
        params.content_changes[0].text.to_owned(),
    )?;

    // TODO: make this part more abstracted
    //       implement typecheck (at least) as part of a persistent AST representation
    //       for now execute the same as above for handling `open` notifications
    let checked = server
        .cache
        .parse(file_id)
        .map_err(|parse_err| parse_err.to_diagnostic(server.cache.files_mut(), None))
        .and_then(|_| {
            trace!("Parsed, checking types");
            typecheck(server, file_id)
        })
        .map_err(|diagnostics| {
            trace!("Parsing or typechecking caused an error!");
            diagnostics
                .into_iter()
                .map(|d| lsp_types::Diagnostic::from_codespan(d, server.cache.files_mut()))
                .flatten()
                .collect()
        });

    match checked.map(|_| Vec::new()) {
        Ok(diagnostics) | Err(diagnostics) => {
            server.notify(Notification::new(
                "textDocument/publishDiagnostics".into(),
                PublishDiagnosticsParams {
                    uri: params.text_document.uri,
                    diagnostics,
                    version: None,
                },
            ));
        }
    }

    Ok(())
}

fn typecheck(server: &mut Server, file_id: FileId) -> Result<CacheOp<()>, Vec<Diagnostic<FileId>>> {
    server
        .cache
        .typecheck_with_analysis(file_id, &server.global_env, &mut server.lin_cache)
        .map_err(|error| match error {
            CacheError::Error(tc_error) => tc_error.to_diagnostic(server.cache.files_mut(), None),
            CacheError::NotParsed => unreachable!(),
        })
}
