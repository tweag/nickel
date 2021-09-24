use std::borrow::BorrowMut;

use anyhow::Result;
use codespan::FileId;
use codespan_reporting::diagnostic::{self, Diagnostic};
use log::{info, trace};
use lsp_server::{Message, Notification, Response};
use lsp_types::{
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, Position,
    PublishDiagnosticsParams,
};
use nickel::{
    cache::{self, CacheError, CacheOp},
    environment::Environment,
    error::{self, ToDiagnostic},
    position::RawSpan,
    typecheck,
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
        .typecheck(file_id, &Environment::new())
        .map_err(|error| match error {
            CacheError::Error(tc_error) => tc_error.to_diagnostic(server.cache.files_mut(), None),
            CacheError::NotParsed => unreachable!(),
        })
}
