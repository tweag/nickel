use anyhow::Result;
use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;
use log::trace;
use lsp_server::RequestId;
use lsp_types::{
    notification::{DidOpenTextDocument, Notification},
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, PublishDiagnosticsParams, Url,
};
use nickel::{
    cache::{CacheError, CacheOp},
    error::ToDiagnostic,
};

use crate::trace::{param::FileUpdate, Enrich, Trace};

use super::cache::CacheExt;
use super::diagnostic::DiagnosticCompat;
use super::server::Server;

pub fn handle_open(server: &mut Server, params: DidOpenTextDocumentParams) -> Result<()> {
    let id: RequestId = format!(
        "{}#{}",
        params.text_document.uri, params.text_document.version
    )
    .into();

    Trace::receive(id.clone(), DidOpenTextDocument::METHOD);
    Trace::enrich(
        &id,
        FileUpdate {
            content: &params.text_document.text,
        },
    );
    let file_id = server
        .cache
        .add_string(params.text_document.uri.as_str(), params.text_document.text);

    parse_and_typecheck(server, params.text_document.uri, file_id)?;
    Trace::reply(id);
    Ok(())
}

pub fn handle_save(server: &mut Server, params: DidChangeTextDocumentParams) -> Result<()> {
    let id: RequestId = format!(
        "{}#{}",
        params.text_document.uri, params.text_document.version
    )
    .into();

    Trace::receive(id.clone(), DidOpenTextDocument::METHOD);
    Trace::enrich(
        &id,
        FileUpdate {
            content: &params.content_changes[0].text,
        },
    );

    let file_id = server.cache.update_content(
        params.text_document.uri.as_str(),
        params.content_changes[0].text.to_owned(),
    )?;

    // TODO: make this part more abstracted
    //       implement typecheck (at least) as part of a persistent AST representation
    //       for now execute the same as above for handling `open` notifications
    parse_and_typecheck(server, params.text_document.uri, file_id)?;
    Trace::reply(id);
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

fn parse_and_typecheck(server: &mut Server, uri: Url, file_id: FileId) -> Result<()> {
    let diagnostics = server
        .cache
        .parse(file_id)
        .map_err(|parse_err| parse_err.to_diagnostic(server.cache.files_mut(), None))
        .and_then(|parse_errs| {
            // Parse errors are not fatal
            let mut d = parse_errs
                .inner()
                .to_diagnostic(server.cache.files_mut(), None);
            trace!("Parsed, checking types");
            let _ = typecheck(server, file_id).map_err(|mut ty_d| d.append(&mut ty_d));
            Ok(d)
        })
        .unwrap_or_else(|d| d);

    let diagnostics = diagnostics
        .into_iter()
        .map(|d| lsp_types::Diagnostic::from_codespan(d, server.cache.files_mut()))
        .flatten()
        .collect();

    server.notify(lsp_server::Notification::new(
        "textDocument/publishDiagnostics".into(),
        PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: None,
        },
    ));

    Ok(())
}
