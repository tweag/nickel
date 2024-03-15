use std::path::PathBuf;

use anyhow::Result;
use lsp_server::RequestId;
use lsp_types::{
    notification::{DidOpenTextDocument, Notification},
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, Url,
};

use crate::{
    error::Error,
    trace::{param::FileUpdate, Enrich, Trace},
};

use super::server::Server;

pub(crate) fn uri_to_path(uri: &Url) -> std::result::Result<PathBuf, Error> {
    if uri.scheme() != "file" {
        Err(Error::SchemeNotSupported(uri.scheme().into()))
    } else {
        uri.to_file_path()
            .map_err(|_| Error::InvalidPath(uri.clone()))
    }
}

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
    let (file_id, invalid) = server
        .world
        .add_file(params.text_document.uri.clone(), params.text_document.text)?;

    let diags = server.world.parse_and_typecheck(file_id);
    server.issue_diagnostics(file_id, diags);

    for rev_dep in &invalid {
        let diags = server.world.parse_and_typecheck(*rev_dep);
        server.issue_diagnostics(*rev_dep, diags);
    }
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

    let (file_id, invalid) = server.world.update_file(
        params.text_document.uri.clone(),
        params.content_changes[0].text.clone(),
    )?;

    let diags = server.world.parse_and_typecheck(file_id);
    server.issue_diagnostics(file_id, diags);

    for f in &invalid {
        let errors = server.world.typecheck(*f).err().unwrap_or_default();
        server.issue_diagnostics(*f, errors);
    }
    Trace::reply(id);
    Ok(())
}
