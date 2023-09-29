use std::path::PathBuf;

use anyhow::Result;
use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;
use log::trace;
use lsp_server::RequestId;
use lsp_types::{
    notification::{DidOpenTextDocument, Notification},
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, Url,
};
use nickel_lang_core::{
    cache::{CacheError, CacheOp, SourcePath},
    error::IntoDiagnostics,
};

use crate::{
    error::Error,
    trace::{param::FileUpdate, Enrich, Trace},
};

use super::cache::CacheExt;
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
    let path = uri_to_path(&params.text_document.uri)?;
    let file_id = server
        .cache
        .add_string(SourcePath::Path(path), params.text_document.text);
    server.file_uris.insert(file_id, params.text_document.uri);

    parse_and_typecheck(server, file_id)?;
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

    let path = uri_to_path(&params.text_document.uri)?;
    let file_id = server.cache.replace_string(
        SourcePath::Path(path),
        params.content_changes[0].text.to_owned(),
    );

    // Any transitive dependency of the modified file needs to be re-type-checked (but not
    // re-parsed).
    let invalid = server.cache.get_rev_imports_transitive(file_id);
    for f in &invalid {
        server.analysis.remove(*f);
    }

    // TODO: make this part more abstracted
    //       implement typecheck (at least) as part of a persistent AST representation
    //       for now execute the same as above for handling `open` notifications
    parse_and_typecheck(server, file_id)?;

    for f in &invalid {
        let errors = typecheck(server, *f).err().unwrap_or_default();
        server.issue_diagnostics(*f, errors);
    }
    Trace::reply(id);
    Ok(())
}

pub(crate) fn typecheck(
    server: &mut Server,
    file_id: FileId,
) -> Result<CacheOp<()>, Vec<Diagnostic<FileId>>> {
    server
        .cache
        .typecheck_with_analysis(
            file_id,
            &server.initial_ctxt,
            &server.initial_term_env,
            &mut server.analysis,
        )
        .map_err(|error| match error {
            CacheError::Error(tc_error) => tc_error
                .into_iter()
                .flat_map(|err| err.into_diagnostics(server.cache.files_mut(), None))
                .collect(),
            CacheError::NotParsed => unreachable!(),
        })
}

fn parse_and_typecheck(server: &mut Server, file_id: FileId) -> Result<()> {
    let (parse_errs, fatal) = match server.cache.parse(file_id) {
        Ok(errs) => (errs.inner(), false),
        Err(errs) => (errs, true),
    };
    let mut diags = parse_errs.into_diagnostics(server.cache.files_mut(), None);

    if !fatal {
        trace!("Parsed, checking types");
        if let Err(e) = typecheck(server, file_id) {
            diags.extend_from_slice(&e);
        }
    }
    server.issue_diagnostics(file_id, diags);

    Ok(())
}
