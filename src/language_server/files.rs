use std::borrow::BorrowMut;

use crate::{
    cache::{self, CacheError, CacheOp},
    environment::Environment,
    error::{self, ToDiagnostic},
    position::RawSpan,
    typecheck,
};
use anyhow::Result;
use codespan::FileId;
use codespan_reporting::diagnostic::Diagnostic;
use log::{info, trace};
use lsp_server::{Message, Notification, Response};
use lsp_types::{DidOpenTextDocumentParams, Position, PublishDiagnosticsParams};

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

    if let Err(diagnostics) = checked {
        info!("Problems detected: {:?}", diagnostics);
        server.notify(Notification::new(
            "textDocument/publishDiagnostics".into(),
            PublishDiagnosticsParams {
                uri: params.text_document.uri,
                diagnostics,
                version: None,
            },
        ));
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

// fn term_pos_to_range(pos: crate::position::TermPos) -> lsp_types::Range {
//     match pos {
//         crate::position::TermPos::Original(_) => todo!(),
//         crate::position::TermPos::Inherited(RawSpan { end, start, src_id }) => {}
//         crate::position::TermPos::None => todo!(),
//     }
// }
