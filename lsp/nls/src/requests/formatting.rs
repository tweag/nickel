use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{DocumentFormattingParams, Position, Range, TextEdit};

use crate::{error::Error, server::Server};

/// Handle the LSP formatting request from a client using an external binary as a formatter.
/// If this succeds, it sends a reponse to the server and returns `Ok(..)`, otherwise,
/// it only returns an `Err(..)`.
pub fn handle_format_document(
    params: DocumentFormattingParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let document_id = params.text_document.uri.to_file_path().unwrap();
    let file_id = server.cache.id_of(document_id).unwrap();
    let text = server.cache.files().source(file_id).clone();
    let document_length = text.lines().count() as u32;
    let last_line_length = text.lines().next_back().unwrap().len() as u32;

    let mut formatted: Vec<u8> = Vec::new();
    nickel_lang_core::format::format(text.as_bytes(), &mut formatted).map_err(|err| {
        Error::FormattingFailed {
            details: format!("{err}"),
            file: params.text_document.uri.clone(),
        }
    })?;

    let formatted = String::from_utf8(formatted).map_err(|_err| Error::FormattingFailed {
        details: "Topiary produced invalid UTF-8".to_owned(),
        file: params.text_document.uri,
    })?;

    let result = Some(vec![TextEdit {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: document_length - 1,
                character: last_line_length,
            },
        },
        new_text: formatted,
    }]);
    server.reply(Response::new_ok(id, result));
    Ok(())
}
