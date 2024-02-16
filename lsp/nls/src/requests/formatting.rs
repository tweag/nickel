use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{DocumentFormattingParams, Position, Range, TextEdit};
use nickel_lang_core::cache::SourcePath;

use crate::{error::Error, files::uri_to_path, server::Server};

/// Handle the LSP formatting request from a client using Topiary as a formatting library.
/// If this succeds, it sends a reponse to the server and returns `Ok(..)`, otherwise,
/// it only returns an `Err(..)`.
pub fn handle_format_document(
    params: DocumentFormattingParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let path = uri_to_path(&params.text_document.uri)?;
    let file_id = server.world.cache.id_of(&SourcePath::Path(path)).unwrap();
    let text = server.world.cache.files().source(file_id).clone();
    let document_length = text.lines().count() as u32;

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

    // TODO: instead of always sending a huge edit, we should compute a diff
    // between `text` and `formatted` and send more granular edits.
    let result = (text != formatted).then_some(vec![TextEdit {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            // The end position is exclusive. Since we want to replace the
            // entire document, we specify the beginning of the line after the
            // last line in the document.
            end: Position {
                line: document_length,
                character: 0,
            },
        },
        new_text: formatted,
    }]);
    server.reply(Response::new_ok(id, result));
    Ok(())
}
