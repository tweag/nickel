use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{DocumentFormattingParams, Position, Range, TextEdit};

use crate::server::Server;

pub fn handle_format_document(
    params: DocumentFormattingParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let document_id = params.text_document.uri.to_file_path().unwrap();
    let file_id = server.cache.id_of(document_id).unwrap();
    let text = server.cache.files().source(file_id);

    let new_text = String::new();
    let result = Some(vec![TextEdit {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            // for now
            end: Position {
                line: 0,
                character: 0,
            },
        },
        new_text,
    }]);
    let response = Response::new_ok(id, result);

    Ok(())
}
