use std::process;

use lsp_server::{ErrorCode, RequestId, Response, ResponseError};
use lsp_types::{DocumentFormattingParams, Position, Range, TextEdit};

use crate::server::{self, Server};

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
    let last_line_length = text.lines().rev().next().unwrap().len() as u32;

    let formatting_command = server::FORMATTING_COMMAND;
    let Ok(mut topiary) = process::Command::new(formatting_command[0])
        .args(&formatting_command[1..])
        .stdin(process::Stdio::piped())
        .stdout(process::Stdio::piped())
        .stderr(process::Stdio::piped())
        .spawn() else {
	    let message = "Executing topiary failed";
	    return Err(ResponseError {
		code: ErrorCode::InternalError as i32,
		message: String::from(message),
		data: None,
            });
	};

    let mut stdin = topiary.stdin.take().unwrap();

    std::thread::spawn(move || {
        let mut text_bytes = text.as_bytes();
        std::io::copy(&mut text_bytes, &mut stdin).unwrap();
    });

    let output = topiary.wait_with_output().unwrap();

    if !output.status.success() {
        let error = String::from_utf8_lossy(&output.stderr);
        return Err(ResponseError {
            code: ErrorCode::InternalError as i32,
            message: error.to_string(),
            data: None,
        });
    }

    let new_text = String::from_utf8(output.stdout).unwrap();

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
        new_text,
    }]);
    server.reply(Response::new_ok(id, result));
    Ok(())
}
