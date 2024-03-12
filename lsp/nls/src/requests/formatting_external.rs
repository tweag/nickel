use std::process;

use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{DocumentFormattingParams, Position, Range, TextEdit};
use nickel_lang_core::cache::SourcePath;

use crate::{error::Error, files::uri_to_path, server::Server};

pub const FORMATTING_COMMAND: [&str; 4] = ["topiary", "fmt", "--language", "nickel"];

/// Handle the LSP formatting request from a client using an external binary as a formatter.
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

    let Ok(mut topiary) = process::Command::new(FORMATTING_COMMAND[0])
        .args(&FORMATTING_COMMAND[1..])
        .stdin(process::Stdio::piped())
        .stdout(process::Stdio::piped())
        .stderr(process::Stdio::piped())
        .spawn()
    else {
        return Err(Error::FormattingFailed {
            details: "Executing topiary failed".to_owned(),
            file: params.text_document.uri.clone(),
        }
        .into());
    };

    let mut stdin = topiary.stdin.take().unwrap();

    std::thread::spawn(move || {
        let mut text_bytes = text.as_bytes();
        std::io::copy(&mut text_bytes, &mut stdin).unwrap();
    });

    let output = topiary.wait_with_output().unwrap();

    if !output.status.success() {
        let error = String::from_utf8_lossy(&output.stderr);
        return Err(Error::FormattingFailed {
            details: error.into(),
            file: params.text_document.uri.clone(),
        }
        .into());
    }

    let new_text = String::from_utf8(output.stdout).unwrap();

    let result = Some(vec![TextEdit {
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
        new_text,
    }]);
    server.reply(Response::new_ok(id, result));

    Ok(())
}
