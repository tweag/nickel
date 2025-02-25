use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{CodeActionOrCommand, CodeActionParams};

use crate::{cache::CacheExt, server::Server};

pub fn handle_code_action(
    params: CodeActionParams,
    req: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let mut actions = Vec::new();

    if server
        .world
        .cache
        .file_id(&params.text_document.uri)?
        .is_some()
    {
        actions.push(CodeActionOrCommand::Command(lsp_types::Command {
            title: "evaluate term".to_owned(),
            command: "eval".to_owned(),
            arguments: Some(vec![serde_json::to_value(&params.text_document).unwrap()]),
        }));
    }

    server.reply(Response::new_ok(req, Some(actions)));
    Ok(())
}
