use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{ExecuteCommandParams, TextDocumentIdentifier, Url};

use crate::{error::Error, server::Server};

pub fn handle_command(
    params: ExecuteCommandParams,
    req: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    match params.command.as_str() {
        "eval" => {
            let doc: TextDocumentIdentifier =
                serde_json::from_value(params.arguments[0].clone()).unwrap();
            eval(server, &doc.uri)?;
            server.reply(Response::new_ok(req, None::<()>));
            Ok(())
        }
        _ => Err(Error::CommandNotFound(params.command).into()),
    }
}

fn eval(server: &mut Server, uri: &Url) -> Result<(), Error> {
    if let Some(file_id) = server.world.file_id(uri)? {
        let config = server.world.local_configs.config_for(uri);
        let diags = server
            .world
            .eval_diagnostics(file_id, config.eval_config.eval_limits.recursion_limit());
        server.issue_diagnostics(file_id, diags);
    }
    Ok(())
}
