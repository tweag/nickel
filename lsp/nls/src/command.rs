use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{ExecuteCommandParams, TextDocumentIdentifier, Url};

use nickel_lang_core::{
    cache::{Caches, ErrorTolerance},
    eval::{cache::CacheImpl, VirtualMachine},
};

use crate::{
    diagnostic::SerializableDiagnostic,
    error::{Error, WarningReporter},
    server::Server,
};

const RECURSION_LIMIT: usize = 128;

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
    if let Some(file_id) = server.world.cache.file_id(uri)? {
        let diags = server.world.eval_diagnostics(file_id, RECURSION_LIMIT);
        server.issue_diagnostics(file_id, diags);
    }
    Ok(())
}
