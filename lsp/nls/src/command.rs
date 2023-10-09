use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{ExecuteCommandParams, TextDocumentIdentifier, Url};
use nickel_lang_core::{
    error::IntoDiagnostics,
    eval::{cache::CacheImpl, VirtualMachine},
};

use crate::{cache::CacheExt, error::Error, server::Server};

pub fn handle_command(
    params: ExecuteCommandParams,
    req: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    match params.command.as_str() {
        "eval" => {
            server.reply(Response::new_ok(req, None::<()>));

            let doc: TextDocumentIdentifier =
                serde_json::from_value(params.arguments[0].clone()).unwrap();
            eval(server, &doc.uri)?;
            Ok(())
        }
        _ => Err(Error::CommandNotFound(params.command).into()),
    }
}

fn eval(server: &mut Server, uri: &Url) -> Result<(), Error> {
    if let Some(file_id) = server.cache.file_id(uri)? {
        // TODO: avoid cloning the cache. Maybe we can have a VM with a &mut ImportResolver?
        let mut vm = VirtualMachine::<_, CacheImpl>::new(server.cache.clone(), std::io::stderr());
        let rt = vm.prepare_eval(file_id)?;
        if let Err(e) = vm.eval_full(rt) {
            let diags = e.into_diagnostics(server.cache.files_mut(), None);
            server.issue_diagnostics(file_id, diags);
        }
    }
    Ok(())
}
