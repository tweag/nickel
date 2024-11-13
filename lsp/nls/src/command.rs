use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{ExecuteCommandParams, TextDocumentIdentifier, Url};
use nickel_lang_core::eval::{cache::CacheImpl, VirtualMachine};

use crate::{
    cache::CacheExt,
    diagnostic::SerializableDiagnostic,
    error::{Error, WarningReporter},
    server::Server,
};

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
    if let Some(file_id) = server.world.cache.file_id(uri)? {
        let (reporter, warnings) = WarningReporter::new();
        // TODO: avoid cloning the cache. Maybe we can have a VM with a &mut Cache?
        let mut vm = VirtualMachine::<_, CacheImpl>::new(
            server.world.cache.clone(),
            std::io::stderr(),
            reporter,
        );
        let rt = vm.prepare_eval(file_id)?;

        let result = vm.eval_full(rt);
        // Get a possibly-updated files from the vm instead of relying on the one
        // in `world`.
        let mut files = vm.import_resolver().files().clone();
        drop(vm);

        let mut diags: Vec<_> = warnings
            .try_iter()
            .flat_map(|(warning, mut files)| {
                SerializableDiagnostic::from(warning, &mut files, file_id)
            })
            .collect();

        if let Err(e) = result {
            let mut error_diags = SerializableDiagnostic::from(e, &mut files, file_id);
            diags.append(&mut error_diags);
        }

        server.issue_diagnostics(file_id, diags);
    }
    Ok(())
}
