use codespan::ByteIndex;
use codespan_lsp::position_to_byte_index;
use log::debug;
use lsp_server::{RequestId, Response};
use lsp_types::{CompletionItem, CompletionParams};
use serde_json::Value;

use crate::{requests::utils::find_linearizaion_index, server::Server};

pub fn handle_completion(params: CompletionParams, id: RequestId, server: &mut Server) {
    let file_id = server
        .cache
        .id_of(params.text_document_position.text_document.uri.as_str())
        .unwrap();

    let start = position_to_byte_index(
        server.cache.files(),
        file_id,
        &params.text_document_position.position,
    )
    .unwrap();

    let locator = (file_id, ByteIndex(start as u32));
    let linearization = server.lin_cache.get(&file_id).unwrap();

    let index = find_linearizaion_index(&linearization.lin, locator);

    if index == None {
        server.reply(Response::new_ok(id, Value::Null));
        return;
    }

    let item = linearization.lin[index.unwrap()].to_owned();
    let in_scope: Vec<_> = linearization
        .get_in_scope(&item)
        .iter()
        .filter_map(|i| match i.kind {
            nickel::typecheck::linearization::TermKind::Declaration(ref ident, _) => {
                Some((ident.clone(), i.ty.clone()))
            }
            _ => None,
        })
        .map(|(ident, _)| CompletionItem {
            label: ident,
            ..Default::default()
        })
        .collect();

    server.reply(Response::new_ok(id, in_scope));

    debug!("found closest item: {:?}", item);
}
