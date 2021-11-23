use crate::term::TermPosExt;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{DocumentSymbol, DocumentSymbolParams, SymbolKind};
use serde_json::Value;

use crate::server::Server;

pub fn handle_document_symbols(
    params: DocumentSymbolParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let file_id = server
        .cache
        .id_of(params.text_document.uri.to_string())
        .unwrap();

    if let Some(completed) = server.lin_cache.get(&file_id) {
        let symbols = completed
            .lin
            .iter()
            .filter_map(|item| match &item.kind {
                nickel::typecheck::linearization::TermKind::Declaration(name, _) => {
                    let range = item
                        .pos
                        .try_to_range()
                        .or_else(|| Some((file_id.clone(), (0usize..0usize))))
                        .map(|(file_id, range)| {
                            codespan_lsp::byte_span_to_range(server.cache.files(), file_id, range)
                                .unwrap()
                        })
                        .unwrap();
                    Some(DocumentSymbol {
                        name: name.to_owned(),
                        detail: Some(format!("{}", item.ty)),
                        kind: SymbolKind::Variable,
                        tags: None,
                        range,
                        selection_range: range,
                        children: None,
                        deprecated: None,
                    })
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        server.reply(Response::new_ok(id, symbols));
    } else {
        server.reply(Response::new_ok(id, Value::Null));
    }

    Ok(())
}
