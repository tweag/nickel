use crate::{
    linearization::interface::TermKind,
    term::RawSpanExt,
    trace::{Enrich, Trace},
};
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
        .id_of(params.text_document.uri.to_file_path().unwrap())
        .unwrap();

    if let Some(completed) = server.lin_registry.map.get(&file_id) {
        Trace::enrich(&id, completed);
        let symbols = completed
            .linearization
            .iter()
            .filter_map(|item| match &item.kind {
                TermKind::Declaration { id: name, .. } => {
                    // Although the position maybe shouldn't be `None`, opening the std library
                    // source inside VSCode made the LSP panic on the previous `item.pos.unwrap()`.
                    // Before investigating further, let's not make the VSCode extension panic in
                    // the meantime.
                    let (file_id, span) = item.pos.into_opt()?.to_range();

                    let range =
                        codespan_lsp::byte_span_to_range(server.cache.files(), file_id, span)
                            .unwrap();

                    // `deprecated` is a required field but causes a warning although we are not using it
                    #[allow(deprecated)]
                    Some(DocumentSymbol {
                        name: name.to_string(),
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
