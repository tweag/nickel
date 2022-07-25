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
        .id_of(params.text_document.uri.to_string())
        .unwrap();

    if let Some(completed) = server.lin_cache.get(&file_id) {
        Trace::enrich(&id, completed);
        let symbols = completed
            .linearization
            .iter()
            .filter_map(|item| match &item.kind {
                TermKind::Declaration(name, _, _) => {
                    // TODO: can `unwrap` fail here?
                    let (file_id, span) = item.pos.unwrap().to_range();

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
