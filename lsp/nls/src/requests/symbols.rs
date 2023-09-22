use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{DocumentSymbol, DocumentSymbolParams, SymbolKind};
use nickel_lang_core::cache::SourcePath;
use nickel_lang_core::typ::Type;

use crate::server::Server;
use crate::{files::uri_to_path, term::RawSpanExt};

pub fn handle_document_symbols(
    params: DocumentSymbolParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let path = uri_to_path(&params.text_document.uri)?;
    let file_id = server
        .cache
        .id_of(&SourcePath::Path(path))
        .ok_or_else(|| crate::error::Error::FileNotFound(params.text_document.uri.clone()))?;

    let usage_lookups = server.lin_registry.usage_lookups.get(&file_id).unwrap();

    let type_lookups = server
        .lin_registry
        .type_lookups
        .get(&file_id)
        .ok_or_else(|| crate::error::Error::FileNotFound(params.text_document.uri.clone()))?;

    let mut symbols = usage_lookups
        .symbols()
        .filter_map(|ident| {
            let (file_id, span) = ident.pos.into_opt()?.to_range();
            let range =
                codespan_lsp::byte_span_to_range(server.cache.files(), file_id, span).ok()?;
            let ty = type_lookups.idents.get(&ident);

            #[allow(deprecated)] // because the `deprecated` field is... wait for it... deprecated.
            Some(DocumentSymbol {
                name: ident.ident.to_string(),
                detail: ty.map(Type::to_string),
                kind: SymbolKind::Variable,
                tags: None,
                range,
                selection_range: range,
                children: None,
                deprecated: None,
            })
        })
        .collect::<Vec<_>>();

    symbols.sort_by_key(|s| s.range.start);

    server.reply(Response::new_ok(id, symbols));

    Ok(())
}
