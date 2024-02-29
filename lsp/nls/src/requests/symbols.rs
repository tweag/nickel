use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{DocumentSymbol, DocumentSymbolParams, SymbolKind};
use nickel_lang_core::source::SourcePath;
use nickel_lang_core::typ::Type;

use crate::cache::CacheExt as _;
use crate::server::Server;
use crate::term::RawSpanExt;

pub fn handle_document_symbols(
    params: DocumentSymbolParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let file_id = server
        .world
        .cache
        .file_id(&params.text_document.uri)?
        .ok_or_else(|| crate::error::Error::FileNotFound(params.text_document.uri.clone()))?;

    let usage_lookups = &server.world.file_analysis(file_id)?.usage_lookup;
    let type_lookups = &server.world.file_analysis(file_id)?.type_lookup;

    let mut symbols = usage_lookups
        .symbols()
        .filter_map(|ident| {
            let (file_id, span) = ident.pos.into_opt()?.to_range();
            let range =
                crate::codespan_lsp::byte_span_to_range(&server.world.cache, file_id, span).ok()?;
            let ty = type_lookups.idents.get(&ident);

            #[allow(deprecated)] // because the `deprecated` field is... wait for it... deprecated.
            Some(DocumentSymbol {
                name: ident.ident.to_string(),
                detail: ty.map(Type::to_string),
                kind: SymbolKind::VARIABLE,
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
