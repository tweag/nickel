use codespan::ByteIndex;
use codespan_lsp::position_to_byte_index;
use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{Hover, HoverContents, HoverParams, LanguageString, MarkedString, Range};
use nickel_lang::position::TermPos;
use serde_json::Value;

use crate::{
    diagnostic::LocationCompat,
    server::Server,
    trace::{Enrich, Trace},
};

pub fn handle(
    params: HoverParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let file_id = server
        .cache
        .id_of(
            params
                .text_document_position_params
                .text_document
                .uri
                .as_str(),
        )
        .unwrap();

    let start = position_to_byte_index(
        server.cache.files(),
        file_id,
        &params.text_document_position_params.position,
    )
    .unwrap();

    debug!("start of hovered item: ByteIndex({})", start);

    let locator = (file_id, ByteIndex(start as u32));

    let linearization = server.lin_cache_get(&file_id)?;
    let item = linearization.item_at(&locator);

    Trace::enrich(&id, linearization);

    if item.is_none() {
        server.reply(Response::new_ok(id, Value::Null));
        return Ok(());
    }

    let item = item.unwrap().to_owned();

    debug!("{:?}", item);

    let (ty, meta) = linearization.resolve_item_type_meta(&item);

    if item.pos == TermPos::None {
        server.reply(Response::new_ok(id, Value::Null));
        return Ok(());
    }
    let pos = item.pos.unwrap();
    let range = Range::from_codespan(
        &file_id,
        &(pos.start.to_usize()..pos.end.to_usize()),
        server.cache.files(),
    );

    server.reply(Response::new_ok(
        id,
        Hover {
            contents: HoverContents::Array(vec![
                MarkedString::LanguageString(LanguageString {
                    language: "nickel".into(),
                    value: ty.to_string(),
                }),
                MarkedString::LanguageString(LanguageString {
                    language: "plain".into(),
                    value: meta.join("\n"),
                }),
            ]),

            range: Some(range),
        },
    ));
    Ok(())
}
