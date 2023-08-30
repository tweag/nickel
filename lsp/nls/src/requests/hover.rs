use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{Hover, HoverContents, HoverParams, LanguageString, MarkedString, Range};
use nickel_lang_core::position::TermPos;
use serde_json::Value;

use crate::{
    cache::CacheExt,
    diagnostic::LocationCompat,
    server::Server,
    trace::{Enrich, Trace},
};

pub fn handle(
    params: HoverParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let pos = server
        .cache
        .position(&params.text_document_position_params)?;

    debug!("pos of hovered item: {pos:?}");

    let linearization = server.lin_cache_get(&pos.src_id)?;
    let item = linearization.item_at(pos);

    Trace::enrich(&id, linearization);

    if item.is_none() {
        server.reply(Response::new_ok(id, Value::Null));
        return Ok(());
    }

    let item = item.unwrap().to_owned();

    debug!("Found item {:?}", item);

    let (ty, meta) = linearization.get_type_and_metadata(&item, &server.lin_registry);

    if item.pos == TermPos::None {
        server.reply(Response::new_ok(id, Value::Null));
        return Ok(());
    }
    let pos = item.pos.unwrap();
    let range = Range::from_codespan(
        &pos.src_id,
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
                MarkedString::String(
                    meta.iter()
                        .flat_map(|s| s.lines())
                        .map(|s| s.trim())
                        .collect::<Vec<_>>()
                        .join("\n"),
                ),
            ]),

            range: Some(range),
        },
    ));
    Ok(())
}
