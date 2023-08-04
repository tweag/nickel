use codespan::ByteIndex;
use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse, Location, Range, ReferenceParams, Url,
};
use nickel_lang_core::position::RawSpan;
use serde_json::Value;

use crate::{
    cache::CacheExt,
    diagnostic::LocationCompat,
    linearization::interface::{TermKind, UsageState},
    server::Server,
    trace::{Enrich, Trace},
};

pub fn handle_to_definition(
    params: GotoDefinitionParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let pos = server
        .cache
        .position(&params.text_document_position_params)?;
    let linearization = server.lin_cache_get(&pos.src_id)?;

    Trace::enrich(&id, linearization);

    let Some(item) = linearization.item_at(pos) else {
        server.reply(Response::new_ok(id, Value::Null));
        return Ok(())
    };

    debug!("found referencing item: {:?}", item);

    let location = match item.kind {
        TermKind::Usage(UsageState::Resolved(usage_id)) => {
            let definition = linearization
                .get_item_with_reg(usage_id, &server.lin_registry)
                .unwrap();
            if server.cache.is_stdlib_module(definition.id.file_id) {
                // The standard library files are embedded in the executable,
                // so we can't possibly go to their definition on disk.
                server.reply(Response::new_ok(id, Value::Null));
                return Ok(());
            }
            let RawSpan {
                start: ByteIndex(start),
                end: ByteIndex(end),
                src_id,
            } = definition.pos.unwrap();
            let location = Location {
                uri: Url::from_file_path(server.cache.name(src_id)).unwrap(),
                range: Range::from_codespan(
                    &src_id,
                    &(start as usize..end as usize),
                    server.cache.files(),
                ),
            };
            Some(location)
        }
        _ => None,
    };

    debug!("referenced location: {:?}", location);

    if let Some(response) = location.map(GotoDefinitionResponse::Scalar) {
        server.reply(Response::new_ok(id, response));
    } else {
        server.reply(Response::new_ok(id, Value::Null));
    }
    Ok(())
}

pub fn handle_to_usages(
    params: ReferenceParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let pos = server.cache.position(&params.text_document_position)?;
    let linearization = server.lin_cache_get(&pos.src_id)?;

    let Some(item) = linearization.item_at(pos) else {
        server.reply(Response::new_ok(id, Value::Null));
        return Ok(());
    };

    debug!("found referencing item: {:?}", item);

    let locations: Option<Vec<Location>> = match &item.kind {
        TermKind::Declaration { usages, .. } | TermKind::RecordField { usages, .. } => Some(
            usages
                .iter()
                .filter_map(|reference_id| {
                    linearization
                        .get_item_with_reg(*reference_id, &server.lin_registry)
                        .unwrap()
                        .pos
                        .as_opt_ref()
                        .map(|RawSpan { start, end, src_id }| Location {
                            uri: Url::from_file_path(server.cache.name(*src_id)).unwrap(),
                            range: Range::from_codespan(
                                src_id,
                                &((*start).into()..(*end).into()),
                                server.cache.files(),
                            ),
                        })
                })
                .collect(),
        ),
        _ => None,
    };

    debug!("referencing locations: {:?}", locations);

    if let Some(response) = locations {
        server.reply(Response::new_ok(id, response));
    } else {
        server.reply(Response::new_ok(id, Value::Null));
    }
    Ok(())
}
