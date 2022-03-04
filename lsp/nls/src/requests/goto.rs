use codespan::ByteIndex;
use codespan_lsp::position_to_byte_index;
use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse, Location, Range, ReferenceParams, Url,
};
use nickel_lang::position::RawSpan;
use serde_json::Value;

use crate::{
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

    let locator = (file_id, ByteIndex(start as u32));
    let linearization = server.lin_cache_get(&file_id)?;

    Trace::enrich(&id, linearization);

    let item = linearization.item_at(&locator);

    if item == None {
        server.reply(Response::new_ok(id, Value::Null));
        return Ok(());
    }

    let item = item.unwrap();

    debug!("found referencing item: {:?}", item);

    let location = match item.kind {
        TermKind::Usage(UsageState::Resolved(usage_id)) => {
            let definition = linearization.get_item(usage_id).unwrap();
            let location = match definition.pos {
                RawSpan {
                    start: ByteIndex(start),
                    end: ByteIndex(end),
                    src_id,
                } => Location {
                    uri: Url::parse(&server.cache.name(src_id).to_string_lossy()).unwrap(),
                    range: Range::from_codespan(
                        &src_id,
                        &(start as usize..end as usize),
                        server.cache.files(),
                    ),
                },
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
    let linearization = server.lin_cache_get(&file_id)?;

    let item = linearization.item_at(&locator);

    if item == None {
        server.reply(Response::new_ok(id, Value::Null));
        return Ok(());
    }

    let item = item.unwrap();

    debug!("found referencing item: {:?}", item);

    let locations = match &item.kind {
        TermKind::Declaration(_, usages, _) | TermKind::RecordField { usages, .. } => {
            let mut locations = Vec::new();

            for reference_id in usages.iter() {
                let reference = linearization.get_item(*reference_id).unwrap();
                let location = match reference.pos {
                    RawSpan {
                        start: ByteIndex(start),
                        end: ByteIndex(end),
                        src_id,
                    } => Location {
                        uri: Url::parse(&server.cache.name(src_id).to_string_lossy()).unwrap(),
                        range: Range::from_codespan(
                            &src_id,
                            &(start as usize..end as usize),
                            server.cache.files(),
                        ),
                    },
                };
                locations.push(location);
            }
            Some(locations)
        }
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
