use codespan::ByteIndex;
use codespan_lsp::position_to_byte_index;
use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse, Location, Range, ReferenceParams, Url,
};
use nickel::{
    position::{RawSpan, TermPos},
    typecheck::linearization::{self, UsageState},
};
use serde_json::Value;

use crate::{
    diagnostic::LocationCompat,
    requests::utils::find_linearization_index,
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

    let index = find_linearization_index(&linearization.lin, locator);

    if index == None {
        server.reply(Response::new_ok(id, Value::Null));
        return Ok(());
    }

    let item = linearization.lin[index.unwrap()].to_owned();

    debug!("found referencing item: {:?}", item);

    let location = match item.kind {
        linearization::TermKind::Usage(UsageState::Resolved(Some(usage_id))) => {
            let definition = linearization.id_mapping[&usage_id];
            let definition = linearization.lin[definition].clone();
            let location = match definition.pos {
                TermPos::Original(RawSpan {
                    start: ByteIndex(start),
                    end: ByteIndex(end),
                    src_id,
                })
                | TermPos::Inherited(RawSpan {
                    start: ByteIndex(start),
                    end: ByteIndex(end),
                    src_id,
                }) => Some(Location {
                    uri: Url::parse(&server.cache.name(src_id).to_string_lossy()).unwrap(),
                    range: Range::from_codespan(
                        &src_id,
                        &(start as usize..end as usize),
                        server.cache.files(),
                    ),
                }),
                TermPos::None => None,
            };
            location
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

    let index = find_linearization_index(&linearization.lin, locator);

    if index == None {
        server.reply(Response::new_ok(id, Value::Null));
        return Ok(());
    }

    let item = linearization.lin[index.unwrap()].to_owned();

    debug!("found referencing item: {:?}", item);

    let locations = match item.kind {
        linearization::TermKind::Declaration(_, usages)
        | linearization::TermKind::RecordField { usages, .. } => {
            let mut locations = Vec::new();

            for reference_id in usages.iter() {
                let reference = linearization.id_mapping[reference_id];
                let reference = linearization.lin[reference].clone();
                let location = match reference.pos {
                    TermPos::Original(RawSpan {
                        start: ByteIndex(start),
                        end: ByteIndex(end),
                        src_id,
                    })
                    | TermPos::Inherited(RawSpan {
                        start: ByteIndex(start),
                        end: ByteIndex(end),
                        src_id,
                    }) => Some(Location {
                        uri: Url::parse(&server.cache.name(src_id).to_string_lossy()).unwrap(),
                        range: Range::from_codespan(
                            &src_id,
                            &(start as usize..end as usize),
                            server.cache.files(),
                        ),
                    }),
                    TermPos::None => None,
                };
                if let Some(location) = location {
                    locations.push(location)
                }
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
