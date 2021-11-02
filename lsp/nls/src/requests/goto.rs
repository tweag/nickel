use codespan::ByteIndex;
use log::debug;
use lsp_server::{ErrorCode, Request, RequestId, Response};
use lsp_types::{
    request::GotoDeclarationResponse, GotoDefinitionParams, GotoDefinitionResponse, Hover,
    HoverContents, HoverParams, Location, MarkedString, MarkupContent, Range, ReferenceParams, Url,
};
use nickel::{
    position::{RawSpan, TermPos},
    typecheck::linearization::{self, Linearization},
};
use serde_json::Value;

use crate::{
    diagnostic::LocationCompat,
    requests::utils::find_linearizaion_index,
    server::{positon_to_byte_index, Server},
};

pub fn handle_to_definition(params: GotoDefinitionParams, id: RequestId, server: &mut Server) {
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

    let start = positon_to_byte_index(
        params.text_document_position_params.position,
        file_id,
        server.cache.files_mut(),
    );

    let locator = (file_id, start);
    let linearization = &server.lin_cache.get(&file_id).unwrap();

    let index = find_linearizaion_index(&linearization.lin, locator);

    if index == None {
        server.reply(Response::new_ok(id, Value::Null));
        return;
    }

    let item = linearization.lin[index.unwrap()].to_owned();

    debug!("found referencing item: {:?}", item);

    let location = match item.kind {
        linearization::TermKind::Usage(Some(usage_id)) => {
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
                    uri: Url::parse(server.cache.name(src_id).to_str().unwrap()).unwrap(),
                    range: Range::from_codespan(
                        &src_id,
                        &(start as usize..end as usize),
                        server.cache.files_mut(),
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
}

pub fn handle_to_usages(params: ReferenceParams, id: RequestId, server: &mut Server) {
    let file_id = server
        .cache
        .id_of(params.text_document_position.text_document.uri.as_str())
        .unwrap();

    let start = positon_to_byte_index(
        params.text_document_position.position,
        file_id,
        server.cache.files_mut(),
    );

    let locator = (file_id, start);
    let linearization = server.lin_cache.get(&file_id).unwrap();

    let index = find_linearizaion_index(&linearization.lin, locator);

    if index == None {
        server.reply(Response::new_ok(id, Value::Null));
        return;
    }

    let item = linearization.lin[index.unwrap()].to_owned();

    debug!("found referencing item: {:?}", item);

    let locations = match item.kind {
        linearization::TermKind::Declaration(_, references) => {
            let mut locations = Vec::new();

            for reference_id in references.iter() {
                let reference = linearization.id_mapping[&reference_id];
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
                        uri: Url::parse(server.cache.name(src_id).to_str().unwrap()).unwrap(),
                        range: Range::from_codespan(
                            &src_id,
                            &(start as usize..end as usize),
                            server.cache.files_mut(),
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
}
