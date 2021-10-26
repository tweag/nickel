use codespan::ByteIndex;
use codespan_lsp::position_to_byte_index;
use log::debug;
use lsp_server::{ErrorCode, Request, RequestId, Response};
use lsp_types::{
    Hover, HoverContents, HoverParams, LanguageString, MarkedString, MarkupContent, Range,
};
use nickel::{
    position::TermPos,
    typecheck::linearization::{self, Linearization},
};
use serde_json::Value;

use crate::{
    diagnostic::LocationCompat,
    requests::utils::find_linearizaion_index,
    server::{positon_to_byte_index, Server},
};

pub fn handle(params: HoverParams, id: RequestId, server: &mut Server) {
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
    let linearization = &server.lin_cache.get(&file_id).unwrap().lin;

    let index = find_linearizaion_index(linearization, locator);

    if index == None {
        server.reply(Response::new_ok(id, Value::Null));
        return;
    }

    let item = linearization[index.unwrap()].to_owned();
    debug!("{:?}", item);

    let range = match item.pos {
        TermPos::Original(span) | TermPos::Inherited(span) => Some(Range::from_codespan(
            &file_id,
            &(span.start.0 as usize..span.end.0 as usize),
            server.cache.files(),
        )),
        TermPos::None => None,
    };
    server.reply(Response::new_ok(
        id,
        Hover {
            contents: HoverContents::Array(vec![
                MarkedString::LanguageString(LanguageString {
                    language: "nickel".into(),
                    value: item.ty.to_string(),
                }),
                MarkedString::LanguageString(LanguageString {
                    language: "plain".into(),
                    value: format!("{:?} @ {:?}", item, range,),
                }),
            ]),

            range,
        },
    ))
}
