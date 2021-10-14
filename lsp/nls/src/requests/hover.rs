use codespan::ByteIndex;
use log::debug;
use lsp_server::{ErrorCode, Request, RequestId, Response};
use lsp_types::{Hover, HoverContents, HoverParams, MarkedString, MarkupContent, Range};
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

    let start = positon_to_byte_index(
        params.text_document_position_params.position,
        file_id,
        server.cache.files_mut(),
    );

    let locator = (file_id, start);
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
            server.cache.files_mut(),
        )),
        TermPos::None => None,
    };
    server.reply(Response::new_ok(
        id,
        Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: format!(
                    "{}
{:?} @ {:?}
                    ",
                    item.ty, item, range
                ),
            }),

            range,
        },
    ))
}
