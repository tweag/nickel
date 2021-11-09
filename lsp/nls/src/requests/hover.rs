use codespan::ByteIndex;
use codespan_lsp::position_to_byte_index;
use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{Hover, HoverContents, HoverParams, LanguageString, MarkedString, Range};
use nickel::{position::TermPos, term::MetaValue, typecheck::linearization};
use serde_json::Value;

use crate::{
    diagnostic::LocationCompat, requests::utils::find_linearization_index, server::Server,
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

    let completed = server.lin_cache_get(&file_id)?;
    let linearization = &completed.lin;
    let index = find_linearization_index(linearization, locator);

    if index == None {
        server.reply(Response::new_ok(id, Value::Null));
        return Ok(());
    }

    let item = linearization[index.unwrap()].to_owned();
    debug!("{:?}", item);

    let mut extra = vec![];

    if let Some(MetaValue {
        ref doc,
        ref types,
        ref contracts,
        priority,
        ..
    }) = item.meta.as_ref().or_else(|| match item.kind {
        linearization::TermKind::Usage(usage) => usage
            .and_then(|u| completed.get_item(u))
            .and_then(|i| i.meta.as_ref()),
        _ => None,
    }) {
        if let Some(doc) = doc {
            extra.push(doc.to_owned());
        }
        if let Some(types) = types {
            extra.push(format!("{}", types.label.tag));
        }
        if !contracts.is_empty() {
            extra.push(
                contracts
                    .iter()
                    .map(|contract| format!("{}", contract.label.types,))
                    .collect::<Vec<_>>()
                    .join(","),
            );
        }

        extra.push(format!("Merge Priority: {:?}", priority));
    }

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
                    value: extra.join("\n"),
                }),
                MarkedString::LanguageString(LanguageString {
                    language: "plain".into(),
                    value: format!("{:?} @ {:?}", item, range,),
                }),
            ]),

            range,
        },
    ));
    Ok(())
}
