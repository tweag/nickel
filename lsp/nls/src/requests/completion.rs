use codespan::ByteIndex;
use codespan_lsp::position_to_byte_index;
use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{CompletionItem, CompletionParams};
use nickel_lang::{
    identifier::Ident,
    term::{Contract, MetaValue},
    types::{AbsType, Types},
};
use serde_json::Value;

use crate::{
    linearization::interface::TermKind,
    server::Server,
    trace::{Enrich, Trace},
};

pub fn handle_completion(
    params: CompletionParams,
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

    Trace::enrich(&id, linearization);

    let item = linearization.item_at(&locator);

    if item == None {
        server.reply(Response::new_ok(id, Value::Null));
        return Ok(());
    }

    let item = item.unwrap().to_owned();

    // How can we get data for record completion based on the TermKind
    // 1. Record: just list it's fields
    // 2. RecordRef: list the field it contains if the item we're at corresponds with it's ID
    // 3. Check the type of the term: If it's a record, list it's fields
    // 4. Check the MetaValue for it's contract: if it's a record, list it's fields

    // Can we seperate record completion from just normal completion?

    /// Extract identifiers from a row type which forms a record.
    /// Ensure that ty is really a row type.
    fn extract_ident(ty: &Box<Types>) -> Vec<Ident> {
        let mut fields = Vec::new();
        let mut item = &**ty;
        loop {
            match item {
                Types(AbsType::RowExtend(ident, _, ref next)) => {
                    fields.push(ident.clone());
                    item = &**next;
                }
                Types(AbsType::RowEmpty()) => break,
                _ => unreachable!(),
            }
        }
        fields
    }

    let in_scope: Vec<_> = linearization
        .get_in_scope(&item)
        .iter()
        .filter_map(|i| match i.kind {
            TermKind::Declaration(ref ident, _, _) => Some((vec![ident.clone()], i.ty.clone())),
            TermKind::RecordRef { ref fields, .. } if i.id == item.id => {
                Some((fields.clone(), i.ty.clone()))
            }
            TermKind::Record(ref fields) => {
                Some((fields.keys().map(|i| i.clone()).collect(), i.ty.clone()))
            }
            _ if matches!(i.ty, Types(AbsType::StaticRecord(..))) => match &i.ty {
                Types(AbsType::StaticRecord(row)) => Some((extract_ident(row), i.ty.clone())),
                _ => unreachable!(),
            },

            _ if i.meta.is_some() => match &i.meta {
                Some(MetaValue { contracts, .. }) => {
                    let fields = contracts
                        .clone()
                        .iter()
                        .map(|Contract { types, .. }| match types {
                            Types(AbsType::StaticRecord(row)) => extract_ident(row),
                            _ => vec![],
                        })
                        .flatten()
                        .collect();
                    Some((fields, i.ty.clone()))
                }
                _ => unreachable!(),
            },
            _ => None,
        })
        .map(|(idents, _)| {
            idents
                .iter()
                .map(|ident| CompletionItem {
                    label: ident.into_label(),
                    ..Default::default()
                })
                .collect::<Vec<_>>()
        })
        .flatten()
        .collect();

    server.reply(Response::new_ok(id, in_scope));

    debug!("found closest item: {:?}", item);
    Ok(())
}
