use codespan::ByteIndex;
use codespan_lsp::position_to_byte_index;
// use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{CompletionContext, CompletionItem, CompletionParams};
use nickel_lang::{
    identifier::Ident,
    term::{Contract, MetaValue},
    types::{AbsType, RowIteratorItem, Types},
};
use serde_json::Value;

use crate::{
    linearization::{completed::Completed, interface::TermKind},
    server::Server,
    trace::{Enrich, Trace},
};

pub fn handle_completion(
    params: CompletionParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    // let mut file = OpenOptions::new()
    //     .write(true)
    //     .append(false)
    //     .create(true)
    //     .open("/home/ebresafegaga/server.log")
    //     .unwrap();

    let file_id = server
        .cache
        .id_of(params.text_document_position.text_document.uri.as_str())
        .unwrap();

    let text = server.cache.files().source(file_id);
    let start = position_to_byte_index(
        server.cache.files(),
        file_id,
        &params.text_document_position.position,
    )
    .unwrap();

    let name = text[..start - 1]
        .chars()
        .rev()
        .take_while(|c| c.is_ascii_alphabetic() || *c == '.')
        .skip_while(|c| *c == '.') // skip . (if any)
        .collect::<String>()
        .chars()
        .rev()
        .collect::<String>();

    // Make sure you parse the string to support:
    // 1. Normal Identifiers
    // 2. Record Index
    // 3. Record terms - Maybe this can be gotten from the Linearizer

    // This is the environment containing stdlib stuff.
    // let env = &server.initial_ctxt.type_env;

    // lin_env is a mapping from idents to IDs.
    // This is from the Linearizer used to build this linearization.
    // idealy we should get the id and use it instead of comparing strings
    let Completed { lin_env: _, .. } = &server.lin_cache.get(&file_id).unwrap();

    let locator = (file_id, ByteIndex(start as u32));
    let linearization = server.lin_cache_get(&file_id)?;

    Trace::enrich(&id, linearization);

    let trigger = match params.context {
        Some(CompletionContext {
            trigger_character: Some(s),
            ..
        }) => Some(s),
        _ => None,
    };

    /// Extract identifiers from a row type which forms a record.
    /// Ensure that ty is really a row type.
    fn extract_ident(ty: &Box<Types>) -> Vec<Ident> {
        ty.iter_as_rows()
            .filter_map(|item| match item {
                RowIteratorItem::Row(ident, _) => Some(ident.clone()),
                RowIteratorItem::Tail(..) => None,
            })
            .collect::<Vec<_>>()
    }

    let in_scope = match trigger {
        // Record completion
        Some(s) if s == "." => linearization
            .linearization
            .iter()
            .filter_map(|i| match i.kind {
                TermKind::RecordBind {
                    ident, ref fields, ..
                } if ident.label() == name => Some((fields.clone(), i.ty.clone())),

                // Get static type info
                TermKind::Declaration(ident, ..)
                    if ident.label() == name
                        && matches!(i.ty, Types(AbsType::StaticRecord(..))) =>
                {
                    match &i.ty {
                        Types(AbsType::StaticRecord(row)) => {
                            Some((extract_ident(row), i.ty.clone()))
                        }
                        _ => unreachable!(),
                    }
                }

                // Get contract info
                TermKind::Declaration(ident, ..) if ident.label() == name && i.meta.is_some() => {
                    match &i.meta {
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
                    }
                }
                _ => None,
            })
            .collect(),

        // variable name completion
        Some(..) | None => {
            let item = linearization.item_at(&locator);
            if item == None {
                server.reply(Response::new_ok(id, Value::Null));
                return Ok(());
            } else {
                let item = item.unwrap().to_owned();
                linearization
                    .get_in_scope(&item)
                    .iter()
                    .filter_map(|i| match i.kind {
                        TermKind::Declaration(ref ident, _, _) => {
                            Some((vec![ident.clone()], i.ty.clone()))
                        }
                        _ => None,
                    })
                    .collect::<Vec<_>>()
            }
        }
    };

    // How can we get data for record completion based on the TermKind
    // 1. Record: just list it's fields
    // 2. RecordRef: list the field it contains if the item we're at corresponds with it's ID
    // 3. Check the type of the term: If it's a record, list it's fields
    // 4. Check the MetaValue for it's contract: if it's a record, list it's fields

    // Can we seperate record completion from just normal completion?

    // We also need data form the standard library functions

    let in_scope: Vec<_> = in_scope
        .iter()
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

    // debug!("found closest item: {:?}", item);
    Ok(())
}
