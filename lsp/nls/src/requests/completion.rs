use codespan::ByteIndex;
use codespan_lsp::position_to_byte_index;
use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{CompletionItem, CompletionParams};
use nickel_lang::{
    identifier::Ident,
    term::{Contract, MetaValue},
    types::{AbsType, RowIteratorItem, Types},
};
use serde_json::Value;

use crate::{
    linearization::{
        interface::{TermKind, UsageState, ValueState},
        LinearizationItem,
    },
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

    let text = server.cache.files().source(file_id);
    let start = position_to_byte_index(
        server.cache.files(),
        file_id,
        &params.text_document_position.position,
    )
    .unwrap();

    let locator = (file_id, ByteIndex((start - 1) as u32));
    let linearization = server.lin_cache_get(&file_id)?;
    // lin_env is a mapping from idents to IDs.
    // This is from the Linearizer used to build this linearization.
    // idealy we should get the id and use it instead of comparing strings labels
    let lin_env = &linearization.lin_env;

    // Try your best to find all record fields
    fn find_record_fields(
        linearization: &Vec<LinearizationItem<Types>>,
        id: usize,
    ) -> Option<Vec<Ident>> {
        linearization.iter().find_map(|item| match item.kind {
            TermKind::Record(ref fields) if item.id == id => {
                Some(fields.keys().map(|ident| ident.clone()).collect())
            }
            TermKind::Declaration(_, _, ValueState::Known(new_id))
            | TermKind::Usage(UsageState::Resolved(new_id))
                if item.id == id =>
            {
                find_record_fields(linearization, new_id)
            }
            _ => None,
        })
    }

    fn find_record_contract(
        linearization: &Vec<LinearizationItem<Types>>,
        id: usize,
    ) -> Option<Contract> {
        linearization.iter().find_map(|item| match &item.meta {
            Some(MetaValue { contracts, .. }) if item.id == id => {
                contracts.iter().find_map(|contract| {
                    if let Types(AbsType::Record(..)) = contract.types {
                        Some(contract.clone())
                    } else {
                        None
                    }
                })
            }
            _ if item.id == id => match item.kind {
                TermKind::Declaration(_, _, ValueState::Known(new_id))
                | TermKind::Usage(UsageState::Resolved(new_id)) => {
                    find_record_contract(&linearization, new_id)
                }
                _ => None,
            },
            _ => None,
        })
    }

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

    let item = linearization.item_at(&locator);
    if item == None {
        // NOTE: this might result in a case where completion is not working.
        // This happens when an item cannot be found from the linearization.
        server.reply(Response::new_ok(id, Value::Null));
        return Ok(());
    }
    let item = item.unwrap().to_owned();

    Trace::enrich(&id, linearization);

    let trigger = params.context.and_then(|context| context.trigger_character);
    let in_scope = match trigger {
        // Record Completion
        Some(s) if s == "." => {
            // Get the ID before the .
            // if the current source is `let var = {a = 10} in var.`
            // name = "var"
            let name = text[..start - 1]
                .chars()
                .rev()
                .skip_while(|c| *c == '.') // skip . (if any)
                .take_while(|c| c.is_ascii_alphabetic())
                .collect::<String>()
                .chars()
                .rev()
                .collect::<String>();

            let name_id = lin_env
                .iter()
                .find(|(ident, _)| ident.label() == name)
                .map(|(_, id)| *id);
            if name_id == None {
                // NOTE: this might result in a case where completion is not working.
                // This happens when an item cannot be found from the linearization.
                server.reply(Response::new_ok(id, Value::Null));
                return Ok(());
            }
            let name_id = name_id.unwrap();

            linearization
                .get_in_scope(&item)
                .iter()
                .filter_map(|i| {
                    let (ty, _) = linearization.resolve_item_type_meta(i);
                    match i.kind {
                        // Get record fields from static type info
                        TermKind::Declaration(..)
                            if name_id == i.id && matches!(ty, Types(AbsType::Record(..))) =>
                        {
                            match &ty {
                                Types(AbsType::Record(row)) => {
                                    Some((extract_ident(row), i.ty.clone()))
                                }
                                _ => unreachable!(),
                            }
                        }
                        // Get record fields from contract metadata
                        TermKind::Declaration(_, _, ValueState::Known(body_id))
                            if name_id == i.id
                                && find_record_contract(&linearization.linearization, body_id)
                                    .is_some() =>
                        {
                            // unwrapping is safe here because of the pattern condition.
                            let contract =
                                find_record_contract(&linearization.linearization, body_id)
                                    .unwrap();
                            let fields = match &contract.types {
                                Types(AbsType::Record(row)) => extract_ident(row),
                                _ => Vec::with_capacity(0),
                            };
                            Some((fields, i.ty.clone()))
                        }

                        // Get record fields from lexical scoping
                        TermKind::Declaration(_, _, ValueState::Known(body_id))
                            if name_id == i.id =>
                        {
                            find_record_fields(&linearization.linearization, body_id)
                                .map(|idents| Some((idents, i.ty.clone())))
                                .unwrap_or(None)
                        }

                        _ => None,
                    }
                })
                .collect()
        }

        // variable name completion
        Some(..) | None => linearization
            .get_in_scope(&item)
            .iter()
            .filter_map(|i| match i.kind {
                TermKind::Declaration(ref ident, _, _) => Some((vec![ident.clone()], i.ty.clone())),
                _ => None,
            })
            .collect::<Vec<_>>(),
    };

    // O(n^2)
    fn remove_duplicates(items: &Vec<CompletionItem>) -> Vec<CompletionItem> {
        let mut seen: Vec<CompletionItem> = Vec::with_capacity(0);
        for item in items {
            if seen
                .iter()
                .find(|seen_item| seen_item.label == item.label)
                .is_none()
            {
                seen.push(item.clone())
            }
        }

        seen
    }

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
    let in_scope = remove_duplicates(&in_scope);

    server.reply(Response::new_ok(id, in_scope));

    debug!("found closest item: {:?}", item);

    Ok(())
}
