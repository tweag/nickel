use codespan::ByteIndex;
use codespan_lsp::position_to_byte_index;
use lazy_static::lazy_static;
use log::debug;
use lsp_server::{ErrorCode, RequestId, Response, ResponseError};
use lsp_types::{CompletionItem, CompletionParams};
use nickel_lang::{
    identifier::Ident,
    term::{Contract, MetaValue},
    types::{AbsType, RowIteratorItem, Types},
};
use serde_json::Value;

use crate::{
    linearization::{
        completed::Completed,
        interface::{TermKind, UsageState, ValueState},
        LinearizationItem,
    },
    server::Server,
    trace::{Enrich, Trace},
};

/// Find record fields for an item with the specified id.
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

/// Find a record contract of the item with the specified id.
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

/// Get the identifier before the `.`, for record completion.
fn get_identifier(text: &str) -> Result<String, ResponseError> {
    lazy_static! {
        // unwrap is safe here because we know this is a correct regex.
        static ref RE: regex::Regex = regex::Regex::new(r"[_a-zA-Z0-9-']*[a-zA-Z]_?").unwrap();
    }

    let text: String = text
        .chars()
        .rev()
        .skip_while(|c| *c == '.') // skip . (if any)
        .collect();

    let name = RE.find(&text).ok_or(ResponseError {
        code: ErrorCode::InternalError as i32,
        message: "Couldn't get identifier for record completion".to_owned(),
        data: None,
    })?;

    Ok(name.as_str().chars().rev().collect())
}

fn remove_duplicates(items: &Vec<CompletionItem>) -> Vec<CompletionItem> {
    let mut seen: Vec<CompletionItem> = Vec::new();
    for item in items {
        if !seen.iter().any(|seen_item| seen_item.label == item.label) {
            seen.push(item.clone())
        }
    }
    seen
}

fn get_completion_identifiers(
    source: &str,
    trigger: Option<&str>,
    linearization: &Completed,
    item: &LinearizationItem<Types>
) -> Result<Vec<CompletionItem>, ResponseError> {
    let lin_env = &linearization.lin_env;
    let in_scope = match trigger {
        // Record Completion
        Some(".") => {
            let name = get_identifier(source)?;
            let name_id = lin_env
                .iter()
                .find(|(ident, _)| ident.label() == name)
                .map(|(_, id)| *id);

            match name_id {
                Some(name_id) => linearization
                    .get_in_scope(item)
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
                                    && find_record_contract(
                                        &linearization.linearization,
                                        body_id,
                                    )
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
                    .collect(),
                None => return Ok(Vec::new())
            }
        }

        // variable name completion
        Some(..) | None => linearization
            .get_in_scope(item)
            .iter()
            .filter_map(|i| match i.kind {
                TermKind::Declaration(ref ident, _, _) => Some((vec![ident.clone()], i.ty.clone())),
                _ => None,
            })
            .collect::<Vec<_>>(),
    };
    let in_scope: Vec<_> = in_scope
        .iter()
        .flat_map(|(idents, _)| {
            idents
                .iter()
                .map(|ident| CompletionItem {
                    label: ident.into_label(),
                    ..Default::default()
                })
                .collect::<Vec<_>>()
        })
        .collect();
    let in_scope = remove_duplicates(&in_scope);
    Ok(in_scope)
}

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

    // -1 because at the current cursor position, there is no linearization item there,
    // so we try to get the item just before the cursor.
    let locator = (file_id, ByteIndex((start - 1) as u32));
    let linearization = server.lin_cache_get(&file_id)?;
    // Cloning to avoid mutable borrow and shared borrow at the same time,
    // which won't compile
    let linearization = linearization.clone();

    let item = linearization.item_at(&locator);
    Trace::enrich(&id, &linearization);

    match item {
        Some(item) => {
            let trigger = params.context.as_ref().and_then(|context| {
                context
                    .trigger_character
                    .as_ref()
                    .map(|trigger| trigger.as_str())
            });

            let in_scope =
                get_completion_identifiers(&text[..start], trigger, &linearization, &item)?;

            server.reply(Response::new_ok(id, in_scope));
            debug!("found closest item: {:?}", item);

            Ok(())
        }
        None => {
            server.reply(Response::new_ok(id, Value::Null));
            return Ok(());
        }
    }
}
