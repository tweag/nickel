use codespan::ByteIndex;
use codespan_lsp::position_to_byte_index;
use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{CompletionContext, CompletionItem, CompletionParams};
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
    let _lin_env = &linearization.lin_env;

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

    let item = linearization.item_at(&locator);
    if item == None {
        // NOTE: this might result in a case where completion is not working.
        // This happens when an item cannot be found from the linearization.
        server.reply(Response::new_ok(id, Value::Null));
        return Ok(());
    }
    let item = item.unwrap().to_owned();

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
        Some(s) if s == "." => {
            // Get the ID before the .
            // if the current source is `let var = {a = 10} in var.`
            // name = var
            let name = text[..start - 1]
                .chars()
                .rev()
                .take_while(|c| c.is_ascii_alphabetic() || *c == '.')
                .skip_while(|c| *c == '.') // skip . (if any)
                .collect::<String>()
                .chars()
                .rev()
                .collect::<String>();

            linearization
                .linearization
                .iter()
                .filter_map(|i| {
                    let (ty, _) = linearization.resolve_item_type_meta(i);
                    match i.kind {
                        // Get record fields from lexical scoping
                        TermKind::Declaration(ident, _, ValueState::Known(body_id))
                            if ident.label() == name =>
                        {
                            find_record_fields(&linearization.linearization, body_id)
                                .map(|idents| Some((idents, i.ty.clone())))
                                .unwrap_or(None)
                        }

                        // Get record fields from static type info
                        TermKind::Declaration(ident, ..)
                            if ident.label() == name
                                && matches!(ty, Types(AbsType::Record(..))) =>
                        {
                            match &ty {
                                Types(AbsType::Record(row)) => {
                                    Some((extract_ident(row), i.ty.clone()))
                                }
                                _ => unreachable!(),
                            }
                        }

                        // Get record fields from contract metadata
                        TermKind::Declaration(ident, ..)
                            if ident.label() == name && i.meta.is_some() =>
                        {
                            i.meta.as_ref().map(|MetaValue { contracts, .. }| {
                                let fields = contracts
                                    .clone()
                                    .iter()
                                    .map(|Contract { types, .. }| match types {
                                        Types(AbsType::Record(row)) => extract_ident(row),
                                        _ => vec![],
                                    })
                                    .flatten()
                                    .collect();
                                (fields, i.ty.clone())
                            })
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

    // TODO: remove duplicates
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

    debug!("found closest item: {:?}", item);

    Ok(())
}
