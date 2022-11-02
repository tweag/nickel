use codespan::ByteIndex;
use codespan_lsp::position_to_byte_index;
use lazy_static::lazy_static;
use log::debug;
use lsp_server::{ErrorCode, RequestId, Response, ResponseError};
use lsp_types::{CompletionItem, CompletionParams};
use nickel_lang::{
    identifier::Ident,
    term::{MetaValue, Term},
    types::{AbsType, RowIteratorItem, Types},
};
use serde_json::Value;

use crate::{
    linearization::{
        completed::Completed,
        interface::{TermKind, UsageState, ValueState},
        LinearizationItem,
    },
    server::{self, Server},
    trace::{Enrich, Trace},
};

/// Find record fields for an item with the specified id.
fn find_record_fields(linearization: &Completed, id: usize) -> Option<Vec<Ident>> {
    let item = linearization.get_item(id)?;
    match item.kind {
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
    }
}

/// Find a record contract of the item with the specified id.
fn find_record_contract_fields(linearization: &Completed, id: usize) -> Option<Vec<Ident>> {
    let item = linearization.get_item(id)?;
    match &item.meta {
        Some(MetaValue { contracts, .. }) if item.id == id => {
            contracts.iter().find_map(|contract| match &contract.types {
                Types(AbsType::Record(row)) => Some(extract_ident(&row)),
                Types(AbsType::Flat(term)) => {
                    if let Term::Record(fields, ..) | Term::RecRecord(fields, ..) = term.as_ref() {
                        Some(fields.keys().map(|ident| ident.clone()).collect())
                    } else {
                        None
                    }
                }
                _ => None,
            })
        }
        _ if item.id == id => match item.kind {
            TermKind::Declaration(_, _, ValueState::Known(new_id))
            | TermKind::Usage(UsageState::Resolved(new_id)) => {
                find_record_contract_fields(&linearization, new_id)
            }
            _ => None,
        },
        _ => None,
    }
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

lazy_static! {
    // unwrap is safe here because we know this is a correct regex.
    // This regexp must be the reverse of the regexp in the lexer (nickel_lang::parser::lexer)
    // to correctly parse identifiers.
    static ref RE: regex::Regex = regex::Regex::new(r"[_a-zA-Z0-9-']*[a-zA-Z]_?").unwrap();
}

/// Get the identifier before the `.`, for record completion.
fn get_identifier_before_dot(text: &str) -> Option<String> {
    let text: String = text
        .chars()
        .rev()
        .skip_while(|c| *c == '.') // skip . (if any)
        .collect();

    let name = RE.find(&text)?;
    Some(name.as_str().chars().rev().collect())
}

/// Get the identifier before `.<text>` for record completion.
fn get_identifier_before_field(text: &str) -> Option<String> {
    // Skip `<text>`
    let text: String = text
        .chars()
        .rev()
        .skip_while(|c| RE.is_match(c.to_string().as_ref()))
        .collect::<String>()
        .chars()
        .rev()
        .collect();

    get_identifier_before_dot(&text[..])
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

/// Search the linearization to find the record information associated with a
/// partiular ID, and in the scope of a given linearization item.
fn collect_record_info(
    linearization: &Completed,
    item: &LinearizationItem<Types>,
    name: usize,
) -> Vec<(Vec<Ident>, Types)> {
    linearization
        .get_in_scope(item)
        .iter()
        .filter_map(|item| {
            let (ty, _) = linearization.resolve_item_type_meta(item);
            match (&item.kind, ty) {
                // Get record fields from static type info
                (TermKind::Declaration(..), Types(AbsType::Record(row))) if name == item.id => {
                    Some((extract_ident(&row), item.ty.clone()))
                }
                (TermKind::Declaration(_, _, ValueState::Known(body_id)), _) if name == item.id => {
                    match find_record_contract_fields(&linearization, *body_id) {
                        // Get record fields from contract metadata
                        Some(fields) => Some((fields, item.ty.clone())),
                        // Get record fields from lexical scoping
                        None => find_record_fields(&linearization, *body_id)
                            .map(|idents| (idents, item.ty.clone())),
                    }
                }
                _ => None,
            }
        })
        .collect()
}

/// Generate possible completion Identifiers given a source text, and its
/// linearization.
fn get_completion_identifiers(
    source: &str,
    trigger: Option<&str>,
    linearization: &Completed,
    item: &LinearizationItem<Types>,
) -> Result<Vec<CompletionItem>, ResponseError> {
    let in_scope = match trigger {
        // Record Completion
        Some(server::DOT_COMPL_TRIGGER) => {
            let name = get_identifier_before_dot(source).ok_or(ResponseError {
                code: ErrorCode::InternalError as i32,
                message: "Couldn't get identifier for record completion".to_owned(),
                data: None,
            })?;
            let name = Ident::from(name);
            if let Some(name) = item.env.get(&name).cloned() {
                collect_record_info(linearization, item, name)
            } else {
                return Ok(Vec::new());
            }
        }
        Some(..) | None => {
            // This is also record completion, but it is in the form
            // <record var>.<partially-typed-field>
            // we also want to give completion based on <record var> in this case.
            if let Some(name) = get_identifier_before_field(source) {
                let name = Ident::from(name);
                if let Some(name) = item.env.get(&name).cloned() {
                    collect_record_info(linearization, item, name)
                } else {
                    return Ok(Vec::new());
                }
            } else {
                // variable name completion
                linearization
                    .get_in_scope(item)
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

    fn adjust_name(name: String) -> String {
        if name.is_ascii() {
            name
        } else {
            format!("\"{}\"", name)
        }
    }

    let in_scope: Vec<_> = in_scope
        .iter()
        .flat_map(|(idents, _)| {
            idents.iter().map(|ident| CompletionItem {
                label: adjust_name(ident.into_label()),
                ..Default::default()
            })
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
    // let linearization = linearization.clone();

    let item = linearization.item_at(&locator).cloned();
    Trace::enrich(&id, linearization);

    let result = match &item {
        Some(item) => {
            let trigger = params.context.as_ref().and_then(|context| {
                context
                    .trigger_character
                    .as_ref()
                    .map(|trigger| trigger.as_str())
            });

            let in_scope =
                get_completion_identifiers(&text[..start], trigger, linearization, item)?;

            Some(in_scope)
        }
        None => None,
    };

    result
        .map(|in_scope| {
            server.reply(Response::new_ok(id.clone(), in_scope));
            debug!("found closest item: {:?}", item);
            Ok(())
        })
        .unwrap_or_else(|| {
            server.reply(Response::new_ok(id, Value::Null));
            Ok(())
        })
}

#[cfg(test)]
mod tests {
    use nickel_lang::position::TermPos;

    use crate::linearization::Environment;

    use super::*;
    use std::collections::{HashMap, HashSet};

    #[test]
    fn test_remove_duplicates() {
        fn completion_item(names: Vec<&str>) -> Vec<CompletionItem> {
            names
                .iter()
                .map(|name| CompletionItem {
                    label: String::from(*name),
                    ..Default::default()
                })
                .collect::<Vec<_>>()
        }
        fn single_test(actual: Vec<&str>, expected: Vec<&str>) {
            // We're using a Hashset, because we don't want to be
            // particular about the order of the completion items.
            let actual: HashSet<_> = remove_duplicates(&completion_item(actual))
                .iter()
                .map(|item| item.label.clone())
                .collect();

            let expected: HashSet<_> = expected.iter().cloned().map(String::from).collect();

            assert_eq!(actual, expected)
        }

        let test_cases = [
            (
                vec!["foo", "bar", "bar", "baz", "bar"],
                vec!["baz", "foo", "bar"],
            ),
            (vec![], vec![]),
            (vec!["c", "c", "c"], vec!["c"]),
            (
                vec!["aaa", "aaa", "b", "b", "c", "c"],
                vec!["aaa", "b", "c"],
            ),
            (vec!["foo"], vec!["foo"]),
            (vec!["foo", "bar"], vec!["foo", "bar"]),
            (vec!["a", "b", "c", "a"], vec!["a", "b", "c"]),
        ];

        for (actual, expected) in test_cases {
            single_test(actual, expected)
        }
    }

    #[test]
    fn test_find_record_fields() {
        fn make_linearization_item(id: usize, kind: TermKind) -> LinearizationItem<Types> {
            LinearizationItem {
                env: Environment::new(),
                id,
                pos: TermPos::None,
                ty: Types(AbsType::Dyn()),
                kind,
                scope: Vec::new(),
                meta: None,
            }
        }
        fn make_completed(linearization: Vec<LinearizationItem<Types>>) -> Completed {
            let id_to_index: HashMap<_, _> = linearization
                .iter()
                .map(|item| item.id)
                .enumerate()
                .map(|(index, id)| (id, index))
                .collect();
            let scope = HashMap::new();
            Completed::new(linearization, scope, id_to_index)
        }

        // ids is an array of the ids from this linearization
        // which would give the expected output
        fn single_case<const N: usize>(
            linearization: Vec<LinearizationItem<Types>>,
            ids: [usize; N],
            mut expected: Vec<Ident>,
        ) {
            expected.sort();
            let completed = make_completed(linearization);
            for id in ids {
                let mut actual = find_record_fields(&completed, id).expect("Expected Some");
                actual.sort();
                assert_eq!(actual, expected)
            }
        }

        let a = make_linearization_item(
            0,
            TermKind::Declaration(Ident::from("a"), vec![3], ValueState::Known(1)),
        );
        let b = make_linearization_item(
            1,
            TermKind::Record(HashMap::from([
                (Ident::from("foo"), 2),
                (Ident::from("bar"), 2),
                (Ident::from("baz"), 2),
            ])),
        );
        let c = make_linearization_item(2, TermKind::Structure);
        let d = make_linearization_item(
            3,
            TermKind::Declaration(Ident::from("d"), Vec::new(), ValueState::Known(4)),
        );
        let e = make_linearization_item(4, TermKind::Usage(UsageState::Resolved(0)));
        let linearization = vec![a, b, c, d, e];
        let expected = vec![Ident::from("foo"), Ident::from("bar"), Ident::from("baz")];
        single_case(linearization, [0, 3], expected);

        let a = make_linearization_item(
            0,
            TermKind::Declaration(Ident::from("a"), Vec::new(), ValueState::Known(1)),
        );
        let b = make_linearization_item(
            1,
            TermKind::Record(HashMap::from([
                (Ident::from("one"), 2),
                (Ident::from("two"), 2),
                (Ident::from("three"), 2),
                (Ident::from("four"), 2),
            ])),
        );
        let c = make_linearization_item(2, TermKind::Structure);
        let d = make_linearization_item(
            3,
            TermKind::Declaration(Ident::from("d"), Vec::new(), ValueState::Known(13)),
        );
        let e = make_linearization_item(
            4,
            TermKind::Declaration(Ident::from("e"), Vec::new(), ValueState::Known(14)),
        );
        let f = make_linearization_item(
            5,
            TermKind::Declaration(Ident::from("f"), Vec::new(), ValueState::Known(15)),
        );
        let g = make_linearization_item(13, TermKind::Usage(UsageState::Resolved(0)));
        let h = make_linearization_item(14, TermKind::Usage(UsageState::Resolved(3)));
        let i = make_linearization_item(15, TermKind::Usage(UsageState::Resolved(4)));
        let expected = vec![
            Ident::from("one"),
            Ident::from("two"),
            Ident::from("three"),
            Ident::from("four"),
        ];
        let linearization = vec![a, b, c, d, e, f, g, h, i];
        single_case(linearization, [0, 3, 4, 5], expected);
    }

    #[test]
    fn test_find_record_contract_fields() {}
}
