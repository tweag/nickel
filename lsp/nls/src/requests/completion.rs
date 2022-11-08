use codespan::ByteIndex;
use codespan_lsp::position_to_byte_index;
use lazy_static::lazy_static;
use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{CompletionItem, CompletionParams};
use nickel_lang::{
    identifier::Ident,
    term::MetaValue,
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

/// Follow the path to find record fields,
/// route should be the reverse of the path we're following
fn find_fields_from_term_kind(
    linearization: &Completed,
    id: usize,
    path: &mut Vec<Ident>,
) -> Option<Vec<Ident>> {
    let item = linearization.get_item(id)?;
    match item.kind {
        TermKind::Record(ref fields) if item.id == id => {
            if path.is_empty() {
                Some(fields.keys().cloned().collect())
            } else {
                let name = path.pop()?;
                let new_id = fields.get(&name)?;
                find_fields_from_term_kind(&linearization, *new_id, path)
            }
        }
        TermKind::RecordField {
            value: ValueState::Known(new_id),
            ..
        }
        | TermKind::Declaration(_, _, ValueState::Known(new_id))
        | TermKind::Usage(UsageState::Resolved(new_id))
            if item.id == id =>
        {
            find_fields_from_term_kind(linearization, new_id, path)
        }
        _ => None,
    }
}

/// Find a record contract of the item with the specified id.
fn find_fields_from_contract(
    linearization: &Completed,
    id: usize,
    path: &mut Vec<Ident>,
) -> Option<Vec<Ident>> {
    let item = linearization.get_item(id)?;
    match &item.meta {
        Some(MetaValue { contracts, .. }) if item.id == id => {
            contracts.iter().find_map(|contract| match &contract.types {
                Types(AbsType::Record(row)) => Some(find_fields_from_type(&row, path)),
                _ => None,
            })
        }
        _ if item.id == id => match item.kind {
            TermKind::Declaration(_, _, ValueState::Known(new_id))
            | TermKind::Usage(UsageState::Resolved(new_id)) => {
                find_fields_from_contract(&linearization, new_id, path)
            }
            _ => None,
        },
        _ => None,
    }
}

fn find_fields_from_type(ty: &Box<Types>, path: &mut Vec<Ident>) -> Vec<Ident> {
    let current = path.pop();
    ty.iter_as_rows()
        .flat_map(|item| match (item, current) {
            (RowIteratorItem::Row(ident, Some(ty)), Some(current)) if current == *ident => {
                if let Types(AbsType::Record(ty)) = ty {
                    find_fields_from_type(ty, path)
                } else {
                    find_fields_from_type(&Box::new(ty.clone()), path)
                }
            }
            (RowIteratorItem::Row(..), Some(_)) => Vec::new(),
            (RowIteratorItem::Row(ident, _), None) => vec![ident.clone()],
            (RowIteratorItem::Tail(..), _) => Vec::new(),
        })
        .collect::<Vec<_>>()
}

lazy_static! {
    // unwrap is safe here because we know this is a correct regex.
    // This regexp must be the reverse of the regexp in the lexer (nickel_lang::parser::lexer)
    // to correctly parse identifiers.
    static ref RE: regex::Regex = regex::Regex::new(r"[_a-zA-Z0-9-']*[a-zA-Z]_?").unwrap();
    static ref RE_SPACE: regex::Regex = regex::Regex::new(r"\s+").unwrap();
}

/// Get the string chunks that make up an identifier path
fn get_identifier_path(text: &str) -> Option<Vec<String>> {
    let text: String = text
        .chars()
        .rev()
        .skip_while(|c| *c == '.') // skip . (if any)
        .collect::<String>()
        .chars()
        .rev()
        .collect();
    let result: Vec<_> = RE_SPACE.split(text.as_ref()).map(String::from).collect();
    let path = result
        .iter()
        .rev()
        .cloned()
        .collect::<Vec<_>>()
        .get(0)
        .cloned()?;
    Some(path.split(".").map(String::from).collect())
}

/// Get the identifier before `.<text>` for record completion.
fn get_identifier_before_field(text: &str) -> Option<Vec<String>> {
    // Skip `<text>`
    let text: String = text
        .chars()
        .rev()
        .skip_while(|c| RE.is_match(c.to_string().as_ref()))
        .collect::<String>()
        .chars()
        .rev()
        .collect();

    get_identifier_path(&text[..])
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
    id: usize,
    path: &mut Vec<Ident>,
) -> Vec<(Vec<Ident>, Types)> {
    linearization
        .get_in_scope(item)
        .iter()
        .filter_map(|item| {
            let (ty, _) = linearization.resolve_item_type_meta(item);
            match (&item.kind, ty) {
                // Get record fields from static type info
                (TermKind::Declaration(..), Types(AbsType::Record(row))) if id == item.id => {
                    Some((find_fields_from_type(&row, path), item.ty.clone()))
                }
                (TermKind::Declaration(_, _, ValueState::Known(body_id)), _) if id == item.id => {
                    match find_fields_from_contract(&linearization, *body_id, path) {
                        // Get record fields from contract metadata
                        Some(fields) => Some((fields, item.ty.clone())),
                        // Get record fields from lexical scoping
                        None => find_fields_from_term_kind(&linearization, id, path)
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
            // empty should return none
            if let Some(path) = get_identifier_path(source) {
                let mut path: Vec<_> = path.iter().rev().cloned().map(Ident::from).collect();
                let name = path.pop().unwrap();
                if let Some(id) = item.env.get(&name).copied() {
                    collect_record_info(linearization, item, id, &mut path)
                } else {
                    return Ok(Vec::new());
                }
            } else {
                return Ok(Vec::new());
            }
        }
        Some(..) | None => {
            // This is also record completion, but it is in the form
            // <record var>.<partially-typed-field>
            // we also want to give completion based on <record var> in this case.
            if let Some(path) = get_identifier_before_field(source) {
                let mut path: Vec<_> = path.iter().rev().cloned().map(Ident::from).collect();
                let name = path.pop().unwrap();
                if let Some(id) = item.env.get(&name).copied() {
                    collect_record_info(linearization, item, id, &mut path)
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

    /// Attach quotes to a non-ASCII string
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
    Ok(remove_duplicates(&in_scope))
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
    let item = linearization.item_at(&locator);
    Trace::enrich(&id, linearization);

    let result = match item {
        Some(item) => {
            debug!("found closest item: {:?}", item);

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
    fn test_get_identifier_path() {
        let tests = [
            ("let person = {} in \n    a.b.c.d", vec!["a", "b", "c", "d"]),
            ("   ab.dec", vec!["ab", "dec"]),
            (
                r##"let name = { sdf.clue.add.bar = 10 } in
            name.sdf.clue.add.bar"##,
                vec!["name", "sdf", "clue", "add", "bar"],
            ),
            ("name.class", vec!["name", "class"]),
            ("name.class.", vec!["name", "class"]),
            ("number", vec!["number"]),
            ("", vec![""]), // TODO: return an empty vector in this case?
            (
                r##"let x = {"fo京o" = {bar = 42}} in x."fo京o".foo"##,
                vec!["x", "\"fo京o\"", "foo"], // TODO: Do we unqote unicode string?
            ),
            (
                r##"let me : _ = { name = "gg", age = 23, a.b.c.d = 10 } in
                    me.ca.cb"##,
                vec!["me", "a", "b"],
            ),
        ];
        for (input, expected) in tests {
            let actual = get_identifier_path(input);
            let expected: Option<Vec<_>> =
                Some(expected.iter().cloned().map(String::from).collect());
            assert_eq!(actual, expected)
        }
    }

    #[test]
    fn test_extract_ident_with_path() {
        let ty = Types(AbsType::RowExtend(
            Ident::from("c2"),
            Some(Box::new(Types(AbsType::Num()))),
            Box::new(Types(AbsType::RowEmpty())),
        ));
        let ty = Types(AbsType::RowExtend(
            Ident::from("c1"),
            Some(Box::new(Types(AbsType::Num()))),
            Box::new(ty),
        ));
        let ty = Types(AbsType::RowExtend(
            Ident::from("b"),
            Some(Box::new(ty)),
            Box::new(Types(AbsType::RowEmpty())),
        ));
        let ty = Types(AbsType::RowExtend(
            Ident::from("a"),
            Some(Box::new(ty)),
            Box::new(Types(AbsType::RowEmpty())),
        ));
        let mut path = vec![Ident::from("b"), Ident::from("a")];
        let result = find_fields_from_type(&Box::new(ty), &mut path);
        let expected = vec![Ident::from("c1"), Ident::from("c2")];
        assert_eq!(
            result.iter().map(Ident::label).collect::<Vec<_>>(),
            expected.iter().map(Ident::label).collect::<Vec<_>>()
        )
    }

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
            Completed::new(linearization, id_to_index)
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
                let mut actual =
                    find_fields_from_term_kind(&completed, id, &mut Vec::new()).expect("Expected Some");
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
}
