use std::convert::TryFrom;

use codespan::{ByteIndex, FileId};
use codespan_lsp::position_to_byte_index;
use lazy_static::lazy_static;
use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{CompletionItem, CompletionParams};
use nickel_lang::{
    identifier::Ident,
    stdlib::StdlibModule,
    term::{MetaValue, RichTerm, Term},
    types::{RecordRows, RecordRowsIteratorItem, TypeF, Types},
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

// General idea:
// A path is the reverse of the list of identifiers that make up a record indexing operation.
// e.g if we have a.b.c, the associated path would be vec![c, b, a]
// Paths are used to guide the completion engine to handle nested record indexing.
// They also generalize the single record indexing case.
// We follow the path by traversing a term, type or contract which represents a record
// and stop when there is nothing else on the path

/// Find the record field associated with a particular ID in the linearization
/// using lexical scoping rules.
fn find_fields_from_term_kind(
    linearization: &Completed,
    id: usize,
    path: &mut Vec<Ident>,
) -> Option<Vec<Ident>> {
    let item = linearization.get_item(id)?;
    match item.kind {
        TermKind::Record(ref fields) => {
            if path.is_empty() {
                Some(fields.keys().cloned().collect())
            } else {
                let name = path.pop()?;
                let new_id = fields.get(&name)?;
                find_fields_from_term_kind(linearization, *new_id, path)
            }
        }
        TermKind::RecordField {
            value: ValueState::Known(new_id),
            ..
        }
        | TermKind::Declaration(_, _, ValueState::Known(new_id))
        | TermKind::Usage(UsageState::Resolved(new_id)) => {
            find_fields_from_term_kind(linearization, new_id, path)
        }
        _ => None,
    }
}

/// Find the record fields associated with an ID in the linearization using
/// its contract information.
fn find_fields_from_contract(
    linearization: &Completed,
    id: usize,
    path: &mut Vec<Ident>,
) -> Option<Vec<Ident>> {
    let item = linearization.get_item(id)?;
    match &item.meta {
        Some(meta_value) => Some(find_fields_from_meta_value(meta_value, path)),
        None => match item.kind {
            TermKind::Declaration(_, _, ValueState::Known(new_id))
            | TermKind::Usage(UsageState::Resolved(new_id)) => {
                find_fields_from_contract(linearization, new_id, path)
            }
            _ => None,
        },
    }
}

/// Find record field associated associated with a MetaValue.
/// This can be gotten from the type or the contracts.
fn find_fields_from_meta_value(meta_value: &MetaValue, path: &mut Vec<Ident>) -> Vec<Ident> {
    meta_value
        .contracts
        .iter()
        .chain(meta_value.types.iter())
        .flat_map(|contract| match &contract.types {
            Types(TypeF::Record(row)) => find_fields_from_type(row, path),
            Types(TypeF::Flat(term)) => find_fields_from_term(term, path).unwrap_or_default(),
            _ => Vec::new(),
        })
        .collect()
}

/// Extract the fields from a given record type.
fn find_fields_from_type(rrows: &RecordRows, path: &mut Vec<Ident>) -> Vec<Ident> {
    if let Some(current) = path.pop() {
        let type_of_current = rrows.iter().find_map(|item| match item {
            RecordRowsIteratorItem::Row(row) if row.id == current => Some(row.types.clone()),
            _ => None,
        });

        match type_of_current {
            Some(Types(TypeF::Record(rrows_current))) => {
                find_fields_from_type(&rrows_current, path)
            }
            Some(Types(TypeF::Flat(term))) => {
                find_fields_from_term(&term, path).unwrap_or_default()
            }
            _ => Vec::new(),
        }
    } else {
        rrows
            .iter()
            .filter_map(|item| match item {
                RecordRowsIteratorItem::Row(row) => Some(row.id),
                _ => None,
            })
            .collect::<Vec<_>>()
    }
}

/// Extract record fields from a record term.
fn find_fields_from_term(term: &RichTerm, path: &mut Vec<Ident>) -> Option<Vec<Ident>> {
    let current = path.pop();
    match (term.as_ref(), current) {
        (Term::Record(data) | Term::RecRecord(data, ..), None) => {
            Some(data.fields.keys().cloned().collect())
        }
        (Term::Record(data) | Term::RecRecord(data, ..), Some(name)) => {
            let term = data.fields.get(&name)?;
            find_fields_from_term(term, path)
        }
        (Term::MetaValue(meta_value), Some(ident)) => {
            // We don't need to pop here, as the metavalue wraps the actual record
            path.push(ident);
            Some(find_fields_from_meta_value(meta_value, path))
        }
        (Term::MetaValue(meta_value), None) => Some(find_fields_from_meta_value(meta_value, path)),
        _ => None,
    }
}

lazy_static! {
    // unwrap is safe here because we know this is a correct regex.
    // This regexp must be the reverse of the regexp in the lexer (nickel_lang::parser::lexer)
    // to correctly parse identifiers.
    static ref RE: regex::Regex = regex::Regex::new(r"[_a-zA-Z0-9-']*[a-zA-Z]_?").unwrap();
    static ref RE_SPACE: regex::Regex = regex::Regex::new(r"\s+").unwrap();
}

/// Get the string chunks that make up an identifier path.
fn get_identifier_path(text: &str) -> Option<Vec<String>> {
    // skip any `.` at the end of the text
    let text = {
        let mut text = String::from(text);
        while text.ends_with('.') {
            text.pop();
        }
        text
    };

    let result: Vec<_> = RE_SPACE.split(text.as_ref()).map(String::from).collect();
    let path = result.iter().rev().next().cloned()?;
    // Remove quotes from a record name
    fn remove_quotes(name: &str) -> String {
        if let (Some('"'), Some('"')) = (name.chars().next(), name.chars().last()) {
            String::from(&name[1..name.len() - 1])
        } else {
            String::from(name)
        }
    }
    Some(path.split('.').map(remove_quotes).collect())
}

/// Get the identifiers before `.<text>` for record completion.
fn get_identifiers_before_field(text: &str) -> Option<Vec<String>> {
    // Skip `<text>`
    let text: String = text
        .chars()
        .rev()
        .skip_while(|c| RE.is_match(c.to_string().as_ref()))
        .collect::<String>()
        .chars()
        .rev()
        .collect();

    get_identifier_path(text.as_str())
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
fn collect_record_info(linearization: &Completed, id: usize, path: &mut Vec<Ident>) -> Vec<Ident> {
    linearization
        .get_item(id)
        .map(|item| {
            let (ty, _) = linearization.resolve_item_type_meta(item);
            match (&item.kind, ty) {
                // Get record fields from static type info
                (TermKind::Declaration(..), Types(TypeF::Record(rrows))) => {
                    find_fields_from_type(&rrows, path)
                }
                (TermKind::Declaration(_, _, ValueState::Known(body_id)), _) => {
                    find_fields_from_contract(&linearization, *body_id, path)
                        .or_else(|| find_fields_from_term_kind(&linearization, id, path))
                        .unwrap_or_default()
                }
                (
                    TermKind::RecordField {
                        value: ValueState::Known(value),
                        ..
                    },
                    _,
                ) => find_fields_from_term_kind(&linearization, *value, path).unwrap_or_default(),
                _ => Vec::new(),
            }
        })
        .unwrap_or_default()
}

/// Generate possible completion identifiers given a source text, its linearization
/// and the current item the cursor points at.
fn get_completion_identifiers(
    source: &str,
    file: FileId,
    trigger: Option<&str>,
    linearization: &Completed,
    item: &LinearizationItem<Types>,
    server: &Server,
) -> Result<Vec<CompletionItem>, ResponseError> {
    fn complete(
        item: &LinearizationItem<Types>,
        name: Ident,
        linearization: &Completed,
        server: &Server,
        path: &mut Vec<Ident>,
        file: FileId,
    ) -> Option<Vec<Ident>> {
        item.env
            .get(&(name, file))
            .map(|key| (key, linearization))
            .or_else(|| {
                // If we can't find in the file's scope, check it from the stdlib's scope.
                let module = StdlibModule::try_from(name).ok()?;
                let file = server.cache.get_submodule_file_id(module)?;
                let lin = server.lin_cache_get(&file).ok()?;
                item.env.get(&(name, file)).map(|key| (key, lin))
            })
            .map(|(id, lin)| collect_record_info(lin, *id, path))
    }

    /// Attach quotes to a non-ASCII string
    fn adjust_name(name: String) -> String {
        if name.is_ascii() {
            name
        } else {
            format!("\"{}\"", name)
        }
    }

    let in_scope = match trigger {
        // Record Completion
        Some(server::DOT_COMPL_TRIGGER) => {
            get_identifier_path(source)
                .and_then(|path| {
                    let mut path: Vec<_> = path.iter().rev().cloned().map(Ident::from).collect();
                    // unwrap is safe here because we are guaranteed by `get_identifier_path`
                    // that it will return a non-empty vector
                    let name = path.pop().unwrap();
                    complete(item, name, linearization, server, &mut path, file)
                })
                .unwrap_or_default()
        }
        Some(..) | None => {
            // This is also record completion, but it is in the form
            // <record path>.<partially-typed-field>
            // we also want to give completion based on <record path> in this case.
            if let Some(path) = get_identifiers_before_field(source) {
                let mut path: Vec<_> = path.iter().rev().cloned().map(Ident::from).collect();
                // unwrap is safe here because we are guaranteed by `get_identifiers_before_field`
                // that it will return a non-empty vector
                let name = path.pop().unwrap();
                complete(item, name, linearization, server, &mut path, file).unwrap_or_default()
            } else {
                // variable name completion
                linearization
                    .get_in_scope(item)
                    .iter()
                    .filter_map(|i| match i.kind {
                        TermKind::Declaration(ref ident, _, _) => Some(ident.clone()),
                        _ => None,
                    })
                    .collect::<Vec<_>>()
            }
        }
    };

    let in_scope: Vec<_> = in_scope
        .iter()
        .map(|ident| CompletionItem {
            label: adjust_name(ident.into_label()),
            ..Default::default()
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

            let trigger = params
                .context
                .as_ref()
                .and_then(|context| context.trigger_character.as_deref());

            let in_scope = get_completion_identifiers(
                &text[..start],
                file_id,
                trigger,
                linearization,
                item,
                &server,
            )?;

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
    use super::*;
    use crate::linearization::Environment;
    use nickel_lang::position::TermPos;
    use std::collections::{HashMap, HashSet};

    #[test]
    fn test_get_identifier_path() {
        let tests = [
            (
                "Simple let binding with nested record index",
                "let person = {} in \n    a.b.c.d",
                vec!["a", "b", "c", "d"],
            ),
            ("Simple record index", "   ab.dec", vec!["ab", "dec"]),
            (
                "Multiple let bindings with nested record index ",
                r##"let name = { sdf.clue.add.bar = 10 } in
                let other = name in
                let another = other in
            name.sdf.clue.add.bar"##,
                vec!["name", "sdf", "clue", "add", "bar"],
            ),
            (
                "Incomplete record with nested record index",
                r##"{
                    foo = let bar = {a.b.c.d = 10} in bar.a.b.c.d"##,
                vec!["bar", "a", "b", "c", "d"],
            ),
            ("Simple record Index", "name.class", vec!["name", "class"]),
            (
                "Simple record Index ending with a dot",
                "name.class.",
                vec!["name", "class"],
            ),
            ("Single record variable", "number", vec!["number"]),
            (
                "Single record variable ending with a dot",
                "number.",
                vec!["number"],
            ),
            (
                "Record binding with unicode string names for fields",
                r##"let x = {"fo京o" = {bar = 42}} in x."fo京o".foo"##,
                vec!["x", "fo京o", "foo"],
            ),
            (
                "Typed record binding with nested record indexing",
                r##"let me : _ = { name = "foo", time = 1800, a.b.c.d = 10 } in
                    me.ca.cb"##,
                vec!["me", "ca", "cb"],
            ),
        ];
        for (case_name, input, expected) in tests {
            let actual = get_identifier_path(input);
            let expected: Option<Vec<_>> =
                Some(expected.iter().cloned().map(String::from).collect());
            assert_eq!(actual, expected, "test failed: {}", case_name)
        }
    }

    #[test]
    fn test_extract_ident_with_path() {
        use nickel_lang::{mk_tyw_record, mk_tyw_row, types::RecordRowsF};
        use std::convert::TryInto;

        // Representing the type: {a: {b : {c1 : Num, c2: Num}}}
        let c_record_type = mk_tyw_record!(("c1", TypeF::Num), ("c2", TypeF::Num));
        let b_record_type = mk_tyw_record!(("b", c_record_type));
        let a_record_type = mk_tyw_row!(("a", b_record_type));

        let mut path = vec![Ident::from("b"), Ident::from("a")];
        // unwrap: the conversion must succeed because we built a type without unification variable
        // nor type constants
        let result = find_fields_from_type(&Box::new(a_record_type.try_into().unwrap()), &mut path);
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
                ty: Types(TypeF::Dyn),
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
                let mut actual = find_fields_from_term_kind(&completed, id, &mut Vec::new())
                    .expect("Expected Some");
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
