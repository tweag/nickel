use crate::{
    cache::CacheExt,
    field_walker::{Def, FieldDefs},
    incomplete,
    linearization::{
        completed::Completed,
        interface::{TermKind, UsageState, ValueState},
        ItemId, LinRegistry, LinearizationItem,
    },
    server::Server,
    trace::{Enrich, Trace},
    usage::Environment,
};
use lazy_static::lazy_static;
use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, Documentation, MarkupContent, MarkupKind,
};
use nickel_lang_core::cache::InputFormat;
use nickel_lang_core::{
    identifier::{Ident, LocIdent},
    position::RawPos,
    term::{
        record::{Field, FieldMetadata},
        RichTerm, Term, TypeAnnotation, UnaryOp,
    },
    typ::{RecordRows, RecordRowsIteratorItem, Type, TypeF},
};
use std::ffi::OsString;
use std::io;

// General idea:
// A path is the reverse of the list of identifiers that make up a record indexing operation.
// e.g if we have a.b.c, the associated path would be vec![c, b, a]
// Paths are used to guide the completion engine to handle nested record indexing.
// They also generalize the single record indexing case.
// We follow the path by traversing a term, type or contract which represents a record
// and stop when there is nothing else on the path

#[derive(Debug)]
struct IdentWithType {
    ident: Ident,
    ty: Type,
    meta: Option<FieldMetadata>,
}

impl From<Ident> for IdentWithType {
    fn from(ident: Ident) -> Self {
        IdentWithType {
            ident,
            ty: Type::from(TypeF::Dyn),
            meta: None,
        }
    }
}

impl From<&str> for IdentWithType {
    fn from(ident: &str) -> Self {
        IdentWithType {
            ident: Ident::from(ident),
            ty: Type::from(TypeF::Dyn),
            meta: None,
        }
    }
}

impl IdentWithType {
    fn detail(&self) -> String {
        self.meta
            .as_ref()
            .and_then(|FieldMetadata { annotation, .. }| {
                annotation
                    .typ
                    .as_ref()
                    .map(|ty| ty.typ.to_string())
                    .or_else(|| annotation.contracts_to_string())
            })
            .unwrap_or_else(|| self.ty.to_string())
    }

    fn completion_item_kind(&self) -> CompletionItemKind {
        match &self.ty {
            ty if ty.is_function_type() => CompletionItemKind::Function,
            _ => CompletionItemKind::Property,
        }
    }

    fn to_lsp_completion_item(&self) -> CompletionItem {
        /// Attach quotes to a non-ASCII string
        fn adjust_name(name: &str) -> String {
            if name.is_ascii() {
                String::from(name)
            } else {
                format!("\"{name}\"")
            }
        }
        let doc = || {
            let meta = self.meta.as_ref()?;
            let doc = meta.doc.as_ref()?;
            let doc = Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: doc.clone(),
            });
            Some(doc)
        };
        CompletionItem {
            label: adjust_name(self.ident.label()),
            detail: Some(self.detail()),
            kind: Some(self.completion_item_kind()),
            documentation: doc(),
            ..Default::default()
        }
    }
}

/// Completion context: general data structures required by most completion functions.
#[derive(Clone, Copy)]
pub struct ComplCtx<'a> {
    linearization: &'a Completed,
    lin_registry: &'a LinRegistry,
}

/// Find the record field associated with a particular ID in the linearization
/// using lexical scoping rules.
fn find_fields_from_term_kind(
    id: ItemId,
    path: &mut Vec<LocIdent>,
    info @ ComplCtx {
        linearization,
        lin_registry,
    }: ComplCtx<'_>,
) -> Vec<IdentWithType> {
    let Some(item) = linearization.get_item_with_reg(id, lin_registry) else {
        return Vec::new();
    };
    let mut path_clone = path.clone();
    let contract_result = find_fields_from_contract(item.id, &mut path_clone, info);
    let result = match item.kind {
        TermKind::Record(ref fields) => {
            if path.is_empty() {
                fields
                    .iter()
                    .map(|(&ident, &id)| {
                        // This unwrap is safe because, `id` is the field of the record
                        // we're currently analyzing. We're sure that the linearization
                        // phase doesn't produce wrong or invalid ids.
                        let item = linearization.get_item_with_reg(id, lin_registry).unwrap();
                        let (ty, _) = linearization.get_type_and_metadata(item, lin_registry);
                        IdentWithType {
                            ident,
                            ty,
                            meta: item.metadata.clone(),
                        }
                    })
                    .collect()
            } else {
                let id = path.pop().and_then(|name| fields.get(&name.ident()));
                match id {
                    Some(id) => find_fields_from_term_kind(
                        *id,
                        path,
                        ComplCtx {
                            linearization,
                            lin_registry,
                        },
                    ),
                    None => Vec::new(),
                }
            }
        }
        TermKind::Declaration {
            value: ValueState::Known(body_id),
            path: Some(ref idents),
            ..
        } => {
            for ident in idents {
                path.push(*ident)
            }
            find_fields_from_term_kind(body_id, path, info)
        }
        TermKind::RecordField {
            value: ValueState::Known(new_id),
            ..
        }
        | TermKind::Declaration {
            value: ValueState::Known(new_id),
            ..
        } => find_fields_from_term_kind(new_id, path, info),
        TermKind::Usage(UsageState::Resolved(new_id)) => {
            find_fields_from_term_kind(new_id, path, info)
        }
        TermKind::Type(ref ty) => find_fields_from_type(ty, path, info),
        _ => Vec::new(),
    };
    result.into_iter().chain(contract_result).collect()
}

/// Find the record fields associated with an ID in the linearization using
/// its contract information.
fn find_fields_from_contract(
    id: ItemId,
    path: &mut Vec<LocIdent>,
    info @ ComplCtx {
        linearization,
        lin_registry: lin_cache,
    }: ComplCtx<'_>,
) -> Vec<IdentWithType> {
    let Some(item) = linearization.get_item_with_reg(id, lin_cache) else {
        return Vec::new();
    };
    match &item.metadata {
        Some(metadata) => find_fields_from_contracts(&metadata.annotation, path, info),
        None => match item.kind {
            TermKind::Declaration {
                value: ValueState::Known(new_id),
                ..
            }
            | TermKind::RecordField {
                value: ValueState::Known(new_id),
                ..
            }
            | TermKind::Usage(UsageState::Resolved(new_id)) => {
                find_fields_from_contract(new_id, path, info)
            }
            _ => Vec::new(),
        },
    }
}

/// Find record field associated with a field's metadata. This can be gotten from the type or the
/// contracts.
fn find_fields_from_contracts(
    annot: &TypeAnnotation,
    path: &[LocIdent],
    info @ ComplCtx { .. }: ComplCtx<'_>,
) -> Vec<IdentWithType> {
    annot
        .iter()
        .flat_map(|contract| {
            let mut path_copy = Vec::from(path);
            find_fields_from_type(&contract.typ, &mut path_copy, info)
        })
        .collect()
}

/// Find the fields that can be found from a type.
fn find_fields_from_type(
    ty: &Type,
    path: &mut Vec<LocIdent>,
    info @ ComplCtx { .. }: ComplCtx<'_>,
) -> Vec<IdentWithType> {
    match &ty.typ {
        TypeF::Record(row) => find_fields_from_rrows(row, path, info),
        TypeF::Dict { type_fields, .. } => match path.pop() {
            Some(..) => find_fields_from_type(type_fields, path, info),
            _ => Vec::new(),
        },
        TypeF::Flat(term) => find_fields_from_term(term, path, info),
        _ => Vec::new(),
    }
}

/// Extract the fields from a given record type.
fn find_fields_from_rrows(
    rrows: &RecordRows,
    path: &mut Vec<LocIdent>,
    info @ ComplCtx { .. }: ComplCtx<'_>,
) -> Vec<IdentWithType> {
    if let Some(current) = path.pop() {
        let type_of_current = rrows.iter().find_map(|item| match item {
            RecordRowsIteratorItem::Row(row) if row.id == current => Some(row.typ.clone()),
            _ => None,
        });

        match type_of_current {
            Some(Type {
                typ: TypeF::Record(rrows_current),
                ..
            }) => find_fields_from_rrows(&rrows_current, path, info),
            Some(Type {
                typ: TypeF::Flat(term),
                ..
            }) => find_fields_from_term(&term, path, info),
            _ => Vec::new(),
        }
    } else {
        rrows
            .iter()
            .filter_map(|item| match item {
                RecordRowsIteratorItem::Row(row) => Some((row.id, row.typ)),
                _ => None,
            })
            .map(|(ident, types)| IdentWithType {
                ident: ident.ident(),
                meta: None,
                ty: types.clone(),
            })
            .collect()
    }
}

fn find_fields_from_field(
    field: &Field,
    path: &mut Vec<LocIdent>,
    info: ComplCtx<'_>,
) -> Vec<IdentWithType> {
    find_fields_from_term_with_annot(&field.metadata.annotation, field.value.as_ref(), path, info)
}

// TODO: impl a trait FindField to avoid having long names
fn find_fields_from_term_with_annot(
    annot: &TypeAnnotation,
    value: Option<&RichTerm>,
    path: &mut Vec<LocIdent>,
    info: ComplCtx<'_>,
) -> Vec<IdentWithType> {
    let mut info_from_metadata = find_fields_from_contracts(annot, path, info);

    if let Some(value) = value {
        info_from_metadata.extend(find_fields_from_term(value, path, info));
    }

    info_from_metadata
}

/// Extract record fields from a record term.
fn find_fields_from_term(
    term: &RichTerm,
    path: &mut Vec<LocIdent>,
    info @ ComplCtx { lin_registry, .. }: ComplCtx<'_>,
) -> Vec<IdentWithType> {
    match term.as_ref() {
        Term::Record(data) | Term::RecRecord(data, ..) => match path.pop() {
            None => data
                .fields
                .iter()
                .map(|(ident, field)| IdentWithType {
                    ident: ident.ident(),
                    // This Dyn type is only displayed if the metadata's
                    // contract or type annotation is not present.
                    ty: Type::from(TypeF::Dyn),
                    meta: Some(field.metadata.clone()),
                })
                .collect(),
            Some(name) => data
                .fields
                .get(&name)
                .map(|field| find_fields_from_field(field, path, info))
                .unwrap_or_default(),
        },
        Term::Annotated(annot, term) => {
            find_fields_from_term_with_annot(annot, Some(term), path, info)
        }
        Term::Var(ident) | Term::Op1(UnaryOp::StaticAccess(ident), _) => {
            let pos = ident.pos;
            let span = pos.unwrap();
            // This unwrap is safe because we're getting an expression from
            // a linearized file, so all terms in the file and all terms in its
            // dependencies must have being linearized and stored in the cache.
            let linearization = lin_registry.map.get(&span.src_id).unwrap();
            let item = linearization.item_at(span.start_pos()).unwrap();
            find_fields_from_term_kind(item.id, path, info)
        }
        _ => Vec::new(),
    }
}

lazy_static! {
    // unwraps are safe here because we know these are correct regexes.
    // This regexp must be the same as the regex for identifiers in the lexer (nickel_lang_core::parser::lexer)
    static ref RE_IDENTIFIER: regex::Regex = regex::Regex::new(r"_?[a-zA-Z][_a-zA-Z0-9-']*").unwrap();
    static ref RE_SPACE: regex::Regex = regex::Regex::new(r"\s+").unwrap();
}

/// Get the string chunks that make up an identifier path.
fn get_identifier_path(text: &str) -> Option<Vec<String>> {
    /// Remove quotes from a record fields name (if any) and return a tuple
    /// of a String (the fields name) with the quotes removed and a bool
    /// indicating if the fields name actually contained quotes.
    fn remove_quotes(name: &str) -> (String, bool) {
        let mut chars = name.chars();
        if let (Some('"'), Some('"')) = (chars.next(), chars.last()) {
            (String::from(&name[1..name.len() - 1]), true)
        } else {
            (String::from(name), false)
        }
    }

    // skip any `.` at the end of the text
    let text = {
        let mut text = String::from(text);
        while text.ends_with('.') {
            text.pop();
        }
        text
    };

    let result: Vec<_> = RE_SPACE.split(text.as_ref()).map(String::from).collect();
    let path = result.iter().next_back().cloned()?;

    let path = path
        .split('.')
        .map(|str| {
            let (str, had_quotes) = remove_quotes(str);
            if had_quotes {
                // Nickel identifiers cannot contain unicode characters
                Some(str)
            } else {
                RE_IDENTIFIER.find(&str).map(|m| String::from(m.as_str()))
            }
        })
        .collect::<Option<Vec<_>>>()?;

    Some(path)
}

/// Get the identifiers before `.<text>` for record completion.
fn get_identifiers_before_field(text: &str) -> Option<Vec<String>> {
    // Skip `<text>`
    let text: String = text
        .chars()
        .rev()
        .skip_while(|c| RE_IDENTIFIER.is_match(c.to_string().as_ref()))
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
fn collect_record_info(
    linearization: &Completed,
    id: ItemId,
    path: &mut Vec<LocIdent>,
    lin_registry: &LinRegistry,
) -> Vec<IdentWithType> {
    let info = ComplCtx {
        linearization,
        lin_registry,
    };
    linearization
        .get_item_with_reg(id, lin_registry)
        .map(|item| {
            let (ty, _) = linearization.get_type_and_metadata(item, lin_registry);
            match (&item.kind, &ty.typ) {
                // Get record fields from static type info
                (_, TypeF::Record(rrows)) => find_fields_from_rrows(rrows, path, info),
                (
                    TermKind::Declaration {
                        value: ValueState::Known(body_id),
                        path: Some(idents),
                        ..
                    },
                    _,
                ) => {
                    for ident in idents {
                        path.push(*ident)
                    }
                    find_fields_from_term_kind(*body_id, path, info)
                }
                (
                    TermKind::Declaration {
                        value: ValueState::Known(body_id),
                        ..
                    }
                    | TermKind::RecordField {
                        value: ValueState::Known(body_id),
                        ..
                    },
                    _,
                ) => {
                    // The path is mutable, so the first case would consume the path
                    // so we have to clone it so that it can be correctly used for the second case.
                    let mut path_copy = path.clone();
                    find_fields_from_contract(*body_id, path, info)
                        .into_iter()
                        .chain(find_fields_from_term_kind(*body_id, &mut path_copy, info))
                        .collect()
                }
                _ => {
                    // The item might not com from a declaration (let-binding nor a record field),
                    // but is nontheless a record with interesting completion data. This is
                    // typically the case for the `std` entry point of the stdlib.
                    find_fields_from_term_kind(id, path, info)
                }
            }
        })
        .unwrap_or_default()
}

/// As we traverse a nested record all the way to the topmost record,
/// accumulate all the record metadata and corresponding path.
fn accumulate_record_meta_data<'a>(
    record: ItemId,
    info @ ComplCtx {
        linearization,
        lin_registry,
    }: ComplCtx<'a>,
    mut path: Vec<LocIdent>,
    result: &mut Vec<(&'a LinearizationItem<Type>, Vec<LocIdent>)>,
) {
    // This unwrap is safe: we know a `RecordField` must have a containing `Record`.
    let parent = linearization
        .get_item_with_reg(record, lin_registry)
        .unwrap();

    // The element just before a value should be the record field, because the linearization
    // items are sorted wrt. position
    let (next, _) = linearization.get_items_adjacent(parent.id);
    let next = next.and_then(|item| match item.kind {
        TermKind::RecordField {
            ident,
            record,
            value: ValueState::Known(value),
            ..
        } if value == parent.id => Some((ident, record)),
        _ => None,
    });

    result.push((parent, path.clone()));
    match next {
        None => {}
        Some((name, record)) => {
            path.push(name);
            accumulate_record_meta_data(record, info, path, result)
        }
    }
}

/// Generate possible completion identifiers given a source text, its linearization
/// and the current item the cursor points at.
fn get_completion_identifiers(
    source: &str,
    trigger: Option<&str>,
    linearization: &Completed,
    item: &LinearizationItem<Type>,
    server: &Server,
) -> Result<Vec<CompletionItem>, ResponseError> {
    fn complete(
        item: &LinearizationItem<Type>,
        name: LocIdent,
        server: &Server,
        path: &mut Vec<LocIdent>,
    ) -> Vec<IdentWithType> {
        let Some(item_id) = item.env.get(&name.ident()) else {
            return Vec::new();
        };
        let lin = server.lin_cache_get(&item_id.file_id).unwrap();
        collect_record_info(lin, *item_id, path, &server.lin_registry)
    }

    fn context_complete(
        item: &LinearizationItem<Type>,
        info: ComplCtx<'_>,
        path: Vec<LocIdent>,
    ) -> Vec<IdentWithType> {
        if let (&TermKind::RecordField { record, .. }, _) | (TermKind::Record(..), record) =
            (&item.kind, item.id)
        {
            let mut result = Vec::new();
            accumulate_record_meta_data(record, info, path, &mut result);

            result
                .into_iter()
                .flat_map(|(parent, path)| {
                    parent
                        .metadata
                        .as_ref()
                        .map(|meta| find_fields_from_contracts(&meta.annotation, &path, info))
                        .unwrap_or_default()
                })
                .collect()
        } else {
            Vec::new()
        }
    }

    let info = ComplCtx {
        linearization,
        lin_registry: &server.lin_registry,
    };

    let in_scope: Vec<_> = match trigger {
        Some(".") => {
            // Record completion
            let Some(path) = get_identifier_path(source) else {
                return Ok(Vec::new());
            };
            let mut path: Vec<_> = path.iter().rev().cloned().map(LocIdent::from).collect();

            let context_path = path.clone();
            let contextual_result = context_complete(item, info, context_path);

            // unwrap is safe here because we are guaranteed by `get_identifier_path`
            // that it will return a non-empty vector
            let name = path.pop().unwrap();
            complete(item, name, server, &mut path)
                .into_iter()
                .chain(contextual_result)
                .collect()
        }
        Some(..) | None => {
            if let Some(path) = get_identifiers_before_field(source) {
                // This is also record completion, but it is in the form
                // <record path>.<partially-typed-field>
                // we also want to give completion based on <record path> in this case.
                let mut path: Vec<_> = path.iter().rev().cloned().map(LocIdent::from).collect();

                // TODO: We need to adjust the linearization item here.
                // Say we have: `config.dat`, this parses as a nested record.
                // The item we get is the uncompleted `dat` field name as the item.
                // What we *really* want as the item is the `config` field name.
                let context_path = path.clone();
                let contextual_result = context_complete(item, info, context_path);

                // unwrap is safe here because we are guaranteed by `get_identifiers_before_field`
                // that it will return a non-empty vector
                let name = path.pop().unwrap();
                complete(item, name, server, &mut path)
                    .into_iter()
                    .chain(contextual_result)
                    .collect()
            } else {
                let contextual_result = context_complete(item, info, Vec::new());
                // Global name completion
                let (ty, _) = linearization.get_type_and_metadata(item, &server.lin_registry);
                linearization
                    .get_in_scope(item, &server.lin_registry)
                    .iter()
                    .filter_map(|i| match i.kind {
                        TermKind::Declaration { id: ident, .. }
                        | TermKind::RecordField { ident, .. } => Some(IdentWithType {
                            ident: ident.ident(),
                            meta: item.metadata.clone(),
                            ty: ty.clone(),
                        }),
                        _ => None,
                    })
                    .chain(contextual_result)
                    .collect()
            }
        }
    };

    let in_scope: Vec<_> = in_scope
        .iter()
        .map(|ident_meta| ident_meta.to_lsp_completion_item())
        .collect();
    Ok(remove_duplicates(&in_scope))
}

fn extract_static_path(mut rt: RichTerm) -> (RichTerm, Vec<Ident>) {
    let mut path = Vec::new();

    loop {
        if let Term::Op1(UnaryOp::StaticAccess(id), parent) = rt.term.as_ref() {
            path.push(id.ident());
            rt = parent.clone();
        } else {
            path.reverse();
            return (rt, path);
        }
    }
}

fn sanitize_term_for_completion(
    term: &RichTerm,
    cursor: RawPos,
    server: &mut Server,
) -> Option<RichTerm> {
    if let (Term::ParseError(_), Some(range)) = (term.term.as_ref(), term.pos.as_opt_ref()) {
        let mut range = *range;
        if cursor.index < range.start || cursor.index > range.end || cursor.src_id != range.src_id {
            return None;
        }

        range.end = cursor.index;
        incomplete::parse_path_from_incomplete_input(range, server)
    } else if let Term::Op1(UnaryOp::StaticAccess(_), parent) = term.term.as_ref() {
        // For completing record paths, we discard the last path element: if we're
        // completing `foo.bar.bla`, we only look at `foo.bar` to find the completions.
        Some(parent.clone())
    } else {
        None
    }
}

fn term_based_completion(
    term: RichTerm,
    initial_env: &Environment,
    server: &Server,
) -> Result<Vec<CompletionItem>, ResponseError> {
    log::info!("term based completion path: {term:?}");
    log::info!("initial env: {initial_env:?}");

    let (start_term, path) = extract_static_path(term);

    let defs = FieldDefs::resolve_path(&start_term, &path, initial_env, server);
    Ok(defs.defs().map(Def::to_completion_item).collect())
}

pub fn handle_completion(
    params: CompletionParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    // The way indexing works here is that if the input file is
    //
    // foo‸
    //
    // then the cursor (denoted by ‸) is at index 3 and the span of foo is [0,
    // 3), which does not contain the cursor. For most purposes we're interested
    // in querying information about foo, so to do that we use the position just
    // *before* the cursor.
    let cursor = server.cache.position(&params.text_document_position)?;
    let pos = RawPos {
        index: (cursor.index.0.saturating_sub(1)).into(),
        ..cursor
    };
    let trigger = params
        .context
        .as_ref()
        .and_then(|context| context.trigger_character.as_deref());

    let term = server.lookup_term_by_position(pos)?.cloned();

    if let Some(Term::Import(import)) = term.as_ref().map(|t| t.term.as_ref()) {
        // Don't respond with anything if trigger is a `.`, as that may be the
        // start of a relative file path `./`, or the start of a file extension
        if !matches!(trigger, Some(".")) {
            let completions = handle_import_completion(&import, &params).unwrap_or_default();
            server.reply(Response::new_ok(id.clone(), completions));
        }
        return Ok(());
    }

    let sanitized_term = term
        .as_ref()
        .and_then(|rt| sanitize_term_for_completion(rt, cursor, server));

    let mut completions = match term.zip(sanitized_term) {
        Some((term, sanitized)) => {
            let env = if matches!(term.term.as_ref(), Term::ParseError(_)) {
                server
                    .lin_registry
                    .get_env(&term)
                    .cloned()
                    .unwrap_or_default()
            } else {
                Environment::new()
            };
            term_based_completion(sanitized, &env, server)?
        }
        None => Vec::new(),
    };

    log::info!("term-based completion provided {completions:?}");
    let linearization = server.lin_cache_get(&pos.src_id)?;
    Trace::enrich(&id, linearization);

    let item = linearization.item_at(pos);
    let text = server.cache.files().source(pos.src_id);
    let start = pos.index.to_usize();
    if let Some(item) = item {
        debug!("found closest item: {:?}", item);

        completions.extend_from_slice(&get_completion_identifiers(
            &text[..start],
            trigger,
            linearization,
            item,
            server,
        )?);
    };
    let completions = remove_duplicates(&completions);

    server.reply(Response::new_ok(id.clone(), completions));
    Ok(())
}

fn handle_import_completion(
    import: &OsString,
    params: &CompletionParams,
) -> io::Result<Vec<CompletionItem>> {
    debug!("handle import completion");

    let current_file = params
        .text_document_position
        .text_document
        .uri
        .to_file_path()
        .unwrap()
        .canonicalize()?;
    let mut current_path = current_file.clone();
    current_path.pop();
    current_path.push(import);
    let dir = std::fs::read_dir(&current_path)?;

    let completions = dir
        .filter_map(|i| i.ok().and_then(|d| d.file_type().ok().zip(Some(d))))
        .filter(|(file_type, d)| {
            // don't try to import a file into itself
            d.path().canonicalize().unwrap_or_default() != current_file
                // check that file is importable
                && (file_type.is_dir() || InputFormat::from_path_buf(d.path().as_path()).is_some())
        })
        .map(|(file_type, d)| {
            let kind = if file_type.is_file() {
                CompletionItemKind::File
            } else {
                CompletionItemKind::Folder
            };
            CompletionItem {
                label: d.file_name().to_string_lossy().to_string(),
                kind: Some(kind),
                ..Default::default()
            }
        })
        .collect::<Vec<_>>();
    return Ok(completions);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::linearization::Environment;
    use codespan::Files;
    use nickel_lang_core::{position::TermPos, term::MergePriority};
    use std::collections::{HashMap, HashSet};

    fn make_lin_item(
        id: ItemId,
        kind: TermKind,
        metadata: Option<FieldMetadata>,
    ) -> LinearizationItem<Type> {
        LinearizationItem {
            env: Environment::new(),
            term: RichTerm::new(Term::Null, TermPos::None),
            id,
            pos: TermPos::None,
            ty: Type::from(TypeF::Dyn),
            kind,
            metadata,
        }
    }

    fn make_completed(linearization: Vec<LinearizationItem<Type>>) -> Completed {
        let id_to_index: HashMap<_, _> = linearization
            .iter()
            .map(|item| item.id)
            .enumerate()
            .map(|(index, id)| (id, index))
            .collect();
        Completed::new(linearization, id_to_index, HashMap::new())
    }

    #[test]
    fn test_get_identifier_path() {
        let tests = [
            (
                "Simple let binding with nested record index",
                "let person = {} in \n    a.b.c.d",
                Some(vec!["a", "b", "c", "d"]),
            ),
            ("Simple record index", "   ab.dec", Some(vec!["ab", "dec"])),
            (
                "Multiple let bindings with nested record index ",
                r##"let name = { sdf.clue.add.bar = 10 } in
                let other = name in
                let another = other in
            name.sdf.clue.add.bar"##,
                Some(vec!["name", "sdf", "clue", "add", "bar"]),
            ),
            (
                "Incomplete record with nested record index",
                r##"{
                    foo = let bar = {a.b.c.d = 10} in bar.a.b.c.d"##,
                Some(vec!["bar", "a", "b", "c", "d"]),
            ),
            (
                "Simple record Index",
                "name.class",
                Some(vec!["name", "class"]),
            ),
            (
                "Simple record Index ending with a dot",
                "name.class.",
                Some(vec!["name", "class"]),
            ),
            ("Single record variable", "number", Some(vec!["number"])),
            (
                "Single record variable ending with a dot",
                "number.",
                Some(vec!["number"]),
            ),
            (
                "Record binding with unicode string names for fields",
                r#"let x = {"fo京o" = {bar = 42}} in x."fo京o".foo"#,
                Some(vec!["x", "fo京o", "foo"]),
            ),
            (
                "Typed record binding with nested record indexing",
                r#"let me : _ = { name = "foo", time = 1800, a.b.c.d = 10 } in
                    me.ca.cb"#,
                Some(vec!["me", "ca", "cb"]),
            ),
            ("Single quote", "\"", None),
            (
                "Nested Parenthesis prefix",
                "(alpha.beta.",
                Some(vec!["alpha", "beta"]),
            ),
            ("Parenthesis prefix", "(single", Some(vec!["single"])),
            (
                "Nested curly brace prefix",
                "{first.second",
                Some(vec!["first", "second"]),
            ),
            ("Curly brace prefix", "{double.", Some(vec!["double"])),
        ];
        for (case_name, input, expected) in tests {
            let actual = get_identifier_path(input);
            let expected: Option<Vec<_>> =
                expected.map(|path| path.iter().map(|s| String::from(*s)).collect());
            assert_eq!(actual, expected, "test failed: {case_name}")
        }
    }

    #[test]
    fn test_extract_ident_with_path() {
        use nickel_lang_core::{mk_uty_record, mk_uty_row};
        use std::convert::TryInto;

        // Representing the type: {a: {b : {c1 : Num, c2: Num}}}
        let c_record_type = mk_uty_record!(("c1", TypeF::Number), ("c2", TypeF::Number));
        let b_record_type = mk_uty_record!(("b", c_record_type));
        let a_record_type = mk_uty_row!(("a", b_record_type));

        let mut path = vec![LocIdent::from("b"), LocIdent::from("a")];
        // unwrap: the conversion must succeed because we built a type without unification variable
        // nor type constants
        let info = ComplCtx {
            linearization: &Default::default(),
            lin_registry: &LinRegistry::new(),
        };
        let result: Vec<_> = find_fields_from_rrows(
            &Box::new(a_record_type.try_into().unwrap()),
            &mut path,
            info,
        )
        .iter()
        .map(|iwm| iwm.ident)
        .collect();
        let expected: Vec<_> = vec![IdentWithType::from("c1"), IdentWithType::from("c2")]
            .iter()
            .map(|iwm| iwm.ident)
            .collect();
        assert_eq!(result, expected)
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
        fn make_completed(linearization: Vec<LinearizationItem<Type>>) -> Completed {
            let id_to_index: HashMap<_, _> = linearization
                .iter()
                .map(|item| item.id)
                .enumerate()
                .map(|(index, id)| (id, index))
                .collect();
            Completed::new(linearization, id_to_index, HashMap::new())
        }

        // ids is an array of the ids from this linearization
        // which would give the expected output
        fn single_case<const N: usize>(
            linearization: Vec<LinearizationItem<Type>>,
            ids: [ItemId; N],
            expected: Vec<IdentWithType>,
        ) {
            let mut expected: Vec<_> = expected.iter().map(|iwm| iwm.ident).collect();
            expected.sort();
            let completed = make_completed(linearization);
            for id in ids {
                let info = ComplCtx {
                    linearization: &completed,
                    lin_registry: &LinRegistry::new(),
                };
                let mut actual: Vec<_> = find_fields_from_term_kind(id, &mut Vec::new(), info)
                    .iter()
                    .map(|iwm| iwm.ident)
                    .collect();
                actual.sort();
                assert_eq!(actual, expected)
            }
        }

        let mut files = Files::new();
        let file_id = files.add("test", "test");

        let a = make_lin_item(
            ItemId { file_id, index: 0 },
            TermKind::Declaration {
                id: LocIdent::from("a"),
                usages: vec![ItemId { file_id, index: 3 }],
                value: ValueState::Known(ItemId { file_id, index: 1 }),
                path: None,
            },
            None,
        );
        let b = make_lin_item(
            ItemId { file_id, index: 1 },
            TermKind::Record(HashMap::from([
                (Ident::from("foo"), ItemId { file_id, index: 2 }),
                (Ident::from("bar"), ItemId { file_id, index: 2 }),
                (Ident::from("baz"), ItemId { file_id, index: 2 }),
            ])),
            None,
        );
        let c = make_lin_item(ItemId { file_id, index: 2 }, TermKind::Structure, None);
        let d = make_lin_item(
            ItemId { file_id, index: 3 },
            TermKind::Declaration {
                id: LocIdent::from("d"),
                usages: Vec::new(),
                value: ValueState::Known(ItemId { file_id, index: 4 }),
                path: None,
            },
            None,
        );
        let e = make_lin_item(
            ItemId { file_id, index: 4 },
            TermKind::Usage(UsageState::Resolved(ItemId { file_id, index: 0 })),
            None,
        );
        let linearization = vec![a, b, c, d, e];
        let expected = vec![
            IdentWithType::from("foo"),
            IdentWithType::from("bar"),
            IdentWithType::from("baz"),
        ];
        single_case(
            linearization,
            [ItemId { file_id, index: 0 }, ItemId { file_id, index: 3 }],
            expected,
        );

        let a = make_lin_item(
            ItemId { file_id, index: 0 },
            TermKind::Declaration {
                id: LocIdent::from("a"),
                usages: Vec::new(),
                value: ValueState::Known(ItemId { file_id, index: 1 }),
                path: None,
            },
            None,
        );
        let b = make_lin_item(
            ItemId { file_id, index: 1 },
            TermKind::Record(HashMap::from([
                (Ident::from("one"), ItemId { file_id, index: 2 }),
                (Ident::from("two"), ItemId { file_id, index: 2 }),
                (Ident::from("three"), ItemId { file_id, index: 2 }),
                (Ident::from("four"), ItemId { file_id, index: 2 }),
            ])),
            None,
        );
        let c = make_lin_item(ItemId { file_id, index: 2 }, TermKind::Structure, None);
        let d = make_lin_item(
            ItemId { file_id, index: 3 },
            TermKind::Declaration {
                id: LocIdent::from("d"),
                usages: Vec::new(),
                value: ValueState::Known(ItemId { file_id, index: 13 }),
                path: None,
            },
            None,
        );
        let e = make_lin_item(
            ItemId { file_id, index: 4 },
            TermKind::Declaration {
                id: LocIdent::from("e"),
                usages: Vec::new(),
                value: ValueState::Known(ItemId { file_id, index: 14 }),
                path: None,
            },
            None,
        );
        let f = make_lin_item(
            ItemId { file_id, index: 5 },
            TermKind::Declaration {
                id: LocIdent::from("f"),
                usages: Vec::new(),
                value: ValueState::Known(ItemId { file_id, index: 15 }),
                path: None,
            },
            None,
        );
        let g = make_lin_item(
            ItemId { file_id, index: 13 },
            TermKind::Usage(UsageState::Resolved(ItemId { file_id, index: 0 })),
            None,
        );
        let h = make_lin_item(
            ItemId { file_id, index: 14 },
            TermKind::Usage(UsageState::Resolved(ItemId { file_id, index: 3 })),
            None,
        );
        let i = make_lin_item(
            ItemId { file_id, index: 15 },
            TermKind::Usage(UsageState::Resolved(ItemId { file_id, index: 4 })),
            None,
        );
        let expected = vec![
            IdentWithType::from("one"),
            IdentWithType::from("two"),
            IdentWithType::from("three"),
            IdentWithType::from("four"),
        ];
        let linearization = vec![a, b, c, d, e, f, g, h, i];
        single_case(
            linearization,
            [
                ItemId { file_id, index: 0 },
                ItemId { file_id, index: 3 },
                ItemId { file_id, index: 4 },
                ItemId { file_id, index: 5 },
            ],
            expected,
        );
    }

    #[test]
    fn test_collect_record_info_with_non_contract_meta() {
        let mut files = Files::new();
        let file_id = files.add("test", "");
        let id = ItemId { file_id, index: 0 };
        let a = make_lin_item(
            id,
            TermKind::Declaration {
                id: LocIdent::from("a"),
                usages: Vec::new(),
                value: ValueState::Known(ItemId { file_id, index: 1 }),
                path: None,
            },
            None,
        );

        let meta = FieldMetadata {
            doc: Some("doc".to_string()),
            annotation: TypeAnnotation {
                typ: None,
                contracts: Vec::new(),
            },
            opt: false,
            not_exported: false,
            priority: MergePriority::Neutral,
        };

        let c = make_lin_item(ItemId { file_id, index: 2 }, TermKind::Structure, None);
        let b = make_lin_item(
            ItemId { file_id, index: 1 },
            TermKind::Record(HashMap::from([
                (Ident::from("one"), ItemId { file_id, index: 2 }),
                (Ident::from("two"), ItemId { file_id, index: 2 }),
            ])),
            Some(meta),
        );
        let lin = make_completed(vec![a, b, c]);

        let actual = collect_record_info(&lin, id, &mut Vec::new(), &LinRegistry::new());
        let mut expected = vec![Ident::from("one"), Ident::from("two")];
        let mut actual = actual.iter().map(|iwm| iwm.ident).collect::<Vec<_>>();
        expected.sort();
        actual.sort();
        assert_eq!(actual, expected)
    }
}
