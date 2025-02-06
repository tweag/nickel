use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{CompletionItemKind, CompletionParams};
use nickel_lang_core::{
    bytecode::ast::{Import, Node, Ast, AstAlloc, record::{FieldDef, FieldMetadata}, typ::Type, primop::PrimOp},
    cache::{self, InputFormat},
    combine::{CombineAlloc, Combine},
    identifier::Ident,
    position::RawPos,
    pretty::Allocator,
};
use pretty::{DocBuilder, Pretty};
use std::collections::{HashMap, HashSet};
use std::ffi::OsString;
use std::io;
use std::iter::Extend;
use std::path::PathBuf;

use crate::{
    cache::CachesExt,
    field_walker::{FieldResolver, Record},
    identifier::LocIdent,
    incomplete,
    server::Server,
    usage::Environment,
    world::World,
};

// Bounds on the depth and total size of the contracts that we'll format for completion
// details.
const DEPTH_BOUND: usize = 2;
const SIZE_BOUND: usize = 32;

/// Filter out completion items that contain the cursor position.
///
/// In situations like
/// ```nickel
///  { foo, ba }
/// #         ^cursor
/// ```
/// we don't want to offer "ba" as a completion.
fn remove_myself<'ast>(
    items: impl Iterator<Item = CompletionItem<'ast>>,
    cursor: RawPos,
) -> impl Iterator<Item = CompletionItem<'ast>> {
    items.filter(move |it| it.ident.is_none_or(|ident| !ident.pos.contains(cursor)))
}

/// Combine duplicate items: take all items that share the same completion text, and
/// combine their documentation strings by removing duplicate documentation and concatenating
/// what's left.
fn combine_duplicates<'ast>(
    items: impl Iterator<Item = CompletionItem<'ast>>,
) -> Vec<lsp_types::CompletionItem> {
    let mut grouped = HashMap::<String, CompletionItem>::new();
    for item in items {
        grouped
            .entry(item.label.clone())
            .and_modify(|old| *old = Combine::combine(old.clone(), item.clone()))
            .or_insert(item);
    }

    grouped.into_values().map(From::from).collect()
}

fn extract_static_path<'ast>(mut ast: &'ast Ast<'ast>) -> (&'ast Ast<'ast>, Vec<Ident>) {
    let mut path = Vec::new();

    loop {
        if let Node::PrimOpApp { op: PrimOp::RecordStatAccess(id), args: [parent]} = &ast.node {
            path.push(id.ident());
            ast = parent;
        } else {
            path.reverse();
            return (ast, path);
        }
    }
}

/// If `term` is a `Term::ParseError`, see if we can find something in it to complete.
/// The situation to keep in mind is something like `{ foo = blah.sub `, in which case
/// this function should return a term representing the static path "blah.sub".
fn parse_term_from_incomplete_input<'ast>(
    ast: &'ast Ast<'ast>,
    cursor: RawPos,
    world: &mut World,
) -> Option<&'ast Ast<'ast>> {
    if let (Node::ParseError(_), Some(range)) = (&ast.node, ast.pos.as_opt_ref()) {
        let mut range = *range;
        let env = world
            .analysis
            .get_env(ast)
            .cloned()
            .unwrap_or_else(Environment::new);
        if cursor.index < range.start || cursor.index > range.end || cursor.src_id != range.src_id {
            return None;
        }

        range.end = cursor.index;
        incomplete::parse_path_from_incomplete_input(range, &env, world)
    } else {
        None
    }
}

// Try to interpret `term` as a record path to offer completions for.
fn sanitize_record_path_for_completion(term: &RichTerm) -> Option<RichTerm> {
    if let Term::Op1(UnaryOp::RecordAccess(_), parent) = term.term.as_ref() {
        // For completing record paths, we discard the last path element: if we're
        // completing `foo.bar.bla`, we only look at `foo.bar` to find the completions.
        Some(parent.clone())
    } else {
        None
    }
}

fn to_short_string(typ: &Type) -> String {
    let alloc = Allocator::bounded(DEPTH_BOUND, SIZE_BOUND);
    let doc: DocBuilder<_, ()> = typ.pretty(&alloc);
    pretty::Doc::pretty(&doc, 80).to_string()
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct CompletionItem<'ast> {
    pub label: String,
    pub metadata: Vec<FieldMetadata<'ast>>,
    pub ident: Option<LocIdent>,
}

impl Combine for CompletionItem {
    fn combine(mut left: Self, mut right: Self) -> Self {
        left.metadata.append(&mut right.metadata);
        left.ident = left.ident.or(right.ident);
        left
    }
}

impl From<CompletionItem> for lsp_types::CompletionItem {
    fn from(my: CompletionItem) -> Self {
        // The details are the type and contract annotations.
        let mut detail: Vec<_> = my
            .metadata
            .iter()
            .flat_map(|m| {
                m.annotation
                    .typ
                    .iter()
                    .map(|ty| to_short_string(&ty.typ))
                    .chain(
                        m.annotation
                            .contracts
                            .iter()
                            .map(|c| to_short_string(&c.label.typ)),
                    )
            })
            .collect();
        detail.sort();
        detail.dedup();
        let detail = detail.join("\n");

        let mut doc: Vec<_> = my
            .metadata
            .iter()
            .filter_map(|m| m.doc.as_deref())
            .collect();
        doc.sort();
        doc.dedup();
        // Docs are likely to be longer than types/contracts, so put
        // a blank line between them.
        let doc = doc.join("\n\n");

        Self {
            label: my.label,
            detail: (!detail.is_empty()).then_some(detail),
            kind: Some(CompletionItemKind::PROPERTY),
            documentation: (!doc.is_empty()).then_some(lsp_types::Documentation::MarkupContent(
                lsp_types::MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: doc,
                },
            )),
            ..Default::default()
        }
    }
}

fn record_path_completion(term: RichTerm, world: &World) -> Vec<CompletionItem> {
    log::info!("term based completion path: {term:?}");

    let (start_term, path) = extract_static_path(term);

    let defs = FieldResolver::new(world).resolve_path(&start_term, path.iter().copied());
    defs.iter().flat_map(Record::completion_items).collect()
}

/// Try to complete a field name in a record, like in
///
/// ```
/// { bar = 1, foo }
///               ^cursor
/// ```
///
/// In this situation we don't care about the environment, but we do care about
/// contracts and merged records.
///
/// If `path` is non-empty, instead of completing fields of `rt` we complete
/// the fields of `rt.<path>`. You might think we'd want to do this in a situation like
///
/// ```
/// { bar = 1, foo.blah.ba }
///                       ^cursor
/// ```
///
/// but in fact the nickel parser has already expanded this to
///
/// ```
/// { bar = 1, foo = { blah = { ba } } }
/// ```
///
/// so we don't encounter the path in this case. Instead, the non-empty path only comes
/// into play when the input fails to parse completely, like in (note the trailing dot)
///
/// ```
/// { bar = 1, foo.blah.ba. }
///                        ^cursor
/// ```
fn field_completion(rt: &RichTerm, world: &World, path: &[Ident]) -> Vec<CompletionItem> {
    let resolver = FieldResolver::new(world);
    let mut records = resolver.resolve_record(rt);

    // Look for identifiers that are "in scope" because they're in a cousin that gets merged
    // into us. For example, when completing
    //
    // { child = { } } | { child | { foo | Number } }
    //            ^
    // here, we want to offer "foo" as a completion.
    records.extend(resolver.cousin_records(rt));

    if path.is_empty() {
        // Avoid some work and allocations if there's no path to resolve further.
        records.iter().flat_map(Record::completion_items).collect()
    } else {
        let containers =
            resolver.resolve_containers_at_path(records.into_iter(), path.iter().copied());

        containers
            .into_iter()
            .filter_map(|c| Record::try_from(c).ok())
            .flat_map(|r| r.completion_items())
            .collect()
    }
}

fn env_completion(rt: &RichTerm, world: &World) -> Vec<CompletionItem> {
    let env = world.analysis.get_env(rt).cloned().unwrap_or_default();
    env.iter_elems()
        .map(|(_, def_with_path)| def_with_path.completion_item())
        .collect()
}

// Is `rt` a dynamic key of `parent`?
//
// If `rt` is a parse error, it will be a dynamic key of `parent` if it's in the
// syntactic position of a field name.
fn is_dynamic_key_of(rt: &RichTerm, parent: &RichTerm) -> bool {
    if let Term::RecRecord(_, dynamic, _) = parent.as_ref() {
        dynamic.iter().any(|(key, _value)| key == rt)
    } else {
        false
    }
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
    let cursor = server
        .world
        .cache
        .position(&params.text_document_position)?;
    let pos = RawPos {
        index: (cursor.index.0.saturating_sub(1)).into(),
        ..cursor
    };
    let trigger = params
        .context
        .as_ref()
        .and_then(|context| context.trigger_character.as_deref());

    let term = server.world.lookup_term_by_position(pos)?.cloned();
    let ident = server.world.lookup_ident_by_position(pos)?;

    if let Some(Term::Import(Import::Path { path: import, .. })) =
        term.as_ref().map(|t| t.term.as_ref())
    {
        // Don't respond with anything if trigger is a `.`, as that may be the
        // start of a relative file path `./`, or the start of a file extension
        if !matches!(trigger, Some(".")) {
            let completions = handle_import_completion(import, &params, server).unwrap_or_default();
            server.reply(Response::new_ok(id.clone(), completions));
        }
        return Ok(());
    }

    let path_term = term.as_ref().and_then(sanitize_record_path_for_completion);

    let completions = if let Some(path_term) = path_term {
        record_path_completion(path_term, &server.world)
    } else if let Some(term) = term {
        if let Some(incomplete_term) =
            parse_term_from_incomplete_input(&term, cursor, &mut server.world)
        {
            // A term coming from incomplete input could be either a record path, as in
            // { foo = bar.‸ }
            // or a record field, as in
            // { foo.bar.‸ }
            // We distinguish the two cases by looking at the the parent of `term` (which,
            // if we end up here, is a `Term::ParseError`).

            let parent = server.world.analysis.get_parent(&term).map(|p| &p.ast);

            if parent.is_some_and(|p| is_dynamic_key_of(&term, p)) {
                let (incomplete_term, mut path) = extract_static_path(incomplete_term);
                if let Term::Var(id) = incomplete_term.as_ref() {
                    path.insert(0, id.ident());
                    field_completion(&term, &server.world, &path)
                } else {
                    record_path_completion(incomplete_term, &server.world)
                }
            } else {
                record_path_completion(incomplete_term, &server.world)
            }
        } else if matches!(term.as_ref(), Term::RecRecord(..) | Term::Record(..)) && ident.is_some()
        {
            field_completion(&term, &server.world, &[])
        } else {
            env_completion(&term, &server.world)
        }
    } else {
        Vec::new()
    };

    let completions = combine_duplicates(remove_myself(completions.into_iter(), pos));

    server.reply(Response::new_ok(id.clone(), completions));
    Ok(())
}

fn handle_import_completion(
    import: &OsString,
    params: &CompletionParams,
    server: &mut Server,
) -> io::Result<Vec<lsp_types::CompletionItem>> {
    debug!("handle import completion");

    let current_file = params
        .text_document_position
        .text_document
        .uri
        .to_file_path()
        .unwrap();
    let current_file = cache::normalize_path(current_file)?;

    let mut current_path = current_file.clone();
    current_path.pop();
    current_path.push(import);

    #[derive(Eq, PartialEq, Hash)]
    struct Entry {
        path: PathBuf,
        file: bool,
    }

    let mut entries = HashSet::new();

    let dir = std::fs::read_dir(&current_path)?;
    let dir_entries = dir
        .filter_map(|i| i.ok().and_then(|d| d.file_type().ok().zip(Some(d))))
        .map(|(file_type, entry)| Entry {
            path: entry.path(),
            file: file_type.is_file(),
        });

    let cached_entries = server
        .world
        .file_uris
        .values()
        .filter_map(|uri| uri.to_file_path().ok())
        .filter(|path| path.starts_with(&current_path))
        .map(|path| Entry { path, file: true });

    entries.extend(dir_entries);
    entries.extend(cached_entries);

    let completions = entries
        .iter()
        .filter(|Entry { path, file }| {
            // don't try to import a file into itself
            cache::normalize_path(path).unwrap_or_default() != current_file
                // check that file is importable
                && (!*file || InputFormat::from_path(path).is_some())
        })
        .map(|entry| {
            let kind = if entry.file {
                CompletionItemKind::FILE
            } else {
                CompletionItemKind::FOLDER
            };
            lsp_types::CompletionItem {
                label: entry
                    .path
                    .file_name()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .to_string(),
                kind: Some(kind),
                ..Default::default()
            }
        })
        .collect::<Vec<_>>();
    Ok(completions)
}
