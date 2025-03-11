use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{CompletionItemKind, CompletionParams};
use nickel_lang_core::{
    bytecode::ast::{compat, primop::PrimOp, record::FieldMetadata, typ::Type, Ast, Import, Node},
    cache::{self, InputFormat, SourceCache},
    combine::Combine,
    identifier::Ident,
    position::{RawPos, RawSpan},
    pretty::Allocator,
};

use pretty::{DocBuilder, Pretty};

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    ffi::OsStr,
    io,
    iter::Extend,
    path::PathBuf,
};

use crate::{
    analysis::PackedAnalysis,
    field_walker::{FieldResolver, Record},
    identifier::LocIdent,
    incomplete,
    server::Server,
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
        if let Node::PrimOpApp {
            op: PrimOp::RecordStatAccess(id),
            args: [parent],
        } = &ast.node
        {
            path.push(id.ident());
            ast = parent;
        } else {
            path.reverse();
            return (ast, path);
        }
    }
}

// Given that the term under the cursor is a parse error, this function tries to reparse a
// sub-expression of the input preceding the cursor.
//
// We assume that the AST at cursor is a parse error; this function doesn't verify this
// precondition again.
//
// Since we need to take the analysis mutably, we don't return the newly parsed AST: such an
// interface would be unusable for the caller, as it would keep a mutable borrow to the analysis
// (and in practice to the analysis registry, to the world and to the server). Instead, we add the
// new AST to the usage table and to the position table, and we return its range upon success. The
// caller can then perform a new position lookup from an immutable borrow to the analysis to get a
// fresh immutable borrow to the new AST.
fn lookup_maybe_incomplete<'ast>(
    cursor: RawPos,
    sources: &SourceCache,
    analysis: &'ast mut PackedAnalysis,
) -> Option<RawSpan> {
    let range_err = analysis
        .analysis()
        .position_lookup
        .at(cursor.index)?
        .pos
        .into_opt()?;

    if cursor.index < range_err.start
        || cursor.index > range_err.end
        || cursor.src_id != range_err.src_id
    {
        return None;
    }

    let mut range = range_err.clone();
    range.end = cursor.index;

    incomplete::parse_incomplete_path(analysis, range, range_err, sources)
}

// Try to interpret `ast` as a record path to offer completions for.
fn sanitize_record_path_for_completion<'ast>(ast: &Ast<'ast>) -> Option<&'ast Ast<'ast>> {
    if let Node::PrimOpApp {
        op: PrimOp::RecordStatAccess(_),
        args: [parent],
    } = &ast.node
    {
        // For completing record paths, we discard the last path element: if we're
        // completing `foo.bar.bla`, we only look at `foo.bar` to find the completions.
        Some(parent)
    } else {
        None
    }
}

fn to_short_string<'ast>(typ: &Type<'ast>) -> String {
    use compat::FromAst as _;

    let alloc = Allocator::bounded(DEPTH_BOUND, SIZE_BOUND);
    //TODO[RFC007]: Implement Pretty for the new AST
    let doc: DocBuilder<_, ()> = nickel_lang_core::typ::Type::from_ast(typ).pretty(&alloc);
    pretty::Doc::pretty(&doc, 80).to_string()
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct CompletionItem<'ast> {
    pub label: String,
    /// Metadata are stored as [std::borrow::Cow] values, because they can come from either from
    /// [crate::bytecode::ast::LetMetadata] or [crate::bytecode::ast::FieldMetadata]. For
    /// simplicity, we convert everything to [crate::bytecode::ast::record::FieldMetadata], which
    /// means that we might need to allocate new metadata on the spot.
    pub metadata: Vec<Cow<'ast, FieldMetadata<'ast>>>,
    pub ident: Option<LocIdent>,
}

impl<'ast> Combine for CompletionItem<'ast> {
    fn combine(mut left: Self, mut right: Self) -> Self {
        left.metadata.append(&mut right.metadata);
        left.ident = left.ident.or(right.ident);
        left
    }
}

impl<'ast> From<CompletionItem<'ast>> for lsp_types::CompletionItem {
    fn from(my: CompletionItem) -> Self {
        // The details are the type and contract annotations.
        let mut detail: Vec<_> = my
            .metadata
            .iter()
            .flat_map(|m| {
                m.as_ref()
                    .annotation
                    .typ
                    .iter()
                    .map(|ty| to_short_string(ty))
                    .chain(m.annotation.contracts.iter().map(|c| to_short_string(c)))
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

fn record_path_completion<'ast>(
    ast: &'ast Ast<'ast>,
    world: &'ast World,
) -> Vec<CompletionItem<'ast>> {
    log::info!("term based completion path: {ast:?}");

    let (start_term, path) = extract_static_path(ast);

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
fn field_completion<'ast>(
    ast: &'ast Ast<'ast>,
    world: &'ast World,
    path: &[Ident],
) -> Vec<CompletionItem<'ast>> {
    let resolver = FieldResolver::new(world);
    let mut records = resolver.resolve_record(ast);

    // Look for identifiers that are "in scope" because they're in a cousin that gets merged
    // into us. For example, when completing
    //
    // { child = { } } | { child | { foo | Number } }
    //            ^
    // here, we want to offer "foo" as a completion.
    records.extend(resolver.cousin_records(ast));

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

fn env_completion<'ast>(ast: &'ast Ast<'ast>, world: &'ast World) -> Vec<CompletionItem<'ast>> {
    let env = world.analysis_reg.get_env(ast);
    env.map(|env| {
        env.iter_elems()
            .map(|(_, def_with_path)| def_with_path.completion_item())
            .collect()
    })
    .unwrap_or_default()
}

// Is `ast` a dynamic key of `parent`?
//
// If `ast` is a parse error, it will be a dynamic key of `parent` if it's in the syntactic
// position of a field name.
fn is_dynamic_key_of<'ast>(ast: &'ast Ast<'ast>, parent: &'ast Ast<'ast>) -> bool {
    if let Node::Record(record) = &parent.node {
        record.toplvl_dyn_fields().iter().any(|field| *field == ast)
    } else {
        false
    }
}

pub fn handle_completion(
    params: CompletionParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    fn to_lsp_types_items<'ast>(
        items: Vec<CompletionItem<'ast>>,
        pos: RawPos,
    ) -> Vec<lsp_types::CompletionItem> {
        debug!("to_lsp_types_items()");
        combine_duplicates(remove_myself(items.into_iter(), pos))
    }

    // The way indexing works here is that if the input file is
    //
    // foo‸
    //
    // then the cursor (denoted by ‸) is at index 3 and the span of foo is [0,
    // 3), which does not contain the cursor. For most purposes we're interested
    // in querying information about foo, so to do that we use the position just
    // *before* the cursor.
    let cursor = server.world.position(&params.text_document_position)?;
    let fixed_cursor = RawPos {
        index: (cursor.index.0.saturating_sub(1)).into(),
        ..cursor
    };
    let trigger = params
        .context
        .as_ref()
        .and_then(|context| context.trigger_character.as_deref());

    let analysis = server
        .world
        .analysis_reg
        .get_or_err(fixed_cursor.src_id)?
        .analysis();
    let ident = analysis.position_lookup.get_ident(fixed_cursor.index);
    let ast = analysis.position_lookup.at(fixed_cursor.index);

    debug!(
        "ast: {}, ident: {}",
        ast.as_ref()
            .map(|t| t.to_string())
            .unwrap_or("None".to_owned()),
        ident
            .as_ref()
            .map(|id| id.ident.to_string())
            .unwrap_or("None".to_owned())
    );

    let path_term = ast.and_then(|ast| sanitize_record_path_for_completion(ast));

    let completions = if let Some(path_term) = path_term {
        record_path_completion(path_term, &server.world)
    } else {
        match ast.as_ref() {
            Some(Ast {
                node: Node::Import(Import::Path { path: import, .. }),
                pos: _,
            }) => {
                debug!("import completion");

                // Don't respond with anything if trigger is a `.`, as that may be the
                // start of a relative file path `./`, or the start of a file extension
                if !matches!(trigger, Some(".")) {
                    let completions =
                        handle_import_completion(import, &params, server).unwrap_or_default();
                    server.reply(Response::new_ok(id.clone(), completions));
                }
                return Ok(());
            }
            Some(
                orig_err @ Ast {
                    node: Node::ParseError(_),
                    pos: _,
                },
            ) => {
                let parent = analysis.parent_lookup.parent(*orig_err).map(|p| p.ast);
                // This covers incomplete field definition, such as `{ foo.bar. }`. In that case, the
                // parse error is considered a dynamic key of the parent record.
                let is_dyn_key = parent.is_some_and(|p| is_dynamic_key_of(&orig_err, p));

                debug!(
                    "parent: {}",
                    parent.map(|p| p.to_string()).unwrap_or("None".to_owned())
                );
                debug!("is_ast_dyn_key: {}", is_dyn_key);

                let analysis = server
                    .world
                    .analysis_reg
                    .analyses
                    // unwrap(): we were already able to extract an ast from the analysis, so the
                    // analysis must exist for this file id
                    .get_mut(&fixed_cursor.src_id)
                    .unwrap();

                let completed_range =
                    lookup_maybe_incomplete(cursor, &server.world.sources, analysis);
                log::debug!("was able to complete (range)");
                // unwrap(): if `completed_range` is `Some`, `lookup_maybe_incomplete` must have
                // updated the position table of the corresponding analysis, which must be in the
                // registry.
                let completed = completed_range
                    .and_then(|r| server.world.lookup_ast_by_position(r.start_pos()).unwrap());

                if let Some(completed) = completed {
                    if is_dyn_key {
                        let (completed, mut path) = extract_static_path(completed);
                        if let Node::Var(id) = &completed.node {
                            path.insert(0, id.ident());
                            field_completion(&completed, &server.world, &path)
                        } else {
                            record_path_completion(completed, &server.world)
                        }
                    } else {
                        record_path_completion(completed, &server.world)
                    }
                } else {
                    // Otherwise, we try environment completion with the original parse error. We need
                    // to look it up again to avoid borrowing conflicts with the other branch.
                    server
                        .world
                        .lookup_ast_by_position(fixed_cursor)?
                        .map(|orig_err| env_completion(orig_err, &server.world))
                        .unwrap_or_default()
                }
            }
            Some(
                record @ Ast {
                    node: Node::Record(..),
                    pos: _,
                },
            ) if ident.is_some() => field_completion(*record, &server.world, &[]),
            Some(ast) => env_completion(*ast, &server.world),
            None => Vec::new(),
        }
    };

    let completions = to_lsp_types_items(completions, fixed_cursor);

    // unwrap(): the previous lookup for `ident` would have failed already if there was no analysis
    // for the current file. If we reach this line, there must be an analysis for the current file.
    //   let lookup_result = lookup_maybe_incomplete(
    //       pos,
    //       &server.world.sources,
    //       server
    //           .world
    //           .analysis_reg
    //           .analyses
    //           .get_mut(&pos.src_id)
    //           .unwrap(),
    //   );

    debug!("completions: {completions:?}");

    server.reply(Response::new_ok(id.clone(), completions));
    Ok(())
}

fn handle_import_completion(
    import: &OsStr,
    params: &CompletionParams,
    server: &Server,
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
