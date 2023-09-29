use log::debug;
use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{CompletionItem, CompletionItemKind, CompletionParams};
use nickel_lang_core::{
    cache::{self, InputFormat},
    identifier::Ident,
    position::RawPos,
    term::{BinaryOp, RichTerm, Term, UnaryOp},
};
use std::collections::HashSet;
use std::ffi::OsString;
use std::io;
use std::iter::Extend;
use std::path::PathBuf;

use crate::{
    cache::CacheExt,
    field_walker::{FieldHaver, FieldResolver},
    incomplete,
    server::Server,
    usage::Environment,
};

fn remove_duplicates(items: &Vec<CompletionItem>) -> Vec<CompletionItem> {
    let mut seen: Vec<CompletionItem> = Vec::new();
    for item in items {
        if !seen.iter().any(|seen_item| seen_item.label == item.label) {
            seen.push(item.clone())
        }
    }
    seen
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

// Try to interpret `term` as a record path to offer completions for.
fn sanitize_record_path_for_completion(
    term: &RichTerm,
    cursor: RawPos,
    server: &mut Server,
) -> Option<RichTerm> {
    if let (Term::ParseError(_), Some(range)) = (term.term.as_ref(), term.pos.as_opt_ref()) {
        let mut range = *range;
        let env = server
            .analysis
            .get_env(term)
            .cloned()
            .unwrap_or_else(Environment::new);
        if cursor.index < range.start || cursor.index > range.end || cursor.src_id != range.src_id {
            return None;
        }

        range.end = cursor.index;
        incomplete::parse_path_from_incomplete_input(range, &env, server)
    } else if let Term::Op1(UnaryOp::StaticAccess(_), parent) = term.term.as_ref() {
        // For completing record paths, we discard the last path element: if we're
        // completing `foo.bar.bla`, we only look at `foo.bar` to find the completions.
        Some(parent.clone())
    } else {
        None
    }
}

fn record_path_completion(term: RichTerm, server: &Server) -> Vec<CompletionItem> {
    log::info!("term based completion path: {term:?}");

    let (start_term, path) = extract_static_path(term);

    let defs = FieldResolver::new(server).resolve_term_path(&start_term, &path);
    defs.iter().flat_map(FieldHaver::completion_items).collect()
}

fn env_completion(rt: &RichTerm, server: &Server) -> Vec<CompletionItem> {
    let env = server.analysis.get_env(rt).cloned().unwrap_or_default();
    let resolver = FieldResolver::new(server);
    let mut items: Vec<_> = env
        .iter_elems()
        // The name `internals` is always in scope, but let's not advertise the fact. We do want
        // to complete `internals` if someone else uses that name, so we only filter out the
        // `internals` that doesn't have a position -- anything user-defined should have one.
        .filter(|(_, def_with_path)| {
            def_with_path.ident.ident != "internals".into() || def_with_path.ident.pos.is_def()
        })
        .map(|(_, def_with_path)| def_with_path.completion_item())
        .collect();

    // If the current term is a record, add its fields. (They won't be in the environment,
    // because that's the environment *of* the current term. And we don't want to treat
    // all possible FieldHavers here, because for example if the current term is a Term::Var
    // that references a record, we don't want it.)
    if matches!(rt.as_ref(), Term::RecRecord(..)) {
        items.extend(
            resolver
                .resolve_term(rt)
                .iter()
                .flat_map(FieldHaver::completion_items),
        );
    }

    // Iterate through all ancestors of our term, looking for identifiers that are "in scope"
    // because they're in an uncle/aunt/cousin that gets merged into our direct ancestors.
    if let Some(parents) = server.analysis.get_parent_chain(rt) {
        // We're only interested in adding identifiers from terms that are records or
        // merges/annotations of records. But actually we can skip the records, because any
        // records that are our direct ancestor have already contributed to `env`.
        let is_env_term = |rt: &RichTerm| {
            matches!(
                rt.as_ref(),
                Term::Op2(BinaryOp::Merge(_), _, _) | Term::Annotated(_, _) | Term::RecRecord(..)
            )
        };

        let is_merge_term = |rt: &RichTerm| {
            matches!(
                rt.as_ref(),
                Term::Op2(BinaryOp::Merge(_), _, _) | Term::Annotated(_, _)
            )
        };

        let mut parents = parents.peekable();
        while let Some(p) = parents.next() {
            // If a parent and a grandparent were both merges, we can skip the parent
            // because the grandparent will have a superset of its fields. This prevents
            // quadratic behavior on long chains of merges.
            if let Some(gp) = parents.peek() {
                if is_merge_term(&gp.0) {
                    continue;
                }
            }

            if is_env_term(&p.0) {
                let records = resolver.resolve_term_path(&p.0, &p.1.unwrap_or_default());
                items.extend(records.iter().flat_map(FieldHaver::completion_items));
            }
        }
    }

    items
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
            let completions = handle_import_completion(import, &params, server).unwrap_or_default();
            server.reply(Response::new_ok(id.clone(), completions));
        }
        return Ok(());
    }

    let sanitized_term = term
        .as_ref()
        .and_then(|rt| sanitize_record_path_for_completion(rt, cursor, server));

    #[allow(unused_mut)] // needs to be mut with feature = old-completer
    let mut completions = match (sanitized_term, term) {
        (Some(sanitized), _) => record_path_completion(sanitized, server),
        (_, Some(term)) => env_completion(&term, server),
        (None, None) => Vec::new(),
    };

    let completions = remove_duplicates(&completions);

    server.reply(Response::new_ok(id.clone(), completions));
    Ok(())
}

fn handle_import_completion(
    import: &OsString,
    params: &CompletionParams,
    server: &mut Server,
) -> io::Result<Vec<CompletionItem>> {
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
                CompletionItemKind::File
            } else {
                CompletionItemKind::Folder
            };
            CompletionItem {
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
