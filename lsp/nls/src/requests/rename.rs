use std::collections::HashMap;

use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{Range, RenameParams, TextEdit, Url, WorkspaceEdit};

use crate::cache::CacheExt as _;
use crate::diagnostic::LocationCompat;
use crate::server::Server;

pub fn handle_rename(
    params: RenameParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let pos = server
        .world
        .cache
        .position(&params.text_document_position)?;

    let ident = server.world.lookup_ident_by_position(pos)?;
    let term = server.world.lookup_term_by_position(pos)?;
    let mut def_locs = term
        .map(|term| server.world.get_defs(term, ident))
        .unwrap_or_default();

    def_locs.extend(ident.and_then(|id| id.pos.into_opt()));

    let mut all_positions: Vec<_> = def_locs
        .iter()
        .flat_map(|id| server.world.analysis.get_usages(id))
        .filter_map(|id| id.pos.into_opt())
        .chain(def_locs.iter().copied())
        .chain(
            def_locs
                .iter()
                .flat_map(|def| server.world.get_field_refs(*def)),
        )
        .collect();

    // Sort in some arbitrary order, for determinism and deduplication.
    all_positions.sort_by_key(|span| (span.src_id, span.start, span.end));
    all_positions.dedup();

    // Group edits by file
    let mut changes = HashMap::<Url, Vec<TextEdit>>::new();
    for pos in all_positions {
        let url = Url::from_file_path(server.world.cache.files().name(pos.src_id)).unwrap();
        changes.entry(url).or_default().push(TextEdit {
            range: Range::from_span(&pos, server.world.cache.files()),
            new_text: params.new_name.clone(),
        });
    }

    server.reply(Response::new_ok(
        id,
        WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        },
    ));
    Ok(())
}
