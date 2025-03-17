use std::collections::HashMap;

use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{Range, RenameParams, TextEdit, Url, WorkspaceEdit};

use crate::{diagnostic::LocationCompat, server::Server};

pub fn handle_rename(
    params: RenameParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let pos = server.world.position(&params.text_document_position)?;

    let ident_data = server.world.ident_data_at(pos)?;
    let term = server.world.ast_at(pos)?;
    let mut def_locs = term
        .map(|term| server.world.get_defs(term, ident_data.as_ref()))
        .unwrap_or_default();

    def_locs.extend(ident_data.and_then(|id_data| id_data.ident.pos.into_opt()));

    let mut all_positions: Vec<_> = def_locs
        .iter()
        .flat_map(|id| server.world.analysis_reg.get_usages(id))
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
        let url = Url::from_file_path(server.world.sources.files().name(pos.src_id)).unwrap();
        if let Some(range) = Range::from_span(&pos, server.world.sources.files()) {
            changes.entry(url).or_default().push(TextEdit {
                range,
                new_text: params.new_name.clone(),
            });
        }
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
