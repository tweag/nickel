use std::collections::HashSet;

use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location, ReferenceParams};
use nickel_lang_core::position::RawSpan;
use serde_json::Value;

use crate::{diagnostic::LocationCompat, server::Server, world::World};

fn spans_to_loc(ids: impl IntoIterator<Item = RawSpan>, world: &World) -> Vec<Location> {
    let mut spans: Vec<_> = ids.into_iter().collect();

    // The sort order of our response is a little arbitrary. But we want to deduplicate, and we
    // don't want the response to be random.
    spans.sort_by_key(|span| {
        (
            world.sources.files().name(span.src_id),
            span.start,
            span.end,
        )
    });
    spans.dedup();
    spans
        .iter()
        .filter_map(|loc| Location::from_span(loc, world.sources.files()))
        .collect()
}

pub fn handle_to_definition(
    params: GotoDefinitionParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let pos = server
        .world
        .position(&params.text_document_position_params)?;

    let ident = server.world.ident_at(pos)?;

    let locations = server
        .world
        .ast_at(pos)?
        .map(|term| server.world.get_defs(term, ident))
        .map(|defs| spans_to_loc(defs, &server.world))
        .unwrap_or_default();

    let response = if locations.is_empty() {
        Response::new_ok(id, Value::Null)
    } else if locations.len() == 1 {
        Response::new_ok(id, GotoDefinitionResponse::Scalar(locations[0].clone()))
    } else {
        Response::new_ok(id, GotoDefinitionResponse::Array(locations))
    };

    server.reply(response);
    Ok(())
}

pub fn handle_references(
    params: ReferenceParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let pos = server.world.position(&params.text_document_position)?;
    let ident = server.world.ident_at(pos)?;

    // The "references" of a symbol are all the usages of its definitions,
    // so first find the definitions and then find their usages.
    let term = server.world.ast_at(pos)?;
    let mut def_locs = term
        .map(|term| server.world.get_defs(term, ident))
        .unwrap_or_default();

    // Maybe the position is pointing straight at the definition already.
    // In that case, def_locs won't have the definition yet; so add it.
    def_locs.extend(ident.and_then(|id| id.pos.into_opt()));

    let mut usages: HashSet<_> = def_locs
        .iter()
        .flat_map(|id| server.world.analysis_reg.get_usages(id))
        .filter_map(|id| id.pos.into_opt())
        .collect();

    if params.context.include_declaration {
        usages.extend(def_locs.iter().cloned());
    }

    for span in def_locs {
        usages.extend(server.world.get_field_refs(span));
    }

    let locations = spans_to_loc(usages, &server.world);

    if locations.is_empty() {
        server.reply(Response::new_ok(id, Value::Null));
    } else {
        server.reply(Response::new_ok(id, locations));
    }
    Ok(())
}
