use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location, ReferenceParams};
use nickel_lang_core::term::{RichTerm, Term, UnaryOp};
use serde_json::Value;

use crate::{
    cache::CacheExt, diagnostic::LocationCompat, field_walker::FieldResolver, identifier::LocIdent,
    server::Server,
};

fn get_defs(term: &RichTerm, server: &Server) -> Vec<LocIdent> {
    match term.as_ref() {
        Term::Var(id) => {
            if let Some(loc) = server
                .lin_registry
                .get_def(&(*id).into())
                .map(|def| def.ident)
            {
                vec![loc]
            } else {
                vec![]
            }
        }
        Term::Op1(UnaryOp::StaticAccess(id), parent) => {
            let resolver = FieldResolver::new(server);
            let parents = resolver.resolve_term(parent);
            parents
                .iter()
                .filter_map(|parent| parent.get_definition_pos(id.ident()))
                .collect()
        }
        _ => vec![],
    }
}

fn ids_to_locations(ids: impl IntoIterator<Item = LocIdent>, server: &Server) -> Vec<Location> {
    let mut spans: Vec<_> = ids.into_iter().filter_map(|id| id.pos.into_opt()).collect();

    // The sort order of our response is a little arbitrary. But we want to deduplicate, and we
    // don't want the response to be random.
    spans.sort_by_key(|span| (server.cache.files().name(span.src_id), span.start, span.end));
    spans.dedup();
    spans
        .iter()
        .map(|loc| Location::from_span(loc, server.cache.files()))
        .collect()
}

pub fn handle_to_definition(
    params: GotoDefinitionParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let pos = server
        .cache
        .position(&params.text_document_position_params)?;

    let locations = server
        .lookup_term_by_position(pos)?
        .map(|term| ids_to_locations(get_defs(term, server), server))
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
    let pos = server.cache.position(&params.text_document_position)?;

    // The "references" of a symbol are all the usages of its definitions,
    // so first find the definitions and then find their usages.
    let mut def_locs = server
        .lookup_term_by_position(pos)?
        .map(|term| get_defs(term, server))
        .unwrap_or_default();

    // Maybe the position is pointing straight at the definition already.
    // In that case, def_locs won't have the definition yet; so add it.
    def_locs.extend(server.lookup_ident_by_position(pos)?);

    let mut usages: Vec<_> = def_locs
        .iter()
        .flat_map(|id| server.lin_registry.get_usages(id))
        .cloned()
        .collect();

    if params.context.include_declaration {
        usages.extend(def_locs.iter().cloned());
    }

    let locations = ids_to_locations(usages, server);

    if locations.is_empty() {
        server.reply(Response::new_ok(id, Value::Null));
    } else {
        server.reply(Response::new_ok(id, locations));
    }
    Ok(())
}
