use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{DocumentSymbol, DocumentSymbolParams, SymbolKind};

use nickel_lang_core::{
    ast::{Ast, record::Record as RecordData, typ::Type},
    identifier::Ident,
};

use crate::{
    analysis::CollectedTypes,
    field_walker::{FieldDefPiece, FieldResolver, Record},
    server::Server,
    world::World,
};

// How deeply are we willing to recurse into records when resolving symbols?
// This needs to be bounded to avoid the stack overflowing for infinitely nested records.
const MAX_SYMBOL_DEPTH: usize = 32;

/// Returns a hierarchy of "publicly accessible" symbols in a term.
///
/// Basically, if the term "evaluates" (in the sense of FieldResolver's heuristics) to a record,
/// all fields in that record count as publicly accessible symbols. Then we recurse into
/// each of those.
fn symbols<'ast>(
    world: &'ast World,
    type_lookups: &CollectedTypes<'ast, Type<'ast>>,
    ast: &'ast Ast<'ast>,
    max_depth: usize,
) -> Vec<DocumentSymbol> {
    let resolver = FieldResolver::new(world);
    let root_records = resolver.resolve_path(ast, [].into_iter());
    root_records
        .into_iter()
        .flat_map(|rec| match rec {
            Record::Term(data) => record_symbols(world, type_lookups, data, max_depth),
            Record::Type(_) => Vec::new(),
            Record::FieldDefPiece(fdp) => def_piece_symbols(world, type_lookups, fdp, max_depth),
        })
        .collect()
}

fn record_symbols<'ast>(
    world: &'ast World,
    type_lookups: &CollectedTypes<'ast, Type<'ast>>,
    record: &'ast RecordData<'ast>,
    max_depth: usize,
) -> Vec<DocumentSymbol> {
    record
        .group_by_field_id()
        .into_iter()
        .filter_map(|(id, fields)| {
            def_pieces_symbols(
                world,
                type_lookups,
                id,
                &fields.into_iter().map(|fd| fd.into()).collect::<Vec<_>>(),
                max_depth,
            )
        })
        .collect()
}

fn def_piece_symbols<'ast>(
    world: &'ast World,
    type_lookups: &CollectedTypes<'ast, Type<'ast>>,
    def_piece: FieldDefPiece<'ast>,
    max_depth: usize,
) -> Vec<DocumentSymbol> {
    def_piece
        .ident()
        .map(|id| {
            def_pieces_symbols(world, type_lookups, id.ident, &[def_piece], max_depth)
                .into_iter()
                .collect()
        })
        .unwrap_or_default()
}

/// Return symbols for a definition consisting of an identifier and its definition pieces. The
/// pieces are assumed to be such that `piece.field_def.path[piece.index]` is equal to `id`.
fn def_pieces_symbols<'ast>(
    world: &'ast World,
    type_lookups: &CollectedTypes<'ast, Type<'ast>>,
    id: Ident,
    fields: &[FieldDefPiece<'ast>],
    max_depth: usize,
) -> Option<DocumentSymbol> {
    // Unfortunately, we can't return several ranges for a single symbol.
    //
    // In the case of a piecewise definition, we either take the first definition with a defined
    // value (which is most likely to be the meat of the definition), or if no piece defines a
    // value, we just take the first element.
    let selected_def = fields
        .iter()
        .find(|field_piece| field_piece.field_def.value.is_some())
        .or(fields.first())?;

    // unwrap(): pre-condition of this function.
    let pos_id = selected_def.ident().unwrap().pos;
    let loc_id = crate::identifier::LocIdent {
        ident: id,
        pos: pos_id,
    };

    let ty = type_lookups.idents.get(&loc_id);
    let pos_id = pos_id.into_opt()?;
    let file_id = pos_id.src_id;
    let id_range = pos_id.to_range();
    let val_span = selected_def.span().unwrap_or(pos_id);

    let selection_range =
        crate::codespan_lsp::byte_span_to_range(world.sources.files(), file_id, id_range.clone())
            .ok()?;

    let range = crate::codespan_lsp::byte_span_to_range(
        world.sources.files(),
        file_id,
        val_span.to_range(),
    )
    .ok()?;

    let children = max_depth.checked_sub(1).map(|depth| {
        fields
            .iter()
            .filter_map(|def_piece| def_piece.field_def.value.as_ref())
            .flat_map(|v| symbols(world, type_lookups, v, depth))
            .collect()
    });

    #[allow(deprecated)]
    // because the `deprecated` field is... wait for it... deprecated.
    Some(DocumentSymbol {
        name: id.to_string(),
        detail: ty.map(Type::to_string),
        kind: SymbolKind::VARIABLE,
        tags: None,
        range,
        selection_range,
        children,
        deprecated: None,
    })
}

pub fn handle_document_symbols(
    params: DocumentSymbolParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let file_id = server
        .world
        .file_id(&params.text_document.uri)?
        .ok_or_else(|| crate::error::Error::FileNotFound(params.text_document.uri.clone()))?;

    let analysis = server.world.file_analysis(file_id)?;
    let type_lookups = &analysis.analysis().type_lookup;
    let ast = analysis.ast();

    let mut symbols = symbols(&server.world, type_lookups, ast, MAX_SYMBOL_DEPTH);
    // Sort so the response is deterministic.
    symbols.sort_by_key(|s| s.range.start);

    server.reply(Response::new_ok(id, symbols));

    Ok(())
}
