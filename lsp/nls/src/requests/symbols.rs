use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{DocumentSymbol, DocumentSymbolParams, SymbolKind};

use nickel_lang_core::{
    bytecode::ast::{Ast, typ::Type},
    position::TermPos,
    //TODO: move that out of Typecheck. Back in bytecode::ast ?
    typecheck::AnnotSeqRef,
};

use crate::{
    analysis::CollectedTypes,
    cache::CachesExt as _,
    field_walker::{FieldResolver, Record},
    server::Server,
    term::RawSpanExt,
    world::World,
};

// How deeply are we willing to recurse into records when resolving symbols?
// This needs to be bounded to avoid the stack overflowing for infinitely nested records.
const MAX_SYMBOL_DEPTH: usize = 32;

// Returns a hierarchy of "publicly accessible" symbols in a term.
//
// Basically, if the term "evaluates" (in the sense of FieldResolver's heuristics) to a record,
// all fields in that record count as publicly accessible symbols. Then we recurse into
// each of those.
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
        .filter_map(|rec| match rec {
            Record::RecordTerm(data) => Some(data),
            Record::RecordType(_) => None,
        })
        .flat_map(|record| {
            record
                .group_by_field_id()
                .into_iter()
                .filter_map(|(id, fields)| {
                    // We use an ident without position, when there are several of them available.
                    let pos_id = match fields.as_slice() {
                        &[field] => field.path.first().expect("empty field path").pos(),
                        _ => TermPos::None,
                    };
                    let loc_id = crate::identifier::LocIdent {
                        ident: id,
                        pos: pos_id,
                    };

                    let ty = type_lookups.idents.get(&loc_id);
                    let pos_id = pos_id.into_opt()?;
                    let (file_id, id_span) = pos_id.to_range();

                    // We need to find the span of the name (that's id_span above), but also the
                    // span of the "whole value," whatever that means. In vscode, there's a little
                    // outline bar at the top that shows you which symbol you're currently in, and it
                    // works by checking whether the cursor is inside the "whole value" range.
                    // We take this range large enough to contain the field value
                    // (if there is one) and any other annotations that we can work
                    // out the positions of.
                    //
                    // We can only return one range. In the case of a piecewise definition, we thus
                    // either take the first definition with a defined value (which is most likely
                    // to be the meat of the definition), or if no piece defines a value, we just
                    // take the identifier of the first field definition.
                    let selected_def_span = fields
                        .iter()
                        .find(|field_def| field_def.value.is_some())
                        .unwrap_or(&fields[0]);

                    let val_span = selected_def_span
                        .metadata
                        .annotation
                        .iter()
                        .filter_map(|ty| ty.pos.into_opt())
                        .chain(selected_def_span.value.as_ref().and_then(|val| val.pos.into_opt()))
                        .fold(pos_id, |a, b| a.fuse(b).unwrap_or(a));

                    let selection_range = crate::codespan_lsp::byte_span_to_range(
                        world.cache.sources.files(),
                        file_id,
                        id_span.clone(),
                    )
                    .ok()?;

                    let range = crate::codespan_lsp::byte_span_to_range(
                        world.cache.sources.files(),
                        file_id,
                        val_span.to_range().1,
                    )
                    .ok()?;

                    let children = max_depth.checked_sub(1).map(|depth| {
                        fields
                            .iter()
                            .filter_map(|field_def| field_def.value.as_ref())
                            .flat_map(|v| symbols(world, type_lookups, &v, depth))
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
                })
        })
        .collect()
}

pub fn handle_document_symbols(
    params: DocumentSymbolParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let file_id = server
        .world
        .cache
        .file_id(&params.text_document.uri)?
        .ok_or_else(|| crate::error::Error::FileNotFound(params.text_document.uri.clone()))?;

    let type_lookups = &server.world.file_analysis(file_id)?.type_lookup;
    let ast = server.world.cache.asts.get(&file_id);

    let mut symbols = ast
        .map(|ast| symbols(&server.world, type_lookups, ast, MAX_SYMBOL_DEPTH))
        .unwrap_or_default();
    // Sort so the response is deterministic.
    symbols.sort_by_key(|s| s.range.start);

    server.reply(Response::new_ok(id, symbols));

    Ok(())
}
