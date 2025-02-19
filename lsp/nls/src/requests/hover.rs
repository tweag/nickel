use std::borrow::Cow;

use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{Hover, HoverContents, HoverParams, LanguageString, MarkedString, Range};

use nickel_lang_core::{
    bytecode::ast::{primop::PrimOp, record::FieldMetadata, typ::Type, Ast, Node},
    combine::Combine,
    identifier::Ident,
    position::RawSpan,
    typecheck::AnnotSeqRef,
};

use serde_json::Value;

use crate::{
    cache::CachesExt,
    diagnostic::LocationCompat,
    field_walker::{Def, FieldResolver, Record},
    identifier::LocIdent,
    server::Server,
    utils::dedup,
    world::World,
};

#[derive(Debug, Default)]
struct HoverData<'ast> {
    values: Vec<&'ast Ast<'ast>>,
    metadata: Vec<Cow<'ast, FieldMetadata<'ast>>>,
    span: Option<RawSpan>,
    ty: Option<Type<'ast>>,
}

impl Combine for HoverData<'_> {
    fn combine(mut left: Self, mut right: Self) -> Self {
        left.values.append(&mut right.values);
        left.metadata.append(&mut right.metadata);
        left.ty = left.ty.or(right.ty);
        left.span = left.span.or(right.span);
        left
    }
}

fn annotated_contracts<'ast>(ast: &'ast Ast<'ast>) -> &'ast [Type<'ast>] {
    match &ast.node {
        Node::Annotated { annot, inner: _ } => annot.contracts,
        _ => &[],
    }
}

fn nickel_string(s: String) -> MarkedString {
    MarkedString::LanguageString(LanguageString {
        language: "nickel".to_owned(),
        value: s,
    })
}

fn values_and_metadata_from_field<'ast>(
    parents: Vec<Record<'ast>>,
    ident: Ident,
) -> (Vec<&'ast Ast<'ast>>, Vec<Cow<'ast, FieldMetadata<'ast>>>) {
    let mut values = Vec::new();
    let mut metadata = Vec::new();
    for parent in parents {
        // TODO: this is probably wrong. We get the metadata from the leaf (the last element of the
        // path), even when we query a field in the middle. We should probably use the same data
        // structure than for containers here: FieldDefPiece.
        for piece in parent.field_pieces(ident) {
            values.extend(piece.value.iter());
            metadata.push(Cow::Borrowed(&piece.metadata));
        }
    }
    (values, metadata)
}

fn ident_hover<'ast>(ident: LocIdent, world: &'ast World) -> Option<HoverData<'ast>> {
    let ty = world.analysis_reg.get_type_for_ident(&ident).cloned();
    let span = ident.pos.into_opt()?;
    let mut ret = HoverData {
        values: Vec::new(),
        metadata: Vec::new(),
        span: Some(span),
        ty,
    };

    if let Some(def) = world.analysis_reg.get_def(&ident) {
        let resolver = FieldResolver::new(world);
        let path = def.path();

        if let Some((last, path)) = path.split_last() {
            for value in def.values() {
                let parents = resolver.resolve_path(value, path.iter().copied());
                let (values, metadata) = values_and_metadata_from_field(parents, *last);
                ret.values.extend(values);
                ret.metadata.extend(metadata);
            }
        } else {
            let cousins = resolver.cousin_defs(def);
            if cousins.is_empty() {
                ret.values.extend(def.values());
            } else {
                for cousin in cousins {
                    ret.values.extend(cousin.value());
                    ret.metadata.extend(cousin.metadata().map(Cow::Borrowed));
                }
            }

            ret.metadata.extend(def.metadata());
        }
    }

    Some(ret)
}

fn term_hover<'ast>(ast: &'ast Ast<'ast>, world: &'ast World) -> Option<HoverData<'ast>> {
    let ty = world.analysis_reg.get_type(ast).cloned();
    let span = ast.pos.into_opt();

    match &ast.node {
        Node::PrimOpApp {
            op: PrimOp::RecordStatAccess(id),
            args: [parent],
        } => {
            let resolver = FieldResolver::new(world);
            let parents = resolver.resolve_record(parent);
            let (values, metadata) = values_and_metadata_from_field(parents, id.ident());
            Some(HoverData {
                values,
                metadata,
                span,
                ty,
            })
        }
        _ => Some(HoverData {
            values: vec![ast],
            metadata: vec![],
            span,
            ty,
        }),
    }
}

pub fn handle(
    params: HoverParams,
    req_id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    let pos = server
        .world
        .cache
        .position(&params.text_document_position_params)?;

    let ident_hover_data = server
        .world
        .lookup_ident_by_position(pos)?
        .and_then(|ident| ident_hover(ident, &server.world));

    let ast = server.world.lookup_term_by_position(pos)?;
    let ast_hover_data = ast.and_then(|rt| term_hover(rt, &server.world));

    // We combine the hover information from the term (which can have better type information)
    // and the ident (which can have better metadata), but only when hovering over a `Var`.
    // In general, the term and the ident can have different meanings (like when hovering over
    // the `x` in `let x = ... in y`) and so it would be confusing to combine them.
    let hover_data = if matches!(ast.as_ref(), Some(Node::Var(_))) {
        Combine::combine(ident_hover_data, ast_hover_data)
    } else {
        ident_hover_data.or(ast_hover_data)
    };

    if let Some(hover) = hover_data {
        let mut contents = Vec::new();

        // Collect all the type and contract annotations we can find. We don't distinguish between them
        // (and we deduplicate annotations if they're present as both types and contracts). However, we
        // do give some special attention to the inferred static type if there is one: we list it first.
        let mut annotations: Vec<_> = hover
            .metadata
            .iter()
            .flat_map(|m| m.annotation.iter().map(|typ| typ.to_string()))
            .chain(
                hover
                    .values
                    .iter()
                    .flat_map(|ast| annotated_contracts(*ast))
                    .map(|contract| contract.to_string()),
            )
            .collect();
        dedup(&mut annotations);

        let ty = hover
            .ty
            .as_ref()
            .map(Type::to_string)
            .unwrap_or_else(|| "Dyn".to_owned());

        // There's no point in repeating the static type in the annotations.
        if let Some(idx) = annotations.iter().position(|a| a == &ty) {
            annotations.remove(idx);
        }

        // Only report a Dyn type if there's no more useful information.
        if ty != "Dyn" || annotations.is_empty() {
            contents.push(nickel_string(ty));
        }

        contents.extend(annotations.into_iter().map(nickel_string));

        // Not sure how to do documentation merging yet, so pick the first non-empty one.
        let doc = hover.metadata.iter().find_map(|m| m.doc.as_ref());
        if let Some(doc) = doc {
            contents.push(MarkedString::String(doc.to_string()));
        }

        server.reply(Response::new_ok(
            req_id,
            Hover {
                contents: HoverContents::Array(contents),
                range: hover
                    .span
                    .and_then(|s| Range::from_span(&s, server.world.cache.sources.files())),
            },
        ));
    } else {
        server.reply(Response::new_ok(req_id, Value::Null));
    }
    Ok(())
}
