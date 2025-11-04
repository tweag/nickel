use std::borrow::Cow;

use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{Hover, HoverContents, HoverParams, LanguageString, MarkedString, Range};

use nickel_lang_core::{
    bytecode::ast::{Ast, Node, primop::PrimOp, record::FieldMetadata, typ::Type},
    combine::Combine,
    identifier::Ident,
    position::RawSpan,
    typecheck::AnnotSeqRef,
};

use serde_json::Value;

use crate::{
    diagnostic::LocationCompat,
    field_walker::{Def, FieldResolver, Record},
    identifier::LocIdent,
    server::Server,
    world::World,
};

/// De-duplicate a vec without changing the order. The first instance of each unique element will
/// be kept.
fn dedup<T: std::hash::Hash + Eq + Clone>(xs: &mut Vec<T>) {
    use std::collections::HashSet;

    let mut seen = HashSet::new();
    // Clone is needed because the signature of retain doesn't let us keep the reference.
    xs.retain(|x| seen.insert(x.clone()));
}

#[derive(Debug, Default)]
struct HoverData<'ast> {
    /// A list of values contributing to the definition of the hovered term. Values are currently
    /// used only for their annotation, in order to aggregate even more type information.
    values: Vec<&'ast Ast<'ast>>,
    /// A list of metadata contributing to the definition of the hovered term.
    metadata: Vec<Cow<'ast, FieldMetadata<'ast>>>,
    span: Option<RawSpan>,
    /// The distinguished type of the hovered term, if any. This is the one inferred by the
    /// typechecker.
    ty: Option<&'ast Type<'ast>>,
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

fn values_and_metadata_from_field(
    parents: Vec<Record<'_>>,
    ident: Ident,
) -> (Vec<&Ast<'_>>, Vec<Cow<'_, FieldMetadata<'_>>>) {
    let mut values = Vec::new();
    let mut metadata = Vec::new();

    for parent in parents {
        for piece in parent.field_pieces(ident) {
            // Note that for hover, we are only interested in the annotations. We can ignore all
            // definition piece with a path longer than 1, as any annotation here would apply to
            // the last field of path, and not `ident`, which is the first. This is luckily taken
            // care of for us by `value()` and `metadata()` methods.
            values.extend(piece.value());
            metadata.extend(piece.metadata().into_iter().map(Cow::Borrowed));
        }
    }
    (values, metadata)
}

fn ident_hover(ident: LocIdent, world: &World) -> Option<HoverData<'_>> {
    let ty = world.analysis_reg.get_ident_type(&ident);
    let span = ident.pos.into_opt()?;
    let mut ret = HoverData {
        values: Vec::new(),
        metadata: Vec::new(),
        span: Some(span),
        ty,
    };

    if let Some(def) = world.analysis_reg.get_def(&ident) {
        let resolver = FieldResolver::new(world);

        if let Def::Field {
            path_in_record: path,
            record,
            ..
        } = def
        {
            // If the path is non-empty, we need to index into the cousins of the parent record of
            // this definition using the same prefix. For example, when hovering on `x` in `{ r.x =
            // 1}`, we want to index at field `r` in the potential cousins.
            let cousins = if let Some((last, prefix)) = path.split_last() {
                resolver.cousin_defs_at_path(record, prefix.iter().cloned(), *last)
            } else {
                resolver.cousin_defs(def)
            };

            if cousins.is_empty() {
                ret.values.extend(def.values());
                ret.metadata.extend(def.metadata());
            } else {
                for cousin in cousins {
                    ret.values.extend(cousin.value());
                    ret.metadata.extend(cousin.metadata().map(Cow::Borrowed));
                }
            }

            ret.metadata.extend(def.metadata());
        } else {
            let path = def.path();

            // This corresponds to a pattern case where the ident indexes into the value.
            if let Some((last, prefix)) = path.split_last() {
                for value in def.values() {
                    let parents = resolver.resolve_path(value, prefix.iter().copied());
                    let (values, metadata) = values_and_metadata_from_field(parents, *last);
                    ret.values.extend(values);
                    ret.metadata.extend(metadata);
                }
            } else {
                ret.values.extend(def.values());
                ret.metadata.extend(def.metadata());
            }
        }
    }

    Some(ret)
}

fn term_hover<'ast>(ast: &'ast Ast<'ast>, world: &'ast World) -> Option<HoverData<'ast>> {
    let ty = world.analysis_reg.get_type(ast);
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
        .position(&params.text_document_position_params)?;

    let ident_hover_data = server
        .world
        .ident_at(pos)?
        .and_then(|ident| ident_hover(ident, &server.world));

    let ast = server.world.ast_at(pos)?;
    let ast_hover_data = ast.and_then(|rt| term_hover(rt, &server.world));

    // We combine the hover information from the term (which can have better type information)
    // and the ident (which can have better metadata), but only when hovering over a `Var`.
    // In general, the term and the ident can have different meanings (like when hovering over
    // the `x` in `let x = ... in y`) and so it would be confusing to combine them.
    let hover_data = if matches!(ast.as_ref().map(|ast| &ast.node), Some(Node::Var(_))) {
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
                    .flat_map(|ast| annotated_contracts(ast))
                    .map(|contract| contract.to_string()),
            )
            .collect();
        dedup(&mut annotations);

        let ty = hover
            .ty
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
                    .and_then(|s| Range::from_span(&s, server.world.sources.files())),
            },
        ));
    } else {
        server.reply(Response::new_ok(req_id, Value::Null));
    }
    Ok(())
}
