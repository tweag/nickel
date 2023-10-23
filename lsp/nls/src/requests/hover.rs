use lsp_server::{RequestId, Response, ResponseError};
use lsp_types::{Hover, HoverContents, HoverParams, LanguageString, MarkedString, Range};
use nickel_lang_core::{
    combine::Combine,
    identifier::Ident,
    position::RawSpan,
    term::{record::FieldMetadata, LabeledType, RichTerm, Term, UnaryOp},
    typ::Type,
};
use serde_json::Value;

use crate::{
    cache::CacheExt,
    diagnostic::LocationCompat,
    field_walker::{FieldHaver, FieldResolver},
    identifier::LocIdent,
    server::Server,
};

#[derive(Debug, Default)]
struct HoverData {
    values: Vec<RichTerm>,
    metadata: Vec<FieldMetadata>,
    span: Option<RawSpan>,
    ty: Option<Type>,
}

impl Combine for HoverData {
    fn combine(mut left: Self, right: Self) -> Self {
        left.values.extend_from_slice(&right.values);
        left.metadata.extend_from_slice(&right.metadata);
        left.ty = left.ty.or(right.ty);
        left.span = left.span.or(right.span);
        left
    }
}

fn annotated_contracts(rt: &RichTerm) -> &[LabeledType] {
    match rt.as_ref() {
        Term::Annotated(annot, _) => &annot.contracts,
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
    parents: Vec<FieldHaver>,
    ident: Ident,
) -> (Vec<RichTerm>, Vec<FieldMetadata>) {
    let mut values = Vec::new();
    let mut metadata = Vec::new();
    for parent in parents {
        if let FieldHaver::RecordTerm(r) = parent {
            if let Some(field) = r.fields.get(&ident) {
                values.extend(field.value.iter().cloned());
                metadata.push(field.metadata.clone());
            }
        }
    }
    (values, metadata)
}

fn ident_hover(ident: LocIdent, server: &Server) -> Option<HoverData> {
    let ty = server.analysis.get_type_for_ident(&ident).cloned();
    let span = ident.pos.into_opt()?;
    let mut ret = HoverData {
        values: Vec::new(),
        metadata: Vec::new(),
        span: Some(span),
        ty,
    };

    if let Some(def) = server.analysis.get_def(&ident) {
        let resolver = FieldResolver::new(server);
        if let Some(((last, path), val)) = def.path.split_last().zip(def.value.as_ref()) {
            let parents = resolver.resolve_term_path(val, path.iter().copied());
            let (values, metadata) = values_and_metadata_from_field(parents, *last);
            ret.values = values;
            ret.metadata = metadata;
        } else if def.path.is_empty() {
            ret.values.extend(def.value.iter().cloned());
        }
    }

    Some(ret)
}

fn term_hover(rt: &RichTerm, server: &Server) -> Option<HoverData> {
    let ty = server.analysis.get_type(rt).cloned();
    let span = rt.pos.into_opt();

    match rt.as_ref() {
        Term::Op1(UnaryOp::StaticAccess(id), parent) => {
            let resolver = FieldResolver::new(server);
            let parents = resolver.resolve_term(parent);
            let (values, metadata) = values_and_metadata_from_field(parents, id.ident());
            Some(HoverData {
                values,
                metadata,
                span,
                ty,
            })
        }
        _ => Some(HoverData {
            values: vec![rt.clone()],
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
        .cache
        .position(&params.text_document_position_params)?;

    let ident_hover_data = server
        .lookup_ident_by_position(pos)?
        .and_then(|ident| ident_hover(ident, server));

    let term_hover_data = server
        .lookup_term_by_position(pos)?
        .and_then(|rt| term_hover(rt, server));

    let hover_data = Combine::combine(ident_hover_data, term_hover_data);

    if let Some(hover) = hover_data {
        let mut contents = Vec::new();

        if let Some(ty) = hover.ty {
            contents.push(nickel_string(ty.to_string()));
        } else {
            // Unclear whether it's useful to report `Dyn` all the
            // time, but it matches the old behavior.
            contents.push(nickel_string("Dyn".to_string()));
        }

        let mut contracts: Vec<_> = hover
            .metadata
            .iter()
            .flat_map(|m| &m.annotation.contracts)
            .chain(hover.values.iter().flat_map(annotated_contracts))
            .map(|contract| contract.label.typ.to_string())
            .collect();

        contracts.sort();
        contracts.dedup();

        contents.extend(contracts.into_iter().map(nickel_string));

        // Not sure how to do documentation merging yet, so pick the first non-empty one.
        let doc = hover.metadata.iter().find_map(|m| m.doc.as_ref());
        if let Some(doc) = doc {
            contents.push(MarkedString::String(doc.to_owned()));
        }

        server.reply(Response::new_ok(
            req_id,
            Hover {
                contents: HoverContents::Array(contents),
                range: hover
                    .span
                    .map(|s| Range::from_span(&s, server.cache.files())),
            },
        ));
    } else {
        server.reply(Response::new_ok(req_id, Value::Null));
    }
    Ok(())
}
