use lsp_types::{CompletionItemKind, Documentation, MarkupContent, MarkupKind};
use nickel_lang_core::{
    identifier::Ident,
    pretty::ident_quoted,
    term::{
        record::{Field, FieldMetadata, RecordData},
        BinaryOp, RichTerm, Term, TypeAnnotation, UnaryOp,
    },
    typ::{RecordRows, RecordRowsIteratorItem, Type, TypeF},
};

use crate::{identifier::LocIdent, requests::completion::CompletionItem, server::Server};

/// A `FieldHaver` is something that... has fields.
///
/// You can use a [`FieldResolver`] to resolve terms, or terms with paths, to `FieldHaver`s.
#[derive(Clone, Debug, PartialEq)]
pub enum FieldHaver {
    RecordTerm(RecordData),
    Dict(Type),
    RecordType(RecordRows),
}

impl FieldHaver {
    /// If this `FieldHaver` has a field named `id`, returns its value.
    fn get(&self, id: Ident) -> Option<FieldContent> {
        match self {
            FieldHaver::RecordTerm(data) => data
                .fields
                .get(&id)
                .map(|field| FieldContent::RecordField(field.clone())),
            FieldHaver::Dict(ty) => Some(FieldContent::Type(ty.clone())),
            FieldHaver::RecordType(rows) => rows
                .find_path(&[id])
                .map(|row| FieldContent::Type(row.typ.clone())),
        }
    }

    pub fn get_definition_pos(&self, id: Ident) -> Option<LocIdent> {
        match self {
            FieldHaver::RecordTerm(data) => data
                .fields
                .get_key_value(&id)
                .map(|(id, _field)| (*id).into()),
            FieldHaver::RecordType(rows) => rows.find_path(&[id]).map(|r| r.id.into()),
            FieldHaver::Dict(_) => None,
        }
    }

    /// Returns all fields in this `FieldHaver`, rendered as LSP completion items.
    pub fn completion_items(&self) -> impl Iterator<Item = CompletionItem> + '_ {
        match self {
            FieldHaver::RecordTerm(data) => {
                let iter = data.fields.iter().map(|(id, val)| CompletionItem {
                    label: ident_quoted(id),
                    detail: metadata_detail(&val.metadata),
                    kind: Some(CompletionItemKind::Property),
                    documentation: metadata_doc(&val.metadata),
                    ident: Some((*id).into()),
                });
                Box::new(iter)
            }
            FieldHaver::Dict(_) => Box::new(std::iter::empty()) as Box<dyn Iterator<Item = _>>,
            FieldHaver::RecordType(rows) => {
                let iter = rows.iter().filter_map(|r| match r {
                    RecordRowsIteratorItem::TailDyn => None,
                    RecordRowsIteratorItem::TailVar(_) => None,
                    RecordRowsIteratorItem::Row(r) => Some(CompletionItem {
                        label: ident_quoted(&r.id),
                        kind: Some(CompletionItemKind::Property),
                        detail: Some(r.typ.to_string()),
                        ..Default::default()
                    }),
                });
                Box::new(iter)
            }
        }
    }
}

/// [`FieldHaver`]s can have fields that are either record fields or types.
#[derive(Clone, Debug, PartialEq)]
enum FieldContent {
    RecordField(Field),
    Type(Type),
}

fn metadata_doc(m: &FieldMetadata) -> Option<Documentation> {
    let doc = m.doc.as_ref()?;
    Some(Documentation::MarkupContent(MarkupContent {
        kind: MarkupKind::Markdown,
        value: doc.clone(),
    }))
}

// If the field is annotated, returns its type annotation (preferred) or its
// contract annotation (fallback).
fn metadata_detail(m: &FieldMetadata) -> Option<String> {
    m.annotation
        .typ
        .as_ref()
        .map(|ty| ty.typ.to_string())
        .or_else(|| m.annotation.contracts_to_string())
}

/// A definition whose value might need to be accessed through a path.
///
/// This arises because of pattern bindings.
///  For example, in
///
/// ```text
/// let { a = { b } } = val in ...
/// ```
///
/// the name `b` is bound to the term `val` at the path `[a, b]`.
///
/// Semantically, a definition with a path is pretty much the same as
/// a definition whose value is a `Op1(StaticAccess, Op1(StaticAccess, ...))`.
#[derive(Clone, Debug, PartialEq)]
pub struct DefWithPath {
    /// The identifier at the definition site.
    pub ident: LocIdent,
    /// The value assigned by the definition, if there is one. If the definition
    /// was made by a `let` binding, there will be a value; if it was made in a
    /// function definition, there will not be a value.
    ///
    /// For example, in `{ foo = 1 }`, this will point at the `1`.
    pub value: Option<RichTerm>,
    /// The path within the value that this binding refers to.
    pub path: Vec<Ident>,
    pub metadata: Option<FieldMetadata>,
}

impl DefWithPath {
    pub fn completion_item(&self) -> CompletionItem {
        CompletionItem {
            label: ident_quoted(&self.ident.into()),
            detail: self.metadata.as_ref().and_then(metadata_detail),
            kind: Some(CompletionItemKind::Property),
            documentation: self.metadata.as_ref().and_then(metadata_doc),
            ..Default::default()
        }
    }
}

#[cfg(test)]
impl DefWithPath {
    pub fn path(&self) -> &[Ident] {
        &self.path
    }

    pub fn value(&self) -> Option<&RichTerm> {
        self.value.as_ref()
    }
}

/// Contains the context needed to resolve fields.
#[derive(Clone)]
pub struct FieldResolver<'a> {
    server: &'a Server,
}

impl<'a> FieldResolver<'a> {
    pub fn new(server: &'a Server) -> Self {
        Self { server }
    }

    /// Resolve a record path iteratively.
    ///
    /// Returns all the field-having objects that the final path element refers to.
    pub fn resolve_term_path(&self, rt: &RichTerm, mut path: &[Ident]) -> Vec<FieldHaver> {
        let mut fields = self.resolve_term(rt);

        while let Some((id, tail)) = path.split_first() {
            path = tail;
            let values = fields
                .iter()
                .filter_map(|haver| haver.get(*id))
                .collect::<Vec<_>>();
            fields.clear();

            for value in values {
                match value {
                    FieldContent::RecordField(field) => {
                        if let Some(val) = &field.value {
                            fields.extend_from_slice(&self.resolve_term(val))
                        }
                        fields.extend(self.resolve_annot(&field.metadata.annotation));
                    }
                    FieldContent::Type(ty) => {
                        fields.extend_from_slice(&self.resolve_type(&ty));
                    }
                }
            }
        }

        fields
    }

    fn resolve_def_with_path(&self, def: &DefWithPath) -> Vec<FieldHaver> {
        let mut fields = Vec::new();

        if let Some(val) = &def.value {
            fields.extend_from_slice(&self.resolve_term_path(val, &def.path))
        }
        if let Some(meta) = &def.metadata {
            fields.extend(self.resolve_annot(&meta.annotation));
        }
        fields
    }

    fn resolve_annot(&'a self, annot: &'a TypeAnnotation) -> impl Iterator<Item = FieldHaver> + 'a {
        annot
            .contracts
            .iter()
            .chain(annot.typ.iter())
            .flat_map(|lty| self.resolve_type(&lty.typ).into_iter())
    }

    /// Find all the fields that are defined on a term.
    ///
    /// This a best-effort thing; it doesn't do full evaluation but it has some reasonable
    /// heuristics. For example, it knows that the fields defined on a merge of two records
    /// are the fields defined on either record.
    pub fn resolve_term(&self, rt: &RichTerm) -> Vec<FieldHaver> {
        let term_fields = match rt.term.as_ref() {
            Term::Record(data) | Term::RecRecord(data, ..) => {
                vec![FieldHaver::RecordTerm(data.clone())]
            }
            Term::Var(id) => self
                .server
                .analysis
                .get_def(&(*id).into())
                .map(|def| {
                    log::info!("got def {def:?}");

                    self.resolve_def_with_path(def)
                })
                .unwrap_or_else(|| {
                    log::info!("no def for {id:?}");
                    Default::default()
                }),
            //.unwrap_or_default(),
            Term::ResolvedImport(file_id) => self
                .server
                .cache
                .get_ref(*file_id)
                .map(|term| self.resolve_term(term))
                .unwrap_or_default(),
            Term::Op2(BinaryOp::Merge(_), t1, t2) => {
                combine(self.resolve_term(t1), self.resolve_term(t2))
            }
            Term::Let(_, _, body, _) | Term::LetPattern(_, _, _, body) => self.resolve_term(body),
            Term::Op1(UnaryOp::StaticAccess(id), term) => {
                self.resolve_term_path(term, &[id.ident()])
            }
            Term::Annotated(annot, term) => {
                let defs = self.resolve_annot(annot);
                defs.chain(self.resolve_term(term)).collect()
            }
            Term::Type(typ) => self.resolve_type(typ),
            _ => Default::default(),
        };

        let typ_fields = if let Some(typ) = self.server.analysis.get_type(rt) {
            log::info!("got inferred type {typ:?}");
            self.resolve_type(typ)
        } else {
            Vec::new()
        };

        combine(term_fields, typ_fields)
    }

    fn resolve_type(&self, typ: &Type) -> Vec<FieldHaver> {
        match &typ.typ {
            TypeF::Record(rows) => vec![FieldHaver::RecordType(rows.clone())],
            TypeF::Dict { type_fields, .. } => vec![FieldHaver::Dict(type_fields.as_ref().clone())],
            TypeF::Flat(rt) => self.resolve_term(rt),
            _ => Default::default(),
        }
    }
}

fn combine<T>(mut left: Vec<T>, mut right: Vec<T>) -> Vec<T> {
    left.append(&mut right);
    left
}
