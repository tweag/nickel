use std::{cell::RefCell, collections::HashSet};

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

/// A `Container` is something that has elements.
///
/// The elements could have names (e.g. in a record) or not (e.g. in an array).
///
/// You can use a [`FieldResolver`] to resolve terms, or terms with paths, to `Container`s.
#[derive(Clone, Debug, PartialEq)]
pub enum Container {
    RecordTerm(RecordData),
    Dict(Type),
    RecordType(RecordRows),
    Array(Type),
}

/// A `ChildId` identifies an element of a container.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EltId {
    /// A named element, for example of an array.
    Ident(Ident),
    /// An array element.
    ArrayElt,
}

impl From<Ident> for EltId {
    fn from(id: Ident) -> Self {
        EltId::Ident(id)
    }
}

impl Container {
    /// If this `Container` has a field named `id`, returns its value.
    fn get(&self, id: EltId) -> Option<FieldContent> {
        match (self, id) {
            (Container::RecordTerm(data), EltId::Ident(id)) => data
                .fields
                .get(&id)
                .map(|field| FieldContent::RecordField(field.clone())),
            (Container::Dict(ty), EltId::Ident(_)) => Some(FieldContent::Type(ty.clone())),
            (Container::RecordType(rows), EltId::Ident(id)) => rows
                .find_path(&[id])
                .map(|row| FieldContent::Type(row.typ.clone())),
            (Container::Array(ty), EltId::ArrayElt) => Some(FieldContent::Type(ty.clone())),
            _ => None,
        }
    }

    /// If this `Container` is a record term, try to retrieve the field named `id`.
    fn get_field_and_loc(&self, id: Ident) -> Option<(LocIdent, &Field)> {
        match self {
            Container::RecordTerm(data) => data
                .fields
                .get_key_value(&id)
                .map(|(id, fld)| (LocIdent::from(*id), fld)),
            _ => None,
        }
    }

    pub fn get_definition_pos(&self, id: Ident) -> Option<LocIdent> {
        match self {
            Container::RecordTerm(data) => data
                .fields
                .get_key_value(&id)
                .map(|(id, _field)| (*id).into()),
            Container::RecordType(rows) => rows.find_path(&[id]).map(|r| r.id.into()),
            Container::Dict(_) => None,
            Container::Array(_) => None,
        }
    }

    /// Returns all fields in this `Container`, rendered as LSP completion items.
    pub fn completion_items(&self) -> impl Iterator<Item = CompletionItem> + '_ {
        match self {
            Container::RecordTerm(data) => {
                let iter = data.fields.iter().map(|(id, val)| CompletionItem {
                    label: ident_quoted(id),
                    detail: metadata_detail(&val.metadata),
                    kind: Some(CompletionItemKind::Property),
                    documentation: metadata_doc(&val.metadata),
                    ident: Some((*id).into()),
                });
                Box::new(iter)
            }
            Container::Dict(_) | Container::Array(_) => {
                Box::new(std::iter::empty()) as Box<dyn Iterator<Item = _>>
            }
            Container::RecordType(rows) => {
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

/// [`Container`]s can have fields that are either record fields or types.
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

/// The definition site of an identifier.
#[derive(Clone, Debug, PartialEq)]
pub enum Def {
    /// A definition site that's a let binding (possibly a pattern binding).
    Let {
        ident: LocIdent,
        /// The right hand side of the let binding. Note that in the case of a
        /// pattern binding, this may not be the value that's actually bound to
        /// `ident`. (See `path`.)
        value: RichTerm,
        /// The path that `ident` refers to in `value`.
        path: Vec<Ident>,
    },
    /// An identifier bound as the argument to a function.
    ///
    /// Note that this can also be a pattern binding (and therefore come with
    /// an associated path) but because we don't track any bound value here, we
    /// don't need to track the path either.
    Fn { ident: LocIdent },
    /// An identifier bound as a record field.
    Field {
        ident: LocIdent,
        value: Option<RichTerm>,
        record: RichTerm,
        metadata: FieldMetadata,
    },
}

impl Def {
    pub fn ident(&self) -> LocIdent {
        match self {
            Def::Let { ident, .. } | Def::Fn { ident, .. } | Def::Field { ident, .. } => *ident,
        }
    }

    pub fn metadata(&self) -> Option<&FieldMetadata> {
        match self {
            Def::Field { metadata, .. } => Some(metadata),
            _ => None,
        }
    }

    pub fn parent_record(&self) -> Option<&RichTerm> {
        match self {
            Def::Field { record, .. } => Some(record),
            _ => None,
        }
    }

    pub fn value(&self) -> Option<&RichTerm> {
        match self {
            Def::Let { value, .. } => Some(value),
            Def::Field { value, .. } => value.as_ref(),
            Def::Fn { .. } => None,
        }
    }

    pub fn path(&self) -> &[Ident] {
        match self {
            Def::Let { path, .. } => path.as_slice(),
            _ => &[],
        }
    }
}

impl Def {
    pub fn completion_item(&self) -> CompletionItem {
        CompletionItem {
            label: ident_quoted(&self.ident().into()),
            detail: self.metadata().and_then(metadata_detail),
            kind: Some(CompletionItemKind::Property),
            documentation: self.metadata().and_then(metadata_doc),
            ..Default::default()
        }
    }
}

/// Contains the context needed to resolve fields.
#[derive(Clone)]
pub struct FieldResolver<'a> {
    server: &'a Server,

    // Most of our analysis moves "down" the AST and so can't get stuck in a loop.
    // Variable resolution is an exception, however, and so we protect against
    // loops by recording the ids that we are currently resolving and refusing to
    // resolve them again.
    blackholed_ids: RefCell<HashSet<LocIdent>>,
}

impl<'a> FieldResolver<'a> {
    pub fn new(server: &'a Server) -> Self {
        Self {
            server,
            blackholed_ids: Default::default(),
        }
    }

    /// Resolve a record path iteratively.
    ///
    /// Returns all the field-having objects that the final path element refers to.
    pub fn resolve_term_path(
        &self,
        rt: &RichTerm,
        path: impl Iterator<Item = EltId>,
    ) -> Vec<Container> {
        let mut fields = self.resolve_term(rt);

        for id in path {
            let values = fields
                .iter()
                .filter_map(|haver| haver.get(id))
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

    /// Find the "cousins" of this definition.
    ///
    /// When resolving references, we often want to take merged records into
    /// account. For example, when looking at the first definition of `foo` in
    ///
    /// ```nickel
    /// { foo = 1 } | { foo | Number | doc "blah blah" }
    /// ```
    ///
    /// we often want to access the information in the second "foo". We call these two
    /// `foo`s "cousin" definitions because they look like cousins in the AST. Note
    /// that these can also happen at arbitrary depths, like the two `foo`s in
    ///
    /// ```nickel
    /// { bar = { foo = 1 } } | { bar | { foo | Number | doc "blah blah" } }
    /// ```
    pub fn get_cousin_defs(&self, def: &Def) -> Vec<(LocIdent, Field)> {
        let mut ret = Vec::new();
        if let Some(parent) = def.parent_record() {
            if let Some(mut ancestors) = self.server.analysis.get_parent_chain(parent) {
                while let Some(ancestor) = ancestors.next_merge() {
                    // We're traversing up the tree starting at the parent, so this is the
                    // path to the parent (not the original def).
                    if let Some(parent_path) = ancestors.path() {
                        let uncles = self.resolve_term_path(&ancestor, parent_path.iter().copied());
                        ret.extend(
                            uncles
                                .iter()
                                .filter_map(|uncle| uncle.get_field_and_loc(def.ident().ident))
                                .map(|(loc, fld)| (loc, fld.clone())),
                        )
                    }
                }
            }
        }
        ret
    }

    fn resolve_def_with_path(&self, def: &Def) -> Vec<Container> {
        let mut fields = Vec::new();

        if let Some(val) = def.value() {
            fields.extend_from_slice(
                &self.resolve_term_path(val, def.path().iter().copied().map(EltId::Ident)),
            )
        }
        if let Some(meta) = def.metadata() {
            fields.extend(self.resolve_annot(&meta.annotation));
        }

        for (_, field) in self.get_cousin_defs(def) {
            fields.extend(self.resolve_annot(&field.metadata.annotation));
            if let Some(val) = &field.value {
                fields.extend(self.resolve_term(val));
            }
        }

        fields
    }

    fn resolve_annot(&'a self, annot: &'a TypeAnnotation) -> impl Iterator<Item = Container> + 'a {
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
    pub fn resolve_term(&self, rt: &RichTerm) -> Vec<Container> {
        let term_fields = match rt.term.as_ref() {
            Term::Record(data) | Term::RecRecord(data, ..) => {
                vec![Container::RecordTerm(data.clone())]
            }
            Term::Var(id) => {
                let id = LocIdent::from(*id);
                if self.blackholed_ids.borrow_mut().insert(id) {
                    let ret = self
                        .server
                        .analysis
                        .get_def(&id)
                        .map(|def| {
                            log::info!("got def {def:?}");

                            self.resolve_def_with_path(def)
                        })
                        .unwrap_or_else(|| {
                            log::info!("no def for {id:?}");
                            Default::default()
                        });
                    self.blackholed_ids.borrow_mut().remove(&id);
                    ret
                } else {
                    log::warn!("detected recursion when resolving {id:?}");
                    Vec::new()
                }
            }
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
                self.resolve_term_path(term, std::iter::once(id.ident().into()))
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

    fn resolve_type(&self, typ: &Type) -> Vec<Container> {
        match &typ.typ {
            TypeF::Record(rows) => vec![Container::RecordType(rows.clone())],
            TypeF::Dict { type_fields, .. } => vec![Container::Dict(type_fields.as_ref().clone())],
            TypeF::Array(elt_ty) => vec![Container::Array(elt_ty.as_ref().clone())],
            TypeF::Flat(rt) => self.resolve_term(rt),
            _ => Default::default(),
        }
    }
}

fn combine<T>(mut left: Vec<T>, mut right: Vec<T>) -> Vec<T> {
    left.append(&mut right);
    left
}
