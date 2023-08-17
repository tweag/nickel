use std::collections::{hash_map::Entry, HashMap};

use lsp_types::{CompletionItem, CompletionItemKind, Documentation, MarkupContent, MarkupKind};
use nickel_lang_core::{
    identifier::Ident,
    pretty::ident_quoted,
    term::{record::FieldMetadata, BinaryOp, RichTerm, Term},
};

use crate::{identifier::LocIdent, linearization::completed::Completed, server::Server};

/// A term and a path.
///
/// This is morally equivalent to (but a more convenient representation than)
/// `Op1(StaticAccess("field2"), Op1(StaticAccess("field1"), term))`.
#[derive(Clone, Debug, PartialEq)]
pub struct TermAtPath {
    pub term: RichTerm,
    /// A path of identifiers, in left-to-right order.
    ///
    /// So, for `term.x.y.z`, this will be `vec!["x", "y", "z"]`.
    pub path: Vec<Ident>,
}

impl TermAtPath {
    fn resolve_terms(&self, linearization: &Completed, server: &Server) -> Vec<RichTerm> {
        if self.path.is_empty() {
            vec![self.term.clone()]
        } else {
            resolve_path(&self.term, &self.path, linearization, server)
                .flat_map(|def| {
                    def.value
                        .into_iter()
                        .flat_map(|val| val.resolve_terms(linearization, server).into_iter())
                })
                .collect()
        }
    }
}

impl From<RichTerm> for TermAtPath {
    fn from(term: RichTerm) -> Self {
        Self {
            term,
            path: Vec::new(),
        }
    }
}

/// The position at which something is defined.
#[derive(Clone, Debug, PartialEq)]
pub struct Def {
    /// The identifier at the definition site.
    pub ident: LocIdent,
    /// The value assigned by the definition, if there is one.
    ///
    /// For example, in `{ foo = 1 }`, this could point at the `1`.
    ///
    /// Because of pattern bindings, there could also be a path associated with
    /// the value. For example, in
    ///
    /// ```text
    /// let { a = { b } } = val in ...
    /// ```
    ///
    /// the name `b` is bound to the term `val` at the path `[a, b]`.
    pub value: Option<TermAtPath>,
    /// Field metadata.
    pub metadata: Option<FieldMetadata>,
}

impl Def {
    fn doc(&self) -> Option<Documentation> {
        let doc = self.metadata.as_ref()?.doc.as_ref()?;
        Some(Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: doc.clone(),
        }))
    }

    // If the field is annotated, returns its type annotation (preferred) or its
    // contract annotation (fallback).
    fn detail(&self) -> Option<String> {
        self.metadata
            .as_ref()
            .and_then(|FieldMetadata { annotation, .. }| {
                annotation
                    .typ
                    .as_ref()
                    .map(|ty| ty.typ.to_string())
                    .or_else(|| annotation.contracts_to_string())
            })
    }

    /// Creates a completion item from this definition.
    pub fn to_completion_item(&self) -> CompletionItem {
        CompletionItem {
            label: ident_quoted(&self.ident.into()),
            detail: self.detail(),
            kind: Some(CompletionItemKind::Property),
            documentation: self.doc(),
            ..Default::default()
        }
    }
}

/// A map from identifiers to the defs that they refer to.
#[derive(Clone, Debug, Default)]
struct FieldDefs {
    fields: HashMap<Ident, Vec<Def>>,
}

/// Resolve a record path iteratively, returning the names of all the fields defined on the final path element.
pub fn resolve_path<'a>(
    rt: &'a RichTerm,
    mut path: &'a [Ident],
    linearization: &Completed,
    server: &Server,
) -> impl Iterator<Item = Def> {
    let mut fields = FieldDefs::resolve(rt, linearization, server);

    while let Some((id, tail)) = path.split_first() {
        path = tail;
        let defs = fields.fields.remove(id).unwrap_or_default();
        fields.fields.clear();

        for def in defs.into_iter().filter_map(|d| d.value) {
            for rt in def.resolve_terms(linearization, server) {
                fields = fields.merge_from(FieldDefs::resolve(&rt, linearization, server));
            }
        }
    }

    fields
        .fields
        .into_values()
        .flat_map(|defs| defs.into_iter())
}

impl FieldDefs {
    /// Find all the fields that are defined on a term.
    ///
    /// This a best-effort thing; it doesn't do full evaluation but it has some reasonable
    /// heuristics. For example, it knows that the fields defined on a merge of two records
    /// are the fields defined on either record.
    fn resolve(rt: &RichTerm, linearization: &Completed, server: &Server) -> FieldDefs {
        match rt.term.as_ref() {
            Term::Record(data) | Term::RecRecord(data, ..) => {
                let fields = data
                    .fields
                    .iter()
                    .map(|(&ident, field)| {
                        (
                            ident.symbol(),
                            vec![Def {
                                ident: ident.into(),
                                value: field.value.clone().map(From::from),
                                metadata: Some(field.metadata.clone()),
                            }],
                        )
                    })
                    .collect();
                FieldDefs { fields }
            }
            Term::Var(_) => linearization
                .lookup_usage(rt)
                .map(|def| FieldDefs::resolve(&def, linearization, server))
                .unwrap_or_default(),
            Term::ResolvedImport(file_id) => server
                .cache
                .get_ref(*file_id)
                .map(|term| FieldDefs::resolve(term, linearization, server))
                .unwrap_or_default(),
            Term::Op2(BinaryOp::Merge(_), t1, t2) => FieldDefs::resolve(t1, linearization, server)
                .merge_from(FieldDefs::resolve(t2, linearization, server)),
            _ => Default::default(),
        }
    }

    fn merge_from(mut self, other: FieldDefs) -> FieldDefs {
        for (ident, defs) in other.fields {
            match self.fields.entry(ident) {
                Entry::Occupied(oc) => {
                    oc.into_mut().extend_from_slice(&defs);
                }
                Entry::Vacant(vac) => {
                    vac.insert(defs);
                }
            }
        }
        self
    }
}
