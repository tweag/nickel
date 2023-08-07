use std::collections::{hash_map::Entry, HashMap};

use lsp_types::{CompletionItem, CompletionItemKind, Documentation, MarkupContent, MarkupKind};
use nickel_lang_core::{
    identifier::Ident,
    term::{record::FieldMetadata, BinaryOp, RichTerm, Term},
};

use crate::{linearization::completed::Completed, server::Server};

/// The position at which a something is defined.
#[derive(Clone, Debug)]
pub struct Def {
    /// The identifier of the definition. (TODO: example)
    pub ident: Ident,
    /// The value assigned by the definition, if there is one.
    pub value: Option<RichTerm>,
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

    pub fn to_completion_item(&self) -> CompletionItem {
        /// Attach quotes to a non-ASCII string
        fn adjust_name(name: &str) -> String {
            if name.is_ascii() {
                String::from(name)
            } else {
                format!("\"{name}\"")
            }
        }

        CompletionItem {
            label: adjust_name(self.ident.label()),
            detail: self.detail(),
            kind: Some(CompletionItemKind::Property),
            documentation: self.doc(),
            ..Default::default()
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct FieldDefs {
    // The key to this map is really a Symbol rather than an Ident. Since the interner is not
    // public, we use an Ident that has had its location removed.
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
        let defs = fields.fields.remove(&id.without_pos()).unwrap_or_default();
        fields.fields.clear();

        for rt in defs.into_iter().filter_map(|d| d.value) {
            fields = fields.merge_from(FieldDefs::resolve(&rt, linearization, server));
        }
    }

    fields
        .fields
        .into_values()
        .flat_map(|defs| defs.into_iter())
}

impl FieldDefs {
    pub fn resolve(rt: &RichTerm, linearization: &Completed, server: &Server) -> FieldDefs {
        match rt.term.as_ref() {
            Term::Record(data) | Term::RecRecord(data, ..) => {
                let fields = data
                    .fields
                    .iter()
                    .map(|(&ident, field)| {
                        (
                            ident.without_pos(),
                            vec![Def {
                                ident,
                                value: field.value.clone(),
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
