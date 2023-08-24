use std::collections::{hash_map::Entry, HashMap};

use lsp_types::{CompletionItem, CompletionItemKind, Documentation, MarkupContent, MarkupKind};
use nickel_lang_core::{
    identifier::Ident,
    pretty::ident_quoted,
    term::{record::FieldMetadata, BinaryOp, RichTerm, Term},
};

use crate::{identifier::LocIdent, server::Server, usage::Environment};

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

impl TermAtPath {}

impl From<RichTerm> for TermAtPath {
    fn from(term: RichTerm) -> Self {
        Self {
            term,
            path: Vec::new(),
        }
    }
}

/// The position at which a name is defined (possibly also with a value).
#[derive(Clone, Debug, PartialEq)]
pub struct Def {
    /// The identifier at the definition site.
    pub ident: LocIdent,
    /// The value assigned by the definition, if there is one. If the definition
    /// was made by a `let` binding, there will be a value; if it was made in a
    /// function definition, there will not be a value.
    ///
    /// For example, in `{ foo = 1 }`, this will point at the `1`.
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

impl DefWithPath {
    fn resolve_terms(&self, env: &Environment, server: &Server) -> Vec<Def> {
        match &self.value {
            Some(val) if !val.path.is_empty() => {
                // Calling `resolve_path` on x with path [foo, bar] returns all
                // the fields *in* x.foo.bar. We want the value(s) of x.foo.bar,
                // so use `resolve_path` to get x.foo and then find the bars in it.
                // unwrap: we just checked the path is non-empty
                let (last, path) = val.path.split_last().unwrap();
                FieldDefs::resolve_path(&val.term, path, env, server)
                    .fields
                    .remove(last)
                    .unwrap_or_default()
            }
            _ => {
                vec![Def {
                    ident: self.ident,
                    value: self.value.as_ref().map(|tp| tp.term.clone()),
                    metadata: self.metadata.clone(),
                }]
            }
        }
    }
}

/// A map from identifiers to the defs that they refer to.
#[derive(Clone, Debug, Default)]
pub struct FieldDefs {
    fields: HashMap<Ident, Vec<Def>>,
}

impl FieldDefs {
    /// Resolve a record path iteratively, returning all the fields defined on the final path element.
    pub fn resolve_path<'a>(
        rt: &'a RichTerm,
        mut path: &'a [Ident],
        env: &Environment,
        server: &Server,
    ) -> Self {
        let mut fields = FieldDefs::resolve(rt, env, server);

        while let Some((id, tail)) = path.split_first() {
            path = tail;
            let defs = fields.fields.remove(id).unwrap_or_default();
            fields.fields.clear();

            for rt in defs.into_iter().filter_map(|d| d.value) {
                fields = fields.merge_from(FieldDefs::resolve(&rt, env, server));
            }
        }

        fields
    }

    pub fn defs(&self) -> impl Iterator<Item = &Def> {
        self.fields.values().flat_map(|defs| defs.iter())
    }

    /// Find all the fields that are defined on a term.
    ///
    /// This a best-effort thing; it doesn't do full evaluation but it has some reasonable
    /// heuristics. For example, it knows that the fields defined on a merge of two records
    /// are the fields defined on either record.
    fn resolve(rt: &RichTerm, env: &Environment, server: &Server) -> FieldDefs {
        match rt.term.as_ref() {
            Term::Record(data) | Term::RecRecord(data, ..) => {
                let fields = data
                    .fields
                    .iter()
                    .map(|(&ident, field)| {
                        (
                            ident.ident(),
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
            Term::Var(id) => server
                .lin_registry
                .get_def(&(*id).into())
                .or_else(|| env.get(&id.ident()))
                .map(|def| {
                    log::info!("got def {def:?}");

                    // The definition of this identifier is unlikely to belong to the
                    // environment we started with, especially because the enviroment
                    // mechanism is only used for providing definitions to incompletely
                    // parsed input.
                    let env = Environment::new();
                    let defs = def.resolve_terms(&env, server);
                    let terms = defs.iter().filter_map(|def| def.value.as_ref());
                    FieldDefs::resolve_all(terms, &env, server)
                })
                .unwrap_or_default(),
            Term::ResolvedImport(file_id) => {
                let env = Environment::new();
                server
                    .cache
                    .get_ref(*file_id)
                    .map(|term| FieldDefs::resolve(term, &env, server))
                    .unwrap_or_default()
            }
            Term::Op2(BinaryOp::Merge(_), t1, t2) => {
                FieldDefs::resolve(t1, env, server).merge_from(FieldDefs::resolve(t2, env, server))
            }
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

    fn resolve_all<'a>(
        terms: impl Iterator<Item = &'a RichTerm>,
        env: &Environment,
        server: &Server,
    ) -> FieldDefs {
        terms.fold(FieldDefs::default(), |acc, term| {
            acc.merge_from(FieldDefs::resolve(term, env, server))
        })
    }
}
