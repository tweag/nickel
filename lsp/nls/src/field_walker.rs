use std::collections::{hash_map::Entry, HashMap};

use lsp_types::{CompletionItem, CompletionItemKind, Documentation, MarkupContent, MarkupKind};
use nickel_lang_core::{
    identifier::Ident,
    pretty::ident_quoted,
    term::{record::FieldMetadata, BinaryOp, RichTerm, Term, UnaryOp},
    typ::{RecordRowsIteratorItem, Type, TypeF},
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

/// The things that can appear as the right hand side of a `Def`
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    /// In `let foo = value`, the term `value` is the right hand side of the `Def`.
    Term(RichTerm),
    /// If type inference has determined that `x` is of type
    ///
    /// `{ foo: Number }`
    ///     ^-----------------------------------------------------------|
    /// then in the expression `x.foo`, the definition of `foo` is here | and the
    /// value of its definition is `Number`.
    Type(Type),
}

#[cfg(test)]
impl Value {
    pub fn as_term(&self) -> Option<&RichTerm> {
        match self {
            Value::Term(rt) => Some(rt),
            Value::Type(_) => None,
        }
    }
}

impl From<RichTerm> for Value {
    fn from(rt: RichTerm) -> Self {
        Value::Term(rt)
    }
}

impl From<Type> for Value {
    fn from(typ: Type) -> Self {
        Value::Type(typ)
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
    pub value: Option<Value>,
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
    pub def: Def,
    pub path: Vec<Ident>,
}

impl DefWithPath {
    fn resolve_terms(&self, env: &Environment, server: &Server) -> Vec<Def> {
        // Calling `resolve_path` on x with path [foo, bar] returns all
        // the fields *in* x.foo.bar. We want the value(s) of x.foo.bar,
        // so use `resolve_path` to get x.foo and then find the bars in it.
        if let Some((last, path)) = self.path.split_last() {
            match &self.def.value {
                Some(val) => FieldDefs::resolve_path(val, path, env, server)
                    .fields
                    .remove(last)
                    .unwrap_or_default(),
                None => vec![],
            }
        } else {
            // The path is empty.
            vec![self.def.clone()]
        }
    }
}

#[cfg(test)]
impl DefWithPath {
    pub fn ident(&self) -> LocIdent {
        self.def.ident
    }

    pub fn path(&self) -> &[Ident] {
        &self.path
    }

    pub fn value(&self) -> Option<&Value> {
        self.def.value.as_ref()
    }
}

/// A map from identifiers to the defs that they refer to.
#[derive(Clone, Debug, Default)]
pub struct FieldDefs {
    fields: HashMap<Ident, Vec<Def>>,
    // A dict defines all possible fields, and they all have the same type.
    // When we encounter a Dict, we (obviously) don't insert all those fields into
    // the field map above. Instead we store the field type here.
    dicts: Vec<Type>,
}

impl FieldDefs {
    pub fn resolve_term_path<'a>(
        rt: &'a RichTerm,
        path: &'a [Ident],
        env: &Environment,
        server: &Server,
    ) -> Self {
        FieldDefs::resolve_path(&rt.clone().into(), path, env, server)
    }

    /// Resolve a record path iteratively, returning all the fields defined on the final path element.
    ///
    /// `env` is an environment that only gets used (and even then, only
    /// as a fallback) for the first layer of variable resolutions. After
    /// that, variables are resolved using the precomputed usage tables. This
    /// mechanism allows providing an initial environment for input that doesn't
    /// parse, and hence doesn't exist in the precomputed usage tables.
    ///
    /// For example, in `let x = ... in let y = x in [ y.` we will rely on the
    /// initial environment to resolve the `y` in `[ y.`, and after that we will
    /// use the precomputed tables to resolve the `x`.
    pub fn resolve_path<'a>(
        rt: &'a Value,
        mut path: &'a [Ident],
        env: &Environment,
        server: &Server,
    ) -> Self {
        let mut fields = FieldDefs::resolve(rt, env, server);

        while let Some((id, tail)) = path.split_first() {
            path = tail;
            let defs = fields.fields.remove(id).unwrap_or_default();
            let dicts = std::mem::take(&mut fields.dicts);
            fields.fields.clear();

            for rt in defs.into_iter().filter_map(|d| d.value) {
                fields = fields.merge_from(FieldDefs::resolve(&rt, env, server));
            }

            for dict_value in dicts {
                fields = fields.merge_from(FieldDefs::resolve_type(&dict_value, env, server));
            }
        }

        fields
    }

    pub fn defs(&self) -> impl Iterator<Item = &Def> {
        self.fields.values().flat_map(|defs| defs.iter())
    }

    fn resolve(v: &Value, env: &Environment, server: &Server) -> FieldDefs {
        match v {
            Value::Term(rt) => FieldDefs::resolve_term(rt, env, server),
            Value::Type(typ) => FieldDefs::resolve_type(typ, env, server),
        }
    }

    /// Find all the fields that are defined on a term.
    ///
    /// This a best-effort thing; it doesn't do full evaluation but it has some reasonable
    /// heuristics. For example, it knows that the fields defined on a merge of two records
    /// are the fields defined on either record.
    ///
    /// `env` is an environment used only for the initial resolutions; see [`Self::resolve_path`]
    fn resolve_term(rt: &RichTerm, env: &Environment, server: &Server) -> FieldDefs {
        let term_fields = match rt.term.as_ref() {
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
                FieldDefs {
                    fields,
                    dicts: Default::default(),
                }
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
                    .map(|term| FieldDefs::resolve_term(term, &env, server))
                    .unwrap_or_default()
            }
            Term::Op2(BinaryOp::Merge(_), t1, t2) => FieldDefs::resolve_term(t1, env, server)
                .merge_from(FieldDefs::resolve_term(t2, env, server)),
            Term::Let(_, _, body, _) | Term::LetPattern(_, _, _, body) => {
                FieldDefs::resolve_term(body, env, server)
            }
            Term::Op1(UnaryOp::StaticAccess(id), term) => {
                FieldDefs::resolve_term_path(term, &[id.ident()], env, server)
            }
            Term::Annotated(annot, term) => {
                let defs = annot
                    .contracts
                    .iter()
                    .chain(annot.typ.iter())
                    .map(|lty| FieldDefs::resolve_type(&lty.typ, env, server));
                defs.fold(FieldDefs::resolve_term(term, env, server), |acc, def| {
                    acc.merge_from(def)
                })
            }
            _ => Default::default(),
        };

        let typ_fields = if let Some(typ) = server.lin_registry.get_type(rt) {
            log::info!("got inferred type {typ:?}");
            FieldDefs::resolve_type(typ, env, server)
        } else {
            FieldDefs::default()
        };

        term_fields.merge_from(typ_fields)
    }

    fn resolve_type(typ: &Type, env: &Environment, server: &Server) -> FieldDefs {
        match &typ.typ {
            TypeF::Record(rows) => {
                let fields = rows.iter().filter_map(|row| {
                    if let RecordRowsIteratorItem::Row(row) = row {
                        Some((
                            row.id.ident(),
                            vec![Def {
                                ident: row.id.into(),
                                value: Some(row.typ.clone().into()),
                                metadata: None,
                            }],
                        ))
                    } else {
                        None
                    }
                });
                FieldDefs {
                    fields: fields.collect(),
                    dicts: Vec::new(),
                }
            }
            TypeF::Dict { type_fields, .. } => FieldDefs {
                fields: HashMap::new(),
                dicts: vec![type_fields.as_ref().clone()],
            },
            TypeF::Flat(rt) => FieldDefs::resolve_term(rt, env, server),
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
        self.dicts.extend_from_slice(&other.dicts);
        self
    }

    fn resolve_all<'a>(
        terms: impl Iterator<Item = &'a Value>,
        env: &Environment,
        server: &Server,
    ) -> FieldDefs {
        terms.fold(FieldDefs::default(), |acc, term| {
            acc.merge_from(FieldDefs::resolve(term, env, server))
        })
    }
}
