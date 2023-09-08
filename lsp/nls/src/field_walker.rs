use lsp_types::{CompletionItem, CompletionItemKind, Documentation, MarkupContent, MarkupKind};
use nickel_lang_core::{
    identifier::Ident,
    pretty::ident_quoted,
    term::{
        record::{Field, FieldMetadata, RecordData},
        BinaryOp, RichTerm, Term, TypeAnnotation, UnaryOp,
    },
    typ::{RecordRows, RecordRowsIteratorItem, Type, TypeF},
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

#[derive(Clone, Debug, PartialEq)]
pub enum FieldHaver {
    RecordTerm(RecordData),
    Dict(Type),
    RecordType(RecordRows),
}

impl FieldHaver {
    fn get(&self, id: Ident) -> Option<FieldValue> {
        match self {
            FieldHaver::RecordTerm(data) => data
                .fields
                .get(&id)
                .map(|field| FieldValue::RecordField(field.clone())),
            FieldHaver::Dict(ty) => Some(FieldValue::Type(ty.clone())),
            FieldHaver::RecordType(rows) => rows.row_find_path(&[id]).map(FieldValue::Type),
        }
    }

    pub fn completion_items(&self) -> impl Iterator<Item = CompletionItem> + '_ {
        match self {
            FieldHaver::RecordTerm(data) => {
                let iter = data.fields.iter().map(|(id, val)| CompletionItem {
                    label: ident_quoted(id),
                    detail: metadata_detail(&val.metadata),
                    kind: Some(CompletionItemKind::Property),
                    documentation: metadata_doc(&val.metadata),
                    ..Default::default()
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

#[derive(Clone, Debug, PartialEq)]
enum FieldValue {
    RecordField(Field),
    Type(Type),
}

/// The things that can appear as the right hand side of a `Def`
#[derive(Clone, Debug, PartialEq)]
enum Value {
    /// In `let foo = value`, the term `value` is the right hand side of the `Def`.
    Term(RichTerm),
    /// If type inference has determined that `x` is of type
    ///
    /// `{ foo: { bar: Number } }`
    ///     ^-----------------------------------------------------------|
    /// then in the expression `x.foo`, the definition of `foo` is here | and the
    /// value of its definition is the type `{ bar: Number }`.
    Type(Type),
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
    fn resolve_terms(&self, env: &Environment, server: &Server) -> Vec<FieldHaver> {
        let mut fields = Vec::new();

        if let Some(val) = &self.value {
            fields.extend_from_slice(&FieldDefs::resolve_term_path(val, &self.path, env, server))
        }
        if let Some(meta) = &self.metadata {
            for typ in meta.annotation.contracts.iter().chain(&meta.annotation.typ) {
                fields.extend_from_slice(&FieldDefs::resolve_path(
                    &Value::Type(typ.typ.clone()),
                    &self.path,
                    env,
                    server,
                ))
            }
        }
        fields
    }
}

#[cfg(test)]
impl DefWithPath {
    pub fn ident(&self) -> LocIdent {
        self.ident
    }

    pub fn path(&self) -> &[Ident] {
        &self.path
    }

    pub fn value(&self) -> Option<&RichTerm> {
        self.value.as_ref()
    }
}

/// A map from identifiers to the defs that they refer to.
#[derive(Clone, Debug, Default)]
pub struct FieldDefs {}

impl FieldDefs {
    pub fn resolve_term_path<'a>(
        rt: &'a RichTerm,
        path: &'a [Ident],
        env: &Environment,
        server: &Server,
    ) -> Vec<FieldHaver> {
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
    fn resolve_path<'a>(
        val: &'a Value,
        mut path: &'a [Ident],
        env: &Environment,
        server: &Server,
    ) -> Vec<FieldHaver> {
        let mut fields = FieldDefs::resolve(val, env, server);

        while let Some((id, tail)) = path.split_first() {
            path = tail;
            let values = fields
                .iter()
                .filter_map(|haver| haver.get(*id))
                .collect::<Vec<_>>();
            fields.clear();

            for value in values {
                match value {
                    FieldValue::RecordField(field) => {
                        if let Some(val) = &field.value {
                            fields.extend_from_slice(&FieldDefs::resolve_term(val, env, server))
                        }
                        fields.extend(FieldDefs::resolve_annot(
                            &field.metadata.annotation,
                            env,
                            server,
                        ));
                    }
                    FieldValue::Type(ty) => {
                        fields.extend_from_slice(&FieldDefs::resolve_type(&ty, env, server));
                    }
                }
            }
        }

        fields
    }

    fn resolve_annot<'a>(
        annot: &'a TypeAnnotation,
        env: &'a Environment,
        server: &'a Server,
    ) -> impl Iterator<Item = FieldHaver> + 'a {
        annot
            .contracts
            .iter()
            .chain(annot.typ.iter())
            .flat_map(|lty| FieldDefs::resolve_type(&lty.typ, env, server).into_iter())
    }

    fn resolve(v: &Value, env: &Environment, server: &Server) -> Vec<FieldHaver> {
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
    fn resolve_term(rt: &RichTerm, env: &Environment, server: &Server) -> Vec<FieldHaver> {
        let term_fields = match rt.term.as_ref() {
            Term::Record(data) | Term::RecRecord(data, ..) => {
                vec![FieldHaver::RecordTerm(data.clone())]
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
                    def.resolve_terms(&env, server)
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
            Term::Op2(BinaryOp::Merge(_), t1, t2) => combine(
                FieldDefs::resolve_term(t1, env, server),
                FieldDefs::resolve_term(t2, env, server),
            ),
            Term::Let(_, _, body, _) | Term::LetPattern(_, _, _, body) => {
                FieldDefs::resolve_term(body, env, server)
            }
            Term::Op1(UnaryOp::StaticAccess(id), term) => {
                FieldDefs::resolve_term_path(term, &[id.ident()], env, server)
            }
            Term::Annotated(annot, term) => {
                let defs = FieldDefs::resolve_annot(annot, env, server);
                defs.chain(FieldDefs::resolve_term(term, env, server))
                    .collect()
            }
            _ => Default::default(),
        };

        let typ_fields = if let Some(typ) = server.lin_registry.get_type(rt) {
            log::info!("got inferred type {typ:?}");
            FieldDefs::resolve_type(typ, env, server)
        } else {
            Vec::new()
        };

        combine(term_fields, typ_fields)
    }

    fn resolve_type(typ: &Type, env: &Environment, server: &Server) -> Vec<FieldHaver> {
        match &typ.typ {
            TypeF::Record(rows) => vec![FieldHaver::RecordType(rows.clone())],
            TypeF::Dict { type_fields, .. } => vec![FieldHaver::Dict(type_fields.as_ref().clone())],
            TypeF::Flat(rt) => FieldDefs::resolve_term(rt, env, server),
            _ => Default::default(),
        }
    }
}

fn combine<T>(mut left: Vec<T>, mut right: Vec<T>) -> Vec<T> {
    left.append(&mut right);
    left
}
