use std::collections::HashMap;

use codespan::ByteIndex;
use log::debug;
use nickel::{
    identifier::Ident,
    position::{RawSpan, TermPos},
    term::{MetaValue, RichTerm, Term, UnaryOp},
    typecheck::{
        linearization::{Linearization, Linearizer, Scope, ScopeId},
        reporting::{to_type, NameReg},
        TypeWrapper, UnifTable,
    },
    types::AbsType,
};

use crate::linearization::interface::ValueState;

use self::{
    building::{Building, ID},
    completed::Completed,
    interface::{ResolutionState, TermKind, UsageState},
};

pub mod building;
pub mod completed;
pub mod interface;

pub type Environment = nickel::environment::Environment<Ident, usize>;

/// A recorded item of a given state of resolution state
/// Tracks a unique id used to build a reference table after finalizing
/// the linearization using the LSP [AnalysisHost]
#[derive(Debug, Clone, PartialEq)]
pub struct LinearizationItem<S: ResolutionState> {
    //term_: Box<Term>,
    pub id: usize,
    pub pos: RawSpan,
    pub ty: S,
    pub kind: TermKind,
    pub scope: Scope,
    pub meta: Option<MetaValue>,
}

/// [Linearizer] used by the LSP
///
/// Tracks a _scope stable_ environment managing variable ident
/// resolution
pub struct AnalysisHost {
    env: Environment,
    scope: Scope,
    next_scope_id: ScopeId,
    meta: Option<MetaValue>,
    /// Indexing a record will store a reference to the record as
    /// well as its fields.
    /// [Self::Scope] will produce a host with a single **`pop`ed**
    /// Ident. As fields are typechecked in the same order, each
    /// in their own scope immediately after the record, which
    /// gives the corresponding record field _term_ to the ident
    /// useable to construct a vale declaration.
    record_fields: Option<(ID, Vec<(ID, Ident)>)>,
    let_binding: Option<ID>,
    /// Accesses to nested records are recorded recursively.
    /// ```
    /// outer.middle.inner -> inner(middle(outer))
    /// ```
    /// To resolve those inner fields, accessors (`inner`, `middle`)
    /// are recorded first until a variable (`outer`). is found.
    /// Then, access to all nested records are resolved at once.
    access: Option<Vec<Ident>>,
}

impl AnalysisHost {
    pub fn new() -> Self {
        Default::default()
    }
}

impl Default for AnalysisHost {
    fn default() -> Self {
        Self {
            env: Environment::new(),
            scope: Default::default(),
            next_scope_id: Default::default(),
            meta: Default::default(),
            record_fields: Default::default(),
            let_binding: Default::default(),
            access: Default::default(),
        }
    }
}

impl Linearizer for AnalysisHost {
    type Building = Building;
    type Completed = Completed;
    type CompletionExtra = (UnifTable, HashMap<usize, Ident>);

    fn add_term(
        &mut self,
        lin: &mut Linearization<Building>,
        term: &Term,
        mut pos: TermPos,
        ty: TypeWrapper,
    ) {
        debug!("adding term: {:?} @ {:?}", term, pos);
        let mut id_gen = lin.id_gen();

        // Register record field if appropriate
        // `record` is the id [LinearizatonItem] of the enclosing record
        // `offset` is used to find the [LinearizationItem] representing the field
        // Field items are inserted immediately after the record
        if !matches!(
            term,
            Term::Op1(UnaryOp::StaticAccess(_), _) | Term::MetaValue(_)
        ) {
            if let Some((record, (offset, Ident { pos: field_pos, .. }))) = self
                .record_fields
                .take()
                .map(|(record, mut fields)| (record, fields.pop().unwrap()))
            {
                pos = field_pos.map(|mut pos| {
                    pos.start = ByteIndex(0);
                    pos.end = ByteIndex(0);
                    pos
                });

                for field in lin.linearization.get_mut(record + offset).into_iter() {
                    debug!("{:?}", field.kind);
                    let usage_offset = if matches!(term, Term::Var(_)) {
                        debug!(
                            "associating nested field {:?} with chain {:?}",
                            term,
                            self.access.as_ref()
                        );
                        self.access.as_ref().map(|v| v.len()).unwrap_or(0)
                    } else {
                        0
                    };
                    match field.kind {
                        TermKind::RecordField { ref mut value, .. } => {
                            *value = ValueState::Known(id_gen.get() + usage_offset);
                        }
                        // The linearization item of a record with n fields is expected to be
                        // followed by n linearization items representing each field
                        _ => unreachable!(),
                    }
                }
            }

            if let Some(declaration) = self.let_binding.take() {
                lin.inform_declaration(declaration, id_gen.get());
            }
        }

        if pos == TermPos::None {
            return;
        }

        let id = id_gen.get();
        match term {
            Term::LetPattern(ident, destruct, ..) | Term::FunPattern(ident, destruct, _) => {
                if let Some(ident) = ident {
                    self.env.insert(ident.to_owned(), id);
                    let id = id_gen.get_and_advance();

                    let value_ptr = match term {
                        Term::LetPattern(_, _, _, _) => {
                            self.let_binding = Some(id);
                            ValueState::Unknown
                        }
                        Term::FunPattern(_, _, _) => ValueState::Known(id),
                        _ => unreachable!(),
                    };

                    lin.push(LinearizationItem {
                        id,
                        ty,
                        pos: ident.pos.unwrap(),
                        scope: self.scope.clone(),
                        kind: TermKind::Declaration(ident.to_owned(), Vec::new(), value_ptr),
                        meta: self.meta.take(),
                    });
                }
                for matched in destruct.to_owned().inner() {
                    let (ident, term) = matched.as_meta_field();
                    self.env.insert(ident.to_owned(), id_gen.get());
                    let id = id_gen.get_and_advance();
                    lin.push(LinearizationItem {
                        id,
                        // TODO: get type from pattern
                        ty: TypeWrapper::Concrete(AbsType::Dyn()),
                        pos: ident.pos.unwrap(),
                        scope: self.scope.clone(),
                        kind: TermKind::Declaration(
                            ident.to_owned(),
                            Vec::new(),
                            ValueState::Known(id),
                        ),
                        meta: match &*term.term {
                            Term::MetaValue(meta) => Some(MetaValue {
                                value: None,
                                ..meta.clone()
                            }),
                            _ => None,
                        },
                    });
                }
            }
            Term::Let(ident, _, _, _) | Term::Fun(ident, _) => {
                self.env.insert(ident.to_owned(), id);
                let value_ptr = match term {
                    Term::LetPattern(_, _, _, _) => {
                        self.let_binding = Some(id);
                        ValueState::Unknown
                    }
                    Term::FunPattern(_, _, _) => ValueState::Known(id),
                    _ => unreachable!(),
                };
                lin.push(LinearizationItem {
                    id,
                    ty,
                    pos: ident.pos.unwrap(),
                    scope: self.scope.clone(),
                    kind: TermKind::Declaration(ident.to_owned(), Vec::new(), value_ptr),
                    meta: self.meta.take(),
                });
            }
            Term::Var(ident) => {
                let root_id = id_gen.get_and_advance();

                debug!(
                    "adding usage of variable {} followed by chain {:?}",
                    ident, self.access
                );

                lin.push(LinearizationItem {
                    id: root_id,
                    pos: ident.pos.unwrap(),
                    ty: TypeWrapper::Concrete(AbsType::Dyn()),
                    scope: self.scope.clone(),
                    kind: TermKind::Usage(UsageState::from(self.env.get(ident))),
                    meta: self.meta.take(),
                });

                if let Some(referenced) = self.env.get(ident) {
                    lin.add_usage(referenced, root_id)
                }

                if let Some(chain) = self.access.take() {
                    let chain: Vec<_> = chain.into_iter().rev().collect();

                    for accessor in chain.iter() {
                        let id = id_gen.get_and_advance();
                        lin.push(LinearizationItem {
                            id,
                            pos: accessor.pos.unwrap(),
                            ty: TypeWrapper::Concrete(AbsType::Dyn()),
                            scope: self.scope.clone(),
                            kind: TermKind::Usage(UsageState::Deferred {
                                parent: id - 1,
                                child: accessor.to_owned(),
                            }),
                            meta: self.meta.take(),
                        });
                    }
                }
            }
            Term::Record(fields, _) | Term::RecRecord(fields, _, _) => {
                lin.push(LinearizationItem {
                    id,
                    pos: pos.unwrap(),
                    ty,
                    kind: TermKind::Record(HashMap::new()),
                    scope: self.scope.clone(),
                    meta: self.meta.take(),
                });

                lin.register_fields(fields, id, self.scope.clone(), &mut self.env);
                let mut field_names = fields.keys().cloned().collect::<Vec<_>>();
                field_names.sort_unstable();

                self.record_fields =
                    Some((id + 1, field_names.into_iter().enumerate().rev().collect()));
            }
            Term::Op1(UnaryOp::StaticAccess(ident), _) => {
                let x = self.access.get_or_insert(Vec::with_capacity(1));
                x.push(ident.to_owned())
            }
            Term::MetaValue(meta) => {
                // DF walk AST (of Contracts)
                // adds usages of user defined contracts to the linearization
                fn walk_terms(
                    lin: &mut Linearization<Building>,
                    mut host: AnalysisHost,
                    RichTerm { term, pos }: &RichTerm,
                ) {
                    host.add_term(lin, term, *pos, TypeWrapper::Concrete(AbsType::Dyn()));
                    match &**term {
                        Term::Op1(_, rt) => walk_terms(lin, host, rt),
                        Term::Op2(_, rt1, rt2) => {
                            walk_terms(lin, host.scope(), rt1);
                            walk_terms(lin, host.scope(), rt2);
                        }
                        Term::OpN(_, rts) => {
                            rts.iter().for_each(|rt| walk_terms(lin, host.scope(), rt))
                        }
                        Term::StrChunks(chunks) => chunks
                            .iter()
                            .filter_map(|chunk| match chunk {
                                nickel::term::StrChunk::Literal(_) => None,
                                nickel::term::StrChunk::Expr(rt, _) => Some(rt),
                            })
                            .for_each(|rt| walk_terms(lin, host.scope(), rt)),
                        Term::App(rt1, rt2) => {
                            walk_terms(lin, host.scope(), rt1);
                            walk_terms(lin, host.scope(), rt2);
                        }
                        Term::Record(rts, _) | Term::RecRecord(rts, _, _) => rts
                            .values()
                            .for_each(|rt| walk_terms(lin, host.scope(), rt)),
                        _ => {}
                    }
                }

                // recursively linearize flat types
                fn walk_types(
                    lin: &mut Linearization<Building>,
                    outer_host: &mut AnalysisHost,
                    t: &nickel::types::Types,
                ) {
                    let mut scope = outer_host.scope.clone();
                    let (scope_id, next_scope_id) = outer_host.next_scope_id.next();
                    outer_host.next_scope_id = next_scope_id;
                    scope.push(scope_id);

                    let inner_host = AnalysisHost {
                        env: outer_host.env.clone(),
                        scope,
                        ..Default::default()
                    };
                    match &t.0 {
                        AbsType::Flat(rt) => walk_terms(lin, inner_host, &rt),
                        AbsType::Arrow(t1, t2) => {
                            walk_types(lin, outer_host, &t1);
                            walk_types(lin, outer_host, &t2);
                        }
                        _ => {}
                    }
                }

                // Notice 1: No push to lin for the `MetaValue` itself
                // Notice 2: we discard the encoded value as anything we
                //           would do with the value will be handled in the following
                //           call to [Self::add_term]
                // Notice 3: we walk the inner contract types to resolve references

                meta.contracts
                    .iter()
                    .for_each(|c| walk_types(lin, self, &c.types));

                if meta.value.is_some() {
                    self.meta = Some(MetaValue {
                        value: None,
                        ..meta.to_owned()
                    })
                }
            }
            other => {
                debug!("Add wildcard item: {:?}", other);

                lin.push(LinearizationItem {
                    id,
                    pos: pos.unwrap(),
                    ty,
                    scope: self.scope.clone(),
                    kind: TermKind::Structure,
                    meta: self.meta.take(),
                })
            }
        }
    }

    /// [Self::add_term] produces a depth first representation or the
    /// traversed AST. This function indexes items by _source position_.
    /// Elements are reorderd to allow efficient lookup of elemts by
    /// their location in the source.
    ///
    /// Additionally, resolves concrete types for all items.
    fn complete(
        self,
        mut lin: Linearization<Building>,
        (table, reported_names): (UnifTable, HashMap<usize, Ident>),
    ) -> Linearization<Completed> {
        debug!("linearizing");

        // TODO: Storing defers while linearizing?
        let defers: Vec<(usize, usize, Ident)> = lin
            .linearization
            .iter()
            .filter_map(|item| match &item.kind {
                TermKind::Usage(UsageState::Deferred { parent, child }) => {
                    Some((item.id, *parent, child.clone()))
                }
                _ => None,
            })
            .collect();

        lin.resolve_record_references(defers);

        let Building {
            mut linearization,
            scope,
        } = lin.into_inner();

        linearization.sort_by_key(|item| (item.pos.src_id, item.pos.start));

        // create an index of id -> new position
        let mut id_mapping = HashMap::new();
        linearization
            .iter()
            .enumerate()
            .for_each(|(index, LinearizationItem { id, .. })| {
                id_mapping.insert(*id, index);
            });

        // resolve types
        let lin_ = linearization
            .into_iter()
            .map(
                |LinearizationItem {
                     id,
                     pos,
                     ty,
                     kind,
                     scope,
                     meta,
                 }| LinearizationItem {
                    ty: to_type(&table, &reported_names, &mut NameReg::new(), ty),
                    id,
                    pos,
                    kind,
                    scope,
                    meta,
                },
            )
            .collect();

        eprintln!("Linearized {:#?}", &lin_);

        Linearization::new(Completed::new(lin_, scope, id_mapping))
    }

    fn scope(&mut self) -> Self {
        let mut scope = self.scope.clone();
        let (scope_id, next_scope_id) = self.next_scope_id.next();
        self.next_scope_id = next_scope_id;

        scope.push(scope_id);

        AnalysisHost {
            scope,
            env: self.env.clone(),
            next_scope_id: ScopeId::default(),
            /// when opening a new scope `meta` is assumed to be `None` as meta data
            /// is immediately followed by a term without opening a scope
            meta: None,
            record_fields: self.record_fields.as_mut().and_then(|(record, fields)| {
                Some(*record).zip(fields.pop().map(|field| vec![field]))
            }),
            let_binding: None,
            access: self.access.clone(),
        }
    }

    fn retype_ident(
        &mut self,
        lin: &mut Linearization<Building>,
        ident: &Ident,
        new_type: TypeWrapper,
    ) {
        if let Some(item) = self
            .env
            .get(ident)
            .and_then(|index| lin.linearization.get_mut(index))
        {
            debug!("retyping {:?} to {:?}", ident, new_type);
            item.ty = new_type;
        }
    }
}

struct IdGen(usize);

impl IdGen {
    /// Make new Generator starting at `base`
    fn new(base: usize) -> Self {
        IdGen(base)
    }

    /// Get the current id
    fn get(&self) -> usize {
        self.0
    }

    /// Return the **current id** and advance the generator.
    /// Following calls to get (and get_and_advance) will return a new id
    fn get_and_advance(&mut self) -> usize {
        let current_id = self.0;
        self.0 += 1;
        current_id
    }
}
