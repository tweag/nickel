use std::{borrow::BorrowMut, cell::UnsafeCell, collections::HashMap};

use codespan::ByteIndex;
use log::{debug, Record};
use nickel::{
    identifier::Ident,
    position::TermPos,
    term::{MetaValue, RichTerm, Term, UnaryOp},
    typecheck::{
        linearization::{
            Building, Completed, Environment, Linearization, LinearizationItem, Linearizer,
            ScopeId, TermKind, Unresolved, UsageState,
        },
        reporting::{to_type, NameReg},
        TypeWrapper, UnifTable,
    },
    types::AbsType,
};

#[derive(Default)]
pub struct BuildingResource {
    pub linearization: Vec<LinearizationItem<Unresolved>>,
    pub scope: HashMap<Vec<ScopeId>, Vec<usize>>,
}

trait BuildingExt {
    fn push(&mut self, item: LinearizationItem<Unresolved>);
    fn add_usage(&mut self, decl: usize, usage: usize);
    fn register_fields(
        &mut self,
        record_fields: &HashMap<Ident, RichTerm>,
        record: usize,
        scope: Vec<ScopeId>,
        env: &mut Environment,
    );
    fn add_record_field(&mut self, record: usize, field: (Ident, usize));

    fn resolve_record_references(&mut self);

    fn id_gen(&self) -> IdGen;
}

impl BuildingExt for Linearization<Building<BuildingResource>> {
    fn push(&mut self, item: LinearizationItem<Unresolved>) {
        self.state
            .resource
            .scope
            .remove(&item.scope)
            .map(|mut s| {
                s.push(item.id);
                s
            })
            .or_else(|| Some(vec![item.id]))
            .into_iter()
            .for_each(|l| {
                self.state.resource.scope.insert(item.scope.clone(), l);
            });
        self.state.resource.linearization.push(item);
    }

    fn add_usage(&mut self, decl: usize, usage: usize) {
        match self
            .state
            .resource
            .linearization
            .get_mut(decl)
            .expect("Could not find parent")
            .kind
        {
            TermKind::Structure => unreachable!(),
            TermKind::Usage(_) => unreachable!(),
            TermKind::Record(_) => unreachable!(),
            TermKind::Declaration(_, ref mut usages)
            | TermKind::RecordField { ref mut usages, .. } => usages.push(usage),
        };
    }

    fn register_fields(
        &mut self,
        record_fields: &HashMap<Ident, RichTerm>,
        record: usize,
        scope: Vec<ScopeId>,
        env: &mut Environment,
    ) {
        for (ident, value) in record_fields.into_iter() {
            let id = self.id_gen().take();
            self.push(LinearizationItem {
                id,
                pos: ident.1,
                // temporary, the actual type is resolved later and the item retyped
                ty: TypeWrapper::Concrete(AbsType::RowEmpty()),
                kind: TermKind::RecordField {
                    record,
                    ident: ident.clone(),
                    usages: Vec::new(),
                    value: None,
                },
                scope: scope.clone(),
                meta: match value.term.as_ref() {
                    Term::MetaValue(meta @ MetaValue { .. }) => Some(MetaValue {
                        value: None,
                        ..meta.clone()
                    }),
                    _ => None,
                },
            });
            env.insert(ident.clone(), id);
            self.add_record_field(record, (ident.clone(), id))
        }
    }

    fn add_record_field(&mut self, record: usize, (field_ident, reference_id): (Ident, usize)) {
        match self
            .state
            .resource
            .linearization
            .get_mut(record)
            .expect("Could not find record")
            .kind
        {
            TermKind::Record(ref mut fields) => {
                fields.insert(field_ident, reference_id);
            }
            _ => unreachable!(),
        }
    }

    fn resolve_record_references(&mut self) {
        let defers: Vec<(usize, usize, Ident)> = self
            .state
            .resource
            .linearization
            .iter()
            .filter_map(|item| match &item.kind {
                TermKind::Usage(UsageState::Deferred { parent, child }) => {
                    Some((item.id, *parent, child.clone()))
                }
                _ => None,
            })
            .collect();

        for (child_item, parent_accessor_id, child_ident) in defers {
            // the value referenced by the parent accessor element
            let parent_referenced = self
                .state
                .resource
                .linearization
                .get(parent_accessor_id)
                .and_then(|parent_accessor| match parent_accessor {
                    LinearizationItem {
                        kind: TermKind::Usage(UsageState::Resolved(parent_referenced)),
                        ..
                    } => parent_referenced.clone(),
                    _ => None,
                });

            // Determine current accessor node
            // root should be the topmost declaration or record
            //
            // root.accessor1.accessor2
            //
            // If such an accessor chain is used in a record whiere the
            // linearization order of fields is not deterministic resolving a variable
            // declared in a sibling branch of the record may fail if that branch is
            // linearized after the current one.
            let referenced_declaration = parent_referenced.and_then(|parent_id| {
                match &self
                    .state
                    .resource
                    .linearization
                    .get(parent_id)
                    .unwrap()
                    .kind
                {
                    // resolve referenced field of record
                    TermKind::RecordField { value, .. } => {
                        debug!("parent referenced a record field {:?}", value);
                        value
                            // retrieve record
                            .and_then(|value_index| {
                                self.state.resource.linearization.get(value_index)
                            })
                            // retrieve field
                            .and_then(|record| match &record.kind {
                                TermKind::Record(fields) => {
                                    debug!("parent referenced a nested record: {:?}", fields);
                                    fields.get(&child_ident).and_then(|accessor_id| {
                                        self.state.resource.linearization.get(*accessor_id)
                                    })
                                }
                                TermKind::Usage(UsageState::Resolved(Some(pointed))) => {
                                    self.state.resource.linearization.get(*pointed)
                                }

                                _ => None,
                            })
                    }
                    TermKind::Declaration(_, _) => {
                        match &self
                            .state
                            .resource
                            .linearization
                            .get(parent_id + 1)
                            .unwrap()
                            .kind
                        {
                            TermKind::Record(fields) => {
                                fields.get(&child_ident).and_then(|accessor_id| {
                                    self.state.resource.linearization.get(*accessor_id)
                                })
                            }
                            TermKind::Usage(UsageState::Resolved(Some(pointed))) => {
                                self.state.resource.linearization.get(*pointed)
                            }

                            _ => None,
                        }
                    }
                    TermKind::Record(fields) => fields.get(&child_ident).and_then(|accessor_id| {
                        self.state.resource.linearization.get(*accessor_id)
                    }),
                    // stop if not a field
                    kind @ _ => {
                        debug!("expected record or record field, was: {:?}", kind);
                        None
                    }
                }
            });

            let referenced_declaration =
                referenced_declaration.and_then(|referenced| match referenced.kind {
                    TermKind::Usage(UsageState::Resolved(Some(pointed))) => {
                        self.state.resource.linearization.get(pointed)
                    }
                    TermKind::RecordField { value, .. } => value
                        // retrieve record
                        .and_then(|value_index| self.state.resource.linearization.get(value_index))
                        // retrieve field
                        .and_then(|record| match &record.kind {
                            TermKind::Record(fields) => {
                                debug!(
                                    "parent referenced a nested record indirectly`: {:?}",
                                    fields
                                );
                                fields.get(&child_ident).and_then(|accessor_id| {
                                    self.state.resource.linearization.get(*accessor_id)
                                })
                            }
                            _ => None,
                        })
                        .or(Some(referenced)),

                    _ => Some(referenced),
                });

            let referenced_id = referenced_declaration.map(|reference| reference.id);

            // let id = id_gen.take();
            // let child_id = child.map(|r| r.id);
            // let referenced_value = referenced_idlin.state.resource.linearization.get().and_then(|child| match child {
            //     child
            //     @
            //     LinearizationItem {
            //         kind: TermKind::Declaration(..),
            //         ..
            //     } => Some(child.id),
            //     LinearizationItem {
            //         kind: TermKind::RecordField { value, .. },
            //         ..
            //     } => value.to_owned(),
            //     _ => None,
            // });

            debug!(
                "Associating child {} to value {:?}",
                child_ident, referenced_declaration
            );

            {
                let child: &mut LinearizationItem<TypeWrapper> = self
                    .state
                    .resource
                    .linearization
                    .get_mut(child_item)
                    .unwrap();
                child.kind = TermKind::Usage(UsageState::Resolved(referenced_id));
            }

            if let Some(referenced_id) = referenced_id {
                self.add_usage(referenced_id, child_item);
            }
        }
    }

    fn id_gen(&self) -> IdGen {
        IdGen::new(self.state.resource.linearization.len())
    }
}

/// [Linearizer] used by the LSP
///
/// Tracks a _scope stable_ environment managing variable ident
/// resolution
pub struct AnalysisHost {
    env: Environment,
    scope: Vec<ScopeId>,
    meta: Option<MetaValue>,
    /// Indexing a record will store a reference to the record as
    /// well as its fields.
    /// [Self::Scope] will produce a host with a single **`pop`ed**
    /// Ident. As fields are typechecked in the same order, each
    /// in their own scope immediately after the record, which
    /// gives the corresponding record field _term_ to the ident
    /// useable to construct a vale declaration.
    record_fields: Option<(usize, Vec<(usize, Ident)>)>,
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
        AnalysisHost {
            env: Environment::new(),
            scope: Vec::new(),
            meta: None,
            record_fields: None,
            access: None,
        }
    }
}

impl Linearizer<BuildingResource, (UnifTable, HashMap<usize, Ident>)> for AnalysisHost {
    fn add_term(
        &mut self,
        lin: &mut Linearization<Building<BuildingResource>>,
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
        if !matches!(term, Term::Op1(UnaryOp::StaticAccess(_), _)) {
            if let Some((record, (offset, Ident(_, field_pos)))) = self
                .record_fields
                .take()
                .map(|(record, mut fields)| (record, fields.pop().unwrap()))
            {
                pos = field_pos.map(|mut pos| {
                    pos.start = ByteIndex(0);
                    pos.end = ByteIndex(0);
                    pos
                });

                for field in lin
                    .state
                    .resource
                    .linearization
                    .get_mut(record + offset)
                    .into_iter()
                {
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
                            *value = Some(id_gen.id() + usage_offset);
                        }
                        // The linearization item of a record with n fields is expected to be
                        // followed by n linearization items representing each field
                        _ => unreachable!(),
                    }
                }
            }
        }

        if pos == TermPos::None {
            return;
        }

        let id = id_gen.id();
        match term {
            Term::Let(ident, _, _) | Term::Fun(ident, _) => {
                self.env
                    .insert(ident.to_owned(), lin.state.resource.linearization.len());
                lin.push(LinearizationItem {
                    id,
                    ty,
                    pos: ident.1,
                    scope: self.scope.clone(),
                    kind: TermKind::Declaration(ident.to_owned(), Vec::new()),
                    meta: self.meta.take(),
                });
            }
            Term::Var(ident) => {
                let root_id = id_gen.take();

                debug!(
                    "adding usage of variable {} followed by chain {:?}",
                    ident, self.access
                );

                lin.push(LinearizationItem {
                    id: root_id,
                    pos: ident.1,
                    ty: TypeWrapper::Concrete(AbsType::Dyn()),
                    scope: self.scope.clone(),
                    kind: TermKind::Usage(UsageState::Resolved(self.env.get(ident))),
                    meta: self.meta.take(),
                });

                if let Some(referenced) = self.env.get(ident) {
                    lin.add_usage(referenced, root_id)
                }

                if let Some(chain) = self.access.take() {
                    let chain: Vec<_> = chain.into_iter().rev().collect();

                    for accessor in chain.iter() {
                        let id = id_gen.take();
                        lin.push(LinearizationItem {
                            id,
                            pos: accessor.1,
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
                    pos,
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
                // Notice 1: No push to lin
                // Notice 2: we discard the encoded value as anything we
                //           would do with the value will be handled in the following
                //           call to [Self::add_term]

                for contract in meta.contracts.iter() {
                    match &contract.types.0 {
                        // Note: we extract the
                        nickel::types::AbsType::Flat(RichTerm { term, pos: _ }) => {
                            match &**term {
                                Term::Var(ident) => {
                                    let parent = self.env.get(&ident);
                                    let id = id_gen.take();
                                    lin.push(LinearizationItem {
                                        id,
                                        pos: ident.1,
                                        ty: TypeWrapper::Concrete(AbsType::Var(ident.to_owned())),
                                        scope: self.scope.clone(),
                                        // id = parent: full let binding including the body
                                        // id = parent + 1: actual delcaration scope, i.e. _ = < definition >
                                        kind: TermKind::Usage(UsageState::Resolved(
                                            parent.map(|id| id),
                                        )),
                                        meta: None,
                                    });
                                    if let Some(parent) = parent {
                                        lin.add_usage(parent, id);
                                    }
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }

                if meta.value.is_some() {
                    self.meta = Some(MetaValue {
                        value: None,
                        ..meta.to_owned()
                    })
                }
            }

            other @ _ => {
                debug!("Add wildcard item: {:?}", other);

                lin.push(LinearizationItem {
                    id,
                    pos,
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
    fn linearize(
        self,
        mut lin: Linearization<Building<BuildingResource>>,
        (table, reported_names): (UnifTable, HashMap<usize, Ident>),
    ) -> Linearization<Completed> {
        eprintln!("linearizing");

        lin.resolve_record_references();

        let mut lin_ = lin.state.resource.linearization;

        lin_.sort_by_key(|item| match item.pos {
            TermPos::Original(span) => (span.src_id, span.start),
            TermPos::Inherited(span) => (span.src_id, span.start),
            TermPos::None => {
                eprintln!("{:?}", item);

                unreachable!()
            }
        });

        // create an index of id -> new position
        let mut id_mapping = HashMap::new();
        lin_.iter()
            .enumerate()
            .for_each(|(index, LinearizationItem { id, .. })| {
                id_mapping.insert(*id, index);
            });

        // resolve types
        let lin_ = lin_
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

        Linearization::completed(Completed {
            lin: lin_,
            id_mapping,
            scope_mapping: lin.state.resource.scope,
        })
    }

    fn scope(&mut self, scope_id: ScopeId) -> Self {
        let mut scope = self.scope.clone();
        scope.push(scope_id);

        AnalysisHost {
            scope,
            env: self.env.clone(),
            /// when opening a new scope `meta` is assumed to be `None` as meta data
            /// is immediately followed by a term without opening a scope
            meta: None,
            record_fields: self.record_fields.as_mut().and_then(|(record, fields)| {
                Some(*record).zip(fields.pop().map(|field| vec![field]))
            }),
            access: self.access.clone(),
        }
    }

    fn retype_ident(
        &mut self,
        lin: &mut Linearization<Building<BuildingResource>>,
        ident: &Ident,
        new_type: TypeWrapper,
    ) {
        if let Some(item) = self
            .env
            .get(ident)
            .and_then(|index| lin.state.resource.linearization.get_mut(index))
        {
            debug!("retyping {:?} to {:?}", ident, new_type);
            item.ty = new_type;
        }
    }
}

struct IdGen(usize);

impl IdGen {
    fn new(base: usize) -> Self {
        IdGen(base)
    }

    fn take(&mut self) -> usize {
        let current_id = self.0;
        self.0 += 1;
        current_id
    }

    fn id(&self) -> usize {
        self.0
    }
}
