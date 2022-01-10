use std::{collections::HashMap, mem};

use codespan::ByteIndex;
use log::debug;
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
    fn resolve_reference<'a>(
        &'a self,
        item: &'a LinearizationItem<TypeWrapper>,
    ) -> Option<&'a LinearizationItem<TypeWrapper>>;
    fn resolve_record_references(&mut self, defers: Vec<(usize, usize, Ident)>);
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
        for (ident, value) in record_fields.iter() {
            let id = self.id_gen().get_and_advance();
            self.push(LinearizationItem {
                id,
                pos: ident.pos,
                // temporary, the actual type is resolved later and the item retyped
                ty: TypeWrapper::Concrete(AbsType::Dyn()),
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

    fn resolve_reference<'a>(
        &'a self,
        item: &'a LinearizationItem<TypeWrapper>,
    ) -> Option<&'a LinearizationItem<TypeWrapper>> {
        // if item is a usage, resolve the usage first
        match item.kind {
            TermKind::Usage(UsageState::Resolved(Some(pointed))) => {
                self.state.resource.linearization.get(pointed)
            }
            _ => Some(item),
        }
        // load referenced value, either from record field or declaration
        .and_then(|item_pointer| {
            match item_pointer.kind {
                // if declaration is a record field, resolve its value
                TermKind::RecordField { value, .. } => {
                    debug!("parent referenced a record field {:?}", value);
                    value
                        // retrieve record
                        .and_then(|value_index| self.state.resource.linearization.get(value_index))
                }
                // if declaration is a let biding resolve its value
                TermKind::Declaration(_, _) => {
                    self.state.resource.linearization.get(item_pointer.id + 1)
                }

                // if something else was referenced, stop.
                _ => Some(item_pointer),
            }
        })
    }

    fn resolve_record_references(&mut self, mut defers: Vec<(usize, usize, Ident)>) {
        let mut unresolved: Vec<(usize, usize, Ident)> = Vec::new();

        while let Some(deferred) = defers.pop() {
            // child_item: current deferred usage item
            //       i.e.: root.<child>
            // parent_accessor_id: id of the parent usage
            //               i.e.: <parent>.child
            // child_ident: identifier the child item references
            let (child_item, parent_accessor_id, child_ident) = &deferred;
            // resolve the value referenced by the parent accessor element
            // get the parent accessor, and read its resolved reference
            let parent_referenced = self.state.resource.linearization.get(*parent_accessor_id);

            if let Some(LinearizationItem {
                kind: TermKind::Usage(UsageState::Deferred { .. }),
                ..
            }) = parent_referenced
            {
                debug!("parent references deferred usage");
                unresolved.push(deferred.clone());
                continue;
            }

            // load the parent referenced declaration (i.e.: a declaration or record field term)
            let parent_declaration = parent_referenced
                .and_then(|parent_usage_value| self.resolve_reference(parent_usage_value));

            if let Some(LinearizationItem {
                kind: TermKind::Usage(UsageState::Deferred { .. }),
                ..
            }) = parent_declaration
            {
                debug!("parent references deferred usage");
                unresolved.push(deferred.clone());
                continue;
            }

            let referenced_declaration = parent_declaration
                // resolve indirection by following the usage
                .and_then(|parent_declaration| self.resolve_reference(parent_declaration))
                // get record field
                .and_then(|parent_declaration| match &parent_declaration.kind {
                    TermKind::Record(fields) => {
                        fields.get(child_ident).and_then(|child_declaration_id| {
                            self.state.resource.linearization.get(*child_declaration_id)
                        })
                    }
                    _ => None,
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
                                fields.get(child_ident).and_then(|accessor_id| {
                                    self.state.resource.linearization.get(*accessor_id)
                                })
                            }
                            TermKind::Usage(UsageState::Resolved(Some(pointed))) => {
                                self.state.resource.linearization.get(*pointed)
                            }
                            _ => None,
                        })
                        .or(Some(referenced)),

                    _ => Some(referenced),
                });

            let referenced_id = referenced_declaration.map(|reference| reference.id);

            debug!(
                "Associating child {} to value {:?}",
                child_ident, referenced_declaration
            );

            {
                let child: &mut LinearizationItem<TypeWrapper> = self
                    .state
                    .resource
                    .linearization
                    .get_mut(*child_item)
                    .unwrap();
                child.kind = TermKind::Usage(UsageState::Resolved(referenced_id));
            }

            if let Some(referenced_id) = referenced_id {
                self.add_usage(referenced_id, *child_item);
            }

            if defers.is_empty() && !unresolved.is_empty() {
                debug!("unresolved references: {:?}", unresolved);
                defers = mem::take(&mut unresolved);
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
                            *value = Some(id_gen.get() + usage_offset);
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

        let id = id_gen.get();
        match term {
            Term::Let(ident, _, _, _) | Term::Fun(ident, _) => {
                self.env
                    .insert(ident.to_owned(), lin.state.resource.linearization.len());
                lin.push(LinearizationItem {
                    id,
                    ty,
                    pos: ident.pos,
                    scope: self.scope.clone(),
                    kind: TermKind::Declaration(ident.to_owned(), Vec::new()),
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
                    pos: ident.pos,
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
                        let id = id_gen.get_and_advance();
                        lin.push(LinearizationItem {
                            id,
                            pos: accessor.pos,
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
            Term::Record(fields, _) | Term::RecRecord(fields, _, _, _) => {
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
                    if let nickel::types::AbsType::Flat(RichTerm { term, pos: _ }) =
                        &contract.types.0
                    {
                        if let Term::Var(ident) = &**term {
                            let parent = self.env.get(ident);
                            let id = id_gen.get_and_advance();
                            lin.push(LinearizationItem {
                                id,
                                pos: ident.pos,
                                ty: TypeWrapper::Concrete(AbsType::Var(ident.to_owned())),
                                scope: self.scope.clone(),
                                // id = parent: full let binding including the body
                                // id = parent + 1: actual delcaration scope, i.e. _ = < definition >
                                kind: TermKind::Usage(UsageState::Resolved(parent)),
                                meta: None,
                            });
                            if let Some(parent) = parent {
                                lin.add_usage(parent, id);
                            }
                        }
                    }
                }

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
        debug!("linearizing");

        // TODO: Storing defers while linearizing?
        let defers: Vec<(usize, usize, Ident)> = lin
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

        lin.resolve_record_references(defers);

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
