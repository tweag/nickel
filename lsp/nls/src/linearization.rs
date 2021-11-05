use std::collections::HashMap;

use log::debug;
use nickel::{
    identifier::Ident,
    position::TermPos,
    term::{MetaValue, RichTerm, Term},
    typecheck::{
        linearization::{
            Building, Completed, Environment, Linearization, LinearizationItem, Linearizer,
            ScopeId, TermKind, Unresolved,
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
    record_fields: Option<(usize, Vec<Ident>)>,
}

impl AnalysisHost {
    pub fn new() -> Self {
        AnalysisHost {
            env: Environment::new(),
            scope: Vec::new(),
            meta: None,
            record_fields: None,
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
        if let Some((record, field)) = self
            .record_fields
            .take()
            .map(|(record, mut fields)| (record, fields.pop().unwrap()))
        {
            // 0. If the record_fields field is set, it is set by [Self::scope]
            //    such that exactly one element/field is present
            // 1. record fields have a position
            // 2. the type of the field is the type of its value
            // 3. the scope of the field is the scope of its parent (the record)
            let meta = match term {
                Term::MetaValue(meta) => Some(MetaValue {
                    value: None,
                    ..meta.clone()
                }),
                _ => None,
            };

            lin.push(LinearizationItem {
                id: id_gen.take(),
                pos: field.1.unwrap(),
                ty: ty.clone(),
                kind: TermKind::RecordField {
                    record,
                    body_pos: match pos {
                        TermPos::None => field.1.unwrap(),
                        _ => pos.clone(),
                    },
                    ident: field.clone(),
                    usages: Vec::new(),
                },
                scope: self.scope[..self.scope.len() - 1].to_vec(),
                meta,
            });

            if pos == TermPos::None {
                pos = field.1.unwrap();
            }
        }

        if pos == TermPos::None {
            return;
        }

        let id = id_gen.id();
        match term {
            Term::Let(ident, _, _) => {
                self.env
                    .insert(ident.to_owned(), lin.state.resource.linearization.len());
                lin.push(LinearizationItem {
                    id,
                    ty,
                    pos: ident.1.unwrap(),
                    scope: self.scope.clone(),
                    kind: TermKind::Declaration(ident.to_owned(), Vec::new()),
                    meta: self.meta.take(),
                });
            }
            Term::Var(ident) => {
                let parent = self.env.get(ident);
                lin.push(LinearizationItem {
                    id,
                    pos,
                    ty,
                    scope: self.scope.clone(),
                    // id = parent: full let binding including the body
                    // id = parent + 1: actual delcaration scope, i.e. _ = < definition >
                    kind: TermKind::Usage(parent.map(|id| id)),
                    meta: self.meta.take(),
                });
                if let Some(parent) = parent {
                    lin.add_usage(parent, id);
                }
            }
            Term::Record(fields, _) | Term::RecRecord(fields, _, _) => {
                self.record_fields = Some((
                    id,
                    fields
                        .keys()
                        .cloned()
                        .collect::<Vec<_>>()
                        .into_iter()
                        .rev()
                        .collect(),
                ));

                lin.push(LinearizationItem {
                    id,
                    pos,
                    ty,
                    kind: TermKind::Record(Vec::new()),
                    scope: self.scope.clone(),
                    meta: self.meta.take(),
                });
            }

            Term::MetaValue(meta) => {
                // Notice 1: No push to lin
                // Notice 2: we discard the encoded value as anything we
                //           would do with the value will be handled in the following
                //           call to [Self::add_term]
                let meta = MetaValue {
                    value: None,
                    ..meta.to_owned()
                };

                for contract in meta.contracts.iter().cloned() {
                    match contract.types.0 {
                        // Note: we extract the
                        nickel::types::AbsType::Flat(RichTerm { term, pos: _ }) => {
                            match *term {
                                Term::Var(ident) => {
                                    let parent = self.env.get(&ident);
                                    let id = id_gen.take();
                                    lin.push(LinearizationItem {
                                        id,
                                        pos: ident.1.unwrap(),
                                        ty: TypeWrapper::Concrete(AbsType::Var(ident)),
                                        scope: self.scope.clone(),
                                        // id = parent: full let binding including the body
                                        // id = parent + 1: actual delcaration scope, i.e. _ = < definition >
                                        kind: TermKind::Usage(parent.map(|id| id)),
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
                    self.meta.insert(meta);
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
        lin: Linearization<Building<BuildingResource>>,
        (table, reported_names): (UnifTable, HashMap<usize, Ident>),
    ) -> Linearization<Completed> {
        let mut lin_ = lin.state.resource.linearization;
        eprintln!("linearizing");
        lin_.sort_by_key(|item| match item.pos {
            TermPos::Original(span) => (span.src_id, span.start),
            TermPos::Inherited(span) => (span.src_id, span.start),
            TermPos::None => {
                eprintln!("{:?}", item);

                unreachable!()
            }
        });

        let mut id_mapping = HashMap::new();
        lin_.iter()
            .enumerate()
            .for_each(|(index, LinearizationItem { id, .. })| {
                id_mapping.insert(*id, index);
            });

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
