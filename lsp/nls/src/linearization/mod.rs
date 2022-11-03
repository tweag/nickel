use std::collections::HashMap;

use codespan::ByteIndex;
use log::debug;
use nickel_lang::{
    identifier::Ident,
    position::TermPos,
    term::{MetaValue, Term, UnaryOp},
    typecheck::{
        linearization::{Linearization, Linearizer, Scope, ScopeId},
        reporting::{to_type, NameReg},
        TypeWrapper,
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

pub type Environment = nickel_lang::environment::Environment<Ident, usize>;

/// A recorded item of a given state of resolution state
/// Tracks a unique id used to build a reference table after finalizing
/// the linearization using the LSP [AnalysisHost]
#[derive(Debug, Clone, PartialEq)]
pub struct LinearizationItem<S: ResolutionState> {
    //term_: Box<Term>,
    pub id: usize,
    pub pos: TermPos,
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

use nickel_lang::typecheck::Extra;
use nickel_lang::types::Types;
impl Linearizer for AnalysisHost {
    type Building = Building;
    type Completed = Completed;
    type CompletionExtra = Extra;

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
                    let value_ptr = match term {
                        Term::LetPattern(..) => {
                            self.let_binding = Some(id);
                            ValueState::Unknown
                        }
                        Term::FunPattern(..) => {
                            // stub object
                            lin.push(LinearizationItem {
                                id: id_gen.get_and_advance(),

                                ty: ty.clone(),
                                pos: ident.pos.clone(),
                                scope: self.scope.clone(),
                                kind: TermKind::Structure,
                                meta: self.meta.take(),
                            });

                            ValueState::Known(id_gen.get())
                        }
                        _ => unreachable!(),
                    };

                    let id = id_gen.get_and_advance();
                    self.env.insert(ident.to_owned(), id);
                    lin.push(LinearizationItem {
                        id,
                        ty,
                        pos: ident.pos,
                        scope: self.scope.clone(),
                        kind: TermKind::Declaration(ident.to_owned(), Vec::new(), value_ptr),
                        meta: None,
                    });
                }
                for matched in destruct.to_owned().inner() {
                    let (ident, term) = matched.as_meta_field();
                    let id = id_gen.get_and_advance();
                    self.env.insert(ident.to_owned(), id);
                    lin.push(LinearizationItem {
                        id,
                        // TODO: get type from pattern
                        ty: TypeWrapper::Concrete(AbsType::Dyn()),
                        pos: ident.pos,
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
            Term::Let(ident, ..) | Term::Fun(ident, ..) => {
                let value_ptr = match term {
                    Term::Let(..) => {
                        self.let_binding = Some(id);
                        ValueState::Unknown
                    }
                    Term::Fun(..) => {
                        // stub object
                        lin.push(LinearizationItem {
                            id: id_gen.get_and_advance(),

                            ty: ty.clone(),
                            pos: ident.pos.clone(),
                            scope: self.scope.clone(),
                            kind: TermKind::Structure,
                            meta: self.meta.take(),
                        });

                        ValueState::Known(id_gen.get())
                    }
                    _ => unreachable!(),
                };
                self.env.insert(ident.to_owned(), id_gen.get());
                lin.push(LinearizationItem {
                    id: id_gen.get(),
                    ty,
                    pos: ident.pos,
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
                    pos: ident.pos,
                    ty: TypeWrapper::Concrete(AbsType::Dyn()),
                    scope: self.scope.clone(),
                    kind: TermKind::Usage(UsageState::from(self.env.get(ident).copied())),
                    meta: self.meta.take(),
                });

                if let Some(referenced) = self.env.get(ident) {
                    lin.add_usage(*referenced, root_id)
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
            Term::Record(record) | Term::RecRecord(record, ..) => {
                lin.push(LinearizationItem {
                    id,
                    pos,
                    ty,
                    kind: TermKind::Record(HashMap::new()),
                    scope: self.scope.clone(),
                    meta: self.meta.take(),
                });

                lin.register_fields(&record.fields, id, self.scope.clone(), &mut self.env);
                let mut field_names = record.fields.keys().cloned().collect::<Vec<_>>();
                field_names.sort_unstable();

                self.record_fields =
                    Some((id + 1, field_names.into_iter().enumerate().rev().collect()));
            }
            Term::Op1(UnaryOp::StaticAccess(ident), _) => {
                let x = self.access.get_or_insert(Vec::with_capacity(1));
                x.push(ident.to_owned())
            }
            Term::MetaValue(meta) => {
                // Notice 1: No push to lin for the `MetaValue` itself
                // Notice 2: we discard the encoded value as anything we
                //           would do with the value will be handled in the following
                //           call to [Self::add_term]
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
    fn complete(
        self,
        mut lin: Linearization<Building>,
        Extra {
            table,
            names: reported_names,
            wildcards,
        }: Extra,
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

        linearization.sort_by(
            |it1, it2| match (it1.pos.as_opt_ref(), it2.pos.as_opt_ref()) {
                (None, None) => std::cmp::Ordering::Equal,
                (None, _) => std::cmp::Ordering::Less,
                (_, None) => std::cmp::Ordering::Greater,
                (Some(pos1), Some(pos2)) => {
                    (pos1.src_id, pos1.start).cmp(&(pos2.src_id, pos2.start))
                }
            },
        );

        // create an index of id -> new position
        let mut id_mapping = HashMap::new();
        linearization
            .iter()
            .enumerate()
            .for_each(|(index, LinearizationItem { id, .. })| {
                id_mapping.insert(*id, index);
            });

        fn transform_wildcard(wildcars: &Vec<Types>, t: Types) -> Types {
            match t {
                Types(AbsType::Wildcard(i)) => wildcars.get(i).unwrap_or(&t).clone(),
                _ => t,
            }
        }
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
            .map(|item| LinearizationItem {
                ty: transform_wildcard(&wildcards, item.ty),
                ..item
            })
            .collect();

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
            meta: self.meta.clone(),
            record_fields: self.record_fields.as_mut().and_then(|(record, fields)| {
                Some(*record).zip(fields.pop().map(|field| vec![field]))
            }),
            let_binding: self.let_binding.take(),
            access: self.access.clone(),
        }
    }

    fn scope_meta(&mut self) -> Self {
        let mut scope = self.scope.clone();
        let (scope_id, next_scope_id) = self.next_scope_id.next();
        self.next_scope_id = next_scope_id;

        scope.push(scope_id);

        AnalysisHost {
            scope,
            env: self.env.clone(),
            next_scope_id: ScopeId::default(),
            // Metadata must be attached to the original scope of the value (`self`), while the new
            // scope for metadata should be clean.
            // In general, the scope for the metadata shouldn't interfere with any of the previous
            // state (record fields, let binding, etc.), which is kept intact inside `self`, while
            // this new scope gets a cleared stated.
            meta: None,
            record_fields: None,
            let_binding: None,
            access: None,
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
            .and_then(|index| lin.linearization.get_mut(*index))
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
