use std::{collections::HashMap, marker::PhantomData};

use codespan::FileId;
use log::debug;
use nickel_lang_core::{
    identifier::Ident,
    position::TermPos,
    term::{
        record::{Field, FieldMetadata},
        RichTerm, Term, UnaryOp,
    },
    typ::TypeF,
    typecheck::{
        linearization::{Linearization, Linearizer},
        reporting::NameReg,
        UnifType,
    },
};

use crate::{
    field_walker::DefWithPath, identifier::LocIdent, position::PositionLookup, term::RichTermPtr,
    usage::UsageLookup,
};

use self::{
    building::Building,
    completed::Completed,
    interface::{ResolutionState, Resolved, TermKind, UsageState, ValueState},
};

pub mod building;
pub mod completed;
pub mod interface;

pub type Environment = nickel_lang_core::environment::Environment<Ident, ItemId>;

/// A registry mapping file ids to their corresponding linearization. The registry stores the
/// linearization of every file that has been imported and analyzed, including the main open
/// document.
///
/// `LinRegistry` wraps some methods of [completed::Completed] by first selecting the linearization
/// with a matching file id and then calling to the corresponding method on it.
#[derive(Clone, Default, Debug)]
pub struct LinRegistry {
    pub map: HashMap<FileId, Completed>,
    // TODO: these are supposed to eventually *replace* part of the linearization, at
    // which point we'll rename `LinRegistry` (and probably just have one HashMap<FileId, everything>)
    pub position_lookups: HashMap<FileId, PositionLookup>,
    pub usage_lookups: HashMap<FileId, UsageLookup>,
    pub type_lookups: HashMap<RichTermPtr, Type>,
}

impl LinRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(
        &mut self,
        file_id: FileId,
        linearization: Completed,
        type_lookups: HashMap<RichTermPtr, Type>,
        term: &RichTerm,
    ) {
        self.map.insert(file_id, linearization);
        self.position_lookups
            .insert(file_id, PositionLookup::new(term));
        self.usage_lookups.insert(file_id, UsageLookup::new(term));

        self.type_lookups.extend(type_lookups);
    }

    /// Look for the linearization corresponding to an item's id, and return the corresponding item
    /// from the linearization, if any.
    pub fn get_item(&self, id: ItemId) -> Option<&LinearizationItem<Resolved>> {
        let lin = self.map.get(&id.file_id).unwrap();
        lin.get_item(id)
    }

    pub fn get_def(&self, ident: &LocIdent) -> Option<&DefWithPath> {
        let file = ident.pos.as_opt_ref()?.src_id;
        self.usage_lookups.get(&file)?.def(ident)
    }

    pub fn get_env(&self, rt: &RichTerm) -> Option<&crate::usage::Environment> {
        let file = rt.pos.as_opt_ref()?.src_id;
        self.usage_lookups.get(&file)?.env(rt)
    }
}

#[derive(PartialEq, Copy, Debug, Clone, Eq, Hash)]
pub struct ItemId {
    pub file_id: FileId,
    pub index: usize,
}

/// A recorded item of a given state of resolution state
/// Tracks a unique id used to build a reference table after finalizing
/// the linearization using the LSP [AnalysisHost]
#[derive(Debug, Clone, PartialEq)]
pub struct LinearizationItem<S: ResolutionState> {
    /// The term that spawned this linearization item. Note that there is not a one-to-one
    /// relationship between terms and linearization items. For example, a single `let` term
    /// with a pattern binding can create multiple `declaration` linearization items.
    pub term: RichTerm,
    pub env: Environment,
    pub id: ItemId,
    /// This is not necessarily the same as `term.pos`, for example if this linearization
    /// item points to a single pattern binding in a `let` term then `pos` will be the position
    /// of just the identifier.
    pub pos: TermPos,
    pub ty: S,
    pub kind: TermKind,
    pub metadata: Option<FieldMetadata>,
}

/// [Linearizer] used by the LSP
///
/// Tracks a _scope stable_ environment managing variable ident
/// resolution
pub struct AnalysisHost<'a> {
    // We need the lifetime on `AnalysisHost` to be able to
    // have a lifetime for the associated `Building` data structure.
    phantom: PhantomData<&'a usize>,
    file: FileId,
    env: Environment,
    meta: Option<FieldMetadata>,
    /// Indexing a record will store a reference to the record as
    /// well as its fields.
    /// `Self::scope` will produce a host with a single **`pop`ed**
    /// Ident. As fields are typechecked in the same order, each
    /// in their own scope immediately after the record, which
    /// gives the corresponding record field _term_ to the ident
    /// useable to construct a vale declaration.
    record_fields: Option<(ItemId, Vec<(ItemId, LocIdent)>)>,
    bindings: Option<Vec<ItemId>>,
    /// Accesses to nested records are recorded recursively.
    ///
    /// ```
    /// outer.middle.inner -> inner(middle(outer))
    /// ```
    ///
    /// To resolve those inner fields, accessors (`inner`, `middle`)
    /// are recorded first until a variable (`outer`) is found.
    /// Then, access to all nested records are resolved at once.
    access: Option<Vec<LocIdent>>,
}

impl<'a> AnalysisHost<'a> {
    pub fn new(file: FileId, env: Environment) -> Self {
        Self {
            phantom: PhantomData,
            file,
            env,
            meta: Default::default(),
            record_fields: Default::default(),
            bindings: Default::default(),
            access: Default::default(),
        }
    }

    fn next_id(&self, lin: &Linearization<Building>) -> ItemId {
        ItemId {
            file_id: self.file,
            index: lin.next_id(),
        }
    }

    // Helper with logic common to let/fun and let pattern/fun pattern.
    //
    // If the term is a let-binding, this function calls to the provided `push` callback to add the next
    // item id to the relevant list of bindings.
    //
    // If the term is a function, this function generates a stub item for it.
    //
    // Returns the value state of the corresponding declaration.
    //
    // Panic if `rt` is neither a let/let pattern nor a fun/fun pattern.
    fn setup_decl(
        &mut self,
        lin: &mut Linearization<Building>,
        rt: &RichTerm,
        ty: &UnifType,
        pos: TermPos,
        push: impl FnOnce(ItemId),
    ) -> ValueState {
        let next_id = self.next_id(lin);

        match rt.as_ref() {
            Term::LetPattern(..) | Term::Let(..) => {
                push(next_id);
                ValueState::Unknown
            }
            Term::FunPattern(..) | Term::Fun(..) => {
                // stub object, representing the whole function
                lin.push(LinearizationItem {
                    env: self.env.clone(),
                    term: rt.clone(),
                    id: next_id,
                    ty: ty.clone(),
                    pos,
                    kind: TermKind::Structure,
                    metadata: self.meta.take(),
                });

                ValueState::Known(self.next_id(lin))
            }
            _ => panic!("expected only a let pattern or a fun pattern in this function"),
        }
    }
}

use nickel_lang_core::typ::Type;
use nickel_lang_core::typecheck::Extra;
impl<'a> Linearizer for AnalysisHost<'a> {
    type Building = Building<'a>;
    type Completed = Completed;
    type CompletionExtra = Extra;
    type ItemId = ItemId;

    fn add_term(
        &mut self,
        lin: &mut Linearization<Building>,
        rt: &RichTerm,
        ty: UnifType,
    ) -> Option<ItemId> {
        let pos = rt.pos;
        let term = rt.term.as_ref();
        debug!("adding term: {:?} @ {:?}", term, pos);

        // The id of the main item added. Some terms give rise to several items (records or
        // functions for example), but this is the id of the first item added, which is considered
        // to be the item attached to the term - the rest is extra declaration/usage/etc
        let main_id = ItemId {
            file_id: self.file,
            index: lin.next_id(),
        };

        // Register record field if appropriate
        // `record` is the id [LinearizatonItem] of the enclosing record
        // `offset` is used to find the [LinearizationItem] representing the field
        // Field items are inserted immediately after the record
        if !matches!(
            term,
            Term::Op1(UnaryOp::StaticAccess(_), _) | Term::Annotated(..)
        ) {
            if let Some((record, (offset, _))) = self
                .record_fields
                // We call take because each record field will be linearized in a different scope.
                // In particular, each record field gets its own copy of `record_fields`, so we can
                // take it.
                .take()
                .map(|(record, mut fields)| (record, fields.pop().unwrap()))
            {
                if let Some(field) = lin.linearization.get_mut(record.index + offset.index) {
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
                            *value = ValueState::Known(ItemId {
                                file_id: self.file,
                                index: main_id.index + usage_offset,
                            });
                        }
                        // The linearization item of a record with n fields is expected to be
                        // followed by n linearization items representing each field
                        _ => unreachable!(),
                    }
                }
            }

            if let Some(decls) = self.bindings.take() {
                let offset = self.access.as_ref().map(|v| v.len()).unwrap_or(0);
                for decl in decls {
                    lin.inform_declaration(
                        self.file,
                        decl,
                        ItemId {
                            file_id: self.file,
                            index: main_id.index + offset,
                        },
                    );
                }
            }
        }

        if pos == TermPos::None {
            return None;
        }

        match term {
            Term::LetPattern(ident, destruct, ..) | Term::FunPattern(ident, destruct, _) => {
                let mut pattern_bindings = Vec::new();

                if let Some(ident) = ident {
                    let value_ptr =
                        self.setup_decl(lin, rt, &ty, pos, |id| pattern_bindings.push(id));

                    let next_id = self.next_id(lin);
                    self.env.insert(ident.ident(), next_id);

                    let kind = TermKind::Declaration {
                        id: ident.to_owned(),
                        usages: Vec::new(),
                        value: value_ptr,
                        path: None,
                    };

                    lin.push(LinearizationItem {
                        env: self.env.clone(),
                        term: rt.clone(),
                        id: next_id,
                        ty,
                        pos: ident.pos,
                        kind,
                        metadata: None,
                    });
                }

                for (path, bind_ident, field) in destruct
                    .to_owned()
                    .inner()
                    .into_iter()
                    .flat_map(|matched| matched.to_flattened_bindings())
                {
                    let decl_id = self.next_id(lin);

                    pattern_bindings.push(decl_id);
                    self.env.insert(bind_ident.ident(), decl_id);

                    lin.push(LinearizationItem {
                        env: self.env.clone(),
                        term: rt.clone(),
                        id: decl_id,
                        // TODO: get type from pattern
                        ty: UnifType::concrete(TypeF::Dyn),
                        pos: bind_ident.pos,
                        kind: TermKind::Declaration {
                            id: bind_ident,
                            usages: Vec::new(),
                            value: ValueState::Unknown,
                            path: Some(path),
                        },
                        metadata: Some(field.metadata),
                    });
                }

                self.bindings = Some(pattern_bindings);
            }
            Term::Let(ident, ..) | Term::Fun(ident, ..) => {
                let mut binding = None;

                let value_ptr = self.setup_decl(lin, rt, &ty, pos, |id| binding = Some(id));
                self.bindings = binding.map(|id| vec![id]);

                let next_id = self.next_id(lin);
                self.env.insert(ident.ident(), next_id);

                let kind = TermKind::Declaration {
                    id: ident.to_owned(),
                    usages: Vec::new(),
                    value: value_ptr,
                    path: None,
                };

                lin.push(LinearizationItem {
                    env: self.env.clone(),
                    term: rt.clone(),
                    id: next_id,
                    ty,
                    pos: ident.pos,
                    kind,
                    metadata: self.meta.take(),
                });
            }
            Term::Var(ident) => {
                debug!(
                    "adding usage of variable {} followed by chain {:?}",
                    ident, self.access
                );

                let pointed = self.env.get(&ident.ident()).copied();

                lin.push(LinearizationItem {
                    env: self.env.clone(),
                    term: rt.clone(),
                    id: main_id,
                    pos: ident.pos,
                    ty: UnifType::concrete(TypeF::Dyn),
                    kind: TermKind::Usage(UsageState::from(pointed)),
                    metadata: self.meta.take(),
                });

                if let Some(referenced) = pointed {
                    lin.add_usage(self.file, referenced, main_id)
                }

                if let Some(chain) = self.access.take() {
                    let chain: Vec<_> = chain.into_iter().rev().collect();

                    for accessor in chain.iter() {
                        let id = self.next_id(lin);

                        lin.push(LinearizationItem {
                            env: self.env.clone(),
                            term: rt.clone(),
                            id,
                            pos: accessor.pos,
                            ty: UnifType::concrete(TypeF::Dyn),
                            kind: TermKind::Usage(UsageState::Deferred {
                                parent: ItemId {
                                    file_id: self.file,
                                    index: id.index - 1,
                                },
                                child: accessor.to_owned().into(),
                            }),
                            metadata: self.meta.take(),
                        });
                    }
                }
            }
            Term::Record(record) | Term::RecRecord(record, ..) => {
                lin.push(LinearizationItem {
                    env: self.env.clone(),
                    term: rt.clone(),
                    id: main_id,
                    pos,
                    ty,
                    kind: TermKind::Record(HashMap::new()),
                    metadata: self.meta.take(),
                });

                lin.register_fields(self.file, rt, &record.fields, main_id, &mut self.env);
                let mut field_names = record.fields.keys().cloned().collect::<Vec<_>>();
                field_names.sort_unstable();

                self.record_fields = Some((
                    ItemId {
                        file_id: main_id.file_id,
                        index: main_id.index + 1,
                    },
                    field_names
                        .into_iter()
                        .enumerate()
                        .map(|(id, ident)| {
                            (
                                ItemId {
                                    file_id: self.file,
                                    index: id,
                                },
                                ident.into(),
                            )
                        })
                        .rev()
                        .collect(),
                ));
            }
            Term::Op1(UnaryOp::StaticAccess(ident), _) => {
                let x = self.access.get_or_insert(Vec::with_capacity(1));
                x.push(ident.to_owned().into())
            }
            Term::Annotated(annot, _) => {
                // Notice 1: No push to lin for the `FieldMetadata` itself
                // Notice 2: we discard the encoded value as anything we
                //           would do with the value will be handled in the following
                //           call to [Self::add_term]
                self.meta = Some(FieldMetadata {
                    annotation: annot.clone(),
                    ..Default::default()
                })
            }
            Term::ResolvedImport(file) => {
                fn final_term_pos(term: &RichTerm) -> &TermPos {
                    let RichTerm { term, pos } = term;
                    match term.as_ref() {
                        Term::Let(_, _, body, _) | Term::LetPattern(_, _, _, body) => {
                            final_term_pos(body)
                        }
                        Term::Op1(UnaryOp::StaticAccess(field), _) => &field.pos,
                        _ => pos,
                    }
                }

                lin.import_locations.insert(*file, pos);

                let linearization = lin.lin_registry.map.get(file)?;

                // This is safe because the import file is resolved before we linearize the
                // containing file, therefore the cache MUST have the term stored.
                let term = lin.cache.get_owned(*file).unwrap();
                let position = final_term_pos(&term);

                // unwrap(): this unwrap fails only when position is a `TermPos::None`, which only
                // happens if the `RichTerm` has been transformed or evaluated. None of these
                // happen before linearization.
                let term_id = linearization.item_at(position.unwrap().start_pos())?.id;

                lin.push(LinearizationItem {
                    env: self.env.clone(),
                    term: rt.clone(),
                    id: main_id,
                    pos,
                    ty,
                    kind: TermKind::Usage(UsageState::Resolved(term_id)),
                    metadata: self.meta.take(),
                })
            }
            Term::Type(t) => {
                lin.push(LinearizationItem {
                    env: self.env.clone(),
                    term: rt.clone(),
                    id: main_id,
                    pos,
                    ty,
                    kind: TermKind::Type(t.clone()),
                    metadata: self.meta.take(),
                });
            }
            other => {
                debug!("Add wildcard item: {:?}", other);

                lin.push(LinearizationItem {
                    env: self.env.clone(),
                    term: rt.clone(),
                    id: main_id,
                    pos,
                    ty,
                    kind: TermKind::Structure,
                    metadata: self.meta.take(),
                })
            }
        }

        Some(main_id)
    }

    fn add_field_metadata(&mut self, _lin: &mut Linearization<Building>, field: &Field) {
        // Notice 1: No push to lin for the `FieldMetadata` itself
        // Notice 2: we discard the encoded value as anything we
        //           would do with the value will be handled in the following
        //           call to [Self::add_term]
        self.meta = Some(field.metadata.clone())
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
        debug!("linearizing {:?}", self.file);
        let mut name_reg = NameReg::new(reported_names);

        // TODO: Storing defers while linearizing?
        let mut defers: Vec<_> = lin
            .linearization
            .iter()
            .filter_map(|item| match &item.kind {
                TermKind::Usage(UsageState::Deferred { parent, child }) => {
                    Some((item.id, *parent, child.ident()))
                }
                _ => None,
            })
            .collect();

        defers.reverse();
        let unresolved = lin.resolve_record_references(self.file, defers);
        debug!("unresolved references: {:?}", unresolved);

        let Building {
            mut linearization,
            import_locations,
            ..
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

        fn transform_wildcard(wildcards: &[Type], t: Type) -> Type {
            match t.typ {
                TypeF::Wildcard(i) => wildcards.get(i).unwrap_or(&t).clone(),
                _ => t,
            }
        }
        // resolve types
        let lin_: Vec<_> = linearization
            .into_iter()
            .map(
                |LinearizationItem {
                     env,
                     term,
                     id,
                     pos,
                     ty,
                     kind,
                     metadata: meta,
                 }| LinearizationItem {
                    ty: name_reg.to_type(&table, ty),
                    term,
                    env,
                    id,
                    pos,
                    kind,
                    metadata: meta,
                },
            )
            .map(|item| LinearizationItem {
                ty: transform_wildcard(&wildcards, item.ty),
                ..item
            })
            .collect();
        Linearization::new(Completed::new(lin_, id_mapping, import_locations))
    }

    fn scope(&mut self) -> Self {
        AnalysisHost {
            phantom: PhantomData,
            file: self.file,
            env: self.env.clone(),
            meta: self.meta.clone(),
            record_fields: self.record_fields.as_mut().and_then(|(record, fields)| {
                Some(*record).zip(fields.pop().map(|field| vec![field]))
            }),
            bindings: self.bindings.take(),
            access: self.access.clone(),
        }
    }

    fn scope_meta(&mut self) -> Self {
        AnalysisHost {
            phantom: PhantomData,
            file: self.file,
            env: self.env.clone(),
            // Metadata must be attached to the original scope of the value (`self`), while the new
            // scope for metadata should be clean.
            // In general, the scope for the metadata shouldn't interfere with any of the previous
            // state (record fields, let binding, etc.), which is kept intact inside `self`, while
            // this new scope gets a cleared stated.
            meta: None,
            record_fields: None,
            bindings: None,
            access: None,
        }
    }

    fn retype_ident(
        &mut self,
        lin: &mut Linearization<Building>,
        ident: &nickel_lang_core::identifier::LocIdent,
        new_type: UnifType,
    ) {
        if let Some(item) = self
            .env
            .get(&ident.ident())
            .and_then(|item_id| lin.linearization.get_mut(item_id.index))
        {
            debug!("retyping {:?} to {:?}", ident, new_type);
            item.ty = new_type;
        } else {
            debug!(
                "retype_indent failed! Environment miss: {}",
                self.env.get(&ident.ident()).is_none()
            );
        }
    }

    fn retype(
        &mut self,
        lin: &mut Linearization<Building>,
        item_id: Option<ItemId>,
        new_type: UnifType,
    ) {
        let Some(item_id) = item_id else {
            return;
        };

        if let Some(item) = lin.linearization.get_mut(item_id.index) {
            debug!("retyping item {:?} to {:?}", item_id, new_type);
            item.ty = new_type;
        } else {
            debug!("retype item failed (item not found)!");
        }
    }
}

pub struct TypeCollector {}

impl Linearizer for TypeCollector {
    type Building = HashMap<RichTermPtr, UnifType>;
    type Completed = HashMap<RichTermPtr, Type>;
    type CompletionExtra = Extra;

    fn scope(&mut self) -> Self {
        TypeCollector {}
    }

    fn scope_meta(&mut self) -> Self {
        TypeCollector {}
    }

    fn add_term(&mut self, lin: &mut Linearization<Self::Building>, rt: &RichTerm, ty: UnifType) {
        lin.insert(RichTermPtr(rt.clone()), ty);
    }

    fn complete(
        self,
        lin: Linearization<Self::Building>,
        Extra {
            table,
            names,
            wildcards,
        }: Extra,
    ) -> Linearization<Self::Completed> {
        let mut name_reg = NameReg::new(names);

        let mut transform_type = |uty: UnifType| -> Type {
            let ty = name_reg.to_type(&table, uty);
            match ty.typ {
                TypeF::Wildcard(i) => wildcards.get(i).unwrap_or(&ty).clone(),
                _ => ty,
            }
        };

        Linearization::new(
            lin.into_inner()
                .into_iter()
                .map(|(rt, uty)| (rt, transform_type(uty)))
                .collect(),
        )
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
