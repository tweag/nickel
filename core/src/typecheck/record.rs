//! Typechecking records.
//!
//! Because record literal definitions are flexible in Nickel (piecewise definitions), they need
//! a bit of preprocessing before they can be typechecked. Preprocessing and typechecking of
//! records is handled in this module.
use super::*;
use crate::{
    bytecode::ast::record::{FieldDef, FieldPathElem, Record},
    combine::Combine,
    position::TermPos,
};

use std::iter;

use indexmap::{map::Entry, IndexMap};

pub(super) trait Resolve<'ast> {
    type Resolved;

    fn resolve(&'ast self) -> Self::Resolved;
}

/// A resolved record literal, without field paths or piecewise definitions. Piecewise definitions
/// of fields have been grouped together, paths have been broken into proper levels and top-level
/// fields are partitioned between static and dynamic.
#[derive(Default, Debug)]
pub(super) struct ResolvedRecord<'ast> {
    /// The static fields of the record.
    pub stat_fields: IndexMap<LocIdent, ResolvedField<'ast>>,
    /// The dynamic fields of the record.
    pub dyn_fields: Vec<(&'ast Ast<'ast>, ResolvedField<'ast>)>,
    /// The position of the resolved record.
    pub pos: TermPos,
}

impl<'ast> ResolvedRecord<'ast> {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.stat_fields.is_empty() && self.dyn_fields.is_empty()
    }

    /// Checks a record with dynamic fields (and potentially static fields as well) against a type.
    ///
    /// # Preconditions
    ///
    /// This method assumes that `self.dyn_fields` is non-empty. Currently, violating this invariant
    /// shouldn't cause panic or unsoundness, but will unduly enforce that `ty` is a dictionary
    /// type.
    fn check_dyn<V: TypecheckVisitor<'ast>>(
        &self,
        state: &mut State<'ast, '_>,
        mut ctxt: Context<'ast>,
        visitor: &mut V,
        ty: UnifType<'ast>,
    ) -> Result<(), TypecheckError> {
        let ty_elts = state.table.fresh_type_uvar(ctxt.var_level);

        ty.unify(mk_uniftype::dict(ty_elts.clone()), state, &ctxt)
            .map_err(|err| err.into_typecheck_err(state, self.pos))?;

        for id in self.stat_fields.keys() {
            ctxt.type_env.insert(id.ident(), ty_elts.clone());
            visitor.visit_ident(id, ty_elts.clone())
        }

        for (expr, field) in &self.dyn_fields {
            expr.check(state, ctxt.clone(), visitor, mk_uniftype::str())?;
            field.check(state, ctxt.clone(), visitor, ty_elts.clone())?;
        }

        // We don't bind recursive fields in the term environment used to check for contract. See
        // [^term-env-rec-bindings] in `./mod.rs`.
        for (_, field) in self.stat_fields.iter() {
            field.check(state, ctxt.clone(), visitor, ty_elts.clone())?;
        }

        Ok(())
    }

    /// Checks a record with only static fields against a type.
    ///
    /// # Preconditions
    ///
    /// This method assumes that `self.dyn_fields` is empty. Currently, violating this invariant
    /// shouldn't cause panic or unsoundness, but will unduly enforce that `ty` is a dictionary
    /// type.
    fn check_stat<V: TypecheckVisitor<'ast>>(
        &self,
        state: &mut State<'ast, '_>,
        mut ctxt: Context<'ast>,
        visitor: &mut V,
        ty: UnifType<'ast>,
    ) -> Result<(), TypecheckError> {
        let root_ty = ty.clone().into_root(state.table);

        if let UnifType::Concrete {
            typ: TypeF::Dict {
                type_fields: rec_ty,
                ..
            },
            ..
        } = root_ty
        {
            // Checking mode for a dictionary
            for (_, field) in self.stat_fields.iter() {
                field.check(state, ctxt.clone(), visitor, (*rec_ty).clone())?;
            }

            Ok(())
        } else {
            // As records are recursive, we look at the apparent type of each field and bind it in ctxt
            // before actually typechecking the content of fields.
            //
            // Fields defined by interpolation are ignored, because they can't be referred to
            // recursively.

            // When we build the recursive environment, there are two different possibilities for each
            // field:
            //
            // 1. The field is annotated. In this case, we use this type to build the type environment.
            //    We don't need to do any additional check that the field respects this annotation:
            //    this will be handled by `check_field` when processing the field.
            // 2. The field isn't annotated. We are going to infer a concrete type later, but for now,
            //    we allocate a fresh unification variable in the type environment. In this case, once
            //    we have inferred an actual type for this field, we need to unify what's inside the
            //    environment with the actual type to ensure that they agree.
            //
            //  `need_unif_step` stores the list of fields corresponding to the case 2, which require
            //  this additional unification step. Note that performing the additional unification in
            //  case 1. should be harmless, but it's wasteful, and is also not entirely trivial because
            //  of polymorphism (we need to make sure to instantiate polymorphic type annotations). At
            //  the end of the day, it's simpler to skip unneeded unifications.
            let mut need_unif_step = HashSet::new();

            for (id, field) in &self.stat_fields {
                let uty_apprt = field.apparent_type(
                    state.ast_alloc,
                    Some(&ctxt.type_env),
                    Some(state.resolver),
                );

                // `Approximated` corresponds to the case where the type isn't obvious (annotation
                // or constant), and thus to case 2. above
                if matches!(uty_apprt, ApparentType::Approximated(_)) {
                    need_unif_step.insert(*id);
                }

                let uty = apparent_or_infer(state, uty_apprt, &ctxt, true);
                ctxt.type_env.insert(id.ident(), uty.clone());
                visitor.visit_ident(id, uty);
            }

            // We build a vector of unification variables representing the type of the fields of
            // the record.
            //
            // Since `IndexMap` guarantees a stable order of iteration, we use a vector instead of
            // hashmap here. To find the type associated to the field `foo`, retrieve the index of
            // `foo` in `self.stat_fields.keys()` and index into `field_types`.
            let field_types: Vec<UnifType<'ast>> =
                iter::repeat_with(|| state.table.fresh_type_uvar(ctxt.var_level))
                    .take(self.stat_fields.len())
                    .collect();

            // Build the type {id1 : ?a1, id2: ?a2, .., idn: ?an}, which is the type of the whole
            // record.
            let rows = self
                .stat_fields
                .keys()
                .rev()
                .zip(field_types.iter().rev())
                .fold(
                    mk_uty_record_row!(),
                    |acc, (id, row_ty)| mk_uty_record_row!((*id, row_ty.clone()); acc),
                );

            ty.unify(mk_uty_record!(; rows), state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, self.pos))?;

            for ((id, field), field_type) in self.stat_fields.iter().zip(field_types) {
                // unwrap(): `field_types` has exactly the same length as `self.stat_fields`, as it
                // was constructed with `.take(self.stat_fields.len()).collect()`.
                // let field_type = field_types.pop().unwrap();

                // For a recursive record and a field which requires the additional unification
                // step (whose type wasn't known when building the recursive environment), we
                // unify the actual type with the type affected in the typing environment
                // (which started as a fresh unification variable, but might have been unified
                // with a more concrete type if the current field has been used recursively
                // from other fields).
                if need_unif_step.contains(id) {
                    // unwrap(): if the field is in `need_unif_step`, it must be in the context.
                    let affected_type = ctxt.type_env.get(&id.ident()).cloned().unwrap();

                    field_type
                        .clone()
                        .unify(affected_type, state, &ctxt)
                        .map_err(|err| {
                            err.into_typecheck_err(
                                state,
                                field.pos(),
                                // field.value.as_ref().map(|v| v.pos).unwrap_or_default(),
                            )
                        })?;
                }

                field.check(state, ctxt.clone(), visitor, field_type)?;
            }

            Ok(())
        }
    }
}

impl<'ast> Check<'ast> for ResolvedRecord<'ast> {
    fn check<V: TypecheckVisitor<'ast>>(
        &self,
        state: &mut State<'ast, '_>,
        ctxt: Context<'ast>,
        visitor: &mut V,
        ty: UnifType<'ast>,
    ) -> Result<(), TypecheckError> {
        // If we have no dynamic fields, we can check the record against a record type or a
        // dictionary type, depending on `ty`.
        if self.dyn_fields.is_empty() {
            self.check_stat(state, ctxt, visitor, ty)
        }
        // If some fields are defined dynamically, the only potential type that works is `{_ : a}`
        // for some `a`.
        else {
            self.check_dyn(state, ctxt, visitor, ty)
        }
    }
}

impl<'ast> Walk<'ast> for ResolvedRecord<'ast> {
    fn walk<V: TypecheckVisitor<'ast>>(
        &self,
        state: &mut State<'ast, '_>,
        ctxt: Context<'ast>,
        visitor: &mut V,
    ) -> Result<(), TypecheckError> {
        for (ast, field) in self.dyn_fields.iter() {
            ast.walk(state, ctxt.clone(), visitor)?;
            field.walk(state, ctxt.clone(), visitor)?;
        }

        for (id, field) in self.stat_fields.iter() {
            let field_type = field_type(state, field_def, &ctxt, false);

            visitor.visit_ident(&id, field_type.clone());
            ctxt.type_env.insert(id.ident(), field_type);

            field.walk(state, ctxt.clone(), visitor)?;
        }

        Ok(())
    }
}

impl<'ast> Walk<'ast> for ResolvedField<'ast> {
    fn walk<V: TypecheckVisitor<'ast>>(
        &self,
        state: &mut State<'ast, '_>,
        ctxt: Context<'ast>,
        visitor: &mut V,
    ) -> Result<(), TypecheckError> {

    }
}

impl<'ast> Combine for ResolvedRecord<'ast> {
    fn combine(this: ResolvedRecord<'ast>, other: ResolvedRecord<'ast>) -> Self {
        use crate::eval::merge::split;

        let split::SplitResult {
            left,
            center,
            right,
        } = split::split(this.stat_fields, other.stat_fields);

        let mut stat_fields = IndexMap::with_capacity(left.len() + center.len() + right.len());

        stat_fields.extend(left);
        stat_fields.extend(right);

        for (id, (field1, field2)) in center.into_iter() {
            stat_fields.insert(id, Combine::combine(field1, field2));
        }

        let dyn_fields = this
            .dyn_fields
            .into_iter()
            .chain(other.dyn_fields)
            .collect();

        let pos = match (this.pos, other.pos) {
            // If only one of the two position is defined, we use it
            (pos, TermPos::None) | (TermPos::None, pos) => pos,
            // Otherwise, we don't know how to combine two disjoint positions of a piecewise
            // definition, so we just return `TermPos::None`.
            _ => TermPos::None,
        };

        ResolvedRecord {
            stat_fields,
            dyn_fields,
            pos,
        }
    }
}

/// A wrapper type around a record that has been resolved but hasn't yet got a position. This is
/// done to force the caller of [Record::resolve] to provide a position before doing anything else.
pub(super) struct PoslessResolvedRecord<'ast>(ResolvedRecord<'ast>);

impl<'ast> PoslessResolvedRecord<'ast> {
    pub(super) fn new(
        stat_fields: IndexMap<LocIdent, ResolvedField<'ast>>,
        dyn_fields: Vec<(&'ast Ast<'ast>, ResolvedField<'ast>)>,
    ) -> Self {
        PoslessResolvedRecord(ResolvedRecord {
            stat_fields,
            dyn_fields,
            pos: TermPos::None,
        })
    }

    pub(super) fn with_pos(self, pos: TermPos) -> ResolvedRecord<'ast> {
        let PoslessResolvedRecord(record) = self;

        ResolvedRecord { pos, ..record }
    }
}

/// The field of a resolved record.
///
/// A resolved field can be either:
///
/// - another resolved record, for the fields coming from elaboration, as
///   `mid` in `{ outer.mid.inner = true }`.
/// - A final value, for the last field of path, as `inner` in `{ outer.mid.inner = true }` or in
///   `fun param => { outer.mid.inner = param}`.
/// - A combination of the previous cases, for a field defined piecewise with multiple
///   definitions, such as `mid` in `fun param => { outer.mid.inner = true, outer.mid = param}`.
///
/// In the combined, the resolved field `mid` will have a resolved part `{inner = true}` and a
/// value part `param`. Values can't be combined statically in all generality (imagine adding
/// another piecewise definition `outer.mid = other_variable` in the previous example), hence we
/// keep accumulating them. However, resolved parts can be merged statically, so we only need one
/// that we update as we collect the pieces of the definition.
///
/// Rather than having an ad-hoc enum with all those cases (that would just take up more memory),
/// we consider the general combined case directly. Others are special cases with an empty
/// `resolved`, or an empty or one-element `values`.
#[derive(Default, Debug)]
pub(super) struct ResolvedField<'ast> {
    /// The resolved part of the field, coming from piecewise definitions where this field appears
    /// in the middle of the path.
    resolved: ResolvedRecord<'ast>,
    /// The accumulated values of the field, coming from piecewise definitions where this field
    /// appears last in the path.
    ///
    /// We store the whole [crate::bytecode::ast::record::FieldDef] here, although we don't need
    /// the path anymore, because it's easier and less costly than creating an ad-hoc structure to
    /// store only the value and the metadata.
    defs: Vec<&'ast FieldDef<'ast>>,
}

impl<'ast> ResolvedField<'ast> {
    /// Return the first type or contract annotation available in the definitions, if any.
    ///
    /// [ResolvedField::first_annot] first looks for a type annotation in all definitions. If we
    /// can't find any, [ResolvedField::first_annot] will look for the first contract annotation.
    /// If there is no annotation at all, `None` is returned.
    ///
    /// [ResolvedField::first_annot] is equivalent to calling
    /// [crate::bytecode::ast::Annotation::first] on the combined metadata of all definitions.
    pub fn first_annot(&self) -> Option<Type<'ast>> {
        self.defs
            .iter()
            .find_map(|def| def.metadata.annotation.typ.as_ref().cloned())
            .or(self
                .defs
                .iter()
                .find_map(|def| def.metadata.annotation.contracts.first().cloned()))
    }

    /// Returns the position of this resolved field if and only if there is a single defined
    /// position (among both the resolved part and the definitions). Otherwise, returns
    /// [crate::position::TermPos::None].
    pub fn pos(&self) -> TermPos {
        self.defs
            .iter()
            .fold(self.resolved.pos, |acc, def| acc.xor(def.pos))
    }
}

impl<'ast> Check<'ast> for ResolvedField<'ast> {
    fn check<V: TypecheckVisitor<'ast>>(
        &self,
        state: &mut State<'ast, '_>,
        ctxt: Context<'ast>,
        visitor: &mut V,
        ty: UnifType<'ast>,
    ) -> Result<(), TypecheckError> {
        match (self.resolved.is_empty(), self.defs.as_slice()) {
            // This shouldn't happen (fields present in the record should either have a definition
            // or comes from record resolution).
            (true, []) => {
                unreachable!("typechecker internal error: checking a vacant field")
            }
            // When there's just one classic field definition and no resolved form, we offload the
            // work to `FieldDef::check`.
            (true, [def]) => {
                eprintln!("Checking resolved field with empty resolver and a single def");
                def.check(state, ctxt, visitor, ty)
            }
            (false, []) => {
                eprintln!("Checking resolved field with only resolved part");
                self.resolved.check(state, ctxt, visitor, ty)
            }
            // Special case for a piecewise definition where at most one definition has a value.
            // This won't result in a runtime merge. Instead, it's always equivalent to one field
            // definition where the annotations have been combined. We reuse the same logic as for
            // checking a standard single field definition thanks to `FieldDefCheckView`.
            (true, defs) if defs.iter().filter(|def| def.value.is_some()).count() <= 1 => {
                eprintln!("Checking resolved field with empty resolver, multiple defs but a single value.");
                let value = defs.iter().find_map(|def| def.value.as_ref());

                FieldDefCheckView {
                    annots: defs,
                    value,
                    // unwrap():
                    //
                    // 1. We treated the case `(true, [])` in the first branch of this pattern, so
                    //    there must be at least one element in `defs`
                    // 2. The path of a field definition must always be non-empty
                    pos_id: defs
                        .first()
                        .unwrap()
                        .path
                        .last()
                        .expect("empty field path")
                        .pos(),
                }
                .check(state, ctxt, visitor, ty)
            }
            // In all other cases, we have either several definitions or at least one definition
            // and a resolved part. Those cases will result in a merge (even if it can be sometimes
            // optimized to a static merge that happens before runtime), so we type everything as
            // `Dyn`.
            (_, defs) => {
                eprintln!("Checking resolved field with at least 2 defined values");

                for def in defs.iter() {
                    def.check(state, ctxt.clone(), visitor, mk_uniftype::dynamic())?;
                }

                if !self.resolved.is_empty() {
                    // This will always raise an error, since the resolved part is equivalent to a
                    // record literal which doesn't type against `Dyn` (at least currently). We
                    // could raise the error directly, but it's simpler to call `check` on
                    // `self.resolved`, which will handle that for us.
                    //
                    // Another reason is that the error situation might change in the future, if we
                    // have proper subtyping for `Dyn`.
                    self.resolved
                        .check(state, ctxt, visitor, mk_uniftype::dynamic())?;
                }

                Ok(())
            }
        }
    }
}

impl Combine for ResolvedField<'_> {
    fn combine(this: Self, other: Self) -> Self {
        let mut defs = this.defs;
        defs.extend(other.defs);

        ResolvedField {
            resolved: Combine::combine(this.resolved, other.resolved),
            defs,
        }
    }
}

impl<'ast> From<&'ast FieldDef<'ast>> for ResolvedField<'ast> {
    fn from(def: &'ast FieldDef<'ast>) -> Self {
        ResolvedField {
            resolved: ResolvedRecord::empty(),
            defs: vec![def],
        }
    }
}

impl<'ast> From<ResolvedRecord<'ast>> for ResolvedField<'ast> {
    fn from(resolved: ResolvedRecord<'ast>) -> Self {
        ResolvedField {
            resolved,
            defs: Vec::new(),
        }
    }
}

impl<'ast> Resolve<'ast> for Record<'ast> {
    type Resolved = PoslessResolvedRecord<'ast>;

    fn resolve(&self) -> PoslessResolvedRecord<'ast> {
        fn insert_static_field<'ast>(
            static_fields: &mut IndexMap<LocIdent, ResolvedField<'ast>>,
            id: LocIdent,
            field: ResolvedField<'ast>,
        ) {
            match static_fields.entry(id) {
                Entry::Occupied(mut occpd) => {
                    // temporarily putting an empty field in the entry to take the previous value.
                    let prev = occpd.insert(ResolvedField::default());

                    // unwrap(): the field's identifier must have a position during parsing.
                    occpd.insert(Combine::combine(prev, field));
                }
                Entry::Vacant(vac) => {
                    vac.insert(field);
                }
            }
        }

        let mut stat_fields = IndexMap::new();
        let mut dyn_fields = Vec::new();

        for def in self.field_defs.iter() {
            // expect(): the field path must have at least one element, it's an invariant.
            let toplvl_field = def.path.first().expect("empty field path");
            let rfield = def.resolve();

            if let Some(id) = toplvl_field.try_as_ident() {
                insert_static_field(&mut stat_fields, id, rfield);
                continue;
            } else {
                // unreachable!(): `try_as_ident` returns `None` only if the path element is a
                // `Expr`
                let FieldPathElem::Expr(expr) = toplvl_field else {
                    unreachable!()
                };
                dyn_fields.push((expr, rfield));
            }
        }

        PoslessResolvedRecord::new(stat_fields, dyn_fields)
    }
}

// This turns a field definition into potentially nested resolved fields. Note that the top-level
// field is left out, as it's already been processed by the caller: resolving `foo.bar.baz.qux =
// 42` will return nested resolved records of the form `{bar = {baz = {qux = 42}}}`.
impl<'ast> Resolve<'ast> for FieldDef<'ast> {
    type Resolved = ResolvedField<'ast>;

    fn resolve(&'ast self) -> ResolvedField<'ast> {
        self.path[1..]
            .iter()
            .rev()
            .fold(self.into(), |acc, path_elem| {
                if let Some(id) = path_elem.try_as_ident() {
                    let pos_acc = acc.pos();

                    ResolvedField::from(ResolvedRecord {
                        stat_fields: iter::once((id, acc)).collect(),
                        dyn_fields: Vec::new(),
                        pos: id.pos.fuse(pos_acc),
                    })
                } else {
                    // unreachable!(): `try_as_ident` returns `None` only if the path element is a
                    // `Expr`
                    let FieldPathElem::Expr(expr) = path_elem else {
                        unreachable!()
                    };

                    let pos_acc = acc.pos();

                    ResolvedField::from(ResolvedRecord {
                        stat_fields: IndexMap::new(),
                        dyn_fields: vec![(expr, acc)],
                        pos: expr.pos.fuse(pos_acc),
                    })
                }
            })
    }
}

impl<'ast> HasApparentType<'ast> for ResolvedField<'ast> {
    // Return the apparent type of a field, by first looking at the type annotation, if any, then at
    // the contracts annotation, and if there is none, fall back to the apparent type of the value. If
    // there is no value, `Approximated(Dyn)` is returned.
    fn apparent_type(
        &self,
        ast_alloc: &'ast AstAlloc,
        env: Option<&TypeEnv<'ast>>,
        resolver: Option<&dyn AstImportResolver<'ast>>,
    ) -> ApparentType<'ast> {
        match self.defs.as_slice() {
            // If there is a resolved part, the apparent type is `Dyn`: a resolved part itself is a
            // record literal without annotation, whose apparent type is indeed `Dyn`. If there are
            // definitions as well, the result will be merged at runtime, and the apparent type of a
            // merge expression is also `Dyn`.
            _ if !self.resolved.is_empty() => ApparentType::Approximated(Type::from(TypeF::Dyn)),
            [] => ApparentType::Approximated(Type::from(TypeF::Dyn)),
            [def] => def.apparent_type(ast_alloc, env, resolver),
            _ => self
                .first_annot()
                .map(ApparentType::Annotated)
                .unwrap_or(ApparentType::Approximated(Type::from(TypeF::Dyn))),
        }
    }
}
