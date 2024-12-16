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

use indexmap::{map::Entry, IndexMap};

use std::iter;

pub(super) trait Resolve<'ast> {
    type Resolved;

    fn resolve(&'ast self) -> Self::Resolved;
}

/// A resolved record literal, without field paths or piecewise definitions. Piecewise definitions
/// of fields have be grouped together, path have been broken into proper levels and top-level
/// fields are partitioned between static and dynamic.
#[derive(Default)]
pub(super) struct ResolvedRecord<'ast> {
    /// The static fields of the record.
    pub stat_fields: IndexMap<LocIdent, ResolvedField<'ast>>,
    /// The dynamic fields of the record.
    pub dyn_fields: Vec<(&'ast Ast<'ast>, ResolvedField<'ast>)>,
}

impl<'ast> ResolvedRecord<'ast> {
    pub fn empty() -> Self {
        ResolvedRecord {
            stat_fields: IndexMap::new(),
            dyn_fields: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.stat_fields.is_empty() && self.dyn_fields.is_empty()
    }

    pub fn check<V: TypecheckVisitor<'ast>>(
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

    /// Check a record with dynamic fields (and potentially static fields as well) against a type.
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
            .map_err(|err| err.into_typecheck_err(state, todo!()))?;

        for id in self.stat_fields.keys() {
            ctxt.type_env.insert(id.ident(), ty_elts.clone());
            visitor.visit_ident(id, ty_elts.clone())
        }

        for (expr, field) in &self.dyn_fields {
            check(state, ctxt.clone(), visitor, expr, mk_uniftype::str())?;
            field.check(state, ctxt.clone(), visitor, expr.pos, ty_elts.clone())?;
        }

        // We don't bind recursive fields in the term environment used to check for contract. See
        // [^term-env-rec-bindings] in `./mod.rs`.
        self.stat_fields
            .iter()
            .try_for_each(|(id, field)| -> Result<(), TypecheckError> {
                field.check(state, ctxt.clone(), visitor, id.pos, ty_elts.clone())
            })
    }

    /// Check a record with only static fields against a type.
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
            self.stat_fields
                .iter()
                .try_for_each(|(id, field)| -> Result<(), TypecheckError> {
                    field.check(state, ctxt.clone(), visitor, id.pos, (*rec_ty).clone())
                })
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
                let uty_apprt = field.apparent_type(Some(&ctxt.type_env), Some(state.resolver));

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
            let mut field_types: Vec<UnifType<'ast>> =
                iter::repeat_with(|| state.table.fresh_type_uvar(ctxt.var_level))
                    .take(self.stat_fields.len())
                    .collect();

            // Build the type {id1 : ?a1, id2: ?a2, .., idn: ?an}, which is the type of the whole
            // record.
            let rows = self.stat_fields.keys().zip(field_types.iter()).fold(
                mk_buty_record_row!(),
                |acc, (id, row_ty)| mk_buty_record_row!((*id, row_ty.clone()); acc),
            );

            ty.unify(mk_buty_record!(; rows), state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, todo!()))?;

            // We reverse the order of `field_types`. The idea is that we can then pop each
            // field type as we iterate a last time over the fields, taking ownership, instead of
            // having to clone (if we needed to index instead).
            field_types.reverse();

            for (id, field) in self.stat_fields.iter() {
                // unwrap(): `field_types` has exactly the same length as `self.stat_fields`, as it
                // was constructed with `.take(self.stat_fields.len()).collect()`.
                let field_type = field_types.pop().unwrap();

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
                                todo!(),
                                //                               field.value.as_ref().map(|v| v.pos).unwrap_or_default(),
                            )
                        })?;
                }

                field.check(state, ctxt.clone(), visitor, id.pos, field_type)?;
            }

            Ok(())
        }
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
            .chain(other.dyn_fields.into_iter())
            .collect();

        ResolvedRecord {
            stat_fields,
            dyn_fields,
        }
    }
}

impl<'ast> Combine for ResolvedField<'ast> {
    fn combine(this: Self, other: Self) -> Self {
        match (this, other) {
            (ResolvedField::Record(r1), ResolvedField::Record(r2)) => {
                ResolvedField::Record(Combine::combine(r1, r2))
            }
            (ResolvedField::Value(v1), ResolvedField::Value(v2)) => ResolvedField::Values {
                resolved: ResolvedRecord::empty(),
                values: vec![v1, v2],
            },
            (
                ResolvedField::Values {
                    mut values,
                    resolved,
                },
                ResolvedField::Value(v),
            )
            | (
                ResolvedField::Value(v),
                ResolvedField::Values {
                    mut values,
                    resolved,
                },
            ) => {
                values.push(v);
                ResolvedField::Values { values, resolved }
            }
            (
                ResolvedField::Values {
                    resolved: r1,
                    values: mut vs1,
                },
                ResolvedField::Values {
                    resolved: r2,
                    values: vs2,
                },
            ) => {
                vs1.extend(vs2);
                ResolvedField::Values {
                    resolved: Combine::combine(r1, r2),
                    values: vs1,
                }
            }
            (ResolvedField::Record(r), ResolvedField::Value(v))
            | (ResolvedField::Value(v), ResolvedField::Record(r)) => ResolvedField::Values {
                resolved: r,
                values: vec![v],
            },
            (
                ResolvedField::Values {
                    resolved: r1,
                    values,
                },
                ResolvedField::Record(r2),
            )
            | (
                ResolvedField::Record(r1),
                ResolvedField::Values {
                    resolved: r2,
                    values,
                },
            ) => ResolvedField::Values {
                resolved: Combine::combine(r1, r2),
                values,
            },
            (rfield, ResolvedField::Vacant) | (ResolvedField::Vacant, rfield) => rfield,
        }
    }
}

/// The field of a resolved record.
///
/// A resolved field can be either:
///
/// - another resolved record, for the fields coming from elaboration, as
///   `mid` in `{ outer.mid.inner = true }`.
/// - A final value, for the last field of path, as `inner` in `{ outer.mid.inner = true }`.
/// - A combination of values, or of values and a resolved record, for a field defined piecewise
///   with multiple definitions, as `mid` in `fun param => { outer.mid.inner = true, outer.mid =
///   param}`. In this case, the resolved field `mid` will have a resolved part `{inner = true}`
///   and a value part `param`. Values can't be combined statically in all generality (imagine
///   adding another piecewise definition `outer.mid = other_variable` in the previous example), we
///   keep accumulating them. However, resolved parts can be merged statically, so we only need one
///   that we update as we collect the pieces of the definition.
#[derive(Default)]
pub(super) enum ResolvedField<'ast> {
    /// Default value. Meaningless but useful to take ownership of mutable references by swapping
    /// a mutable reference to a resolved field with this default value (`mem::swap`, hashmap entires, etc.)
    #[default]
    Vacant,
    /// A resolved record.
    Record(ResolvedRecord<'ast>),
    /// A final value (or no value at all). We only need to store an optional value and its
    /// metadata, but there is no such structure in the AST, so we store the whole field definition
    /// instead.
    Value(&'ast FieldDef<'ast>),
    /// Several values (coming from piecewise definitions).
    Values {
        resolved: ResolvedRecord<'ast>,
        values: Vec<&'ast FieldDef<'ast>>,
    },
}

impl<'ast> ResolvedField<'ast> {
    pub fn check<V: TypecheckVisitor<'ast>>(
        &self,
        state: &mut State<'ast, '_>,
        ctxt: Context<'ast>,
        visitor: &mut V,
        pos_name: TermPos,
        ty: UnifType<'ast>,
    ) -> Result<(), TypecheckError> {
        match self {
            // This shouldn't happen, but we can handle it as a field without a definition
            ResolvedField::Vacant => {
                debug_assert!(false, "checking a vacant field");
                Ok(())
            }
            ResolvedField::Record(resolved_record) => {
                resolved_record.check(state, ctxt, visitor, ty)
            }
            ResolvedField::Value(FieldDef {
                value,
                metadata,
                pos,
                ..
            }) if metadata.is_empty() => {
                if let Some(value) = value {
                    check(state, ctxt, visitor, value, ty)
                } else {
                    // It might make sense to accept any type for a value without definition (which would
                    // act a bit like a function parameter). But for now, we play safe and implement a more
                    // restrictive rule, which is that a value without a definition has type `Dyn`
                    ty.unify(mk_uniftype::dynamic(), state, &ctxt)
                        .map_err(|err| err.into_typecheck_err(state, *pos))
                }
            }
            ResolvedField::Value(FieldDef {
                value,
                metadata,
                pos,
                ..
            }) => {
                let pos = value.as_ref().map(|v| v.pos).unwrap_or(*pos);

                let inferred = infer_with_annot(
                    state,
                    ctxt.clone(),
                    visitor,
                    &metadata.annotation,
                    value.as_ref(),
                )?;

                inferred
                    .subsumed_by(ty, state, ctxt)
                    .map_err(|err| err.into_typecheck_err(state, pos))
            }
            ResolvedField::Values { resolved, values } => todo!(),
        }
    }
}

impl<'ast> Resolve<'ast> for Record<'ast> {
    type Resolved = ResolvedRecord<'ast>;

    fn resolve(&self) -> ResolvedRecord<'ast> {
        fn insert_static_field<'ast>(
            static_fields: &mut IndexMap<LocIdent, ResolvedField<'ast>>,
            id: LocIdent,
            field: ResolvedField<'ast>,
        ) {
            match static_fields.entry(id) {
                Entry::Occupied(mut occpd) => {
                    // temporarily putting an empty field in the entry to take the previous value.
                    let prev = occpd.insert(ResolvedField::Vacant);

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

        ResolvedRecord {
            stat_fields,
            dyn_fields,
        }
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
            .fold(ResolvedField::Value(self), |acc, path_elem| {
                if let Some(id) = path_elem.try_as_ident() {
                    ResolvedField::Record(ResolvedRecord {
                        stat_fields: iter::once((id, acc)).collect(),
                        dyn_fields: Vec::new(),
                    })
                } else {
                    // unreachable!(): `try_as_ident` returns `None` only if the path element is a
                    // `Expr`
                    let FieldPathElem::Expr(expr) = path_elem else {
                        unreachable!()
                    };

                    ResolvedField::Record(ResolvedRecord {
                        stat_fields: IndexMap::new(),
                        dyn_fields: vec![(expr, acc)],
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
        env: Option<&TypeEnv<'ast>>,
        resolver: Option<&dyn ImportResolver>,
    ) -> ApparentType<'ast> {
        match self {
            ResolvedField::Vacant | ResolvedField::Record(_) => {
                ApparentType::Approximated(Type::from(TypeF::Dyn))
            }
            ResolvedField::Value(def) => def.apparent_type(env, resolver),
            ResolvedField::Values { resolved, values } => match values.as_slice() {
                [] => ApparentType::Approximated(Type::from(TypeF::Dyn)),
                [def] => def.apparent_type(env, resolver),
                values => {
                    // We first look for a type annotation somewhere in the definitions. If we
                    // can't find it, we'll look for a contract annotation. Finally, if we can't
                    // find one either, we return `Approximated(Dyn)`, because the resulting value
                    // will be elaborated as a merge expression which doesn't have an apparent
                    // type.
                    values
                        .iter()
                        .find_map(|def| {
                            def.metadata
                                .annotation
                                .typ
                                .as_ref()
                                .map(|ty| ApparentType::Annotated(ty.clone()))
                        })
                        .or(values.iter().find_map(|def| {
                            def.metadata
                                .annotation
                                .contracts
                                .first()
                                .map(|ty| ApparentType::Annotated(ty.clone()))
                        }))
                        .unwrap_or(ApparentType::Approximated(Type::from(TypeF::Dyn)))
                }
            },
        }
    }
}
