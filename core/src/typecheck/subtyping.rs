//! Type subsumption (subtyping)
//!
//! Subtyping is a relation between types that allows a value of one type to be used at a place
//! where another type is expected, because the value's actual type is subsumed by the expected
//! type.
//!
//! The subsumption rule is applied when switching from inference mode to checking mode, as
//! customary in bidirectional type checking.
//!
//! Currently, there is one core subtyping axiom:
//!
//! - Record / Dictionary : `{a1 : T1,...,an : Tn} <: {_ : U}` if for every n `Tn <: U`
//!
//! The subtyping relation is extended to a congruence on other type constructors in the obvious
//! way:
//!
//! - `Array T <: Array U` if `T <: U`
//! - `{_ : T} <: {_ : U}` if `T <: U`
//! - `{a1 : T1,...,an : Tn} <: {b1 : U1,...,bn : Un}` if for every n `Tn <: Un`
//!
//! In all other cases, we fallback to unification (although we instantiate polymorphic types as
//! needed before). That is, we try to apply reflexivity: `T <: U` if `T = U`.
//!
//! The type instantiation corresponds to the zero-ary case of application in the current
//! specification (which is loosely based on [A Quick Look at Impredicativity][quick-look],
//! although we currently don't support impredicative polymorphism).
//!
//! [quick-look]: https://www.microsoft.com/en-us/research/uploads/prod/2020/01/quick-look-icfp20-fixed.pdf
use super::*;

pub(super) trait SubsumedBy<'ast> {
    type Error;

    /// Checks if `self` is subsumed by `t2`, that is if `self <: t2`. Returns an error otherwise.
    fn subsumed_by(
        self,
        t2: Self,
        state: &mut State<'ast, '_>,
        ctxt: Context<'ast>,
    ) -> Result<(), Self::Error>;
}

impl<'ast> SubsumedBy<'ast> for UnifType<'ast> {
    type Error = UnifError<'ast>;

    fn subsumed_by(
        self,
        t2: Self,
        state: &mut State<'ast, '_>,
        mut ctxt: Context<'ast>,
    ) -> Result<(), Self::Error> {
        let inferred = instantiate_foralls(state, &mut ctxt, self, ForallInst::UnifVar);
        let checked = t2.into_root(state.table);

        match (inferred, checked) {
            // {a1 : T1,...,an : Tn} <: {_ : U} if for every n `Tn <: U`
            (
                UnifType::Concrete {
                    typ: TypeF::Record(rrows),
                    ..
                },
                UnifType::Concrete {
                    typ:
                        TypeF::Dict {
                            type_fields,
                            flavour,
                        },
                    var_levels_data,
                },
            ) => {
                for row in rrows.iter() {
                    match row {
                        RecordRowsElt::Row(a) => {
                            a.typ
                                .clone()
                                .subsumed_by(*type_fields.clone(), state, ctxt.clone())?
                        }
                        RecordRowsElt::TailUnifVar { id, .. } =>
                        // We don't need to perform any variable level checks when unifying a free
                        // unification variable with a ground type
                        // We close the tail because there is no guarantee that
                        // { a : Number, b : Number, _ : a?} <= { _ : Number}
                        {
                            state
                                .table
                                .assign_rrows(id, UnifRecordRows::concrete(RecordRowsF::Empty))
                        }
                        RecordRowsElt::TailConstant(id) => {
                            let checked = UnifType::Concrete {
                                typ: TypeF::Dict {
                                    type_fields: type_fields.clone(),
                                    flavour,
                                },
                                var_levels_data,
                            };
                            Err(UnifError::WithConst {
                                var_kind: VarKindDiscriminant::RecordRows,
                                expected_const_id: id,
                                inferred: checked,
                            })?
                        }
                        _ => (),
                    }
                }
                Ok(())
            }
            // Array T <: Array U if T <: U
            (
                UnifType::Concrete {
                    typ: TypeF::Array(a),
                    ..
                },
                UnifType::Concrete {
                    typ: TypeF::Array(b),
                    ..
                },
            )
            // Dict T <: Dict U if T <: U
            | (
                UnifType::Concrete {
                    typ: TypeF::Dict { type_fields: a, .. },
                    ..
                },
                UnifType::Concrete {
                    typ: TypeF::Dict { type_fields: b, .. },
                    ..
                },
            ) => a.subsumed_by(*b, state, ctxt),
            // {a1 : T1,...,an : Tn} <: {b1 : U1,...,bn : Un} if for every n `Tn <: Un`
            (
                UnifType::Concrete {
                    typ: TypeF::Record(rrows1),
                    ..
                },
                UnifType::Concrete {
                    typ: TypeF::Record(rrows2),
                    ..
                },
            ) => rrows1
                .clone()
                .subsumed_by(rrows2.clone(), state, ctxt)
                .map_err(|err| err.into_unif_err(mk_buty_record!(;rrows2), mk_buty_record!(;rrows1))),
            // T <: U if T = U
            (inferred, checked) => checked.unify(inferred, state, &ctxt),
        }
    }
}

impl<'ast> SubsumedBy<'ast> for UnifRecordRows<'ast> {
    type Error = RowUnifError<'ast>;

    fn subsumed_by(
        self,
        t2: Self,
        state: &mut State<'ast, '_>,
        ctxt: Context<'ast>,
    ) -> Result<(), Self::Error> {
        // This code is almost taken verbatim fro `unify`, but where some recursive calls are
        // changed to be `subsumed_by` instead of `unify`. We can surely factorize both into a
        // generic function, but this is left for future work.
        let inferred = self.into_root(state.table);
        let checked = t2.into_root(state.table);

        match (inferred, checked) {
            (
                UnifRecordRows::Concrete { rrows: rrows1, .. },
                UnifRecordRows::Concrete {
                    rrows: rrows2,
                    var_levels_data: levels2,
                },
            ) => match (rrows1, rrows2) {
                (RecordRowsF::Extend { row, tail }, rrows2 @ RecordRowsF::Extend { .. }) => {
                    let urrows2 = UnifRecordRows::Concrete {
                        rrows: rrows2,
                        var_levels_data: levels2,
                    };
                    let (ty_res, urrows_without_ty_res) = urrows2
                        .remove_row(&row.id, &row.typ, state, ctxt.var_level)
                        .map_err(|err| match err {
                            RemoveRowError::Missing => RowUnifError::MissingRow(row.id),
                            RemoveRowError::Conflict => {
                                RowUnifError::RecordRowConflict(row.clone())
                            }
                        })?;
                    if let RemoveRowResult::Extracted(ty) = ty_res {
                        row.typ
                            .subsumed_by(ty, state, ctxt.clone())
                            .map_err(|err| RowUnifError::RecordRowMismatch {
                                id: row.id,
                                cause: Box::new(err),
                            })?;
                    }
                    tail.subsumed_by(urrows_without_ty_res, state, ctxt)
                }
                (RecordRowsF::TailVar(id), _) | (_, RecordRowsF::TailVar(id)) => {
                    Err(RowUnifError::UnboundTypeVariable(id))
                }
                (RecordRowsF::Empty, RecordRowsF::Empty)
                | (RecordRowsF::TailDyn, RecordRowsF::TailDyn) => Ok(()),
                (RecordRowsF::Empty, RecordRowsF::TailDyn)
                | (RecordRowsF::TailDyn, RecordRowsF::Empty) => Err(RowUnifError::ExtraDynTail),
                (
                    RecordRowsF::Empty,
                    RecordRowsF::Extend {
                        row: UnifRecordRow { id, .. },
                        ..
                    },
                )
                | (
                    RecordRowsF::TailDyn,
                    RecordRowsF::Extend {
                        row: UnifRecordRow { id, .. },
                        ..
                    },
                ) => Err(RowUnifError::MissingRow(id)),
                (
                    RecordRowsF::Extend {
                        row: UnifRecordRow { id, .. },
                        ..
                    },
                    RecordRowsF::TailDyn,
                )
                | (
                    RecordRowsF::Extend {
                        row: UnifRecordRow { id, .. },
                        ..
                    },
                    RecordRowsF::Empty,
                ) => Err(RowUnifError::ExtraRow(id)),
            },
            (UnifRecordRows::UnifVar { id, .. }, urrows)
            | (urrows, UnifRecordRows::UnifVar { id, .. }) => {
                if let UnifRecordRows::Constant(cst_id) = urrows {
                    let constant_level = state.table.get_rrows_level(cst_id);
                    state.table.force_rrows_updates(constant_level);
                    if state.table.get_rrows_level(id) < constant_level {
                        return Err(RowUnifError::VarLevelMismatch {
                            constant_id: cst_id,
                            var_kind: VarKindDiscriminant::RecordRows,
                        });
                    }
                }
                urrows.propagate_constrs(state.constr, id)?;
                state.table.assign_rrows(id, urrows);
                Ok(())
            }
            (UnifRecordRows::Constant(i1), UnifRecordRows::Constant(i2)) if i1 == i2 => Ok(()),
            (UnifRecordRows::Constant(i1), UnifRecordRows::Constant(i2)) => {
                Err(RowUnifError::ConstMismatch {
                    var_kind: VarKindDiscriminant::RecordRows,
                    expected_const_id: i2,
                    inferred_const_id: i1,
                })
            }
            (urrows, UnifRecordRows::Constant(i)) | (UnifRecordRows::Constant(i), urrows) => {
                Err(RowUnifError::WithConst {
                    var_kind: VarKindDiscriminant::RecordRows,
                    expected_const_id: i,
                    inferred: UnifType::concrete(TypeF::Record(urrows)),
                })
            }
        }
    }
}
