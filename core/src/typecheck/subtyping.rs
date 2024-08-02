//! Types subsumption.
/// Change from inference mode to checking mode, and apply a potential subsumption rule.
///
/// Currently, there is record/dictionary subtyping, if we are not in this case we fallback to perform
/// polymorphic type instantiation with unification variable on the left (on the inferred type),
/// and then simply performs unification (put differently, the subtyping relation when it is not
/// a record/dictionary subtyping is the equality
/// relation).
///
/// The type instantiation corresponds to the zero-ary case of application in the current
/// specification (which is based on [A Quick Look at Impredicativity][quick-look], although we
/// currently don't support impredicative polymorphism).
///
/// In the future, this function might implement a other non-trivial subsumption rule.
///
/// [quick-look]: https://www.microsoft.com/en-us/research/uploads/prod/2020/01/quick-look-icfp20-fixed.pdf
use super::*;

pub(super) trait IsSubsumedBy {
    type Error;
    fn is_subsumed_by(
        self,
        t2: Self,
        state: &mut State,
        ctxt: &mut Context,
    ) -> Result<(), Self::Error>;
}

impl Subsume for UnifType {
    type Error = UnifError;
    fn is_subsumed_by(
        self,
        t2: Self,
        state: &mut State,
        ctxt: &mut Context,
    ) -> Result<(), Self::Error> {
        let inferred = instantiate_foralls(state, ctxt, self, ForallInst::UnifVar);
        let checked = t2.into_root(state.table);
        match (inferred, checked) {
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
                        GenericUnifRecordRowsIteratorItem::Row(a) => {
                            a.typ
                                .clone()
                                .is_subsumed_by(*type_fields.clone(), state, ctxt)?
                        }
                        GenericUnifRecordRowsIteratorItem::TailUnifVar { id, .. } =>
                        // We don't need to perform any variable level checks when unifying a free
                        // unification variable with a ground type
                        // We close the tail because there is no guarantee that
                        // { a : Number, b : Number, _ : a?} <= { _ : Number}
                        {
                            state
                                .table
                                .assign_rrows(id, UnifRecordRows::concrete(RecordRowsF::Empty))
                        }
                        GenericUnifRecordRowsIteratorItem::TailConstant(id) => {
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
            | (
                UnifType::Concrete {
                    typ: TypeF::Dict { type_fields: a, .. },
                    ..
                },
                UnifType::Concrete {
                    typ: TypeF::Dict { type_fields: b, .. },
                    ..
                },
            ) => a.is_subsumed_by(*b, state, ctxt),
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
                .is_subsumed_by(rrows2.clone(), state, ctxt)
                .map_err(|err| err.into_unif_err(mk_uty_record!(;rrows2), mk_uty_record!(;rrows1))),
            (inferred, checked) => checked.unify(inferred, state, ctxt),
        }
    }
}

impl Subsume for UnifRecordRows {
    type Error = RowUnifError;
    fn is_subsumed_by(
        self,
        t2: Self,
        state: &mut State,
        ctxt: &mut Context,
    ) -> Result<(), Self::Error> {
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
                        row.typ.is_subsumed_by(ty, state, ctxt).map_err(|err| {
                            RowUnifError::RecordRowMismatch {
                                id: row.id,
                                cause: Box::new(err),
                            }
                        })?;
                    }
                    tail.is_subsumed_by(urrows_without_ty_res, state, ctxt)
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
