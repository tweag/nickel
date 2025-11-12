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
    ///
    /// For error reporting, note that the order of "expected" and "inferred" type is reversed when
    /// compared to `unify`: in the latter, `self` is the "expected" type (coming from the context)
    /// and "inferred" the type deduced from the expression, while here, the inferred type is
    /// usually `self` and the expected type is `t2`. This is made to match the usual order of
    /// subtyping.
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
                let mut prev_row_typ: Option<&UnifType<'_>> = None;
                for row in rrows.iter() {
                    match row {
                        RecordRowsElt::Row(a) => {
                            let row_result =
                                a.typ
                                .clone()
                                .subsumed_by(*type_fields.clone(), state, ctxt.clone());

                            // One way that this row can fail to typecheck is if the record has rows with
                            // differing types. In that case, the dict's field type will unify with the
                            // first row's type, and then it will trigger a type mismatch when it tries
                            // to unify with another row's type. In this case, the `TypeMismatch` error is
                            // confusing: checking whether `{ x: String, y: Number }` is subsumed by
                            // `{ _ : a }` will complain that `String` and `Number` are incompatible, but we
                            // should really be complaining that inhomogeneous records and dicts are incompatible.
                            //
                            // To improve this, we special-case the error message if a previous row
                            // checked successfully and this one failed.
                            match (row_result, prev_row_typ) {
                                (Err(_), Some(prev_ty)) => {
                                     Err(Box::new(UnifErrorKind::InhomogeneousRecord { row_a: prev_ty.clone(), row_b: a.typ.clone() }))
                                }
                                (x, _) => x,
                            }?;
                            prev_row_typ = Some(a.typ);
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
                            Err(UnifErrorKind::WithConst {
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
                .map_err(|err| err.into_unif_err(mk_uty_record!(;rrows2), mk_uty_record!(;rrows1))),
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
        // This code is almost taken verbatim from `unify`, but where some recursive calls are
        // changed to be `subsumed_by` instead of `unify`, and where missing row and extra row
        // errors have been swapped (since `subsumed_by` and `unify` are reversed with respect to
        // which argument is the expected type and which is the inferred type).
        //
        // We can surely factorize both into a generic function, but this is left for future work.
        let inferred = self.into_root(state.table);
        let checked = t2.into_root(state.table);

        match (inferred, checked) {
            (
                UnifRecordRows::Concrete {
                    rrows: rrows1,
                    var_levels_data: levels1,
                },
                UnifRecordRows::Concrete { rrows: rrows2, .. },
            ) => match (rrows1, rrows2) {
                // We reverse the order of the arguments when compared to `unify`. It shouldn't
                // really matter, but experience shows that the inferred type - here on the left -
                // is often much bigger in a structural type system, than the expected type (coming
                // from constraints or annotations).
                //
                // For example, when checking `array.fold_left` where `array` is `std.array`. The
                // expected type (right hand side of `subsume`) is `{ fold_left : ?a; ?b }`. The
                // inferred type is, on the other hand, quite big: it's the list of all functions
                // in the `array` module.
                //
                // If we started to pop stuff from the inferred type, we would iterate through all
                // array functions - until `fold_left` - and consecutively extend `?b` to match
                // that, which is useless. In the end, we just want to find `fold_left` in the type
                // of `array` and match `?a` to its type. Starting to pop stuff from the expected
                // type instead achieve that in one step.
                (
                    rrows1 @ RecordRowsF::Extend { .. },
                    RecordRowsF::Extend {
                        row: row2,
                        tail: tail2,
                    },
                ) => {
                    let urrows1 = UnifRecordRows::Concrete {
                        rrows: rrows1,
                        var_levels_data: levels1,
                    };

                    let (ty_res, urrows_without_ty_res) = urrows1
                        .remove_row(&row2.id, &row2.typ, state, ctxt.var_level)
                        .map_err(|err| match err {
                            RemoveRowError::Missing => RowUnifErrorKind::MissingRecordRow(row2.id),
                            RemoveRowError::Conflict => {
                                RowUnifErrorKind::RecordRowConflict(row2.clone())
                            }
                        })?;
                    if let RemoveRowResult::Extracted(ty) = ty_res {
                        ty.subsumed_by(*row2.typ, state, ctxt.clone())
                            .map_err(|err| {
                                Box::new(RowUnifErrorKind::RecordRowMismatch {
                                    id: row2.id,
                                    cause: err,
                                })
                            })?;
                    }

                    urrows_without_ty_res.subsumed_by(*tail2, state, ctxt)
                }
                (RecordRowsF::TailVar(id), _) | (_, RecordRowsF::TailVar(id)) => {
                    Err(Box::new(RowUnifErrorKind::UnboundTypeVariable(id)))
                }
                (RecordRowsF::Empty, RecordRowsF::Empty)
                | (RecordRowsF::TailDyn, RecordRowsF::TailDyn) => Ok(()),
                (RecordRowsF::Empty, RecordRowsF::TailDyn)
                | (RecordRowsF::TailDyn, RecordRowsF::Empty) => {
                    Err(Box::new(RowUnifErrorKind::ExtraDynTail))
                }
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
                ) => Err(Box::new(RowUnifErrorKind::MissingRecordRow(id))),
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
                ) => Err(Box::new(RowUnifErrorKind::ExtraRow(id))),
            },
            (UnifRecordRows::UnifVar { id, .. }, urrows)
            | (urrows, UnifRecordRows::UnifVar { id, .. }) => {
                if let UnifRecordRows::Constant(cst_id) = urrows {
                    let constant_level = state.table.get_rrows_level(cst_id);
                    state.table.force_rrows_updates(constant_level);
                    if state.table.get_rrows_level(id) < constant_level {
                        return Err(Box::new(RowUnifErrorKind::VarLevelMismatch {
                            constant_id: cst_id,
                            var_kind: VarKindDiscriminant::RecordRows,
                        }));
                    }
                }
                urrows.propagate_constrs(state.constr, id)?;
                state.table.assign_rrows(id, urrows);
                Ok(())
            }
            (UnifRecordRows::Constant(i1), UnifRecordRows::Constant(i2)) if i1 == i2 => Ok(()),
            (UnifRecordRows::Constant(i1), UnifRecordRows::Constant(i2)) => {
                Err(Box::new(RowUnifErrorKind::ConstMismatch {
                    var_kind: VarKindDiscriminant::RecordRows,
                    expected_const_id: i2,
                    inferred_const_id: i1,
                }))
            }
            (urrows, UnifRecordRows::Constant(i)) | (UnifRecordRows::Constant(i), urrows) => {
                Err(Box::new(RowUnifErrorKind::WithConst {
                    var_kind: VarKindDiscriminant::RecordRows,
                    expected_const_id: i,
                    inferred: UnifType::concrete(TypeF::Record(urrows)),
                }))
            }
        }
    }
}
