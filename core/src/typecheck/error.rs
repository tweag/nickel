//! Internal error types for typechecking.
use super::{
    reporting::{self, ToType},
    State, UnifEnumRow, UnifRecordRow, UnifType, VarId,
};
use crate::{
    error::TypecheckError,
    identifier::LocIdent,
    label::ty_path,
    position::TermPos,
    term::RichTerm,
    typ::{TypeF, VarKindDiscriminant},
};

/// Error during the unification of two row types.
#[derive(Debug, PartialEq)]
pub enum RowUnifError {
    /// The LHS had a binding that was missing in the RHS.
    MissingRow(LocIdent),
    /// The LHS had a `Dyn` tail that was missing in the RHS.
    MissingDynTail,
    /// The RHS had a binding that was not in the LHS.
    ExtraRow(LocIdent),
    /// The RHS had a additional `Dyn` tail.
    ExtraDynTail,
    /// There were two incompatible definitions for the same record row.
    RecordRowMismatch {
        id: LocIdent,
        /// The underlying unification error that caused the mismatch.
        cause: Box<UnifError>,
    },
    /// There were two incompatible definitions for the same enum row.
    ///
    /// Because enum rows have an optional argument, there might not be any underlying unification
    /// error (e.g. one of the row has an argument, and the other does not). This is why the
    /// underlying unification error is optional, as opposed to record rows.
    EnumRowMismatch {
        id: LocIdent,
        /// The underlying unification error that caused the mismatch.
        cause: Option<Box<UnifError>>,
    },
    /// A [row constraint][super::RowConstrs] was violated.
    RecordRowConflict(UnifRecordRow),
    /// A [row constraint][super::RowConstrs] was violated.
    EnumRowConflict(UnifEnumRow),
    /// Tried to unify a type constant with another different type.
    WithConst {
        var_kind: VarKindDiscriminant,
        expected_const_id: VarId,
        inferred: UnifType,
    },
    /// Tried to unify two distinct type constants.
    ConstMismatch {
        var_kind: VarKindDiscriminant,
        expected_const_id: usize,
        inferred_const_id: usize,
    },
    /// An unbound type variable was referenced.
    UnboundTypeVariable(LocIdent),
    /// Tried to unify a constant with a unification variable with a strictly lower level.
    VarLevelMismatch {
        constant_id: VarId,
        var_kind: VarKindDiscriminant,
    },
}

impl RowUnifError {
    /// Convert a row unification error to a unification error.
    ///
    /// There is a hierarchy between error types, from the most local/specific to the most
    /// high-level:
    /// - [`RowUnifError`]
    /// - [`UnifError`]
    /// - [`crate::error::TypecheckError`]
    ///
    /// Each level usually adds information (such as types or positions) and group different
    /// specific errors into most general ones.
    pub fn into_unif_err(self, expected: UnifType, inferred: UnifType) -> UnifError {
        match self {
            RowUnifError::MissingRow(id) => UnifError::MissingRow {
                id,
                expected,
                inferred,
            },
            RowUnifError::MissingDynTail => UnifError::MissingDynTail { expected, inferred },
            RowUnifError::ExtraRow(id) => UnifError::ExtraRow {
                id,
                expected,
                inferred,
            },
            RowUnifError::ExtraDynTail => UnifError::ExtraDynTail { expected, inferred },
            RowUnifError::RecordRowMismatch { id, cause } => UnifError::RecordRowMismatch {
                id,
                expected,
                inferred,
                cause,
            },
            RowUnifError::EnumRowMismatch { id, cause } => UnifError::EnumRowMismatch {
                id,
                expected,
                inferred,
                cause,
            },
            RowUnifError::RecordRowConflict(row) => UnifError::RecordRowConflict {
                row,
                expected,
                inferred,
            },
            RowUnifError::EnumRowConflict(row) => UnifError::EnumRowConflict {
                row,
                expected,
                inferred,
            },
            RowUnifError::WithConst {
                var_kind,
                expected_const_id,
                inferred,
            } => UnifError::WithConst {
                var_kind,
                expected_const_id,
                inferred,
            },
            RowUnifError::ConstMismatch {
                var_kind,
                expected_const_id,
                inferred_const_id,
            } => UnifError::ConstMismatch {
                var_kind,
                expected_const_id,
                inferred_const_id,
            },
            RowUnifError::UnboundTypeVariable(id) => UnifError::UnboundTypeVariable(id),
            RowUnifError::VarLevelMismatch {
                constant_id,
                var_kind,
            } => UnifError::VarLevelMismatch {
                constant_id,
                var_kind,
            },
        }
    }
}

/// Error during the unification of two types.
///
/// In each variant, `expected` and `inferred` refers to the two types that failed to unify.
#[derive(Debug, PartialEq)]
pub enum UnifError {
    /// Tried to unify two incompatible types.
    TypeMismatch {
        expected: UnifType,
        inferred: UnifType,
    },
    /// There are two incompatible definitions for the same row.
    RecordRowMismatch {
        id: LocIdent,
        expected: UnifType,
        inferred: UnifType,
        /// The uderlying unification error (`expected` and `inferred` should be the record types
        /// that failed to unify, while this error is the specific cause of the mismatch for the
        /// `id` row)
        cause: Box<UnifError>,
    },
    /// There are two incompatible definitions for the same row.
    ///
    /// Because enum rows have an optional argument, there might not be any underlying unification
    /// error (e.g. one of the row has an argument, and the other does not). This is why the
    /// underlying unification error is optional, as opposed to record rows.
    EnumRowMismatch {
        id: LocIdent,
        expected: UnifType,
        inferred: UnifType,
        cause: Option<Box<UnifError>>,
    },
    /// Tried to unify two distinct type constants.
    ConstMismatch {
        var_kind: VarKindDiscriminant,
        expected_const_id: VarId,
        inferred_const_id: VarId,
    },
    /// Tried to unify two rows, but a row from the expected type was absent from the inferred type.
    MissingRow {
        id: LocIdent,
        expected: UnifType,
        inferred: UnifType,
    },
    /// Tried to unify two rows, but a row from the inferred type was absent from the expected type.
    ExtraRow {
        id: LocIdent,
        expected: UnifType,
        inferred: UnifType,
    },
    /// Tried to unify two rows, but the `Dyn` tail of the expected type was absent from the
    /// inferred type.
    MissingDynTail {
        expected: UnifType,
        inferred: UnifType,
    },
    /// Tried to unify two rows, but the `Dyn` tail of the RHS was absent from the LHS.
    ExtraDynTail {
        expected: UnifType,
        inferred: UnifType,
    },
    /// Tried to unify a unification variable with a row type violating the [row
    /// constraints][super::RowConstrs] of the variable.
    RecordRowConflict {
        /// The row that conflicts with an existing one.
        row: UnifRecordRow,
        /// The original expected type that led to the row conflict (when unified with the inferred
        /// type).
        expected: UnifType,
        /// The original inferred type that led to the row conflict (when unified with the expected
        /// type).
        inferred: UnifType,
    },
    /// Tried to unify a unification variable with a row type violating the [row
    /// constraints][super::RowConstrs] of the variable.
    EnumRowConflict {
        /// The row that conflicts with an existing one.
        row: UnifEnumRow,
        /// The original expected type that led to the row conflict (when unified with the inferred
        /// type).
        expected: UnifType,
        /// The original inferred type that led to the row conflict (when unified with the expected
        /// type).
        inferred: UnifType,
    },
    /// Tried to unify a type constant with another different type.
    WithConst {
        var_kind: VarKindDiscriminant,
        expected_const_id: VarId,
        inferred: UnifType,
    },
    /// A flat type, which is an opaque type corresponding to custom contracts, contained a Nickel
    /// term different from a variable. Only a variables is a legal inner term of a flat type.
    IncomparableFlatTypes {
        expected: RichTerm,
        inferred: RichTerm,
    },
    /// An unbound type variable was referenced.
    UnboundTypeVariable(LocIdent),
    /// An error occurred when unifying the domains of two arrows.
    DomainMismatch {
        expected: UnifType,
        inferred: UnifType,
        cause: Box<UnifError>,
    },
    /// An error occurred when unifying the codomains of two arrows.
    CodomainMismatch {
        expected: UnifType,
        inferred: UnifType,
        cause: Box<UnifError>,
    },
    /// Tried to unify a constant with a unification variable with a strictly lower level.
    VarLevelMismatch {
        constant_id: VarId,
        var_kind: VarKindDiscriminant,
    },
}

impl UnifError {
    /// Convert a unification error to a typechecking error. There is a hierarchy between error
    /// types, from the most local/specific to the most high-level:
    /// - [`RowUnifError`]
    /// - [`UnifError`]
    /// - [`crate::error::TypecheckError`]
    ///
    /// Each level usually adds information (such as types or positions) and group different
    /// specific errors into most general ones.
    ///
    /// # Parameters
    ///
    /// - `state`: the state of unification. Used to access the unification table, and the original
    /// names of of unification variable or type constant.
    /// - `pos_opt`: the position span of the expression that failed to typecheck.
    pub fn into_typecheck_err(self, state: &State, pos_opt: TermPos) -> TypecheckError {
        let mut names = reporting::NameReg::new(state.names.clone());
        self.into_typecheck_err_(state, &mut names, pos_opt)
    }

    /// Convert a unification error to a typechecking error, given a populated [name
    /// registry][reporting::NameReg]. Actual meat of the implementation of
    /// [`Self::into_typecheck_err`].
    fn into_typecheck_err_(
        self,
        state: &State,
        names_reg: &mut reporting::NameReg,
        pos: TermPos,
    ) -> TypecheckError {
        match self {
            UnifError::TypeMismatch { expected, inferred } => TypecheckError::TypeMismatch {
                expected: expected.to_type(names_reg, state.table),
                inferred: inferred.to_type(names_reg, state.table),
                pos,
            },
            UnifError::RecordRowMismatch {
                id,
                expected,
                inferred,
                cause,
            } => TypecheckError::RecordRowMismatch {
                id,
                expected: expected.to_type(names_reg, state.table),
                inferred: inferred.to_type(names_reg, state.table),
                cause: Box::new((*cause).into_typecheck_err_(state, names_reg, TermPos::None)),
                pos,
            },
            UnifError::EnumRowMismatch {
                id,
                expected,
                inferred,
                cause,
            } => TypecheckError::EnumRowMismatch {
                id,
                expected: expected.to_type(names_reg, state.table),
                inferred: inferred.to_type(names_reg, state.table),
                cause: cause.map(|err| {
                    Box::new((*err).into_typecheck_err_(state, names_reg, TermPos::None))
                }),
                pos,
            },
            // TODO: for now, failure to unify with a type constant causes the same error as a
            // usual type mismatch. It could be nice to have a specific error message in the
            // future.
            UnifError::ConstMismatch {
                var_kind,
                expected_const_id,
                inferred_const_id,
            } => TypecheckError::TypeMismatch {
                expected: UnifType::from_constant_of_kind(expected_const_id, var_kind)
                    .to_type(names_reg, state.table),
                inferred: UnifType::from_constant_of_kind(inferred_const_id, var_kind)
                    .to_type(names_reg, state.table),
                pos,
            },
            UnifError::WithConst {
                var_kind: VarKindDiscriminant::Type,
                expected_const_id,
                inferred,
            } => TypecheckError::TypeMismatch {
                expected: UnifType::Constant(expected_const_id).to_type(names_reg, state.table),
                inferred: inferred.to_type(names_reg, state.table),
                pos,
            },
            UnifError::WithConst {
                var_kind,
                expected_const_id,
                inferred,
            } => TypecheckError::ForallParametricityViolation {
                kind: var_kind,
                tail: UnifType::from_constant_of_kind(expected_const_id, var_kind)
                    .to_type(names_reg, state.table),
                violating_type: inferred.to_type(names_reg, state.table),
                pos,
            },
            UnifError::IncomparableFlatTypes { expected, inferred } => {
                TypecheckError::IncomparableFlatTypes {
                    expected,
                    inferred,
                    pos,
                }
            }
            UnifError::MissingRow {
                id,
                expected,
                inferred,
            } => TypecheckError::MissingRow {
                id,
                expected: expected.to_type(names_reg, state.table),
                inferred: inferred.to_type(names_reg, state.table),
                pos,
            },
            UnifError::MissingDynTail { expected, inferred } => TypecheckError::MissingDynTail {
                expected: expected.to_type(names_reg, state.table),
                inferred: inferred.to_type(names_reg, state.table),
                pos,
            },
            UnifError::ExtraRow {
                id,
                expected,
                inferred,
            } => TypecheckError::ExtraRow {
                id,
                expected: expected.to_type(names_reg, state.table),
                inferred: inferred.to_type(names_reg, state.table),
                pos,
            },
            UnifError::ExtraDynTail { expected, inferred } => TypecheckError::ExtraDynTail {
                expected: expected.to_type(names_reg, state.table),
                inferred: inferred.to_type(names_reg, state.table),
                pos,
            },
            UnifError::RecordRowConflict {
                row,
                expected,
                inferred,
            } => TypecheckError::RecordRowConflict {
                row: row.to_type(names_reg, state.table),
                expected: expected.to_type(names_reg, state.table),
                inferred: inferred.to_type(names_reg, state.table),
                pos,
            },
            UnifError::EnumRowConflict {
                row,
                expected,
                inferred,
            } => TypecheckError::EnumRowConflict {
                row: row.to_type(names_reg, state.table),
                expected: expected.to_type(names_reg, state.table),
                inferred: inferred.to_type(names_reg, state.table),
                pos,
            },
            UnifError::UnboundTypeVariable(ident) => TypecheckError::UnboundTypeVariable(ident),
            err @ UnifError::CodomainMismatch { .. } | err @ UnifError::DomainMismatch { .. } => {
                let (expected, inferred, type_path, err_final) = err.into_type_path().unwrap();
                TypecheckError::ArrowTypeMismatch {
                    expected: expected.to_type(names_reg, state.table),
                    inferred: inferred.to_type(names_reg, state.table),
                    type_path,
                    cause: Box::new(err_final.into_typecheck_err_(state, names_reg, TermPos::None)),
                    pos,
                }
            }
            UnifError::VarLevelMismatch {
                constant_id,
                var_kind,
            } => TypecheckError::VarLevelMismatch {
                type_var: names_reg.gen_cst_name(constant_id, var_kind).into(),
                pos,
            },
        }
    }

    /// Transform a `(Co)DomainMismatch` into a type path and other data.
    ///
    /// `(Co)DomainMismatch` can be nested: when unifying `Num -> Num -> Num` with `Num -> Bool ->
    /// Num`, the resulting error is of the form `CodomainMismatch(.., DomainMismatch(..,
    /// TypeMismatch(..)))`. The heading sequence of `(Co)DomainMismatch` is better represented as
    /// a type path, here `[Codomain, Domain]`, while the last error of the chain -- which thus
    /// cannot be a `(Co)DomainMismatch` -- is the actual cause of the unification failure.
    ///
    /// This function breaks down a `(Co)Domain` mismatch into a more convenient representation.
    ///
    /// # Return
    ///
    /// Return `None` if `self` is not a `DomainMismatch` nor a `CodomainMismatch`.
    ///
    /// Otherwise, return the following tuple:
    ///  - the original expected type.
    ///  - the original inferred type.
    ///  - a type path pointing at the subtypes which failed to be unified.
    ///  - the final error, which is the actual cause of that failure.
    pub fn into_type_path(self) -> Option<(UnifType, UnifType, ty_path::Path, Self)> {
        let mut curr: Self = self;
        let mut path = ty_path::Path::new();
        // The original expected and inferred type. They are just updated once, in the first
        // iteration of the loop below.
        let mut utys: Option<(UnifType, UnifType)> = None;

        loop {
            match curr {
                UnifError::DomainMismatch {
                    expected:
                        expected @ UnifType::Concrete {
                            typ: TypeF::Arrow(_, _),
                            ..
                        },
                    inferred:
                        inferred @ UnifType::Concrete {
                            typ: TypeF::Arrow(_, _),
                            ..
                        },
                    cause: mismatch,
                } => {
                    utys = utys.or(Some((expected, inferred)));
                    path.push(ty_path::Elem::Domain);
                    curr = *mismatch;
                }
                UnifError::DomainMismatch { .. } => panic!(
                    "typechecking::to_type_path(): domain mismatch error on a non arrow type"
                ),
                UnifError::CodomainMismatch {
                    expected:
                        expected @ UnifType::Concrete {
                            typ: TypeF::Arrow(_, _),
                            ..
                        },
                    inferred:
                        inferred @ UnifType::Concrete {
                            typ: TypeF::Arrow(_, _),
                            ..
                        },
                    cause: mismatch,
                } => {
                    utys = utys.or(Some((expected, inferred)));
                    path.push(ty_path::Elem::Codomain);
                    curr = *mismatch;
                }
                UnifError::CodomainMismatch { .. } => panic!(
                    "typechecking::to_type_path(): codomain mismatch error on a non arrow type"
                ),
                // utys equals to `None` iff we did not even enter the case above once, i.e. if
                // `self` was indeed neither a `DomainMismatch` nor a `CodomainMismatch`
                _ => break utys.map(|(expected, inferred)| (expected, inferred, path, curr)),
            }
        }
    }
}
