//! Internal error types for typechecking.
use super::{reporting, State, UnifType, VarId};
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
    MissingDynTail(),
    /// The RHS had a binding that was not in the LHS.
    ExtraRow(LocIdent),
    /// The RHS had a additional `Dyn` tail.
    ExtraDynTail(),
    /// There were two incompatible definitions for the same record row.
    RecordRowMismatch(LocIdent, Box<UnifError>),
    /// There were two incompatible definitions for the same enum row.
    ///
    /// Because enum rows have an optional argument, there might not be any underlying unification
    /// error (e.g. one of the row has an argument, and the other does not). This is why the
    /// underlying unification error is optional, as opposed to record rows.
    EnumRowMismatch(LocIdent, Option<Box<UnifError>>),
    /// A [row constraint][super::RowConstr] was violated.
    UnsatConstr(LocIdent, UnifType),
    /// Tried to unify a type constant with another different type.
    WithConst(VarKindDiscriminant, usize, UnifType),
    /// Tried to unify two distinct type constants.
    ConstMismatch(VarKindDiscriminant, usize, usize),
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
    pub fn into_unif_err(self, left: UnifType, right: UnifType) -> UnifError {
        match self {
            RowUnifError::MissingRow(id) => UnifError::MissingRow(id, left, right),
            RowUnifError::MissingDynTail() => UnifError::MissingDynTail(left, right),
            RowUnifError::ExtraRow(id) => UnifError::ExtraRow(id, left, right),
            RowUnifError::ExtraDynTail() => UnifError::ExtraDynTail(left, right),
            RowUnifError::RecordRowMismatch(id, err) => {
                UnifError::RecordRowMismatch(id, left, right, err)
            }
            RowUnifError::EnumRowMismatch(id, err) => {
                UnifError::EnumRowMismatch(id, left, right, err)
            }
            RowUnifError::UnsatConstr(id, uty) => UnifError::RowConflict(id, uty, left, right),
            RowUnifError::WithConst(c, k, uty) => UnifError::WithConst(c, k, uty),
            RowUnifError::ConstMismatch(k, c1, c2) => UnifError::ConstMismatch(k, c1, c2),
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
#[derive(Debug, PartialEq)]
pub enum UnifError {
    /// Tried to unify two incompatible types.
    TypeMismatch(UnifType, UnifType),
    /// There are two incompatible definitions for the same row.
    RecordRowMismatch(LocIdent, UnifType, UnifType, Box<UnifError>),
    /// There are two incompatible definitions for the same row.
    ///
    /// Because enum rows have an optional argument, there might not be any underlying unification
    /// error (e.g. one of the row has an argument, and the other does not). This is why the
    /// underlying unification error is optional, as opposed to record rows.
    EnumRowMismatch(LocIdent, UnifType, UnifType, Option<Box<UnifError>>),
    /// Tried to unify two distinct type constants.
    ConstMismatch(VarKindDiscriminant, usize, usize),
    /// Tried to unify two rows, but an identifier of the LHS was absent from the RHS.
    MissingRow(LocIdent, UnifType, UnifType),
    /// Tried to unify two rows, but the `Dyn` tail of the RHS was absent from the LHS.
    MissingDynTail(UnifType, UnifType),
    /// Tried to unify two rows, but an identifier of the RHS was absent from the LHS.
    ExtraRow(LocIdent, UnifType, UnifType),
    /// Tried to unify two rows, but the `Dyn` tail of the RHS was absent from the LHS.
    ExtraDynTail(UnifType, UnifType),
    /// Tried to unify a unification variable with a row type violating the [row
    /// constraints][super::RowConstr] of the variable.
    RowConflict(LocIdent, UnifType, UnifType, UnifType),
    /// Tried to unify a type constant with another different type.
    WithConst(VarKindDiscriminant, usize, UnifType),
    /// A flat type, which is an opaque type corresponding to custom contracts, contained a Nickel
    /// term different from a variable. Only a variables is a legal inner term of a flat type.
    IncomparableFlatTypes(RichTerm, RichTerm),
    /// An unbound type variable was referenced.
    UnboundTypeVariable(LocIdent),
    /// An error occurred when unifying the domains of two arrows.
    DomainMismatch(UnifType, UnifType, Box<UnifError>),
    /// An error occurred when unifying the codomains of two arrows.
    CodomainMismatch(UnifType, UnifType, Box<UnifError>),
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
        pos_opt: TermPos,
    ) -> TypecheckError {
        match self {
            UnifError::TypeMismatch(ty1, ty2) => TypecheckError::TypeMismatch(
                names_reg.to_type(state.table, ty1),
                names_reg.to_type(state.table, ty2),
                pos_opt,
            ),
            UnifError::RecordRowMismatch(ident, uty1, uty2, err) => {
                TypecheckError::RecordRowMismatch(
                    ident,
                    names_reg.to_type(state.table, uty1),
                    names_reg.to_type(state.table, uty2),
                    Box::new((*err).into_typecheck_err_(state, names_reg, TermPos::None)),
                    pos_opt,
                )
            }
            UnifError::EnumRowMismatch(ident, uty1, uty2, err) => TypecheckError::EnumRowMismatch(
                ident,
                names_reg.to_type(state.table, uty1),
                names_reg.to_type(state.table, uty2),
                err.map(|err| {
                    Box::new((*err).into_typecheck_err_(state, names_reg, TermPos::None))
                }),
                pos_opt,
            ),
            // TODO: for now, failure to unify with a type constant causes the same error as a
            // usual type mismatch. It could be nice to have a specific error message in the
            // future.
            UnifError::ConstMismatch(k, c1, c2) => TypecheckError::TypeMismatch(
                names_reg.to_type(state.table, UnifType::from_constant_of_kind(c1, k)),
                names_reg.to_type(state.table, UnifType::from_constant_of_kind(c2, k)),
                pos_opt,
            ),
            UnifError::WithConst(VarKindDiscriminant::Type, c, ty) => TypecheckError::TypeMismatch(
                names_reg.to_type(state.table, UnifType::Constant(c)),
                names_reg.to_type(state.table, ty),
                pos_opt,
            ),
            UnifError::WithConst(kind, c, ty) => TypecheckError::ForallParametricityViolation {
                kind,
                tail: names_reg.to_type(state.table, UnifType::from_constant_of_kind(c, kind)),
                violating_type: names_reg.to_type(state.table, ty),
                pos: pos_opt,
            },
            UnifError::IncomparableFlatTypes(rt1, rt2) => {
                TypecheckError::IncomparableFlatTypes(rt1, rt2, pos_opt)
            }
            UnifError::MissingRow(id, uty1, uty2) => TypecheckError::MissingRow(
                id,
                names_reg.to_type(state.table, uty1),
                names_reg.to_type(state.table, uty2),
                pos_opt,
            ),
            UnifError::MissingDynTail(uty1, uty2) => TypecheckError::MissingDynTail(
                names_reg.to_type(state.table, uty1),
                names_reg.to_type(state.table, uty2),
                pos_opt,
            ),
            UnifError::ExtraRow(id, uty1, uty2) => TypecheckError::ExtraRow(
                id,
                names_reg.to_type(state.table, uty1),
                names_reg.to_type(state.table, uty2),
                pos_opt,
            ),
            UnifError::ExtraDynTail(uty1, uty2) => TypecheckError::ExtraDynTail(
                names_reg.to_type(state.table, uty1),
                names_reg.to_type(state.table, uty2),
                pos_opt,
            ),
            UnifError::RowConflict(id, uty, left, right) => TypecheckError::RowConflict(
                id,
                names_reg.to_type(state.table, uty),
                names_reg.to_type(state.table, left),
                names_reg.to_type(state.table, right),
                pos_opt,
            ),
            UnifError::UnboundTypeVariable(ident) => TypecheckError::UnboundTypeVariable(ident),
            err @ UnifError::CodomainMismatch(_, _, _)
            | err @ UnifError::DomainMismatch(_, _, _) => {
                let (expd, actual, path, err_final) = err.into_type_path().unwrap();
                TypecheckError::ArrowTypeMismatch(
                    names_reg.to_type(state.table, expd),
                    names_reg.to_type(state.table, actual),
                    path,
                    Box::new(err_final.into_typecheck_err_(state, names_reg, TermPos::None)),
                    pos_opt,
                )
            }
            UnifError::VarLevelMismatch {
                constant_id,
                var_kind,
            } => TypecheckError::VarLevelMismatch {
                type_var: names_reg.gen_cst_name(constant_id, var_kind).into(),
                pos: pos_opt,
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
    ///  - the original actual type.
    ///  - a type path pointing at the subtypes which failed to be unified.
    ///  - the final error, which is the actual cause of that failure.
    pub fn into_type_path(self) -> Option<(UnifType, UnifType, ty_path::Path, Self)> {
        let mut curr: Self = self;
        let mut path = ty_path::Path::new();
        // The original expected and actual type. They are just updated once, in the first
        // iteration of the loop below.
        let mut utys: Option<(UnifType, UnifType)> = None;

        loop {
            match curr {
                UnifError::DomainMismatch(
                    uty1 @ UnifType::Concrete {
                        typ: TypeF::Arrow(_, _),
                        ..
                    },
                    uty2 @ UnifType::Concrete {
                        typ: TypeF::Arrow(_, _),
                        ..
                    },
                    err,
                ) => {
                    utys = utys.or(Some((uty1, uty2)));
                    path.push(ty_path::Elem::Domain);
                    curr = *err;
                }
                UnifError::DomainMismatch(_, _, _) => panic!(
                    "typechecking::to_type_path(): domain mismatch error on a non arrow type"
                ),
                UnifError::CodomainMismatch(
                    uty1 @ UnifType::Concrete {
                        typ: TypeF::Arrow(_, _),
                        ..
                    },
                    uty2 @ UnifType::Concrete {
                        typ: TypeF::Arrow(_, _),
                        ..
                    },
                    err,
                ) => {
                    utys = utys.or(Some((uty1, uty2)));
                    path.push(ty_path::Elem::Codomain);
                    curr = *err;
                }
                UnifError::CodomainMismatch(_, _, _) => panic!(
                    "typechecking::to_type_path(): codomain mismatch error on a non arrow type"
                ),
                // utys equals to `None` iff we did not even enter the case above once, i.e. if
                // `self` was indeed neither a `DomainMismatch` nor a `CodomainMismatch`
                _ => break utys.map(|(expd, actual)| (expd, actual, path, curr)),
            }
        }
    }
}
