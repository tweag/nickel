//! Internal error types for typechecking.
use super::{reporting, State, UnifType};
use crate::{
    error::TypecheckError, identifier::Ident, label::ty_path, position::TermPos, term::RichTerm,
    types::TypeF,
};

/// Error during the unification of two row types.
#[derive(Debug, PartialEq)]
pub enum RowUnifError {
    /// The LHS had a binding that was missing in the RHS.
    MissingRow(Ident),
    /// The LHS had a `Dyn` tail that was missing in the RHS.
    MissingDynTail(),
    /// The RHS had a binding that was not in the LHS.
    ExtraRow(Ident),
    /// The RHS had a additional `Dyn` tail.
    ExtraDynTail(),
    /// There were two incompatible definitions for the same row.
    RowMismatch(Ident, Box<UnifError>),
    /// Tried to unify an enum row and a record row.
    RowKindMismatch(Ident, Option<UnifType>, Option<UnifType>),
    /// A [row constraint][super::RowConstr] was violated.
    UnsatConstr(Ident, Option<UnifType>),
    /// Tried to unify a type constant with another different type.
    WithConst(usize, UnifType),
    /// Tried to unify two distinct type constants.
    ConstMismatch(usize, usize),
    /// An unbound type variable was referenced.
    UnboundTypeVariable(Ident),
}

impl RowUnifError {
    /// Convert a row unification error to a unification error.
    ///
    /// There is a hierarchy between error types, from the most local/specific to the most high-level:
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
            RowUnifError::RowKindMismatch(id, uty1, uty2) => {
                UnifError::RowKindMismatch(id, uty1, uty2)
            }
            RowUnifError::RowMismatch(id, err) => UnifError::RowMismatch(id, left, right, err),
            RowUnifError::UnsatConstr(id, uty) => UnifError::RowConflict(id, uty, left, right),
            RowUnifError::WithConst(c, uty) => UnifError::WithConst(c, uty),
            RowUnifError::ConstMismatch(c1, c2) => UnifError::ConstMismatch(c1, c2),
            RowUnifError::UnboundTypeVariable(id) => UnifError::UnboundTypeVariable(id),
        }
    }
}

/// Error during the unification of two types.
#[derive(Debug, PartialEq)]
pub enum UnifError {
    /// Tried to unify two incompatible types.
    TypeMismatch(UnifType, UnifType),
    /// There are two incompatible definitions for the same row.
    RowMismatch(Ident, UnifType, UnifType, Box<UnifError>),
    /// Tried to unify an enum row and a record row.
    RowKindMismatch(Ident, Option<UnifType>, Option<UnifType>),
    /// Tried to unify two distinct type constants.
    ConstMismatch(usize, usize),
    /// Tried to unify two rows, but an identifier of the LHS was absent from the RHS.
    MissingRow(Ident, UnifType, UnifType),
    /// Tried to unify two rows, but the `Dyn` tail of the RHS was absent from the LHS.
    MissingDynTail(UnifType, UnifType),
    /// Tried to unify two rows, but an identifier of the RHS was absent from the LHS.
    ExtraRow(Ident, UnifType, UnifType),
    /// Tried to unify two rows, but the `Dyn` tail of the RHS was absent from the LHS.
    ExtraDynTail(UnifType, UnifType),
    /// Tried to unify a unification variable with a row type violating the [row
    /// constraints][super::RowConstr] of the variable.
    RowConflict(Ident, Option<UnifType>, UnifType, UnifType),
    /// Tried to unify a type constant with another different type.
    WithConst(usize, UnifType),
    /// A flat type, which is an opaque type corresponding to custom contracts, contained a Nickel
    /// term different from a variable. Only a variables is a legal inner term of a flat type.
    IncomparableFlatTypes(RichTerm, RichTerm),
    /// An unbound type variable was referenced.
    UnboundTypeVariable(Ident),
    /// An error occurred when unifying the domains of two arrows.
    DomainMismatch(UnifType, UnifType, Box<UnifError>),
    /// An error occurred when unifying the codomains of two arrows.
    CodomainMismatch(UnifType, UnifType, Box<UnifError>),
}

impl UnifError {
    /// Convert a unification error to a typechecking error.
    ///
    /// Wrapper that calls [`Self::into_typecheck_err_`] with an empty [name
    /// registry][reporting::NameReg].
    pub fn into_typecheck_err(self, state: &State, pos_opt: TermPos) -> TypecheckError {
        self.into_typecheck_err_(state, &mut reporting::NameReg::new(), pos_opt)
    }

    /// Convert a unification error to a typechecking error.
    ///
    /// There is a hierarchy between error types, from the most local/specific to the most high-level:
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
    /// - `names`: a [name registry][reporting::NameReg], structure used to assign
    /// unique a humain-readable names to unification variables and type constants.
    /// - `pos_opt`: the position span of the expression that failed to typecheck.
    pub fn into_typecheck_err_(
        self,
        state: &State,
        names: &mut reporting::NameReg,
        pos_opt: TermPos,
    ) -> TypecheckError {
        match self {
            UnifError::TypeMismatch(ty1, ty2) => TypecheckError::TypeMismatch(
                reporting::to_type(state.table, state.names, names, ty1),
                reporting::to_type(state.table, state.names, names, ty2),
                pos_opt,
            ),
            UnifError::RowMismatch(ident, uty1, uty2, err) => TypecheckError::RowMismatch(
                ident,
                reporting::to_type(state.table, state.names, names, uty1),
                reporting::to_type(state.table, state.names, names, uty2),
                Box::new((*err).into_typecheck_err_(state, names, TermPos::None)),
                pos_opt,
            ),
            UnifError::RowKindMismatch(id, ty1, ty2) => TypecheckError::RowKindMismatch(
                id,
                ty1.map(|tw| reporting::to_type(state.table, state.names, names, tw)),
                ty2.map(|tw| reporting::to_type(state.table, state.names, names, tw)),
                pos_opt,
            ),
            // TODO: for now, failure to unify with a type constant causes the same error as a
            // usual type mismatch. It could be nice to have a specific error message in the
            // future.
            UnifError::ConstMismatch(c1, c2) => TypecheckError::TypeMismatch(
                reporting::to_type(state.table, state.names, names, UnifType::Constant(c1)),
                reporting::to_type(state.table, state.names, names, UnifType::Constant(c2)),
                pos_opt,
            ),
            UnifError::WithConst(c, ty) => TypecheckError::TypeMismatch(
                reporting::to_type(state.table, state.names, names, UnifType::Constant(c)),
                reporting::to_type(state.table, state.names, names, ty),
                pos_opt,
            ),
            UnifError::IncomparableFlatTypes(rt1, rt2) => {
                TypecheckError::IncomparableFlatTypes(rt1, rt2, pos_opt)
            }
            UnifError::MissingRow(id, uty1, uty2) => TypecheckError::MissingRow(
                id,
                reporting::to_type(state.table, state.names, names, uty1),
                reporting::to_type(state.table, state.names, names, uty2),
                pos_opt,
            ),
            UnifError::MissingDynTail(uty1, uty2) => TypecheckError::MissingDynTail(
                reporting::to_type(state.table, state.names, names, uty1),
                reporting::to_type(state.table, state.names, names, uty2),
                pos_opt,
            ),
            UnifError::ExtraRow(id, uty1, uty2) => TypecheckError::ExtraRow(
                id,
                reporting::to_type(state.table, state.names, names, uty1),
                reporting::to_type(state.table, state.names, names, uty2),
                pos_opt,
            ),
            UnifError::ExtraDynTail(uty1, uty2) => TypecheckError::ExtraDynTail(
                reporting::to_type(state.table, state.names, names, uty1),
                reporting::to_type(state.table, state.names, names, uty2),
                pos_opt,
            ),
            UnifError::RowConflict(id, uty, left, right) => TypecheckError::RowConflict(
                id,
                uty.map(|uty| reporting::to_type(state.table, state.names, names, uty)),
                reporting::to_type(state.table, state.names, names, left),
                reporting::to_type(state.table, state.names, names, right),
                pos_opt,
            ),
            UnifError::UnboundTypeVariable(ident) => {
                TypecheckError::UnboundTypeVariable(ident, pos_opt)
            }
            err @ UnifError::CodomainMismatch(_, _, _)
            | err @ UnifError::DomainMismatch(_, _, _) => {
                let (expd, actual, path, err_final) = err.into_type_path().unwrap();
                TypecheckError::ArrowTypeMismatch(
                    reporting::to_type(state.table, state.names, names, expd),
                    reporting::to_type(state.table, state.names, names, actual),
                    path,
                    Box::new(err_final.into_typecheck_err_(state, names, TermPos::None)),
                    pos_opt,
                )
            }
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
                    uty1 @ UnifType::Concrete(TypeF::Arrow(_, _)),
                    uty2 @ UnifType::Concrete(TypeF::Arrow(_, _)),
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
                    uty1 @ UnifType::Concrete(TypeF::Arrow(_, _)),
                    uty2 @ UnifType::Concrete(TypeF::Arrow(_, _)),
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
