//! Implementation of the typechecker.
//!
//! # Mode
//!
//! Typechecking can be made in to different modes:
//! - **Strict**: correspond to traditional typechecking in strongly, statically typed languages.
//! This happens inside a `Promise` block. Promise block are introduced by the typing operator `:`,
//! as in `1 + 1 : Num` or `let f : Num -> Num = fun x => x + 1 in ..`.
//! - **Non strict**: do not enforce any typing, but still store the annotations of let bindings in
//! the environment, and continue to traverse the AST looking for other `Promise` blocks to
//! typecheck.
//!
//! The algorithm starts in non strict mode. It is switched to strict mode when entering a
//! `Promise` block, and is switched to non-strict mode when entering an `Assume` block.  `Promise`
//! and `Assume` thus serve both two purposes: annotate a term with a type, and set the
//! typechecking mode.
//!
//! # Type inference
//!
//! Type inference is done via a standard unification algorithm. The type of unannotated let-bound
//! expressions (the type of `bound_exp` in `let x = bound_exp in body`) is inferred in strict
//! mode, but it is never implicitly generalized. For example, the following program is rejected:
//!
//! ```
//! // Rejected
//! let id = fun x => x in seq (id "a") (id 5) : Num
//! ```
//!
//! Indeed, `id` is given the type `_a -> _a`, where `_a` is a unification variable, but is not
//! generalized to `forall a. a -> a`. At the first call site, `_a` is unified with `Str`, and at the second
//! call site the typechecker complains that `5` is not of type `Str`.
//!
//! This restriction is on purpose, as generalization is not trivial to implement efficiently and
//! can interact with other parts of type inference. If polymorphism is required, a simple
//! annotation is sufficient:
//!
//! ```
//! // Accepted
//! let id : forall a. a -> a = fun x => x in seq (id "a") (id 5) : Num
//! ```
//!
//! In non-strict mode, all let-bound expressions are given type `Dyn`, unless annotated.
use crate::error::TypecheckError;
use crate::eval;
use crate::identifier::Ident;
use crate::label::ty_path;
use crate::position::RawSpan;
use crate::program::ImportResolver;
use crate::term::{BinaryOp, MetaValue, RichTerm, StrChunk, Term, UnaryOp};
use crate::types::{AbsType, Types};
use crate::{mk_tyw_arrow, mk_tyw_enum, mk_tyw_enum_row, mk_tyw_record, mk_tyw_row};
use std::collections::{HashMap, HashSet};

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
    RowMismatch(Ident, UnifError),
    /// Tried to unify an enum row and a record row.
    RowKindMismatch(Ident, Option<TypeWrapper>, Option<TypeWrapper>),
    /// One of the row was ill-formed (typically, a tail was neither a row, a variable nor `Dyn`).
    ///
    /// This should probably not happen with proper restrictions on the parser and a correct
    /// typechecking algorithm. We let it as an error for now, but it could be removed in the
    /// future.
    IllformedRow(TypeWrapper),
    /// A [row constraint](./type.RowConstr.html) was violated.
    UnsatConstr(Ident, Option<TypeWrapper>),
    /// Tried to unify a type constant with another different type.
    WithConst(usize, TypeWrapper),
    /// Tried to unify two distinct type constants.
    ConstMismatch(usize, usize),
}

impl RowUnifError {
    /// Convert a row unification error to a unification error.
    ///
    /// There is a hierarchy between error types, from the most local/specific to the most high-level:
    /// - [`RowUnifError`](./enum.RowUnifError.html)
    /// - [`UnifError`](./enum.UnifError.html)
    /// - [`TypecheckError`](../errors/enum.TypecheckError.html)
    ///
    /// Each level usually adds information (such as types or positions) and group different
    /// specific errors into most general ones.
    pub fn to_unif_err(self, left: TypeWrapper, right: TypeWrapper) -> UnifError {
        match self {
            RowUnifError::MissingRow(id) => UnifError::MissingRow(id, left, right),
            RowUnifError::MissingDynTail() => UnifError::MissingDynTail(left, right),
            RowUnifError::ExtraRow(id) => UnifError::ExtraRow(id, left, right),
            RowUnifError::ExtraDynTail() => UnifError::ExtraDynTail(left, right),
            RowUnifError::RowKindMismatch(id, tyw1, tyw2) => {
                UnifError::RowKindMismatch(id, tyw1, tyw2)
            }
            RowUnifError::RowMismatch(id, err) => {
                UnifError::RowMismatch(id, left, right, Box::new(err))
            }
            RowUnifError::IllformedRow(tyw) => UnifError::IllformedRow(tyw),
            RowUnifError::UnsatConstr(id, tyw) => UnifError::RowConflict(id, tyw, left, right),
            RowUnifError::WithConst(c, tyw) => UnifError::WithConst(c, tyw),
            RowUnifError::ConstMismatch(c1, c2) => UnifError::ConstMismatch(c1, c2),
        }
    }
}

/// Error during the unification of two types.
#[derive(Debug, PartialEq)]
pub enum UnifError {
    /// Tried to unify two incompatible types.
    TypeMismatch(TypeWrapper, TypeWrapper),
    /// There are two incompatible definitions for the same row.
    RowMismatch(Ident, TypeWrapper, TypeWrapper, Box<UnifError>),
    /// Tried to unify an enum row and a record row.
    RowKindMismatch(Ident, Option<TypeWrapper>, Option<TypeWrapper>),
    /// Tried to unify two distinct type constants.
    ConstMismatch(usize, usize),
    /// Tried to unify two rows, but an identifier of the LHS was absent from the RHS.
    MissingRow(Ident, TypeWrapper, TypeWrapper),
    /// Tried to unify two rows, but the `Dyn` tail of the RHS was absent from the LHS.
    MissingDynTail(TypeWrapper, TypeWrapper),
    /// Tried to unify two rows, but an identifier of the RHS was absent from the LHS.
    ExtraRow(Ident, TypeWrapper, TypeWrapper),
    /// Tried to unify two rows, but the `Dyn` tail of the RHS was absent from the LHS.
    ExtraDynTail(TypeWrapper, TypeWrapper),
    /// A row was ill-formed.
    IllformedRow(TypeWrapper),
    /// Tried to unify a unification variable with a row type violating the [row
    /// constraints](./type.RowConstr.html) of the variable.
    RowConflict(Ident, Option<TypeWrapper>, TypeWrapper, TypeWrapper),
    /// Tried to unify a type constant with another different type.
    WithConst(usize, TypeWrapper),
    /// A flat type, which is an opaque type corresponding to custom contracts, contained a Nickel
    /// term different from a variable. Only a variables is a legal inner term of a flat type.
    IllformedFlatType(RichTerm),
    /// A generic type was ill-formed. Currently, this happens if a `StatRecord` or `Enum` type
    /// does not contain a row type.
    IllformedType(TypeWrapper),
    /// An unbound type variable was referenced.
    UnboundTypeVariable(Ident),
    /// An error occurred when unifying the domains of two arrows.
    DomainMismatch(TypeWrapper, TypeWrapper, Box<UnifError>),
    /// An error occurred when unifying the codomains of two arrows.
    CodomainMismatch(TypeWrapper, TypeWrapper, Box<UnifError>),
}

impl UnifError {
    /// Convert a unification error to a typechecking error.
    ///
    /// Wrapper that calls [`to_typecheck_err_`](./fn.to_typecheck_err_.html) with an empty [name
    /// registry](./reporting/struct.NameReg.html).
    pub fn to_typecheck_err(self, state: &State, pos_opt: &Option<RawSpan>) -> TypecheckError {
        self.to_typecheck_err_(state, &mut reporting::NameReg::new(), pos_opt)
    }

    /// Convert a unification error to a typechecking error.
    ///
    /// There is a hierarchy between error types, from the most local/specific to the most high-level:
    /// - [`RowUnifError`](./enum.RowUnifError.html)
    /// - [`UnifError`](./enum.UnifError.html)
    /// - [`TypecheckError`](../errors/enum.TypecheckError.html)
    ///
    /// Each level usually adds information (such as types or positions) and group different
    /// specific errors into most general ones.
    ///
    /// # Parameters
    ///
    /// - `state`: the state of unification. Used to access the unification table, and the original
    /// names of of unification variable or type constant.
    /// - `names`: a [name registry](./reporting/struct.NameReg.html), structure used to assign
    /// unique a humain-readable names to unification variables and type constants.
    /// - `pos_opt`: the position span of the expression that failed to typecheck.
    pub fn to_typecheck_err_(
        self,
        state: &State,
        names: &mut reporting::NameReg,
        pos_opt: &Option<RawSpan>,
    ) -> TypecheckError {
        let pos_opt = pos_opt.as_ref().cloned();
        match self {
            UnifError::TypeMismatch(ty1, ty2) => TypecheckError::TypeMismatch(
                reporting::to_type(state, names, ty1),
                reporting::to_type(state, names, ty2),
                pos_opt,
            ),
            UnifError::RowMismatch(ident, tyw1, tyw2, err) => TypecheckError::RowMismatch(
                ident,
                reporting::to_type(state, names, tyw1),
                reporting::to_type(state, names, tyw2),
                Box::new((*err).to_typecheck_err_(state, names, &None)),
                pos_opt,
            ),
            UnifError::RowKindMismatch(id, ty1, ty2) => TypecheckError::RowKindMismatch(
                id,
                ty1.map(|tw| reporting::to_type(state, names, tw)),
                ty2.map(|tw| reporting::to_type(state, names, tw)),
                pos_opt,
            ),
            // TODO: for now, failure to unify with a type constant causes the same error as a
            // usual type mismatch. It could be nice to have a specific error message in the
            // future.
            UnifError::ConstMismatch(c1, c2) => TypecheckError::TypeMismatch(
                reporting::to_type(state, names, TypeWrapper::Constant(c1)),
                reporting::to_type(state, names, TypeWrapper::Constant(c2)),
                pos_opt,
            ),
            UnifError::WithConst(c, ty) => TypecheckError::TypeMismatch(
                reporting::to_type(state, names, TypeWrapper::Constant(c)),
                reporting::to_type(state, names, ty),
                pos_opt,
            ),
            UnifError::IllformedFlatType(rt) => {
                TypecheckError::IllformedType(Types(AbsType::Flat(rt)))
            }
            UnifError::IllformedType(tyw) => {
                TypecheckError::IllformedType(reporting::to_type(state, names, tyw))
            }
            UnifError::MissingRow(id, tyw1, tyw2) => TypecheckError::MissingRow(
                id,
                reporting::to_type(state, names, tyw1),
                reporting::to_type(state, names, tyw2),
                pos_opt,
            ),
            UnifError::MissingDynTail(tyw1, tyw2) => TypecheckError::MissingDynTail(
                reporting::to_type(state, names, tyw1),
                reporting::to_type(state, names, tyw2),
                pos_opt,
            ),

            UnifError::ExtraRow(id, tyw1, tyw2) => TypecheckError::ExtraRow(
                id,
                reporting::to_type(state, names, tyw1),
                reporting::to_type(state, names, tyw2),
                pos_opt,
            ),
            UnifError::ExtraDynTail(tyw1, tyw2) => TypecheckError::ExtraDynTail(
                reporting::to_type(state, names, tyw1),
                reporting::to_type(state, names, tyw2),
                pos_opt,
            ),
            UnifError::IllformedRow(tyw) => {
                TypecheckError::IllformedType(reporting::to_type(state, names, tyw))
            }
            UnifError::RowConflict(id, tyw, left, right) => TypecheckError::RowConflict(
                id,
                tyw.map(|tyw| reporting::to_type(state, names, tyw)),
                reporting::to_type(state, names, left),
                reporting::to_type(state, names, right),
                pos_opt,
            ),
            UnifError::UnboundTypeVariable(ident) => {
                TypecheckError::UnboundTypeVariable(ident, pos_opt)
            }
            err @ UnifError::CodomainMismatch(_, _, _)
            | err @ UnifError::DomainMismatch(_, _, _) => {
                let (expd, actual, path, err_final) = err.to_type_path().unwrap();
                TypecheckError::ArrowTypeMismatch(
                    reporting::to_type(state, names, expd),
                    reporting::to_type(state, names, actual),
                    path,
                    Box::new(err_final.to_typecheck_err_(state, names, &None)),
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
    pub fn to_type_path(self) -> Option<(TypeWrapper, TypeWrapper, ty_path::Path, Self)> {
        let mut curr: Self = self;
        let mut path = ty_path::Path::new();
        // The original expected and actual type. They are just updated once, in the first
        // iteration of the loop below.
        let mut tyws: Option<(TypeWrapper, TypeWrapper)> = None;

        loop {
            match curr {
                UnifError::DomainMismatch(
                    tyw1 @ TypeWrapper::Concrete(AbsType::Arrow(_, _)),
                    tyw2 @ TypeWrapper::Concrete(AbsType::Arrow(_, _)),
                    err,
                ) => {
                    tyws = tyws.or(Some((tyw1, tyw2)));
                    path.push(ty_path::Elem::Domain);
                    curr = *err;
                }
                UnifError::DomainMismatch(_, _, _) => panic!(
                    "typechecking::to_type_path(): domain mismatch error on a non arrow type"
                ),
                UnifError::CodomainMismatch(
                    tyw1 @ TypeWrapper::Concrete(AbsType::Arrow(_, _)),
                    tyw2 @ TypeWrapper::Concrete(AbsType::Arrow(_, _)),
                    err,
                ) => {
                    tyws = tyws.or(Some((tyw1, tyw2)));
                    path.push(ty_path::Elem::Codomain);
                    curr = *err;
                }
                UnifError::CodomainMismatch(_, _, _) => panic!(
                    "typechecking::to_type_path(): codomain mismatch error on a non arrow type"
                ),
                // tyws equals to `None` iff we did not even enter the case above once, i.e. if
                // `self` was indeed neither a `DomainMismatch` nor a `CodomainMismatch`
                _ => break tyws.map(|(expd, actual)| (expd, actual, path, curr)),
            }
        }
    }
}

/// The typing environment.
pub type Environment = HashMap<Ident, TypeWrapper>;

/// A structure holding the two typing environments, the global and the local.
///
/// The global typing environment is constructed from the global term environment (see
/// [`eval`](../eval/fn.eval.html)) which holds the Nickel builtin functions. It is a read-only
/// shared environment used to retrieve the type of such functions.
#[derive(Debug, PartialEq, Clone)]
pub struct Envs<'a> {
    global: &'a Environment,
    local: Environment,
}

impl<'a> Envs<'a> {
    /// Create an `Envs` value with an empty local environment from a global environment.
    pub fn from_global(global: &'a Environment) -> Self {
        Envs {
            global,
            local: Environment::new(),
        }
    }

    /// Populate a new global typing environment from a global term environment.
    pub fn mk_global(eval_env: &eval::Environment, table: &mut UnifTable) -> Environment {
        eval_env
            .iter()
            .map(|(id, (rc, _))| {
                (
                    id.clone(),
                    apparent_type(rc.borrow().body.as_ref(), table, false),
                )
            })
            .collect()
    }

    /// Fetch a binding from the environment. Try first in the local environment, and then in the
    /// global.
    pub fn get(&self, ident: &Ident) -> Option<TypeWrapper> {
        self.local
            .get(ident)
            .or_else(|| self.global.get(ident))
            .cloned()
    }

    /// Wrapper to insert a new binding in the local environment.
    pub fn insert(&mut self, ident: Ident, tyw: TypeWrapper) -> Option<TypeWrapper> {
        self.local.insert(ident, tyw)
    }
}

/// The shared state of unification.
pub struct State<'a> {
    /// The import resolver, to retrieve and typecheck imports.
    resolver: &'a dyn ImportResolver,
    /// The unification table.
    table: &'a mut UnifTable,
    /// Row constraints.
    constr: &'a mut RowConstr,
    /// A mapping from unification variable or constants to the name of the corresponding type
    /// variable which introduced it, if any.
    ///
    /// Used for error reporting.
    names: &'a mut HashMap<usize, Ident>,
}

/// Typecheck a term.
///
/// Return the inferred type in case of success. This is just a wrapper that calls
/// [`type_check_`](fn.type_check_.html) with a fresh unification variable as goal.
pub fn type_check(
    t: &RichTerm,
    global_eval_env: &eval::Environment,
    resolver: &dyn ImportResolver,
) -> Result<Types, TypecheckError> {
    let mut state = State {
        resolver,
        table: &mut UnifTable::new(),
        constr: &mut RowConstr::new(),
        names: &mut HashMap::new(),
    };
    let ty = TypeWrapper::Ptr(new_var(state.table));
    let global = Envs::mk_global(global_eval_env, state.table);
    type_check_(&mut state, Envs::from_global(&global), false, t, ty.clone())?;

    Ok(to_type(&state.table, ty))
}

/// Typecheck a term using the given global typing environment. Same as
/// [`type_check`](./fun.type_check.html), but it directly takes a global typing environment,
/// instead of building one from a term environment as `type_check` does.
///
/// This function is used to typecheck an import in a clean environment, when we don't have access
/// to the original term environment anymore, and hence cannot call `type_check` directly, but we
/// already have built a global typing environment.
///
/// Return the inferred type in case of success. This is just a wrapper that calls
/// [`type_check_`](fn.type_check_.html) with a fresh unification variable as goal.
pub fn type_check_in_env(
    t: &RichTerm,
    global: &Environment,
    resolver: &dyn ImportResolver,
) -> Result<Types, TypecheckError> {
    let mut state = State {
        resolver,
        table: &mut UnifTable::new(),
        constr: &mut RowConstr::new(),
        names: &mut HashMap::new(),
    };
    let ty = TypeWrapper::Ptr(new_var(state.table));
    type_check_(&mut state, Envs::from_global(global), false, t, ty.clone())?;

    Ok(to_type(&state.table, ty))
}

/// Typecheck a term against a specific type.
///
/// # Arguments
///
/// - `state`: the unification state (see [`State`](struct.State.html)).
/// - `env`: the typing environment, mapping free variable to types.
/// - `strict`: the typechecking mode.
/// - `t`: the term to check.
/// - `ty`: the type to check the term against.
fn type_check_(
    state: &mut State,
    mut envs: Envs,
    strict: bool,
    rt: &RichTerm,
    ty: TypeWrapper,
) -> Result<(), TypecheckError> {
    let RichTerm { term: t, pos } = rt;

    match t.as_ref() {
        Term::Bool(_) => unify(state, strict, ty, mk_typewrapper::bool())
            .map_err(|err| err.to_typecheck_err(state, &rt.pos)),
        Term::Num(_) => unify(state, strict, ty, mk_typewrapper::num())
            .map_err(|err| err.to_typecheck_err(state, &rt.pos)),
        Term::Str(_) => unify(state, strict, ty, mk_typewrapper::str())
            .map_err(|err| err.to_typecheck_err(state, &rt.pos)),
        Term::StrChunks(chunks) => {
            unify(state, strict, ty, mk_typewrapper::str())
                .map_err(|err| err.to_typecheck_err(state, &rt.pos))?;

            chunks
                .iter()
                .try_for_each(|chunk| -> Result<(), TypecheckError> {
                    match chunk {
                        StrChunk::Literal(_) => Ok(()),
                        StrChunk::Expr(t, _) => {
                            type_check_(state, envs.clone(), strict, t, mk_typewrapper::str())
                        }
                    }
                })
        }
        Term::Fun(x, t) => {
            let src = TypeWrapper::Ptr(new_var(state.table));
            // TODO what to do here, this makes more sense to me, but it means let x = foo in bar
            // behaves quite different to (\x.bar) foo, worth considering if it's ok to type these two differently
            // let src = TypeWrapper::The(AbsType::Dyn());
            let trg = TypeWrapper::Ptr(new_var(state.table));
            let arr = mk_tyw_arrow!(src.clone(), trg.clone());

            unify(state, strict, ty, arr).map_err(|err| err.to_typecheck_err(state, &rt.pos))?;

            envs.insert(x.clone(), src);
            type_check_(state, envs, strict, t, trg)
        }
        Term::List(terms) => {
            unify(state, strict, ty, mk_typewrapper::list())
                .map_err(|err| err.to_typecheck_err(state, &rt.pos))?;

            terms
                .iter()
                .try_for_each(|t| -> Result<(), TypecheckError> {
                    // Since lists elements are checked against the type `Dyn`, it does not make sense
                    // to typecheck them even in strict mode, as this will always fails, unless they
                    // are annotated with an `Assume(Dyn, ..)`, which will always succeed.
                    type_check_(state, envs.clone(), false, t, mk_typewrapper::dynamic())
                })
        }
        Term::Lbl(_) => {
            // TODO implement lbl type
            unify(state, strict, ty, mk_typewrapper::dynamic())
                .map_err(|err| err.to_typecheck_err(state, &rt.pos))
        }
        Term::Let(x, re, rt) => {
            let ty_let = apparent_type(re.as_ref(), state.table, strict);
            type_check_(state, envs.clone(), strict, re, ty_let.clone())?;

            // TODO move this up once lets are rec
            envs.insert(x.clone(), ty_let);
            type_check_(state, envs, strict, rt, ty)
        }
        Term::App(e, t) => {
            let src = TypeWrapper::Ptr(new_var(state.table));
            let arr = mk_tyw_arrow!(src.clone(), ty);

            // This order shouldn't be changed, since applying a function to a record
            // may change how it's typed (static or dynamic)
            // This is good hint a bidirectional algorithm would make sense...
            type_check_(state, envs.clone(), strict, e, arr)?;
            type_check_(state, envs, strict, t, src)
        }
        Term::Var(x) => {
            let x_ty = envs
                .get(&x)
                .ok_or_else(|| TypecheckError::UnboundIdentifier(x.clone(), pos.clone()))?;

            let instantiated = instantiate_foralls(state, x_ty.clone(), ForallInst::Ptr);
            unify(state, strict, ty, instantiated)
                .map_err(|err| err.to_typecheck_err(state, &rt.pos))
        }
        Term::Enum(id) => {
            let row = TypeWrapper::Ptr(new_var(state.table));
            unify(state, strict, ty, mk_tyw_enum!(id.clone(), row))
                .map_err(|err| err.to_typecheck_err(state, &rt.pos))
        }
        Term::Record(stat_map) | Term::RecRecord(stat_map) => {
            // For recursive records, we look at the apparent type of each field and bind it in
            // env before actually typechecking the content of fields
            if let Term::RecRecord(_) = t.as_ref() {
                envs.local.extend(
                    stat_map.iter().map(|(id, rt)| {
                        (id.clone(), apparent_type(rt.as_ref(), state.table, strict))
                    }),
                );
            }

            let root_ty = if let TypeWrapper::Ptr(p) = ty {
                get_root(state.table, p)
            } else {
                ty.clone()
            };

            if let TypeWrapper::Concrete(AbsType::DynRecord(rec_ty)) = root_ty.clone() {
                // Checking for a dynamic record
                stat_map
                    .into_iter()
                    .try_for_each(|(_, t)| -> Result<(), TypecheckError> {
                        type_check_(state, envs.clone(), strict, t, (*rec_ty).clone())
                    })
            } else {
                let row = stat_map.into_iter().try_fold(
                    mk_tyw_row!(),
                    |acc, (id, field)| -> Result<TypeWrapper, TypecheckError> {
                        // In the case of a recursive record, new types (either type variables or
                        // annotations) have already be determined and put in the typing
                        // environment, and we need to use the same.
                        let ty = if let Term::RecRecord(_) = t.as_ref() {
                            envs.get(&id).unwrap().clone()
                        } else {
                            TypeWrapper::Ptr(new_var(state.table))
                        };

                        type_check_(state, envs.clone(), strict, field, ty.clone())?;

                        Ok(mk_tyw_row!((id.clone(), ty); acc))
                    },
                )?;

                unify(state, strict, ty, mk_tyw_record!(; row))
                    .map_err(|err| err.to_typecheck_err(state, &rt.pos))
            }
        }
        Term::Op1(op, t) => {
            let ty_op = get_uop_type(state, envs.clone(), strict, op)?;

            let src = TypeWrapper::Ptr(new_var(state.table));
            let arr = mk_tyw_arrow!(src.clone(), ty);

            unify(state, strict, arr, ty_op).map_err(|err| err.to_typecheck_err(state, &rt.pos))?;
            type_check_(state, envs.clone(), strict, t, src)
        }
        Term::Op2(op, e, t) => {
            let ty_op = get_bop_type(state, envs.clone(), strict, op)?;

            let src1 = TypeWrapper::Ptr(new_var(state.table));
            let src2 = TypeWrapper::Ptr(new_var(state.table));
            let arr = mk_tyw_arrow!(src1.clone(), src2.clone(), ty);

            unify(state, strict, arr, ty_op).map_err(|err| err.to_typecheck_err(state, &rt.pos))?;
            type_check_(state, envs.clone(), strict, e, src1)?;
            type_check_(state, envs, strict, t, src2)
        }
        Term::Promise(ty2, _, t) => {
            let tyw2 = to_typewrapper(ty2.clone());

            let instantiated = instantiate_foralls(state, tyw2, ForallInst::Constant);

            unify(state, strict, ty.clone(), to_typewrapper(ty2.clone()))
                .map_err(|err| err.to_typecheck_err(state, &rt.pos))?;
            type_check_(state, envs, true, t, instantiated)
        }
        Term::Assume(ty2, _, t) => {
            unify(state, strict, ty.clone(), to_typewrapper(ty2.clone()))
                .map_err(|err| err.to_typecheck_err(state, &rt.pos))?;
            let new_ty = TypeWrapper::Ptr(new_var(state.table));
            type_check_(state, envs, false, t, new_ty)
        }
        Term::Sym(_) => unify(state, strict, ty, mk_typewrapper::sym())
            .map_err(|err| err.to_typecheck_err(state, &rt.pos)),
        Term::Wrapped(_, t)
        | Term::MetaValue(MetaValue {
            contract: None,
            value: Some(t),
            ..
        }) => type_check_(state, envs, strict, t, ty),
        // Handle a metavalue with a contract together with a value in the same way as an assume
        Term::MetaValue(MetaValue {
            contract: Some((ty2, _)),
            value: Some(t),
            ..
        }) => {
            unify(state, strict, ty.clone(), to_typewrapper(ty2.clone()))
                .map_err(|err| err.to_typecheck_err(state, &rt.pos))?;
            let new_ty = TypeWrapper::Ptr(new_var(state.table));
            type_check_(state, envs, false, t, new_ty)
        }
        Term::MetaValue(_) => Ok(()),
        Term::Import(_) => unify(state, strict, ty, mk_typewrapper::dynamic())
            .map_err(|err| err.to_typecheck_err(state, &rt.pos)),
        Term::ResolvedImport(file_id) => {
            let t = state
                .resolver
                .get(file_id.clone())
                .expect("Internal error: resolved import not found ({:?}) during typechecking.");
            type_check_in_env(&t, envs.global, state.resolver).map(|_ty| ())
        }
    }
}

/// Determine the apparent type of a let-bound expression.
///
/// When a let-binding `let x = bound_exp in body` is processed, the type of `bound_exp` must be
/// determined to be associated to the bound variable `x` in the typing environment (`typed_vars`).
/// Then, future occurrences of `x` can be given this type when used in a `Promise` block.
///
/// The role of `apparent_type` is precisely to determine the type of `bound_exp`:
/// - if `bound_exp` is annotated by an `Assume` or a `Promise`, use the user-provided type.
/// - Otherwise:
///     * in non strict mode, we won't (and possibly can't) infer the type of `bound_exp`: just
///       return `Dyn`.
///     * in strict mode, we will typecheck `bound_exp`: return a new unification variable to be
///       associated to `bound_exp`.
fn apparent_type(t: &Term, table: &mut UnifTable, strict: bool) -> TypeWrapper {
    match t {
        Term::Assume(ty, _, _) | Term::Promise(ty, _, _) => to_typewrapper(ty.clone()),
        Term::MetaValue(MetaValue {
            contract: Some((ty, _)),
            ..
        }) => to_typewrapper(ty.clone()),
        Term::MetaValue(MetaValue {
            contract: None,
            value: Some(v),
            ..
        }) => apparent_type(v.as_ref(), table, strict),
        _ if strict => TypeWrapper::Ptr(new_var(table)),
        _ => mk_typewrapper::dynamic(),
    }
}

/// The types on which the unification algorithm operates, which may be either a concrete type, a
/// type constant or a unification variable.
#[derive(Clone, PartialEq, Debug)]
pub enum TypeWrapper {
    /// A concrete type (like `Num` or `Str -> Str`).
    Concrete(AbsType<Box<TypeWrapper>>),
    /// A rigid type constant which cannot be unified with anything but itself.
    Constant(usize),
    /// A unification variable.
    Ptr(usize),
}

impl TypeWrapper {
    /// Substitute all the occurrences of a type variable for a typewrapper.
    pub fn subst(self, id: Ident, to: TypeWrapper) -> TypeWrapper {
        use self::TypeWrapper::*;
        match self {
            Concrete(AbsType::Var(ref i)) if *i == id => to,
            Concrete(AbsType::Var(i)) => Concrete(AbsType::Var(i)),

            Concrete(AbsType::Forall(i, t)) => {
                if i == id {
                    Concrete(AbsType::Forall(i, t))
                } else {
                    let tt = *t;
                    Concrete(AbsType::Forall(i, Box::new(tt.subst(id, to))))
                }
            }
            // Trivial recursion
            Concrete(AbsType::Dyn()) => Concrete(AbsType::Dyn()),
            Concrete(AbsType::Num()) => Concrete(AbsType::Num()),
            Concrete(AbsType::Bool()) => Concrete(AbsType::Bool()),
            Concrete(AbsType::Str()) => Concrete(AbsType::Str()),
            Concrete(AbsType::Sym()) => Concrete(AbsType::Sym()),
            Concrete(AbsType::Flat(t)) => Concrete(AbsType::Flat(t)),
            Concrete(AbsType::Arrow(s, t)) => {
                let fs = s.subst(id.clone(), to.clone());
                let ft = t.subst(id, to);

                Concrete(AbsType::Arrow(Box::new(fs), Box::new(ft)))
            }
            Concrete(AbsType::RowEmpty()) => Concrete(AbsType::RowEmpty()),
            Concrete(AbsType::RowExtend(tag, ty, rest)) => Concrete(AbsType::RowExtend(
                tag,
                ty.map(|x| Box::new(x.subst(id.clone(), to.clone()))),
                Box::new(rest.subst(id, to)),
            )),
            Concrete(AbsType::Enum(row)) => Concrete(AbsType::Enum(Box::new(row.subst(id, to)))),
            Concrete(AbsType::StaticRecord(row)) => {
                Concrete(AbsType::StaticRecord(Box::new(row.subst(id, to))))
            }
            Concrete(AbsType::DynRecord(def_ty)) => {
                Concrete(AbsType::DynRecord(Box::new(def_ty.subst(id, to))))
            }
            Concrete(AbsType::List()) => Concrete(AbsType::List()),
            Constant(x) => Constant(x),
            Ptr(x) => Ptr(x),
        }
    }
}

impl From<AbsType<Box<TypeWrapper>>> for TypeWrapper {
    fn from(ty: AbsType<Box<TypeWrapper>>) -> Self {
        TypeWrapper::Concrete(ty)
    }
}

#[macro_use]
/// Helpers for building `TypeWrapper`s.
pub mod mk_typewrapper {
    use super::{AbsType, TypeWrapper};

    /// Multi-ary arrow constructor for types implementing `Into<TypeWrapper>`.
    #[macro_export]
    macro_rules! mk_tyw_arrow {
        ($left:expr, $right:expr) => {
            $crate::typecheck::TypeWrapper::Concrete(
                $crate::types::AbsType::Arrow(
                    Box::new($crate::typecheck::TypeWrapper::from($left)),
                    Box::new($crate::typecheck::TypeWrapper::from($right))
                )
            )
        };
        ( $fst:expr, $snd:expr , $( $types:expr ),+ ) => {
            mk_tyw_arrow!($fst, mk_tyw_arrow!($snd, $( $types ),+))
        };
    }

    /// Multi-ary enum row constructor for types implementing `Into<TypeWrapper>`.
    /// `mk_tyw_enum_row!(id1, .., idn, tail)` correspond to `<id1, .., idn | tail>.
    #[macro_export]
    macro_rules! mk_tyw_enum_row {
        ($id:expr, $tail:expr) => {
            $crate::typecheck::TypeWrapper::Concrete(
                $crate::types::AbsType::RowExtend(
                    Ident::from($id),
                    None,
                    Box::new($crate::typecheck::TypeWrapper::from($tail))
                )
            )
        };
        ( $fst:expr, $snd:expr , $( $rest:expr ),+ ) => {
            mk_tyw_enum_row!($fst, mk_tyw_enum_row!($snd, $( $rest),+))
        };
    }

    /// Multi-ary record row constructor for types implementing `Into<TypeWrapper>`.
    /// `mk_tyw_row!((id1, ty1), .., (idn, tyn); tail)` correspond to `{id1: ty1, .., idn: tyn |
    /// tail}. The tail can be omitted, in which case the empty row is uses as a tail instead.
    #[macro_export]
    macro_rules! mk_tyw_row {
        () => {
            $crate::typecheck::TypeWrapper::from(AbsType::RowEmpty())
        };
        (; $tail:expr) => {
            $crate::typecheck::TypeWrapper::from($tail)
        };
        (($id:expr, $ty:expr) $(($ids:expr, $tys:expr)),* $(; $tail:expr)?) => {
            $crate::typecheck::TypeWrapper::Concrete(
                $crate::types::AbsType::RowExtend(
                    Ident::from($id),
                    Some($ty.into()),
                    Box::new(mk_tyw_row!($(($ids, $tys)),* $(; $tail)?))
                )
            )
        };
    }

    /// Wrapper around `mk_tyw_enum_row!` to build an enum type from an enum row.
    #[macro_export]
    macro_rules! mk_tyw_enum {
        ( $rows:expr ) => {
            $crate::typecheck::TypeWrapper::Concrete(
                $crate::types::AbsType::Enum(
                    Box::new($rows.into())
                )
            )
        };
        ( $fst:expr, $( $rest:expr ),+ ) => {
            mk_tyw_enum!(mk_tyw_enum_row!($fst, $( $rest),+))
        };
    }

    /// Wrapper around `mk_tyw_record!` to build a record type from a record row.
    #[macro_export]
    macro_rules! mk_tyw_record {
        ($(($ids:expr, $tys:expr)),* $(; $tail:expr)?) => {
            $crate::typecheck::TypeWrapper::Concrete(
                $crate::types::AbsType::StaticRecord(
                    Box::new(mk_tyw_row!($(($ids, $tys)),* $(; $tail)?))
                )
            )
        };
    }

    /// Generate an helper function to build a 0-ary type.
    macro_rules! generate_builder {
        ($fun:ident, $var:ident) => {
            pub fn $fun() -> TypeWrapper {
                TypeWrapper::Concrete(AbsType::$var())
            }
        };
    }

    pub fn dyn_record<T>(ty: T) -> TypeWrapper
    where
        T: Into<TypeWrapper>,
    {
        TypeWrapper::Concrete(AbsType::DynRecord(Box::new(ty.into())))
    }

    // dyn is a reserved keyword
    generate_builder!(dynamic, Dyn);
    generate_builder!(str, Str);
    generate_builder!(num, Num);
    generate_builder!(bool, Bool);
    generate_builder!(sym, Sym);
    generate_builder!(list, List);
    generate_builder!(row_empty, RowEmpty);
}

/// Look for a binding in a row, or add a new one if it is not present and if allowed by [row
/// constraints](type.RowConstr.html).
///
/// The row may be given as a concrete type or as a unification variable.
///
/// # Return
///
/// The type newly bound to `id` in the row together with the tail of the new row. If `id` was
/// already in `r`, it does not change the binding and return the corresponding type instead as a
/// first component.
fn row_add(
    state: &mut State,
    id: &Ident,
    ty: Option<Box<TypeWrapper>>,
    mut r: TypeWrapper,
) -> Result<(Option<Box<TypeWrapper>>, TypeWrapper), RowUnifError> {
    if let TypeWrapper::Ptr(p) = r {
        r = get_root(state.table, p);
    }
    match r {
        TypeWrapper::Concrete(AbsType::RowEmpty()) | TypeWrapper::Concrete(AbsType::Dyn()) => {
            Err(RowUnifError::MissingRow(id.clone()))
        }
        TypeWrapper::Concrete(AbsType::RowExtend(id2, ty2, r2)) => {
            if *id == id2 {
                Ok((ty2, *r2))
            } else {
                let (extracted_type, subrow) = row_add(state, id, ty, *r2)?;
                Ok((
                    extracted_type,
                    TypeWrapper::Concrete(AbsType::RowExtend(id2, ty2, Box::new(subrow))),
                ))
            }
        }
        TypeWrapper::Ptr(root) => {
            if let Some(set) = state.constr.get(&root) {
                if set.contains(&id) {
                    return Err(RowUnifError::UnsatConstr(id.clone(), ty.map(|tyw| *tyw)));
                }
            }
            let new_row = TypeWrapper::Ptr(new_var(state.table));
            constraint(state, new_row.clone(), id.clone())?;
            state.table.insert(
                root,
                Some(TypeWrapper::Concrete(AbsType::RowExtend(
                    id.clone(),
                    ty.clone(),
                    Box::new(new_row.clone()),
                ))),
            );
            Ok((ty, new_row))
        }
        other => Err(RowUnifError::IllformedRow(other)),
    }
}

/// Try to unify two types.
///
/// A wrapper around `unify_` which just checks if `strict` is set to true. If not, it directly
/// returns `Ok(())` without unifying anything.
pub fn unify(
    state: &mut State,
    strict: bool,
    t1: TypeWrapper,
    t2: TypeWrapper,
) -> Result<(), UnifError> {
    if strict {
        unify_(state, t1, t2)
    } else {
        Ok(())
    }
}

/// Try to unify two types.
pub fn unify_(
    state: &mut State,
    mut t1: TypeWrapper,
    mut t2: TypeWrapper,
) -> Result<(), UnifError> {
    if let TypeWrapper::Ptr(pt1) = t1 {
        t1 = get_root(state.table, pt1);
    }
    if let TypeWrapper::Ptr(pt2) = t2 {
        t2 = get_root(state.table, pt2);
    }

    // t1 and t2 are roots of the type
    match (t1, t2) {
        (TypeWrapper::Concrete(s1), TypeWrapper::Concrete(s2)) => match (s1, s2) {
            (AbsType::Dyn(), AbsType::Dyn()) => Ok(()),
            (AbsType::Num(), AbsType::Num()) => Ok(()),
            (AbsType::Bool(), AbsType::Bool()) => Ok(()),
            (AbsType::Str(), AbsType::Str()) => Ok(()),
            (AbsType::List(), AbsType::List()) => Ok(()),
            (AbsType::Sym(), AbsType::Sym()) => Ok(()),
            (AbsType::Arrow(s1s, s1t), AbsType::Arrow(s2s, s2t)) => {
                unify_(state, (*s1s).clone(), (*s2s).clone()).map_err(|err| {
                    UnifError::DomainMismatch(
                        TypeWrapper::Concrete(AbsType::Arrow(s1s.clone(), s1t.clone())),
                        TypeWrapper::Concrete(AbsType::Arrow(s2s.clone(), s2t.clone())),
                        Box::new(err),
                    )
                })?;
                unify_(state, (*s1t).clone(), (*s2t).clone()).map_err(|err| {
                    UnifError::CodomainMismatch(
                        TypeWrapper::Concrete(AbsType::Arrow(s1s, s1t)),
                        TypeWrapper::Concrete(AbsType::Arrow(s2s, s2t)),
                        Box::new(err),
                    )
                })
            }
            (AbsType::Flat(s), AbsType::Flat(t)) => match (s.as_ref(), t.as_ref()) {
                (Term::Var(vs), Term::Var(ts)) if vs == ts => Ok(()),
                (Term::Var(_), Term::Var(_)) => Err(UnifError::TypeMismatch(
                    TypeWrapper::Concrete(AbsType::Flat(s)),
                    TypeWrapper::Concrete(AbsType::Flat(t)),
                )),
                (Term::Var(_), _) => Err(UnifError::IllformedFlatType(t)),
                _ => Err(UnifError::IllformedFlatType(s)),
            },
            (r1, r2) if r1.is_row_type() && r2.is_row_type() => {
                unify_rows(state, r1.clone(), r2.clone()).map_err(|err| {
                    err.to_unif_err(TypeWrapper::Concrete(r1), TypeWrapper::Concrete(r2))
                })
            }
            (AbsType::Enum(tyw1), AbsType::Enum(tyw2)) => match (*tyw1, *tyw2) {
                (TypeWrapper::Concrete(r1), TypeWrapper::Concrete(r2))
                    if r1.is_row_type() && r2.is_row_type() =>
                {
                    unify_rows(state, r1.clone(), r2.clone())
                        .map_err(|err| err.to_unif_err(mk_tyw_enum!(r1), mk_tyw_enum!(r2)))
                }
                (TypeWrapper::Concrete(r), _) if !r.is_row_type() => {
                    Err(UnifError::IllformedType(mk_tyw_enum!(r)))
                }
                (_, TypeWrapper::Concrete(r)) if !r.is_row_type() => {
                    Err(UnifError::IllformedType(mk_tyw_enum!(r)))
                }
                (tyw1, tyw2) => unify_(state, tyw1, tyw2),
            },
            (AbsType::StaticRecord(tyw1), AbsType::StaticRecord(tyw2)) => match (*tyw1, *tyw2) {
                (TypeWrapper::Concrete(r1), TypeWrapper::Concrete(r2))
                    if r1.is_row_type() && r2.is_row_type() =>
                {
                    unify_rows(state, r1.clone(), r2.clone())
                        .map_err(|err| err.to_unif_err(mk_tyw_record!(; r1), mk_tyw_record!(; r2)))
                }
                (TypeWrapper::Concrete(r), _) if !r.is_row_type() => {
                    Err(UnifError::IllformedType(mk_tyw_record!(; r)))
                }
                (_, TypeWrapper::Concrete(r)) if !r.is_row_type() => {
                    Err(UnifError::IllformedType(mk_tyw_record!(; r)))
                }
                (tyw1, tyw2) => unify_(state, tyw1, tyw2),
            },
            (AbsType::DynRecord(t), AbsType::DynRecord(t2)) => unify_(state, *t, *t2),
            (AbsType::Forall(i1, t1t), AbsType::Forall(i2, t2t)) => {
                // Very stupid (slow) implementation
                let constant_type = TypeWrapper::Constant(new_var(state.table));

                unify_(
                    state,
                    t1t.subst(i1, constant_type.clone()),
                    t2t.subst(i2, constant_type),
                )
            }
            (AbsType::Var(ident), _) | (_, AbsType::Var(ident)) => {
                Err(UnifError::UnboundTypeVariable(ident))
            }
            (ty1, ty2) => Err(UnifError::TypeMismatch(
                TypeWrapper::Concrete(ty1),
                TypeWrapper::Concrete(ty2),
            )),
        },
        // The two following cases are not merged just to correctly distinguish between the
        // expected type (first component of the tuple) and the inferred type when reporting a row
        // unification error.
        (TypeWrapper::Ptr(p), tyw) => {
            constr_unify(state.constr, p, &tyw)
                .map_err(|err| err.to_unif_err(TypeWrapper::Ptr(p), tyw.clone()))?;
            state.table.insert(p, Some(tyw));
            Ok(())
        }
        (tyw, TypeWrapper::Ptr(p)) => {
            constr_unify(state.constr, p, &tyw)
                .map_err(|err| err.to_unif_err(tyw.clone(), TypeWrapper::Ptr(p)))?;
            state.table.insert(p, Some(tyw));
            Ok(())
        }
        (TypeWrapper::Constant(i1), TypeWrapper::Constant(i2)) if i1 == i2 => Ok(()),
        (TypeWrapper::Constant(i1), TypeWrapper::Constant(i2)) => {
            Err(UnifError::ConstMismatch(i1, i2))
        }
        (ty, TypeWrapper::Constant(i)) | (TypeWrapper::Constant(i), ty) => {
            Err(UnifError::WithConst(i, ty))
        }
    }
}

/// Try to unify two row types. Return an [`IllformedRow`](./enum.RowUnifError.html#variant.IllformedRow) error if one of the given type
/// is not a row type.
pub fn unify_rows(
    state: &mut State,
    t1: AbsType<Box<TypeWrapper>>,
    t2: AbsType<Box<TypeWrapper>>,
) -> Result<(), RowUnifError> {
    match (t1, t2) {
        (AbsType::RowEmpty(), AbsType::RowEmpty()) | (AbsType::Dyn(), AbsType::Dyn()) => Ok(()),
        (AbsType::RowEmpty(), AbsType::Dyn()) => Err(RowUnifError::ExtraDynTail()),
        (AbsType::Dyn(), AbsType::RowEmpty()) => Err(RowUnifError::MissingDynTail()),
        (AbsType::RowEmpty(), AbsType::RowExtend(ident, _, _))
        | (AbsType::Dyn(), AbsType::RowExtend(ident, _, _)) => Err(RowUnifError::ExtraRow(ident)),
        (AbsType::RowExtend(ident, _, _), AbsType::Dyn())
        | (AbsType::RowExtend(ident, _, _), AbsType::RowEmpty()) => {
            Err(RowUnifError::MissingRow(ident))
        }
        (AbsType::RowExtend(id, ty, t), r2 @ AbsType::RowExtend(_, _, _)) => {
            let (ty2, t2_tail) =
                row_add(state, &id, ty.clone(), TypeWrapper::Concrete(r2.clone()))?;
            match (ty, ty2) {
                (None, None) => Ok(()),
                (Some(ty), Some(ty2)) => unify_(state, *ty, *ty2)
                    .map_err(|err| RowUnifError::RowMismatch(id.clone(), err)),
                (ty1, ty2) => Err(RowUnifError::RowKindMismatch(
                    id,
                    ty1.map(|t| *t),
                    ty2.map(|t| *t),
                )),
            }?;

            match (*t, t2_tail) {
                (TypeWrapper::Concrete(r1_tail), TypeWrapper::Concrete(r2_tail)) => {
                    unify_rows(state, r1_tail, r2_tail)
                }
                // If one of the tail is not a concrete type, it is either a unification variable
                // or a constant (rigid type variable). `unify` already knows how to treat these
                // cases, so we delegate the work. However it returns `UnifError` instead of
                // `RowUnifError`, hence we have a bit of wrapping and unwrapping to do. Note that
                // since we are unifying types with a constant or a unification variable somewhere,
                // the only unification errors that should be possible are related to constants or
                // row constraints.
                (t1_tail, t2_tail) => unify_(state, t1_tail, t2_tail).map_err(|err| match err {
                    UnifError::ConstMismatch(c1, c2) => RowUnifError::ConstMismatch(c1, c2),
                    UnifError::WithConst(c1, tyw) => RowUnifError::WithConst(c1, tyw),
                    UnifError::RowConflict(id, tyw_opt, _, _) => {
                        RowUnifError::UnsatConstr(id, tyw_opt)
                    }
                    err => panic!(
                        "typechecker::unify_rows(): unexpected error while unifying row tails {:?}",
                        err
                    ),
                }),
            }
        }
        (ty, _) if !ty.is_row_type() => Err(RowUnifError::IllformedRow(TypeWrapper::Concrete(ty))),
        (_, ty) => Err(RowUnifError::IllformedRow(TypeWrapper::Concrete(ty))),
    }
}

/// Convert a vanilla Nickel type to a type wrapper.
fn to_typewrapper(t: Types) -> TypeWrapper {
    let Types(t2) = t;

    let t3 = t2.map(|x| Box::new(to_typewrapper(*x)));

    TypeWrapper::Concrete(t3)
}

/// Extract the concrete type corresponding to a type wrapper. Free unification variables as well
/// as type constants are replaced with the type `Dyn`.
fn to_type(table: &UnifTable, ty: TypeWrapper) -> Types {
    match ty {
        TypeWrapper::Ptr(p) => match get_root(table, p) {
            t @ TypeWrapper::Concrete(_) => to_type(table, t),
            _ => Types(AbsType::Dyn()),
        },
        TypeWrapper::Constant(_) => Types(AbsType::Dyn()),
        TypeWrapper::Concrete(t) => {
            let mapped = t.map(|btyp| Box::new(to_type(table, *btyp)));
            Types(mapped)
        }
    }
}

/// Helpers to convert a `TypeWrapper` to a human-readable `Types` representation for error
/// reporting purpose.
mod reporting {
    use super::*;
    use std::collections::HashSet;

    /// A name registry used to replace unification variables and type constants with human-readable
    /// and distinct names when reporting errors.
    pub struct NameReg {
        reg: HashMap<usize, Ident>,
        taken: HashSet<String>,
        var_count: usize,
        cst_count: usize,
    }

    impl NameReg {
        pub fn new() -> Self {
            NameReg {
                reg: HashMap::new(),
                taken: HashSet::new(),
                var_count: 0,
                cst_count: 0,
            }
        }
    }

    /// Create a fresh name candidate for a type variable or a type constant.
    ///
    /// Used by [`to_type_report`](./fn.to_type_report.html) and subfunctions
    /// [`var_to_type`](./fn.var_to_type) and [`cst_to_type`](./fn.cst_to_type) when converting a type
    /// wrapper to a human-readable representation.
    ///
    /// To select a candidate, first check in `names` if the variable or the constant corresponds to a
    /// type variable written by the user. If it is, return the name of the variable. Otherwise, use
    /// the given counter to generate a new single letter.
    ///
    /// Generated name is clearly not necessarily unique. This is handled by
    /// [`select_uniq`](./fn.select_uniq.html).
    fn mk_name(names: &HashMap<usize, Ident>, counter: &mut usize, id: usize) -> String {
        match names.get(&id) {
            // First check if that constant or variable was introduced by a forall. If it was, try
            // to use the same name.
            Some(orig) => format!("{}", orig),
            None => {
                //Otherwise, generate a new character
                let next = *counter;
                *counter += 1;
                std::char::from_u32(('a' as u32) + ((next % 26) as u32))
                    .unwrap()
                    .to_string()
            }
        }
    }

    /// Select a name distinct from all the others, starting from a candidate name for a type
    /// variable or a type constant.
    ///
    /// If the name is already taken, it just iterates by adding a numeric suffix `1`, `2`, .., and
    /// so on until a free name is found. See [`var_to_type`](./fn.var_to_type.html) and
    /// [`cst_to_type`](./fn.cst_to_type.html).
    fn select_uniq(name_reg: &mut NameReg, mut name: String, id: usize) -> Ident {
        // To avoid clashing with already picked names, we add a numeric suffix to the picked
        // letter.
        if name_reg.taken.contains(&name) {
            let mut suffix = 1;

            while name_reg.taken.contains(&format!("{}{}", name, suffix)) {
                suffix += 1;
            }

            name = format!("{}{}", name, suffix);
        }

        let ident = Ident(name);
        name_reg.reg.insert(id, ident.clone());
        ident
    }

    /// Either retrieve or generate a new fresh name for a unification variable for error reporting,
    /// and wrap it as a type variable. Constant are named `_a`, `_b`, .., `_a1`, `_b1`, .. and so on.
    fn var_to_type(names: &HashMap<usize, Ident>, name_reg: &mut NameReg, p: usize) -> Types {
        let ident = name_reg.reg.get(&p).cloned().unwrap_or_else(|| {
            // Select a candidate name and add a "_" prefix
            let name = format!("_{}", mk_name(names, &mut name_reg.var_count, p));
            // Add a suffix to make it unique if it has already been picked
            select_uniq(name_reg, name, p)
        });

        Types(AbsType::Var(ident))
    }

    /// Either retrieve or generate a new fresh name for a constant for error reporting, and wrap it as
    /// type variable. Constant are named `a`, `b`, .., `a1`, `b1`, .. and so on.
    fn cst_to_type(names: &HashMap<usize, Ident>, name_reg: &mut NameReg, c: usize) -> Types {
        let ident = name_reg.reg.get(&c).cloned().unwrap_or_else(|| {
            // Select a candidate name
            let name = mk_name(names, &mut name_reg.cst_count, c);
            // Add a suffix to make it unique if it has already been picked
            select_uniq(name_reg, name, c)
        });

        Types(AbsType::Var(ident))
    }

    /// Extract a concrete type corresponding to a type wrapper for error reporting.
    ///
    /// Similar to [`to_type`](./fn.to_type.html), excepted that free unification variables and
    /// type constants are replaced by type variables which names are determined by the
    /// [`var_to_type`](./fn.var_to_type.html) and [`cst_to_type`](./fn.cst_tot_type.html).
    /// Distinguishing occurrences of unification variables and type constants is more informative
    /// than having `Dyn` everywhere.
    pub fn to_type(state: &State, names: &mut NameReg, ty: TypeWrapper) -> Types {
        match ty {
            TypeWrapper::Ptr(p) => match get_root(state.table, p) {
                TypeWrapper::Ptr(p) => var_to_type(state.names, names, p),
                tyw => to_type(state, names, tyw),
            },
            TypeWrapper::Constant(c) => cst_to_type(state.names, names, c),
            TypeWrapper::Concrete(t) => {
                let mapped = t.map(|btyp| Box::new(to_type(state, names, *btyp)));
                Types(mapped)
            }
        }
    }
}

/// Type of the parameter controlling instantiation of foralls.
///
/// See [`instantiate_foralls`](./fn.instantiate_foralls.html).
#[derive(Copy, Clone, Debug, PartialEq)]
enum ForallInst {
    Constant,
    Ptr,
}

/// Instantiate the type variables which are quantified in head position with either unification
/// variables or type constants.
///
/// For example, if `inst` is `Constant`, `forall a. forall b. a -> (forall c. b -> c)` is
/// transformed to `cst1 -> (forall c. cst2 -> c)` where `cst1` and `cst2` are fresh type
/// constants.  This is used when typechecking `forall`s: all quantified type variables in head
/// position are replaced by rigid type constants, and the term is then typechecked normally. As
/// these constants cannot be unified with anything, this forces all the occurrences of a type
/// variable to be the same type.
///
/// # Parameters
/// - `state`: the unification state
/// - `ty`: the polymorphic type to instantiate
/// - `inst`: the type of instantiation, either by a type constant or by a unification variable
fn instantiate_foralls(state: &mut State, mut ty: TypeWrapper, inst: ForallInst) -> TypeWrapper {
    if let TypeWrapper::Ptr(p) = ty {
        ty = get_root(state.table, p);
    }

    while let TypeWrapper::Concrete(AbsType::Forall(id, forall_ty)) = ty {
        let fresh_id = new_var(state.table);
        let var = match inst {
            ForallInst::Constant => TypeWrapper::Constant(fresh_id),
            ForallInst::Ptr => TypeWrapper::Ptr(fresh_id),
        };
        state.names.insert(fresh_id, id.clone());
        ty = forall_ty.subst(id, var);

        if inst == ForallInst::Ptr {
            constrain_var(state, &ty, fresh_id)
        }
    }

    ty
}

/// Type of unary operations.
pub fn get_uop_type(
    state: &mut State,
    envs: Envs,
    strict: bool,
    op: &UnaryOp<RichTerm>,
) -> Result<TypeWrapper, TypecheckError> {
    Ok(match op {
        // forall a. bool -> a -> a -> a
        UnaryOp::Ite() => {
            let branches = TypeWrapper::Ptr(new_var(state.table));

            mk_tyw_arrow!(
                AbsType::Bool(),
                branches.clone(),
                branches.clone(),
                branches
            )
        }
        // forall a. a -> Bool
        UnaryOp::IsNum()
        | UnaryOp::IsBool()
        | UnaryOp::IsStr()
        | UnaryOp::IsFun()
        | UnaryOp::IsList()
        | UnaryOp::IsRecord() => {
            let inp = TypeWrapper::Ptr(new_var(state.table));

            mk_tyw_arrow!(inp, AbsType::Bool())
        }
        // Bool -> Bool -> Bool
        UnaryOp::BoolAnd() | UnaryOp::BoolOr() => {
            mk_tyw_arrow!(AbsType::Bool(), AbsType::Bool(), AbsType::Bool())
        }
        // Bool -> Bool
        UnaryOp::BoolNot() => mk_tyw_arrow!(AbsType::Bool(), AbsType::Bool()),
        // forall a. Dyn -> a
        UnaryOp::Blame() => {
            let res = TypeWrapper::Ptr(new_var(state.table));

            mk_tyw_arrow!(AbsType::Dyn(), res)
        }
        // Dyn -> Bool
        UnaryOp::Pol() => mk_tyw_arrow!(AbsType::Dyn(), AbsType::Bool()),
        // forall rows. < | rows> -> <id | rows>
        UnaryOp::Embed(id) => {
            let row = TypeWrapper::Ptr(new_var(state.table));
            // Constraining a freshly created variable should never fail.
            constraint(state, row.clone(), id.clone()).unwrap();
            mk_tyw_arrow!(mk_tyw_enum!(row.clone()), mk_tyw_enum!(id.clone(), row))
        }
        // 1. rows -> a
        // 2. forall b. b -> a
        // Rows is ( `label1, .., `labeln ) for label in l.keys().
        // Unify each branch in l.values() with a.
        // If the switch has a default case, the more general type 2. is used.
        UnaryOp::Switch(l, d) => {
            // Currently, if it has a default value, we typecheck the whole thing as
            // taking ANY enum, since it's more permissive and there's not a loss of information
            let res = TypeWrapper::Ptr(new_var(state.table));

            for exp in l.values() {
                type_check_(state, envs.clone(), strict, exp, res.clone())?;
            }

            let row = match d {
                Some(e) => {
                    type_check_(state, envs.clone(), strict, e, res.clone())?;
                    TypeWrapper::Ptr(new_var(state.table))
                }
                None => l.iter().try_fold(
                    mk_typewrapper::row_empty(),
                    |acc, x| -> Result<TypeWrapper, TypecheckError> {
                        Ok(mk_tyw_enum_row!(x.0.clone(), acc))
                    },
                )?,
            };

            mk_tyw_arrow!(mk_tyw_enum!(row), res)
        }
        // Dyn -> Dyn
        UnaryOp::ChangePolarity() | UnaryOp::GoDom() | UnaryOp::GoCodom() | UnaryOp::Tag(_) => {
            mk_tyw_arrow!(AbsType::Dyn(), AbsType::Dyn())
        }
        // Sym -> Dyn -> Dyn
        UnaryOp::Wrap() => mk_tyw_arrow!(AbsType::Sym(), AbsType::Dyn(), AbsType::Dyn()),
        // forall rows a. { id: a | rows} -> a
        UnaryOp::StaticAccess(id) => {
            let row = TypeWrapper::Ptr(new_var(state.table));
            let res = TypeWrapper::Ptr(new_var(state.table));

            mk_tyw_arrow!(mk_tyw_record!((id.clone(), res.clone()); row), res)
        }
        // List -> List
        // Unify f with a -> b.
        UnaryOp::ListMap(f) => {
            let a = TypeWrapper::Ptr(new_var(state.table));
            let b = TypeWrapper::Ptr(new_var(state.table));

            let f_type = mk_tyw_arrow!(a.clone(), b.clone());
            type_check_(state, envs.clone(), strict, f, f_type)?;

            mk_tyw_arrow!(AbsType::List(), AbsType::List())
        }
        // { _ : a} -> { _ : b }
        // Unify f with Str -> a -> b.
        UnaryOp::RecordMap(f) => {
            // Assuming f has type Str -> a -> b,
            // this has type DynRecord(a) -> DynRecord(b)

            let a = TypeWrapper::Ptr(new_var(state.table));
            let b = TypeWrapper::Ptr(new_var(state.table));

            let f_type = mk_tyw_arrow!(AbsType::Str(), a.clone(), b.clone());
            type_check_(state, envs.clone(), strict, f, f_type)?;

            mk_tyw_arrow!(mk_typewrapper::dyn_record(a), mk_typewrapper::dyn_record(b))
        }
        // forall a b. a -> b -> b
        UnaryOp::Seq() | UnaryOp::DeepSeq() => {
            let fst = TypeWrapper::Ptr(new_var(state.table));
            let snd = TypeWrapper::Ptr(new_var(state.table));

            mk_tyw_arrow!(fst, snd.clone(), snd)
        }
        // List -> Dyn
        UnaryOp::ListHead() => mk_tyw_arrow!(AbsType::List(), AbsType::Dyn()),
        // List -> List
        UnaryOp::ListTail() => mk_tyw_arrow!(AbsType::List(), AbsType::List()),
        // List -> Num
        UnaryOp::ListLength() => mk_tyw_arrow!(AbsType::List(), AbsType::Num()),
        // This should not happen, as ChunksConcat() is only produced during evaluation.
        UnaryOp::ChunksConcat(_, _, _) => panic!("cannot type ChunksConcat()"),
        // BEFORE: forall rows. { rows } -> List
        // Dyn -> List
        UnaryOp::FieldsOf() => mk_tyw_arrow!(
            AbsType::Dyn(),
            //mk_tyw_record!(; TypeWrapper::Ptr(new_var(state.table))),
            AbsType::List()
        ),
    })
}

/// Type of a binary operation.
pub fn get_bop_type(
    state: &mut State,
    envs: Envs,
    strict: bool,
    op: &BinaryOp<RichTerm>,
) -> Result<TypeWrapper, TypecheckError> {
    Ok(match op {
        // Num -> Num -> Num
        BinaryOp::Plus()
        | BinaryOp::Sub()
        | BinaryOp::Mult()
        | BinaryOp::Div()
        | BinaryOp::Modulo() => mk_tyw_arrow!(AbsType::Num(), AbsType::Num(), AbsType::Num()),
        // Str -> Str -> Str
        BinaryOp::PlusStr() => mk_tyw_arrow!(AbsType::Str(), AbsType::Str(), AbsType::Str()),
        // Sym -> Dyn -> Dyn -> Dyn
        BinaryOp::Unwrap() => mk_tyw_arrow!(
            AbsType::Sym(),
            AbsType::Dyn(),
            AbsType::Dyn(),
            AbsType::Dyn()
        ),
        // forall a b. a -> b -> Bool
        BinaryOp::Eq() => mk_tyw_arrow!(
            TypeWrapper::Ptr(new_var(state.table)),
            TypeWrapper::Ptr(new_var(state.table)),
            AbsType::Bool()
        ),
        // Num -> Num -> Bool
        BinaryOp::LessThan()
        | BinaryOp::LessOrEq()
        | BinaryOp::GreaterThan()
        | BinaryOp::GreaterOrEq() => mk_tyw_arrow!(AbsType::Num(), AbsType::Num(), AbsType::Bool()),
        // Str -> Dyn -> Dyn
        BinaryOp::GoField() => mk_tyw_arrow!(AbsType::Str(), AbsType::Dyn(), AbsType::Dyn()),
        // forall a. Str -> { _ : a} -> a
        BinaryOp::DynAccess() => {
            let res = TypeWrapper::Ptr(new_var(state.table));

            mk_tyw_arrow!(AbsType::Str(), mk_typewrapper::dyn_record(res.clone()), res)
        }
        // Str -> { _ : a } -> { _ : a }
        // Unify t with a.
        BinaryOp::DynExtend(t) => {
            let res = TypeWrapper::Ptr(new_var(state.table));

            type_check_(state, envs.clone(), strict, t, res.clone())?;

            mk_tyw_arrow!(
                AbsType::Str(),
                mk_typewrapper::dyn_record(res.clone()),
                mk_typewrapper::dyn_record(res)
            )
        }
        // forall a. Str -> { _ : a } -> { _ : a}
        BinaryOp::DynRemove() => {
            let res = TypeWrapper::Ptr(new_var(state.table));

            mk_tyw_arrow!(
                AbsType::Str(),
                mk_typewrapper::dyn_record(res.clone()),
                mk_typewrapper::dyn_record(res)
            )
        }
        // Str -> Dyn -> Bool
        BinaryOp::HasField() => mk_tyw_arrow!(AbsType::Str(), AbsType::Dyn(), AbsType::Bool()),
        // List -> List -> List
        BinaryOp::ListConcat() => mk_tyw_arrow!(AbsType::List(), AbsType::List(), AbsType::List()),
        // List -> Num -> Dyn
        BinaryOp::ListElemAt() => mk_tyw_arrow!(AbsType::List(), AbsType::Num(), AbsType::Dyn()),
        // Dyn -> Dyn -> Dyn
        BinaryOp::Merge() => mk_tyw_arrow!(AbsType::Dyn(), AbsType::Dyn(), AbsType::Dyn()),
    })
}

/// The unification table.
///
/// Map each unification variable to either another type variable or a concrete type it has been
/// unified with. Each binding `(ty, var)` in this map should be thought of an edge in a
/// unification graph.
pub type UnifTable = HashMap<usize, Option<TypeWrapper>>;

/// Row constraints.
///
/// A row constraint applies to a unification variable appearing inside a row type (such as `r` in
/// `{ someId: SomeType | r }`). It is a set of identifiers that said row must NOT contain, to
/// forbid ill-formed types with multiple declaration of the same id, for example `{ a: Num, a:
/// String}`.
pub type RowConstr = HashMap<usize, HashSet<Ident>>;

/// Create a fresh unification variable.
fn new_var(table: &mut UnifTable) -> usize {
    let next = table.len();
    table.insert(next, None);
    next
}

/// Add a row constraint on a type.
///
/// See [`RowConstr`](type.RowConstr.html).
fn constraint(state: &mut State, x: TypeWrapper, id: Ident) -> Result<(), RowUnifError> {
    match x {
        TypeWrapper::Ptr(p) => match get_root(state.table, p) {
            ty @ TypeWrapper::Concrete(_) => constraint(state, ty, id),
            TypeWrapper::Ptr(root) => {
                if let Some(v) = state.constr.get_mut(&root) {
                    v.insert(id);
                } else {
                    state.constr.insert(root, vec![id].into_iter().collect());
                }
                Ok(())
            }
            c @ TypeWrapper::Constant(_) => Err(RowUnifError::IllformedRow(c)),
        },
        TypeWrapper::Concrete(AbsType::RowEmpty()) => Ok(()),
        TypeWrapper::Concrete(AbsType::RowExtend(id2, tyw, t)) => {
            if id2 == id {
                Err(RowUnifError::UnsatConstr(id, tyw.map(|tyw| *tyw)))
            } else {
                constraint(state, *t, id)
            }
        }
        other => Err(RowUnifError::IllformedRow(other)),
    }
}

/// Add row constraints on a freshly instantiated type variable.
///
/// When instantiating a quantified type variable with a unification variable, row constraints may
/// apply. For example, if we instantiate `forall a. {x: Num | a} -> Num` by replacing `a` with a
/// unification variable `Ptr(p)`, this unification variable requires a constraint to avoid being
/// unified with a row type containing another declaration for the field `x`.
///
/// # Preconditions
///
/// Because `constraint_var` should be called on a fresh unification variable `p`, the following is
/// assumed:
/// - `get_root(state.table, p) == p`
/// - `state.constr.get(&p) == None`
fn constrain_var(state: &mut State, tyw: &TypeWrapper, p: usize) {
    fn constrain_var_(state: &mut State, mut constr: HashSet<Ident>, tyw: &TypeWrapper, p: usize) {
        match tyw {
            TypeWrapper::Ptr(u) if p == *u && !constr.is_empty() => {
                state.constr.insert(p, constr);
            }
            TypeWrapper::Ptr(u) => match get_root(state.table, *u) {
                TypeWrapper::Ptr(_) => (),
                tyw => constrain_var_(state, constr, &tyw, p),
            },
            TypeWrapper::Concrete(ty) => match ty {
                AbsType::Arrow(tyw1, tyw2) => {
                    constrain_var_(state, HashSet::new(), tyw1.as_ref(), p);
                    constrain_var_(state, HashSet::new(), tyw2.as_ref(), p);
                }
                AbsType::Forall(_, tyw) => constrain_var_(state, HashSet::new(), tyw.as_ref(), p),
                AbsType::Dyn()
                | AbsType::Num()
                | AbsType::Bool()
                | AbsType::Str()
                | AbsType::Sym()
                | AbsType::Flat(_)
                | AbsType::RowEmpty()
                | AbsType::Var(_)
                | AbsType::List() => (),
                AbsType::RowExtend(id, tyw, rest) => {
                    constr.insert(id.clone());
                    tyw.iter()
                        .for_each(|tyw| constrain_var_(state, HashSet::new(), tyw.as_ref(), p));
                    constrain_var_(state, constr, rest, p)
                }
                AbsType::Enum(row) => constrain_var_(state, constr, row, p),
                AbsType::StaticRecord(row) => constrain_var_(state, constr, row, p),
                AbsType::DynRecord(tyw) => constrain_var_(state, constr, tyw, p),
            },
            TypeWrapper::Constant(_) => (),
        }
    }

    constrain_var_(state, HashSet::new(), tyw, p);
}

/// Check that unifying a variable with a type doesn't violate row constraints, and update the row
/// constraints of the unified type accordingly if needed.
///
/// When a unification variable `Ptr(p)` is unified with a type `tyw` which is either a row type or
/// another unification variable which could be later unified with a row type itself, the following
/// operations are required:
///
/// 1. If `tyw` is a concrete row, check that it doesn't contain an identifier which is forbidden
///    by a row constraint on `p`.
/// 2. If the type is either a unification variable or a row type ending with a unification
///    variable `Ptr(u)`, we must add the constraints of `p` to the constraints of `u`. Indeed,
///    take the following situation: `p` appears in a row type `{a: Num | p}`, hence has a
///    constraint that it must not contain a field `a`. Then `p` is unified with a fresh type
///    variable `u`. If we don't constrain `u`, `u` could be unified later with a row type `{a :
///    Str}` which violates the original constraint on `p`. Thus, when unifying `p` with `u` or a
///    row ending with `u`, `u` must inherit all the constraints of `p`.
///
/// If `tyw` is neither a row nor a unification variable, `constr_unify` immediately returns `Ok(())`.
pub fn constr_unify(
    constr: &mut RowConstr,
    p: usize,
    mut tyw: &TypeWrapper,
) -> Result<(), RowUnifError> {
    if let Some(p_constr) = constr.remove(&p) {
        loop {
            match tyw {
                TypeWrapper::Concrete(AbsType::RowExtend(ident, ty, _))
                    if p_constr.contains(ident) =>
                {
                    break Err(RowUnifError::UnsatConstr(
                        ident.clone(),
                        ty.as_ref().map(|boxed| (**boxed).clone()),
                    ))
                }
                TypeWrapper::Concrete(AbsType::RowExtend(_, _, tail)) => tyw = tail,
                TypeWrapper::Ptr(u) if *u != p => {
                    if let Some(u_constr) = constr.get_mut(&u) {
                        u_constr.extend(p_constr.into_iter());
                    } else {
                        constr.insert(*u, p_constr);
                    }

                    break Ok(());
                }
                _ => break Ok(()),
            }
        }
    } else {
        Ok(())
    }
}

/// Follow the links in the unification table to find the representative of the equivalence class
/// of unification variable `x`.
///
/// This corresponds to the find in union-find.
// TODO This should be a union find like algorithm
pub fn get_root(table: &UnifTable, x: usize) -> TypeWrapper {
    // All queried variable must have been introduced by `new_var` and thus a corresponding entry
    // must always exist in `state`. If not, the typechecking algorithm is not correct, and we
    // panic.
    match table.get(&x).unwrap() {
        None => TypeWrapper::Ptr(x),
        Some(TypeWrapper::Ptr(y)) => get_root(table, *y),
        Some(ty @ TypeWrapper::Concrete(_)) => ty.clone(),
        Some(k @ TypeWrapper::Constant(_)) => k.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::ImportError;
    use crate::label::Label;
    use crate::mk_app;
    use crate::parser::lexer;
    use crate::program::resolvers::{DummyResolver, SimpleResolver};
    use crate::term::make as mk_term;
    use crate::transformations::transform;
    use codespan::Files;

    use crate::parser;

    fn type_check_no_import(rt: &RichTerm) -> Result<Types, TypecheckError> {
        type_check_in_env(rt, &Environment::new(), &mut DummyResolver {})
    }

    fn parse_and_typecheck(s: &str) -> Result<Types, TypecheckError> {
        let id = Files::new().add("<test>", s);

        match parser::grammar::TermParser::new().parse(id, lexer::Lexer::new(&s)) {
            Ok(p) => type_check_no_import(&p),
            Err(e) => panic!("Couldn't parse {}: {:?}", s, e),
        }
    }

    #[test]
    fn simple_no_promises() -> Result<(), TypecheckError> {
        // It's easy to check these will never fail, that's why we keep them all together

        type_check_no_import(&Term::Bool(true).into())?;
        type_check_no_import(&Term::Num(45.).into())?;
        type_check_no_import(&mk_term::id())?;
        type_check_no_import(&mk_term::let_in("x", Term::Num(3.0), mk_term::var("x")))?;

        type_check_no_import(&mk_app!(Term::Num(5.0), Term::Bool(true)))?;
        type_check_no_import(&mk_term::op2(
            BinaryOp::Plus(),
            Term::Num(4.),
            Term::Bool(false),
        ))?;

        Ok(())
    }

    #[test]
    fn unbound_variable_always_throws() {
        type_check_no_import(&mk_term::var("x")).unwrap_err();
    }

    #[test]
    fn promise_simple_checks() {
        type_check_no_import(
            &Term::Promise(
                Types(AbsType::Bool()),
                Label::dummy(),
                Term::Bool(true).into(),
            )
            .into(),
        )
        .unwrap();
        type_check_no_import(
            &Term::Promise(
                Types(AbsType::Num()),
                Label::dummy(),
                Term::Bool(true).into(),
            )
            .into(),
        )
        .unwrap_err();

        type_check_no_import(
            &Term::Promise(
                Types(AbsType::Num()),
                Label::dummy(),
                Term::Num(34.5).into(),
            )
            .into(),
        )
        .unwrap();
        type_check_no_import(
            &Term::Promise(
                Types(AbsType::Bool()),
                Label::dummy(),
                Term::Num(34.5).into(),
            )
            .into(),
        )
        .unwrap_err();

        type_check_no_import(
            &Term::Promise(
                Types(AbsType::Num()),
                Label::dummy(),
                Term::Assume(
                    Types(AbsType::Num()),
                    Label::dummy(),
                    Term::Bool(true).into(),
                )
                .into(),
            )
            .into(),
        )
        .unwrap();
        type_check_no_import(
            &Term::Promise(
                Types(AbsType::Num()),
                Label::dummy(),
                Term::Assume(
                    Types(AbsType::Bool()),
                    Label::dummy(),
                    Term::Num(34.).into(),
                )
                .into(),
            )
            .into(),
        )
        .unwrap_err();

        parse_and_typecheck("\"hello\" : Str").unwrap();
        parse_and_typecheck("\"hello\" : Num").unwrap_err();
    }

    #[test]
    fn promise_complicated() {
        // Inside Promises we typecheck strictly
        parse_and_typecheck("(fun x => if x then x + 1 else 34) false").unwrap();
        parse_and_typecheck("let f : Bool -> Num = fun x => if x then x + 1 else 34 in f false")
            .unwrap_err();

        // not annotated let bindings type to Dyn
        parse_and_typecheck(
            "let id : Num -> Num = fun x => x in
            (id 4 : Num)",
        )
        .unwrap();
        parse_and_typecheck(
            "let id = fun x => x in
            (id 4 : Num)",
        )
        .unwrap_err();

        // lambdas don't annotate to Dyn
        parse_and_typecheck("(fun id => (id 4 : Num)) (fun x => x)").unwrap();

        // But they are not polymorphic
        parse_and_typecheck("(fun id => (id 4 : Num) + (id true : Bool)) (fun x => x)")
            .unwrap_err();

        // Non strict zones don't unify
        parse_and_typecheck("(fun id => (id 4) + (id true: Bool)) (fun x => x)").unwrap();

        // We can typecheck any contract
        parse_and_typecheck(
            "let alwaysTrue = fun l t => if t then t else %blame% l in
            (fun x => x) : #alwaysTrue -> #alwaysTrue",
        )
        .unwrap();
        // Only if they're named the same way
        parse_and_typecheck("(fun x => x) : #(fun l t => t) -> #(fun l t => t)").unwrap_err();
    }

    #[test]
    fn simple_forall() {
        parse_and_typecheck(
            "let f : forall a. a -> a = fun x => x in
        (if (f true) then (f 2) else 3) : Num",
        )
        .unwrap();

        parse_and_typecheck(
            "let f : forall a. (forall b. a -> b -> a) = fun x y => x in
            (if (f true 3) then (f 2 false) else 3) : Num",
        )
        .unwrap();

        parse_and_typecheck(
            "let f : forall a. (forall b. b -> b) -> a -> a = fun f x => f x in
            f (fun z => z : forall y. y -> y)",
        )
        .unwrap();

        parse_and_typecheck(
            "let f : forall a. (forall b. a -> b -> a) = fun x y => y in
            f",
        )
        .unwrap_err();

        parse_and_typecheck(
            "((fun f => let g : forall b. b -> b = fun y => y in f g)
               : ((forall a. a -> a) -> Num) -> Num)
            (fun x => 3)",
        )
        .unwrap_err();

        parse_and_typecheck(
            "let g : Num -> Num = fun x => x in
            let f : forall a. a -> a = fun x => g x in
            f",
        )
        .unwrap_err();
    }

    #[test]
    fn forall_nested() {
        parse_and_typecheck(
            "let f : forall a. a -> a = let g = Assume(forall a. (a -> a), fun x => x) in g in
            (if (f true) then (f 2) else 3) : Num",
        )
        .unwrap();

        parse_and_typecheck(
            "let f : forall a. a -> a = let g = Assume(forall a. (a -> a), fun x => x) in g g in
            (if (f true) then (f 2) else 3) : Num",
        )
        .unwrap();

        parse_and_typecheck(
            "let f : forall a. a -> a = let g : forall a. (forall b. (b -> (a -> a))) = fun y x => x in g 0 in
            (if (f true) then (f 2) else 3) : Num",
        )
        .unwrap();
    }

    #[test]
    fn enum_simple() {
        parse_and_typecheck("`bla : <bla>").unwrap();
        parse_and_typecheck("`blo : <bla>").unwrap_err();

        parse_and_typecheck("`blo : <bla, blo>").unwrap();
        parse_and_typecheck("`bla : forall r. <bla | r>").unwrap();
        parse_and_typecheck("`bla : forall r. <bla, blo | r>").unwrap();

        parse_and_typecheck("(switch { bla => 3, } `bla) : Num").unwrap();
        parse_and_typecheck("(switch { bla => 3, } `blo) : Num").unwrap_err();

        parse_and_typecheck("(switch { bla => 3, _ => 2, } `blo) : Num").unwrap();
        parse_and_typecheck("(switch { bla => 3, ble => true, } `bla) : Num").unwrap_err();
    }

    #[test]
    fn enum_complex() {
        parse_and_typecheck("(fun x => switch {bla => 1, ble => 2,} x) : <bla, ble> -> Num")
            .unwrap();
        parse_and_typecheck(
            "(fun x => switch {bla => 1, ble => 2, bli => 4,} x) : <bla, ble> -> Num",
        )
        .unwrap_err();
        parse_and_typecheck(
            "(fun x => switch {bla => 1, ble => 2, bli => 4,} (%embed% bli x)) : <bla, ble> -> Num",
        )
        .unwrap();

        parse_and_typecheck(
            "(fun x =>
                (switch {bla => 3, bli => 2,} x) +
                (switch {bli => 6, bla => 20,} x) ) `bla : Num",
        )
        .unwrap();
        // TODO typecheck this, I'm not sure how to do it with row variables
        parse_and_typecheck(
            "(fun x =>
                (switch {bla => 3, bli => 2,} x) +
                (switch {bla => 6, blo => 20,} x) ) `bla : Num",
        )
        .unwrap_err();

        parse_and_typecheck(
            "let f : forall r. <blo, ble | r> -> Num =
            fun x => (switch {blo => 1, ble => 2, _ => 3, } x) in
            f `bli : Num",
        )
        .unwrap();
        parse_and_typecheck(
            "let f : forall r. <blo, ble, | r> -> Num =
                fun x => (switch {blo => 1, ble => 2, bli => 3, } x) in
            f",
        )
        .unwrap_err();

        parse_and_typecheck(
            "let f : forall r. (forall p. <blo, ble | r> -> <bla, bli | p>) =
                fun x => (switch {blo => `bla, ble => `bli, _ => `bla, } x) in
            f `bli",
        )
        .unwrap();
        parse_and_typecheck(
            "let f : forall r. (forall p. <blo, ble | r> -> <bla, bli | p>) =
                fun x => (switch {blo => `bla, ble => `bli, _ => `blo, } x) in
            f `bli",
        )
        .unwrap_err();
    }

    #[test]
    fn static_record_simple() {
        parse_and_typecheck("{bla = 1} : {bla : Num}").unwrap();
        parse_and_typecheck("{bla = true} : {bla : Num}").unwrap_err();
        parse_and_typecheck("{blo = 1} : {bla : Num}").unwrap_err();

        parse_and_typecheck("{blo = true; bla = 1} : {bla : Num, blo : Bool}").unwrap();

        parse_and_typecheck("{blo = 1}.blo : Num").unwrap();
        parse_and_typecheck("{bla = true; blo = 1}.blo : Num").unwrap();
        parse_and_typecheck("{blo = 1}.blo : Bool").unwrap_err();

        parse_and_typecheck(
            "let r : {bla : Bool, blo : Num} = {blo = 1; bla = true; } in
            (if r.bla then r.blo else 2) : Num",
        )
        .unwrap();

        // It worked at first try :O
        parse_and_typecheck(
            "let f : forall a. (forall r. {bla : Bool, blo : a, ble : a | r} -> a) =
                fun r => if r.bla then r.blo else r.ble
            in
            (if (f {bla = true; blo = false; ble = true; blip = 1; }) then
                (f {bla = true; blo = 1; ble = 2; blip = `blip; })
            else
                (f {bla = true; blo = 3; ble = 4; bloppo = `bloppop; })) : Num",
        )
        .unwrap();

        parse_and_typecheck(
            "let f : forall a. (forall r. {bla : Bool, blo : a, ble : a | r} -> a) =
                fun r => if r.bla then r.blo else r.ble
            in
            (f {bla = true; blo = 1; ble = true; blip = `blip}) : Num",
        )
        .unwrap_err();
        parse_and_typecheck(
            "let f : forall a. (forall r. {bla : Bool, blo : a, ble : a | r} -> a) =
                fun r => if r.bla then (r.blo + 1) else r.ble
            in
            (f {bla = true; blo = 1; ble = 2; blip = `blip; }) : Num",
        )
        .unwrap_err();
    }

    #[test]
    fn dynamic_record_simple() {
        parse_and_typecheck("{ $(if true then \"foo\" else \"bar\") = 2; } : {_ : Num}").unwrap();

        parse_and_typecheck("({ $(if true then \"foo\" else \"bar\") = 2; }.$(\"bla\")) : Num")
            .unwrap();

        parse_and_typecheck(
            "({ $(if true then \"foo\" else \"bar\") = 2; $(\"foo\") = true; }.$(\"bla\")) : Num",
        )
        .unwrap_err();

        parse_and_typecheck("{ foo = 3; bar = 4; } : {_ : Num}").unwrap();
    }

    #[test]
    fn seq() {
        parse_and_typecheck("%seq% false 1 : Num").unwrap();
        parse_and_typecheck("(fun x y => %seq% x y) : forall a. (forall b. a -> b -> b)").unwrap();
        parse_and_typecheck("let xDyn = false in let yDyn = 1 in (%seq% xDyn yDyn : Dyn)").unwrap();
    }

    #[test]
    fn simple_list() {
        parse_and_typecheck("[1, \"2\", false]").unwrap();
        parse_and_typecheck("[\"a\", 3, true] : List").unwrap();
        parse_and_typecheck("[(fun x => x : forall a. a -> a), true] : List").unwrap();
        parse_and_typecheck("fun x => [x] : forall a. a -> List").unwrap();

        parse_and_typecheck("[1, (\"2\" : Num), false]").unwrap_err();
        parse_and_typecheck("[(1 : String), true, \"b\"] : List").unwrap_err();
        parse_and_typecheck("[1, 2, \"3\"] : Num").unwrap_err();
    }

    #[test]
    fn lists_operations() {
        parse_and_typecheck("fun l => %tail% l : List -> List").unwrap();
        parse_and_typecheck("fun l => %head% l : List -> Dyn").unwrap();
        parse_and_typecheck(
            "fun f l => %map% f l : forall a. (forall b. (a -> b) -> List -> List)",
        )
        .unwrap();
        parse_and_typecheck("(fun l1 => fun l2 => l1 @ l2) : List -> List -> List").unwrap();
        parse_and_typecheck("(fun i l => %elemAt% l i) : Num -> List -> Dyn ").unwrap();

        parse_and_typecheck("(fun l => %head% l) : forall a. (List -> a)").unwrap_err();
        parse_and_typecheck(
            "(fun f l => %elemAt% (%map% f l) 0) : forall a. (forall b. (a -> b) -> List -> b)",
        )
        .unwrap_err();
    }

    #[test]
    fn imports() {
        let mut resolver = SimpleResolver::new();
        resolver.add_source(String::from("good"), String::from("1 + 1 : Num"));
        resolver.add_source(String::from("bad"), String::from("false : Num"));
        resolver.add_source(
            String::from("proxy"),
            String::from("let x = import \"bad\" in x"),
        );

        fn mk_import<R>(import: &str, resolver: &mut R) -> Result<RichTerm, ImportError>
        where
            R: ImportResolver,
        {
            transform(
                mk_term::let_in("x", Term::Import(String::from(import)), mk_term::var("x")),
                resolver,
            )
        };

        type_check_in_env(
            &mk_import("good", &mut resolver).unwrap(),
            &Environment::new(),
            &mut resolver,
        )
        .unwrap();
        type_check_in_env(
            &mk_import("proxy", &mut resolver).unwrap(),
            &Environment::new(),
            &mut resolver,
        )
        .unwrap_err();
    }

    #[test]
    fn recursive_records() {
        parse_and_typecheck("{a : Num = 1; b = a + 1} : {a : Num, b : Num}").unwrap();
        parse_and_typecheck("{a : Num = true; b = a + 1} : {a : Num, b : Num}").unwrap_err();
        parse_and_typecheck("{a = 1; b : Bool = a} : {a : Num, b : Bool}").unwrap_err();
        parse_and_typecheck("{a : Num = 1 + a} : {a : Num}").unwrap();
    }

    #[test]
    fn let_inference() {
        parse_and_typecheck("(let x = 1 + 2 in let f = fun x => x + 1 in f x) : Num").unwrap();
        parse_and_typecheck("(let x = 1 + 2 in let f = fun x => x ++ \"a\" in f x) : Num")
            .unwrap_err();

        // Fields in recursive records are treated in the type environment in the same way as let-bound expressions
        parse_and_typecheck("{a = 1; b = 1 + a} : {a : Num, b : Num}").unwrap();
        parse_and_typecheck(
            "{ f = fun x => if x == 0 then 1 else 1 + (f (x + (-1)));} : {f : Num -> Num}",
        )
        .unwrap();
        parse_and_typecheck(
            "{ f = fun x => if x == 0 then false else 1 + (f (x + (-1)))} : {f : Num -> Num}",
        )
        .unwrap_err();
    }

    /// Regression test following [#144](https://github.com/tweag/nickel/issues/144). Check that
    /// polymorphic type variables appearing inside a row type are correctly constrained at
    /// instantiation.
    #[test]
    fn polymorphic_row_constraints() {
        // Assert that the result of evaluation is either directly a `RowConflict` error, or a
        // `RowConflict` wrapped in an `ArrowTypeMismatch`.
        fn assert_row_conflict(res: Result<Types, TypecheckError>) {
            assert!(match res.unwrap_err() {
                TypecheckError::RowConflict(_, _, _, _, _) => true,
                TypecheckError::ArrowTypeMismatch(_, _, _, err_boxed, _) => {
                    if let TypecheckError::RowConflict(_, _, _, _, _) = err_boxed.as_ref() {
                        true
                    } else {
                        false
                    }
                }
                _ => true,
            })
        }

        let mut res = parse_and_typecheck(
            "let extend = Assume(forall c. { | c} -> {a: Str | c}, 0) in
           (let bad = extend {a = 1;} in 0) : Num",
        );
        assert_row_conflict(res);

        parse_and_typecheck(
            "let extend = Assume(forall c. { | c} -> {a: Str | c}, 0) in
           let remove = Assume(forall c. {a: Str | c} -> { | c}, 0) in
           (let good = remove (extend {}) in 0) : Num",
        )
        .unwrap();
        res = parse_and_typecheck(
            "let remove = Assume(forall c. {a: Str | c} -> { | c}, 0) in
           (let bad = remove (remove {a = \"a\"}) in 0) : Num",
        );
        assert_row_conflict(res);
    }

    #[test]
    fn dynamic_row_tail() {
        parse_and_typecheck("let r = Assume({a: Num | Dyn}, {a = 1; b = 2}) in (r.a : Num)")
            .unwrap();
        parse_and_typecheck("Assume({a: Num | Dyn}, {a = 1; b = 2}) : {a: Num | Dyn}").unwrap();
        // Currently, typechecking is conservative wrt the dynamic row type, meaning it can't to a
        // less precise type with a dynamic tail.
        parse_and_typecheck("{a = 1; b = 2} : {a: Num | Dyn}").unwrap_err();
        parse_and_typecheck("{a = 1} : {a: Num | Dyn}").unwrap_err();
        parse_and_typecheck("Assume({a: Num | Dyn}, {a = 1}) : {a: Num}").unwrap_err();
        parse_and_typecheck("{a = 1} : {a: Num | Dyn}").unwrap_err();
    }
}
