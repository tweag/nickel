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
//! ```text
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
//! ```text
//! // Accepted
//! let id : forall a. a -> a = fun x => x in seq (id "a") (id 5) : Num
//! ```
//!
//! In non-strict mode, all let-bound expressions are given type `Dyn`, unless annotated.
use crate::cache::ImportResolver;
use crate::environment::Environment as GenericEnvironment;
use crate::error::TypecheckError;
use crate::eval;
use crate::identifier::Ident;
use crate::label::ty_path;
use crate::position::TermPos;
use crate::term::{BinaryOp, Contract, MetaValue, NAryOp, RichTerm, StrChunk, Term, UnaryOp};
use crate::types::{AbsType, Types};
use crate::{mk_tyw_arrow, mk_tyw_enum, mk_tyw_enum_row, mk_tyw_record, mk_tyw_row};
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;

use self::linearization::{Building, Completed, Linearization, Linearizer, ScopeId, StubHost};

pub mod linearization;

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
    pub fn into_unif_err(self, left: TypeWrapper, right: TypeWrapper) -> UnifError {
        match self {
            RowUnifError::MissingRow(id) => UnifError::MissingRow(id, left, right),
            RowUnifError::MissingDynTail() => UnifError::MissingDynTail(left, right),
            RowUnifError::ExtraRow(id) => UnifError::ExtraRow(id, left, right),
            RowUnifError::ExtraDynTail() => UnifError::ExtraDynTail(left, right),
            RowUnifError::RowKindMismatch(id, tyw1, tyw2) => {
                UnifError::RowKindMismatch(id, tyw1, tyw2)
            }
            RowUnifError::RowMismatch(id, err) => UnifError::RowMismatch(id, left, right, err),
            RowUnifError::IllformedRow(tyw) => UnifError::IllformedRow(tyw),
            RowUnifError::UnsatConstr(id, tyw) => UnifError::RowConflict(id, tyw, left, right),
            RowUnifError::WithConst(c, tyw) => UnifError::WithConst(c, tyw),
            RowUnifError::ConstMismatch(ty1, ty2) => UnifError::ConstMismatch(ty1, ty2),
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
    pub fn into_typecheck_err(self, state: &State, pos_opt: TermPos) -> TypecheckError {
        self.into_typecheck_err_(state, &mut reporting::NameReg::new(), pos_opt)
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
            UnifError::RowMismatch(ident, ty1, ty2, err) => TypecheckError::RowMismatch(
                ident,
                reporting::to_type(state.table, state.names, names, ty1),
                reporting::to_type(state.table, state.names, names, ty2),
                Box::new((*err).into_typecheck_err_(state, names, TermPos::None)),
                pos_opt,
            ),
            UnifError::RowKindMismatch(id, ty1, ty2) => TypecheckError::RowKindMismatch(
                id,
                ty1.map(|ty| reporting::to_type(state.table, state.names, names, ty)),
                ty2.map(|ty| reporting::to_type(state.table, state.names, names, ty)),
                pos_opt,
            ),
            // TODO: for now, failure to unify with a type constant causes the same error as a
            // usual type mismatch. It could be nice to have a specific error message in the
            // future.
            UnifError::ConstMismatch(ty1, ty2) => TypecheckError::TypeMismatch(
                reporting::to_type(state.table, state.names, names, TypeWrapper::Constant(ty1)),
                reporting::to_type(state.table, state.names, names, TypeWrapper::Constant(ty2)),
                pos_opt,
            ),
            UnifError::WithConst(c, ty) => TypecheckError::TypeMismatch(
                reporting::to_type(state.table, state.names, names, TypeWrapper::Constant(c)),
                reporting::to_type(state.table, state.names, names, ty),
                pos_opt,
            ),
            UnifError::IllformedFlatType(rt) => {
                TypecheckError::IllformedType(Types(AbsType::Flat(rt)))
            }
            UnifError::IllformedType(tyw) => TypecheckError::IllformedType(reporting::to_type(
                state.table,
                state.names,
                names,
                tyw,
            )),
            UnifError::MissingRow(id, tyw1, tyw2) => TypecheckError::MissingRow(
                id,
                reporting::to_type(state.table, state.names, names, tyw1),
                reporting::to_type(state.table, state.names, names, tyw2),
                pos_opt,
            ),
            UnifError::MissingDynTail(tyw1, tyw2) => TypecheckError::MissingDynTail(
                reporting::to_type(state.table, state.names, names, tyw1),
                reporting::to_type(state.table, state.names, names, tyw2),
                pos_opt,
            ),
            UnifError::ExtraRow(id, tyw1, tyw2) => TypecheckError::ExtraRow(
                id,
                reporting::to_type(state.table, state.names, names, tyw1),
                reporting::to_type(state.table, state.names, names, tyw2),
                pos_opt,
            ),
            UnifError::ExtraDynTail(tyw1, tyw2) => TypecheckError::ExtraDynTail(
                reporting::to_type(state.table, state.names, names, tyw1),
                reporting::to_type(state.table, state.names, names, tyw2),
                pos_opt,
            ),
            UnifError::IllformedRow(tyw) => TypecheckError::IllformedType(reporting::to_type(
                state.table,
                state.names,
                names,
                tyw,
            )),
            UnifError::RowConflict(id, tyw, left, right) => TypecheckError::RowConflict(
                id,
                tyw.map(|tyw| reporting::to_type(state.table, state.names, names, tyw)),
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
    pub fn into_type_path(self) -> Option<(TypeWrapper, TypeWrapper, ty_path::Path, Self)> {
        let mut curr: Self = self;
        let mut path = ty_path::Path::new();
        // The original expected and actual type. They are just updated once, in the first
        // iteration of the loop below.
        let mut tyws: Option<(TypeWrapper, TypeWrapper)> = None;

        loop {
            match curr {
                UnifError::DomainMismatch(
                    ty1 @ TypeWrapper::Concrete(AbsType::Arrow(_, _)),
                    ty2 @ TypeWrapper::Concrete(AbsType::Arrow(_, _)),
                    err,
                ) => {
                    tyws = tyws.or(Some((ty1, ty2)));
                    path.push(ty_path::Elem::Domain);
                    curr = *err;
                }
                UnifError::DomainMismatch(_, _, _) => panic!(
                    "typechecking::to_type_path(): domain mismatch error on a non arrow type"
                ),
                UnifError::CodomainMismatch(
                    ty1 @ TypeWrapper::Concrete(AbsType::Arrow(_, _)),
                    ty2 @ TypeWrapper::Concrete(AbsType::Arrow(_, _)),
                    err,
                ) => {
                    tyws = tyws.or(Some((ty1, ty2)));
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

/// Populate a new global typing environment from a global term environment.
pub fn mk_global(envs: Vec<RichTerm>) -> Result<GlobalEnvironment, eval::EnvBuildError> {
    Ok(envs
        .iter()
        .map(|rt| {
            if let Term::RecRecord(rec, ..) = rt.as_ref() {
                Ok(rec
                    .iter()
                    .map(|(id, rt)| (id.clone(), infer_type(rt.as_ref()))))
            } else {
                Err(eval::EnvBuildError::NotARecord(rt.clone()))
            }
        })
        .collect::<Result<Vec<_>, eval::EnvBuildError>>()?
        .into_iter()
        .flatten()
        .collect())
}

/// The typing environment.
pub type GlobalEnvironment = GenericEnvironment<Ident, TypeWrapper>;
pub type TypeEnvironment = GenericEnvironment<Ident, Closure>;

impl From<&GlobalEnvironment> for TypeEnvironment {
    fn from(g: &GlobalEnvironment) -> Self {
        let mut env = TypeEnvironment::new();
        for (id, tw) in g.iter() {
            env.insert(id.clone(), tw.into());
        }
        env
    }
}
/// A structure holding the two typing environments, the global and the local.
///
/// The global typing environment is constructed from the global term environment (see
/// [`eval`](../eval/fn.eval.html)) which holds the Nickel builtin functions. It is a read-only
/// shared environment used to retrieve the type of such functions.
#[derive(Debug, PartialEq, Clone)]
pub struct Envs {
    local: TypeEnvironment,
    types: TypeEnvironment,
}

impl Envs {
    pub fn new() -> Envs {
        Envs {
            local: TypeEnvironment::new(),
            types: TypeEnvironment::new(),
        }
    }

    pub fn from_global(global: &GlobalEnvironment) -> Envs {
        Envs {
            local: TypeEnvironment::from(global),
            types: TypeEnvironment::new(),
        }
    }

    /// Add the bindings of a record to a typing environment. Ignore fields whose name are defined
    /// through interpolation.
    //TODO: support the case of a record with a type annotation.
    pub fn env_add_term(
        global: &mut GlobalEnvironment,
        rt: &RichTerm,
    ) -> Result<(), eval::EnvBuildError> {
        let RichTerm { term, pos } = rt;

        match term.as_ref() {
            Term::Record(bindings, _) | Term::RecRecord(bindings, ..) => {
                for (id, t) in bindings {
                    let clos_env = Envs::from_global(global);
                    let tyw: TypeWrapper =
                        apparent_type(t.as_ref(), Some((&clos_env, global))).into();
                    global.insert(id.clone(), tyw);
                }
                Ok(())
            }
            t => Err(eval::EnvBuildError::NotARecord(RichTerm::new(
                t.clone(),
                *pos,
            ))),
        }
    }

    /// Bind one term in a typing environment.
    pub fn env_add(env: &mut GlobalEnvironment, id: Ident, rt: &RichTerm) {
        env.insert(
            id,
            apparent_type(rt.as_ref(), Some((&Envs::from_global(env), env))).into(),
        );
    }

    /// Fetch a binding from the environment. Try first in the local environment, and then in the
    /// global.
    pub fn get(&self, ident: &Ident, global: &GlobalEnvironment) -> Option<Closure> {
        self.local
            .get(ident)
            .or_else(|| global.get(ident).map(|ty| Closure::new(ty, Envs::new())))
    }

    /// Wrapper to insert a new binding in the local environment.
    pub fn insert(&mut self, ident: Ident, tyc: Closure) {
        self.local.insert(ident, tyc);
    }

    pub fn register_typedef(&mut self, ident: Ident, tyw: TypeWrapper) {
        self.types.insert(ident, Closure::new(tyw, self.clone()));
    }

    pub fn get_typedef(&self, ident: &Ident) -> Option<Closure> {
        self.types.get(ident)
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

#[derive(Debug, PartialEq, Clone)]
pub struct Closure {
    ty: TypeWrapper,
    env: Envs,
}

impl From<&TypeWrapper> for Closure {
    fn from(ty: &TypeWrapper) -> Self {
        Closure {
            ty: ty.clone(),
            env: Envs::new(),
        }
    }
}

impl Closure {
    pub fn new(ty: TypeWrapper, env: Envs) -> Closure {
        Closure { ty, env }
    }

    pub fn atomic(ty: TypeWrapper) -> Closure {
        Closure {
            ty,
            env: Envs::new(),
        }
    }

    pub fn from_type(ty: Types, env: Envs) -> Closure {
        Closure {
            ty: TypeWrapper::from(ty),
            env,
        }
    }

    pub fn from_abs(ty: AbsType<Box<TypeWrapper>>, env: Envs) -> Closure {
        Closure {
            ty: TypeWrapper::Concrete(ty),
            env,
        }
    }

    pub fn resolve(self, state: &State) -> Result<Closure, UnifError> {
        let res = if let TypeWrapper::Ptr(pt) = &self.ty {
            get_root(state.table, *pt)
        } else {
            self
        };

        res.resolve_typealias()
    }

    pub fn resolve_typealias(self) -> Result<Closure, UnifError> {
        if let TypeWrapper::Concrete(AbsType::Var(ident)) = self.ty {
            if let Some(clos) = self.env.get_typedef(&ident) {
                Ok(clos)
            } else {
                Err(UnifError::UnboundTypeVariable(ident))
            }
        } else {
            Ok(self)
        }
    }

    pub fn register_typedef(&self, id: Ident, def: TypeWrapper) -> Closure {
        let mut env = self.env.clone();
        env.register_typedef(id, def);
        Closure::new(self.ty.clone(), env)
    }

    pub fn flatten_type(self) -> TypeWrapper {
        let clos = self.clone().resolve_typealias().unwrap_or(self);
        match clos.ty {
            ptr @ TypeWrapper::Ptr(_) => ptr,
            cst @ TypeWrapper::Constant(_) => cst,
            ty @ TypeWrapper::Concrete(..) => ty,
        }
    }
}

/// Typecheck a term.
///
/// Return the inferred type in case of success. This is just a wrapper that calls
/// [`type_check_`](fn.type_check_.html) with a fresh unification variable as goal.
///
/// Note that this function doesn't recursively typecheck imports (anymore), but just the current
/// file. It however still needs the resolver to get the apparent type of imports.
pub fn type_check<L>(
    t: &RichTerm,
    global: &GlobalEnvironment, //eval::Environment,
    resolver: &impl ImportResolver,
    mut linearizer: impl Linearizer<L, (UnifTable, HashMap<usize, Ident>)>,
) -> Result<(Types, Completed), TypecheckError>
where
    L: Default,
{
    let (mut table, mut names) = (UnifTable::new(), HashMap::new());
    let mut building = Linearization::building();
    let ty = TypeWrapper::Ptr(new_var(&mut table));

    {
        let mut state: State = State {
            resolver,
            table: &mut table,
            constr: &mut RowConstr::new(),
            names: &mut names,
        };

        type_check_(
            &mut state,
            &global,
            &mut building,
            linearizer.scope(linearization::ScopeId::Right),
            false,
            t,
            Closure {
                ty: ty.clone(),
                env: Envs::new(),
            },
        )?;
    }

    let lin = linearizer
        .linearize(building, (table.clone(), names))
        .into();
    Ok((to_type(&table, ty), lin))
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
    global: &GlobalEnvironment,
    resolver: &dyn ImportResolver,
) -> Result<Types, TypecheckError> {
    let mut state = State {
        resolver,
        table: &mut UnifTable::new(),
        constr: &mut RowConstr::new(),
        names: &mut HashMap::new(),
    };
    let ty = TypeWrapper::Ptr(new_var(state.table));
    type_check_(
        &mut state,
        global,
        &mut Linearization::building::<()>(),
        StubHost::<(), ()>::new(),
        false,
        t,
        Closure::atomic(ty.clone()),
    )?;

    Ok(to_type(&state.table, ty))
}

/// Typecheck a term against a specific type.
///
/// # Arguments
///
/// - `state`: the unification state (see [`State`](struct.State.html)).
/// - `env`: the typing environment, mapping free variable to types.
/// - `lin`: The current building linearization of building state `S`
/// - `linearizer`: A linearizer that can modify the linearization
/// - `strict`: the typechecking mode.
/// - `t`: the term to check.
/// - `ty`: the type to check the term against.
///
/// Registers every term with the `linearizer` and makes sure to scope the
/// liearizer accordingly
fn type_check_<S, E>(
    state: &mut State,
    global: &GlobalEnvironment,
    lin: &mut Linearization<Building<S>>,
    mut linearizer: impl Linearizer<S, E>,
    strict: bool,
    rt: &RichTerm,
    tcc: Closure,
) -> Result<(), TypecheckError> {
    let RichTerm { term: t, pos } = rt;
    let Closure { ty, mut env } = tcc;

    linearizer.add_term(lin, t, *pos, ty.clone());

    match t.as_ref() {
        Term::ParseError => return Ok(()),
        // null is inferred to be of type Dyn
        Term::Null => unify(
            state,
            strict,
            Closure::new(TypeWrapper::from(ty), env.clone()),
            Closure::atomic(mk_typewrapper::dynamic()),
        )
        .map_err(|err| err.into_typecheck_err(state, rt.pos)),

        Term::Bool(_) => unify(
            state,
            strict,
            Closure::new(TypeWrapper::from(ty), env.clone()),
            Closure::atomic(mk_typewrapper::bool()),
        )
        .map_err(|err| err.into_typecheck_err(state, rt.pos)),

        Term::Num(_) => unify(
            state,
            strict,
            Closure::new(TypeWrapper::from(ty), env.clone()),
            Closure::atomic(mk_typewrapper::num()),
        )
        .map_err(|err| err.into_typecheck_err(state, rt.pos)),

        Term::Str(_) => unify(
            state,
            strict,
            Closure::new(TypeWrapper::from(ty), env.clone()),
            Closure::atomic(mk_typewrapper::str()),
        )
        .map_err(|err| err.into_typecheck_err(state, rt.pos)),

        Term::StrChunks(chunks) => {
            unify(
                state,
                strict,
                Closure::new(TypeWrapper::from(ty), env.clone()),
                Closure::atomic(mk_typewrapper::str()),
            )
            .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            chunks.iter().enumerate().try_for_each(
                |(choice, chunk)| -> Result<(), TypecheckError> {
                    match chunk {
                        StrChunk::Literal(_) => Ok(()),
                        StrChunk::Expr(t, _) => type_check_(
                            state,
                            global,
                            lin,
                            linearizer.scope(ScopeId::Choice(choice)),
                            strict,
                            t,
                            Closure::new(mk_typewrapper::str(), env.clone()),
                        ),
                    }
                },
            )
        }
        Term::Fun(x, t) => {
            let src = TypeWrapper::Ptr(new_var(state.table));
            // TODO what to do here, this makes more sense to me, but it means let x = foo in bar
            // behaves quite different to (\x.bar) foo, worth considering if it's ok to type these two differently
            // let src = TypeWrapper::The(AbsType::Dyn());
            let trg = TypeWrapper::Ptr(new_var(state.table));
            let arr = mk_tyw_arrow!(src.clone(), trg.clone());

            unify(
                state,
                strict,
                Closure::new(ty, env.clone()),
                Closure::atomic(arr),
            )
            .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            env.insert(x.clone(), Closure::atomic(src));
            type_check_(
                state,
                global,
                lin,
                linearizer,
                strict,
                t,
                Closure::new(trg, env.clone()),
            )
        }
        Term::List(terms) => {
            let ty_elts = TypeWrapper::Ptr(new_var(state.table));

            unify(
                state,
                strict,
                Closure::new(TypeWrapper::from(ty), env.clone()),
                Closure::atomic(mk_typewrapper::list(ty_elts.clone())),
            )
            .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            terms
                .iter()
                .enumerate()
                .try_for_each(|(choice, t)| -> Result<(), TypecheckError> {
                    type_check_(
                        state,
                        &global,
                        lin,
                        linearizer.scope(ScopeId::Choice(choice)),
                        strict,
                        t,
                        Closure::new(ty_elts.clone(), env.clone()),
                    )
                })
        }
        Term::Lbl(_) => {
            // TODO implement lbl type
            unify(
                state,
                strict,
                Closure::new(ty, env.clone()),
                Closure::atomic(mk_typewrapper::dynamic()),
            )
            .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Let(x, re, rt) => {
            let ty_let = binding_type(re.as_ref(), &mut env, global, state.table, strict);
            type_check_(
                state,
                global,
                lin,
                linearizer.scope(ScopeId::Left),
                strict,
                re,
                Closure::new(ty_let.clone(), env.clone()),
            )?;
            env.insert(x.clone(), Closure::new(ty_let, env.clone()));
            type_check_(
                state,
                global,
                lin,
                linearizer,
                strict,
                rt,
                Closure::new(ty, env),
            )
        }
        // TODO move this up once lets are rec
        Term::LetPattern(x, pat, re, rt) => {
            use crate::destruct::*;
            fn inject_pat_vars(pat: &Destruct, envs: &mut Envs) {
                match pat {
                    Destruct::Record(matches, _, rst) => {
                        if let Some(id) = rst {
                            envs.insert(id.clone(), Closure::atomic(mk_typewrapper::dynamic()));
                        }
                        matches.iter().for_each(|m| match m {
                            Match::Simple(id, ..) => {
                                envs.insert(id.clone(), Closure::atomic(mk_typewrapper::dynamic()));
                            }
                            Match::Assign(_, _, (bind_id, pat)) => {
                                if let Some(id) = bind_id {
                                    envs.insert(
                                        id.clone(),
                                        Closure::atomic(mk_typewrapper::dynamic()),
                                    );
                                }
                                if !pat.is_empty() {
                                    inject_pat_vars(pat, envs);
                                }
                            }
                        });
                    }
                    _ => (),
                }
            }
            let ty_let = binding_type(re.as_ref(), &mut env, global, state.table, strict);
            type_check_(
                state,
                global,
                lin,
                linearizer.scope(ScopeId::Left),
                strict,
                re,
                Closure::new(ty_let.clone(), env.clone()),
            )?;

            // TODO typecheck the interior of the patern
            if let Some(x) = x {
                env.insert(x.clone(), Closure::new(ty_let, env.clone()));
                inject_pat_vars(pat, &mut env);
            }
            //type_check_(state, envs, strict, rt, Closure::new(ty, env))
            Ok(())
        }

        Term::App(e, t) => {
            let src = TypeWrapper::Ptr(new_var(state.table));
            let arr = mk_tyw_arrow!(src.clone(), ty);

            type_check_(
                state,
                global,
                lin,
                linearizer.scope(ScopeId::Left),
                strict,
                e,
                Closure::new(arr, env.clone()),
            )?;
            type_check_(
                state,
                global,
                lin,
                linearizer,
                strict,
                t,
                Closure::new(src, env.clone()),
            )
        }
        Term::Switch(exp, cases, default) => {
            // Currently, if it has a default value, we typecheck the whole thing as
            // taking ANY enum, since it's more permissive and there's no loss of information
            let res = TypeWrapper::Ptr(new_var(state.table));

            for (choice, case) in cases.values().enumerate() {
                type_check_(
                    state,
                    &global,
                    lin,
                    linearizer.scope(ScopeId::Choice(choice)),
                    strict,
                    case,
                    Closure::new(res.clone(), env.clone()),
                )?;
            }

            let row = match default {
                Some(t) => {
                    type_check_(
                        state,
                        global,
                        lin,
                        linearizer.scope(ScopeId::Right),
                        strict,
                        t,
                        Closure::new(res.clone(), env.clone()),
                    )?;
                    TypeWrapper::Ptr(new_var(state.table))
                }
                None => cases.iter().try_fold(
                    mk_typewrapper::row_empty(),
                    |acc, x| -> Result<TypeWrapper, TypecheckError> {
                        Ok(mk_tyw_enum_row!(x.0.clone(), acc))
                    },
                )?,
            };

            unify(
                state,
                strict,
                Closure::new(ty, env.clone()),
                Closure::new(res, env.clone()),
            )
            .map_err(|err| err.into_typecheck_err(state, rt.pos))?;
            type_check_(
                state,
                global,
                lin,
                linearizer,
                strict,
                exp,
                Closure::new(mk_tyw_enum!(row), env),
            )
        }
        Term::Var(x) => {
            let x_ty = env
                .get(&x, global)
                .ok_or_else(|| TypecheckError::UnboundIdentifier(x.clone(), *pos))?;

            let instantiated = instantiate_foralls(state, x_ty, ForallInst::Ptr);
            unify(state, strict, Closure::new(ty, env.clone()), instantiated)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Enum(id) => {
            let row = TypeWrapper::Ptr(new_var(state.table));
            unify(
                state,
                strict,
                Closure::new(ty, env.clone()),
                Closure::new(mk_tyw_enum!(id.clone(), row), env),
            )
            .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        // If some fields are defined dynamically, the only potential type that works is `{_ : a}`
        // for some `a`
        Term::RecRecord(stat_map, dynamic, _) if !dynamic.is_empty() => {
            let ty_dyn = TypeWrapper::Ptr(new_var(state.table));

            for (id, _) in stat_map {
                env.insert(id.clone(), Closure::new(ty_dyn.clone(), env.clone()));
            }

            stat_map.iter().enumerate().try_for_each(
                |(choice, (_, t))| -> Result<(), TypecheckError> {
                    type_check_(
                        state,
                        global,
                        lin,
                        linearizer.scope(ScopeId::Choice(choice)),
                        strict,
                        t,
                        Closure::new(ty_dyn.clone(), env.clone()),
                    )
                },
            )?;

            unify(
                state,
                strict,
                Closure::new(ty, env.clone()),
                Closure::atomic(mk_typewrapper::dyn_record(ty_dyn)),
            )
            .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Record(stat_map, _) | Term::RecRecord(stat_map, ..) => {
            // For recursive records, we look at the apparent type of each field and bind it in
            // env before actually typechecking the content of fields.
            // Fields defined by interpolation are ignored.
            if let Term::RecRecord(..) = t.as_ref() {
                for (id, rt) in stat_map {
                    let tyw = binding_type(rt.as_ref(), &mut env, global, state.table, strict);
                    env.insert(id.clone(), Closure::new(tyw, env.clone()));
                }
            }

            let root_ty = if let TypeWrapper::Ptr(p) = ty {
                get_root(state.table, p)
            } else {
                Closure::new(ty.clone(), env.clone())
            };

            if let Closure {
                ty: TypeWrapper::Concrete(AbsType::DynRecord(rec_ty)),
                ..
            } = root_ty
            {
                // Checking for a dynamic record
                stat_map.iter().enumerate().try_for_each(
                    |(choice, (_, t))| -> Result<(), TypecheckError> {
                        type_check_(
                            state,
                            global,
                            lin,
                            linearizer.scope(ScopeId::Choice(choice)),
                            strict,
                            t,
                            Closure::new((*rec_ty).clone(), env.clone()),
                        )
                    },
                )
            } else {
                let row = stat_map.iter().enumerate().try_fold(
                    mk_tyw_row!(),
                    |acc, (choice, (id, field))| -> Result<TypeWrapper, TypecheckError> {
                        // In the case of a recursive record, new types (either type variables or
                        // annotations) have already be determined and put in the typing
                        // environment, and we need to use the same.
                        let ty = if let Term::RecRecord(..) = t.as_ref() {
                            env.get(&id, global).unwrap().ty
                        } else {
                            TypeWrapper::Ptr(new_var(state.table))
                        };

                        type_check_(
                            state,
                            global,
                            lin,
                            linearizer.scope(ScopeId::Choice(choice)),
                            strict,
                            field,
                            Closure::new(ty.clone(), env.clone()),
                        )?;

                        Ok(mk_tyw_row!((id.clone(), ty); acc))
                    },
                )?;

                unify(
                    state,
                    strict,
                    Closure::new(ty, env.clone()),
                    Closure::new(mk_tyw_record!(; row), env),
                )
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
            }
        }
        Term::Op1(op, t) => {
            let (ty_arg, ty_res) = get_uop_type(state, op)?;

            unify(
                state,
                strict,
                Closure::new(ty, env.clone()),
                Closure::new(ty_res, env.clone()),
            )
            .map_err(|err| err.into_typecheck_err(state, rt.pos))?;
            type_check_(
                state,
                global,
                lin,
                linearizer.scope(ScopeId::Right),
                strict,
                t,
                Closure::new(ty_arg, env),
            )
        }
        Term::Op2(op, t1, t2) => {
            let (ty_arg1, ty_arg2, ty_res) = get_bop_type(state, op)?;

            unify(
                state,
                strict,
                Closure::new(ty, env.clone()),
                Closure::atomic(ty_res),
            )
            .map_err(|err| err.into_typecheck_err(state, rt.pos))?;
            type_check_(
                state,
                global,
                lin,
                linearizer.scope(ScopeId::Left),
                strict,
                t1,
                Closure::new(ty_arg1, env.clone()),
            )?;
            type_check_(
                state,
                global,
                lin,
                linearizer,
                strict,
                t2,
                Closure::new(ty_arg2, env.clone()),
            )
        }
        Term::OpN(op, args) => {
            let (tys_op, ty_ret) = get_nop_type(state, op)?;

            unify(
                state,
                strict,
                Closure::new(ty, env.clone()),
                Closure::new(ty_ret, env.clone()),
            )
            .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            tys_op
                .into_iter()
                .enumerate()
                .zip(args.iter())
                .try_for_each(|((choice, ty_t), t)| {
                    type_check_(
                        state,
                        global,
                        lin,
                        linearizer.scope(ScopeId::Choice(choice)),
                        strict,
                        t,
                        Closure::new(ty_t, env.clone()),
                    )?;
                    Ok(())
                })?;

            Ok(())
        }
        Term::MetaValue(MetaValue {
            types: Some(Contract { types: ty2, .. }),
            value: Some(t),
            ..
        }) => {
            let clos = if let Types(AbsType::Var(ident)) = ty2 {
                if let Some(replace_type) = env.get_typedef(ident) {
                    Closure::new(replace_type.ty.clone(), env.clone())
                } else {
                    Closure::new(TypeWrapper::from(ty2.clone()), env.clone())
                }
            } else {
                Closure::new(TypeWrapper::from(ty2.clone()), env.clone())
            };

            let instantiated = instantiate_foralls(state, clos.clone(), ForallInst::Constant);

            unify(state, strict, clos, Closure::new(ty, env.clone()))
                .map_err(|err| err.into_typecheck_err(state, rt.pos))?;
            type_check_(state, global, lin, linearizer, true, t, instantiated)
        }
        // A metavalue with at least one contract is an assume. If there's several
        // contracts, we arbitrarily chose the first one as the type annotation.
        Term::MetaValue(MetaValue {
            contracts, value, ..
        }) if !contracts.is_empty() => {
            let ctr = contracts.get(0).unwrap();
            let Contract { types: ty2, .. } = ctr;

            unify(
                state,
                strict,
                Closure::new(ty, env.clone()),
                Closure::new(ty2.clone().into(), env.clone()),
            )
            .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            // if there's an inner value, we have to recursively typecheck it, but in non strict
            // mode.
            if let Some(t) = value {
                type_check_(
                    state,
                    global,
                    lin,
                    linearizer,
                    false,
                    t,
                    Closure::new(mk_typewrapper::dynamic(), env.clone()),
                )
            } else {
                Ok(())
            }
        }
        Term::Sym(_) => unify(
            state,
            strict,
            Closure::new(ty, env.clone()),
            Closure::atomic(mk_typewrapper::sym()),
        )
        .map_err(|err| err.into_typecheck_err(state, rt.pos)),

        Term::Wrapped(_, t) => type_check_(
            state,
            global,
            lin,
            linearizer,
            strict,
            t,
            Closure::new(ty, env),
        ),
        // A non-empty metavalue without a type or contract annotation is typechecked in the same way as its inner value
        Term::MetaValue(MetaValue { value: Some(t), .. }) => type_check_(
            state,
            global,
            lin,
            linearizer,
            strict,
            t,
            Closure::new(ty, env),
        ),
        // A metavalue without a body nor a type annotation is a record field without definition.
        // This should probably be non representable in the syntax, as it doesn't really make
        // sense. In any case, we infer it to be of type `Dyn` for now.
        Term::MetaValue(_) => unify(
            state,
            strict,
            Closure::new(ty, env.clone()),
            Closure::atomic(mk_typewrapper::dynamic()),
        )
        .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Import(_) => unify(
            state,
            strict,
            Closure::new(ty, env.clone()),
            Closure::atomic(mk_typewrapper::dynamic()),
        )
        .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        // We use the apparent type of the import for checking. This function doesn't recursively
        // typecheck imports: this is the responsibility of the caller.
        Term::ResolvedImport(file_id) => {
            let t = state
                .resolver
                .get(*file_id)
                .expect("Internal error: resolved import not found ({:?}) during typechecking.");
            let ty_import: TypeWrapper = apparent_type(t.as_ref(), Some((&env, global))).into();
            unify(
                state,
                strict,
                Closure::new(ty, env.clone()),
                Closure::new(ty_import, env),
            )
            .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
    }
}

/// Determine the type of a let-bound expression, or more generally of any binding (e.g. fields)
/// that may be stored in a typing environment at some point.
///
/// Call [`apparent_type`](./fn.apparent_type.html) to see if the binding is annotated. If
/// it is, return this type as a [`TypeWrapper`](./enum.TypeWrapper.html). Otherwise:
///     * in non strict mode, we won't (and possibly can't) infer the type of `bound_exp`: just
///       return `Dyn`.
///     * in strict mode, we will typecheck `bound_exp`: return a new unification variable to be
///       associated to `bound_exp`.
fn binding_type(
    t: &Term,
    envs: &mut Envs,
    global: &GlobalEnvironment,
    table: &mut UnifTable,
    strict: bool,
) -> TypeWrapper {
    let ty_apt = apparent_type(t, Some((envs, global)));

    match ty_apt {
        ApparentType::Approximated(_) if strict => TypeWrapper::Ptr(new_var(table)),
        ty_apt => ty_apt.into(),
    }
}

/// Different kinds of apparent types (see [`apparent_type`](fn.apparent_type.html)).
///
/// Indicate the nature of an apparent type. In particular, when in strict mode, the typechecker
/// throws away approximations as it can do better and infer the actual type of an expression by
/// generating a fresh unification variable.  In non-strict mode, however, the approximation is the
/// best we can do. This type allows the caller of `apparent_type` to determine which situation it
/// is.
#[derive(Debug)]
pub enum ApparentType {
    /// The apparent type is given by a user-provided annotation, such as an `Assume`, a `Promise`,
    /// or a metavalue.
    Annotated(Types),
    /// The apparent type has been inferred from a simple expression.
    Inferred(Types),
    /// The term is a variable and its type was retrieved from the typing environment.
    FromEnv(TypeWrapper),
    /// The apparent type wasn't trivial to determine, and an approximation (most of the time,
    /// `Dyn`) has been returned.
    Approximated(Types),
}

impl From<ApparentType> for Types {
    fn from(at: ApparentType) -> Self {
        match at {
            ApparentType::Annotated(ty)
            | ApparentType::Inferred(ty)
            | ApparentType::Approximated(ty) => ty,
            ApparentType::FromEnv(tyw) => tyw.try_into().ok().unwrap_or(Types(AbsType::Dyn())),
        }
    }
}

impl From<ApparentType> for TypeWrapper {
    fn from(at: ApparentType) -> TypeWrapper {
        match at {
            ApparentType::Annotated(ty)
            | ApparentType::Inferred(ty)
            | ApparentType::Approximated(ty) => ty.into(),
            ApparentType::FromEnv(tyw) => tyw,
        }
    }
}

/// Determine the apparent type of a let-bound expression.
///
/// When a let-binding `let x = bound_exp in body` is processed, the type of `bound_exp` must be
/// determined in order to be bound to the variable `x` in the typing environment.
/// Then, future occurrences of `x` can be given this type when used in a `Promise` block.
///
/// The role of `apparent_type` is precisely to determine the type of `bound_exp`:
/// - if `bound_exp` is annotated by an `Assume`, a `Promise` or a metavalue, return the
///   user-provided type.
/// - if `bound_exp` is a constant (string, number, boolean or symbol) which type can be deduced
///   directly without unfolding the expression further, return the corresponding exact type.
/// - if `bound_exp` is a list, return `List Dyn`.
/// - Otherwise, return an approximation of the type (currently `Dyn`, but could be more precise in
///   the future, such as `Dyn -> Dyn` for functions, `{ | Dyn}` for records, and so on).
pub fn apparent_type(t: &Term, envs: Option<(&Envs, &GlobalEnvironment)>) -> ApparentType {
    match t {
        Term::MetaValue(MetaValue {
            types: Some(Contract { types: ty, .. }),
            ..
        }) => ApparentType::Annotated(ty.clone()),
        // For metavalues, if there's no type annotation, choose the first contract appearing.
        Term::MetaValue(MetaValue { contracts, .. }) if !contracts.is_empty() => {
            ApparentType::Annotated(contracts.get(0).unwrap().types.clone())
        }
        Term::MetaValue(MetaValue { value: Some(v), .. }) => apparent_type(v.as_ref(), envs),
        Term::Num(_) => ApparentType::Inferred(Types(AbsType::Num())),
        Term::Bool(_) => ApparentType::Inferred(Types(AbsType::Bool())),
        Term::Sym(_) => ApparentType::Inferred(Types(AbsType::Sym())),
        Term::Str(_) | Term::StrChunks(_) => ApparentType::Inferred(Types(AbsType::Str())),
        Term::List(_) => {
            ApparentType::Approximated(Types(AbsType::List(Box::new(Types(AbsType::Dyn())))))
        }
        Term::Var(id) => envs
            .and_then(|(envs, glob)| envs.get(id, glob).map(|clos| clos.ty))
            .map(ApparentType::FromEnv)
            .unwrap_or(ApparentType::Approximated(Types(AbsType::Dyn()))),
        _ => ApparentType::Approximated(Types(AbsType::Dyn())),
    }
}

/// Infer type of a more complex structure.
/// For now, it's implemented only to infer the type of a non annotated record by gathering the apparent type of the fields.
/// It's used essentially to type the stdlib.
pub fn infer_type(t: &Term) -> TypeWrapper {
    match t {
        Term::Record(rec, ..) | Term::RecRecord(rec, ..) => AbsType::StaticRecord(Box::new(
            TypeWrapper::Concrete(rec.iter().fold(AbsType::RowEmpty(), |r, (id, rt)| {
                AbsType::RowExtend(
                    id.clone(),
                    Some(Box::new(infer_type(rt.term.as_ref()))),
                    Box::new(r.into()),
                )
            })),
        ))
        .into(),
        Term::MetaValue(MetaValue {
            value: Some(rt),
            types: None,
            ..
        }) => infer_type(rt.as_ref()),
        t => apparent_type(t, None).into(),
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

impl std::convert::TryInto<Types> for TypeWrapper {
    type Error = ();

    fn try_into(self) -> Result<Types, ()> {
        match self {
            TypeWrapper::Concrete(ty) => {
                let converted: AbsType<Box<Types>> = ty.try_map(|tyw_boxed| {
                    let ty: Types = (*tyw_boxed).try_into()?;
                    Ok(Box::new(ty))
                })?;
                Ok(Types(converted))
            }
            _ => Err(()),
        }
    }
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
            Concrete(AbsType::List(ty)) => Concrete(AbsType::List(Box::new(ty.subst(id, to)))),
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

impl From<Types> for TypeWrapper {
    fn from(ty: Types) -> Self {
        TypeWrapper::Concrete(ty.0.map(|ty_| Box::new(TypeWrapper::from(*ty_))))
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
        (($id:expr, $ty:expr) $(,($ids:expr, $tys:expr))* $(; $tail:expr)?) => {
            $crate::typecheck::TypeWrapper::Concrete(
                $crate::types::AbsType::RowExtend(
                    Ident::from($id),
                    Some(Box::new($ty.into())),
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

    pub fn list<T>(ty: T) -> TypeWrapper
    where
        T: Into<TypeWrapper>,
    {
        TypeWrapper::Concrete(AbsType::List(Box::new(ty.into())))
    }

    // dyn is a reserved keyword
    generate_builder!(dynamic, Dyn);
    generate_builder!(str, Str);
    generate_builder!(num, Num);
    generate_builder!(bool, Bool);
    generate_builder!(sym, Sym);
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
    mut c: Closure,
) -> Result<(Option<Box<TypeWrapper>>, TypeWrapper), RowUnifError> {
    if let TypeWrapper::Ptr(p) = c.ty {
        c = get_root(state.table, p);
    }
    let Closure { ty: r, env } = c;
    match r {
        TypeWrapper::Concrete(AbsType::RowEmpty()) | TypeWrapper::Concrete(AbsType::Dyn()) => {
            Err(RowUnifError::MissingRow(id.clone()))
        }
        TypeWrapper::Concrete(AbsType::RowExtend(id2, ty2, r2)) => {
            if *id == id2 {
                Ok((ty2, *r2))
            } else {
                let (extracted_type, subrow) = row_add(state, id, ty, Closure::new(*r2, env))?;
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
            constraint(
                state,
                Closure::new(new_row.clone(), env.clone()),
                id.clone(),
            )?;
            state.table.insert(
                root,
                Some(Closure::new(
                    TypeWrapper::Concrete(AbsType::RowExtend(
                        id.clone(),
                        ty.clone(),
                        Box::new(new_row.clone()),
                    )),
                    env,
                )),
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
pub fn unify(state: &mut State, strict: bool, t1c: Closure, t2c: Closure) -> Result<(), UnifError> {
    if strict {
        unify_(state, t1c, t2c)
    } else {
        Ok(())
    }
}

/// Try to unify two types.
pub fn unify_(state: &mut State, t1c: Closure, t2c: Closure) -> Result<(), UnifError> {
    let Closure { ty: t1, env: env1 } = t1c.resolve(state)?;
    let Closure { ty: t2, env: env2 } = t2c.resolve(state)?;

    // t1 and t2 are roots of the type
    match (t1, t2) {
        (TypeWrapper::Concrete(s1), TypeWrapper::Concrete(s2)) => match (s1, s2) {
            (AbsType::Dyn(), AbsType::Dyn()) => Ok(()),
            (AbsType::Num(), AbsType::Num()) => Ok(()),
            (AbsType::Bool(), AbsType::Bool()) => Ok(()),
            (AbsType::Str(), AbsType::Str()) => Ok(()),
            (AbsType::Var(Ident { .. }), _) | (_, AbsType::Var(Ident { .. })) => {
                unreachable!()
            }
            (AbsType::List(tyw1), AbsType::List(tyw2)) => {
                unify_(state, Closure::new(*tyw1, env1), Closure::new(*tyw2, env2))
            }
            (AbsType::Sym(), AbsType::Sym()) => Ok(()),
            (AbsType::Arrow(s1s, s1t), AbsType::Arrow(s2s, s2t)) => {
                unify_(
                    state,
                    Closure::new((*s1s).clone(), env1.clone()),
                    Closure::new((*s2s).clone(), env2.clone()),
                )
                .map_err(|err| {
                    UnifError::DomainMismatch(
                        TypeWrapper::Concrete(AbsType::Arrow(s1s.clone(), s1t.clone())),
                        TypeWrapper::Concrete(AbsType::Arrow(s2s.clone(), s2t.clone())),
                        Box::new(err),
                    )
                })?;
                unify_(
                    state,
                    Closure::new((*s1t).clone(), env1.clone()),
                    Closure::new((*s2t).clone(), env2.clone()),
                )
                .map_err(|err| {
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
                unify_rows(state, r1.clone(), env1.clone(), r2.clone(), env2.clone()).map_err(
                    |err| err.into_unif_err(TypeWrapper::Concrete(r1), TypeWrapper::Concrete(r2)),
                )
            }
            (AbsType::Enum(tyw1), AbsType::Enum(tyw2)) => match (*tyw1, *tyw2) {
                (TypeWrapper::Concrete(r1), TypeWrapper::Concrete(r2))
                    if r1.is_row_type() && r2.is_row_type() =>
                {
                    unify_rows(state, r1.clone(), env1.clone(), r2.clone(), env2.clone())
                        .map_err(|err| err.into_unif_err(mk_tyw_enum!(r1), mk_tyw_enum!(r2)))
                }
                (TypeWrapper::Concrete(r), _) if !r.is_row_type() => {
                    Err(UnifError::IllformedType(mk_tyw_enum!(r)))
                }
                (_, TypeWrapper::Concrete(r)) if !r.is_row_type() => {
                    Err(UnifError::IllformedType(mk_tyw_enum!(r)))
                }
                (tyw1, tyw2) => unify_(state, Closure::new(tyw1, env1), Closure::new(tyw2, env2)),
            },
            (AbsType::StaticRecord(tyw1), AbsType::StaticRecord(tyw2)) => match (*tyw1, *tyw2) {
                (TypeWrapper::Concrete(r1), TypeWrapper::Concrete(r2))
                    if r1.is_row_type() && r2.is_row_type() =>
                {
                    unify_rows(state, r1.clone(), env1.clone(), r2.clone(), env2.clone()).map_err(
                        |err| err.into_unif_err(mk_tyw_record!(; r1), mk_tyw_record!(; r2)),
                    )
                }
                (TypeWrapper::Concrete(r), _) if !r.is_row_type() => {
                    Err(UnifError::IllformedType(mk_tyw_record!(; r)))
                }
                (_, TypeWrapper::Concrete(r)) if !r.is_row_type() => {
                    Err(UnifError::IllformedType(mk_tyw_record!(; r)))
                }
                (tyw1, tyw2) => unify_(state, Closure::new(tyw1, env1), Closure::new(tyw2, env2)),
            },
            (AbsType::DynRecord(t), AbsType::DynRecord(t2)) => {
                unify_(state, Closure::new(*t, env1), Closure::new(*t2, env2))
            }
            (AbsType::Forall(i1, t1t), AbsType::Forall(i2, t2t)) => {
                // Very stupid (slow) implementation
                let constant_type = TypeWrapper::Constant(new_var(state.table));

                unify_(
                    state,
                    Closure::new(t1t.subst(i1, constant_type.clone()), env1),
                    Closure::new(t2t.subst(i2, constant_type), env2),
                )
            }
            (ty1, ty2) => Err(UnifError::TypeMismatch(
                TypeWrapper::Concrete(ty1),
                TypeWrapper::Concrete(ty2),
            )),
        },
        (TypeWrapper::Ptr(p1), TypeWrapper::Ptr(p2)) if p1 == p2 => Ok(()),
        // The two following cases are not merged just to correctly distinguish between the
        // expected type (first component of the tuple) and the inferred type when reporting a row
        // unification error.
        (TypeWrapper::Ptr(p), tyw) => {
            constr_unify(state.constr, p, &tyw)
                .map_err(|err| err.into_unif_err(TypeWrapper::Ptr(p), tyw.clone()))?;
            state.table.insert(p, Some(Closure::new(tyw, env2)));
            Ok(())
        }
        (tyw, TypeWrapper::Ptr(p)) => {
            constr_unify(state.constr, p, &tyw)
                .map_err(|err| err.into_unif_err(tyw.clone(), TypeWrapper::Ptr(p)))?;
            state.table.insert(p, Some(Closure::new(tyw, env1)));
            Ok(())
        }
        (TypeWrapper::Constant(i1), TypeWrapper::Constant(i2)) if i1 == i2 => Ok(()),
        (TypeWrapper::Constant(i1), TypeWrapper::Constant(i2)) => {
            Err(UnifError::ConstMismatch(i1, i2))
        }
        (ty, TypeWrapper::Constant(i)) => Err(UnifError::WithConst(i, ty)),
        (TypeWrapper::Constant(i), ty) => Err(UnifError::WithConst(i, ty)),
    }
}

/// Try to unify two row types. Return an [`IllformedRow`](./enum.RowUnifError.html#variant.IllformedRow) error if one of the given type
/// is not a row type.
pub fn unify_rows(
    state: &mut State,
    t1: AbsType<Box<TypeWrapper>>,
    env1: Envs,
    t2: AbsType<Box<TypeWrapper>>,
    env2: Envs,
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
            let (ty2, t2_tail) = row_add(
                state,
                &id,
                ty.clone(),
                Closure::new(TypeWrapper::Concrete(r2), env1.clone()),
            )?;
            match (ty, ty2) {
                (None, None) => Ok(()),
                (Some(ty), Some(ty2)) => unify_(
                    state,
                    Closure::new(*ty, env1.clone()),
                    Closure::new(*ty2, env2.clone()),
                )
                .map_err(|err| RowUnifError::RowMismatch(id.clone(), Box::new(err))),
                (ty1, ty2) => Err(RowUnifError::RowKindMismatch(
                    id,
                    ty1.map(|t| *t),
                    ty2.map(|t| *t),
                )),
            }?;

            match (*t, t2_tail) {
                (TypeWrapper::Concrete(r1_tail), TypeWrapper::Concrete(r2_tail)) => {
                    unify_rows(state, r1_tail, env1.clone(), r2_tail, env2.clone())
                }
                // If one of the tail is not a concrete type, it is either a unification variable
                // or a constant (rigid type variable). `unify` already knows how to treat these
                // cases, so we delegate the work. However it returns `UnifError` instead of
                // `RowUnifError`, hence we have a bit of wrapping and unwrapping to do. Note that
                // since we are unifying types with a constant or a unification variable somewhere,
                // the only unification errors that should be possible are related to constants or
                // row constraints.
                (t1_tail, t2_tail) => unify_(
                    state,
                    Closure::new(t1_tail, env1),
                    Closure::new(t2_tail, env2),
                )
                .map_err(|err| match err {
                    UnifError::ConstMismatch(ty1, ty2) => RowUnifError::ConstMismatch(ty1, ty2),
                    UnifError::WithConst(ty1, tyw) => RowUnifError::WithConst(ty1, tyw),
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

/// Extract the concrete type corresponding to a type wrapper. Free unification variables as well
/// as type constants are replaced with the type `Dyn`.
fn to_type(table: &UnifTable, ty: TypeWrapper) -> Types {
    match ty {
        TypeWrapper::Ptr(p) => {
            let root = get_root(table, p);
            match root.ty {
                TypeWrapper::Concrete(_) => to_type(table, root.ty),
                _ => Types(AbsType::Dyn()),
            }
        }
        TypeWrapper::Constant(_) => Types(AbsType::Dyn()),
        TypeWrapper::Concrete(t) => {
            let mapped = t.map(|btyp| Box::new(to_type(table, *btyp)));
            Types(mapped)
        }
    }
}

/// Helpers to convert a `TypeWrapper` to a human-readable `Types` representation for error
/// reporting purpose.
pub mod reporting {
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

        let ident = Ident::from(name);
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
    pub fn to_type(
        table: &UnifTable,
        reported_names: &HashMap<usize, Ident>,
        names: &mut NameReg,
        ty: TypeWrapper,
    ) -> Types {
        match ty {
            TypeWrapper::Ptr(p) => {
                let root = get_root(table, p);
                if let TypeWrapper::Ptr(ref p) = root.ty {
                    var_to_type(reported_names, names, *p)
                } else {
                    to_type(table, reported_names, names, root.ty)
                }
            }
            TypeWrapper::Constant(c) => cst_to_type(reported_names, names, c),
            TypeWrapper::Concrete(t) => {
                let mapped = t.map(|btyp| Box::new(to_type(table, reported_names, names, *btyp)));
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
fn instantiate_foralls(state: &mut State, mut clos: Closure, inst: ForallInst) -> Closure {
    if let TypeWrapper::Ptr(p) = &clos.ty {
        clos = get_root(state.table, *p);
    }
    while let TypeWrapper::Concrete(AbsType::Forall(id, forall_ty)) = clos.ty {
        let fresh_id = new_var(state.table);
        let var = match inst {
            ForallInst::Constant => TypeWrapper::Constant(fresh_id),
            ForallInst::Ptr => TypeWrapper::Ptr(fresh_id),
        };
        state.names.insert(fresh_id, id.clone());
        clos.ty = forall_ty.subst(id, var);

        if inst == ForallInst::Ptr {
            constrain_var(state, &clos.ty, fresh_id)
        }
    }

    clos
}

/// Type of unary operations.
pub fn get_uop_type(
    state: &mut State,
    op: &UnaryOp,
) -> Result<(TypeWrapper, TypeWrapper), TypecheckError> {
    Ok(match op {
        // forall a. bool -> a -> a -> a
        UnaryOp::Ite() => {
            let branches = TypeWrapper::Ptr(new_var(state.table));

            (
                mk_typewrapper::bool(),
                mk_tyw_arrow!(branches.clone(), branches.clone(), branches),
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
            (inp, mk_typewrapper::bool())
        }
        // Bool -> Bool -> Bool
        UnaryOp::BoolAnd() | UnaryOp::BoolOr() => (
            mk_typewrapper::bool(),
            mk_tyw_arrow!(AbsType::Bool(), AbsType::Bool()),
        ),
        // Bool -> Bool
        UnaryOp::BoolNot() => (mk_typewrapper::bool(), mk_typewrapper::bool()),
        // forall a. Dyn -> a
        UnaryOp::Blame() => {
            let res = TypeWrapper::Ptr(new_var(state.table));

            (mk_typewrapper::dynamic(), res)
        }
        // Dyn -> Bool
        UnaryOp::Pol() => (mk_typewrapper::dynamic(), mk_typewrapper::bool()),
        // forall rows. < | rows> -> <id | rows>
        UnaryOp::Embed(id) => {
            let row = TypeWrapper::Ptr(new_var(state.table));
            // Constraining a freshly created variable should never fail.
            constraint(state, Closure::new(row.clone(), Envs::new()), id.clone()).unwrap();
            (mk_tyw_enum!(row.clone()), mk_tyw_enum!(id.clone(), row))
        }
        // This should not happen, as Switch() is only produced during evaluation.
        UnaryOp::Switch(_) => panic!("cannot typecheck Switch()"),
        // Dyn -> Dyn
        UnaryOp::ChangePolarity() | UnaryOp::GoDom() | UnaryOp::GoCodom() | UnaryOp::GoList() => {
            (mk_typewrapper::dynamic(), mk_typewrapper::dynamic())
        }
        // Sym -> Dyn -> Dyn
        UnaryOp::Wrap() => (
            mk_typewrapper::sym(),
            mk_tyw_arrow!(AbsType::Dyn(), AbsType::Dyn()),
        ),
        // forall rows a. { id: a | rows} -> a
        UnaryOp::StaticAccess(id) => {
            let row = TypeWrapper::Ptr(new_var(state.table));
            let res = TypeWrapper::Ptr(new_var(state.table));

            (mk_tyw_record!((id.clone(), res.clone()); row), res)
        }
        // forall a b. List a -> (a -> b) -> List b
        UnaryOp::ListMap() => {
            let a = TypeWrapper::Ptr(new_var(state.table));
            let b = TypeWrapper::Ptr(new_var(state.table));

            let f_type = mk_tyw_arrow!(a.clone(), b.clone());
            (
                mk_typewrapper::list(a),
                mk_tyw_arrow!(f_type, mk_typewrapper::list(b)),
            )
        }
        // forall a. Num -> (Num -> a) -> List a
        UnaryOp::ListGen() => {
            let a = TypeWrapper::Ptr(new_var(state.table));

            let f_type = mk_tyw_arrow!(AbsType::Num(), a.clone());
            (
                mk_typewrapper::num(),
                mk_tyw_arrow!(f_type, mk_typewrapper::list(a)),
            )
        }
        // forall a b. { _ : a} -> (Str -> a -> b) -> { _ : b }
        UnaryOp::RecordMap() => {
            // Assuming f has type Str -> a -> b,
            // this has type DynRecord(a) -> DynRecord(b)

            let a = TypeWrapper::Ptr(new_var(state.table));
            let b = TypeWrapper::Ptr(new_var(state.table));

            let f_type = mk_tyw_arrow!(AbsType::Str(), a.clone(), b.clone());
            (
                mk_typewrapper::dyn_record(a),
                mk_tyw_arrow!(f_type, mk_typewrapper::dyn_record(b)),
            )
        }
        // forall a b. a -> b -> b
        UnaryOp::Seq() | UnaryOp::DeepSeq() => {
            let fst = TypeWrapper::Ptr(new_var(state.table));
            let snd = TypeWrapper::Ptr(new_var(state.table));

            (fst, mk_tyw_arrow!(snd.clone(), snd))
        }
        // forall a. List a -> a
        UnaryOp::ListHead() => {
            let ty_elt = TypeWrapper::Ptr(new_var(state.table));
            (mk_typewrapper::list(ty_elt.clone()), ty_elt)
        }
        // forall a. List a -> List a
        UnaryOp::ListTail() => {
            let ty_elt = TypeWrapper::Ptr(new_var(state.table));
            (
                mk_typewrapper::list(ty_elt.clone()),
                mk_typewrapper::list(ty_elt),
            )
        }
        // forall a. List a -> Num
        UnaryOp::ListLength() => {
            let ty_elt = TypeWrapper::Ptr(new_var(state.table));
            (mk_typewrapper::list(ty_elt), mk_typewrapper::num())
        }
        // This should not happen, as ChunksConcat() is only produced during evaluation.
        UnaryOp::ChunksConcat() => panic!("cannot type ChunksConcat()"),
        // BEFORE: forall rows. { rows } -> List
        // Dyn -> List Str
        UnaryOp::FieldsOf() => (
            mk_typewrapper::dynamic(),
            //mk_tyw_record!(; TypeWrapper::Ptr(new_var(state.table))),
            mk_typewrapper::list(AbsType::Str()),
        ),
        // Dyn -> List
        UnaryOp::ValuesOf() => (
            mk_typewrapper::dynamic(),
            mk_typewrapper::list(AbsType::Dyn()),
        ),
        // Str -> Str
        UnaryOp::StrTrim() => (mk_typewrapper::str(), mk_typewrapper::str()),
        // Str -> List Str
        UnaryOp::StrChars() => (
            mk_typewrapper::str(),
            mk_typewrapper::list(mk_typewrapper::str()),
        ),
        // Str -> Num
        UnaryOp::CharCode() => (mk_typewrapper::str(), mk_typewrapper::num()),
        // Num -> Str
        UnaryOp::CharFromCode() => (mk_typewrapper::num(), mk_typewrapper::str()),
        // Str -> Str
        UnaryOp::StrUppercase() => (mk_typewrapper::str(), mk_typewrapper::str()),
        // Str -> Str
        UnaryOp::StrLowercase() => (mk_typewrapper::str(), mk_typewrapper::str()),
        // Str -> Num
        UnaryOp::StrLength() => (mk_typewrapper::str(), mk_typewrapper::num()),
        // Dyn -> Str
        UnaryOp::ToStr() => (mk_typewrapper::dynamic(), mk_typewrapper::num()),
        // Str -> Num
        UnaryOp::NumFromStr() => (mk_typewrapper::str(), mk_typewrapper::num()),
        // Str -> < | Dyn>
        UnaryOp::EnumFromStr() => (
            mk_typewrapper::str(),
            mk_tyw_enum!(mk_typewrapper::dynamic()),
        ),
    })
}

/// Type of a binary operation.
pub fn get_bop_type(
    state: &mut State,
    op: &BinaryOp,
) -> Result<(TypeWrapper, TypeWrapper, TypeWrapper), TypecheckError> {
    Ok(match op {
        // Num -> Num -> Num
        BinaryOp::Plus()
        | BinaryOp::Sub()
        | BinaryOp::Mult()
        | BinaryOp::Div()
        | BinaryOp::Modulo() => (
            mk_typewrapper::num(),
            mk_typewrapper::num(),
            mk_typewrapper::num(),
        ),
        // Str -> Str -> Str
        BinaryOp::StrConcat() => (
            mk_typewrapper::str(),
            mk_typewrapper::str(),
            mk_typewrapper::str(),
        ),
        // Sym -> Dyn -> Dyn -> Dyn
        // This should not happen, as `ApplyContract()` is only produced during evaluation.
        BinaryOp::Assume() => panic!("cannot typecheck assume"),
        BinaryOp::Unwrap() => (
            mk_typewrapper::sym(),
            mk_typewrapper::dynamic(),
            mk_tyw_arrow!(AbsType::Dyn(), AbsType::Dyn()),
        ),
        // Str -> Dyn -> Dyn
        BinaryOp::Tag() => (
            mk_typewrapper::str(),
            mk_typewrapper::dynamic(),
            mk_typewrapper::dynamic(),
        ),
        // forall a b. a -> b -> Bool
        BinaryOp::Eq() => (
            TypeWrapper::Ptr(new_var(state.table)),
            TypeWrapper::Ptr(new_var(state.table)),
            mk_typewrapper::bool(),
        ),
        // Num -> Num -> Bool
        BinaryOp::LessThan()
        | BinaryOp::LessOrEq()
        | BinaryOp::GreaterThan()
        | BinaryOp::GreaterOrEq() => (
            mk_typewrapper::num(),
            mk_typewrapper::num(),
            mk_typewrapper::bool(),
        ),
        // Str -> Dyn -> Dyn
        BinaryOp::GoField() => (
            mk_typewrapper::str(),
            mk_typewrapper::dynamic(),
            mk_typewrapper::dynamic(),
        ),
        // forall a. Str -> { _ : a} -> a
        BinaryOp::DynAccess() => {
            let res = TypeWrapper::Ptr(new_var(state.table));

            (
                mk_typewrapper::str(),
                mk_typewrapper::dyn_record(res.clone()),
                res,
            )
        }
        // forall a. Str -> { _ : a } -> a -> { _ : a }
        BinaryOp::DynExtend() => {
            let res = TypeWrapper::Ptr(new_var(state.table));
            (
                mk_typewrapper::str(),
                mk_typewrapper::dyn_record(res.clone()),
                mk_tyw_arrow!(res.clone(), mk_typewrapper::dyn_record(res)),
            )
        }
        // forall a. Str -> { _ : a } -> { _ : a}
        BinaryOp::DynRemove() => {
            let res = TypeWrapper::Ptr(new_var(state.table));

            (
                mk_typewrapper::str(),
                mk_typewrapper::dyn_record(res.clone()),
                mk_typewrapper::dyn_record(res),
            )
        }
        // Str -> Dyn -> Bool
        BinaryOp::HasField() => (
            mk_typewrapper::str(),
            mk_typewrapper::dynamic(),
            mk_typewrapper::bool(),
        ),
        // forall a. List a -> List a -> List a
        BinaryOp::ListConcat() => {
            let ty_elt = TypeWrapper::Ptr(new_var(state.table));
            let ty_list = mk_typewrapper::list(ty_elt);
            (ty_list.clone(), ty_list.clone(), ty_list)
        }
        // forall a. List a -> Num -> a
        BinaryOp::ListElemAt() => {
            let ty_elt = TypeWrapper::Ptr(new_var(state.table));
            (
                mk_typewrapper::list(ty_elt.clone()),
                mk_typewrapper::num(),
                ty_elt,
            )
        }
        // Dyn -> Dyn -> Dyn
        BinaryOp::Merge() => (
            mk_typewrapper::dynamic(),
            mk_typewrapper::dynamic(),
            mk_typewrapper::dynamic(),
        ),
        // <Md5, Sha1, Sha256, Sha512> -> Str -> Str
        BinaryOp::Hash() => (
            mk_tyw_enum!(
                "Md5",
                "Sha1",
                "Sha256",
                "Sha512",
                mk_typewrapper::row_empty()
            ),
            mk_typewrapper::str(),
            mk_typewrapper::str(),
        ),
        // forall a. <Json, Yaml, Toml> -> a -> Str
        BinaryOp::Serialize() => {
            let ty_input = TypeWrapper::Ptr(new_var(state.table));
            (
                mk_tyw_enum!("Json", "Yaml", "Toml", mk_typewrapper::row_empty()),
                ty_input,
                mk_typewrapper::str(),
            )
        }
        // <Json, Yaml, Toml> -> Str -> Dyn
        BinaryOp::Deserialize() => (
            mk_tyw_enum!("Json", "Yaml", "Toml", mk_typewrapper::row_empty()),
            mk_typewrapper::str(),
            mk_typewrapper::dynamic(),
        ),
        // Num -> Num -> Num
        BinaryOp::Pow() => (
            mk_typewrapper::num(),
            mk_typewrapper::num(),
            mk_typewrapper::num(),
        ),
        // Str -> Str -> Bool
        BinaryOp::StrContains() => (
            mk_typewrapper::str(),
            mk_typewrapper::str(),
            mk_typewrapper::bool(),
        ),
        // Str -> Str -> Bool
        BinaryOp::StrIsMatch() => (
            mk_typewrapper::str(),
            mk_typewrapper::str(),
            mk_typewrapper::bool(),
        ),
        // Str -> Str -> {match: Str, index: Num, groups: List Str}
        BinaryOp::StrMatch() => (
            mk_typewrapper::str(),
            mk_typewrapper::str(),
            mk_tyw_record!(
                ("match", AbsType::Str()),
                ("index", AbsType::Num()),
                ("groups", mk_typewrapper::list(AbsType::Str()))
            ),
        ),
        // Str -> Str -> List Str
        BinaryOp::StrSplit() => (
            mk_typewrapper::str(),
            mk_typewrapper::str(),
            mk_typewrapper::list(AbsType::Str()),
        ),
    })
}

pub fn get_nop_type(
    _state: &mut State,
    op: &NAryOp,
) -> Result<(Vec<TypeWrapper>, TypeWrapper), TypecheckError> {
    Ok(match op {
        // Str -> Str -> Str -> Str
        NAryOp::StrReplace() | NAryOp::StrReplaceRegex() => (
            vec![
                mk_typewrapper::str(),
                mk_typewrapper::str(),
                mk_typewrapper::str(),
            ],
            mk_typewrapper::str(),
        ),
        // Str -> Num -> Num -> Str
        NAryOp::StrSubstr() => (
            vec![
                mk_typewrapper::str(),
                mk_typewrapper::num(),
                mk_typewrapper::num(),
            ],
            mk_typewrapper::str(),
        ),
        // This should not happen, as Switch() is only produced during evaluation.
        NAryOp::MergeContract() => panic!("cannot typecheck MergeContract()"),
    })
}

/// The unification table.
///
/// Map each unification variable to either another type variable or a concrete type it has been
/// unified with. Each binding `(ty, var)` in this map should be thought of an edge in a
/// unification graph.
pub type UnifTable = HashMap<usize, Option<Closure>>;

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
fn constraint(state: &mut State, x: Closure, id: Ident) -> Result<(), RowUnifError> {
    match x.ty.clone() {
        TypeWrapper::Ptr(p) => match get_root(state.table, p) {
            clos
            @
            Closure {
                ty: TypeWrapper::Concrete(_),
                ..
            } => constraint(state, clos, id),
            Closure {
                ty: TypeWrapper::Ptr(root),
                ..
            } => {
                if let Some(v) = state.constr.get_mut(&root) {
                    v.insert(id);
                } else {
                    state.constr.insert(root, vec![id].into_iter().collect());
                }
                Ok(())
            }
            c
            @
            Closure {
                ty: TypeWrapper::Constant(_),
                ..
            } => Err(RowUnifError::IllformedRow(c.flatten_type())),
        },
        TypeWrapper::Concrete(AbsType::RowEmpty()) => Ok(()),
        TypeWrapper::Concrete(AbsType::RowExtend(id2, tyw, t)) => {
            if id2 == id {
                Err(RowUnifError::UnsatConstr(id, tyw.map(|tyw| *tyw)))
            } else {
                constraint(state, Closure::new(*t, x.env.clone()), id)
            }
        }
        _ => Err(RowUnifError::IllformedRow(x.flatten_type())),
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
                Closure {
                    ty: TypeWrapper::Ptr(_),
                    ..
                } => (),
                clos => constrain_var_(state, constr, &clos.ty, p),
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
                | AbsType::Var(_) => (),
                AbsType::List(tyw) => constrain_var_(state, HashSet::new(), tyw.as_ref(), p),
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
                        ty.as_ref().map(|boxed| *boxed.clone()),
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
pub fn get_root(table: &UnifTable, x: usize) -> Closure {
    // All queried variable must have been introduced by `new_var` and thus a corresponding entry
    // must always exist in `state`. If not, the typechecking algorithm is not correct, and we
    // panic.
    if let Some(c) = table.get(&x).unwrap() {
        match &c.ty {
            TypeWrapper::Ptr(y) => get_root(table, *y),
            TypeWrapper::Concrete(_) => c.clone(),
            TypeWrapper::Constant(_) => c.clone(),
        }
    } else {
        Closure::atomic(TypeWrapper::Ptr(x))
    }
}
