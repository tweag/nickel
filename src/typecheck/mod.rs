//! Implementation of the typechecker.
//!
//! # Mode
//!
//! Typechecking can be made in to different modes:
//! - **Strict**: correspond to traditional typechecking in statically typed languages. This
//!   happens inside a statically typed block. Statically typed blocks are introduced by the type
//!   ascription operator `:`, as in `1 + 1 : Num` or `let f : Num -> Num = fun x => x + 1 in ..`. This is
//!   implemented by [`type_check_`] and variants.
//! - **Non strict**: do not enforce any typing but continue to traverse the AST looking for other
//!   typed blocks to typecheck and store the annotations of let bindings in the environment. This is
//!   implemented by the [`walk`] function.
//!
//! The algorithm starts in non strict mode. It is switched to strict mode when entering a
//! statically typed block (an expression annotated with a type), and is switched back to
//! non-strict mode when entering an expression annotated with a contract. Type and contract
//! annotations thus serve both another purpose beside enforcing a type or a contract, which is to
//! switch the typechecking mode.
//!
//! # Type inference
//!
//! Type inference is done via a form of bidirectional typechecking coupled with unification, in
//! the same spirit as GHC (Haskell), albeit the type system of Nickel is much simpler. The type of
//! un-annotated let-bound expressions (the type of `bound_exp` in `let x = bound_exp in body`) is
//! inferred in strict mode, but it is never implicitly generalized. For example, the following
//! program is rejected:
//!
//! ```nickel
//! # Rejected
//! let id = fun x => x in seq (id "a") (id 5) : Num
//! ```
//!
//! Indeed, `id` is given the type `_a -> _a`, where `_a` is a unification variable, but is not
//! generalized to `forall a. a -> a`. At the first call site, `_a` is unified with `Str`, and at the second
//! call site the typechecker complains that `5` is not of type `Str`.
//!
//! This restriction is on purpose, as generalization is not trivial to implement efficiently and
//! more importantly can interact with other components of the type system and type inference. If
//! polymorphism is required, the user can simply add annotation:
//!
//! ```nickel
//! # Accepted
//! let id : forall a. a -> a = fun x => x in seq (id "a") (id 5) : Num
//! ```
//!
//! In non-strict mode, the type of let-bound expressions is inferred in a shallow way (see
//! [`apparent_type`]).
use crate::cache::ImportResolver;
use crate::destruct::*;
use crate::environment::Environment as GenericEnvironment;
use crate::error::TypecheckError;
use crate::identifier::Ident;
use crate::term::{Contract, MetaValue, RichTerm, StrChunk, Term, TraverseOrder};
use crate::types::{AbsType, Types};
use crate::{mk_tyw_arrow, mk_tyw_enum, mk_tyw_enum_row, mk_tyw_record, mk_tyw_row};
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;

use self::linearization::{Linearization, Linearizer, StubHost};

pub mod error;
pub mod linearization;
pub mod operation;
pub mod reporting;
#[macro_use]
pub mod mk_typewrapper;

use error::*;
use operation::{get_bop_type, get_nop_type, get_uop_type};

/// The typing environment.
pub type Environment = GenericEnvironment<Ident, TypeWrapper>;

/// Mapping from wildcard ID to inferred type
pub type Wildcards = Vec<Types>;

/// A structure holding the two typing environments, the global and the local.
///
/// The global typing environment is constructed from the global term environment (see
/// [`crate::eval::eval`]) which holds the Nickel builtin functions. It is a read-only shared
/// environment used to retrieve the type of such functions.
#[derive(Debug, PartialEq, Clone)]
pub struct Envs<'a> {
    global: &'a Environment,
    local: Environment,
}

#[derive(Clone, Debug)]
pub enum EnvBuildError {
    NotARecord(RichTerm),
}

impl<'a> Envs<'a> {
    /// Create an `Envs` value with an empty local environment from a global environment.
    pub fn from_global(global: &'a Environment) -> Self {
        Envs {
            global,
            local: Environment::new(),
        }
    }

    /// Similar to [`Envs::from_global`], but use another `Envs` to provide the global environment.
    pub fn from_envs(envs: &'a Envs) -> Self {
        Envs {
            global: envs.global,
            local: Environment::new(),
        }
    }

    /// Populate a new global typing environment from a `Vec` of parsed files.
    pub fn mk_global(envs: Vec<RichTerm>) -> Result<Environment, EnvBuildError> {
        Ok(envs
            .iter()
            .map(|rt| {
                if let Term::RecRecord(rec, ..) = rt.as_ref() {
                    Ok(rec
                        .iter()
                        .map(|(id, rt)| (id.clone(), infer_type(rt.as_ref()))))
                } else {
                    Err(EnvBuildError::NotARecord(rt.clone()))
                }
            })
            .collect::<Result<Vec<_>, EnvBuildError>>()?
            .into_iter()
            .flatten()
            .collect())
    }

    /// Add the bindings of a record to a typing environment. Ignore fields whose name are defined
    /// through interpolation.
    //TODO: support the case of a record with a type annotation.
    pub fn env_add_term(
        env: &mut Environment,
        rt: &RichTerm,
        resolver: &dyn ImportResolver,
    ) -> Result<(), EnvBuildError> {
        let RichTerm { term, pos } = rt;

        match term.as_ref() {
            Term::Record(bindings, _) | Term::RecRecord(bindings, ..) => {
                for (id, t) in bindings {
                    let tyw: TypeWrapper =
                        apparent_type(t.as_ref(), Some(&Envs::from_global(env)), Some(resolver))
                            .into();
                    env.insert(id.clone(), tyw);
                }

                Ok(())
            }
            t => Err(EnvBuildError::NotARecord(RichTerm::new(t.clone(), *pos))),
        }
    }

    /// Bind one term in a typing environment.
    pub fn env_add(env: &mut Environment, id: Ident, rt: &RichTerm, resolver: &dyn ImportResolver) {
        env.insert(
            id,
            apparent_type(rt.as_ref(), Some(&Envs::from_global(env)), Some(resolver)).into(),
        );
    }

    /// Fetch a binding from the environment. Try first in the local environment, and then in the
    /// global.
    pub fn get(&self, ident: &Ident) -> Option<TypeWrapper> {
        self.local.get(ident).or_else(|| self.global.get(ident))
    }

    /// Wrapper to insert a new binding in the local environment.
    pub fn insert(&mut self, ident: Ident, tyw: TypeWrapper) {
        self.local.insert(ident, tyw);
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
    /// A mapping from wildcard ID to unification variable.
    wildcard_vars: &'a mut Vec<TypeWrapper>,
}

/// Typecheck a term.
///
/// Return the inferred type in case of success. This is just a wrapper that calls `type_check_`
/// with a fresh unification variable as goal.
///
/// Note that this function doesn't recursively typecheck imports (anymore), but just the current
/// file. It however still needs the resolver to get the apparent type of imports.
///
/// Return the type inferred for type wildcards.
pub fn type_check<LL>(
    t: &RichTerm,
    global_env: &Environment,
    resolver: &impl ImportResolver,
    mut linearizer: LL,
) -> Result<(Wildcards, LL::Completed), TypecheckError>
where
    LL: Linearizer<CompletionExtra = (UnifTable, HashMap<usize, Ident>)>,
{
    let (mut table, mut names) = (UnifTable::new(), HashMap::new());
    let mut building = Linearization::new(LL::Building::default());
    let mut wildcard_vars = Vec::new();

    {
        let mut state: State = State {
            resolver,
            table: &mut table,
            constr: &mut RowConstr::new(),
            names: &mut names,
            wildcard_vars: &mut wildcard_vars,
        };

        walk(
            &mut state,
            Envs::from_global(global_env),
            &mut building,
            linearizer.scope(),
            t,
        )?;
    }

    let result = wildcard_vars_to_type(wildcard_vars, &table);
    let lin = linearizer.complete(building, (table, names)).into_inner();

    Ok((result, lin))
}

/// Typecheck a term using the given global typing environment. Same as [`type_check`], but it
/// directly takes a global typing environment, instead of building one from a term environment as
/// `type_check` does.
///
/// This function is used to typecheck an import in a clean environment, when we don't have access
/// to the original term environment anymore, and hence cannot call `type_check` directly, but we
/// already have built a global typing environment.
///
/// Return the type inferred for type wildcards. This is just a wrapper that calls `type_check_`
/// with a fresh unification variable as goal.
pub fn type_check_in_env(
    t: &RichTerm,
    global: &Environment,
    resolver: &dyn ImportResolver,
) -> Result<Wildcards, TypecheckError> {
    let mut table = UnifTable::new();
    let mut wildcard_vars = Vec::new();

    let mut state = State {
        resolver,
        table: &mut table,
        constr: &mut RowConstr::new(),
        names: &mut HashMap::new(),
        wildcard_vars: &mut wildcard_vars,
    };

    walk(
        &mut state,
        Envs::from_global(global),
        &mut Linearization::new(()),
        StubHost::<()>::new(),
        t,
    )?;

    Ok(wildcard_vars_to_type(wildcard_vars, &table))
}

/// Walk the AST of a term looking for statically typed block to check. Fill the linearization
/// alongside and store the apparent type of variable inside the typing environment.
fn walk<L: Linearizer>(
    state: &mut State,
    mut envs: Envs,
    lin: &mut Linearization<L::Building>,
    mut linearizer: L,
    rt: &RichTerm,
) -> Result<(), TypecheckError> {
    let RichTerm { term: t, pos } = rt;
    linearizer.add_term(lin, t, *pos, mk_typewrapper::dynamic());

    match t.as_ref() {
        Term::ParseError
        | Term::Null
        | Term::Bool(_)
        | Term::Num(_)
        | Term::Str(_)
        | Term::Lbl(_)
        | Term::Enum(_)
        | Term::Sym(_)
        // This function doesn't recursively typecheck imports: this is the responsibility of the
        // caller.
        | Term::Import(_)
        | Term::ResolvedImport(_) => Ok(()),
        Term::Var(x) => envs
            .get(x)
            .ok_or_else(|| TypecheckError::UnboundIdentifier(x.clone(), *pos))
            .map(|_| ()),
        Term::StrChunks(chunks) => {
            chunks
                .iter()
                .try_for_each(|chunk| -> Result<(), TypecheckError> {
                    match chunk {
                        StrChunk::Literal(_) => Ok(()),
                        StrChunk::Expr(t, _) => {
                            walk(state, envs.clone(), lin, linearizer.scope(), t)
                        }
                    }
                })
        }
        Term::Fun(id, t) => {
            // The parameter of an un-annotated function is assigned the type `Dyn`.
            envs.insert(id.clone(), mk_typewrapper::dynamic());
            walk(state, envs, lin, linearizer, t)
        }
        Term::FunPattern(id, pat, t) => {
            if let Some(id) = id {
                envs.insert(id.clone(), binding_type(state, t.as_ref(), &envs, false));
            }

            inject_pat_vars(pat, &mut envs);
            walk(state, envs, lin, linearizer, t)
        }
        Term::Array(terms, _) => terms
            .iter()
            .try_for_each(|t| -> Result<(), TypecheckError> {
                walk(state, envs.clone(), lin, linearizer.scope(), t)
            }),
        Term::Let(x, re, rt, attrs) => {
            let ty_let = binding_type(state, re.as_ref(), &envs, false);

            if attrs.rec {
                envs.insert(x.clone(), ty_let.clone());
            }

            linearizer.retype_ident(lin, x, ty_let.clone());
            walk(state, envs.clone(), lin, linearizer.scope(), re)?;

            if !attrs.rec {
                envs.insert(x.clone(), ty_let);
            }

            walk(state, envs, lin, linearizer, rt)
        }
        Term::LetPattern(x, pat, re, rt) => {
            let ty_let = binding_type(state, re.as_ref(), &envs, false);
            walk(state, envs.clone(), lin, linearizer.scope(), re)?;

            if let Some(x) = x {
                linearizer.retype_ident(lin, x, ty_let.clone());
                envs.insert(x.clone(), ty_let);
            }

            inject_pat_vars(pat, &mut envs);

            walk(state, envs, lin, linearizer, rt)
        }
        Term::App(e, t) => {
            walk(state, envs.clone(), lin, linearizer.scope(), e)?;
            walk(state, envs, lin, linearizer, t)
        }
        Term::Switch(exp, cases, default) => {
            cases.values().chain(default.iter()).try_for_each(|case| {
                walk(state, envs.clone(), lin, linearizer.scope(), case)
            })?;

            walk(state, envs, lin, linearizer, exp)
        }
        Term::RecRecord(stat_map, dynamic, ..) => {
            for id in stat_map.keys() {
                let binding_type = binding_type(
                    state,
                    stat_map.get(id).unwrap().as_ref(),
                    &envs,
                    false,
                );
                envs.insert(id.clone(), binding_type.clone());
                linearizer.retype_ident(lin, id, binding_type);
            }

            stat_map
                .iter()
                .map(|(_, t)| t)
                .chain(dynamic.iter().map(|(_, t)| t))
                .try_for_each(|t| -> Result<(), TypecheckError> {
                    walk(state, envs.clone(), lin, linearizer.scope(), t)
                })
        }
        Term::Record(stat_map, _) => {
            stat_map
                .iter()
                .try_for_each(|(_, t)| -> Result<(), TypecheckError> {
                    walk(state, envs.clone(), lin, linearizer.scope(), t)
                })
        }
        Term::Op1(_, t) => walk(state, envs.clone(), lin, linearizer.scope(), t),
        Term::Op2(_, t1, t2) => {
            walk(state, envs.clone(), lin, linearizer.scope(), t1)?;
            walk(state, envs, lin, linearizer, t2)
        }
        Term::OpN(_, args) => {
           args.iter().try_for_each(|t| -> Result<(), TypecheckError> {
                    walk(
                        state,
                        envs.clone(),
                        lin,
                        linearizer.scope(),
                        t,
                    )
                },
            )
        }
        // An type annotation switches mode to check.
        Term::MetaValue(meta) => {
            meta.contracts.iter().chain(meta.types.iter()).try_for_each(|ty| walk_type(state, envs.clone(), lin, linearizer.scope(), &ty.types))?;

            match meta {
                MetaValue {
                types: Some(Contract { types: ty2, .. }),
                value: Some(t),
                ..
                } => {
                    let tyw2 = TypeWrapper::from(ty2.clone());
                    let instantiated = instantiate_foralls(state, tyw2, ForallInst::Constant);
                    type_check_(state, envs, lin, linearizer, t, instantiated)
                }
                MetaValue {value: Some(t), .. } =>  walk(state, envs, lin, linearizer, t),
                // A metavalue without a body nor a type annotation is a record field without definition.
                _ => Ok(()),
            }
        }
        Term::Wrapped(_, t) => walk(state, envs, lin, linearizer, t),
   }
}

/// Same as [`walk`] but operate on a type, which can contain terms as contracts (`AbsType::Flat`),
/// instead of a term.
fn walk_type<L: Linearizer>(
    state: &mut State,
    envs: Envs,
    lin: &mut Linearization<L::Building>,
    mut linearizer: L,
    ty: &Types,
) -> Result<(), TypecheckError> {
    match &ty.0 {
       AbsType::Dyn()
       | AbsType::Num()
       | AbsType::Bool()
       | AbsType::Str()
       | AbsType::Sym()
       // Currently, the parser can't generate unbound type variables by construction. Thus we
       // don't check here for unbound type variables again.
       | AbsType::Var(_)
       | AbsType::Wildcard(_)
       | AbsType::RowEmpty() => Ok(()),
       AbsType::Arrow(ty1, ty2) => {
           walk_type(state, envs.clone(), lin, linearizer.scope(), ty1.as_ref())?;
           walk_type(state, envs, lin, linearizer, ty2.as_ref())
       }
       AbsType::RowExtend(_, ty_row, tail) => {
         if let Some(ty_row) = ty_row { walk_type(state, envs.clone(), lin, linearizer.scope(), ty_row)? };
         walk_type(state, envs, lin,linearizer, tail)
       }
       AbsType::Flat(t) => walk(state, envs, lin, linearizer, t),
       AbsType::Enum(ty2)
       | AbsType::DynRecord(ty2)
       | AbsType::StaticRecord(ty2)
       | AbsType::Array(ty2)
       | AbsType::Forall(_, ty2) => walk_type(state, envs, lin, linearizer, ty2),
    }
}

// TODO: The insertion of values in the type environment is done but everything is
// typed as `Dyn`.
fn inject_pat_vars(pat: &Destruct, envs: &mut Envs) {
    if let Destruct::Record { matches, rest, .. } = pat {
        if let Some(id) = rest {
            envs.insert(id.clone(), TypeWrapper::Concrete(AbsType::Dyn()));
        }
        matches.iter().for_each(|m| match m {
            Match::Simple(id, ..) => envs.insert(id.clone(), TypeWrapper::Concrete(AbsType::Dyn())),
            Match::Assign(id, _, (bind_id, pat)) => {
                let id = bind_id.as_ref().unwrap_or(id);
                envs.insert(id.clone(), TypeWrapper::Concrete(AbsType::Dyn()));
                if !pat.is_empty() {
                    inject_pat_vars(pat, envs);
                }
            }
        });
    }
}

/// Typecheck a term against a specific type.
///
/// # Arguments
///
/// - `state`: the unification state (see [`State`]).
/// - `env`: the typing environment, mapping free variable to types.
/// - `lin`: The current building linearization of building state `S`
/// - `linearizer`: A linearizer that can modify the linearization
/// - `t`: the term to check.
/// - `ty`: the type to check the term against.
///
/// Registers every term with the `linearizer` and makes sure to scope the
/// liearizer accordingly
fn type_check_<L: Linearizer>(
    state: &mut State,
    mut envs: Envs,
    lin: &mut Linearization<L::Building>,
    mut linearizer: L,
    rt: &RichTerm,
    ty: TypeWrapper,
) -> Result<(), TypecheckError> {
    let RichTerm { term: t, pos } = rt;
    linearizer.add_term(lin, t, *pos, ty.clone());

    match t.as_ref() {
        Term::ParseError => Ok(()),
        // null is inferred to be of type Dyn
        Term::Null => unify(state, ty, mk_typewrapper::dynamic())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Bool(_) => unify(state, ty, mk_typewrapper::bool())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Num(_) => unify(state, ty, mk_typewrapper::num())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Str(_) => unify(state, ty, mk_typewrapper::str())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::StrChunks(chunks) => {
            unify(state, ty, mk_typewrapper::str())
                .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            chunks
                .iter()
                .try_for_each(|chunk| -> Result<(), TypecheckError> {
                    match chunk {
                        StrChunk::Literal(_) => Ok(()),
                        StrChunk::Expr(t, _) => type_check_(
                            state,
                            envs.clone(),
                            lin,
                            linearizer.scope(),
                            t,
                            mk_typewrapper::str(),
                        ),
                    }
                })
        }
        Term::Fun(x, t) => {
            let src = state.table.fresh_unif_var();
            // TODO what to do here, this makes more sense to me, but it means let x = foo in bar
            // behaves quite different to (\x.bar) foo, worth considering if it's ok to type these two differently
            let trg = state.table.fresh_unif_var();
            let arr = mk_tyw_arrow!(src.clone(), trg.clone());
            linearizer.retype_ident(lin, x, src.clone());

            unify(state, ty, arr).map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            envs.insert(x.clone(), src);
            type_check_(state, envs, lin, linearizer, t, trg)
        }
        Term::FunPattern(x, pat, t) => {
            let src = state.table.fresh_unif_var();
            // TODO what to do here, this makes more sense to me, but it means let x = foo in bar
            // behaves quite different to (\x.bar) foo, worth considering if it's ok to type these two differently
            let trg = state.table.fresh_unif_var();
            let arr = mk_tyw_arrow!(src.clone(), trg.clone());
            if let Some(x) = x {
                linearizer.retype_ident(lin, x, src.clone());
                envs.insert(x.clone(), src);
            }
            inject_pat_vars(pat, &mut envs);
            unify(state, ty, arr).map_err(|err| err.into_typecheck_err(state, rt.pos))?;
            type_check_(state, envs, lin, linearizer, t, trg)
        }
        Term::Array(terms, _) => {
            let ty_elts = state.table.fresh_unif_var();

            unify(state, ty, mk_typewrapper::array(ty_elts.clone()))
                .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            terms
                .iter()
                .try_for_each(|t| -> Result<(), TypecheckError> {
                    type_check_(
                        state,
                        envs.clone(),
                        lin,
                        linearizer.scope(),
                        t,
                        ty_elts.clone(),
                    )
                })
        }
        Term::Lbl(_) => {
            // TODO implement lbl type
            unify(state, ty, mk_typewrapper::dynamic())
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Let(x, re, rt, attrs) => {
            let ty_let = binding_type(state, re.as_ref(), &envs, true);
            if attrs.rec {
                envs.insert(x.clone(), ty_let.clone());
            }

            linearizer.retype_ident(lin, x, ty_let.clone());
            type_check_(
                state,
                envs.clone(),
                lin,
                linearizer.scope(),
                re,
                ty_let.clone(),
            )?;

            if !attrs.rec {
                envs.insert(x.clone(), ty_let);
            }
            type_check_(state, envs, lin, linearizer, rt, ty)
        }
        Term::LetPattern(x, pat, re, rt) => {
            let ty_let = binding_type(state, re.as_ref(), &envs, true);
            type_check_(
                state,
                envs.clone(),
                lin,
                linearizer.scope(),
                re,
                ty_let.clone(),
            )?;

            if let Some(x) = x {
                linearizer.retype_ident(lin, x, ty_let.clone());
                envs.insert(x.clone(), ty_let);
            }
            inject_pat_vars(pat, &mut envs);
            type_check_(state, envs, lin, linearizer, rt, ty)
        }
        Term::App(e, t) => {
            let src = state.table.fresh_unif_var();
            let arr = mk_tyw_arrow!(src.clone(), ty);

            type_check_(state, envs.clone(), lin, linearizer.scope(), e, arr)?;
            type_check_(state, envs, lin, linearizer, t, src)
        }
        Term::Switch(exp, cases, default) => {
            // Currently, if it has a default value, we typecheck the whole thing as
            // taking ANY enum, since it's more permissive and there's no loss of information
            let res = state.table.fresh_unif_var();

            for case in cases.values() {
                type_check_(
                    state,
                    envs.clone(),
                    lin,
                    linearizer.scope(),
                    case,
                    res.clone(),
                )?;
            }

            let row = match default {
                Some(t) => {
                    type_check_(state, envs.clone(), lin, linearizer.scope(), t, res.clone())?;
                    state.table.fresh_unif_var()
                }
                None => cases.iter().try_fold(
                    mk_typewrapper::row_empty(),
                    |acc, x| -> Result<TypeWrapper, TypecheckError> {
                        Ok(mk_tyw_enum_row!(x.0.clone(); acc))
                    },
                )?,
            };

            unify(state, ty, res).map_err(|err| err.into_typecheck_err(state, rt.pos))?;
            type_check_(state, envs, lin, linearizer, exp, mk_tyw_enum!(; row))
        }
        Term::Var(x) => {
            let x_ty = envs
                .get(x)
                .ok_or_else(|| TypecheckError::UnboundIdentifier(x.clone(), *pos))?;

            let instantiated = instantiate_foralls(state, x_ty, ForallInst::Ptr);
            unify(state, ty, instantiated).map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Enum(id) => {
            let row = state.table.fresh_unif_var();
            unify(state, ty, mk_tyw_enum!(id.clone(); row))
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        // If some fields are defined dynamically, the only potential type that works is `{_ : a}`
        // for some `a`
        Term::RecRecord(stat_map, dynamic, ..) if !dynamic.is_empty() => {
            let ty_dyn = state.table.fresh_unif_var();

            for id in stat_map.keys() {
                envs.insert(id.clone(), ty_dyn.clone());
                linearizer.retype_ident(lin, id, ty_dyn.clone())
            }

            stat_map
                .iter()
                .try_for_each(|(_, t)| -> Result<(), TypecheckError> {
                    type_check_(
                        state,
                        envs.clone(),
                        lin,
                        linearizer.scope(),
                        t,
                        ty_dyn.clone(),
                    )
                })?;

            unify(state, ty, mk_typewrapper::dyn_record(ty_dyn))
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Record(stat_map, _) | Term::RecRecord(stat_map, ..) => {
            // For recursive records, we look at the apparent type of each field and bind it in
            // env before actually typechecking the content of fields.
            // Fields defined by interpolation are ignored.
            if let Term::RecRecord(..) = t.as_ref() {
                for (id, rt) in stat_map {
                    let tyw = binding_type(state, rt.as_ref(), &envs, true);
                    envs.insert(id.clone(), tyw.clone());
                    linearizer.retype_ident(lin, id, tyw);
                }
            }

            let root_ty = if let TypeWrapper::Ptr(p) = ty {
                state.table.root(p)
            } else {
                ty.clone()
            };

            if let TypeWrapper::Concrete(AbsType::DynRecord(rec_ty)) = root_ty {
                // Checking for a dynamic record
                stat_map
                    .iter()
                    .try_for_each(|(_, t)| -> Result<(), TypecheckError> {
                        type_check_(
                            state,
                            envs.clone(),
                            lin,
                            linearizer.scope(),
                            t,
                            (*rec_ty).clone(),
                        )
                    })
            } else {
                let row = stat_map.iter().try_fold(
                    mk_tyw_row!(),
                    |acc, (id, field)| -> Result<TypeWrapper, TypecheckError> {
                        // In the case of a recursive record, new types (either type variables or
                        // annotations) have already be determined and put in the typing
                        // environment, and we need to use the same.
                        let ty = if let Term::RecRecord(..) = t.as_ref() {
                            envs.get(id).unwrap()
                        } else {
                            state.table.fresh_unif_var()
                        };

                        type_check_(
                            state,
                            envs.clone(),
                            lin,
                            linearizer.scope(),
                            field,
                            ty.clone(),
                        )?;

                        Ok(mk_tyw_row!((id.clone(), ty); acc))
                    },
                )?;

                unify(state, ty, mk_tyw_record!(; row))
                    .map_err(|err| err.into_typecheck_err(state, rt.pos))
            }
        }
        Term::Op1(op, t) => {
            let (ty_arg, ty_res) = get_uop_type(state, op)?;

            type_check_(state, envs.clone(), lin, linearizer.scope(), t, ty_arg)?;

            let instantiated = instantiate_foralls(state, ty_res, ForallInst::Ptr);
            unify(state, ty, instantiated).map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Op2(op, t1, t2) => {
            let (ty_arg1, ty_arg2, ty_res) = get_bop_type(state, op)?;

            unify(state, ty, ty_res).map_err(|err| err.into_typecheck_err(state, rt.pos))?;
            type_check_(state, envs.clone(), lin, linearizer.scope(), t1, ty_arg1)?;
            type_check_(state, envs, lin, linearizer, t2, ty_arg2)
        }
        Term::OpN(op, args) => {
            let (tys_op, ty_ret) = get_nop_type(state, op)?;

            unify(state, ty, ty_ret).map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            tys_op.into_iter().zip(args.iter()).try_for_each(
                |(ty_t, t)| -> Result<_, TypecheckError> {
                    type_check_(state, envs.clone(), lin, linearizer.scope(), t, ty_t)?;
                    Ok(())
                },
            )?;

            Ok(())
        }
        Term::MetaValue(meta) => {
            meta.contracts
                .iter()
                .chain(meta.types.iter())
                .try_for_each(|ty| {
                    walk_type(state, envs.clone(), lin, linearizer.scope(), &ty.types)
                })?;

            match meta {
                MetaValue {
                    types: Some(Contract { types: ty2, .. }),
                    value: Some(t),
                    ..
                } => {
                    let tyw2 = TypeWrapper::from(ty2.clone());
                    let instantiated =
                        instantiate_foralls(state, tyw2.clone(), ForallInst::Constant);

                    unify(state, tyw2, ty).map_err(|err| err.into_typecheck_err(state, rt.pos))?;
                    type_check_(state, envs, lin, linearizer, t, instantiated)
                }
                // A metavalue without a type annotation but with a contract annotation switches
                // the typechecker back to walk mode. If there are several contracts, we
                // arbitrarily chose the first one as the apparent type.
                MetaValue {
                    contracts, value, ..
                } if !contracts.is_empty() => {
                    let ctr = contracts.get(0).unwrap();
                    let Contract { types: ty2, .. } = ctr;

                    unify(state, ty, ty2.clone().into())
                        .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

                    // if there's an inner value, we still have to walk it, as it may contain
                    // statically typed block.
                    if let Some(t) = value {
                        walk(state, envs, lin, linearizer, t)
                    } else {
                        Ok(())
                    }
                }
                // A non-empty metavalue without a type or a contract annotation is typechecked in
                // the same way as its inner value
                MetaValue { value: Some(t), .. } => {
                    type_check_(state, envs, lin, linearizer, t, ty)
                }
                // A metavalue without a body nor a type annotation is a record field without definition.
                // We infer it to be of type `Dyn` for now.
                _ => unify(state, ty, mk_typewrapper::dynamic())
                    .map_err(|err| err.into_typecheck_err(state, rt.pos)),
            }
        }
        Term::Sym(_) => unify(state, ty, mk_typewrapper::sym())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Wrapped(_, t) => type_check_(state, envs, lin, linearizer, t, ty),
        Term::Import(_) => unify(state, ty, mk_typewrapper::dynamic())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        // We use the apparent type of the import for checking. This function doesn't recursively
        // typecheck imports: this is the responsibility of the caller.
        Term::ResolvedImport(file_id) => {
            let t = state
                .resolver
                .get(*file_id)
                .expect("Internal error: resolved import not found during typechecking.");
            let ty_import: TypeWrapper = apparent_type(
                t.as_ref(),
                Some(&Envs::from_envs(&envs)),
                Some(state.resolver),
            )
            .into();
            unify(state, ty, ty_import).map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
    }
}

/// Determine the type of a let-bound expression, or more generally of any binding (e.g. fields)
/// that may be stored in a typing environment at some point.
///
/// Call [`apparent_type`] to see if the binding is annotated. If it is, return this type as a
/// [`TypeWrapper`]. Otherwise:
///     * in non strict mode, we won't (and possibly can't) infer the type of `bound_exp`: just
///       return `Dyn`.
///     * in strict mode, we will typecheck `bound_exp`: return a new unification variable to be
///       associated to `bound_exp`.
/// As this function is always called in a context where an `ImportResolver` is present, expect
/// it passed in arguments.
///
/// If the annotated type contains any wildcard:
///     * in non strict mode, wildcards are assigned `Dyn`.
///     * in strict mode, the wildcard is typechecked, and we return the unification variable
///       corresponding to it.
fn binding_type(state: &mut State, t: &Term, envs: &Envs, strict: bool) -> TypeWrapper {
    let ty_apt = apparent_type(t, Some(envs), Some(state.resolver));

    match ty_apt {
        ApparentType::Annotated(ty) if strict => {
            replace_wildcards_with_var(state.table, state.wildcard_vars, ty)
        }
        ApparentType::Approximated(_) if strict => state.table.fresh_unif_var(),
        ty_apt => ty_apt.into(),
    }
}

/// Substitute wildcards in a type for their unification variable.
fn replace_wildcards_with_var(
    table: &mut UnifTable,
    wildcard_vars: &mut Vec<TypeWrapper>,
    ty: Types,
) -> TypeWrapper {
    match ty.0 {
        AbsType::Wildcard(i) => get_wildcard_var(table, wildcard_vars, i),
        _ => TypeWrapper::Concrete(
            ty.0.map(|ty| Box::new(replace_wildcards_with_var(table, wildcard_vars, *ty))),
        ),
    }
}

/// Different kinds of apparent types (see [`apparent_type`]).
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
            ApparentType::Annotated(ty) if has_wildcards(&ty) => Types(AbsType::Dyn()),
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
            ApparentType::Annotated(ty) if has_wildcards(&ty) => {
                TypeWrapper::Concrete(AbsType::Dyn())
            }
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
///   user-provided type, unless that type is a wildcard.
/// - if `bound_exp` is a constant (string, number, boolean or symbol) which type can be deduced
///   directly without unfolding the expression further, return the corresponding exact type.
/// - if `bound_exp` is an array, return `Array Dyn`.
/// - if `bound_exp` is a resolved import, return the apparent type of the imported term. Returns
///   `Dyn` if the resolver is not passed as a parameter to the function.
/// - Otherwise, return an approximation of the type (currently `Dyn`, but could be more precise in
///   the future, such as `Dyn -> Dyn` for functions, `{ | Dyn}` for records, and so on).
pub fn apparent_type(
    t: &Term,
    envs: Option<&Envs>,
    resolver: Option<&dyn ImportResolver>,
) -> ApparentType {
    match t {
        Term::MetaValue(MetaValue {
            types: Some(Contract { types: ty, .. }),
            ..
        }) => ApparentType::Annotated(ty.clone()),
        // For metavalues, if there's no type annotation, choose the first contract appearing.
        Term::MetaValue(MetaValue { contracts, .. }) if !contracts.is_empty() => {
            ApparentType::Annotated(contracts.get(0).unwrap().types.clone())
        }
        Term::MetaValue(MetaValue { value: Some(v), .. }) => {
            apparent_type(v.as_ref(), envs, resolver)
        }
        Term::Num(_) => ApparentType::Inferred(Types(AbsType::Num())),
        Term::Bool(_) => ApparentType::Inferred(Types(AbsType::Bool())),
        Term::Sym(_) => ApparentType::Inferred(Types(AbsType::Sym())),
        Term::Str(_) | Term::StrChunks(_) => ApparentType::Inferred(Types(AbsType::Str())),
        Term::Array(..) => {
            ApparentType::Approximated(Types(AbsType::Array(Box::new(Types(AbsType::Dyn())))))
        }
        Term::Var(id) => envs
            .and_then(|envs| envs.get(id))
            .map(ApparentType::FromEnv)
            .unwrap_or(ApparentType::Approximated(Types(AbsType::Dyn()))),
        Term::ResolvedImport(f) => {
            if let Some(r) = resolver {
                let t = r
                    .get(*f)
                    .expect("Internal error: resolved import not found during typechecking.");
                apparent_type(&t.term, envs, Some(r))
            } else {
                ApparentType::Approximated(Types(AbsType::Dyn()))
            }
        }
        _ => ApparentType::Approximated(Types(AbsType::Dyn())),
    }
}

/// Infer the type of a non annotated record by gathering the apparent type of the fields. It's
/// currently used essentially to type the stdlib.
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
        t => apparent_type(t, None, None).into(),
    }
}

/// Deeply check whether a type contains a wildcard.
fn has_wildcards(ty: &Types) -> bool {
    let mut has_wildcard = false;
    ty.clone()
        .traverse::<_, _, std::convert::Infallible>(
            &mut |ty, has_wildcard| {
                if ty.0.is_wildcard() {
                    *has_wildcard = true;
                }
                Ok(ty)
            },
            &mut has_wildcard,
            TraverseOrder::TopDown,
        )
        .unwrap();
    has_wildcard
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
            Concrete(AbsType::Array(ty)) => Concrete(AbsType::Array(Box::new(ty.subst(id, to)))),
            Concrete(AbsType::Wildcard(i)) => Concrete(AbsType::Wildcard(i)),
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

/// Look for a binding in a row, or add a new one if it is not present and if allowed by [row
/// constraints][RowConstr].
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
        r = state.table.root(p);
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
                if set.contains(id) {
                    return Err(RowUnifError::UnsatConstr(id.clone(), ty.map(|tyw| *tyw)));
                }
            }
            let new_row = state.table.fresh_unif_var();
            constraint(state, new_row.clone(), id.clone())?;
            state.table.assign(
                root,
                TypeWrapper::Concrete(AbsType::RowExtend(
                    id.clone(),
                    ty.clone(),
                    Box::new(new_row.clone()),
                )),
            );
            Ok((ty, new_row))
        }
        other => Err(RowUnifError::IllformedRow(other)),
    }
}

/// Try to unify two types.
pub fn unify(state: &mut State, mut t1: TypeWrapper, mut t2: TypeWrapper) -> Result<(), UnifError> {
    if let TypeWrapper::Ptr(pt1) = t1 {
        t1 = state.table.root(pt1);
    }
    if let TypeWrapper::Ptr(pt2) = t2 {
        t2 = state.table.root(pt2);
    }

    // t1 and t2 are roots of the type
    match (t1, t2) {
        // If either type is a wildcard, unify with the associated type var
        (TypeWrapper::Concrete(AbsType::Wildcard(id)), ty2)
        | (ty2, TypeWrapper::Concrete(AbsType::Wildcard(id))) => {
            let ty1 = get_wildcard_var(state.table, state.wildcard_vars, id);
            unify(state, ty1, ty2)
        }
        (TypeWrapper::Concrete(s1), TypeWrapper::Concrete(s2)) => match (s1, s2) {
            (AbsType::Dyn(), AbsType::Dyn()) => Ok(()),
            (AbsType::Num(), AbsType::Num()) => Ok(()),
            (AbsType::Bool(), AbsType::Bool()) => Ok(()),
            (AbsType::Str(), AbsType::Str()) => Ok(()),
            (AbsType::Array(tyw1), AbsType::Array(tyw2)) => unify(state, *tyw1, *tyw2),
            (AbsType::Sym(), AbsType::Sym()) => Ok(()),
            (AbsType::Arrow(s1s, s1t), AbsType::Arrow(s2s, s2t)) => {
                unify(state, (*s1s).clone(), (*s2s).clone()).map_err(|err| {
                    UnifError::DomainMismatch(
                        TypeWrapper::Concrete(AbsType::Arrow(s1s.clone(), s1t.clone())),
                        TypeWrapper::Concrete(AbsType::Arrow(s2s.clone(), s2t.clone())),
                        Box::new(err),
                    )
                })?;
                unify(state, (*s1t).clone(), (*s2t).clone()).map_err(|err| {
                    UnifError::CodomainMismatch(
                        TypeWrapper::Concrete(AbsType::Arrow(s1s, s1t)),
                        TypeWrapper::Concrete(AbsType::Arrow(s2s, s2t)),
                        Box::new(err),
                    )
                })
            }
            (AbsType::Flat(s), AbsType::Flat(t)) => Err(UnifError::IncomparableFlatTypes(s, t)),
            (r1, r2) if r1.is_row_type() && r2.is_row_type() => {
                unify_rows(state, r1.clone(), r2.clone()).map_err(|err| {
                    err.into_unif_err(TypeWrapper::Concrete(r1), TypeWrapper::Concrete(r2))
                })
            }
            (AbsType::Enum(tyw1), AbsType::Enum(tyw2)) => match (*tyw1, *tyw2) {
                (TypeWrapper::Concrete(r1), TypeWrapper::Concrete(r2))
                    if r1.is_row_type() && r2.is_row_type() =>
                {
                    unify_rows(state, r1.clone(), r2.clone())
                        .map_err(|err| err.into_unif_err(mk_tyw_enum!(; r1), mk_tyw_enum!(; r2)))
                }
                (TypeWrapper::Concrete(r), _) if !r.is_row_type() => {
                    Err(UnifError::IllformedType(mk_tyw_enum!(; r)))
                }
                (_, TypeWrapper::Concrete(r)) if !r.is_row_type() => {
                    Err(UnifError::IllformedType(mk_tyw_enum!(; r)))
                }
                (tyw1, tyw2) => unify(state, tyw1, tyw2),
            },
            (AbsType::StaticRecord(tyw1), AbsType::StaticRecord(tyw2)) => match (*tyw1, *tyw2) {
                (TypeWrapper::Concrete(r1), TypeWrapper::Concrete(r2))
                    if r1.is_row_type() && r2.is_row_type() =>
                {
                    unify_rows(state, r1.clone(), r2.clone()).map_err(|err| {
                        err.into_unif_err(mk_tyw_record!(; r1), mk_tyw_record!(; r2))
                    })
                }
                (TypeWrapper::Concrete(AbsType::Var(id)), _)
                | (_, TypeWrapper::Concrete(AbsType::Var(id))) => {
                    Err(UnifError::UnboundTypeVariable(id))
                }
                (TypeWrapper::Concrete(r), _) | (_, TypeWrapper::Concrete(r))
                    if !r.is_row_type() =>
                {
                    Err(UnifError::IllformedType(mk_tyw_record!(; r)))
                }
                (tyw1, tyw2) => unify(state, tyw1, tyw2),
            },
            (AbsType::DynRecord(t), AbsType::DynRecord(t2)) => unify(state, *t, *t2),
            (AbsType::Forall(i1, t1t), AbsType::Forall(i2, t2t)) => {
                // Very stupid (slow) implementation
                let constant_type = state.table.fresh_const();

                unify(
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
        (TypeWrapper::Ptr(p1), TypeWrapper::Ptr(p2)) if p1 == p2 => Ok(()),
        // The two following cases are not merged just to correctly distinguish between the
        // expected type (first component of the tuple) and the inferred type when reporting a row
        // unification error.
        (TypeWrapper::Ptr(p), tyw) => {
            constr_unify(state.constr, p, &tyw)
                .map_err(|err| err.into_unif_err(TypeWrapper::Ptr(p), tyw.clone()))?;
            state.table.assign(p, tyw);
            Ok(())
        }
        (tyw, TypeWrapper::Ptr(p)) => {
            constr_unify(state.constr, p, &tyw)
                .map_err(|err| err.into_unif_err(tyw.clone(), TypeWrapper::Ptr(p)))?;
            state.table.assign(p, tyw);
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

/// Try to unify two row types. Return an [`RowUnifError::IllformedRow`] error if one of the given
/// type is not a row type.
pub fn unify_rows(
    state: &mut State,
    t1: AbsType<Box<TypeWrapper>>,
    t2: AbsType<Box<TypeWrapper>>,
) -> Result<(), RowUnifError> {
    match (t1, t2) {
        (AbsType::Var(id), _) | (_, AbsType::Var(id)) => Err(RowUnifError::UnboundTypeVariable(id)),
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
            let (ty2, t2_tail) = row_add(state, &id, ty.clone(), TypeWrapper::Concrete(r2))?;
            match (ty, ty2) {
                (None, None) => Ok(()),
                (Some(ty), Some(ty2)) => unify(state, *ty, *ty2)
                    .map_err(|err| RowUnifError::RowMismatch(id.clone(), Box::new(err))),
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
                (t1_tail, t2_tail) => unify(state, t1_tail, t2_tail).map_err(|err| match err {
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

/// Extract the concrete type corresponding to a type wrapper. Free unification variables as well
/// as type constants are replaced with the type `Dyn`.
fn to_type(table: &UnifTable, ty: TypeWrapper) -> Types {
    match ty {
        TypeWrapper::Ptr(p) => match table.root(p) {
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

/// Type of the parameter controlling instantiation of foralls.
///
/// See [`instantiate_foralls`].
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
        ty = state.table.root(p);
    }

    while let TypeWrapper::Concrete(AbsType::Forall(id, forall_ty)) = ty {
        let fresh_id = state.table.fresh_var();
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

/// The unification table.
///
/// Map each unification variable to either another type variable or a concrete type it has been
/// unified with. Each binding `(ty, var)` in this map should be thought of an edge in a
/// unification graph.
pub struct UnifTable(Vec<Option<TypeWrapper>>);

impl UnifTable {
    pub fn new() -> Self {
        UnifTable(Vec::new())
    }

    /// Assign a type to a unification variable.
    pub fn assign(&mut self, var: usize, tyw: TypeWrapper) {
        debug_assert!(self.0[var].is_none());
        self.0[var] = Some(tyw);
    }

    /// Retrieve the current assignement of a unification variable.
    pub fn get(&self, var: usize) -> Option<&TypeWrapper> {
        self.0[var].as_ref()
    }

    /// Create a fresh variable identifier and allocate a corresponding slot in the table.
    fn fresh_var(&mut self) -> usize {
        let next = self.0.len();
        self.0.push(None);
        next
    }

    /// Create a fresh unification variable and allocate a corresponding slot in the table.
    pub fn fresh_unif_var(&mut self) -> TypeWrapper {
        TypeWrapper::Ptr(self.fresh_var())
    }

    /// Create a fresh type constant.
    pub fn fresh_const(&mut self) -> TypeWrapper {
        TypeWrapper::Constant(self.fresh_var())
    }

    /// Follow the links in the unification table to find the representative of the equivalence class
    /// of unification variable `x`.
    ///
    /// This corresponds to the find in union-find.
    // TODO This should be a union find like algorithm
    pub fn root(&self, x: usize) -> TypeWrapper {
        // All queried variable must have been introduced by `new_var` and thus a corresponding entry
        // must always exist in `state`. If not, the typechecking algorithm is not correct, and we
        // panic.
        match &self.0[x] {
            None => TypeWrapper::Ptr(x),
            Some(TypeWrapper::Ptr(y)) => self.root(*y),
            Some(ty @ TypeWrapper::Concrete(_)) => ty.clone(),
            Some(k @ TypeWrapper::Constant(_)) => k.clone(),
        }
    }
}

/// Row constraints.
///
/// A row constraint applies to a unification variable appearing inside a row type (such as `r` in
/// `{ someId: SomeType | r }`). It is a set of identifiers that said row must NOT contain, to
/// forbid ill-formed types with multiple declaration of the same id, for example `{ a: Num, a:
/// String}`.
pub type RowConstr = HashMap<usize, HashSet<Ident>>;

/// Add a row constraint on a type.
///
/// See [`RowConstr`].
fn constraint(state: &mut State, x: TypeWrapper, id: Ident) -> Result<(), RowUnifError> {
    match x {
        TypeWrapper::Ptr(p) => match state.table.root(p) {
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
/// - `state.table.root_of(p) == p`
/// - `state.constr.get(&p) == None`
fn constrain_var(state: &mut State, tyw: &TypeWrapper, p: usize) {
    fn constrain_var_(state: &mut State, mut constr: HashSet<Ident>, tyw: &TypeWrapper, p: usize) {
        match tyw {
            TypeWrapper::Ptr(u) if p == *u && !constr.is_empty() => {
                state.constr.insert(p, constr);
            }
            TypeWrapper::Ptr(u) => match state.table.root(*u) {
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
                | AbsType::Wildcard(_) => (),
                AbsType::Array(tyw) => constrain_var_(state, HashSet::new(), tyw.as_ref(), p),
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
                    if let Some(u_constr) = constr.get_mut(u) {
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

/// Get the typevar associated with a given wildcard ID.
fn get_wildcard_var(
    table: &mut UnifTable,
    wildcard_vars: &mut Vec<TypeWrapper>,
    id: usize,
) -> TypeWrapper {
    // If `id` is not in `wildcard_vars`, populate it with fresh vars up to `id`
    if id >= wildcard_vars.len() {
        wildcard_vars.extend((wildcard_vars.len()..=id).map(|_| table.fresh_unif_var()));
    }
    wildcard_vars[id].clone()
}

/// Convert a mapping from wildcard ID to type var, into a mapping from wildcard ID to concrete type.
fn wildcard_vars_to_type(wildcard_vars: Vec<TypeWrapper>, table: &UnifTable) -> Wildcards {
    wildcard_vars
        .into_iter()
        .map(|var| to_type(table, var))
        .collect()
}
