//! Computation of type equality for contracts (flat types).
//!
//! Determine if two contracts are equal as opaque types. Used to decide if two contract should
//! unify.
//!
//! ## Aliases
//!
//! One basic case we want to handle is aliases, which come in handy for parametrized contracts. We
//! want to equate `Alias` with e.g. `Foo "bar" "baz"` if `Alias` has been defined as
//! `let Alias = Foo "bar" "baz" in ...`.
//!
//! We also want to equate different aliases with the same definition: `Alias type_eq Alias'` if
//! `let Alias' = Foo "bar" "baz" in ...`, or `let Alias' = Alias in ...`.
//!
//! ## Recursion
//!
//! We must refrain from following all variables links blindly, as there could be cycles in the
//! graph leading to an infinite loop:
//!
//! ```nickel
//! {
//!   Foo = Bar,
//!   Bar = Foo,
//! }
//! ```
//!
//! Because we just follows variables, and don't apply functions, we can detect cycles while
//! walking the graph. Still, as it is potentially performed many times during typechecking, type
//! equality ought to stay reasonably cheap. We choose to just set an arbitrary limit (the gas) on
//! the number of variable links that the type equality may follow. Doing so, we don't have to
//! worry about loops anymore.
//!
//! ## Equality on terms
//!
//! The terms inside a type may be arbitrarily complex. Primops applications, `match`, and the
//! like are quite unlikely to appear inside an annotation (they surely appear inside contract
//! definitions). We don't want to compare functions syntactically either. The spirit of this
//! implementation is to equate aliases or simple constructs that may appear inlined inside an
//! annotation (applications, records, primitive constants and arrays, mostly) in a structural way.
//! We first test for physical equality (both as an optimization and to detect two variables
//! pointing to the same contract definition in the AST). If the comparison fails, we do a simple
//! structural recursion, unfolding simple forms and following variables with a limited number of
//! times. For anything more complex, we return false.

use super::*;
use crate::{
    eval::{self, cache::Cache},
    identifier::LocIdent,
    term::{self, record::Field, IndexMap, UnaryOp},
};

use std::fmt::Debug;

/// The maximal number of variable links we want to unfold before abandoning the check. It should
/// stay low, but has been fixed arbitrarily: feel fee to increase reasonably if it turns out
/// legitimate type equalities between simple contracts are unduly rejected in practice.
pub const MAX_GAS: u8 = 10;

/// Like `std::borrow::ToOwned`, but defined as a local trait for convenience and to be implemented
/// on the reference type directly. Beside the slightly different signature, the blanket
/// implementations of `ToOwned` would interfere with our own.
pub trait ToOwnedEnv<Owned> {
    fn to_owned_env(self) -> Owned;
}

/// Abstract over the term environment, which is represented differently in the typechecker and
/// during evaluation.
///
/// The evaluation environment holds [crate::eval::cache::CacheIndex]es, while the term environment
/// used during typechecking just maps identifiers to a pair `(RichTerm, Environment)`. To have an
/// interface that works with both, [TermEnvironment] provides [TermEnvironment::get_then], which
/// has to take a closure representing the continuation of the task to do with the result instead
/// of merely returning it.
pub trait TermEnvironment: Clone {
    /// The representation of the environment is different during typechecking and evaluation. In
    /// particular, the evaluation's environment is actually a pair of environments (the initial
    /// environment and the local environment), and thus the notion of owned environment and
    /// reference to the environment don't always map to the obvious `Self` and `&Self`
    /// respectively.
    /// The trait abstract over thos notions by defining a reference type and an owned type.
    type Owned: Clone + PartialEq + Debug;
    type Ref<'a>: Copy + ToOwnedEnv<Self::Owned>;

    fn get_then<F, T>(env: Self::Ref<'_>, id: Ident, f: F) -> T
    where
        F: FnOnce(Option<(&RichTerm, Self::Ref<'_>)>) -> T;
}

/// A simple term environment, as a mapping from identifiers to a tuple of a term and an
/// environment (i.e. a closure), sufficient for the needs of typechecking.
#[derive(PartialEq, Clone, Debug)]
pub struct SimpleTermEnvironment(pub GenericEnvironment<Ident, (RichTerm, SimpleTermEnvironment)>);

impl SimpleTermEnvironment {
    pub fn new() -> Self {
        SimpleTermEnvironment(GenericEnvironment::new())
    }
}

impl Default for SimpleTermEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, E: Clone> ToOwnedEnv<E> for &'a E {
    fn to_owned_env(self) -> E {
        self.clone()
    }
}

impl TermEnvironment for SimpleTermEnvironment {
    type Owned = Self;
    type Ref<'a> = &'a Self;

    fn get_then<F, T>(env: &Self, id: Ident, f: F) -> T
    where
        F: FnOnce(Option<(&RichTerm, &SimpleTermEnvironment)>) -> T,
    {
        f(env.0.get(&id).map(|(rt, env)| (rt, env)))
    }
}

impl<'a> AsRef<SimpleTermEnvironment> for &'a SimpleTermEnvironment {
    fn as_ref(&self) -> Self {
        self
    }
}

impl std::iter::FromIterator<(Ident, (RichTerm, SimpleTermEnvironment))> for SimpleTermEnvironment {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (Ident, (RichTerm, SimpleTermEnvironment))>,
    {
        SimpleTermEnvironment(
            GenericEnvironment::<Ident, (RichTerm, SimpleTermEnvironment)>::from_iter(iter),
        )
    }
}

#[derive(Clone, Copy)]
pub struct EvalEnvsRef<'a> {
    pub eval_env: &'a eval::Environment,
    pub initial_env: &'a eval::Environment,
}

#[derive(Clone, PartialEq, Debug)]
pub struct EvalEnvsOwned {
    pub eval_env: eval::Environment,
    pub initial_env: eval::Environment,
}

impl<'a> ToOwnedEnv<EvalEnvsOwned> for EvalEnvsRef<'a> {
    fn to_owned_env(self) -> EvalEnvsOwned {
        EvalEnvsOwned {
            eval_env: self.eval_env.clone(),
            initial_env: self.initial_env.clone(),
        }
    }
}

impl<'a> TermEnvironment for EvalEnvsRef<'a> {
    type Ref<'b> = EvalEnvsRef<'b>;
    type Owned = EvalEnvsOwned;

    fn get_then<F, T>(env: EvalEnvsRef<'_>, id: Ident, f: F) -> T
    where
        F: FnOnce(Option<(&RichTerm, EvalEnvsRef<'_>)>) -> T,
    {
        debug_assert!(
            env.eval_env.get(&id).or(env.initial_env.get(&id)).is_some(),
            "unbound variable `{}`",
            id
        );

        match env
            .eval_env
            .get(&id)
            .or(env.initial_env.get(&id))
            .map(eval::cache::lazy::Thunk::borrow)
        {
            Some(closure_ref) => f(Some((
                &closure_ref.body,
                EvalEnvsRef {
                    eval_env: &closure_ref.env,
                    initial_env: env.initial_env,
                },
            ))),
            None => f(None),
        }
    }
}

pub trait FromEnv<C: Cache> {
    fn from_env(eval_env: eval::Environment, cache: &C) -> Self;
}

impl<C: Cache> FromEnv<C> for SimpleTermEnvironment {
    fn from_env(eval_env: eval::Environment, cache: &C) -> Self {
        let generic_env: GenericEnvironment<_, _> = eval_env
            .iter_elems()
            .map(|(id, idx)| {
                let borrowed = cache.get_then(idx.clone(), |c| {
                    (c.body.clone(), Self::from_env(c.env.clone(), cache))
                });
                (*id, borrowed)
            })
            .collect();
        SimpleTermEnvironment(generic_env)
    }
}

/// Dummy environment used in conjunction with [VarEq::Direct] to compare variables by name.
///
/// **Warning**: calling `get_then` on this environment will panic, as it has no way to generate a
/// term from a variable. It must only be used in conjunction with [VarEq::Direct], which doesn't
/// access the environment.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct NoEnvironment;

impl TermEnvironment for NoEnvironment {
    type Owned = Self;
    type Ref<'a> = &'a Self;

    fn get_then<F, T>(_env: &Self, id: Ident, _f: F) -> T
    where
        F: FnOnce(Option<(&RichTerm, &NoEnvironment)>) -> T,
    {
        panic!("contract equality checking: cannot get variable `{id}` from no environment")
    }
}

/// State threaded through the type equality computation.
#[derive(Copy, Clone, Default)]
struct State {
    /// Used to generate temporary rigid type variables for substituting type variables when
    /// comparing foralls. Those ids never escape the type equality computations and are used
    /// solely as rigid type variables: this is why they don't need proper allocation in the
    /// unification table or to care about those ids clashing with the one generated by the
    /// typechecker. Generated type constants simply needs to be unique for the duration of the
    /// type equality computation.
    var_uid: usize,

    /// The current gas remaining for variable substitutions. Once it reaches zero and we encounter
    /// a variable, we abort the computation and return false.
    gas: u8,
}

impl State {
    fn new(var_uid: usize) -> Self {
        State {
            var_uid,
            gas: MAX_GAS,
        }
    }

    /// Create a fresh unique id for a rigid type variable.
    fn fresh_cst_id(&mut self) -> VarId {
        let result = self.var_uid;
        self.var_uid += 1;
        result
    }

    /// Try to consume one unit of gas for a variable substitution. Return true in case of success,
    /// or false if the gas was already at zero.
    fn use_gas(&mut self) -> bool {
        if self.gas == 0 {
            false
        } else {
            self.gas -= 1;
            true
        }
    }
}

/// Different possible behavior for variables comparison.
#[derive(Copy, Clone, PartialEq, Eq, Default)]
pub enum VarEq {
    #[default]
    /// Use the environment to fetch the content of variables and compare them.
    Environment,
    /// Don't use an environment and simply compare variables by name. This is unsound, and should
    /// only be used to deduplicate annotations for pretty-printing. In this case, use
    /// [NoEnvironment] as an environment.
    Direct,
}

/// Compute equality between two contracts.
///
/// # Parameters
///
/// - `env`: an environment mapping variables to their definition (the second placeholder in a
///   `let _ = _ in _`)
pub fn contract_eq<E: TermEnvironment>(
    var_uid: usize,
    t1: &RichTerm,
    env1: E::Ref<'_>,
    t2: &RichTerm,
    env2: E::Ref<'_>,
) -> bool {
    contract_eq_bounded::<E>(
        &mut State::new(var_uid),
        VarEq::Environment,
        t1,
        env1,
        t2,
        env2,
    )
}

/// **Warning**: this function isn't computing a sound contract equality (it could equate contracts
/// that aren't actually the same). It is used to deduplicate type and contract annotations for
/// pretty-printing, where there is no notion of environment and the only thing that matters is
/// that they are printed the same or not.
///
/// Compute equality between two contracts, considering that two variables with the same name are
/// equal.
pub fn type_eq_noenv(var_uid: usize, t1: &Type, t2: &Type) -> bool {
    type_eq_bounded::<NoEnvironment>(
        &mut State::new(var_uid),
        VarEq::Direct,
        &GenericUnifType::<NoEnvironment>::from_type(t1.clone(), &NoEnvironment),
        &NoEnvironment,
        &GenericUnifType::<NoEnvironment>::from_type(t2.clone(), &NoEnvironment),
        &NoEnvironment,
    )
}

/// Decide type equality on contracts in their respective environment and given the remaining gas
/// in `state`.
fn contract_eq_bounded<E: TermEnvironment>(
    state: &mut State,
    var_eq: VarEq,
    t1: &RichTerm,
    env1: E::Ref<'_>,
    t2: &RichTerm,
    env2: E::Ref<'_>,
) -> bool {
    use Term::*;

    // Test for physical equality as both an optimization and a way to cheaply equate complex
    // contracts that happen to point to the same definition (while the purposely limited
    // structural checks below may reject the equality)
    if term::SharedTerm::ptr_eq(&t1.term, &t2.term) {
        return true;
    }

    match (t1.as_ref(), t2.as_ref()) {
        (Null, Null) => true,
        (Bool(b1), Bool(b2)) => b1 == b2,
        (Num(n1), Num(n2)) => n1 == n2,
        (Str(s1), Str(s2)) => s1 == s2,
        (Enum(id1), Enum(id2)) => id1 == id2,
        (SealingKey(s1), SealingKey(s2)) => s1 == s2,
        (Sealed(key1, inner1, _), Sealed(key2, inner2, _)) => {
            key1 == key2 && contract_eq_bounded::<E>(state, var_eq, inner1, env1, inner2, env2)
        }
        // We only compare string chunks when they represent a plain string (they don't contain any
        // interpolated expression), as static string may be currently parsed as such. We return
        // false for anything more complex.
        (StrChunks(scs1), StrChunks(scs2)) => {
            scs1.len() == scs2.len()
                && scs1
                    .iter()
                    .zip(scs2.iter())
                    .all(|(chunk1, chunk2)| match (chunk1, chunk2) {
                        (StrChunk::Literal(s1), StrChunk::Literal(s2)) => s1 == s2,
                        _ => false,
                    })
        }
        (App(head1, arg1), App(head2, arg2)) => {
            contract_eq_bounded::<E>(state, var_eq, head1, env1, head2, env2)
                && contract_eq_bounded::<E>(state, var_eq, arg1, env1, arg2, env2)
        }
        (Var(id1), Var(id2)) if var_eq == VarEq::Direct => id1 == id2,
        // All variables must be bound at this stage. This is checked by the typechecker when
        // walking annotations. However, we may assume that `env` is a local environment (that it
        // doesn't include the stdlib). In that case, free variables (unbound) may be deemed equal
        // if they have the same identifier: whatever global environment the term will be put in,
        // free variables are not redefined locally and will be bound to the same value in any case.
        (Var(id1), Var(id2)) => {
            <E as TermEnvironment>::get_then(env1, id1.ident(), |binding1| {
                <E as TermEnvironment>::get_then(env2, id2.ident(), |binding2| {
                    match (binding1, binding2) {
                        (None, None) => id1 == id2,
                        (Some((t1, env1)), Some((t2, env2))) => {
                            // We may end up using one more gas unit if gas was exactly 1. That is
                            // not very important, and it's simpler to just ignore this case. We
                            // still return false if gas was already at zero.
                            let had_gas = state.use_gas();
                            state.use_gas();
                            had_gas && contract_eq_bounded::<E>(state, var_eq, t1, env1, t2, env2)
                        }
                        _ => false,
                    }
                })
            })
        }
        // If one term is a variable and not the other, and we don't have access to an environment, the
        // term are considered unequal
        (Var(_), _) | (_, Var(_)) if var_eq == VarEq::Direct => false,
        (Var(id), _) => {
            state.use_gas()
                && <E as TermEnvironment>::get_then(env1, id.ident(), |binding| {
                    binding
                        .map(|(t1, env1)| {
                            contract_eq_bounded::<E>(state, var_eq, t1, env1, t2, env2)
                        })
                        .unwrap_or(false)
                })
        }
        (_, Var(id)) => {
            state.use_gas()
                && <E as TermEnvironment>::get_then(env2, id.ident(), |binding| {
                    binding
                        .map(|(t2, env2)| {
                            contract_eq_bounded::<E>(state, var_eq, t1, env1, t2, env2)
                        })
                        .unwrap_or(false)
                })
        }
        (Record(r1), Record(r2)) => {
            map_eq::<_, _, E>(
                contract_eq_fields::<E>,
                state,
                var_eq,
                &r1.fields,
                env1,
                &r2.fields,
                env2,
            ) && r1.attrs == r2.attrs
        }
        (RecRecord(r1, dyn_fields1, _), RecRecord(r2, dyn_fields2, _)) =>
        // We only compare records whose field structure is statically known (i.e. without dynamic
        // fields).
        {
            dyn_fields1.is_empty()
                && dyn_fields2.is_empty()
                && map_eq::<_, _, E>(
                    contract_eq_fields::<E>,
                    state,
                    var_eq,
                    &r1.fields,
                    env1,
                    &r2.fields,
                    env2,
                )
                && r1.attrs == r2.attrs
        }
        (Array(ts1, attrs1), Array(ts2, attrs2)) => {
            ts1.len() == ts2.len()
                && ts1
                    .iter()
                    .zip(ts2.iter())
                    .all(|(t1, t2)| contract_eq_bounded::<E>(state, var_eq, t1, env1, t2, env2))
                && attrs1 == attrs2
        }
        // We must compare the inner values as well as the corresponding contracts or type
        // annotations.
        (Annotated(annot1, t1), Annotated(annot2, t2)) => {
            let value_eq = contract_eq_bounded::<E>(state, var_eq, t1, env1, t2, env2);

            // TODO:
            // - does it really make sense to compare the annotations?
            // - does it even happen to have contracts having themselves type annotations?
            // - and in the latter case, should they be declared unequal because of that?
            //   The answer to the last question is probably yes, because contracts are
            //   fundamentally as powerful as function application, so they can change their
            //   argument.

            // We use the same logic as in the typechecker: the type associated to an annotated
            // value is either the type annotation, or the first contract annotation.
            let ty1 = annot1.first();
            let ty2 = annot2.first();

            let ty_eq = match (ty1, ty2) {
                (None, None) => true,
                (Some(ctr1), Some(ctr2)) => type_eq_bounded(
                    state,
                    var_eq,
                    &GenericUnifType::<E>::from_type(ctr1.typ.clone(), env1),
                    env1,
                    &GenericUnifType::<E>::from_type(ctr2.typ.clone(), env2),
                    env2,
                ),
                _ => false,
            };

            value_eq && ty_eq
        }
        (Op1(UnaryOp::StaticAccess(id1), t1), Op1(UnaryOp::StaticAccess(id2), t2)) => {
            id1 == id2 && contract_eq_bounded::<E>(state, var_eq, t1, env1, t2, env2)
        }
        (Type(ty1), Type(ty2)) => type_eq_bounded(
            state,
            var_eq,
            &GenericUnifType::<E>::from_type(ty1.clone(), env1),
            env1,
            &GenericUnifType::<E>::from_type(ty2.clone(), env2),
            env2,
        ),
        // We don't treat imports, parse errors, nor pairs of terms that don't have the same shape
        _ => false,
    }
}

/// Compute the equality between two hashmaps holding either types or terms.
fn map_eq<V, F, E: TermEnvironment>(
    mut f: F,
    state: &mut State,
    var_eq: VarEq,
    map1: &IndexMap<LocIdent, V>,
    env1: E::Ref<'_>,
    map2: &IndexMap<LocIdent, V>,
    env2: E::Ref<'_>,
) -> bool
where
    F: FnMut(&mut State, VarEq, &V, E::Ref<'_>, &V, E::Ref<'_>) -> bool,
{
    map1.len() == map2.len()
        && map1.iter().all(|(id, v1)| {
            map2.get(id)
                .map(|v2| f(state, var_eq, v1, env1, v2, env2))
                .unwrap_or(false)
        })
}

/// Convert record rows to a hashmap.
///
/// Require the rows to be closed (i.e. the last element must be `RowEmpty`), otherwise `None` is
/// returned. `None` is returned as well if a type encountered is not row, or if it is a enum row.
fn rows_as_map<E: TermEnvironment>(
    erows: &GenericUnifRecordRows<E>,
) -> Option<IndexMap<LocIdent, &GenericUnifType<E>>> {
    let map: Option<IndexMap<LocIdent, _>> = erows
        .iter()
        .map(|item| match item {
            GenericUnifRecordRowsIteratorItem::Row(RecordRowF { id, typ: types }) => {
                Some((id, types))
            }
            _ => None,
        })
        .collect();

    map
}

/// Convert enum rows to a hashset.
///
/// Require the rows to be closed (i.e. the last element must be `RowEmpty`), otherwise `None` is
/// returned. `None` is returned as well if a type encountered is not row type, or if it is a
/// record row.
fn rows_as_set(erows: &UnifEnumRows) -> Option<HashSet<LocIdent>> {
    let set: Option<HashSet<_>> = erows
        .iter()
        .map(|item| match item {
            UnifEnumRowsIteratorItem::Row(id) => Some(*id),
            _ => None,
        })
        .collect();

    set
}

/// Check for contract equality between record fields. Fields are equal if they are both without a
/// definition, or are both defined and their values are equal.
fn contract_eq_fields<E: TermEnvironment>(
    state: &mut State,
    var_eq: VarEq,
    field1: &Field,
    env1: E::Ref<'_>,
    field2: &Field,
    env2: E::Ref<'_>,
) -> bool {
    match (&field1.value, &field2.value) {
        (Some(ref value1), Some(ref value2)) => {
            contract_eq_bounded::<E>(state, var_eq, value1, env1, value2, env2)
        }
        (None, None) => true,
        _ => false,
    }
}

/// Perform the type equality comparison on types. Structurally recurse into type constructors and
/// test that subtypes or subterms (contracts) are equals.
///
/// Currently, this function operates on `Type` rather than `TypeWrapper`s as it is called by
/// `contract_eq_bounded` on type annotations. But we need to substitute variables to correctly
/// compare `foralls`, hence it accepts more general `TypeWrapper`s. However, we expect to never
/// meet unification variables (we treat them for completeness and to be future proof), and that all
/// the rigid type variables encountered have been introduced by `type_eq_bounded` itself. This is
/// why we don't need unique identifiers that are distinct from the one used during typechecking,
/// and we can just start from `0`.
fn type_eq_bounded<E: TermEnvironment>(
    state: &mut State,
    var_eq: VarEq,
    ty1: &GenericUnifType<E>,
    env1: E::Ref<'_>,
    ty2: &GenericUnifType<E>,
    env2: E::Ref<'_>,
) -> bool {
    match (ty1, ty2) {
        (GenericUnifType::Concrete { typ: s1, .. }, GenericUnifType::Concrete { typ: s2, .. }) => {
            match (s1, s2) {
                (TypeF::Wildcard(id1), TypeF::Wildcard(id2)) => id1 == id2,
                (TypeF::Dyn, TypeF::Dyn)
                | (TypeF::Number, TypeF::Number)
                | (TypeF::Bool, TypeF::Bool)
                | (TypeF::Symbol, TypeF::Symbol)
                | (TypeF::String, TypeF::String) => true,
                (
                    TypeF::Dict {
                        type_fields: uty1,
                        flavour: attrs1,
                    },
                    TypeF::Dict {
                        type_fields: uty2,
                        flavour: attrs2,
                    },
                ) if attrs1 == attrs2 => type_eq_bounded(state, var_eq, uty1, env1, uty2, env2),
                (TypeF::Array(uty1), TypeF::Array(uty2)) => {
                    type_eq_bounded(state, var_eq, uty1, env1, uty2, env2)
                }
                (TypeF::Arrow(s1, t1), TypeF::Arrow(s2, t2)) => {
                    type_eq_bounded(state, var_eq, s1, env1, s2, env2)
                        && type_eq_bounded(state, var_eq, t1, env1, t2, env2)
                }
                (TypeF::Enum(uty1), TypeF::Enum(uty2)) => {
                    let rows1 = rows_as_set(uty1);
                    let rows2 = rows_as_set(uty2);
                    rows1.is_some() && rows2.is_some() && rows1 == rows2
                }
                (TypeF::Record(uty1), TypeF::Record(uty2)) => {
                    fn type_eq_bounded_wrapper<E: TermEnvironment>(
                        state: &mut State,
                        var_eq: VarEq,
                        uty1: &&GenericUnifType<E>,
                        env1: E::Ref<'_>,
                        uty2: &&GenericUnifType<E>,
                        env2: E::Ref<'_>,
                    ) -> bool {
                        type_eq_bounded(state, var_eq, *uty1, env1, *uty2, env2)
                    }

                    let map1 = rows_as_map(uty1);
                    let map2 = rows_as_map(uty2);

                    map1.zip(map2)
                        .map(|(m1, m2)| {
                            map_eq::<_, _, E>(
                                type_eq_bounded_wrapper,
                                state,
                                var_eq,
                                &m1,
                                env1,
                                &m2,
                                env2,
                            )
                        })
                        .unwrap_or(false)
                }
                (TypeF::Flat(t1), TypeF::Flat(t2)) => {
                    contract_eq_bounded::<E>(state, var_eq, t1, env1, t2, env2)
                }
                (
                    TypeF::Forall {
                        var: var1,
                        var_kind: var_kind1,
                        body: body1,
                    },
                    TypeF::Forall {
                        var: var2,
                        var_kind: var_kind2,
                        body: body2,
                    },
                ) => {
                    let cst_id = state.fresh_cst_id();

                    if var_kind1 != var_kind2 {
                        return false;
                    }

                    let body1 = body1.clone();
                    let body2 = body2.clone();

                    let (uty1_subst, uty2_subst) = match var_kind1 {
                        VarKind::Type => (
                            body1.subst(var1, &GenericUnifType::Constant(cst_id)),
                            body2.subst(var2, &GenericUnifType::Constant(cst_id)),
                        ),
                        VarKind::RecordRows { .. } => (
                            body1.subst(var1, &GenericUnifRecordRows::Constant(cst_id)),
                            body2.subst(var2, &GenericUnifRecordRows::Constant(cst_id)),
                        ),
                        VarKind::EnumRows => (
                            body1.subst(var1, &UnifEnumRows::Constant(cst_id)),
                            body2.subst(var2, &UnifEnumRows::Constant(cst_id)),
                        ),
                    };

                    type_eq_bounded(state, var_eq, &uty1_subst, env1, &uty2_subst, env2)
                }
                // We can't compare type variables without knowing what they are instantiated to,
                // and all type variables should have been substituted at this point, so we bail
                // out.
                _ => false,
            }
        }
        (GenericUnifType::UnifVar { id: id1, .. }, GenericUnifType::UnifVar { id: id2, .. }) => {
            debug_assert!(
                false,
                "we shouldn't come across unification variables during type equality computation"
            );
            id1 == id2
        }
        (GenericUnifType::Constant(i1), GenericUnifType::Constant(i2)) => i1 == i2,
        _ => false,
    }
}
