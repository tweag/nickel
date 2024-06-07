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
//!
//! We first test for physical equality (both as an optimization and to detect two variables
//! pointing to the same contract definition in the AST). If the comparison fails, we do a simple
//! structural recursion, unfolding simple forms and following variables with a limited number of
//! times. For anything more complex, we return false.

use super::*;
use crate::{
    eval::{
        self,
        cache::lazy::Thunk,
        cache::{Cache, CacheIndex},
    },
    identifier::LocIdent,
    term::{self, record::Field, IndexMap, UnaryOp},
};

use std::fmt::Debug;

/// The maximal number of variable links we want to unfold before abandoning the check. It should
/// stay low, but has been fixed arbitrarily: feel fee to increase reasonably if it turns out
/// legitimate type equalities between simple contracts are unduly rejected in practice.
pub const MAX_GAS: u8 = 12;

/// Abstract over the term environment, which is represented differently in the typechecker and
/// during evaluation.
///
/// The evaluation environment holds [crate::eval::cache::CacheIndex]es, while the term environment
/// used during typechecking just maps identifiers to a pair `(RichTerm, Environment)`. To have an
/// interface that works with both, [TermEnvironment] provides [TermEnvironment::get_then], which
/// has to take a closure representing the continuation of the task to do with the result instead
/// of merely returning it.
pub trait TermEnvironment: Clone {
    fn get_then<F, T>(env: &Self, id: Ident, f: F) -> T
    where
        F: FnOnce(Option<(&RichTerm, &Self)>) -> T;

    /// Cheap check that two environment are physically equal. This check is used to avoid doing
    /// extra work and usually boils down to pointer equality.
    fn fast_eq(_this: &Self, _that: &Self) -> bool;

    /// When comparing closure, we don't get an identifier, but a cache index (a thunk).
    fn get_idx_then<F, T>(env: &Self, idx: &CacheIndex, f: F) -> T
    where
        F: FnOnce(Option<(&RichTerm, &Self)>) -> T;
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

impl TermEnvironment for SimpleTermEnvironment {
    fn get_then<F, T>(env: &Self, id: Ident, f: F) -> T
    where
        F: FnOnce(Option<(&RichTerm, &SimpleTermEnvironment)>) -> T,
    {
        // See https://github.com/rust-lang/rust-clippy/issues/11764. It's been fixed upstream, but
        // hasn't landed in a clippy release yet. We can remove the allow(clippy) once it does
        // land.
        #[allow(clippy::map_identity)]
        f(env.0.get(&id).map(|(rt, env)| (rt, env)))
    }

    fn get_idx_then<F, T>(_env: &Self, _idx: &CacheIndex, f: F) -> T
    where
        F: FnOnce(Option<(&RichTerm, &SimpleTermEnvironment)>) -> T,
    {
        debug_assert!(
            false,
            "we shouldn't see closure when computing contract equality at typechecking time"
        );
        f(None)
    }

    fn fast_eq(this: &Self, that: &Self) -> bool {
        GenericEnvironment::ptr_eq(&this.0, &that.0)
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

impl TermEnvironment for eval::Environment {
    fn get_then<F, T>(env: &eval::Environment, id: Ident, f: F) -> T
    where
        F: FnOnce(Option<(&RichTerm, &eval::Environment)>) -> T,
    {
        match env.get(&id).map(eval::cache::lazy::Thunk::borrow) {
            Some(closure_ref) => f(Some((&closure_ref.body, &closure_ref.env))),
            None => f(None),
        }
    }

    fn get_idx_then<F, T>(_env: &eval::Environment, idx: &CacheIndex, f: F) -> T
    where
        F: FnOnce(Option<(&RichTerm, &eval::Environment)>) -> T,
    {
        let closure_ref = idx.borrow_orig();

        f(Some((&closure_ref.body, &closure_ref.env)))
    }

    fn fast_eq(this: &Self, that: &Self) -> bool {
        Self::ptr_eq(this, that)
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

/// Compute equality between two contracts.
///
/// # Parameters
///
/// - `env`: an environment mapping variables to their definition (the second placeholder in a
///   `let _ = _ in _`)
pub fn contract_eq<E: TermEnvironment>(
    var_uid: usize,
    t1: &RichTerm,
    env1: &E,
    t2: &RichTerm,
    env2: &E,
) -> bool {
    contract_eq_bounded(&mut State::new(var_uid), t1, env1, t2, env2)
}

/// **Warning**: this function isn't computing a sound contract equality (it could equate contracts
/// that aren't actually the same). It is used to deduplicate type and contract annotations for
/// pretty-printing, where there is no notion of environment and the only thing that matters is
/// that they are printed the same or not.
///
/// Compute equality between two contracts in an empty environment. This means that two variables
/// with the same name are considered equal.
pub fn type_eq_noenv(var_uid: usize, t1: &Type, t2: &Type) -> bool {
    let empty = eval::Environment::new();

    type_eq_bounded(
        &mut State::new(var_uid),
        &GenericUnifType::from_type(t1.clone(), &empty),
        &empty,
        &GenericUnifType::from_type(t2.clone(), &empty),
        &empty,
    )
}

/// Decide type equality on contracts in their respective environment and given the remaining gas
/// in `state`.
fn contract_eq_bounded<E: TermEnvironment>(
    state: &mut State,
    t1: &RichTerm,
    env1: &E,
    t2: &RichTerm,
    env2: &E,
) -> bool {
    use Term::*;

    // Test for physical equality as both an optimization and a way to cheaply equate complex
    // contracts that happen to point to the same definition (while the purposely limited
    // structural checks below may reject the equality)
    if term::SharedTerm::ptr_eq(&t1.term, &t2.term) && E::fast_eq(env1, env2) {
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
            key1 == key2 && contract_eq_bounded(state, inner1, env1, inner2, env2)
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
            contract_eq_bounded(state, head1, env1, head2, env2)
                && contract_eq_bounded(state, arg1, env1, arg2, env2)
        }
        (Closure(id1), Closure(id2)) if Thunk::ptr_eq(id1, id2) => true,
        // All variables must be bound at this stage. This is checked by the typechecker when
        // walking annotations. However, we may assume that `env` is a local environment (that it
        // doesn't include the stdlib). In that case, free variables (unbound) may be deemed equal
        // if they have the same identifier: whatever global environment the term will be put in,
        // free variables are not redefined locally and will be bound to the same value in any case.
        (Var(id1), Var(id2)) => {
            <E as TermEnvironment>::get_then(env1, id1.ident(), |binding1| {
                <E as TermEnvironment>::get_then(env2, id2.ident(), |binding2| {
                    match (binding1, binding2) {
                        (Some((t1, env1)), Some((t2, env2))) => {
                            // We may end up using one more gas unit if gas was exactly 1. That is
                            // not very important, and it's simpler to just ignore this case. We
                            // still return false if gas was already at zero.
                            let had_gas = state.use_gas();
                            state.use_gas();
                            had_gas && contract_eq_bounded(state, t1, env1, t2, env2)
                        }
                        (None, None) => id1 == id2,
                        _ => false,
                    }
                })
            })
        }
        (Closure(idx1), Closure(idx2)) => {
            <E as TermEnvironment>::get_idx_then(env1, idx1, |binding1| {
                <E as TermEnvironment>::get_idx_then(env2, idx2, |binding2| {
                    match (binding1, binding2) {
                        (Some((t1, env1)), Some((t2, env2))) => {
                            // We may end up using one more gas unit if gas was exactly 1. That is
                            // not very important, and it's simpler to just ignore this case. We
                            // still return false if gas was already at zero.
                            let had_gas = state.use_gas();
                            state.use_gas();
                            contract_eq_bounded(state, t1, env1, t2, env2) && had_gas
                        }
                        _ => false,
                    }
                })
            })
        }
        (Var(id), _) => {
            state.use_gas()
                && <E as TermEnvironment>::get_then(env1, id.ident(), |binding| {
                    binding
                        .map(|(t1, env1)| contract_eq_bounded(state, t1, env1, t2, env2))
                        .unwrap_or(false)
                })
        }
        (_, Var(id)) => {
            state.use_gas()
                && <E as TermEnvironment>::get_then(env2, id.ident(), |binding| {
                    binding
                        .map(|(t2, env2)| contract_eq_bounded(state, t1, env1, t2, env2))
                        .unwrap_or(false)
                })
        }
        (Closure(idx), _) => {
            state.use_gas()
                && <E as TermEnvironment>::get_idx_then(env1, idx, |binding| {
                    binding
                        .map(|(t1, env1)| contract_eq_bounded(state, t1, env1, t2, env2))
                        .unwrap_or(false)
                })
        }
        (_, Closure(idx)) => {
            state.use_gas()
                && <E as TermEnvironment>::get_idx_then(env2, idx, |binding| {
                    binding
                        .map(|(t2, env2)| contract_eq_bounded(state, t1, env1, t2, env2))
                        .unwrap_or(false)
                })
        }
        (Record(r1), Record(r2)) => {
            map_eq(
                contract_eq_fields,
                state,
                &r1.fields,
                env1,
                &r2.fields,
                env2,
            ) && r1.attrs.open == r2.attrs.open
        }
        (RecRecord(r1, dyn_fields1, _), RecRecord(r2, dyn_fields2, _)) =>
        // We only compare records whose field structure is statically known (i.e. without dynamic
        // fields).
        {
            dyn_fields1.is_empty()
                && dyn_fields2.is_empty()
                && map_eq(
                    contract_eq_fields,
                    state,
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
                    .all(|(t1, t2)| contract_eq_bounded(state, t1, env1, t2, env2))
                // Ideally we would compare pending contracts, but it's a bit advanced and for now
                // we only equate arrays without additional contracts
                && attrs1.pending_contracts.is_empty() && attrs2.pending_contracts.is_empty()
        }
        // We must compare the inner values as well as the corresponding contracts or type
        // annotations.
        (Annotated(annot1, t1), Annotated(annot2, t2)) => {
            let value_eq = contract_eq_bounded(state, t1, env1, t2, env2);

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
                    &GenericUnifType::from_type(ctr1.typ.clone(), env1),
                    env1,
                    &GenericUnifType::from_type(ctr2.typ.clone(), env2),
                    env2,
                ),
                _ => false,
            };

            value_eq && ty_eq
        }
        (Op1(UnaryOp::RecordAccess(id1), t1), Op1(UnaryOp::RecordAccess(id2), t2)) => {
            id1 == id2 && contract_eq_bounded(state, t1, env1, t2, env2)
        }
        (Type(ty1), Type(ty2)) => type_eq_bounded(
            state,
            &GenericUnifType::from_type(ty1.clone(), env1),
            env1,
            &GenericUnifType::from_type(ty2.clone(), env2),
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
    map1: &IndexMap<LocIdent, V>,
    env1: &E,
    map2: &IndexMap<LocIdent, V>,
    env2: &E,
) -> bool
where
    F: FnMut(&mut State, &V, &E, &V, &E) -> bool,
{
    map1.len() == map2.len()
        && map1.iter().all(|(id, v1)| {
            map2.get(id)
                .map(|v2| f(state, v1, env1, v2, env2))
                .unwrap_or(false)
        })
}

/// Convert record rows to a hashmap.
///
/// Require the rows to be closed (i.e. the last element must be `RowEmpty`), otherwise `None` is
/// returned. `None` is returned as well if a type encountered is not row, or if it is a enum row.
fn rrows_as_map<E: TermEnvironment>(
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

/// Convert enum rows to a hashmap.
///
/// Require the rows to be closed (i.e. the last element must be `RowEmpty`), otherwise `None` is
/// returned. `None` is returned as well if a type encountered is not row type, or if it is a
/// record row.
fn erows_as_map<E: TermEnvironment>(
    erows: &GenericUnifEnumRows<E>,
) -> Option<IndexMap<LocIdent, Option<&GenericUnifType<E>>>> {
    let set: Option<IndexMap<LocIdent, Option<_>>> = erows
        .iter()
        .map(|item| match item {
            GenericUnifEnumRowsIteratorItem::Row(EnumRowF { id, typ: types }) => Some((id, types)),
            _ => None,
        })
        .collect();

    set
}

/// Check for contract equality between record fields. Fields are equal if they are both without a
/// definition, or are both defined and their values are equal.
fn contract_eq_fields<E: TermEnvironment>(
    state: &mut State,
    field1: &Field,
    env1: &E,
    field2: &Field,
    env2: &E,
) -> bool {
    match (&field1.value, &field2.value) {
        (Some(ref value1), Some(ref value2)) => {
            contract_eq_bounded(state, value1, env1, value2, env2)
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
    ty1: &GenericUnifType<E>,
    env1: &E,
    ty2: &GenericUnifType<E>,
    env2: &E,
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
                ) if attrs1 == attrs2 => type_eq_bounded(state, uty1, env1, uty2, env2),
                (TypeF::Array(uty1), TypeF::Array(uty2)) => {
                    type_eq_bounded(state, uty1, env1, uty2, env2)
                }
                (TypeF::Arrow(s1, t1), TypeF::Arrow(s2, t2)) => {
                    type_eq_bounded(state, s1, env1, s2, env2)
                        && type_eq_bounded(state, t1, env1, t2, env2)
                }
                (TypeF::Enum(uty1), TypeF::Enum(uty2)) => {
                    fn type_eq_bounded_wrapper<E: TermEnvironment>(
                        state: &mut State,
                        uty1: &Option<&GenericUnifType<E>>,
                        env1: &E,
                        uty2: &Option<&GenericUnifType<E>>,
                        env2: &E,
                    ) -> bool {
                        match (uty1, uty2) {
                            (Some(uty1), Some(uty2)) => {
                                type_eq_bounded(state, *uty1, env1, *uty2, env2)
                            }
                            (None, None) => true,
                            _ => false,
                        }
                    }

                    let map1 = erows_as_map(uty1);
                    let map2 = erows_as_map(uty2);

                    map1.zip(map2)
                        .map(|(m1, m2)| {
                            map_eq(type_eq_bounded_wrapper, state, &m1, env1, &m2, env2)
                        })
                        .unwrap_or(false)
                }
                (TypeF::Record(uty1), TypeF::Record(uty2)) => {
                    fn type_eq_bounded_wrapper<E: TermEnvironment>(
                        state: &mut State,
                        uty1: &&GenericUnifType<E>,
                        env1: &E,
                        uty2: &&GenericUnifType<E>,
                        env2: &E,
                    ) -> bool {
                        type_eq_bounded(state, *uty1, env1, *uty2, env2)
                    }

                    let map1 = rrows_as_map(uty1);
                    let map2 = rrows_as_map(uty2);

                    map1.zip(map2)
                        .map(|(m1, m2)| {
                            map_eq(type_eq_bounded_wrapper, state, &m1, env1, &m2, env2)
                        })
                        .unwrap_or(false)
                }
                (TypeF::Flat(t1), TypeF::Flat(t2)) => {
                    contract_eq_bounded(state, t1, env1, t2, env2)
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
                        VarKind::EnumRows { .. } => (
                            body1.subst(var1, &GenericUnifEnumRows::Constant(cst_id)),
                            body2.subst(var2, &GenericUnifEnumRows::Constant(cst_id)),
                        ),
                    };

                    type_eq_bounded(state, &uty1_subst, env1, &uty2_subst, env2)
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
        (GenericUnifType::Contract(t1, env2), GenericUnifType::Contract(t2, env1)) => {
            contract_eq_bounded(state, t1, env1, t2, env2)
        }
        _ => false,
    }
}
