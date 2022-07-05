//! Computation of type equality.
//!
//! Determine if two types are equals. Used for unification. The interesting case is contracts
//! (flat types).
//!
//!
//! ## Type equality for contracts
//!
//! ### Aliases
//!
//! One basic case we want to handle is aliases, which come in handy for parametrized contracts. We
//! want to be able to equate `Alias` with e.g. `Foo "bar" "baz"` if `Alias` has been defined as
//! such: `let Alias = Foo "bar" "baz" in ...`.
//!
//! We also want to equate different aliases with the same definition: `Alias type_eq Alias'` if
//! `let Alias' = Foo "bar" "baz" in ...`, or `let Alias' = Alias in `.
//!
//! ### Recursion
//!
//! We don't want follow all variables links blindly, as it could be non terminating:
//!
//! ```nickel
//! {
//!   Foo = Bar,
//!   Bar = Foo,
//! }
//! ```
//!
//! Though because we just follows variables, and don't apply functions, we can detect cycles while walking the graph.
//!
//! But beside infinite loops, type equality ought to stay reasonably cheap. A pragmatic approach
//! is just to set an arbitrary limit (the gas) on the number of variable links that the type
//! equality may follow. Doing so, we won't have to worry about loops again.
//!
//! ### Equality on terms
//!
//! The terms inside a type may be arbitrarily complex. Primops applications, `switch`, etc. are
//! quite unlikely to appear inside an annotation (they may appear inside contract definitions). We
//! don't want to compare functions syntactically either. The spirit of this module is to to equate
//! aliases or simple constructs that may appear inlined inside an annotation (applications,
//! records, primitive constants and arrays,mostly) in a structural way. We first test for
//! physical equality (both as an optimization and to detect two variables pointing to the same
//! contract definition in the AST), and if the comparison fails, we do a simple structural
//! recursion, unfolding applications, records, and variables with a limited number of gas. For
//! anything more complex, we return false.

use super::*;

/// The maximal number of variable links we want to unfold before abandoning the check. It should
/// stay low, but has been fixed arbitrarily: feel fee to increase reasonably if it turns out
/// Legitimate type equalities between contracts are unduly rejected in practice.
pub const MAX_GAS: u8 = 8;

#[derive(PartialEq, Clone)]
pub struct ContractEnvironment<'a>(
    pub crate::environment::Environment<Ident, (&'a RichTerm, ContractEnvironment<'a>)>,
);

/// Try to consume one unit of gas for a variable substitution. Return true in case of success,
/// or false if the gas was already at zero.
pub fn use_gas(gas: &mut u8) -> bool {
    if *gas == 0 {
        false
    } else {
        *gas -= 1;
        true
    }
}

/// Compute the equality between two flat types (contracts).
///
/// # Params
///
/// - `env`: an environment mapping variables to their definition (the second placeholder in a
///   `let _ = _ in _`)
/// - the unique id of the last unification variable generated plus one. Used to generate unique
///   rigid type variables during the computation of the equality.
pub fn type_eq(t1: &RichTerm, t2: &RichTerm, env: ContractEnvironment) -> bool {
    contract_eq_bounded(t1, env.clone(), t2, env, &mut 0)
}

fn contract_eq_bounded(
    t1: &RichTerm,
    env1: ContractEnvironment,
    t2: &RichTerm,
    env2: ContractEnvironment,
    gas: &mut u8,
) -> bool {
    use Term::*;

    // Test for physical equality as both an optimization and a way to cheaply equate complex
    // contracts that happen to point to the same definition (while the purposely limited
    // structural checks below may reject the equality)
    if crate::term::SharedTerm::ptr_eq(&t1.term, &t2.term) {
        return true;
    }

    match (t1.as_ref(), t2.as_ref()) {
        (Null, Null) => true,
        (Bool(b1), Bool(b2)) => b1 == b2,
        (Num(n1), Num(n2)) => n1 == n2,
        (Str(s1), Str(s2)) => s1 == s2,
        (Enum(id1), Enum(id2)) => id1 == id2,
        (Sym(s1), Sym(s2)) => s1 == s2,
        (Wrapped(key1, inner1), Wrapped(key2, inner2)) => {
            key1 == key2 && contract_eq_bounded(inner1, env1, inner2, env2, gas)
        }
        // We only compare string chunks when they represent a plain string (they don't contain any
        // interpolated expression).
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
            contract_eq_bounded(head1, env1.clone(), head2, env2.clone(), gas)
                && contract_eq_bounded(arg1, env1, arg2, env2, gas)
        }
        // All variables must be bound at this stage. This is checked by the typechecker when
        // walking annotations. However, we may assume that `env` is a local environment (that it
        // doesn't include e.g. the standard library). In that case, free variables (unbound) may
        // be deemed equal if they have the same identifier: whatever global environment the term
        // will be put in, if they are not redefined locally, they have to be bound to the same
        // value.
        (Var(id1), Var(id2)) => match (env1.0.get(id1), env2.0.get(id2)) {
            (None, None) => id1 == id2,
            (Some((t1, env1)), Some((t2, env2))) => {
                // We may end up using one more gas unit if gas was exactly 1. That is not very
                // important, and it's simpler to just ignore this case. We still return false
                // if gas was already at zero.
                let had_gas = use_gas(gas);
                use_gas(gas);
                had_gas && contract_eq_bounded(t1, env1, t2, env2, gas)
            }
            _ => false,
        },
        (Var(id), _) => {
            use_gas(gas)
                && env1
                    .0
                    .get(id)
                    .map(|(t1, env1)| contract_eq_bounded(t1, env1, t2, env2, gas))
                    .unwrap_or(false)
        }
        (_, Var(id)) => {
            use_gas(gas)
                && env2
                    .0
                    .get(id)
                    .map(|(t2, env2)| contract_eq_bounded(t1, env1, t2, env2, gas))
                    .unwrap_or(false)
        }
        (Record(m1, attrs1), Record(m2, attrs2)) => {
            map_eq(m1, env1, m2, env2, gas) && attrs1 == attrs2
        }
        (RecRecord(m1, dyn_fields1, attrs1, _), RecRecord(m2, dyn_fields2, attrs2, _)) =>
        // We only compare records which field structure is statically known (i.e. without
        // dynamic fields).
        {
            dyn_fields1.is_empty()
                && dyn_fields2.is_empty()
                && map_eq(m1, env1, m2, env2, gas)
                && attrs1 == attrs2
        }
        (Array(ts1, attrs1), Array(ts2, attrs2)) => {
            ts1.len() == ts2.len()
                && ts1
                    .iter()
                    .zip(ts2.iter())
                    .all(|(t1, t2)| contract_eq_bounded(t1, env1.clone(), t2, env2.clone(), gas))
                && attrs1 == attrs2
        }
        // We must compare the inner values as well as corresponding contracts or type annotations.
        // Documentation, merge priority, etc. shouldn't impact the result on the other hand.
        (MetaValue(m1), MetaValue(m2)) => match (&m1.value, &m2.value) {
            (None, None) => true,
            (Some(v1), Some(v2)) => contract_eq_bounded(v1, env1, v2, env2, gas),
            _ => false,
        },
        // We dont treat imports and parse errors
        _ => false,
    }
}

/// Compute the equality between two records.
fn map_eq(
    map1: &HashMap<Ident, RichTerm>,
    env1: ContractEnvironment,
    map2: &HashMap<Ident, RichTerm>,
    env2: ContractEnvironment,
    gas: &mut u8,
) -> bool {
    map1.len() == map2.len()
        && map1.iter().all(|(id, t1)| {
            map2.get(id)
                .map(|t2| contract_eq_bounded(t1, env1.clone(), t2, env2.clone(), gas))
                .unwrap_or(false)
        })
}
