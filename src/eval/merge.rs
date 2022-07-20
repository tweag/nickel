//! Evaluation of the merge operator.
//!
//! Merge is a primitive operation of Nickel, which recursively combines records. Together with
//! enriched values, it allows to write and mix contracts with standard records.
//!
//! # Operational semantics
//!
//! ## On records
//!
//! When records `r1` and `r2` are merged, the result is a new record with the following fields:
//! - All the fields of `r1` that are not in `r2`
//! - All the fields of `r2` that are not in `r1`
//! - Fields that are both in `r1` and `r2` are recursively merged: for a field `f`, the result
//! contains the binding `f = r1.f & r2.f`
//!
//! As fields are recursively merged, merge needs to operate on any value, not only on records.
//!
//! ## On simple values
//!
//! Simple values are terms which are not enriched values.
//!
//! - *Function*: merging a function with anything else fails
//! - *Values*: merging any other values succeeds if and only if these two values are equals, in which case it evaluates to
//! this common value.
//!
//! Note that merging of arrays is not yet implemented.
//!
//! ## On enriched values
//!
//! Enriched values (currently `Contract`, `Default`, `ContractDefault` or `Docstring`) get their
//! special powers from their interaction with the merge operator.
//!
//! ### Enriched/Enriched
//!
//! - *Contract/contract*: merging two contracts evaluates to a contract which is the composition
//! of the two
//! - *Default/default*: merging two default values evaluates to a default which value is the merge
//! of the two
//! - *Contract/default*: merging a `Default` with a `Contract` evaluates to a `ContractDefault`
//! - *ContractDefault/_*: Merging `ContractDefault` is done component-wise: with another
//! `ContractDefault`, it evaluates to a `ContractDefault` where the two contracts as well as the
//! two default values are respectively merged together. With either just a `Contract` or a
//! `Default`, it simply merges the corresponding component and let the other unchanged.
//!
//! ### Enriched/Simple
//!
//! - *Docstring*: merging a docstring (with inner term `inner`) with another term `t` recursively merges
//! `inner` and `t`, and evaluates to this result wrapped in the original docstring (`t` may be a simple value or an
//! enriched one here)
//! - *Default erasure*: merging a `Default` with a simple value drops the default value and
//! evaluates to the simple value
//! - *Contract check*: merging a `Contract` or a `ContractDefault` with a simple value `t`
//! evaluates to a contract check, that is an `Assume(..., t)`
use super::*;
use crate::error::EvalError;
use crate::label::Label;
use crate::position::TermPos;
use crate::term::{
    make as mk_term, BinaryOp, Contract, MetaValue, RecordAttrs, RichTerm, SharedTerm, Term,
};
use crate::transform::Closurizable;
use std::collections::HashMap;

/// Merging mode. Merging is used both to combine standard data and to apply contracts defined as
/// records.
#[derive(Clone, PartialEq, Debug)]
pub enum MergeMode {
    /// Standard merging, for combining data.
    Standard,
    /// Merging to apply a record contract to a value, with the associated label.
    Contract(Label),
}

impl Default for MergeMode {
    fn default() -> Self {
        MergeMode::Standard
    }
}

/// Compute the merge of two evaluated operands. Support both standard merging and record contract
/// application.
///
/// # Mode
///
/// In [`MergeMode::Contract`] mode, `t1` must be the value and `t2` must be the contract. It is
/// important as `merge` is not commutative in this mode.
pub fn merge(
    t1: RichTerm,
    mut env1: Environment,
    t2: RichTerm,
    mut env2: Environment,
    pos_op: TermPos,
    mode: MergeMode,
) -> Result<Closure, EvalError> {
    // Merging a simple value and a metavalue is equivalent to first wrapping the simple value in a
    // new metavalue (with no attribute set excepted the value), and then merging the two
    let (t1, t2) = match (t1.term.is_metavalue(), t2.term.is_metavalue()) {
        (true, false) => {
            let pos = t2.pos;
            let t = Term::MetaValue(MetaValue::from(t2));
            (t1, RichTerm::new(t, pos))
        }
        (false, true) => {
            let pos = t1.pos;
            let t = Term::MetaValue(MetaValue::from(t1));
            (RichTerm::new(t, pos), t2)
        }
        _ => (t1, t2),
    };

    let RichTerm {
        term: t1,
        pos: pos1,
    } = t1;
    let RichTerm {
        term: t2,
        pos: pos2,
    } = t2;

    match (t1.into_owned(), t2.into_owned()) {
        // Merge is idempotent on basic terms
        (Term::Null, Term::Null) => Ok(Closure::atomic_closure(RichTerm::new(
            Term::Null,
            pos_op.into_inherited(),
        ))),
        (Term::Bool(b1), Term::Bool(b2)) => {
            if b1 == b2 {
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Bool(b1),
                    pos_op.into_inherited(),
                )))
            } else {
                Err(EvalError::MergeIncompatibleArgs(
                    RichTerm {
                        term: SharedTerm::new(Term::Bool(b1)),
                        pos: pos1,
                    },
                    RichTerm {
                        term: SharedTerm::new(Term::Bool(b2)),
                        pos: pos2,
                    },
                    pos_op,
                ))
            }
        }
        (Term::Num(n1), Term::Num(n2)) => {
            if (n1 - n2).abs() < f64::EPSILON {
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Num(n1),
                    pos_op.into_inherited(),
                )))
            } else {
                Err(EvalError::MergeIncompatibleArgs(
                    RichTerm {
                        term: SharedTerm::new(Term::Num(n1)),
                        pos: pos1,
                    },
                    RichTerm {
                        term: SharedTerm::new(Term::Num(n2)),
                        pos: pos2,
                    },
                    pos_op,
                ))
            }
        }
        (Term::Str(s1), Term::Str(s2)) => {
            if s1 == s2 {
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Str(s1),
                    pos_op.into_inherited(),
                )))
            } else {
                Err(EvalError::MergeIncompatibleArgs(
                    RichTerm {
                        term: SharedTerm::new(Term::Str(s1)),
                        pos: pos1,
                    },
                    RichTerm {
                        term: SharedTerm::new(Term::Str(s2)),
                        pos: pos2,
                    },
                    pos_op,
                ))
            }
        }
        (Term::Lbl(l1), Term::Lbl(l2)) => {
            if l1 == l2 {
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Lbl(l1),
                    pos_op.into_inherited(),
                )))
            } else {
                Err(EvalError::MergeIncompatibleArgs(
                    RichTerm {
                        term: SharedTerm::new(Term::Lbl(l1)),
                        pos: pos1,
                    },
                    RichTerm {
                        term: SharedTerm::new(Term::Lbl(l2)),
                        pos: pos2,
                    },
                    pos_op,
                ))
            }
        }
        (Term::Enum(i1), Term::Enum(i2)) => {
            if i1 == i2 {
                Ok(Closure::atomic_closure(RichTerm::new(
                    Term::Enum(i1),
                    pos_op.into_inherited(),
                )))
            } else {
                Err(EvalError::MergeIncompatibleArgs(
                    RichTerm {
                        term: SharedTerm::new(Term::Enum(i1)),
                        pos: pos1,
                    },
                    RichTerm {
                        term: SharedTerm::new(Term::Enum(i2)),
                        pos: pos2,
                    },
                    pos_op,
                ))
            }
        }
        (Term::Array(arr1, _attrs1), Term::Array(arr2, _attrs2))
            if arr1.is_empty() && arr2.is_empty() =>
        {
            Ok(Closure::atomic_closure(RichTerm::new(
                Term::Array(arr1, ArrayAttrs { closurized: true }),
                pos_op.into_inherited(),
            )))
        }
        (Term::MetaValue(meta1), Term::MetaValue(meta2)) => {
            // For now, we blindly closurize things and copy environments in this section. A
            // careful analysis would make it possible to spare a few closurize operations and more
            // generally environment cloning.

            let MetaValue {
                doc: doc1,
                types: types1,
                contracts: contracts1,
                priority: priority1,
                value: value1,
            } = meta1;
            let MetaValue {
                doc: doc2,
                types: types2,
                contracts: contracts2,
                priority: priority2,
                value: value2,
            } = meta2;

            let doc = merge_doc(doc1, doc2);

            // If:
            // 1. meta1 has a value
            // 2. meta2 has a contract
            // 3. The priorities (or the fact that meta2's value is not defined) are such that
            //    meta1's value will be used in the final value
            // Then, we apply meta2's contract to meta1. This creates a new value and a new
            // intermediate environment.
            let (value1, val_env1) = match value1 {
                Some(v1)
                    if (types2.is_some() || !contracts2.is_empty())
                        && (priority1 >= priority2 || value2.is_none()) =>
                {
                    let (v, e) = cross_apply_contracts(
                        v1,
                        &env1,
                        types2.iter().chain(contracts2.iter()),
                        &env2,
                    )?;
                    (Some(v), e)
                }
                v1 => (v1, env1.clone()),
            };

            // Dually, we cross apply meta1's contracts to meta2's value.
            let (value2, val_env2) = match value2 {
                Some(v2)
                    if (types1.is_some() || !contracts1.is_empty())
                        && (priority2 >= priority1 || value1.is_none()) =>
                {
                    let (v, e) = cross_apply_contracts(
                        v2,
                        &env2,
                        types1.iter().chain(contracts1.iter()),
                        &env1,
                    )?;
                    (Some(v), e)
                }
                v2 => (v2, env2.clone()),
            };

            // Selecting either meta1's value, meta2's value, or the merge of the two values,
            // depending on which is defined and respective priorities.
            let (value, priority, mut env) = match (value1, value2) {
                (Some(t1), Some(t2)) if priority1 == priority2 => {
                    let mut env = Environment::new();
                    (
                        Some(merge_closurize(&mut env, t1, val_env1, t2, val_env2)),
                        priority1,
                        env,
                    )
                }
                (Some(t1), _) if priority1 > priority2 => (Some(t1), priority1, val_env1),
                (Some(t1), None) => (Some(t1), priority1, val_env1),
                (_, Some(t2)) if priority2 > priority1 => (Some(t2), priority2, val_env2),
                (None, Some(t2)) => (Some(t2), priority2, val_env2),
                (None, None) => (None, Default::default(), Environment::new()),
                _ => unreachable!(),
            };

            // Finally, we also need to closurize the contracts in the final envirnment.
            let mut contracts1: Vec<Contract> = contracts1
                .into_iter()
                .map(|ctr| ctr.closurize(&mut env, env1.clone()))
                .collect();
            let contracts2: Vec<Contract> = contracts2
                .into_iter()
                .map(|ctr| ctr.closurize(&mut env, env2.clone()))
                .collect();
            let types1 = types1.map(|ctr| ctr.closurize(&mut env, env1));
            let types2 = types2.map(|ctr| ctr.closurize(&mut env, env2));

            // If both have type annotations, we arbitrarily choose the first one. At this point we
            // are evaluating the term, and types annotations and contracts make no difference
            // operationnally. Even for a query, it's strange to show multiple static types. So if
            // both are set, we turn types2 to a contract and keep type1 as the type annotation.
            let types = match types2 {
                Some(ctr) if types1.is_some() => {
                    contracts1.push(ctr);
                    types1
                }
                _ => types1,
            };

            let contracts: Vec<_> = contracts1
                .into_iter()
                .chain(contracts2.into_iter())
                .collect();
            let meta = MetaValue {
                doc,
                types,
                contracts,
                priority,
                value,
            };

            Ok(Closure {
                body: RichTerm::new(Term::MetaValue(meta), pos_op.into_inherited()),
                env,
            })
        }
        // Merge put together the fields of records, and recursively merge
        // fields that are present in both terms
        (Term::Record(mut m1, attrs1), Term::Record(mut m2, attrs2)) => {
            /* Terms inside m1 and m2 may capture variables of resp. env1 and env2.  Morally, we
             * need to store closures, or a merge of closures, inside the resulting record.  We use
             * the same trick as in the evaluation of the operator DynExtend, and replace each such
             * term by a variable bound to an appropriate closure in the environment
             */
            // Merging recursive record is the one operation that may override recursive fields. To
            // have the recursive fields depend on the updated values, we need to revert the thunks
            // first.
            rev_thunks(m1.values_mut(), &mut env1);
            rev_thunks(m2.values_mut(), &mut env2);

            // We save the original fields before they are potentially merged in order to patch
            // their environment in the final record (cf `fixpoint::patch_fields`). Note that we
            // are only cloning shared terms (`Rc`s) here.
            let m1_values: Vec<_> = m1.values().cloned().collect();
            let m2_values: Vec<_> = m2.values().cloned().collect();

            let (left, center, right) = hashmap::split(m1, m2);

            match mode {
                MergeMode::Contract(mut lbl) if !attrs2.open && !left.is_empty() => {
                    let fields: Vec<String> =
                        left.keys().map(|field| format!("`{}`", field)).collect();
                    let plural = if fields.len() == 1 { "" } else { "s" };
                    lbl.tag = format!("extra field{} {}", plural, fields.join(","));
                    return Err(EvalError::BlameError(lbl, CallStack::new()));
                }
                _ => (),
            };

            let mut m = HashMap::with_capacity(left.len() + center.len() + right.len());
            let mut env = Environment::new();

            for (field, t) in left.into_iter() {
                m.insert(field, t.closurize(&mut env, env1.clone()));
            }

            for (field, t) in right.into_iter() {
                m.insert(field, t.closurize(&mut env, env2.clone()));
            }

            for (field, (t1, t2)) in center.into_iter() {
                m.insert(
                    field,
                    merge_closurize(&mut env, t1, env1.clone(), t2, env2.clone()),
                );
            }

            let rec_env = fixpoint::rec_env(m.iter(), &env)?;
            m1_values
                .iter()
                .try_for_each(|rt| fixpoint::patch_field(rt, &rec_env, &env1))?;
            m2_values
                .iter()
                .try_for_each(|rt| fixpoint::patch_field(rt, &rec_env, &env2))?;

            let final_pos = if mode == MergeMode::Standard {
                pos_op.into_inherited()
            } else {
                pos1.into_inherited()
            };

            Ok(Closure {
                body: RichTerm::new(
                    Term::Record(m, RecordAttrs::merge(attrs1, attrs2)),
                    final_pos,
                ),
                env,
            })
        }
        //The following cases are either errors or not yet implemented
        (t1_, t2_) => Err(EvalError::MergeIncompatibleArgs(
            RichTerm {
                term: SharedTerm::new(t1_),
                pos: pos1,
            },
            RichTerm {
                term: SharedTerm::new(t2_),
                pos: pos2,
            },
            pos_op,
        )),
    }
}

/// Apply a series of contract to term and closurize the result, and apply the necessary
/// intermediate closurization.
///
/// # Parameters
///
/// - the term is given by `t1` in its environment `env1`
/// - the contracts are given as an iterator `it2` together with their environment `env2`
fn cross_apply_contracts<'a>(
    t1: RichTerm,
    env1: &Environment,
    mut it2: impl Iterator<Item = &'a Contract>,
    env2: &Environment,
) -> Result<(RichTerm, Environment), EvalError> {
    let mut env = Environment::new();
    let mut env1_local = env1.clone();

    let pos = t1.pos.into_inherited();
    let result = it2
        .try_fold(t1, |acc, ctr| {
            let ty_closure = ctr.types.clone().closurize(&mut env1_local, env2.clone());
            mk_term::assume(ty_closure, ctr.label.clone(), acc)
                .map_err(|crate::types::UnboundTypeVariableError(id)| {
                    let pos = id.pos;
                    EvalError::UnboundIdentifier(id, pos)
                })
                .map(|rt| rt.with_pos(pos))
        })?
        .closurize(&mut env, env1_local);

    Ok((result, env))
}

/// Merge the two optional documentations of a metavalue.
fn merge_doc(doc1: Option<String>, doc2: Option<String>) -> Option<String> {
    //FIXME: how to merge documentation? Just concatenate?
    doc1.or(doc2)
}

/// Take the current environment, two terms with their local environment, and return a term which
/// is the closurized merge of the two.
fn merge_closurize(
    env: &mut Environment,
    t1: RichTerm,
    env1: Environment,
    t2: RichTerm,
    env2: Environment,
) -> RichTerm {
    let mut local_env = Environment::new();
    let body = RichTerm::from(Term::Op2(
        BinaryOp::Merge(),
        t1.closurize(&mut local_env, env1),
        t2.closurize(&mut local_env, env2),
    ));
    body.closurize(env, local_env)
}

fn rev_thunks<'a, I: Iterator<Item = &'a mut RichTerm>>(map: I, env: &mut Environment) {
    use crate::transform::fresh_var;

    for rt in map {
        if let Term::Var(id) = rt.as_ref() {
            // This create a fresh variable which is bound to a reverted copy of the original thunk
            let reverted = env.get(id).unwrap().revert();
            let fresh_id = fresh_var();
            env.insert(fresh_id.clone(), reverted);
            *(SharedTerm::make_mut(&mut rt.term)) = Term::Var(fresh_id);
        }
        // Otherwise, if it is not a variable after the share normal form transformations, it
        // should be a constant and we don't need to revert anything
    }
}

pub mod hashmap {
    use std::collections::HashMap;

    /// Split two hashmaps m1 and m2 in three parts (left,center,right), where left holds bindings
    /// `(key,value)` where key is not in `m2.keys()`, right is the dual (keys of m2 that are not
    /// in m1), and center holds bindings for keys that are both in m1 and m2.
    pub fn split<K, V1, V2>(
        m1: HashMap<K, V1>,
        m2: HashMap<K, V2>,
    ) -> (HashMap<K, V1>, HashMap<K, (V1, V2)>, HashMap<K, V2>)
    where
        K: std::hash::Hash + Eq,
    {
        let mut left = HashMap::new();
        let mut center = HashMap::new();
        let mut right = m2;

        for (key, value) in m1 {
            if let Some(v2) = right.remove(&key) {
                center.insert(key, (value, v2));
            } else {
                left.insert(key, value);
            }
        }

        (left, center, right)
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn all_left() -> Result<(), String> {
            let mut m1 = HashMap::new();
            let m2 = HashMap::<isize, isize>::new();

            m1.insert(1, 1);
            let (mut left, center, right) = split(m1, m2);

            if left.remove(&1) == Some(1)
                && left.is_empty()
                && center.is_empty()
                && right.is_empty()
            {
                Ok(())
            } else {
                Err(String::from("Expected all elements to be in the left part"))
            }
        }

        #[test]
        fn all_right() -> Result<(), String> {
            let m1 = HashMap::<isize, isize>::new();
            let mut m2 = HashMap::new();

            m2.insert(1, 1);
            let (left, center, mut right) = split(m1, m2);

            if right.remove(&1) == Some(1)
                && right.is_empty()
                && left.is_empty()
                && center.is_empty()
            {
                Ok(())
            } else {
                Err(String::from(
                    "Expected all elements to be in the right part",
                ))
            }
        }

        #[test]
        fn all_center() -> Result<(), String> {
            let mut m1 = HashMap::new();
            let mut m2 = HashMap::new();

            m1.insert(1, 1);
            m2.insert(1, 2);
            let (left, mut center, right) = split(m1, m2);

            if center.remove(&1) == Some((1, 2))
                && center.is_empty()
                && left.is_empty()
                && right.is_empty()
            {
                Ok(())
            } else {
                Err(String::from(
                    "Expected all elements to be in the center part",
                ))
            }
        }

        #[test]
        fn mixed() -> Result<(), String> {
            let mut m1 = HashMap::new();
            let mut m2 = HashMap::new();

            m1.insert(1, 1);
            m1.insert(2, 1);
            m2.insert(1, -1);
            m2.insert(3, -1);
            let (mut left, mut center, mut right) = split(m1, m2);

            if left.remove(&2) == Some(1)
                && center.remove(&1) == Some((1, -1))
                && right.remove(&3) == Some(-1)
                && left.is_empty()
                && center.is_empty()
                && right.is_empty()
            {
                Ok(())
            } else {
                Err(String::from(
                    "Expected all elements to be in the center part",
                ))
            }
        }
    }
}
