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
use crate::term::record::RecordData;
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
    env1: Environment,
    t2: RichTerm,
    env2: Environment,
    pos_op: TermPos,
    mode: MergeMode,
    call_stack: &CallStack,
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
                Term::Array(arr1, ArrayAttrs::new().closurized()),
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
                opt: opt1,
                priority: priority1,
                value: value1,
            } = meta1;
            let MetaValue {
                doc: doc2,
                types: types2,
                contracts: contracts2,
                opt: opt2,
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
            // Clippy is wrong to complain about the useless `collect` here:
            // It is necessary to release the mutable borrow on `env`
            // See https://github.com/rust-lang/rust-clippy/issues/7526
            #[allow(clippy::needless_collect)]
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
                // If one of the record requires this field, then it musn't be optional. The
                // resulting field is optional iff both are.
                opt: opt1 && opt2,
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
        (Term::Record(r1), Term::Record(r2)) => {
            /* Terms inside m1 and m2 may capture variables of resp. env1 and env2.  Morally, we
             * need to store closures, or a merge of closures, inside the resulting record.  We use
             * the same trick as in the evaluation of the operator DynExtend, and replace each such
             * term by a variable bound to an appropriate closure in the environment
             */

            // We save the original fields before they are potentially merged in order to patch
            // their environment in the final record (cf `fixpoint::patch_fields`). Note that we
            // are only cloning shared terms (`Rc`s) here.
            //let m1_values: Vec<_> = r1.fields.values().cloned().collect();
            //let m2_values: Vec<_> = r2.fields.values().cloned().collect();

            let hashmap::SplitResult {
                left,
                center,
                right,
            } = hashmap::split(r1.fields, r2.fields);

            match mode {
                MergeMode::Contract(mut lbl) if !r2.attrs.open && !left.is_empty() => {
                    let fields: Vec<String> =
                        left.keys().map(|field| format!("`{}`", field)).collect();
                    let plural = if fields.len() == 1 { "" } else { "s" };
                    lbl.tag = format!("extra field{} {}", plural, fields.join(","));
                    return Err(EvalError::BlameError(lbl, CallStack::new()));
                }
                _ => (),
            };

            // We use field_names for saturating revertible thunks. The iterator order is important
            // and must stay the same within this function. Because hashset are randomized, cloning
            // field_names, inserting new values, etc. will change iteration order. Please don't do
            // that.
            let field_names: Vec<_> = left
                .keys()
                .chain(center.keys())
                .chain(right.keys())
                .cloned()
                .collect();
            let mut m = HashMap::with_capacity(left.len() + center.len() + right.len());
            let mut env = Environment::new();

            // Merging recursive record is the one operation that may override recursive fields. To
            // have the recursive fields depend on the updated values, we need to revert the
            // thunks.
            //
            // We do that for the left and the right part. The fields in the intersection (center)
            // needs a more general treatment to correctly propagate the recursive values down each
            // field.
            m.extend(
                left.into_iter()
                    .map(|(field, t)| (field, rev_thunk_closurize(t, &mut env, &env1))),
            );
            m.extend(
                right
                    .into_iter()
                    .map(|(field, t)| (field, rev_thunk_closurize(t, &mut env, &env2))),
            );

            for (field, (t1, t2)) in center.into_iter() {
                m.insert(
                    field,
                    fields_merge_closurize(&mut env, t1, &env1, t2, &env2, field_names.iter())?,
                );
            }

            let final_pos = if mode == MergeMode::Standard {
                pos_op.into_inherited()
            } else {
                pos1.into_inherited()
            };

            Ok(Closure {
                body: RichTerm::new(
                    // We don't have to provide RecordDeps, which are required in a previous stage
                    // of program transformations. At this point, the interpreter doesn't care
                    // about them anymore, and dependencies are stored at the level of revertible
                    // thunks directly.
                    Term::RecRecord(
                        RecordData::new(m, RecordAttrs::merge(r1.attrs, r2.attrs)),
                        Vec::new(),
                        None,
                    ),
                    final_pos,
                ),
                env,
            })
        }
        (t1_, t2_) => match (mode, &t2_) {
            // We want to merge a non-record term with a record contract
            (MergeMode::Contract(label), Term::Record(..)) => {
                Err(EvalError::BlameError(label, call_stack.clone()))
            }
            // The following cases are either errors or not yet implemented
            _ => Err(EvalError::MergeIncompatibleArgs(
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
        },
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

/// Take the content of a record field, and saturate the potential revertible thunk with the given
/// fields. See [crate::eval::lazy::Thunk::saturate].
///
/// If the expression is not a variable referring to a thunk (this can happen e.g. for numeric
/// constant), we just return the term as it is, which falls into the zero dependencies special
/// case as well.
fn saturate<'a, I: DoubleEndedIterator<Item = &'a Ident> + Clone>(
    rt: RichTerm,
    env: &mut Environment,
    local_env: &Environment,
    fields: I,
) -> Result<RichTerm, EvalError> {
    if let Term::Var(var_id) = &*rt.term {
        let thunk = local_env
            .get(var_id)
            .cloned()
            .ok_or(EvalError::UnboundIdentifier(*var_id, rt.pos))?;

        Ok(thunk.saturate(env, fields).with_pos(rt.pos))
    } else {
        Ok(rt)
    }
}

/// Return the dependencies of a field when represented as a `RichTerm`.
fn field_deps(rt: &RichTerm, local_env: &Environment) -> Result<ThunkDeps, EvalError> {
    if let Term::Var(var_id) = &*rt.term {
        local_env
            .get(var_id)
            .map(Thunk::deps)
            .ok_or(EvalError::UnboundIdentifier(*var_id, rt.pos))
    } else {
        Ok(ThunkDeps::Empty)
    }
}

/// Take the current environment, two fields with their local environment, and return a term which
/// is the merge of the two fields, closurized in the provided final environment.
///
/// The thunk allocated for the result is revertible if and only if at least one of the original
/// thunk is (if one of the original value is overridable, then so is the merge of the two). In
/// this case, the field dependencies are the union of the dependencies of each field.
///
/// The fields are saturaed (see [saturate]) to properly propagate recursive dependencies to the
/// other fields in the final, merged record.
fn fields_merge_closurize<'a, I: DoubleEndedIterator<Item = &'a Ident> + Clone>(
    env: &mut Environment,
    t1: RichTerm,
    env1: &Environment,
    t2: RichTerm,
    env2: &Environment,
    fields: I,
) -> Result<RichTerm, EvalError> {
    use std::{collections::HashSet, rc::Rc};

    let mut local_env = Environment::new();

    // May deserve a plus operation on ThunkDeps
    let combined_deps = match (field_deps(&t1, env1)?, field_deps(&t2, env2)?) {
        // If neither field has dependencies, the merge of the two fields doesn't have dependencies
        (ThunkDeps::Empty, ThunkDeps::Empty) => ThunkDeps::Empty,
        // If one of the field has unknown dependencies (understand: may depend on all the other
        // fields), then the resulting fields has unknown dependencies as well
        (ThunkDeps::Unknown, _) | (_, ThunkDeps::Unknown) => ThunkDeps::Unknown,
        (ThunkDeps::Empty, ThunkDeps::Known(deps)) | (ThunkDeps::Known(deps), ThunkDeps::Empty) => {
            ThunkDeps::Known(deps)
        }
        (ThunkDeps::Known(deps1), ThunkDeps::Known(deps2)) => {
            let union: HashSet<Ident> = deps1.union(&*deps2).cloned().collect();
            ThunkDeps::Known(Rc::new(union))
        }
    };

    let body = RichTerm::from(Term::Op2(
        BinaryOp::Merge(),
        saturate(t1, &mut local_env, env1, fields.clone())?,
        saturate(t2, &mut local_env, env2, fields)?,
    ));

    // We closurize the final result in a thunk with appropriate dependencies
    let closure = Closure {
        body,
        env: local_env,
    };
    let fresh_var = Ident::fresh();

    match combined_deps {
        ThunkDeps::Empty => env.insert(fresh_var, Thunk::new(closure, IdentKind::Record)),
        ThunkDeps::Known(deps) => env.insert(
            fresh_var,
            Thunk::new_rev(closure, IdentKind::Record, Some(deps)),
        ),
        ThunkDeps::Unknown => {
            env.insert(fresh_var, Thunk::new_rev(closure, IdentKind::Record, None))
        }
    };

    Ok(RichTerm::from(Term::Var(fresh_var)))
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

fn rev_thunk_closurize(rt: RichTerm, env: &mut Environment, local_env: &Environment) -> RichTerm {
    if let Term::Var(id) = rt.as_ref() {
        // This create a fresh variable which is bound to a reverted copy of the original thunk
        let reverted = local_env.get(id).unwrap().revert();
        let fresh_id = Ident::fresh();
        env.insert(fresh_id, reverted);
        RichTerm::new(Term::Var(fresh_id), rt.pos)
    } else {
        // Otherwise, if it is not a variable after the share normal form transformations, it
        // should be a constant and we don't need to revert anything
        rt
    }
}

pub mod hashmap {
    use std::collections::HashMap;

    pub struct SplitResult<K, V1, V2> {
        pub left: HashMap<K, V1>,
        pub center: HashMap<K, (V1, V2)>,
        pub right: HashMap<K, V2>,
    }

    /// Split two hashmaps m1 and m2 in three parts (left,center,right), where left holds bindings
    /// `(key,value)` where key is not in `m2.keys()`, right is the dual (keys of m2 that are not
    /// in m1), and center holds bindings for keys that are both in m1 and m2.
    pub fn split<K, V1, V2>(m1: HashMap<K, V1>, m2: HashMap<K, V2>) -> SplitResult<K, V1, V2>
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

        SplitResult {
            left,
            center,
            right,
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn all_left() -> Result<(), String> {
            let mut m1 = HashMap::new();
            let m2 = HashMap::<isize, isize>::new();

            m1.insert(1, 1);
            let SplitResult {
                mut left,
                center,
                right,
            } = split(m1, m2);

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
            let SplitResult {
                left,
                center,
                mut right,
            } = split(m1, m2);

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
            let SplitResult {
                left,
                mut center,
                right,
            } = split(m1, m2);

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
            let SplitResult {
                mut left,
                mut center,
                mut right,
            } = split(m1, m2);

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
