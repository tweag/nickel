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
//! contains the binding `f = merge r1.f r2.f`
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
//! Note that merging of lists is not yet implemented.
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
use crate::error::EvalError;
use crate::eval::{Closure, Environment};
use crate::label::Label;
use crate::position::RawSpan;
use crate::term::{BinaryOp, RichTerm, Term};
use crate::transformations::Closurizable;
use crate::types::{AbsType, Types};
use std::collections::HashMap;

/// Compute the merge of two evaluated operands.
pub fn merge(
    t1: RichTerm,
    env1: Environment,
    t2: RichTerm,
    env2: Environment,
    pos_op: Option<RawSpan>,
) -> Result<Closure, EvalError> {
    let RichTerm {
        term: t1,
        pos: pos1,
    } = t1;
    let RichTerm {
        term: t2,
        pos: pos2,
    } = t2;
    match (*t1, *t2) {
        // Merge is idempotent on basic terms
        (Term::Bool(b1), Term::Bool(b2)) => {
            if b1 == b2 {
                Ok(Closure::atomic_closure(Term::Bool(b1).into()))
            } else {
                Err(EvalError::MergeIncompatibleArgs(
                    RichTerm {
                        term: Box::new(Term::Bool(b1)),
                        pos: pos1,
                    },
                    RichTerm {
                        term: Box::new(Term::Bool(b2)),
                        pos: pos2,
                    },
                    pos_op,
                ))
            }
        }
        (Term::Num(n1), Term::Num(n2)) => {
            if n1 == n2 {
                Ok(Closure::atomic_closure(Term::Num(n1).into()))
            } else {
                Err(EvalError::MergeIncompatibleArgs(
                    RichTerm {
                        term: Box::new(Term::Num(n1)),
                        pos: pos1,
                    },
                    RichTerm {
                        term: Box::new(Term::Num(n2)),
                        pos: pos2,
                    },
                    pos_op,
                ))
            }
        }
        (Term::Str(s1), Term::Str(s2)) => {
            if s1 == s2 {
                Ok(Closure::atomic_closure(Term::Str(s1).into()))
            } else {
                Err(EvalError::MergeIncompatibleArgs(
                    RichTerm {
                        term: Box::new(Term::Str(s1)),
                        pos: pos1,
                    },
                    RichTerm {
                        term: Box::new(Term::Str(s2)),
                        pos: pos2,
                    },
                    pos_op,
                ))
            }
        }
        (Term::Lbl(l1), Term::Lbl(l2)) => {
            if l1 == l2 {
                Ok(Closure::atomic_closure(Term::Lbl(l1).into()))
            } else {
                Err(EvalError::MergeIncompatibleArgs(
                    RichTerm {
                        term: Box::new(Term::Lbl(l1)),
                        pos: pos1,
                    },
                    RichTerm {
                        term: Box::new(Term::Lbl(l2)),
                        pos: pos2,
                    },
                    pos_op,
                ))
            }
        }
        // Right-biased: when merging two docstrings (s1,t2) and (s2,t2), the right one will end up
        // as the outermost position in the resulting term (s2,(s1,merge t1 t2))
        (t1, Term::Docstring(s, t2)) => {
            let Closure { body, env } = mk_merge_closure(
                RichTerm {
                    term: Box::new(t1),
                    pos: pos1,
                },
                env1,
                t2,
                env2,
            );
            let body = Term::Docstring(s, body).into();
            Ok(Closure { body, env })
        }
        (Term::Docstring(s, t1), t2) => {
            let Closure { body, env } = mk_merge_closure(
                t1,
                env1,
                RichTerm {
                    term: Box::new(t2),
                    pos: pos2,
                },
                env2,
            );
            let body = Term::Docstring(s, body).into();
            Ok(Closure { body, env })
        }
        // Default merging
        (Term::DefaultValue(t1), Term::DefaultValue(t2)) => {
            let Closure { body, env } = mk_merge_closure(t1, env1, t2, env2);
            let body = Term::DefaultValue(body).into();
            Ok(Closure { body, env })
        }
        (Term::DefaultValue(t1), Term::ContractWithDefault(ty, lbl, t2))
        | (Term::ContractWithDefault(ty, lbl, t2), Term::DefaultValue(t1)) => {
            let Closure { body, mut env } = mk_merge_closure(t1, env1, t2, env2.clone());
            let ty_closure = ty.closurize(&mut env, env2);
            let body = Term::ContractWithDefault(ty_closure, lbl, body).into();
            Ok(Closure { body, env })
        }
        // Composed contracts carry and blame their original label. As any contract, the composite
        // still requires a label, but it will be ignored, so we can provide a dummy one.
        (Term::ContractWithDefault(ty1, lbl1, t1), Term::ContractWithDefault(ty2, lbl2, t2)) => {
            let Closure { body, mut env } = mk_merge_closure(t1, env1.clone(), t2, env2.clone());
            let body = Term::ContractWithDefault(
                merge_types_closure(&mut env, ty1, lbl1, env1, ty2, lbl2, env2),
                Label::dummy(),
                body,
            )
            .into();
            Ok(Closure { body, env })
        }
        // We need to keep the environment of contracts as well: custom contracts may use variables
        // from the environment, and even standard contracts need access to builtins contracts (see
        // issue https://github.com/tweag/nickel/issues/117)
        (Term::DefaultValue(t), Term::Contract(ty, lbl)) => {
            let mut env = HashMap::new();
            let t_closure = t.closurize(&mut env, env1);
            let ty_closure = ty.closurize(&mut env, env2);
            let body = Term::ContractWithDefault(ty_closure, lbl, t_closure).into();
            Ok(Closure { body, env })
        }
        (Term::Contract(ty, lbl), Term::DefaultValue(t)) => {
            let mut env = HashMap::new();
            let ty_closure = ty.closurize(&mut env, env1);
            let t_closure = t.closurize(&mut env, env2);
            let body = Term::ContractWithDefault(ty_closure, lbl, t_closure).into();
            Ok(Closure { body, env })
        }
        (Term::DefaultValue(_), t) => {
            let body = RichTerm {
                term: Box::new(t),
                pos: pos2,
            };
            Ok(Closure { body, env: env2 })
        }
        (t, Term::DefaultValue(_)) => {
            let body = RichTerm {
                term: Box::new(t),
                pos: pos1,
            };
            Ok(Closure { body, env: env1 })
        }
        // Contracts merging
        (Term::Contract(ty1, lbl1), Term::Contract(ty2, lbl2)) => {
            let mut env = HashMap::new();
            let body = Term::Contract(
                merge_types_closure(&mut env, ty1, lbl1, env1, ty2, lbl2, env2),
                Label::dummy(),
            )
            .into();
            Ok(Closure { body, env })
        }
        (Term::Contract(ty1, lbl1), Term::ContractWithDefault(ty2, lbl2, t)) => {
            let mut env = HashMap::new();
            let ty_closure =
                merge_types_closure(&mut env, ty1, lbl1, env1, ty2, lbl2, env2.clone());
            let t_closure = t.closurize(&mut env, env2);
            let body = Term::ContractWithDefault(ty_closure, Label::dummy(), t_closure).into();
            Ok(Closure { body, env })
        }
        (Term::ContractWithDefault(ty1, lbl1, t), Term::Contract(ty2, lbl2)) => {
            let mut env = HashMap::new();
            let ty_closure =
                merge_types_closure(&mut env, ty1, lbl1, env1.clone(), ty2, lbl2, env2);
            let t_closure = t.closurize(&mut env, env1);
            let body = Term::ContractWithDefault(ty_closure, Label::dummy(), t_closure).into();
            Ok(Closure { body, env })
        }
        (Term::Contract(ty, lbl), t) | (Term::ContractWithDefault(ty, lbl, _), t) => {
            let mut env = HashMap::new();
            let t = RichTerm {
                term: Box::new(t),
                pos: pos2,
            };
            let ty_closure = ty.closurize(&mut env, env1);
            let t_closure = t.closurize(&mut env, env2);
            let body = Term::Assume(ty_closure, lbl, t_closure).into();
            Ok(Closure { body, env })
        }
        (t, Term::Contract(ty, lbl)) | (t, Term::ContractWithDefault(ty, lbl, _)) => {
            let mut env = HashMap::new();
            let t = RichTerm {
                term: Box::new(t),
                pos: pos1,
            };
            let t_closure = t.closurize(&mut env, env1);
            let ty_closure = ty.closurize(&mut env, env2);
            let body = Term::Assume(ty_closure, lbl, t_closure).into();
            Ok(Closure { body, env })
        }
        // Merge put together the fields of records, and recursively merge
        // fields that are present in both terms
        (Term::Record(m1), Term::Record(m2)) => {
            /* Terms inside m1 and m2 may capture variables of resp. env1 and env2.  Morally, we
             * need to store closures, or a merge of closures, inside the resulting record.  We use
             * the same trick as in the evaluation of the operator DynExtend, and replace each such
             * term by a variable bound to an appropriate closure in the environment
             */
            let mut m = HashMap::new();
            let mut env = HashMap::new();
            let (mut left, mut center, mut right) = hashmap::split(m1, m2);

            for (field, t) in left.drain() {
                m.insert(field, t.closurize(&mut env, env1.clone()));
            }

            for (field, t) in right.drain() {
                m.insert(field, t.closurize(&mut env, env2.clone()));
            }

            for (field, (t1, t2)) in center.drain() {
                m.insert(
                    field,
                    Term::Op2(
                        BinaryOp::Merge(),
                        t1.closurize(&mut env, env1.clone()),
                        t2.closurize(&mut env, env2.clone()),
                    )
                    .into(),
                );
            }

            Ok(Closure {
                body: Term::Record(m).into(),
                env,
            })
        }
        //The following cases are either errors or not yet implemented
        (t1_, t2_) => Err(EvalError::MergeIncompatibleArgs(
            RichTerm {
                term: Box::new(t1_),
                pos: pos1,
            },
            RichTerm {
                term: Box::new(t2_),
                pos: pos2,
            },
            pos_op,
        )),
    }
}

/// Take two terms together with their environment, and return a closure representing their merge.
fn mk_merge_closure(t1: RichTerm, env1: Environment, t2: RichTerm, env2: Environment) -> Closure {
    let mut env = HashMap::new();

    let body = Term::Op2(
        BinaryOp::Merge(),
        t1.closurize(&mut env, env1),
        t2.closurize(&mut env, env2),
    )
    .into();

    Closure { body, env }
}

/// Compose two contracts, given as terms.
///
/// To compose contracts `c1` and `c2`, construct the term `fun _l x => c1 l1 (c2 l2 x)`, where
/// `l1` and `l2` are the original respective labels of `c1` and `c2`, and return the corresponding
/// flat type.
///
/// This type corresponds to the intersection of the types associated to `c1` and `c2`.  This
/// function is not correct for the intersection of higher-order contracts, which is way more
/// involved (see the [corresponding
/// notes](https://github.com/tweag/nickel/blob/master/notes/intersection-and-union-types.md) in
/// the repository).
fn merge_contracts(c1: RichTerm, l1: Label, c2: RichTerm, l2: Label) -> Types {
    let contract = RichTerm::fun(
        "_l".to_string(),
        RichTerm::fun(
            "x".to_string(),
            RichTerm::app(
                RichTerm::app(c1, Term::Lbl(l1).into()),
                RichTerm::app(
                    RichTerm::app(c2, Term::Lbl(l2).into()),
                    RichTerm::var("x".to_string()),
                ),
            ),
        ),
    );

    Types(AbsType::Flat(contract.into()))
}

/// [Closurize](../transformations/trait.Closurizable.html) two types with their respective
/// environment and merge them by composing their underlying contracts.
///
/// See [`merge_contracts`](./fn.merge_contracts.html).
fn merge_types_closure(
    env: &mut Environment,
    ty1: Types,
    l1: Label,
    env1: Environment,
    ty2: Types,
    l2: Label,
    env2: Environment,
) -> Types {
    let c1 = ty1.contract().closurize(env, env1);
    let c2 = ty2.contract().closurize(env, env2);
    merge_contracts(c1, l1, c2, l2)
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
