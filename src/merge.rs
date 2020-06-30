//! Evaluation of the merge operator
use crate::eval::{Closure, Environment, EvalError, IdentKind};
use crate::identifier::Ident;
use crate::label::{Label, TyPath};
use crate::term::{BinaryOp, RichTerm, Term};
use crate::types::{AbsType, Types};
use simple_counter::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

generate_counter!(FreshVariableCounter, usize);

/// Compute the merge of the two operands once they have been evaluated
pub fn merge(fst: Closure, snd: Closure) -> Result<Closure, EvalError> {
    let Closure {
        body: RichTerm { term: t1, pos: p1 },
        env: env1,
    } = fst;
    let Closure {
        body: RichTerm { term: t2, pos: p2 },
        env: env2,
    } = snd;
    match (*t1, *t2) {
        // Merge is idempotent on basic terms
        (Term::Bool(b1), Term::Bool(b2)) => {
            if b1 == b2 {
                Ok(Closure::atomic_closure(Term::Bool(b1).into()))
            } else {
                Err(EvalError::TypeError(String::from(
                    "Trying to merge two distinct booleans",
                )))
            }
        }
        (Term::Num(n1), Term::Num(n2)) => {
            if n1 == n2 {
                Ok(Closure::atomic_closure(Term::Num(n1).into()))
            } else {
                Err(EvalError::TypeError(format!(
                    "Trying to merge two distinct numbers {} and {}",
                    n1, n2
                )))
            }
        }
        (Term::Str(s1), Term::Str(s2)) => {
            if s1 == s2 {
                Ok(Closure::atomic_closure(Term::Str(s1).into()))
            } else {
                Err(EvalError::TypeError(format!(
                    "Trying to merge two distinct strings \"{}\" and \"{}\"",
                    s1, s2
                )))
            }
        }
        (Term::Lbl(l1), Term::Lbl(l2)) => {
            if l1 == l2 {
                Ok(Closure::atomic_closure(Term::Lbl(l1).into()))
            } else {
                Err(EvalError::TypeError(format!(
                    "Trying to merge two distinct labels \"{:?}\" and \"{:?}\"",
                    l1, l2
                )))
            }
        }
        // Right-biased: when merging two docstrings (s1,t2) and (s2,t2), the right one will end up
        // as the outermost position in the resulting term (s2,(s1,merge t1 t2))
        (t1, Term::Docstring(s, t2)) => {
            let Closure { body, env } = mk_merge_closure(
                RichTerm {
                    term: Box::new(t1),
                    pos: p1,
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
                    pos: p2,
                },
                env2,
            );
            let body = Term::Docstring(s, body).into();
            Ok(Closure { body, env })
        }
        // Default merging
        (Term::DefaultValue(_), Term::DefaultValue(_))
        | (Term::DefaultValue(_), Term::ContractWithDefault(_, _))
        | (Term::ContractWithDefault(_, _), Term::DefaultValue(_))
        | (Term::ContractWithDefault(_, _), Term::ContractWithDefault(_, _)) => Err(
            EvalError::TypeError("Trying to merge two default values".to_string()),
        ),
        // Should we keep the environment of contracts ?
        (Term::DefaultValue(t), Term::Contract(ty)) => Ok(Closure {
            body: Term::ContractWithDefault(ty, t).into(),
            env: env1,
        }),
        (Term::Contract(ty), Term::DefaultValue(t)) => Ok(Closure {
            body: Term::ContractWithDefault(ty, t).into(),
            env: env2,
        }),
        (Term::DefaultValue(_), t) => {
            let t = RichTerm {
                term: Box::new(t),
                pos: p2,
            };
            Ok(Closure { body: t, env: env2 })
        }
        (t, Term::DefaultValue(_)) => {
            let t = RichTerm {
                term: Box::new(t),
                pos: p1,
            };
            Ok(Closure { body: t, env: env1 })
        }
        // Contracts merging
        (Term::Contract(ty1), Term::Contract(ty2)) => Ok(Closure {
            body: Term::Contract(compose_contracts(ty1, ty2)).into(),
            env: HashMap::new(),
        }),
        (Term::Contract(ty1), Term::ContractWithDefault(ty2, t)) => Ok(Closure {
            body: Term::ContractWithDefault(compose_contracts(ty1, ty2), t).into(),
            env: HashMap::new(),
        }),
        (Term::ContractWithDefault(ty1, t), Term::Contract(ty2)) => {
            let ty = compose_contracts(ty1, ty2);
            let t = Term::ContractWithDefault(ty, t).into();
            Ok(Closure { body: t, env: env1 })
        }
        (Term::Contract(ty), t) | (Term::ContractWithDefault(ty, _), t) => {
            let t = RichTerm {
                term: Box::new(t),
                pos: p2,
            };
            let t = Term::Assume(ty, mk_merge_lbl(), t).into();
            Ok(Closure { body: t, env: env2 })
        }
        (t, Term::Contract(ty)) | (t, Term::ContractWithDefault(ty, _)) => {
            let t = RichTerm {
                term: Box::new(t),
                pos: p1,
            };
            let t = Term::Assume(ty, mk_merge_lbl(), t).into();
            Ok(Closure { body: t, env: env1 })
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
                m.insert(field, closurize(&mut env, t, env1.clone()));
            }

            for (field, t) in right.drain() {
                m.insert(field, closurize(&mut env, t, env2.clone()));
            }

            for (field, (t1, t2)) in center.drain() {
                m.insert(
                    field,
                    Term::Op2(
                        BinaryOp::Merge(),
                        closurize(&mut env, t1, env1.clone()),
                        closurize(&mut env, t2, env2.clone()),
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
        (ref t1, ref t2) => Err(EvalError::TypeError(format!(
            "Could not merge {:?} and {:?}",
            *t1, *t2
        ))),
    }
}

/// Create a RichTerm that represents the term `t` together with an environment `with_env`.
/// It generates a fresh variable, binds it to the corresponding closure `(t,with_env)` in env,
/// and returns this new variable as a term
fn closurize(env: &mut Environment, t: RichTerm, with_env: Environment) -> RichTerm {
    // To avoid clashing with fresh variables introduced by DynExtend, we add an 'm' in the prefix
    let var = format!("_m{}", FreshVariableCounter::next());
    let c = Closure {
        body: t,
        env: with_env,
    };

    env.insert(
        Ident(var.clone()),
        (Rc::new(RefCell::new(c)), IdentKind::Record()),
    );

    Term::Var(Ident(var)).into()
}

/// Take two terms together with their environment, and return a closure representing their merge
fn mk_merge_closure(t1: RichTerm, env1: Environment, t2: RichTerm, env2: Environment) -> Closure {
    let mut env = HashMap::new();

    let t = Term::Op2(
        BinaryOp::Merge(),
        closurize(&mut env, t1, env1),
        closurize(&mut env, t2, env2),
    )
    .into();

    Closure { body: t, env }
}

/// Generate a fresh label with an appropriate tag for a cast introduced by merge. Positional
/// information is ignored for now, but should be encoded in the label at some point
fn mk_merge_lbl() -> Label {
    Label {
        tag: "Merged with contract".to_string(),
        l: 0,
        r: 0,
        polarity: true,
        path: TyPath::Nil(),
    }
}

/// Return a type which contract is the composed of the contracts of `ty1` and `ty2`.
/// This type corresponds to the intersection of `ty1` and `ty2`
fn compose_contracts(ty1: Types, ty2: Types) -> Types {
    let c1 = ty1.contract();
    let c2 = ty2.contract();

    // composed = fun l => fun x => c1 l (c2 l x)
    let composed = RichTerm::fun(
        "_l".to_string(),
        RichTerm::fun(
            "_x".to_string(),
            RichTerm::app(
                RichTerm::app(c1, RichTerm::var("_l".to_string())),
                RichTerm::app(
                    RichTerm::app(c2, RichTerm::var("_l".to_string())),
                    RichTerm::var("_x".to_string()),
                ),
            ),
        ),
    );

    Types(AbsType::Flat(composed))
}

pub mod hashmap {
    use std::collections::HashMap;

    /// Split two hashmaps m1 and m2 in three parts (left,center,right), where left holds bindings
    /// `(key,value)` where key is not in `m2.keys()`, right is the dual (keys of m2 that are not in m1),
    /// and center holds bindings for keys that are both in m1 and m2
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
