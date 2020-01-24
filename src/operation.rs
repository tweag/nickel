use crate::eval::{CallStack, Closure, EvalError};
use crate::identifier::Ident;
use crate::label::TyPath;
use crate::stack::Stack;
use crate::term::{BinaryOp, RichTerm, Term, UnaryOp};

#[derive(Debug, PartialEq)]
pub enum OperationCont {
    Op1(UnaryOp),
    Op2First(BinaryOp, Closure),
    Op2Second(BinaryOp, Closure),
}

pub fn continuate_operation(
    mut clos: Closure,
    stack: &mut Stack,
    call_stack: &mut CallStack,
) -> Result<Closure, EvalError> {
    let (cont, cs_len) = stack.pop_op_cont().expect("Condition already checked");
    call_stack.truncate(cs_len);
    match cont {
        OperationCont::Op1(u_op) => process_unary_operation(u_op, clos, stack),
        OperationCont::Op2First(b_op, mut snd_clos) => {
            std::mem::swap(&mut clos, &mut snd_clos);
            stack.push_op_cont(OperationCont::Op2Second(b_op, snd_clos), cs_len);
            Ok(clos)
        }
        OperationCont::Op2Second(b_op, fst_clos) => {
            process_binary_operation(b_op, fst_clos, clos, stack)
        }
    }
}

fn process_unary_operation(
    u_op: UnaryOp,
    clos: Closure,
    stack: &mut Stack,
) -> Result<Closure, EvalError> {
    let Closure {
        body: RichTerm { term: t, .. },
        env: _env,
    } = clos;
    match u_op {
        UnaryOp::Ite() => {
            if let Term::Bool(b) = *t {
                if stack.count_args() >= 2 {
                    let (fst, _) = stack.pop_arg().expect("Condition already checked.");
                    let (snd, _) = stack.pop_arg().expect("Condition already checked.");

                    Ok(if b { fst } else { snd })
                } else {
                    panic!("An If-Then-Else wasn't saturated")
                }
            } else {
                Err(EvalError::TypeError(format!("Expected Bool, got {:?}", *t)))
            }
        }
        UnaryOp::IsZero() => {
            if let Term::Num(n) = *t {
                // TODO Discuss and decide on this comparison for 0 on f64
                Ok(Closure::atomic_closure(Term::Bool(n == 0.).into()))
            } else {
                Err(EvalError::TypeError(format!("Expected Num, got {:?}", *t)))
            }
        }
        UnaryOp::IsNum() => {
            if let Term::Num(_) = *t {
                Ok(Closure::atomic_closure(Term::Bool(true).into()))
            } else {
                Ok(Closure::atomic_closure(Term::Bool(false).into()))
            }
        }
        UnaryOp::IsBool() => {
            if let Term::Bool(_) = *t {
                Ok(Closure::atomic_closure(Term::Bool(true).into()))
            } else {
                Ok(Closure::atomic_closure(Term::Bool(false).into()))
            }
        }
        UnaryOp::IsStr() => {
            if let Term::Str(_) = *t {
                Ok(Closure::atomic_closure(Term::Bool(true).into()))
            } else {
                Ok(Closure::atomic_closure(Term::Bool(false).into()))
            }
        }
        UnaryOp::IsFun() => {
            if let Term::Fun(_, _) = *t {
                Ok(Closure::atomic_closure(Term::Bool(true).into()))
            } else {
                Ok(Closure::atomic_closure(Term::Bool(false).into()))
            }
        }
        UnaryOp::Blame() => {
            if let Term::Lbl(l) = *t {
                Err(EvalError::BlameError(l, None))
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Label, got {:?}",
                    *t
                )))
            }
        }
        UnaryOp::ChangePolarity() => {
            if let Term::Lbl(mut l) = *t {
                l.polarity = !l.polarity;
                Ok(Closure::atomic_closure(Term::Lbl(l).into()))
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Label, got {:?}",
                    *t
                )))
            }
        }
        UnaryOp::Pol() => {
            if let Term::Lbl(l) = *t {
                Ok(Closure::atomic_closure(Term::Bool(l.polarity).into()))
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Label, got {:?}",
                    *t
                )))
            }
        }
        UnaryOp::GoDom() => {
            if let Term::Lbl(mut l) = *t {
                l.path = TyPath::Domain(Box::new(l.path.clone()));
                Ok(Closure::atomic_closure(Term::Lbl(l).into()))
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Label, got {:?}",
                    *t
                )))
            }
        }
        UnaryOp::GoCodom() => {
            if let Term::Lbl(mut l) = *t {
                l.path = TyPath::Codomain(Box::new(l.path.clone()));
                Ok(Closure::atomic_closure(Term::Lbl(l).into()))
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Label, got {:?}",
                    *t
                )))
            }
        }
        UnaryOp::Tag(s) => {
            if let Term::Lbl(mut l) = *t {
                l.tag.push_str("\n");
                l.tag.push_str(&s);
                Ok(Closure::atomic_closure(Term::Lbl(l).into()))
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Label, got {:?}",
                    *t
                )))
            }
        }
        UnaryOp::Wrap() => {
            if let Term::Sym(s) = *t {
                Ok(Closure::atomic_closure(
                    Term::Fun(
                        Ident("x".to_string()),
                        Term::Wrapped(s, RichTerm::var("x".to_string())).into(),
                    )
                    .into(),
                ))
            } else {
                Err(EvalError::TypeError(format!("Expected Sym, got {:?}", *t)))
            }
        }
    }
}

fn process_binary_operation(
    b_op: BinaryOp,
    fst_clos: Closure,
    clos: Closure,
    _stack: &mut Stack,
) -> Result<Closure, EvalError> {
    let Closure {
        body: RichTerm { term: t1, .. },
        env: _env1,
    } = fst_clos;
    let Closure {
        body: RichTerm { term: t2, .. },
        env: env2,
    } = clos;
    match b_op {
        BinaryOp::Plus() => {
            if let Term::Num(n1) = *t1 {
                if let Term::Num(n2) = *t2 {
                    Ok(Closure::atomic_closure(Term::Num(n1 + n2).into()))
                } else {
                    Err(EvalError::TypeError(format!("Expected Num, got {:?}", *t2)))
                }
            } else {
                Err(EvalError::TypeError(format!("Expected Num, got {:?}", *t1)))
            }
        }
        BinaryOp::PlusStr() => {
            if let Term::Str(s1) = *t1 {
                if let Term::Str(s2) = *t2 {
                    Ok(Closure::atomic_closure(Term::Str(s1 + &s2).into()))
                } else {
                    Err(EvalError::TypeError(format!("Expected Str, got {:?}", *t2)))
                }
            } else {
                Err(EvalError::TypeError(format!("Expected Str, got {:?}", *t1)))
            }
        }
        BinaryOp::Unwrap() => {
            if let Term::Sym(s1) = *t1 {
                // Return a function that either behaves like the identity or
                // const unwrapped_term

                Ok(if let Term::Wrapped(s2, t) = *t2 {
                    if s1 == s2 {
                        Closure {
                            body: Term::Fun(Ident("-invld".to_string()), t).into(),
                            env: env2,
                        }
                    } else {
                        Closure::atomic_closure(
                            Term::Fun(Ident("x".to_string()), RichTerm::var("x".to_string()))
                                .into(),
                        )
                    }
                } else {
                    Closure::atomic_closure(
                        Term::Fun(Ident("x".to_string()), RichTerm::var("x".to_string())).into(),
                    )
                })
            } else {
                Err(EvalError::TypeError(format!("Expected Sym, got {:?}", *t1)))
            }
        }
        BinaryOp::EqBool() => {
            if let Term::Bool(b1) = *t1 {
                if let Term::Bool(b2) = *t2 {
                    Ok(Closure::atomic_closure(Term::Bool(b1 == b2).into()))
                } else {
                    Err(EvalError::TypeError(format!(
                        "Expected Bool, got {:?}",
                        *t2
                    )))
                }
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Bool, got {:?}",
                    *t1
                )))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{CallStack, Environment};
    use std::collections::HashMap;

    fn some_env() -> Environment {
        HashMap::new()
    }

    #[test]
    fn ite_operation() {
        let cont = OperationCont::Op1(UnaryOp::Ite());
        let mut stack = Stack::new();
        stack.push_arg(Closure::atomic_closure(Term::Num(5.0).into()), None);
        stack.push_arg(Closure::atomic_closure(Term::Num(46.0).into()), None);

        let mut clos = Closure {
            body: Term::Bool(true).into(),
            env: some_env(),
        };

        stack.push_op_cont(cont, 0);
        let mut call_stack = CallStack::new();

        clos = continuate_operation(clos, &mut stack, &mut call_stack).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(46.0).into(),
                env: some_env()
            }
        );
        assert_eq!(0, stack.count_args());
    }

    #[test]
    fn plus_first_term_operation() {
        let cont = OperationCont::Op2First(
            BinaryOp::Plus(),
            Closure {
                body: Term::Num(6.0).into(),
                env: some_env(),
            },
        );

        let mut clos = Closure {
            body: Term::Num(7.0).into(),
            env: some_env(),
        };
        let mut stack = Stack::new();
        stack.push_op_cont(cont, 0);
        let mut call_stack = CallStack::new();

        clos = continuate_operation(clos, &mut stack, &mut call_stack).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(6.0).into(),
                env: some_env()
            }
        );

        assert_eq!(1, stack.count_conts());
        assert_eq!(
            (
                OperationCont::Op2Second(
                    BinaryOp::Plus(),
                    Closure {
                        body: Term::Num(7.0).into(),
                        env: some_env(),
                    }
                ),
                0
            ),
            stack.pop_op_cont().expect("Condition already checked.")
        );
    }

    #[test]
    fn plus_second_term_operation() {
        let cont = OperationCont::Op2Second(
            BinaryOp::Plus(),
            Closure {
                body: Term::Num(7.0).into(),
                env: some_env(),
            },
        );
        let mut clos = Closure {
            body: Term::Num(6.0).into(),
            env: some_env(),
        };
        let mut stack = Stack::new();
        stack.push_op_cont(cont, 0);
        let mut call_stack = CallStack::new();

        clos = continuate_operation(clos, &mut stack, &mut call_stack).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(13.0).into(),
                env: some_env()
            }
        );
    }

}
