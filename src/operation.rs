use eval::{Closure, EvalError};
use stack::Stack;
use term::Term;

#[derive(Debug, PartialEq)]
pub enum OperationCont {
    Op1(UnaryOp),
    Op2First(BinaryOp, Closure),
    Op2Second(BinaryOp, Closure),
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Ite(),
    IsZero(),
    IsNum(),
    IsBool(),
    IsFun(),
    Blame(),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Plus(),
}

pub fn continuate_operation(
    cont: OperationCont,
    clos: &mut Closure,
    stack: &mut Stack,
) -> Result<(), EvalError> {
    match cont {
        OperationCont::Op1(u_op) => process_unary_operation(u_op, clos, stack),
        OperationCont::Op2First(b_op, mut snd_clos) => {
            std::mem::swap(clos, &mut snd_clos);
            stack.push_op_cont(OperationCont::Op2Second(b_op, snd_clos));
            Ok(())
        }
        OperationCont::Op2Second(b_op, fst_clos) => {
            process_binary_operation(b_op, fst_clos, clos, stack)
        }
    }
}

fn process_unary_operation(
    u_op: UnaryOp,
    clos: &mut Closure,
    stack: &mut Stack,
) -> Result<(), EvalError> {
    match u_op {
        UnaryOp::Ite() => {
            if let Closure {
                body: Term::Bool(b),
                env: _,
            } = *clos
            {
                if stack.count_args() >= 2 {
                    let fst = stack.pop_arg().expect("Condition already checked.");
                    let snd = stack.pop_arg().expect("Condition already checked.");

                    *clos = if b { fst } else { snd };
                    Ok(())
                } else {
                    panic!("An If-Then-Else wasn't saturated")
                }
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Bool, got {:?}",
                    clos.body
                )))
            }
        }
        UnaryOp::IsZero() => {
            if let Closure {
                body: Term::Num(n),
                env: _,
            } = *clos
            {
                // TODO Discuss and decide on this comparison for 0 on f64
                *clos = Closure::atomic_closure(Term::Bool(n == 0.));
                Ok(())
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Num, got {:?}",
                    clos.body
                )))
            }
        }
        UnaryOp::IsNum() => {
            if let Closure {
                body: Term::Num(_),
                env: _,
            } = *clos
            {
                *clos = Closure::atomic_closure(Term::Bool(true));
            } else {
                *clos = Closure::atomic_closure(Term::Bool(false));
            }
            Ok(())
        }
        UnaryOp::IsBool() => {
            if let Closure {
                body: Term::Bool(_),
                env: _,
            } = *clos
            {
                *clos = Closure::atomic_closure(Term::Bool(true));
            } else {
                *clos = Closure::atomic_closure(Term::Bool(false));
            }
            Ok(())
        }
        UnaryOp::IsFun() => {
            if let Closure {
                body: Term::Fun(_, _),
                env: _,
            } = *clos
            {
                *clos = Closure::atomic_closure(Term::Bool(true));
            } else {
                *clos = Closure::atomic_closure(Term::Bool(false));
            }
            Ok(())
        }
        UnaryOp::Blame() => {
            if let Closure {
                body: Term::Lbl(ref l),
                env: _,
            } = *clos
            {
                Err(EvalError::BlameError(l.clone()))
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Label, got {:?}",
                    clos.body
                )))
            }
        }
    }
}

fn process_binary_operation(
    b_op: BinaryOp,
    fst_clos: Closure,
    clos: &mut Closure,
    _stack: &mut Stack,
) -> Result<(), EvalError> {
    match b_op {
        BinaryOp::Plus() => {
            if let Closure {
                body: Term::Num(n1),
                env: _,
            } = fst_clos
            {
                if let Closure {
                    body: Term::Num(n2),
                    env: _,
                } = *clos
                {
                    *clos = Closure::atomic_closure(Term::Num(n1 + n2));
                    Ok(())
                } else {
                    Err(EvalError::TypeError(format!(
                        "Expected Num, got {:?}",
                        clos.body
                    )))
                }
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Num, got {:?}",
                    fst_clos.body
                )))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use eval::Enviroment;
    use std::collections::HashMap;

    fn some_env() -> Enviroment {
        HashMap::new()
    }

    #[test]
    fn ite_operation() {
        let cont = OperationCont::Op1(UnaryOp::Ite());
        let mut stack = Stack::new();
        stack.push_arg(Closure::atomic_closure(Term::Num(5.0)));
        stack.push_arg(Closure::atomic_closure(Term::Num(46.0)));

        let mut clos = Closure {
            body: Term::Bool(true),
            env: some_env(),
        };

        continuate_operation(cont, &mut clos, &mut stack).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(46.0),
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
                body: Term::Num(6.0),
                env: some_env(),
            },
        );

        let mut clos = Closure {
            body: Term::Num(7.0),
            env: some_env(),
        };
        let mut stack = Stack::new();

        continuate_operation(cont, &mut clos, &mut stack).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(6.0),
                env: some_env()
            }
        );

        assert_eq!(1, stack.count_conts());
        assert_eq!(
            OperationCont::Op2Second(
                BinaryOp::Plus(),
                Closure {
                    body: Term::Num(7.0),
                    env: some_env(),
                }
            ),
            stack.pop_op_cont().expect("Condition already checked.")
        );
    }

    #[test]
    fn plus_second_term_operation() {
        let cont = OperationCont::Op2Second(
            BinaryOp::Plus(),
            Closure {
                body: Term::Num(7.0),
                env: some_env(),
            },
        );
        let mut clos = Closure {
            body: Term::Num(6.0),
            env: some_env(),
        };
        let mut stack = Stack::new();

        continuate_operation(cont, &mut clos, &mut stack).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(13.0),
                env: some_env()
            }
        );
    }

}
