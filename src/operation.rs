use eval::{Closure, EvalError};
use label::TyPath;
use stack::Stack;
use term::{BinaryOp, Term, UnaryOp};

#[derive(Debug, PartialEq)]
pub enum OperationCont {
    Op1(UnaryOp),
    Op2First(BinaryOp, Closure),
    Op2Second(BinaryOp, Closure),
}

pub fn continuate_operation(
    cont: OperationCont,
    mut clos: Closure,
    stack: &mut Stack,
) -> Result<Closure, EvalError> {
    match cont {
        OperationCont::Op1(u_op) => process_unary_operation(u_op, clos, stack),
        OperationCont::Op2First(b_op, mut snd_clos) => {
            std::mem::swap(&mut clos, &mut snd_clos);
            stack.push_op_cont(OperationCont::Op2Second(b_op, snd_clos));
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
    match u_op {
        UnaryOp::Ite() => {
            if let Closure {
                body: Term::Bool(b),
                env: _,
            } = clos
            {
                if stack.count_args() >= 2 {
                    let fst = stack.pop_arg().expect("Condition already checked.");
                    let snd = stack.pop_arg().expect("Condition already checked.");

                    Ok(if b { fst } else { snd })
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
            } = clos
            {
                // TODO Discuss and decide on this comparison for 0 on f64
                Ok(Closure::atomic_closure(Term::Bool(n == 0.)))
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
            } = clos
            {
                Ok(Closure::atomic_closure(Term::Bool(true)))
            } else {
                Ok(Closure::atomic_closure(Term::Bool(false)))
            }
        }
        UnaryOp::IsBool() => {
            if let Closure {
                body: Term::Bool(_),
                env: _,
            } = clos
            {
                Ok(Closure::atomic_closure(Term::Bool(true)))
            } else {
                Ok(Closure::atomic_closure(Term::Bool(false)))
            }
        }
        UnaryOp::IsFun() => {
            if let Closure {
                body: Term::Fun(_, _),
                env: _,
            } = clos
            {
                Ok(Closure::atomic_closure(Term::Bool(true)))
            } else {
                Ok(Closure::atomic_closure(Term::Bool(false)))
            }
        }
        UnaryOp::Blame() => {
            if let Closure {
                body: Term::Lbl(l),
                env: _,
            } = clos
            {
                Err(EvalError::BlameError(l))
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Label, got {:?}",
                    clos.body
                )))
            }
        }
        UnaryOp::ChangePolarity() => {
            if let Closure {
                body: Term::Lbl(mut l),
                env: _,
            } = clos
            {
                l.polarity = !l.polarity;
                Ok(Closure::atomic_closure(Term::Lbl(l)))
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Label, got {:?}",
                    clos.body
                )))
            }
        }
        UnaryOp::GoDom() => {
            if let Closure {
                body: Term::Lbl(mut l),
                env: _,
            } = clos
            {
                l.path = TyPath::Domain(Box::new(l.path.clone()));
                Ok(Closure::atomic_closure(Term::Lbl(l)))
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Label, got {:?}",
                    clos.body
                )))
            }
        }
        UnaryOp::GoCodom() => {
            if let Closure {
                body: Term::Lbl(mut l),
                env: _,
            } = clos
            {
                l.path = TyPath::Codomain(Box::new(l.path.clone()));
                Ok(Closure::atomic_closure(Term::Lbl(l)))
            } else {
                Err(EvalError::TypeError(format!(
                    "Expected Label, got {:?}",
                    clos.body
                )))
            }
        }
        UnaryOp::Tag(s) => {
            if let Closure {
                body: Term::Lbl(mut l),
                env: _,
            } = clos
            {
                l.tag.push_str("\n");
                l.tag.push_str(&s);
                Ok(Closure::atomic_closure(Term::Lbl(l)))
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
    clos: Closure,
    _stack: &mut Stack,
) -> Result<Closure, EvalError> {
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
                } = clos
                {
                    Ok(Closure::atomic_closure(Term::Num(n1 + n2)))
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

        clos = continuate_operation(cont, clos, &mut stack).unwrap();

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

        clos = continuate_operation(cont, clos, &mut stack).unwrap();

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

        clos = continuate_operation(cont, clos, &mut stack).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(13.0),
                env: some_env()
            }
        );
    }

}
