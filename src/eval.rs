use identifier::Ident;
use label::Label;
use operation::{continuate_operation, OperationCont};
use stack::Stack;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use term::{RichTerm, Term};

pub type Enviroment = HashMap<Ident, Rc<RefCell<Closure>>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    pub body: RichTerm,
    pub env: Enviroment,
}

impl Closure {
    pub fn atomic_closure(body: RichTerm) -> Closure {
        Closure {
            body,
            env: HashMap::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum EvalError {
    BlameError(Label),
    TypeError(String),
}

fn is_value(_term: &Term) -> bool {
    false
}

pub fn eval(t0: RichTerm) -> Result<Term, EvalError> {
    let empty_env = HashMap::new();
    let mut clos = Closure {
        body: t0,
        env: empty_env,
    };
    let mut stack = Stack::new();

    loop {
        let Closure {
            body: RichTerm { term: boxed_term },
            mut env,
        } = clos;
        let term = *boxed_term;
        match term {
            // Var
            Term::Var(x) => {
                let mut thunk = Rc::clone(env.get(&x).expect(&format!("Unbound variable {:?}", x)));
                std::mem::drop(env); // thunk may be a 1RC pointer
                if !is_value(&thunk.borrow().body.term) {
                    stack.push_thunk(Rc::downgrade(&thunk));
                }
                match Rc::try_unwrap(thunk) {
                    Ok(c) => {
                        // thunk was the only strong ref to the closure
                        clos = c.into_inner();
                    }
                    Err(rc) => {
                        // We need to clone it, there are other strong refs
                        clos = rc.borrow().clone();
                    }
                }
            }
            // App
            Term::App(t1, t2) => {
                stack.push_arg(Closure {
                    body: t2,
                    env: env.clone(),
                });
                clos = Closure { body: t1, env };
            }
            // Let
            Term::Let(x, s, t) => {
                let thunk = Rc::new(RefCell::new(Closure {
                    body: s,
                    env: env.clone(),
                }));
                env.insert(x, Rc::clone(&thunk));
                clos = Closure { body: t, env: env };
            }
            // Unary Operation
            Term::Op1(op, t) => {
                stack.push_op_cont(OperationCont::Op1(op));
                clos = Closure { body: t, env };
            }
            // Binary Operation
            Term::Op2(op, fst, snd) => {
                stack.push_op_cont(OperationCont::Op2First(
                    op,
                    Closure {
                        body: snd,
                        env: env.clone(),
                    },
                ));
                clos = Closure { body: fst, env };
            }
            // Promise and Assume
            Term::Promise(ty, l, t) | Term::Assume(ty, l, t) => {
                stack.push_arg(Closure {
                    body: t,
                    env: env.clone(),
                });
                stack.push_arg(Closure::atomic_closure(RichTerm::new(Term::Lbl(l))));
                clos = Closure {
                    body: ty.contract(),
                    env,
                };
            }
            // Continuate Operation
            // Update
            _ if 0 < stack.count_thunks() || 0 < stack.count_conts() => {
                clos = Closure {
                    body: term.into(),
                    env,
                };
                if 0 < stack.count_thunks() {
                    while let Some(thunk) = stack.pop_thunk() {
                        if let Some(safe_thunk) = Weak::upgrade(&thunk) {
                            *safe_thunk.borrow_mut() = clos.clone();
                        }
                    }
                } else {
                    clos = continuate_operation(
                        stack.pop_op_cont().expect("Condition already checked"),
                        clos,
                        &mut stack,
                    )?;
                }
            }
            // Call
            Term::Fun(x, t) => {
                if 0 < stack.count_args() {
                    let thunk = Rc::new(RefCell::new(
                        stack.pop_arg().expect("Condition already checked."),
                    ));
                    env.insert(x, thunk);
                    clos = Closure { body: t, env }
                } else {
                    return Ok(Term::Fun(x, t));
                }
            }

            _ => {
                return Ok(term);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use label::TyPath;
    use term::UnaryOp;

    #[test]
    fn identity_over_values() {
        let num = Term::Num(45.3);
        assert_eq!(Ok(num.clone()), eval(num.into()));

        let boolean = Term::Bool(true);
        assert_eq!(Ok(boolean.clone()), eval(boolean.into()));

        let lambda = Term::Fun(
            Ident("x".to_string()),
            RichTerm::app(RichTerm::var("x".into()), RichTerm::var("x".into())),
        );
        assert_eq!(Ok(lambda.clone()), eval(lambda.into()));
    }

    #[test]
    fn blame_panics() {
        let label = Label {
            tag: "testing".to_string(),
            l: 0,
            r: 1,
            polarity: false,
            path: TyPath::Nil(),
        };
        assert_eq!(
            Err(EvalError::BlameError(label.clone())),
            eval(Term::Op1(UnaryOp::Blame(), Term::Lbl(label).into()).into())
        );
    }

    #[test]
    #[should_panic]
    fn lone_var_panics() {
        eval(RichTerm::var("unbound".into())).unwrap();
    }

    #[test]
    fn simple_app() {
        let t = RichTerm::app(
            Term::Fun(Ident("x".to_string()), RichTerm::var("x".into())).into(),
            Term::Num(5.0).into(),
        );

        assert_eq!(Ok(Term::Num(5.0)), eval(t));
    }

    #[test]
    fn simple_let() {
        let t = RichTerm::let_in("x", Term::Num(5.0).into(), RichTerm::var("x".into()));

        assert_eq!(Ok(Term::Num(5.0)), eval(t));
    }

    #[test]
    fn simple_ite() {
        let t = RichTerm::ite(
            Term::Bool(true).into(),
            Term::Num(5.0).into(),
            Term::Bool(false).into(),
        );

        assert_eq!(Ok(Term::Num(5.0)), eval(t));
    }

    #[test]
    fn simple_plus() {
        let t = RichTerm::plus(Term::Num(5.0).into(), Term::Num(7.5).into());

        assert_eq!(Ok(Term::Num(12.5)), eval(t));
    }

    #[test]
    fn simple_is_zero() {
        let t = Term::Op1(UnaryOp::IsZero(), Term::Num(7.0).into()).into();

        assert_eq!(Ok(Term::Bool(false)), eval(t));
    }

    #[test]
    fn asking_for_various_types() {
        let num = Term::Op1(UnaryOp::IsNum(), Term::Num(45.3).into()).into();
        assert_eq!(Ok(Term::Bool(true)), eval(num));

        let boolean = Term::Op1(UnaryOp::IsBool(), Term::Bool(true).into()).into();
        assert_eq!(Ok(Term::Bool(true)), eval(boolean));

        let lambda = Term::Op1(
            UnaryOp::IsFun(),
            Term::Fun(
                Ident("x".to_string()),
                RichTerm::app(RichTerm::var("x".into()), RichTerm::var("x".into())),
            )
            .into(),
        )
        .into();
        assert_eq!(Ok(Term::Bool(true)), eval(lambda));
    }
}
