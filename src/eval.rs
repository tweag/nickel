use continuation::{continuate_operation, OperationCont};
use identifier::Ident;
use label::Label;
use stack::Stack;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use term::Term;

pub type Enviroment = HashMap<Ident, Rc<RefCell<Closure>>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    pub body: Term,
    pub env: Enviroment,
}

impl Closure {
    pub fn atomic_closure(body: Term) -> Closure {
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

pub fn eval(t0: Term) -> Result<Term, EvalError> {
    let empty_env = HashMap::new();
    let mut clos = Closure {
        body: t0,
        env: empty_env,
    };
    let mut stack = Stack::new();

    loop {
        match clos {
            // Var
            Closure {
                body: Term::Var(x),
                env,
            } => {
                let mut thunk = Rc::clone(env.get(&x).expect(&format!("Unbound variable {:?}", x)));
                std::mem::drop(env); // thunk may be a 1RC pointer
                if !is_value(&thunk.borrow().body) {
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
            Closure {
                body: Term::App(t1, t2),
                env,
            } => {
                stack.push_arg(Closure {
                    body: *t2,
                    env: env.clone(),
                });
                clos = Closure { body: *t1, env };
            }
            // Let
            Closure {
                body: Term::Let(x, s, t),
                mut env,
            } => {
                let thunk = Rc::new(RefCell::new(Closure {
                    body: *s,
                    env: env.clone(),
                }));
                env.insert(x, Rc::clone(&thunk));
                clos = Closure { body: *t, env: env };
            }
            // Unary Operation
            Closure {
                body: Term::Op1(op, t),
                env,
            } => {
                stack.push_op_cont(OperationCont::Op1(op));
                clos = Closure { body: *t, env };
            }
            // Binary Operation
            Closure {
                body: Term::Op2(op, fst, snd),
                env,
            } => {
                stack.push_op_cont(OperationCont::Op2First(
                    op,
                    Closure {
                        body: *snd,
                        env: env.clone(),
                    },
                ));
                clos = Closure { body: *fst, env };
            }
            // Blame
            Closure {
                body: Term::Blame(t),
                env: _,
            } => {
                if let Term::Lbl(l) = *t {
                    return Err(EvalError::BlameError(l));
                } else {
                    panic!("I still don't know how to properly treat a label");
                }
            }
            // Update
            _ if 0 < stack.count_thunks() => {
                while let Some(thunk) = stack.pop_thunk() {
                    if let Some(safe_thunk) = Weak::upgrade(&thunk) {
                        *safe_thunk.borrow_mut() = clos.clone();
                    }
                }
            }
            // Continuate Operation
            _ if 0 < stack.count_conts() => {
                if let Err(s) = continuate_operation(
                    stack.pop_op_cont().expect("Condition already checked"),
                    &mut clos,
                    &mut stack,
                ) {
                    // An operation can only raise an error if it's wrongly applied
                    // ... at least how the current implementation goes
                    return Err(EvalError::TypeError(s));
                }
            }
            // Call
            Closure {
                body: Term::Fun(mut xs, t),
                mut env,
            } => {
                if xs.len() <= stack.count_args() {
                    let args = &mut stack;
                    for x in xs.drain(..).rev() {
                        let arg = args.pop_arg().expect("Condition already checked.");
                        let thunk = Rc::new(RefCell::new(arg));
                        env.insert(x, thunk);
                    }
                    clos = Closure { body: *t, env: env }
                } else {
                    clos = Closure {
                        body: Term::Fun(xs, t),
                        env: env,
                    };
                    break;
                }
            }

            _ => {
                break;
            }
        }
    }

    Ok(clos.body)
}

#[cfg(test)]
mod tests {
    use super::*;
    use continuation::{BinaryOp, UnaryOp};

    fn app(t0: Term, t1: Term) -> Term {
        Term::App(Box::new(t0), Box::new(t1))
    }

    fn var(id: &str) -> Term {
        Term::Var(Ident(id.to_string()))
    }

    fn let_in(id: &str, e: Term, t: Term) -> Term {
        Term::Let(Ident(id.to_string()), Box::new(e), Box::new(t))
    }

    fn ite(c: Term, t: Term, e: Term) -> Term {
        app(app(Term::Op1(UnaryOp::Ite(), Box::new(c)), t), e)
    }

    fn plus(t0: Term, t1: Term) -> Term {
        Term::Op2(BinaryOp::Plus(), Box::new(t0), Box::new(t1))
    }

    #[test]
    fn identity_over_values() {
        let num = Term::Num(45.3);
        assert_eq!(Ok(num.clone()), eval(num));

        let boolean = Term::Bool(true);
        assert_eq!(Ok(boolean.clone()), eval(boolean));

        let lambda = Term::Fun(
            vec![Ident("x".to_string()), Ident("y".to_string())],
            Box::new(app(var("y"), var("x"))),
        );
        assert_eq!(Ok(lambda.clone()), eval(lambda));
    }

    #[test]
    fn blame_panics() {
        let label = Label {
            tag: "testing".to_string(),
            l: 0,
            r: 1,
        };
        assert_eq!(
            Err(EvalError::BlameError(label.clone())),
            eval(Term::Blame(Box::new(Term::Lbl(label))))
        );
    }

    #[test]
    #[should_panic]
    fn lone_var_panics() {
        eval(var("unbound"));
    }

    #[test]
    fn simple_app() {
        let t = app(
            Term::Fun(vec![Ident("x".to_string())], Box::new(var("x"))),
            Term::Num(5.0),
        );

        assert_eq!(Ok(Term::Num(5.0)), eval(t));
    }

    #[test]
    fn simple_let() {
        let t = let_in("x", Term::Num(5.0), var("x"));

        assert_eq!(Ok(Term::Num(5.0)), eval(t));
    }

    #[test]
    fn simple_ite() {
        let t = ite(Term::Bool(true), Term::Num(5.0), Term::Bool(false));

        assert_eq!(Ok(Term::Num(5.0)), eval(t));
    }

    #[test]
    fn simple_plus() {
        let t = plus(Term::Num(5.0), Term::Num(7.5));

        assert_eq!(Ok(Term::Num(12.5)), eval(t));
    }

    #[test]
    fn simple_is_zero() {
        let t = Term::Op1(UnaryOp::IsZero(), Box::new(Term::Num(7.0)));

        assert_eq!(Ok(Term::Bool(false)), eval(t));
    }

    #[test]
    fn asking_for_various_types() {
        let num = Term::Op1(UnaryOp::IsNum(), Box::new(Term::Num(45.3)));
        assert_eq!(Ok(Term::Bool(true)), eval(num));

        let boolean = Term::Op1(UnaryOp::IsBool(), Box::new(Term::Bool(true)));
        assert_eq!(Ok(Term::Bool(true)), eval(boolean));

        let lambda = Term::Op1(
            UnaryOp::IsFun(),
            Box::new(Term::Fun(
                vec![Ident("x".to_string()), Ident("y".to_string())],
                Box::new(app(var("y"), var("x"))),
            )),
        );
        assert_eq!(Ok(Term::Bool(true)), eval(lambda));
    }
}
