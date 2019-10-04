use identifier::Ident;
use label::Label;
use operation::{continuate_operation, OperationCont};
use stack::Stack;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use term::{RichTerm, Term};

pub type Enviroment = HashMap<Ident, (Rc<RefCell<Closure>>, IdentKind)>;
pub type CallStack = Vec<StackElem>;

#[derive(Debug, PartialEq, Clone)]
pub enum StackElem {
    App(Option<(usize, usize)>),
    Var(IdentKind, Ident, Option<(usize, usize)>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum IdentKind {
    Let(),
    Lam(),
}

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
    BlameError(Label, Option<CallStack>),
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
    let mut call_stack = CallStack::new();
    let mut stack = Stack::new();

    loop {
        let Closure {
            body: RichTerm {
                term: boxed_term,
                pos,
            },
            mut env,
        } = clos;
        let term = *boxed_term;
        match term {
            // Var
            Term::Var(x) => {
                let (thunk, id_kind) = env.remove(&x).expect(&format!("Unbound variable {:?}", x));
                std::mem::drop(env); // thunk may be a 1RC pointer
                if !is_value(&thunk.borrow().body.term) {
                    stack.push_thunk(Rc::downgrade(&thunk));
                }
                call_stack.push(StackElem::Var(id_kind, x, pos));
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
                stack.push_arg(
                    Closure {
                        body: t2,
                        env: env.clone(),
                    },
                    pos,
                );
                clos = Closure { body: t1, env };
            }
            // Let
            Term::Let(x, s, t) => {
                let thunk = Rc::new(RefCell::new(Closure {
                    body: s,
                    env: env.clone(),
                }));
                env.insert(x, (Rc::clone(&thunk), IdentKind::Let()));
                clos = Closure { body: t, env: env };
            }
            // Unary Operation
            Term::Op1(op, t) => {
                stack.push_op_cont(OperationCont::Op1(op), call_stack.len());
                clos = Closure { body: t, env };
            }
            // Binary Operation
            Term::Op2(op, fst, snd) => {
                stack.push_op_cont(
                    OperationCont::Op2First(
                        op,
                        Closure {
                            body: snd,
                            env: env.clone(),
                        },
                    ),
                    call_stack.len(),
                );
                clos = Closure { body: fst, env };
            }
            // Promise and Assume
            Term::Promise(ty, l, t) | Term::Assume(ty, l, t) => {
                stack.push_arg(
                    Closure {
                        body: t,
                        env: env.clone(),
                    },
                    None,
                );
                stack.push_arg(Closure::atomic_closure(RichTerm::new(Term::Lbl(l))), None);
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
                    let mut cont_result = continuate_operation(clos, &mut stack, &mut call_stack);

                    if let Err(EvalError::BlameError(l, _)) = cont_result {
                        return Err(EvalError::BlameError(l, Some(call_stack)));
                    }
                    clos = cont_result?;
                }
            }
            // Call
            Term::Fun(x, t) => {
                if 0 < stack.count_args() {
                    let (arg, pos) = stack.pop_arg().expect("Condition already checked.");
                    call_stack.push(StackElem::App(pos));
                    let thunk = Rc::new(RefCell::new(arg));
                    env.insert(x, (thunk, IdentKind::Lam()));
                    clos = Closure { body: t, env }
                } else {
                    return Ok(Term::Fun(x, t));
                }
            }

            t => {
                if 0 < stack.count_args() {
                    let (arg, _) = stack.pop_arg().expect("Condition already checked.");
                    return Err(EvalError::TypeError(format!(
                        "The term {:?} was applied to {:?}",
                        t, arg.body
                    )));
                } else {
                    return Ok(t);
                }
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
        if let Err(EvalError::BlameError(l, _)) =
            eval(Term::Op1(UnaryOp::Blame(), Term::Lbl(label.clone()).into()).into())
        {
            assert_eq!(l, label);
        } else {
            panic!("This evaluation should've returned a BlameError!");
        }
    }

    #[test]
    #[should_panic]
    fn lone_var_panics() {
        eval(RichTerm::var("unbound".into())).unwrap();
    }

    #[test]
    fn only_fun_are_applicable() {
        eval(RichTerm::app(Term::Bool(true).into(), Term::Num(45.).into()).into()).unwrap_err();
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
