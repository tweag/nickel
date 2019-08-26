use continuation::{continuate, Continuation};
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

fn is_value(_term: &Term) -> bool {
    false
}

pub fn eval(t0: Term) -> Term {
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
            // Ite
            Closure {
                body: Term::Ite(b, t, e),
                env,
            } => {
                stack.push_cont(Continuation::Ite(env.clone(), *t, *e));
                clos = Closure { body: *b, env };
            }
            // Plus
            Closure {
                body: Term::Plus(t1, t2),
                env,
            } => {
                stack.push_cont(Continuation::Plus0(Closure {
                    body: *t2,
                    env: env.clone(),
                }));
                clos = Closure { body: *t1, env };
            }
            // isNum
            Closure {
                body: Term::IsNum(t1),
                env,
            } => {
                stack.push_cont(Continuation::IsNum());
                clos = Closure { body: *t1, env };
            }
            // isBool
            Closure {
                body: Term::IsBool(t1),
                env,
            } => {
                stack.push_cont(Continuation::IsBool());
                clos = Closure { body: *t1, env };
            }
            // isFun
            Closure {
                body: Term::IsFun(t1),
                env,
            } => {
                stack.push_cont(Continuation::IsFun());
                clos = Closure { body: *t1, env };
            }
            // Blame
            Closure {
                body: Term::Blame(t),
                env: _,
            } => {
                blame(stack, *t);
            }
            // Update
            _ if 0 < stack.count_thunks() => {
                while let Some(thunk) = stack.pop_thunk() {
                    if let Some(safe_thunk) = Weak::upgrade(&thunk) {
                        *safe_thunk.borrow_mut() = clos.clone();
                    }
                }
            }
            // Continuate
            _ if 0 < stack.count_conts() => continuate(
                stack.pop_cont().expect("Condition already checked"),
                &mut clos,
                &mut stack,
            ),
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

    clos.body
}

fn blame(stack: Stack, t: Term) -> ! {
    for x in stack.into_iter() {
        println!("{:?}", x);
    }
    panic!("Reached Blame: {:?}", t);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn app(t0: Term, t1: Term) -> Term {
        Term::App(Box::new(t0), Box::new(t1))
    }

    fn var(id: &str) -> Term {
        Term::Var(Ident(id.to_string()))
    }

    #[test]
    fn identity_over_values() {
        let num = Term::Num(45.3);
        assert_eq!(num.clone(), eval(num));

        let boolean = Term::Bool(true);
        assert_eq!(boolean.clone(), eval(boolean));

        let lambda = Term::Fun(
            vec![Ident("x".to_string()), Ident("y".to_string())],
            Box::new(app(var("y"), var("x"))),
        );
        assert_eq!(lambda.clone(), eval(lambda));
    }

    #[test]
    #[should_panic]
    fn blame_panics() {
        eval(Term::Blame(Box::new(Term::Lbl(Label {
            tag: "testing".to_string(),
            l: 0,
            r: 1,
        }))));
    }

    #[test]
    #[should_panic]
    fn lone_var_panics() {
        eval(var("unbound"));
    }
}
