use identifier::Ident;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use term::Term;

type Enviroment = HashMap<Ident, Rc<RefCell<Closure>>>;

#[derive(Clone, Debug)]
struct Closure {
    body: Term,
    env: Enviroment,
}

#[derive(Debug)]
enum Continuation {
    Ite(Closure, Closure),
    Plus0(Enviroment, Closure),
    Plus1(Enviroment, f64),
}

#[derive(Debug)]
enum Marker {
    Arg(Closure),
    Thunk(Weak<RefCell<Closure>>),
    Cont(Continuation),
}

impl Marker {
    pub fn is_arg(&self) -> bool {
        match *self {
            Marker::Arg(_) => true,
            Marker::Thunk(_) => false,
            Marker::Cont(_) => false,
        }
    }

    pub fn is_thunk(&self) -> bool {
        match *self {
            Marker::Arg(_) => false,
            Marker::Thunk(_) => true,
            Marker::Cont(_) => false,
        }
    }

    pub fn is_cont(&self) -> bool {
        match *self {
            Marker::Arg(_) => false,
            Marker::Thunk(_) => false,
            Marker::Cont(_) => true,
        }
    }
}

#[derive(Debug)]
struct Stack(Vec<Marker>);

impl IntoIterator for Stack {
    type Item = Marker;
    type IntoIter = ::std::vec::IntoIter<Marker>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Stack {
    pub fn new() -> Stack {
        Stack(Vec::new())
    }

    fn count<P>(&self, pred: P) -> usize
    where
        P: Fn(&Marker) -> bool,
    {
        let mut count = 0;
        for marker in self.0.iter().rev() {
            if pred(marker) {
                count += 1;
            } else {
                break;
            }
        }
        count
    }

    /// Count the number of arguments at the top of the stack.
    pub fn count_args(&self) -> usize {
        Stack::count(self, Marker::is_arg)
    }

    pub fn count_thunks(&self) -> usize {
        Stack::count(self, Marker::is_thunk)
    }

    pub fn count_conts(&self) -> usize {
        Stack::count(self, Marker::is_cont)
    }

    pub fn push_arg(&mut self, arg: Closure) {
        self.0.push(Marker::Arg(arg))
    }

    pub fn push_thunk(&mut self, thunk: Weak<RefCell<Closure>>) {
        self.0.push(Marker::Thunk(thunk))
    }

    pub fn push_cont(&mut self, cont: Continuation) {
        self.0.push(Marker::Cont(cont))
    }

    pub fn pop_arg(&mut self) -> Option<Closure> {
        match self.0.pop() {
            Some(Marker::Arg(arg)) => Some(arg),
            Some(m) => {
                self.0.push(m);
                None
            }
            _ => None,
        }
    }

    pub fn pop_thunk(&mut self) -> Option<Weak<RefCell<Closure>>> {
        match self.0.pop() {
            Some(Marker::Thunk(thunk)) => Some(thunk),
            Some(m) => {
                self.0.push(m);
                None
            }
            _ => None,
        }
    }

    pub fn pop_cont(&mut self) -> Option<Continuation> {
        match self.0.pop() {
            Some(Marker::Cont(cont)) => Some(cont),
            Some(m) => {
                self.0.push(m);
                None
            }
            _ => None,
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
                let thunk = Rc::clone(env.get(&x).expect(&format!("Unbound variable {:?}", x)));
                if !is_value(&thunk.borrow().body) {
                    stack.push_thunk(Rc::downgrade(&thunk));
                }
                clos = thunk.borrow().clone();
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
                env: mut env,
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
                env: env,
            } => {
                stack.push_cont(Continuation::Ite(
                    Closure {
                        body: *t,
                        env: env.clone(),
                    },
                    Closure {
                        body: *e,
                        env: env.clone(),
                    },
                ));
                clos = Closure { body: *b, env };
            }
            // Plus
            Closure {
                body: Term::Plus(t1, t2),
                env,
            } => {
                stack.push_cont(Continuation::Plus0(
                    env.clone(),
                    Closure {
                        body: *t2,
                        env: env.clone(),
                    },
                ));
                clos = Closure { body: *t1, env };
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
                env: mut env,
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

fn continuate(cont: Continuation, clos: &mut Closure, stack: &mut Stack) {
    match cont {
        // If Then Else
        Continuation::Ite(t, e) => {
            if let Closure {
                body: Term::Bool(b),
                env: _,
            } = *clos
            {
                *clos = if b { t } else { e };
            } else {
                panic!("Expected Bool, got {:?}", clos);
            }
        }
        // Plus unapplied
        Continuation::Plus0(plus_env, t) => {
            if let Closure {
                body: Term::Num(n),
                env: _,
            } = *clos
            {
                stack.push_cont(Continuation::Plus1(plus_env, n));
                *clos = t;
            } else {
                panic!("Expected Num, got {:?}", clos);
            }
        }
        // Plus partially applied
        Continuation::Plus1(plus_env, n) => {
            if let Closure {
                body: Term::Num(n2),
                env: _,
            } = *clos
            {
                *clos = Closure {
                    body: Term::Num(n + n2),
                    env: plus_env,
                };
            } else {
                panic!("Expected Num, got {:?}", clos);
            }
        }
        _ => {
            panic!("Unimplemented continuation");
        }
    }
}
