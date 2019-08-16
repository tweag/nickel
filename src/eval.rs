use identifier::Ident;
use term::Term;
use std::collections::HashMap;
use std::rc::{Rc, Weak};
use std::cell::RefCell;

type Enviroment = HashMap<Ident, Rc<RefCell<Closure>>>;

#[derive(Clone, Debug)]
struct Closure {
    body: Term,
    env: Enviroment,
}

#[derive(Debug)]
enum Marker {
    Arg(Closure),
    Thunk(Weak<RefCell<Closure>>),
}

impl Marker {
    pub fn is_arg(&self) -> bool {
        match *self {
            Marker::Arg(_) => true,
            Marker::Thunk(_) => false,
        }
    }

    pub fn is_thunk(&self) -> bool {
        match *self {
            Marker::Arg(_) => false,
            Marker::Thunk(_) => true,
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

    fn count<P>(&self, pred: P) -> usize where P: Fn(&Marker) -> bool {
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
    
    pub fn push_arg(&mut self, arg: Closure) {
        self.0.push(Marker::Arg(arg))
    }

    pub fn push_thunk(&mut self, thunk: Weak<RefCell<Closure>>) {
        self.0.push(Marker::Thunk(thunk))
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
                stack.push_arg(Closure { body: *t2, env: env.clone() });
                clos = Closure { body: *t1, env };
            }
            // Let
            Closure {
                body: Term::Let(x, s, t),
                env: mut env,
            } => {
                let thunk = Rc::new(RefCell::new(Closure{ body: *s, env: env.clone()}));
                env.insert(x, Rc::clone(&thunk));
                clos = Closure { body: *t, env: env};
            }
            // Update
            _ if 0 < stack.count_thunks() => {
                while let Some(thunk) = stack.pop_thunk() {
                    if let Some(safe_thunk) =  Weak::upgrade(&thunk) {
                        *safe_thunk.borrow_mut() = clos.clone();
                    }
                }
            }
            // Call
            Closure {
                body: Term::Fun(mut xs, t),
                env: mut env,
            } => {
                if xs.len() <= stack.count_args() {
                    let args = &mut stack;
                    for x in xs.drain(..).rev(){
                        let arg = args.pop_arg().expect("Condition already checked.");
                        let thunk = Rc::new(RefCell::new(arg));
                        env.insert(x, thunk);
                    }
                    clos = Closure {
                        body: *t,
                        env: env,
                    }
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
