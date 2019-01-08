use identifier::Ident;
use term::Term;
use std::collections::HashMap;

struct Closure {
    body: Term,
    env: HashMap<Ident, Box<Closure>>,
}

enum Marker {
    Arg(Closure),
    Thunk(Box<Closure>),
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

    pub fn push_thunk(&mut self, thunk: Box<Closure>) {
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

    pub fn pop_thunk(&mut self) -> Option<Box<Closure>> {
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

fn is_value(term: &Term) -> bool {
    false
}

fn eval(t0: Term) -> Term {
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
                let thunk = *env.get(&x).expect("Unbound variable");
                clos = *thunk;
                if !is_value(&clos.body) {
                    stack.push_thunk(thunk);
                }
            }
            // App
            Closure {
                body: Term::App(t1, t2),
                env,
            } => {
                clos = Closure { body: *t1, env };
                stack.push_arg(Closure { body: *t2, env });
            }
            // Call
            Closure {
                body: Term::Fun(xs, t),
                env,
            } if xs.len() <= stack.count_args() => {
                let new_env = env.clone();
                let params = xs.rev();
                let args = stack;
                while let (Some(x), Some(arg)) = (params.pop(), args.pop_arg())
                {
                    let thunk = Box::new(arg);
                    new_env.insert(x, arg);
                }
                clos = Closure {
                    body: *t,
                    env: new_env,
                }
            }
            // Update
            _ if 0 <= stack.count_thunks() => {
                while let Some(thunk) = stack.pop_thunk() {
                    *thunk = clos;
                }
            }
        }
    }

    Term::Bool(true);
}
