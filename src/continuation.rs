use eval::{Closure, Stack, Enviroment};
use term::Term;
use std::collections::HashMap;

#[derive(Debug)]
pub enum Continuation {
    Ite(Closure, Closure),
    Plus0(Enviroment, Closure),
    Plus1(Enviroment, f64),
    IsNum(),
    IsBool(),
    IsFun(),
}


pub fn continuate(cont: Continuation, clos: &mut Closure, stack: &mut Stack) {
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
        // isNum
        Continuation::IsNum() => {
            let mut value = false;
            if let Closure {
                body: Term::Num(_),
                env: _,
            } = *clos
            {
                value = true;
            }
            *clos = Closure {
                body: Term::Bool(value),
                env: HashMap::new(),
            };
        }
        // isBool
        Continuation::IsBool() => {
            let mut value = false;
            if let Closure {
                body: Term::Bool(_),
                env: _,
            } = *clos
            {
                value = true;
            }

            *clos = Closure {
                body: Term::Bool(value),
                env: HashMap::new(),
            };
        }
        // isFun
        Continuation::IsFun() => {
            let mut value = false;
            if let Closure {
                body: Term::Fun(_, _),
                env: _,
            } = *clos
            {
                value = true;
            }

            *clos = Closure {
                body: Term::Bool(value),
                env: HashMap::new(),
            };
        }
    }
}
