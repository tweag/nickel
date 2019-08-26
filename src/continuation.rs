use eval::{Closure, Enviroment};
use stack::Stack;
use term::Term;

#[derive(Debug, PartialEq)]
pub enum Continuation {
    Ite(Enviroment, Term, Term),
    Plus0(Closure),
    Plus1(f64),
    IsNum(),
    IsBool(),
    IsFun(),
}

pub fn continuate(cont: Continuation, clos: &mut Closure, stack: &mut Stack) {
    match cont {
        // If Then Else
        Continuation::Ite(e, t1, t2) => {
            if let Closure {
                body: Term::Bool(b),
                env: _,
            } = *clos
            {
                *clos = Closure {
                    body: (if b { t1 } else { t2 }),
                    env: e,
                };
            } else {
                panic!("Expected Bool, got {:?}", clos);
            }
        }
        // Plus unapplied
        Continuation::Plus0(t) => {
            if let Closure {
                body: Term::Num(n),
                env: _,
            } = *clos
            {
                stack.push_cont(Continuation::Plus1(n));
                *clos = t;
            } else {
                panic!("Expected Num, got {:?}", clos);
            }
        }
        // Plus partially applied
        Continuation::Plus1(n) => {
            if let Closure {
                body: Term::Num(n2),
                env: _,
            } = *clos
            {
                *clos = Closure::atomic_closure(Term::Num(n + n2));
            } else {
                panic!("Expected Num, got {:?}", clos);
            }
        }
        // isNum
        Continuation::IsNum() => {
            if let Closure {
                body: Term::Num(_),
                env: _,
            } = *clos
            {
                *clos = Closure::atomic_closure(Term::Bool(true));
            } else {
                *clos = Closure::atomic_closure(Term::Bool(false));
            }
        }
        // isBool
        Continuation::IsBool() => {
            if let Closure {
                body: Term::Bool(_),
                env: _,
            } = *clos
            {
                *clos = Closure::atomic_closure(Term::Bool(true));
            } else {
                *clos = Closure::atomic_closure(Term::Bool(false));
            }
        }
        // isFun
        Continuation::IsFun() => {
            if let Closure {
                body: Term::Fun(_, _),
                env: _,
            } = *clos
            {
                *clos = Closure::atomic_closure(Term::Bool(true));
            } else {
                *clos = Closure::atomic_closure(Term::Bool(false));
            }
        }
    }
}
