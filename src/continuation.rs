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

pub fn continuate(cont: Continuation, clos: &mut Closure, stack: &mut Stack) -> Result<(), String> {
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
                Ok(())
            } else {
                Err(format!("Expected Bool, got {:?}", clos))
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
                Ok(())
            } else {
                Err(format!("Expected Num, got {:?}", clos))
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
                
                Ok(())
            } else {
                Err(format!("Expected Num, got {:?}", clos))
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

            Ok(())
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

            Ok(())
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

            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn some_env() -> Enviroment {
        HashMap::new()
    }

    #[test]
    fn ite_continuation() {
        let cont = Continuation::Ite(some_env(), Term::Num(5.0), Term::Bool(true));
        let mut clos = Closure {
            body: Term::Bool(true),
            env: some_env(),
        };
        let mut stack = Stack::new();

        continuate(cont, &mut clos, &mut stack).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(5.0),
                env: some_env()
            }
        );
    }

    #[test]
    fn plus_first_term_continuation() {
        let cont = Continuation::Plus0(Closure {
            body: Term::Num(6.0),
            env: some_env(),
        });
        let mut clos = Closure {
            body: Term::Num(7.0),
            env: some_env(),
        };
        let mut stack = Stack::new();

        continuate(cont, &mut clos, &mut stack).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(6.0),
                env: some_env()
            }
        );

        assert_eq!(1, stack.count_conts());
        assert_eq!(
            Continuation::Plus1(7.0),
            stack.pop_cont().expect("Condition already checked.")
        );
    }

    #[test]
    fn plus_second_term_continuation() {
        let cont = Continuation::Plus1(6.0);
        let mut clos = Closure {
            body: Term::Num(7.0),
            env: some_env(),
        };
        let mut stack = Stack::new();

        continuate(cont, &mut clos, &mut stack).unwrap();

        assert_eq!(
            clos,
            Closure {
                body: Term::Num(13.0),
                env: some_env()
            }
        );
    }

}
