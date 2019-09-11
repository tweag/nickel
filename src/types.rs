use identifier::Ident;
use term::Term;

#[derive(Clone, PartialEq, Debug)]
pub enum Types {
    Dyn(),
    Num(),
    Bool(),
    Arrow(Box<Types>, Box<Types>),
}

impl Types {
    pub fn contract(&self) -> Term {
        match self {
            Types::Dyn() => Term::Var(Ident("dyn".to_string())),
            Types::Num() => Term::Var(Ident("num".to_string())),
            Types::Bool() => Term::Var(Ident("bool".to_string())),
            Types::Arrow(s, t) => Term::App(
                Box::new(Term::App(
                    Box::new(Term::Var(Ident("func".to_string()))),
                    Box::new(s.contract()),
                )),
                Box::new(t.contract()),
            ),
        }
    }
}
