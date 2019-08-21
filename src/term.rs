use identifier::Ident;
use label::Label;

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    Var(Ident),
    Bool(bool),
    Num(f64),
    Fun(Vec<Ident>, Box<Term>),
    Let(Ident, Box<Term>, Box<Term>),
    App(Box<Term>, Box<Term>),
    Ite(Box<Term>, Box<Term>, Box<Term>),
    Plus(Box<Term>, Box<Term>),
    IsNum(Box<Term>),
    IsBool(Box<Term>),
    IsFun(Box<Term>),
    Blame(Label),
}
