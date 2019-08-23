use identifier::Ident;
use label::Label;

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    // Values
    Bool(bool),
    Num(f64),
    Fun(Vec<Ident>, Box<Term>),
    // Other lambda
    Let(Ident, Box<Term>, Box<Term>),
    App(Box<Term>, Box<Term>),
    Var(Ident),
    // Primitives
    Ite(Box<Term>, Box<Term>, Box<Term>),
    Plus(Box<Term>, Box<Term>),
    // Type Checking
    IsNum(Box<Term>),
    IsBool(Box<Term>),
    IsFun(Box<Term>),
    Blame(Box<Term>),
    Lbl(Label),
}
