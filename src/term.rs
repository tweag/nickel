use continuation::{BinaryOp, UnaryOp};
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
    Op1(UnaryOp, Box<Term>),
    Op2(BinaryOp, Box<Term>, Box<Term>),
    // Blame
    Blame(Box<Term>),
    Lbl(Label),
}
