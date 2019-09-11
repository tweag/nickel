use identifier::Ident;
use label::Label;
use operation::{BinaryOp, UnaryOp};

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    // Values
    Bool(bool),
    Num(f64),
    Fun(Vec<Ident>, Box<Term>),
    Lbl(Label),
    // Other lambda
    Let(Ident, Box<Term>, Box<Term>),
    App(Box<Term>, Box<Term>),
    Var(Ident),
    // Primitives
    Op1(UnaryOp, Box<Term>),
    Op2(BinaryOp, Box<Term>, Box<Term>),
    // Typing
    Promise(Box<Term>, Label, Box<Term>),
    Assume(Box<Term>, Label, Box<Term>),
}
