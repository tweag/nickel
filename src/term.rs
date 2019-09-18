use identifier::Ident;
use label::Label;
use types::Types;

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    // Values
    Bool(bool),
    Num(f64),
    Fun(Ident, Box<Term>),
    Lbl(Label),
    // Other lambda
    Let(Ident, Box<Term>, Box<Term>),
    App(Box<Term>, Box<Term>),
    Var(Ident),
    // Primitives
    Op1(UnaryOp, Box<Term>),
    Op2(BinaryOp, Box<Term>, Box<Term>),
    // Typing
    Promise(Types, Label, Box<Term>),
    Assume(Types, Label, Box<Term>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Ite(),
    IsZero(),
    IsNum(),
    IsBool(),
    IsFun(),
    Blame(),
    ChangePolarity(),
    GoDom(),
    GoCodom(),
    Tag(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Plus(),
}
