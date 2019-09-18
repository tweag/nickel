use identifier::Ident;
use label::Label;
use types::Types;

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    // Values
    Bool(bool),
    Num(f64),
    Fun(Ident, RichTerm),
    Lbl(Label),
    // Other lambda
    Let(Ident, RichTerm, RichTerm),
    App(RichTerm, RichTerm),
    Var(Ident),
    // Primitives
    Op1(UnaryOp, RichTerm),
    Op2(BinaryOp, RichTerm, RichTerm),
    // Typing
    Promise(Types, Label, RichTerm),
    Assume(Types, Label, RichTerm),
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

#[derive(Debug, PartialEq, Clone)]
pub struct RichTerm {
    pub term: Box<Term>,
}

impl RichTerm {
    pub fn new(t: Term) -> RichTerm {
        RichTerm { term: Box::new(t) }
    }

    pub fn app(rt1: RichTerm, rt2: RichTerm) -> RichTerm {
        Self::new(Term::App(rt1, rt2))
    }

    pub fn var(s: String) -> RichTerm {
        Self::new(Term::Var(Ident(s)))
    }
}

impl From<RichTerm> for Term {
    fn from(rt: RichTerm) -> Self {
        *rt.term
    }
}

impl From<Term> for RichTerm {
    fn from(t: Term) -> Self {
        Self::new(t)
    }
}
