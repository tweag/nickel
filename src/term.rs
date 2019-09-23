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
        Term::App(rt1, rt2).into()
    }

    pub fn var(s: String) -> RichTerm {
        Term::Var(Ident(s)).into()
    }

    pub fn let_in(id: &str, e: RichTerm, t: RichTerm) -> RichTerm {
        Term::Let(Ident(id.to_string()), e, t).into()
    }

    pub fn ite(c: RichTerm, t: RichTerm, e: RichTerm) -> RichTerm {
        RichTerm::app(RichTerm::app(Term::Op1(UnaryOp::Ite(), c).into(), t), e)
    }

    pub fn plus(t0: RichTerm, t1: RichTerm) -> RichTerm {
        Term::Op2(BinaryOp::Plus(), t0, t1).into()
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
