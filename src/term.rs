use identifier::Ident;
use label::Label;
use types::{AbsType, Types};

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

impl Term {
    pub fn apply_to_rich_terms<F>(&mut self, func: F)
    where
        F: Fn(&mut RichTerm),
    {
        use self::Term::*;
        match self {
            Bool(_) | Num(_) | Lbl(_) | Var(_) => {}
            Fun(_, ref mut t)
            | Op1(_, ref mut t)
            | Promise(_, _, ref mut t)
            | Assume(_, _, ref mut t) => {
                func(t);
            }
            Let(_, ref mut t1, ref mut t2)
            | App(ref mut t1, ref mut t2)
            | Op2(_, ref mut t1, ref mut t2) => {
                func(t1);
                func(t2);
            }
        }
    }
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

impl UnaryOp {
    pub fn get_type(&self) -> Types {
        match self {
            UnaryOp::Ite() => {
                println!("ITE unsupported");
                Types(AbsType::dyn())
            }
            UnaryOp::IsZero() => Types(AbsType::arrow(
                Box::new(Types(AbsType::num())),
                Box::new(Types(AbsType::bool())),
            )),
            UnaryOp::IsNum() | UnaryOp::IsBool() | UnaryOp::IsFun() => Types(AbsType::arrow(
                Box::new(Types(AbsType::dyn())),
                Box::new(Types(AbsType::bool())),
            )),
            UnaryOp::Blame() => Types(AbsType::arrow(
                Box::new(Types(AbsType::dyn())),
                Box::new(Types(AbsType::dyn())),
            )),
            UnaryOp::ChangePolarity() | UnaryOp::GoDom() | UnaryOp::GoCodom() | UnaryOp::Tag(_) => {
                Types(AbsType::arrow(
                    Box::new(Types(AbsType::dyn())),
                    Box::new(Types(AbsType::dyn())),
                ))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Plus(),
}

impl BinaryOp {
    pub fn get_type(&self) -> Types {
        match self {
            BinaryOp::Plus() => Types(AbsType::arrow(
                Box::new(Types(AbsType::num())),
                Box::new(Types(AbsType::arrow(
                    Box::new(Types(AbsType::num())),
                    Box::new(Types(AbsType::num())),
                ))),
            )),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct RichTerm {
    pub term: Box<Term>,
    pub pos: Option<(usize, usize)>,
}

impl RichTerm {
    pub fn new(t: Term) -> RichTerm {
        RichTerm {
            term: Box::new(t),
            pos: None,
        }
    }

    pub fn clean_pos(&mut self) {
        self.pos = None;
        self.term
            .apply_to_rich_terms(|rt: &mut Self| rt.clean_pos());
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
