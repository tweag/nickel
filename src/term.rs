use crate::identifier::Ident;
use crate::label::Label;
use crate::types::Types;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    // Values
    Bool(bool),
    Num(f64),
    Str(String),
    Fun(Ident, RichTerm),
    Lbl(Label),

    // Other lambda
    Let(Ident, RichTerm, RichTerm),
    App(RichTerm, RichTerm),
    Var(Ident),

    // Enums
    Enum(Ident),

    // Records
    Record(HashMap<Ident, RichTerm>),

    // Lists
    List(Vec<RichTerm>),

    // Primitives
    Op1(UnaryOp<RichTerm>, RichTerm),
    Op2(BinaryOp<RichTerm>, RichTerm, RichTerm),

    // Typing
    Promise(Types, Label, RichTerm),
    Assume(Types, Label, RichTerm),
    Sym(i32),
    Wrapped(i32, RichTerm),
    // Enriched terms
    Contract(Types),
    DefaultValue(RichTerm),
    ContractWithDefault(Types, RichTerm),
    Docstring(String, RichTerm),
}

impl Term {
    pub fn apply_to_rich_terms<F>(&mut self, func: F)
    where
        F: Fn(&mut RichTerm),
    {
        use self::Term::*;
        match self {
            Op1(UnaryOp::Switch(ref mut map, ref mut def), ref mut t) => {
                map.iter_mut().for_each(|e| {
                    let (_, t) = e;
                    func(t);
                });
                func(t);
                if let Some(def) = def {
                    func(def)
                }
            }
            Record(ref mut static_map) => {
                static_map.iter_mut().for_each(|e| {
                    let (_, t) = e;
                    func(t);
                });
            }
            Op2(BinaryOp::DynExtend(ref mut t), ref mut t1, ref mut t2) => {
                func(t);
                func(t1);
                func(t2)
            }

            Bool(_) | Num(_) | Str(_) | Lbl(_) | Var(_) | Sym(_) | Enum(_) | Contract(_) => {}
            Fun(_, ref mut t)
            | Op1(_, ref mut t)
            | Promise(_, _, ref mut t)
            | Assume(_, _, ref mut t)
            | Wrapped(_, ref mut t)
            | DefaultValue(ref mut t)
            | Docstring(_, ref mut t)
            | ContractWithDefault(_, ref mut t) => {
                func(t);
            }
            Let(_, ref mut t1, ref mut t2)
            | App(ref mut t1, ref mut t2)
            | Op2(_, ref mut t1, ref mut t2) => {
                func(t1);
                func(t2);
            }
            List(ref mut terms) => terms.iter_mut().for_each(|t| {
                func(t);
            }),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp<CapturedTerm> {
    Ite(),

    IsZero(),

    IsNum(),
    IsBool(),
    IsStr(),
    IsFun(),
    IsList(),

    Blame(),

    Embed(Ident),
    /// This is a hacky way to deal with this for now.
    ///
    /// Ideally it should change to eliminate the dependency with RichTerm
    /// in the future.
    Switch(HashMap<Ident, CapturedTerm>, Option<CapturedTerm>),

    StaticAccess(Ident),
    MapRec(CapturedTerm),

    ChangePolarity(),
    Pol(),
    GoDom(),
    GoCodom(),
    Tag(String),

    Wrap(),

    Seq(),
    DeepSeq(),

    ListHead(),
    ListTail(),
    ListLength(),
}

impl<Ty> UnaryOp<Ty> {
    pub fn map<To, F: Fn(Ty) -> To>(self, f: F) -> UnaryOp<To> {
        use UnaryOp::*;

        match self {
            Switch(m, op) => Switch(
                m.into_iter()
                    .map(|e| {
                        let (id, t) = e;
                        (id, f(t))
                    })
                    .collect(),
                op.map(f),
            ),
            MapRec(t) => MapRec(f(t)),

            Ite() => Ite(),

            IsZero() => IsZero(),

            IsNum() => IsNum(),
            IsBool() => IsBool(),
            IsStr() => IsStr(),
            IsFun() => IsFun(),
            IsList() => IsList(),

            Blame() => Blame(),

            Embed(id) => Embed(id),

            StaticAccess(id) => StaticAccess(id),

            ChangePolarity() => ChangePolarity(),
            Pol() => Pol(),
            GoDom() => GoDom(),
            GoCodom() => GoCodom(),
            Tag(s) => Tag(s),

            Wrap() => Wrap(),

            Seq() => Seq(),
            DeepSeq() => DeepSeq(),

            ListHead() => ListHead(),
            ListTail() => ListTail(),
            ListLength() => ListLength(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp<CapturedTerm> {
    Plus(),
    PlusStr(),
    Unwrap(),
    EqBool(),
    DynExtend(CapturedTerm),
    DynRemove(),
    DynAccess(),
    HasField(),
    ListConcat(),
    ListMap(),
    ListElemAt(),
    Merge(),
}

impl<Ty> BinaryOp<Ty> {
    pub fn map<To, F: Fn(Ty) -> To>(self, f: F) -> BinaryOp<To> {
        use BinaryOp::*;

        match self {
            DynExtend(t) => DynExtend(f(t)),
            Plus() => Plus(),
            PlusStr() => PlusStr(),
            Unwrap() => Unwrap(),
            EqBool() => EqBool(),
            DynRemove() => DynRemove(),
            DynAccess() => DynAccess(),
            HasField() => HasField(),
            ListConcat() => ListConcat(),
            ListMap() => ListMap(),
            ListElemAt() => ListElemAt(),
            Merge() => Merge(),
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

impl AsRef<Term> for RichTerm {
    fn as_ref(&self) -> &Term {
        &self.term
    }
}

impl From<Term> for RichTerm {
    fn from(t: Term) -> Self {
        Self::new(t)
    }
}
