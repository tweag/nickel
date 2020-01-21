use crate::identifier::Ident;
use crate::term::{RichTerm, Term, UnaryOp};
use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
pub enum AbsType<Ty> {
    Dyn(),
    Num(),
    Bool(),
    Str(),
    Sym(),
    Flat(RichTerm),
    Arrow(Ty, Ty),
    Var(Ident),
    Forall(Ident, Ty),

    // A kind system would be nice
    RowEmpty(),
    RowExtend(Ident, Ty /* Row */),
    Enum(Ty /* Row */),
}

impl<Ty> AbsType<Ty> {
    pub fn map<To, F: Fn(Ty) -> To>(self, f: F) -> AbsType<To> {
        match self {
            AbsType::Dyn() => AbsType::Dyn(),
            AbsType::Num() => AbsType::Num(),
            AbsType::Bool() => AbsType::Bool(),
            AbsType::Str() => AbsType::Str(),
            AbsType::Sym() => AbsType::Sym(),
            AbsType::Flat(t) => AbsType::Flat(t),
            AbsType::Arrow(s, t) => {
                let fs = f(s);
                let ft = f(t);

                AbsType::Arrow(fs, ft)
            }
            AbsType::Var(i) => AbsType::Var(i),
            AbsType::Forall(i, t) => {
                let ft = f(t);

                AbsType::Forall(i, ft)
            }
            AbsType::RowEmpty() => AbsType::RowEmpty(),
            AbsType::RowExtend(id, t) => AbsType::RowExtend(id, f(t)),
            AbsType::Enum(t) => AbsType::Enum(f(t)),
        }
    }

    pub fn arrow(s: Ty, t: Ty) -> Self {
        AbsType::Arrow(s, t)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Types(pub AbsType<Box<Types>>);

impl Types {
    pub fn contract(&self) -> RichTerm {
        let mut sy = 0;
        self.contract_open(HashMap::new(), true, &mut sy)
    }

    pub fn contract_open(
        &self,
        mut h: HashMap<Ident, RichTerm>,
        pol: bool,
        sy: &mut i32,
    ) -> RichTerm {
        match self.0 {
            AbsType::Dyn() => RichTerm::var("dyn".to_string()),
            AbsType::Num() => RichTerm::var("num".to_string()),
            AbsType::Bool() => RichTerm::var("bool".to_string()),
            AbsType::Str() => RichTerm::var("string".to_string()),
            AbsType::Sym() => panic!("Are you trying to check a Sym at runtime?"),
            AbsType::Arrow(ref s, ref t) => RichTerm::app(
                RichTerm::app(
                    RichTerm::var("func".to_string()),
                    s.contract_open(h.clone(), !pol, sy),
                ),
                t.contract_open(h, pol, sy),
            ),
            AbsType::Flat(ref t) => t.clone(),
            AbsType::Var(ref i) => {
                let rt = h.get(i).expect(&format!("Unbound type variable {:?}", i));
                rt.clone()
            }
            AbsType::Forall(ref i, ref t) => {
                let inst_var = RichTerm::app(
                    RichTerm::app(
                        RichTerm::var("forall_var".to_string()),
                        Term::Sym(*sy).into(),
                    ),
                    Term::Bool(pol).into(),
                );

                h.insert(i.clone(), inst_var);
                *sy += 1;
                t.contract_open(h, pol, sy)
            }
            AbsType::RowEmpty() | AbsType::RowExtend(_, _) => {
                panic!("These types should not be checked at runtime!")
            }
            AbsType::Enum(ref r) => {
                fn form(ty: Types, h: HashMap<Ident, RichTerm>) -> RichTerm {
                    match ty.0 {
                        AbsType::RowEmpty() => RichTerm::var("fail".to_string()),
                        AbsType::RowExtend(id, rest) => {
                            let rest_contract = form(*rest, h);
                            let mut map = HashMap::new();
                            map.insert(id, Term::Bool(true).into());

                            RichTerm::app(
                                RichTerm::app(
                                    RichTerm::var("row_extend".to_string()),
                                    rest_contract,
                                ),
                                Term::Fun(
                                    Ident("x".to_string()),
                                    Term::Op1(
                                        UnaryOp::Switch(map, Some(Term::Bool(false).into())),
                                        Term::Var(Ident("x".to_string())).into(),
                                    )
                                    .into(),
                                )
                                .into(),
                            )
                        }
                        AbsType::Var(ref i) => {
                            let rt = h.get(i).expect(&format!("Unbound type variable {:?}", i));
                            rt.clone()
                        }
                        not_row => panic!("It should be a row!! {:?}", not_row),
                    }
                }

                form(*r.clone(), h)
            }
        }
    }
}
