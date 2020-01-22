use crate::identifier::Ident;
use crate::term::{RichTerm, Term};
use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
pub enum AbsType<Ty> {
    Dyn(),
    Num(),
    Bool(),
    Sym(),
    Flat(RichTerm),
    Arrow(Ty, Ty),
    Var(Ident),
    Forall(Ident, Ty),
}

impl<Ty> AbsType<Ty> {
    pub fn map<To, F: Fn(Ty) -> To>(self, f: F) -> AbsType<To> {
        match self {
            AbsType::Dyn() => AbsType::Dyn(),
            AbsType::Num() => AbsType::Num(),
            AbsType::Bool() => AbsType::Bool(),
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
        }
    }
}
