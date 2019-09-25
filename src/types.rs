use term::RichTerm;

#[derive(Clone, PartialEq, Debug)]
pub enum AbsType<Ty> {
    Dyn(),
    Num(),
    Bool(),
    Flat(RichTerm),
    Arrow(Ty, Ty),
}

impl<Ty> AbsType<Ty> {
    pub fn map<To, F: Fn(Ty) -> To>(self, f: F) -> AbsType<To> {
        match self {
            AbsType::Dyn() => AbsType::Dyn(),
            AbsType::Num() => AbsType::Num(),
            AbsType::Bool() => AbsType::Bool(),
            AbsType::Arrow(s, t) => {
                let fs = f(s);
                let ft = f(t);

                AbsType::Arrow(fs, ft)
            }
        }
    }

    pub fn bool() -> Self {
        AbsType::Bool()
    }

    pub fn num() -> Self {
        AbsType::Num()
    }

    pub fn dyn() -> Self {
        AbsType::Dyn()
    }

    pub fn arrow(s: Ty, t: Ty) -> Self {
        AbsType::Arrow(s, t)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Types(pub AbsType<Box<Types>>);

impl Types {
    pub fn contract(&self) -> RichTerm {
        match self.0 {
            AbsType::Dyn() => RichTerm::var("dyn".to_string()),
            AbsType::Num() => RichTerm::var("num".to_string()),
            AbsType::Bool() => RichTerm::var("bool".to_string()),
            AbsType::Arrow(ref s, ref t) => RichTerm::app(
                RichTerm::app(RichTerm::var("func".to_string()), s.contract()),
                t.contract(),
            ),
            Types::Flat(t) => t.clone(),
        }
    }
}
