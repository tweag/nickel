use term::RichTerm;

#[derive(Clone, PartialEq, Debug)]
pub enum AbsType<Ty> {
    Dyn(),
    Num(),
    Bool(),
    Flat(RichTerm),
    Arrow(Ty, Ty),
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
