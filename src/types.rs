use term::RichTerm;

#[derive(Clone, PartialEq, Debug)]
pub enum Types {
    Dyn(),
    Num(),
    Bool(),
    Arrow(Box<Types>, Box<Types>),
}

impl Types {
    pub fn contract(&self) -> RichTerm {
        match self {
            Types::Dyn() => RichTerm::var("dyn".to_string()),
            Types::Num() => RichTerm::var("num".to_string()),
            Types::Bool() => RichTerm::var("bool".to_string()),
            Types::Arrow(s, t) => RichTerm::app(
                RichTerm::app(RichTerm::var("func".to_string()), s.contract()),
                t.contract(),
            ),
        }
    }
}
