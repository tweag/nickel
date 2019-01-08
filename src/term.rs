use identifier::Ident;

#[derive(Debug, PartialEq)]
pub enum Term {
    Var(Ident),
    Bool(bool),
    Num(f64),
    Fun(Vec<Ident>, Box<Term>),
    Let(Ident, Box<Term>, Box<Term>),
    App(Box<Term>, Box<Term>),
}
