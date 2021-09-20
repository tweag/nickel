
use crate::identifier::Ident;
use serde::{Deserialize, Serialize};

#[derive(Debug, Eq, Hash, PartialEq, Ord, PartialOrd, Clone, Serialize, Deserialize)]
pub enum Match{
    Assign(Ident,Ident),
    Simple(Ident),
}

#[derive(Debug, Eq, Hash, PartialEq, Ord, PartialOrd, Clone, Serialize, Deserialize)]
pub enum Destruct{
    Record(Vec<Match>),
    List(Vec<Match>),
    Empty,
}
