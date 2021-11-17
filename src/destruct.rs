use crate::identifier::Ident;
use crate::label::Label;
use crate::term::{Contract, MetaValue, RecordAttrs, RichTerm, Term};
use crate::types::{AbsType, Types};
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone)]
pub enum Match {
    Assign(Ident, MetaValue, (Option<Ident>, Destruct)),
    Simple(Ident, MetaValue),
}

#[derive(Debug, PartialEq, Clone)]
pub enum LastMatch {
    Match(Match),
    Ellipsis(Option<Ident>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Destruct {
    Record(Vec<Match>, bool, Option<Ident>),
    List(Vec<Match>),
    Empty,
}

impl Destruct {
    pub fn as_contract(self) -> MetaValue {
        println!("{:#?}", self);
        let open = self.opened().clone();
        MetaValue {
            contracts: vec![Contract {
                types: Types(AbsType::Flat(
                    Term::Record(
                        self.inner()
                            .iter()
                            .map(|m| (m.ident(), m.as_meta()))
                            .collect(),
                        RecordAttrs { open },
                    )
                    .into(),
                )),
                label: Label::dummy(),
            }],
            ..MetaValue::new()
        }
    }

    fn inner(self) -> Vec<Match> {
        match self {
            Destruct::Record(i, _, _) | Destruct::List(i) => i,
            Destruct::Empty => unreachable!(),
        }
    }

    pub fn opened(&self) -> bool {
        match self {
            Destruct::Record(_, o, _) => *o,
            _ => false,
        }
    }
}

impl Match {
    pub fn as_meta(&self) -> RichTerm {
        match self {
            Match::Assign(_, m, _) | Match::Simple(_, m) => Term::MetaValue(m.clone()),
        }
        .into()
    }

    pub fn ident(&self) -> Ident {
        match self {
            Match::Simple(id, _) | Match::Assign(id, _, _) => id.clone(),
        }
    }
}
