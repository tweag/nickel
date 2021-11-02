use crate::identifier::Ident;
use crate::label::Label;
use crate::term::{Contract, MetaValue, RecordAttrs, RichTerm, Term};
use crate::types::{AbsType, Types};
use serde::{Deserialize, Serialize};

#[derive(Debug, Eq, Hash, PartialEq, Ord, PartialOrd, Clone, Serialize, Deserialize)]
pub enum Match {
    Assign(Ident, (Option<Ident>, Destruct)),
    Simple(Ident),
}

#[derive(Debug, Eq, Hash, PartialEq, Ord, PartialOrd, Clone, Serialize, Deserialize)]
pub enum Destruct {
    Record(Vec<Match>),
    List(Vec<Match>),
    Empty,
}

impl Destruct {
    pub fn as_contract(self) -> MetaValue {
        MetaValue {
            contracts: vec![Contract {
                types: Types(AbsType::Flat(
                    Term::Record(
                        self.inner()
                            .iter()
                            .map(|m| (m.ident(), m.as_meta()))
                            .collect(),
                        RecordAttrs { open: false },
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
            Destruct::Record(i) | Destruct::List(i) => i,
            Destruct::Empty => unreachable!(),
        }
    }
}

impl Match {
    pub fn as_meta(&self) -> RichTerm {
        Term::MetaValue(MetaValue {
            contracts: vec![Contract {
                types: Types(AbsType::Dyn()),
                label: Label::dummy(),
            }],
            ..MetaValue::new()
        })
        .into()
    }

    pub fn ident(&self) -> Ident {
        match self {
            Match::Simple(id) | Match::Assign(id, _) => id.clone(),
        }
    }
}
