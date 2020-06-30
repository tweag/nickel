use crate::position::{ShowWithSource, SourceMapper};

#[derive(Debug, Clone, PartialEq)]
pub enum TyPath {
    Nil(),
    Domain(Box<TyPath>),
    Codomain(Box<TyPath>),
}

impl ShowWithSource for TyPath {
    fn show_append(&self, s: &mut String, _m: &SourceMapper) {
        s.push_str(&format!("{:?}", self));
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    pub tag: String,
    pub l: usize,
    pub r: usize,
    pub polarity: bool,
    pub path: TyPath,
}
