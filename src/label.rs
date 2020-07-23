use crate::position::RawSpan;

#[derive(Debug, Clone, PartialEq)]
pub enum TyPath {
    Nil(),
    Domain(Box<TyPath>),
    Codomain(Box<TyPath>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    pub tag: String,
    pub span: RawSpan,
    pub polarity: bool,
    pub path: TyPath,
}
