use std::{hash::Hash, ops::Range};

use codespan::FileId;
use nickel_lang_core::{
    position::RawSpan,
    term::{RichTerm, SharedTerm, Term},
};

// A term that uses a pointer to Term to implement Eq and Hash.
#[derive(Clone, Debug)]
pub struct RichTermPtr(pub RichTerm);

impl PartialEq for RichTermPtr {
    fn eq(&self, other: &Self) -> bool {
        SharedTerm::ptr_eq(&self.0.term, &other.0.term)
    }
}

impl Hash for RichTermPtr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0.term.as_ref() as *const Term).hash(state)
    }
}

impl Eq for RichTermPtr {}

pub trait RawSpanExt {
    fn to_range(self) -> (FileId, Range<usize>);
}

impl RawSpanExt for RawSpan {
    fn to_range(self) -> (FileId, Range<usize>) {
        (self.src_id, (self.start.to_usize()..self.end.to_usize()))
    }
}
