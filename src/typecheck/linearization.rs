use std::{borrow::BorrowMut, cell::RefCell, rc::Rc};

use crate::environment::Environment as GenericEnvironment;
use crate::{
    identifier::Ident,
    position::{self, RawPos, TermPos},
    term::{self, Term},
};

use super::TypeWrapper;

pub trait LinearizationHost {
    fn add_term(&mut self, term: Box<Term>, position: TermPos, ty: TypeWrapper) {}
    fn add_usage(&mut self, decl: usize, usage: usize) {}
    fn linearize(&mut self) -> Linearization {
        Vec::new()
    }
    fn scope(&mut self) -> Self;
}

/// Stub linearization used during evaluation/run-time typechecking
pub struct StubHost;
impl LinearizationHost for StubHost {
    fn scope<'b>(&mut self) -> Self {
        Self
    }
}

pub type Environment = GenericEnvironment<Ident, usize>;
pub type Linearization = Vec<LinearizationItem>;

/// Linearization
pub struct AnalysisHost {
    env: Environment,
    lin: Rc<RefCell<Linearization>>,
    cached_lin: Option<Linearization>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TermKind {
    Structure,
    Declaration(Vec<usize>),
    Usage(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LinearizationItem {
    //term_: Box<Term>,
    pos: TermPos,
    ty: TypeWrapper,
    kind: TermKind,
}

impl LinearizationHost for AnalysisHost {
    fn add_term(&mut self, term: Box<Term>, pos: TermPos, ty: TypeWrapper) {
        match term.as_ref() {
            Term::Let(ident, _, _) => {
                self.env
                    .insert(ident.to_owned(), self.lin.as_ref().borrow().len());
                self.push_lin(LinearizationItem {
                    pos,
                    ty,
                    kind: TermKind::Declaration(Vec::new()),
                });
            }
            Term::Var(ident) => {
                let id = self.lin.as_ref().borrow().len();
                let parent = self
                    .env
                    .get(ident)
                    .expect("Declaration should be defined before is usage");
                self.push_lin(LinearizationItem {
                    pos,
                    ty,
                    kind: TermKind::Usage(parent),
                });
                self.add_usage(parent, id);
            }
            _ => self.push_lin(LinearizationItem {
                pos,
                ty,
                kind: TermKind::Structure,
            }),
        }
    }

    fn add_usage(&mut self, decl: usize, usage: usize) {
        let mut lin = (*self.lin).borrow_mut();
        match lin.get_mut(decl).expect("Coundt find parent").kind {
            TermKind::Structure => unreachable!(),
            TermKind::Usage(_) => unreachable!(),
            TermKind::Declaration(ref mut usages) => usages.push(usage),
        };
    }

    fn scope(&mut self) -> Self {
        AnalysisHost {
            env: self.env.clone(),
            lin: self.lin.clone(),
            cached_lin: None,
        }
    }

    fn linearize(&mut self) -> Linearization {
        let mut lin = self.lin.as_ref().borrow().clone();
        lin.sort_by_key(|item| match item.pos {
            TermPos::Original(span) => (span.src_id, span.start),
            TermPos::Inherited(span) => (span.src_id, span.start),
            TermPos::None => unreachable!(),
        });

        lin
    }
}
impl AnalysisHost {
    pub fn new() -> Self {
        todo!()
    }

    fn push_lin(&mut self, item: LinearizationItem) {
        self.lin.as_ref().borrow_mut().push(item);
    }
}
