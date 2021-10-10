use std::{collections::HashMap, marker::PhantomData, str::EncodeUtf16};

use super::reporting::NameResolution;
use super::{State, TypeWrapper, UnifTable};
use crate::environment::Environment as GenericEnvironment;
use crate::typecheck::to_type;
use crate::types::{AbsType, Types};
use crate::{identifier::Ident, position::TermPos, term::Term};

pub struct Linearization<LinearizationState> {
    pub state: LinearizationState,
}

impl Linearization<()> {
    pub fn completed(completed: Completed) -> Linearization<Completed> {
        Linearization { state: completed }
    }
    pub fn building<T: Default>() -> Linearization<Building<T>> {
        Linearization {
            state: Building {
                resource: T::default(),
            },
        }
    }
}

pub struct Building<T> {
    resource: T,
}

#[derive(Debug)]
pub struct Completed {
    pub lin: Vec<LinearizationItem<Resolved>>,
    pub id_mapping: HashMap<usize, usize>,
}

pub trait ResolutionState {}
type Resolved = Types;
impl ResolutionState for Resolved {}

type Unresolved = TypeWrapper;
impl ResolutionState for Unresolved {}

trait LinearizationState {}
impl<T> LinearizationState for Building<T> {}

impl LinearizationState for () {}

#[derive(Debug, Clone, PartialEq)]
pub struct LinearizationItem<ResolutionState> {
    //term_: Box<Term>,
    pub id: usize,
    pub pos: TermPos,
    pub ty: ResolutionState,
    pub kind: TermKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TermKind {
    Structure,
    Declaration(Vec<usize>),
    Usage(Option<usize>),
}

pub trait Linearizer<L, S> {
    fn add_term(
        &mut self,
        lin: &mut Linearization<Building<L>>,
        term: &Term,
        pos: TermPos,
        ty: TypeWrapper,
    ) {
    }
    fn linearize(self, lin: Linearization<Building<L>>, extra: &S) -> Linearization<Completed>
    where
        Self: Sized,
    {
        Linearization {
            state: Completed {
                lin: Vec::new(),
                id_mapping: HashMap::new(),
            },
        }
    }
    fn scope(&self) -> Self;
}

pub struct StubHost<L>(PhantomData<L>);
impl<L, S> Linearizer<L, S> for StubHost<L> {
    fn scope(&self) -> Self {
        StubHost::new()
    }
}

impl<L> StubHost<L> {
    pub fn new() -> StubHost<L> {
        StubHost(PhantomData)
    }
}

pub type Environment = GenericEnvironment<Ident, usize>;
pub struct AnalysisHost {
    env: Environment,
}

impl AnalysisHost {
    pub fn new() -> Self {
        AnalysisHost {
            env: Environment::new(),
        }
    }
}

impl Linearizer<Vec<LinearizationItem<Unresolved>>, UnifTable> for AnalysisHost {
    fn add_term(
        &mut self,
        lin: &mut Linearization<Building<Vec<LinearizationItem<Unresolved>>>>,
        term: &Term,
        pos: TermPos,
        ty: TypeWrapper,
    ) {
        if pos == TermPos::None {
            eprintln!("{:?}", term);
            return;
        }
        let id = lin.state.resource.len();
        match term {
            Term::Let(ident, _, _) => {
                self.env.insert(ident.to_owned(), lin.state.resource.len());
                lin.push(LinearizationItem {
                    id,
                    pos,
                    ty,
                    kind: TermKind::Declaration(Vec::new()),
                });
            }
            Term::Var(ident) => {
                let parent = self.env.get(ident);
                lin.push(LinearizationItem {
                    id,
                    pos,
                    ty,
                    kind: TermKind::Usage(parent),
                });
                if let Some(parent) = parent {
                    lin.add_usage(parent, id);
                }
            }
            _ => lin.push(LinearizationItem {
                id,
                pos,
                ty,
                kind: TermKind::Structure,
            }),
        }
    }

    fn linearize(
        self,
        lin: Linearization<Building<Vec<LinearizationItem<Unresolved>>>>,
        extra: &UnifTable,
    ) -> Linearization<Completed> {
        let mut lin_ = lin.state.resource;
        eprintln!("linearizing");
        lin_.sort_by_key(|item| match item.pos {
            TermPos::Original(span) => (span.src_id, span.start),
            TermPos::Inherited(span) => (span.src_id, span.start),
            TermPos::None => {
                eprintln!("{:?}", item);

                unreachable!()
            }
        });

        let mut id_mapping = HashMap::new();
        lin_.iter()
            .enumerate()
            .for_each(|(index, LinearizationItem { id, .. })| {
                id_mapping.insert(*id, index);
            });

        let lin_ = lin_
            .into_iter()
            .map(
                |LinearizationItem { id, pos, ty, kind }| LinearizationItem {
                    ty: to_type(extra, ty),
                    id,
                    pos,
                    kind,
                },
            )
            .collect();

        Linearization::completed(Completed {
            lin: lin_,
            id_mapping,
        })
    }

    fn scope(&self) -> Self {
        AnalysisHost {
            env: self.env.clone(),
        }
    }
}

impl Linearization<Building<Vec<LinearizationItem<Unresolved>>>> {
    fn push(&mut self, item: LinearizationItem<Unresolved>) {
        self.state.resource.push(item);
    }

    fn add_usage(&mut self, decl: usize, usage: usize) {
        match self
            .state
            .resource
            .get_mut(decl)
            .expect("Coundt find parent")
            .kind
        {
            TermKind::Structure => unreachable!(),
            TermKind::Usage(_) => unreachable!(),
            TermKind::Declaration(ref mut usages) => usages.push(usage),
        };
    }
}
