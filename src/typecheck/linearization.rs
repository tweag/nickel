//! Linearization interface and Empty implementation
//!
//! The linearization module mainly serves to assist the LSP.
//!
//! **Like the LSP crate, this part of nickel is currently **under heavy development**
//! and may change unadvertedly.**
//!
//! At this point it is integrated into the typechecking allowing to trace an AST
//! into a linear representation. This representation, being separate from the
//! AST can include additional meta information about types, references, and
//! possibly more in the future.
//!
//! # Components
//!
//! - [Linearizer]: Implements functionality to record terms (integrated into typechecking)
//!                 into a temporary [Building] structure and linearize them into a
//!                 [Completed] Linearization.
//!                 Additionally handles registration in different scopes.
//! - [Linearization]: Linearization in a given state.
//!                    The state holds context while building or the finalized linearization
//! - [StubHost]: The purpose of this is to do nothing. It serves as an implementation used
//!               outside the LSP context meaning to cause as little runtime impact as possible.

use std::{collections::HashMap, marker::PhantomData, str::EncodeUtf16};

use super::{State, TypeWrapper, UnifTable};
use crate::environment::Environment as GenericEnvironment;
use crate::typecheck::to_type;
use crate::types::{AbsType, Types};
use crate::{identifier::Ident, position::TermPos, term::Term};

/// Holds the state of a linearization, either in progress or finalized
/// Restricts the possible states of a linearization to entities marked
/// as [LinearizationState]
pub struct Linearization<S: LinearizationState> {
    state: S,
}

/// Constructors for different phases
impl Linearization<Uninit> {
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

/// A concrete [LinearizationState]
/// Holds any inner datatype that can be used as stable resource
/// while recording terms.
pub struct Building<T> {
    resource: T,
}

/// Finalized linearization
#[derive(Debug)]
pub struct Completed {
    pub lin: Vec<LinearizationItem<Resolved>>,
    pub id_mapping: HashMap<usize, usize>,
}

pub struct Uninit;

/// Marker trait for possible states of the linearization
pub trait LinearizationState {}
impl<T> LinearizationState for Building<T> {}
impl LinearizationState for Completed {}
impl LinearizationState for Uninit {}

/// Possible resolution states of a LinearizationItem.
///
/// [LinearizationItem]s are initialized as [Unresolved] and are
/// being resolved when completing the linearization
///
/// As implementors are used as typestate items of the [LinearizationItem]
/// to determine the type of the Item.
pub trait ResolutionState {}
/// Types are available as [TypeWrapper] only during recording
/// They are resolved after typechecking has collected all terms into concrete
/// [Types]
type Unresolved = TypeWrapper;
impl ResolutionState for Unresolved {}

/// When resolved a concrete [Types] is known
type Resolved = Types;
impl ResolutionState for Resolved {}

/// A recorded item of a given state of resolution state
/// Tracks a unique id used to build a reference table after finalizing
/// the linearization using the LSP [AnalysisHost]
#[derive(Debug, Clone, PartialEq)]
pub struct LinearizationItem<S: ResolutionState> {
    //term_: Box<Term>,
    pub id: usize,
    pub pos: TermPos,
    pub ty: S,
    pub kind: TermKind,
}

/// Abstact term kinds.
/// Currently only tracks Declaration and Usages, with Structure being a
/// wildcard for any other kind of term.
/// Can be extended later to represent Contracts, Records, etc.
#[derive(Debug, Clone, PartialEq)]
pub enum TermKind {
    Structure,
    Declaration(String, Vec<usize>),
    Usage(Option<usize>),
}

/// The linearizer trait is what is refered to during typechecking.
/// It is the interface to recording terms (while tracking their scope)
/// and finalizing a linearization using generically defined external information
///
/// **Generic Terms**
/// `L`: The data type available during build
/// `S`: Type of external state passed into the linearization
pub trait Linearizer<L, S> {
    /// Record a new type
    ///
    /// `self` is assumed to be _scope stable_ meaning, it can hold information
    /// valid in the current scope.
    ///
    /// In practice this mainly includes environment information. Providing this
    /// state is the responsibility of [Linearizer::scope]
    fn add_term(
        &mut self,
        lin: &mut Linearization<Building<L>>,
        term: &Term,
        pos: TermPos,
        ty: TypeWrapper,
    ) {
    }

    /// Defines how to turn a [Building] Linearization of the tracked type into
    /// a [Completed] linearization.
    /// By default creates an entirely empty [Completed] object
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

/// [Linearizer] that deliberately does not maintain any state or act
/// in any way.
/// Ideally the compiler would eliminate code using this [Linearizer]
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

/// [Linearizer] used by the LSP
///
/// Tracks a _scope stable_ environment managing variable ident
/// resolution
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
            Term::Let(ident, definition, _) => {
                self.env.insert(ident.to_owned(), lin.state.resource.len());
                lin.push(LinearizationItem {
                    id,
                    ty,
                    pos: definition.pos,
                    kind: TermKind::Declaration(ident.to_string(), Vec::new()),
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

    /// [Self::add_term] produces a depth first representation or the
    /// traversed AST. This function indexes items by _source position_.
    /// Elements are reorderd to allow efficient lookup of elemts by
    /// their location in the source.
    ///
    /// Additionally, resolves concrete types for all items.
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

impl Into<Completed> for Linearization<Completed> {
    fn into(self) -> Completed {
        self.state
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
            TermKind::Declaration(_, ref mut usages) => usages.push(usage),
        };
    }
}
