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
    pub scope_mapping: HashMap<Vec<ScopeId>, Vec<usize>>,
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
    scope: Vec<ScopeId>,
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
                scope_mapping: HashMap::new(),
            },
        }
    }
    fn scope(&self, branch: ScopeId) -> Self;
}

/// [Linearizer] that deliberately does not maintain any state or act
/// in any way.
/// Ideally the compiler would eliminate code using this [Linearizer]
pub struct StubHost<L>(PhantomData<L>);
impl<L, S> Linearizer<L, S> for StubHost<L> {
    fn scope(&self, _: ScopeId) -> Self {
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
    scope: Vec<ScopeId>,
}

impl AnalysisHost {
    pub fn new() -> Self {
        AnalysisHost {
            env: Environment::new(),
            scope: Vec::new(),
        }
    }
}

trait ScopeIdElem: Clone + Eq {}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum ScopeId {
    Left,
    Right,
    Choice(usize),
}

impl ScopeIdElem for ScopeId {}

#[derive(Default)]
pub struct BuildingResource {
    linearization: Vec<LinearizationItem<Unresolved>>,
    scope: HashMap<Vec<ScopeId>, Vec<usize>>,
}

impl Linearizer<BuildingResource, UnifTable> for AnalysisHost {
    fn add_term(
        &mut self,
        lin: &mut Linearization<Building<BuildingResource>>,
        term: &Term,
        pos: TermPos,
        ty: TypeWrapper,
    ) {
        if pos == TermPos::None {
            eprintln!("{:?}", term);
            return;
        }
        let id = lin.state.resource.linearization.len();
        match term {
            Term::Let(ident, _, _) => {
                self.env
                    .insert(ident.to_owned(), lin.state.resource.linearization.len());
                lin.push(LinearizationItem {
                    id,
                    ty,
                    pos,
                    scope: self.scope.clone(),
                    kind: TermKind::Declaration(ident.to_string(), Vec::new()),
                });
            }
            Term::Var(ident) => {
                let parent = self.env.get(ident);
                lin.push(LinearizationItem {
                    id,
                    pos,
                    ty,
                    scope: self.scope.clone(),
                    // id = parent: full let binding including the body
                    // id = parent + 1: actual delcaration scope, i.e. _ = < definition >
                    kind: TermKind::Usage(parent.map(|id| id + 1)),
                });
                if let Some(parent) = parent {
                    lin.add_usage(parent, id);
                }
            }
            _ => lin.push(LinearizationItem {
                id,
                pos,
                ty,
                scope: self.scope.clone(),
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
        lin: Linearization<Building<BuildingResource>>,
        extra: &UnifTable,
    ) -> Linearization<Completed> {
        let mut lin_ = lin.state.resource.linearization;
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
                |LinearizationItem {
                     id,
                     pos,
                     ty,
                     kind,
                     scope,
                 }| LinearizationItem {
                    ty: to_type(extra, ty),
                    id,
                    pos,
                    kind,
                    scope,
                },
            )
            .collect();

        Linearization::completed(Completed {
            lin: lin_,
            id_mapping,
            scope_mapping: lin.state.resource.scope,
        })
    }

    fn scope(&self, scope_id: ScopeId) -> Self {
        let mut scope = self.scope.clone();
        scope.push(scope_id);

        AnalysisHost {
            scope,
            env: self.env.clone(),
        }
    }
}

impl Into<Completed> for Linearization<Completed> {
    fn into(self) -> Completed {
        self.state
    }
}

impl Linearization<Building<BuildingResource>> {
    fn push(&mut self, item: LinearizationItem<Unresolved>) {
        self.state
            .resource
            .scope
            .remove(&item.scope)
            .map(|mut s| {
                s.push(item.id);
                s
            })
            .or_else(|| Some(vec![item.id]))
            .into_iter()
            .for_each(|l| {
                self.state.resource.scope.insert(item.scope.clone(), l);
            });
        self.state.resource.linearization.push(item);
    }

    fn add_usage(&mut self, decl: usize, usage: usize) {
        match self
            .state
            .resource
            .linearization
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

impl Linearization<Completed> {
    pub fn get_item(&self, id: usize) -> Option<&LinearizationItem<Resolved>> {
        self.state
            .id_mapping
            .get(&id)
            .and_then(|index| self.state.lin.get(*index))
    }

    pub fn get_in_scope(
        &self,
        LinearizationItem { scope, .. }: &LinearizationItem<Resolved>,
    ) -> Vec<&LinearizationItem<Resolved>> {
        (0..scope.len())
            .into_iter()
            .map(|end| &scope[..end])
            .flat_map(|scope| {
                self.state
                    .scope_mapping
                    .get(scope)
                    .map_or_else(|| Vec::new(), Clone::clone)
            })
            .map(|id| self.get_item(id))
            .filter(Option::is_some)
            .map(Option::unwrap)
            .collect()
    }
}
