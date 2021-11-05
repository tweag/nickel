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
//! - [LinearizationItem]: Abstract information for each term.

use std::{collections::HashMap, marker::PhantomData};

use super::TypeWrapper;
use crate::environment::Environment as GenericEnvironment;
use crate::term::{MetaValue, RecordAttrs};
use crate::types::Types;
use crate::{identifier::Ident, position::TermPos, term::Term};

/// Holds the state of a linearization, either in progress or finalized
/// Restricts the possible states of a linearization to entities marked
/// as [LinearizationState]
pub struct Linearization<S: LinearizationState> {
    pub state: S,
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
    pub resource: T,
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
pub type Unresolved = TypeWrapper;
impl ResolutionState for Unresolved {}

/// When resolved a concrete [Types] is known
pub type Resolved = Types;
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
    pub scope: Vec<ScopeId>,
    pub meta: Option<MetaValue>,
}

/// Abstact term kinds.
/// Currently tracks
/// 1. Declarations
/// 2. Usages, with Structure being a
/// 3. Records, listing their fields
/// 4. wildcard for any other kind of term.
/// Can be extended later to represent Contracts, Records, etc.
#[derive(Debug, Clone, PartialEq)]
pub enum TermKind {
    Declaration(Ident, Vec<usize>),
    Usage(Option<usize>),
    Record(HashMap<Ident, usize>),
    RecordField {
        ident: Ident,
        body_pos: TermPos,
        record: usize,
        usages: Vec<usize>,
    },
    Structure,
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
        _lin: &mut Linearization<Building<L>>,
        _term: &Term,
        _pos: TermPos,
        _ty: TypeWrapper,
    ) {
    }
    /// Defines how to turn a [Building] Linearization of the tracked type into
    /// a [Completed] linearization.
    /// By default creates an entirely empty [Completed] object
    fn linearize(self, _lin: Linearization<Building<L>>, _extra: S) -> Linearization<Completed>
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
    fn scope(&mut self, branch: ScopeId) -> Self;
}

/// [Linearizer] that deliberately does not maintain any state or act
/// in any way.
/// Ideally the compiler would eliminate code using this [Linearizer]
pub struct StubHost<L, S>(PhantomData<L>, PhantomData<S>);
impl<L, S> Linearizer<L, S> for StubHost<L, S> {
    fn scope(&mut self, _: ScopeId) -> Self {
        StubHost::new()
    }
}

impl<L, S> StubHost<L, S> {
    pub fn new() -> StubHost<L, S> {
        StubHost(PhantomData, PhantomData)
    }
}

pub type Environment = GenericEnvironment<Ident, usize>;

trait ScopeIdElem: Clone + Eq {}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum ScopeId {
    Left,
    Right,
    Choice(usize),
}

impl ScopeIdElem for ScopeId {}

impl Into<Completed> for Linearization<Completed> {
    fn into(self) -> Completed {
        self.state
    }
}

impl Completed {
    pub fn get_item(&self, id: usize) -> Option<&LinearizationItem<Resolved>> {
        self.id_mapping
            .get(&id)
            .and_then(|index| self.lin.get(*index))
    }

    pub fn get_in_scope(
        &self,
        LinearizationItem { scope, .. }: &LinearizationItem<Resolved>,
    ) -> Vec<&LinearizationItem<Resolved>> {
        (0..scope.len())
            .into_iter()
            .map(|end| &scope[..=end])
            .flat_map(|scope| {
                eprintln!("in scope {:?}: {:?}", scope, self.scope_mapping.get(scope));

                self.scope_mapping
                    .get(scope)
                    .map_or_else(|| Vec::new(), Clone::clone)
            })
            .map(|id| self.get_item(id))
            .filter(Option::is_some)
            .map(Option::unwrap)
            .collect()
    }
}
