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

use std::marker::PhantomData;

use super::TypeWrapper;
use crate::environment::Environment as GenericEnvironment;
use crate::{identifier::Ident, position::TermPos, term::Term};

/// Holds the state of a linearization, either in progress or finalized
/// Restricts the possible states of a linearization to entities marked
/// as [LinearizationState]
pub struct Linearization<S: LinearizationState> {
    pub state: S,
}

impl<S: LinearizationState> Linearization<S> {
    pub fn into_inner(self) -> S {
        self.state
    }
}

/// Constructors for different phases
impl Linearization<Uninit> {
    pub fn new<S: LinearizationState>(state: S) -> Linearization<S> {
        Linearization { state }
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

pub struct Uninit;

/// Marker trait for possible states of the linearization
pub trait LinearizationState {}
impl<T> LinearizationState for Building<T> {}
impl LinearizationState for () {}
impl LinearizationState for Uninit {}

/// Possible resolution states of a LinearizationItem.
///
/// [LinearizationItem]s are initialized as [Unresolved] and are
/// being resolved when completing the linearization
///
/// As implementors are used as typestate items of the [LinearizationItem]
/// to determine the type of the Item.

/// The linearizer trait is what is refered to during typechecking.
/// It is the interface to recording terms (while tracking their scope)
/// and finalizing a linearization using generically defined external information
///
/// **Generic Terms**
/// `L`: The data type available during build
/// `S`: Type of external state passed into the linearization
pub trait Linearizer<L, S> {
    type Completed: LinearizationState + Default;

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

    /// Allows to amend the type of an ident in scope
    fn retype_ident(
        &mut self,
        _lin: &mut Linearization<Building<L>>,
        _ident: &Ident,
        _new_type: TypeWrapper,
    ) {
    }

    /// Defines how to turn a [Building] Linearization of the tracked type into
    /// a [Self::Completed] linearization.
    /// By default creates an entirely empty [Self::Completed] object
    fn linearize(
        self,
        _lin: Linearization<Building<L>>,
        _extra: S,
    ) -> Linearization<Self::Completed>
    where
        Self: Sized,
    {
        Linearization {
            state: Self::Completed::default(),
        }
    }

    /// Ensures the scope structure of the source can be represented in the
    /// linearization.
    /// The specific implementations need to take care of how to represent
    /// decending into a lower scope.
    /// Notice, the resulting instance is a fresh value, any resource that is
    /// required or produced in parallel instances should therefore be put
    /// into the Building State `L` which is passed
    fn scope(&mut self, branch: ScopeId) -> Self;
}

/// [Linearizer] that deliberately does not maintain any state or act
/// in any way.
/// Ideally the compiler would eliminate code using this [Linearizer]
pub struct StubHost<L, S>(PhantomData<L>, PhantomData<S>);
impl<L, S> Linearizer<L, S> for StubHost<L, S> {
    type Completed = ();
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
