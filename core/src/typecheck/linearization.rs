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
//!                 into a temporary [Linearizer::Building] structure and linearize them into a
//!                 [Linearizer::Completed] Linearization.
//!                 Additionally handles registration in different scopes.
//! - [StubHost]: The purpose of this is to do nothing. It serves as an implementation used
//!               outside the LSP context meaning to cause as little runtime impact as possible.

use std::marker::PhantomData;

use super::UnifType;
use crate::term::RichTerm;
use crate::{identifier::LocIdent, term::record::Field};

pub struct Uninit;

/// The linearizer trait is what is referred to during typechecking.
/// It is the interface to recording terms (while tracking their scope)
/// and finalizing a linearization using generically defined external information
///
/// **Generic Terms**
/// `L`: The data type available during build
/// `S`: Type of external state passed into the linearization
pub trait Linearizer {
    type Building;
    type Completed: Default;
    type ItemId: Copy;
    type CompletionExtra;

    /// Record a new term.
    ///
    /// `self` is assumed to be _scope stable_ meaning, it can hold information valid in the
    /// current scope.
    ///
    /// In practice this mainly includes environment information. Providing this
    /// state is the responsibility of [Linearizer::scope]
    ///
    /// Returns an item id that can then be used to complete e.g. type information. Some terms
    /// might give rise to several items in the linearization, but usually, there's a main one that
    /// is relevant for amending the type. The result can be `None` if the term doesn't have a
    /// location. In this case, the LSP currently ignores it.
    fn add_term(
        &mut self,
        _lin: &mut Self::Building,
        _term: &RichTerm,
        _ty: UnifType,
    ) -> Option<Self::ItemId>;

    /// Record the metadata of a record field. The record field is guaranteed to be linearized
    /// next, if the it has a value.
    ///
    /// `self` is assumed to be _scope stable_ meaning, it can hold information
    /// valid in the current scope.
    ///
    /// In practice this mainly includes environment information. Providing this state is the
    /// responsibility of [Linearizer::scope]
    fn add_field_metadata(&mut self, _lin: &mut Self::Building, _field: &Field) {}

    /// Allows to amend the type of an ident in scope
    fn retype_ident(&mut self, _lin: &mut Self::Building, _ident: &LocIdent, _new_type: UnifType) {}

    /// Allows to amend the type of an item.
    ///
    /// Because [Self::add_term] returns an optional, it's more ergonomic to have `retype` accept an
    /// optional as well. If the item id is `None`, `retype` simply does nothing.
    fn retype(
        &mut self,
        _lin: &mut Linearization<Self::Building>,
        _item_id: Option<Self::ItemId>,
        _new_type: UnifType,
    ) {
    }

    /// Defines how to turn a [Self::Building] Linearization of the tracked type into
    /// a [Self::Completed] linearization.
    /// By default creates an entirely empty [Self::Completed] object
    fn complete(self, _lin: Self::Building, _extra: &Self::CompletionExtra) -> Self::Completed
    where
        Self: Sized,
    {
        Self::Completed::default()
    }

    /// Ensures the scope structure of the source can be represented in the
    /// linearization.
    /// The specific implementations need to take care of how to represent
    /// descending into a lower scope.
    /// Notice, the resulting instance is a fresh value, any resource that is
    /// required or produced in parallel instances should therefore be put
    /// into the Building State `L` which is passed
    fn scope(&mut self) -> Self;

    /// Create a scope for the processing of terms potentially contained in metadata (contracts
    /// expressions in the annotations). When walking such terms, scoping behaves differently with
    /// respect to the previous state (pending record fields, let binding, field metadata, etc.).
    /// Mostly, the `self` linearizer keeps the state while the returned one has a clean state.
    fn scope_meta(&mut self) -> Self;
}

/// [Linearizer] that deliberately does not maintain any state or act
/// in any way.
/// Ideally the compiler would eliminate code using this [Linearizer]
pub struct StubHost<B = (), C = (), E = ()>(PhantomData<B>, PhantomData<C>, PhantomData<E>);
impl<B, C, E> Linearizer for StubHost<B, C, E>
where
    B: Default,
    C: Default,
{
    type Building = B;
    type Completed = C;
    type CompletionExtra = E;
    type ItemId = ();

    fn add_term(
        &mut self,
        _lin: &mut Linearization<Self::Building>,
        _term: &RichTerm,
        _ty: UnifType,
    ) -> Option<()> {
        None
    }

    fn scope(&mut self) -> Self {
        StubHost::new()
    }

    fn scope_meta(&mut self) -> Self {
        self.scope()
    }
}

impl<B, C, E> StubHost<B, C, E> {
    pub fn new() -> StubHost<B, C, E> {
        StubHost(PhantomData, PhantomData, PhantomData)
    }
}

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq, Hash)]
pub struct ScopeId(usize);

impl ScopeId {
    pub fn next(self) -> (Self, Self) {
        let next = ScopeId(self.0 + 1);
        (self, next)
    }
}

pub type Scope = Vec<ScopeId>;
