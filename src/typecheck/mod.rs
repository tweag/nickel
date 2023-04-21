//! Typechecking and type inference.
//!
//! Nickel uses a mix of a bidirectional typechecking algorithm, together with standard
//! unification-based type inference. Nickel is gradually typed, and dynamic typing is the default.
//! Static typechecking is triggered by a type annotation.
//!
//! # Modes
//!
//! The typechecking algorithm runs in two separate modes, corresponding to static and dynamic
//! typing:
//!
//! - **enforce** corresponds to traditional typechecking in a statically typed language. This
//!   happens inside a statically typed block. Such blocks are introduced by the type ascription
//!   operator `:`, as in `1 + 1 : Number` or `let f : Number -> Number = fun x => x + 1 in ..`. Enforce
//!   mode is implemented by [`type_check`] and variants.
//! - **walk** doesn't enforce any typing but traverses the AST looking for typed blocks to
//!   typecheck. Walk mode also stores the annotations of bound identifiers in the environment. This
//!   is implemented by the [`walk`] function.
//!
//! The algorithm starts in walk mode. A typed block (an expression annotated with a type) switches
//! to enforce mode, and is switched back to walk mode when entering an expression annotated with a
//! contract. Type and contract annotations thus serve as a switch for the typechecking mode.
//!
//! Note that the static typing part is based on the bidirectional typing framework, which defines
//! two different modes. Thus, the enforce mode is itself divided again into **checking** mode and
//! **inference** mode.
//!
//! # Type inference
//!
//! Type inference is done via a form of bidirectional typechecking coupled with unification, in
//! the same spirit as GHC (Haskell), although the type system of Nickel is simpler. The type of
//! un-annotated let-bound expressions (the type of `bound_exp` in `let x = bound_exp in body`) is
//! inferred in enforce mode, but it is never implicitly generalized. For example, the following
//! program is rejected:
//!
//! ```nickel
//! # Rejected
//! (let id = fun x => x in std.seq (id "a") (id 5)) : Number
//! ```
//!
//! Indeed, `id` is given the type `_a -> _a`, where `_a` is a unification variable, but is not
//! generalized to `forall a. a -> a`. At the first call site, `_a` is unified with `String`, and at the second
//! call site the typechecker complains that `5` is not of type `String`.
//!
//! This restriction is on purpose, as generalization is not trivial to implement efficiently and
//! more importantly can interact with other components of the type system and type inference. If
//! polymorphism is required, the user can simply add annotation:
//!
//! ```nickel
//! # Accepted
//! (let id : forall a. a -> a = fun x => x in std.seq (id "a") (id 5)) : Num
//! ```
//!
//! In walk mode, the type of let-bound expressions is inferred in a shallow way (see
//! [`apparent_type`]).
use crate::{
    cache::ImportResolver,
    environment::Environment as GenericEnvironment,
    error::TypecheckError,
    identifier::Ident,
    position::TermPos,
    stdlib as nickel_stdlib,
    term::{
        record::Field, LabeledType, RichTerm, StrChunk, Term, Traverse, TraverseOrder,
        TypeAnnotation,
    },
    types::{
        EnumRow, EnumRows, EnumRowsF, EnumRowsIterator, RecordRowF, RecordRows, RecordRowsF,
        RecordRowsIterator, TypeF, Types, VarKind,
    },
    {mk_uty_arrow, mk_uty_enum, mk_uty_enum_row, mk_uty_record, mk_uty_row},
};

use std::{
    collections::{HashMap, HashSet},
    convert::TryInto,
};

use self::linearization::{Linearization, Linearizer, StubHost};

mod destructuring;
pub mod error;
pub mod linearization;
pub mod operation;
pub mod reporting;
#[macro_use]
pub mod mk_uniftype;
pub mod eq;

use eq::{SimpleTermEnvironment, TermEnvironment};
use error::*;
use operation::{get_bop_type, get_nop_type, get_uop_type};

/// The max depth parameter used to limit the work performed when inferring the type of the stdlib.
const INFER_RECORD_MAX_DEPTH: u8 = 4;

/// The typing environment.
pub type Environment = GenericEnvironment<Ident, UnifType>;

/// Mapping from wildcard ID to inferred type
pub type Wildcards = Vec<Types>;
/// Unification variable or type constants unique identifier.
pub type VarId = usize;

/// A unifiable record row.
pub type GenericUnifRecordRow<E> = RecordRowF<Box<GenericUnifType<E>>>;

/// Unifiable record rows. Same shape as [`crate::types::RecordRows`], but where each type is
/// unifiable, and each tail may be a unification variable (or a constant).
#[derive(Clone, PartialEq, Debug)]
pub enum GenericUnifRecordRows<E: TermEnvironment + Clone> {
    Concrete(RecordRowsF<Box<GenericUnifType<E>>, Box<GenericUnifRecordRows<E>>>),
    Constant(VarId),
    UnifVar(VarId),
}

/// Unifiable enum rows. Same shape as [`crate::types::EnumRows`] but where each tail may be a
/// unification variable (or a constant).
///
/// Enum rows don't store any type (they are just a sequence of identifiers), so there is no
/// `GenericUnifEnumRows` taking an additional `E` parameter.
#[derive(Clone, PartialEq, Debug)]
pub enum UnifEnumRows {
    Concrete(EnumRowsF<Box<UnifEnumRows>>),
    Constant(VarId),
    UnifVar(VarId),
}

/// The types on which the unification algorithm operates, which may be either a concrete type, a
/// type constant or a unification variable.
///
/// Contracts store an additional term environment for contract equality checking, which is
/// represented by `E`. The typechecker always uses the same type for `E`. However, the evaluation
/// phase may also resort to checking contract equality, using a different environment
/// representation, hence the parametrization.
#[derive(Clone, PartialEq, Debug)]
pub enum GenericUnifType<E: TermEnvironment> {
    /// A concrete type (like `Number` or `String -> String`).
    Concrete(TypeF<Box<GenericUnifType<E>>, GenericUnifRecordRows<E>, UnifEnumRows>),
    /// A contract, seen as an opaque type. In order to compute type equality between contracts or
    /// between a contract and a type, we need to carry an additional environment. This is why we
    /// don't reuse the variant from [`crate::types::TypeF`].
    Contract(RichTerm, E),
    /// A rigid type constant which cannot be unified with anything but itself.
    Constant(VarId),
    /// A unification variable.
    UnifVar(VarId),
}

impl<E: TermEnvironment + Clone> std::convert::TryInto<RecordRows> for GenericUnifRecordRows<E> {
    type Error = ();

    fn try_into(self) -> Result<RecordRows, ()> {
        match self {
            GenericUnifRecordRows::Concrete(rrows) => {
                let converted: RecordRowsF<Box<Types>, Box<RecordRows>> = rrows.try_map(
                    |uty| Ok(Box::new(GenericUnifType::try_into(*uty)?)),
                    |urrows| {
                        let rrows: RecordRows = (*urrows).try_into()?;
                        Ok(Box::new(rrows))
                    },
                )?;
                Ok(RecordRows(converted))
            }
            _ => Err(()),
        }
    }
}

impl std::convert::TryInto<EnumRows> for UnifEnumRows {
    type Error = ();

    fn try_into(self) -> Result<EnumRows, ()> {
        match self {
            UnifEnumRows::Concrete(erows) => {
                let converted: EnumRowsF<Box<EnumRows>> =
                    erows.try_map(|erows| Ok(Box::new(UnifEnumRows::try_into(*erows)?)))?;
                Ok(EnumRows(converted))
            }
            _ => Err(()),
        }
    }
}

impl<E: TermEnvironment + Clone> std::convert::TryInto<Types> for GenericUnifType<E> {
    type Error = ();

    fn try_into(self) -> Result<Types, ()> {
        match self {
            GenericUnifType::Concrete(ty) => {
                let converted: TypeF<Box<Types>, RecordRows, EnumRows> = ty.try_map(
                    |uty_boxed| {
                        let ty: Types = (*uty_boxed).try_into()?;
                        Ok(Box::new(ty))
                    },
                    GenericUnifRecordRows::try_into,
                    UnifEnumRows::try_into,
                )?;
                Ok(Types::from(converted))
            }
            GenericUnifType::Contract(t, _) => {
                let pos = t.pos;
                Ok(Types {
                    types: TypeF::Flat(t),
                    pos,
                })
            }
            _ => Err(()),
        }
    }
}

// As opposed to `UnifType` and `UnifRecordRows` which can contain types and thus contracts, with
// all the subtleties associated with contract equality checking (see `typecheck::eq` module), we can
// convert enum rows directly to unifiable enum rows without additional data: instead of
// implementing a function `from_enum_rows`, we rather implement the more natural trait
// `From<EnumRows>`.
impl From<EnumRows> for UnifEnumRows {
    fn from(erows: EnumRows) -> Self {
        UnifEnumRows::Concrete(erows.0.map(|erows| Box::new(UnifEnumRows::from(*erows))))
    }
}

impl UnifEnumRows {
    /// Return an iterator producing immutable references to individual rows.
    pub fn iter(&self) -> EnumRowsIterator<UnifEnumRows> {
        EnumRowsIterator { erows: Some(self) }
    }
}

impl<E: TermEnvironment + Clone> GenericUnifRecordRows<E> {
    /// Create `GenericUnifRecordRows` from `RecordRows`. Contracts are represented as the separate
    /// variant [`GenericUnifType::Contract`] which also stores a term environment, required for
    /// checking type equality involving contracts.
    pub fn from_record_rows(rrows: RecordRows, env: &E) -> Self {
        let f_rrow = |ty: Box<Types>| Box::new(GenericUnifType::from_type(*ty, env));
        let f_rrows =
            |rrows: Box<RecordRows>| Box::new(GenericUnifRecordRows::from_record_rows(*rrows, env));

        GenericUnifRecordRows::Concrete(rrows.0.map(f_rrow, f_rrows))
    }
}

impl<E: TermEnvironment> GenericUnifRecordRows<E> {
    pub(super) fn iter(&self) -> RecordRowsIterator<GenericUnifType<E>, GenericUnifRecordRows<E>> {
        RecordRowsIterator {
            rrows: Some(self),
            ty: std::marker::PhantomData,
        }
    }
}

trait SubstType<E: TermEnvironment> {
    fn subst_type(self, id: &Ident, to: &GenericUnifType<E>) -> Self;
}

trait SubstRRows<E: TermEnvironment> {
    fn subst_rrows(self, id: &Ident, to: &GenericUnifRecordRows<E>) -> Self;
}

trait SubstERows {
    fn subst_erows(self, id: &Ident, to: &UnifEnumRows) -> Self;
}

impl<E: TermEnvironment> SubstType<E> for GenericUnifType<E> {
    fn subst_type(self, id: &Ident, to: &GenericUnifType<E>) -> Self {
        match self {
            GenericUnifType::Concrete(TypeF::Var(var_id)) if var_id == *id => to.clone(),
            GenericUnifType::Concrete(t) => GenericUnifType::Concrete(t.map(
                |ty| Box::new(ty.subst_type(id, to)),
                |rrows| rrows.subst_type(id, to),
                |erows| erows,
            )),
            _ => self,
        }
    }
}

impl<E: TermEnvironment> SubstType<E> for GenericUnifRecordRows<E> {
    fn subst_type(self, id: &Ident, to: &GenericUnifType<E>) -> Self {
        match self {
            GenericUnifRecordRows::Concrete(rrows) => GenericUnifRecordRows::Concrete(rrows.map(
                |ty| Box::new(ty.subst_type(id, to)),
                |rrows| Box::new(rrows.subst_type(id, to)),
            )),
            _ => self,
        }
    }
}

impl<E: TermEnvironment> SubstRRows<E> for GenericUnifType<E> {
    fn subst_rrows(self, id: &Ident, to: &GenericUnifRecordRows<E>) -> Self {
        match self {
            GenericUnifType::Concrete(t) => GenericUnifType::Concrete(t.map(
                |ty| Box::new(ty.subst_rrows(id, to)),
                |rrows| rrows.subst_rrows(id, to),
                |erows| erows,
            )),
            _ => self,
        }
    }
}

impl<E: TermEnvironment> SubstRRows<E> for GenericUnifRecordRows<E> {
    fn subst_rrows(self, id: &Ident, to: &GenericUnifRecordRows<E>) -> Self {
        match self {
            GenericUnifRecordRows::Concrete(RecordRowsF::TailVar(var_id)) if var_id == *id => {
                to.clone()
            }
            GenericUnifRecordRows::Concrete(rrows) => GenericUnifRecordRows::Concrete(rrows.map(
                |ty| Box::new(ty.subst_rrows(id, to)),
                |rrows| Box::new(rrows.subst_rrows(id, to)),
            )),
            _ => self,
        }
    }
}

impl<E: TermEnvironment> SubstERows for GenericUnifType<E> {
    fn subst_erows(self, id: &Ident, to: &UnifEnumRows) -> Self {
        match self {
            GenericUnifType::Concrete(t) => GenericUnifType::Concrete(t.map(
                |ty| Box::new(ty.subst_erows(id, to)),
                |rrows| rrows.subst_erows(id, to),
                |erows| erows.subst_erows(id, to),
            )),
            _ => self,
        }
    }
}

impl<E: TermEnvironment> SubstERows for GenericUnifRecordRows<E> {
    fn subst_erows(self, id: &Ident, to: &UnifEnumRows) -> Self {
        match self {
            GenericUnifRecordRows::Concrete(rrows) => GenericUnifRecordRows::Concrete(rrows.map(
                |ty| Box::new(ty.subst_erows(id, to)),
                |rrows| Box::new(rrows.subst_erows(id, to)),
            )),
            _ => self,
        }
    }
}

impl SubstERows for UnifEnumRows {
    fn subst_erows(self, id: &Ident, to: &UnifEnumRows) -> Self {
        match self {
            UnifEnumRows::Concrete(EnumRowsF::TailVar(var_id)) if var_id == *id => to.clone(),
            UnifEnumRows::Concrete(rrows) => {
                UnifEnumRows::Concrete(rrows.map(|erows| Box::new(erows.subst_erows(id, to))))
            }
            _ => self,
        }
    }
}

impl<E: TermEnvironment + Clone> GenericUnifType<E> {
    /// Create a [`GenericUnifType`] from a [`Types`]. Contracts are represented as the separate variant
    /// [`GenericUnifType::Contract`] which also stores a term environment, required for checking type
    /// equality involving contracts.
    pub fn from_type(ty: Types, env: &E) -> Self {
        match ty.types {
            TypeF::Flat(t) => GenericUnifType::Contract(t, env.clone()),
            ty => GenericUnifType::Concrete(ty.map(
                |ty_| Box::new(GenericUnifType::from_type(*ty_, env)),
                |rrows| GenericUnifRecordRows::from_record_rows(rrows, env),
                UnifEnumRows::from,
            )),
        }
    }
}

pub type UnifRecordRow = GenericUnifRecordRow<SimpleTermEnvironment>;
pub type UnifRecordRows = GenericUnifRecordRows<SimpleTermEnvironment>;
pub type UnifType = GenericUnifType<SimpleTermEnvironment>;

impl UnifRecordRows {
    /// Extract the concrete [`RecordRows`] corresponding to a [`UnifRecordRows`]. Free unification variables as well
    /// as type constants are replaced with the empty row.
    fn into_rrows(self, table: &UnifTable) -> RecordRows {
        match self {
            UnifRecordRows::UnifVar(p) => match table.root_rrows(p) {
                t @ UnifRecordRows::Concrete(_) => t.into_rrows(table),
                _ => RecordRows(RecordRowsF::Empty),
            },
            UnifRecordRows::Constant(_) => RecordRows(RecordRowsF::Empty),
            UnifRecordRows::Concrete(t) => {
                let mapped = t.map(
                    |ty| Box::new(ty.into_type(table)),
                    |rrows| Box::new(rrows.into_rrows(table)),
                );
                RecordRows(mapped)
            }
        }
    }

    /// Return the unification root associated with these record rows. If the rows are a unification
    /// variable, return the result of `table.root_rrows`. Return `self` otherwise.
    fn into_root(self, table: &UnifTable) -> Self {
        match self {
            UnifRecordRows::UnifVar(var_id) => table.root_rrows(var_id),
            urrows => urrows,
        }
    }
}

impl UnifEnumRows {
    /// Extract the concrete [`EnumRows`] corresponding to a [`UnifEnumRows`]. Free unification variables as well
    /// as type constants are replaced with the empty row.
    fn into_erows(self, table: &UnifTable) -> EnumRows {
        match self {
            UnifEnumRows::UnifVar(p) => match table.root_erows(p) {
                t @ UnifEnumRows::Concrete(_) => t.into_erows(table),
                _ => EnumRows(EnumRowsF::Empty),
            },
            UnifEnumRows::Constant(_) => EnumRows(EnumRowsF::Empty),
            UnifEnumRows::Concrete(t) => {
                let mapped = t.map(|erows| Box::new(erows.into_erows(table)));
                EnumRows(mapped)
            }
        }
    }

    /// Return the unification root associated with these enum rows. If the rows are a unification
    /// variable, return the result of `table.root_erows`. Return `self` otherwise.
    fn into_root(self, table: &UnifTable) -> Self {
        match self {
            UnifEnumRows::UnifVar(var_id) => table.root_erows(var_id),
            uerows => uerows,
        }
    }
}

impl UnifType {
    /// Create a [`UnifType`] from an [`ApparentType`]. As for [`GenericUnifType::from_type`], this function requires
    /// the current term environment.
    pub fn from_apparent_type(at: ApparentType, env: &SimpleTermEnvironment) -> Self {
        match at {
            ApparentType::Annotated(ty) if has_wildcards(&ty) => {
                GenericUnifType::Concrete(TypeF::Dyn)
            }
            ApparentType::Annotated(ty)
            | ApparentType::Inferred(ty)
            | ApparentType::Approximated(ty) => GenericUnifType::from_type(ty, env),
            ApparentType::FromEnv(uty) => uty,
        }
    }

    /// Extract the concrete type corresponding to a unifiable type. Free unification variables as well
    /// as type constants are replaced with the type `Dyn`.
    fn into_type(self, table: &UnifTable) -> Types {
        match self {
            UnifType::UnifVar(p) => match table.root_type(p) {
                t @ UnifType::Concrete(_) => t.into_type(table),
                _ => Types::from(TypeF::Dyn),
            },
            UnifType::Constant(_) => Types::from(TypeF::Dyn),
            UnifType::Concrete(t) => {
                let mapped = t.map(
                    |btyp| Box::new(btyp.into_type(table)),
                    |urrows| urrows.into_rrows(table),
                    |uerows| uerows.into_erows(table),
                );
                Types::from(mapped)
            }
            UnifType::Contract(t, _) => Types::from(TypeF::Flat(t)),
        }
    }

    /// Return the unification root associated with this type. If the type is a unification variable,
    /// return the result of `table.root_type`. Return `self` otherwise.
    fn into_root(self, table: &UnifTable) -> Self {
        match self {
            UnifType::UnifVar(var_id) => table.root_type(var_id),
            uty => uty,
        }
    }
}

// This implementation assumes that `TypeF::Flat` is not possible. If a [`UnifType`] has been
// correctly created from a type using `from_type`, this must be the case.
impl From<TypeF<Box<UnifType>, UnifRecordRows, UnifEnumRows>> for UnifType {
    fn from(ty: TypeF<Box<UnifType>, UnifRecordRows, UnifEnumRows>) -> Self {
        debug_assert!(!matches!(ty, TypeF::Flat(_)));
        UnifType::Concrete(ty)
    }
}

impl From<RecordRowsF<Box<UnifType>, Box<UnifRecordRows>>> for UnifRecordRows {
    fn from(rrows: RecordRowsF<Box<UnifType>, Box<UnifRecordRows>>) -> Self {
        UnifRecordRows::Concrete(rrows)
    }
}

impl From<EnumRowsF<Box<UnifEnumRows>>> for UnifEnumRows {
    fn from(ty: EnumRowsF<Box<UnifEnumRows>>) -> Self {
        UnifEnumRows::Concrete(ty)
    }
}

/// Iterator items produced by [RecordRowsIterator] on [GenericUnifRecordRows].
pub enum GenericUnifRecordRowsIteratorItem<'a, E: TermEnvironment> {
    TailDyn,
    TailVar(&'a Ident),
    TailUnifVar(VarId),
    TailConstant(VarId),
    Row(RecordRowF<&'a GenericUnifType<E>>),
}

pub type UnifRecordRowsIteratorItem<'a> =
    GenericUnifRecordRowsIteratorItem<'a, SimpleTermEnvironment>;

impl<'a, E: TermEnvironment> Iterator
    for RecordRowsIterator<'a, GenericUnifType<E>, GenericUnifRecordRows<E>>
{
    type Item = GenericUnifRecordRowsIteratorItem<'a, E>;

    fn next(&mut self) -> Option<Self::Item> {
        self.rrows.and_then(|next| match next {
            GenericUnifRecordRows::Concrete(rrows) => match rrows {
                RecordRowsF::Empty => {
                    self.rrows = None;
                    None
                }
                RecordRowsF::TailDyn => {
                    self.rrows = None;
                    Some(GenericUnifRecordRowsIteratorItem::TailDyn)
                }
                RecordRowsF::TailVar(id) => {
                    self.rrows = None;
                    Some(GenericUnifRecordRowsIteratorItem::TailVar(id))
                }
                RecordRowsF::Extend { row, tail } => {
                    self.rrows = Some(tail);
                    Some(GenericUnifRecordRowsIteratorItem::Row(RecordRowF {
                        id: row.id,
                        types: row.types.as_ref(),
                    }))
                }
            },
            GenericUnifRecordRows::UnifVar(var_id) => {
                self.rrows = None;
                Some(GenericUnifRecordRowsIteratorItem::TailUnifVar(*var_id))
            }
            GenericUnifRecordRows::Constant(var_id) => {
                self.rrows = None;
                Some(GenericUnifRecordRowsIteratorItem::TailConstant(*var_id))
            }
        })
    }
}

/// Iterator items produced by [`EnumRowsIterator`].
pub enum UnifEnumRowsIteratorItem<'a> {
    TailVar(&'a Ident),
    TailUnifVar(VarId),
    TailConstant(VarId),
    Row(&'a EnumRow),
}

impl<'a> Iterator for EnumRowsIterator<'a, UnifEnumRows> {
    type Item = UnifEnumRowsIteratorItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.erows.and_then(|next| match next {
            UnifEnumRows::Concrete(erows) => match erows {
                EnumRowsF::Empty => {
                    self.erows = None;
                    None
                }
                EnumRowsF::TailVar(id) => {
                    self.erows = None;
                    Some(UnifEnumRowsIteratorItem::TailVar(id))
                }
                EnumRowsF::Extend { row, tail } => {
                    self.erows = Some(tail);
                    Some(UnifEnumRowsIteratorItem::Row(row))
                }
            },
            UnifEnumRows::UnifVar(var_id) => {
                self.erows = None;
                Some(UnifEnumRowsIteratorItem::TailUnifVar(*var_id))
            }
            UnifEnumRows::Constant(var_id) => {
                self.erows = None;
                Some(UnifEnumRowsIteratorItem::TailConstant(*var_id))
            }
        })
    }
}

pub trait ReifyAsUnifType {
    fn unif_type() -> UnifType;
}

/// The typing context is a structure holding the scoped, environment-like data structures required
/// to perform typechecking.
///
/// The typing context currently includes the typing environment, counterpart of the eval
/// environment for typechecking, and the term environment.
#[derive(Debug, PartialEq, Clone)]
pub struct Context {
    pub type_env: Environment,
    pub term_env: SimpleTermEnvironment,
}

impl Context {
    pub fn new() -> Self {
        Context {
            type_env: Environment::new(),
            term_env: SimpleTermEnvironment::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum EnvBuildError {
    NotARecord(RichTerm),
}

/// Populate the initial typing environment from a `Vec` of parsed files.
pub fn mk_initial_ctxt(
    initial_env: &[(nickel_stdlib::StdlibModule, RichTerm)],
) -> Result<Context, EnvBuildError> {
    // Collect the bindings for each module, clone them and flatten the result to a single list.
    let mut bindings = Vec::new();

    for (module, rt) in initial_env {
        match (module, rt.as_ref()) {
            // The internals module is special: it is required to be syntactically a record,
            // and is added directly to the top-level environment.
            (nickel_stdlib::StdlibModule::Internals, Term::RecRecord(record, ..)) => {
                // We reject fields without a value (that would be a stdlib module without defintion)
                bindings.extend(record.fields.iter().map(|(id, field)| {
                    (
                        *id,
                        field
                            .value
                            .as_ref()
                            .unwrap_or_else(|| {
                                panic!("expected stdlib module {id} to have a definition")
                            })
                            .clone(),
                    )
                }));
            }
            (nickel_stdlib::StdlibModule::Internals, _) => {
                return Err(EnvBuildError::NotARecord(rt.clone()));
            }
            // Otherwise, we insert a value in the environment bound to the name of the module
            (module, _) => bindings.push((module.name().into(), rt.clone())),
        }
    }

    let term_env = bindings
        .iter()
        .cloned()
        .map(|(id, rt)| (id, (rt, SimpleTermEnvironment::new())))
        .collect();

    let type_env = bindings
        .into_iter()
        .map(|(id, rt)| {
            (
                id,
                infer_record_type(&rt, &term_env, INFER_RECORD_MAX_DEPTH),
            )
        })
        .collect();

    Ok(Context { type_env, term_env })
}

/// Add the bindings of a record to a typing environment. Ignore fields whose name are defined
/// through interpolation.
//TODO: support the case of a record with a type annotation.
pub fn env_add_term(
    env: &mut Environment,
    rt: &RichTerm,
    term_env: &SimpleTermEnvironment,
    resolver: &dyn ImportResolver,
) -> Result<(), EnvBuildError> {
    let RichTerm { term, pos } = rt;

    match term.as_ref() {
        Term::Record(record) | Term::RecRecord(record, ..) => {
            for (id, field) in &record.fields {
                let uty = UnifType::from_apparent_type(
                    field_apparent_type(field, Some(env), Some(resolver)),
                    term_env,
                );
                env.insert(*id, uty);
            }

            Ok(())
        }
        t => Err(EnvBuildError::NotARecord(RichTerm::new(t.clone(), *pos))),
    }
}

/// Bind one term in a typing environment.
pub fn env_add(
    env: &mut Environment,
    id: Ident,
    rt: &RichTerm,
    term_env: &SimpleTermEnvironment,
    resolver: &dyn ImportResolver,
) {
    env.insert(
        id,
        UnifType::from_apparent_type(
            apparent_type(rt.as_ref(), Some(env), Some(resolver)),
            term_env,
        ),
    );
}

/// The shared state of unification.
pub struct State<'a> {
    /// The import resolver, to retrieve and typecheck imports.
    resolver: &'a dyn ImportResolver,
    /// The unification table.
    table: &'a mut UnifTable,
    /// Row constraints.
    constr: &'a mut RowConstr,
    /// A mapping from unification variable or constants to the name of the corresponding type
    /// variable which introduced it, if any.
    ///
    /// Used for error reporting.
    names: &'a mut HashMap<VarId, Ident>,
    /// A mapping from wildcard ID to unification variable.
    wildcard_vars: &'a mut Vec<UnifType>,
}

/// Immutable and owned data, required by the LSP to carry out specific analysis.
/// It is basically an owned-subset of the typechecking state.
pub struct Extra {
    pub table: UnifTable,
    pub names: HashMap<VarId, Ident>,
    pub wildcards: Vec<Types>,
}

/// Typecheck a term.
///
/// Return the inferred type in case of success. This is just a wrapper that calls
/// `type_check_linearize` with a blanket implementation for the linearizer.
///
/// Note that this function doesn't recursively typecheck imports (anymore), but just the current
/// file. It however still needs the resolver to get the apparent type of imports.
///
/// Return the type inferred for type wildcards.
pub fn type_check(
    t: &RichTerm,
    initial_ctxt: Context,
    resolver: &impl ImportResolver,
) -> Result<Wildcards, TypecheckError> {
    type_check_linearize(
        t,
        initial_ctxt,
        resolver,
        StubHost::<(), (), _>::new(),
        Linearization::new(()),
    )
    .map(|(wildcards, _)| wildcards)
}

/// Typecheck a term and build its linearization. A linearization is a sequential data structure
/// that holds additional information (compared to the AST), such as types of subterms, variable
/// usages, etc.
///
/// Linearization is solely used by the LSP server.
pub fn type_check_linearize<LL>(
    t: &RichTerm,
    initial_ctxt: Context,
    resolver: &impl ImportResolver,
    mut linearizer: LL,
    mut building: Linearization<LL::Building>,
) -> Result<(Wildcards, LL::Completed), TypecheckError>
where
    LL: Linearizer<CompletionExtra = Extra>,
{
    let (mut table, mut names) = (UnifTable::new(), HashMap::new());
    let mut wildcard_vars = Vec::new();

    {
        let mut state: State = State {
            resolver,
            table: &mut table,
            constr: &mut RowConstr::new(),
            names: &mut names,
            wildcard_vars: &mut wildcard_vars,
        };

        walk(
            &mut state,
            initial_ctxt,
            &mut building,
            linearizer.scope(),
            t,
        )?;
    }

    let result = wildcard_vars_to_type(wildcard_vars.clone(), &table);
    let extra = Extra {
        table,
        names,
        wildcards: result.clone(),
    };
    let lin = linearizer.complete(building, extra).into_inner();

    Ok((result, lin))
}

/// Walk the AST of a term looking for statically typed block to check. Fill the linearization
/// alongside and store the apparent type of variable inside the typing environment.
fn walk<L: Linearizer>(
    state: &mut State,
    mut ctxt: Context,
    lin: &mut Linearization<L::Building>,
    mut linearizer: L,
    rt: &RichTerm,
) -> Result<(), TypecheckError> {
    let RichTerm { term: t, pos } = rt;
    linearizer.add_term(
        lin,
        t,
        *pos,
        UnifType::from_apparent_type(
            apparent_type(t, Some(&ctxt.type_env), Some(state.resolver)),
            &ctxt.term_env,
        ),
    );

    match t.as_ref() {
        Term::ParseError(_)
        | Term::RuntimeError(_)
        | Term::Null
        | Term::Bool(_)
        | Term::Num(_)
        | Term::Str(_)
        | Term::Lbl(_)
        | Term::Enum(_)
        | Term::SealingKey(_)
        // This function doesn't recursively typecheck imports: this is the responsibility of the
        // caller.
        | Term::Import(_)
        | Term::ResolvedImport(_) => Ok(()),
        Term::Var(x) => ctxt.type_env
            .get(x)
            .ok_or(TypecheckError::UnboundIdentifier(*x, *pos))
            .map(|_| ()),
        Term::StrChunks(chunks) => {
            chunks
                .iter()
                .try_for_each(|chunk| -> Result<(), TypecheckError> {
                    match chunk {
                        StrChunk::Literal(_) => Ok(()),
                        StrChunk::Expr(t, _) => {
                            walk(state, ctxt.clone(), lin, linearizer.scope(), t)
                        }
                    }
                })
        }
        Term::Fun(id, t) => {
            // The parameter of an unannotated function is always assigned type `Dyn`.
            ctxt.type_env.insert(*id, mk_uniftype::dynamic());
            walk(state, ctxt, lin, linearizer, t)
        }
        Term::FunPattern(id, pat, t) => {
            if let Some(id) = id {
                // The parameter of an unannotated function is always assigned type `Dyn`.
                ctxt.type_env.insert(*id, mk_uniftype::dynamic());
            }

            let pattern_ty = destructuring::build_pattern_type_walk_mode(state, &ctxt, pat)?;
            destructuring::inject_pattern_variables(state, &mut ctxt.type_env, pat, pattern_ty);
            walk(state, ctxt, lin, linearizer, t)
        }
        Term::Array(terms, _) => terms
            .iter()
            .try_for_each(|t| -> Result<(), TypecheckError> {
                walk(state, ctxt.clone(), lin, linearizer.scope(), t)
            }),
        Term::Let(x, re, rt, attrs) => {
            let ty_let = binding_type(state, re.as_ref(), &ctxt, false);

            // We don't support recursive binding when checking for contract equality.
            //
            // This would quickly lead to cycles, which are hard to deal with without leaking
            // memory. In order to deal with recursive bindings, the best way is probably to
            // allocate all the term environments inside an arena, local to each statically typed
            // block, and use bare references to represent cycles. Then everything would be cleaned
            // at the end of the block.
            ctxt.term_env.0.insert(*x, (re.clone(), ctxt.term_env.clone()));

            if attrs.rec {
                ctxt.type_env.insert(*x, ty_let.clone());
            }

            linearizer.retype_ident(lin, x, ty_let.clone());
            walk(state, ctxt.clone(), lin, linearizer.scope(), re)?;

            if !attrs.rec {
                ctxt.type_env.insert(*x, ty_let);
            }

            walk(state, ctxt, lin, linearizer, rt)
        }
        Term::LetPattern(x, pat, re, rt) => {
            let ty_let = binding_type(state, re.as_ref(), &ctxt, false);
            walk(state, ctxt.clone(), lin, linearizer.scope(), re)?;

            if let Some(x) = x {
                linearizer.retype_ident(lin, x, ty_let.clone());
                ctxt.type_env.insert(*x, ty_let);
            }

            let pattern_ty = destructuring::build_pattern_type_walk_mode(state, &ctxt, pat)?;
            destructuring::inject_pattern_variables(state, &mut ctxt.type_env, pat, pattern_ty);

            walk(state, ctxt, lin, linearizer, rt)
        }
        Term::App(e, t) => {
            walk(state, ctxt.clone(), lin, linearizer.scope(), e)?;
            walk(state, ctxt, lin, linearizer, t)
        }
        Term::Match {cases, default} => {
            cases.values().chain(default.iter()).try_for_each(|case| {
                walk(state, ctxt.clone(), lin, linearizer.scope(), case)
            })
        }
        Term::RecRecord(record, dynamic, ..) => {
            for (id, field) in record.fields.iter() {
                let field_type = field_type(
                    state,
                    field,
                    &ctxt,
                    false,
                );
                ctxt.type_env.insert(*id, field_type.clone());
                linearizer.retype_ident(lin, id, field_type);
            }

            // Walk the type and contract annotations

            // We don't bind the fields in the term environment used to check for contract
            // equality. See the `Let` case above for more details on why such recursive bindings
            // are currently ignored.
            record.fields
                .values()
                .try_for_each(|field| -> Result<(), TypecheckError> {
                    walk_field(state, ctxt.clone(), lin, linearizer.scope(), field)
                })?;

            dynamic.iter().map(|(_, field)| field)
                .try_for_each(|field| -> Result<(), TypecheckError> {
                    walk_field(state, ctxt.clone(), lin, linearizer.scope(), field)
                })
        }
        Term::Record(record) => {
            record.fields
                .values()
                .filter_map(|field| field.value.as_ref())
                .try_for_each(|t| -> Result<(), TypecheckError> {
                    walk(state, ctxt.clone(), lin, linearizer.scope(), t)
                })
        }
        Term::Op1(_, t) => walk(state, ctxt.clone(), lin, linearizer.scope(), t),
        Term::Op2(_, t1, t2) => {
            walk(state, ctxt.clone(), lin, linearizer.scope(), t1)?;
            walk(state, ctxt, lin, linearizer, t2)
        }
        Term::OpN(_, args) => {
           args.iter().try_for_each(|t| -> Result<(), TypecheckError> {
                    walk(
                        state,
                        ctxt.clone(),
                        lin,
                        linearizer.scope(),
                        t,
                    )
                },
            )
        }
        Term::Annotated(annot, rt) => {
            walk_annotated(state, ctxt, lin, linearizer, annot, rt)
        }
        Term::Sealed(_, t, _) => walk(state, ctxt, lin, linearizer, t)
   }
}

/// Same as [`walk`] but operate on a type, which can contain terms as contracts (`TypeF::Flat`),
/// instead of a term.
fn walk_type<L: Linearizer>(
    state: &mut State,
    ctxt: Context,
    lin: &mut Linearization<L::Building>,
    mut linearizer: L,
    ty: &Types,
) -> Result<(), TypecheckError> {
    match &ty.types {
       TypeF::Dyn
       | TypeF::Number
       | TypeF::Bool
       | TypeF::String
       | TypeF::Symbol
       // Currently, the parser can't generate unbound type variables by construction. Thus we
       // don't check here for unbound type variables again.
       | TypeF::Var(_)
       // An enum type can't contain a flat type inside.
       | TypeF::Enum(_)
       | TypeF::Wildcard(_) => Ok(()),
       TypeF::Arrow(ty1, ty2) => {
           walk_type(state, ctxt.clone(), lin, linearizer.scope(), ty1.as_ref())?;
           walk_type(state, ctxt, lin, linearizer, ty2.as_ref())
       }
       TypeF::Record(rrows) => walk_rrows(state, ctxt, lin, linearizer, rrows),
       TypeF::Flat(t) => walk(state, ctxt, lin, linearizer, t),
       TypeF::Dict { type_fields: ty2, .. }
       | TypeF::Array(ty2)
       | TypeF::Forall {body: ty2, ..} => walk_type(state, ctxt, lin, linearizer, ty2),
    }
}

/// Same as [`walk_type`] but operate on record rows.
fn walk_rrows<L: Linearizer>(
    state: &mut State,
    ctxt: Context,
    lin: &mut Linearization<L::Building>,
    mut linearizer: L,
    rrows: &RecordRows,
) -> Result<(), TypecheckError> {
    match rrows.0 {
        RecordRowsF::Empty
        // Currently, the parser can't generate unbound type variables by construction. Thus we
        // don't check here for unbound type variables again.
        | RecordRowsF::TailVar(_)
        | RecordRowsF::TailDyn => Ok(()),
        RecordRowsF::Extend { ref row, ref tail } => {
            walk_type(state, ctxt.clone(), lin, linearizer.scope(), &row.types)?;
            walk_rrows(state, ctxt, lin, linearizer, tail)
        }
    }
}

fn walk_field<L: Linearizer>(
    state: &mut State,
    ctxt: Context,
    lin: &mut Linearization<L::Building>,
    mut linearizer: L,
    field: &Field,
) -> Result<(), TypecheckError> {
    linearizer.add_field_metadata(lin, field);

    walk_with_annot(
        state,
        ctxt,
        lin,
        linearizer,
        &field.metadata.annotation,
        field.value.as_ref(),
    )
}

fn walk_annotated<L: Linearizer>(
    state: &mut State,
    ctxt: Context,
    lin: &mut Linearization<L::Building>,
    linearizer: L,
    annot: &TypeAnnotation,
    rt: &RichTerm,
) -> Result<(), TypecheckError> {
    walk_with_annot(state, ctxt, lin, linearizer, annot, Some(rt))
}

/// Walk an annotated term, either via [crate::term::record::FieldMetadata], or via a standalone
/// type or contract annotation. A type annotation switches the typechecking mode to _enforce_.
fn walk_with_annot<L: Linearizer>(
    state: &mut State,
    ctxt: Context,
    lin: &mut Linearization<L::Building>,
    mut linearizer: L,
    annot: &TypeAnnotation,
    value: Option<&RichTerm>,
) -> Result<(), TypecheckError> {
    annot.iter().try_for_each(|ty| {
        walk_type(state, ctxt.clone(), lin, linearizer.scope_meta(), &ty.types)
    })?;

    match (annot, value) {
        (
            TypeAnnotation {
                types: Some(LabeledType { types: ty2, .. }),
                ..
            },
            Some(value),
        ) => {
            let uty2 = UnifType::from_type(ty2.clone(), &ctxt.term_env);
            let instantiated = instantiate_foralls(state, uty2, ForallInst::Constant);
            check(state, ctxt, lin, linearizer.scope(), value, instantiated)
        }
        (_, Some(value)) => walk(state, ctxt, lin, linearizer.scope(), value),
        // TODO: we might have something to do with the linearizer to clear the current
        // metadata. It looks like it may be unduly attached to the next field definition,
        // which is not critical, but still a bug.
        _ => Ok(()),
    }
}

/// Check a term against a given type. Although this method mostly corresponds to checking mode in
/// the classical bidirectional framework, it combines both checking and inference modes in
/// practice, to avoid duplicating rules (that is, code) as detailed below.
///
/// # Literals
///
/// Checking a literal (a number, a string, a boolean, etc.) unifies the checked type with the
/// corresponding primitive type (`Number`, `String`, `Bool`, etc.). If the checked type is a
/// unification variable, `check` acts as an inference rule. If the type is concrete, unification
/// enforces equality, and `check` acts as a checking rule.
///
/// # Introduction rules
///
/// Following Pfenning's recipe (see [Bidirectional Typing][bidirectional-typing]),
/// introduction rules (e.g. typechecking a record) are checking. `check` follows the same logic
/// here: it uses unification to "match" on the expected type (in the case of record, a record type
/// or a dictionary type) and pushes typechecking down the record fields.
///
/// # Elimination rules
///
/// Elimination rules (such as function application or primitive operator application) only exist in
/// inference mode (still following Pfenning's recipe). `check` follows the inference mode here
/// (typically on function application, where we first call to `infer` on the function part, and
/// then check the argument).
///
/// Still, `check` is supposed to be implementing checking mode from the outside. We thus also
/// apply the typing rule which switches from inference to checking mode. Currently, subtyping
/// isn't supported yet in Nickel but is planned as part of RFC004. When subtyping lands, as the
/// name suggests, [`subsumption`] will be the place where we apply subsumption, as customary in
/// bidirectional type systems with subtyping.
///
/// To sum up, elimination rules inside `check` correspond to an inference rule composed with the
/// switching/subsumption rule, resulting in a composite checking rule.
///
/// # Parameters
///
/// - `state`: the unification state (see [`State`]).
/// - `env`: the typing environment, mapping free variable to types.
/// - `lin`: The current building linearization of building state `S`
/// - `linearizer`: A linearizer that can modify the linearization
/// - `t`: the term to check.
/// - `ty`: the type to check the term against.
///
/// # Linearization (LSP)
///
/// `check` is in charge of registering every term with the `linearizer` and makes sure to scope
/// the linearizer accordingly
///
/// [bidirectional-typing]: (https://arxiv.org/abs/1908.05839)
fn check<L: Linearizer>(
    state: &mut State,
    mut ctxt: Context,
    lin: &mut Linearization<L::Building>,
    mut linearizer: L,
    rt: &RichTerm,
    ty: UnifType,
) -> Result<(), TypecheckError> {
    let RichTerm { term: t, pos } = rt;
    linearizer.add_term(lin, t, *pos, ty.clone());

    match t.as_ref() {
        Term::ParseError(_) => Ok(()),
        Term::RuntimeError(_) => panic!("unexpected RuntimeError term during typechecking"),
        // null is inferred to be of type Dyn
        Term::Null => unify(state, &ctxt, ty, mk_uniftype::dynamic())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Bool(_) => unify(state, &ctxt, ty, mk_uniftype::bool())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Num(_) => unify(state, &ctxt, ty, mk_uniftype::num())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Str(_) => unify(state, &ctxt, ty, mk_uniftype::str())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::StrChunks(chunks) => {
            unify(state, &ctxt, ty, mk_uniftype::str())
                .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            chunks
                .iter()
                .try_for_each(|chunk| -> Result<(), TypecheckError> {
                    match chunk {
                        StrChunk::Literal(_) => Ok(()),
                        StrChunk::Expr(t, _) => check(
                            state,
                            ctxt.clone(),
                            lin,
                            linearizer.scope(),
                            t,
                            mk_uniftype::str(),
                        ),
                    }
                })
        }
        // Fun is an introduction rule for the arrow type. The target type is thus expected to be
        // `T -> U`, which is enforced by unification, and we then check the body of the function
        // against `U`, after adding `x : T` in the environment.
        Term::Fun(x, t) => {
            let src = state.table.fresh_type_uvar();
            let trg = state.table.fresh_type_uvar();
            let arr = mk_uty_arrow!(src.clone(), trg.clone());

            linearizer.retype_ident(lin, x, src.clone());

            unify(state, &ctxt, ty, arr).map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            ctxt.type_env.insert(*x, src);
            check(state, ctxt, lin, linearizer, t, trg)
        }
        Term::FunPattern(x, pat, t) => {
            let src_rows_ty = destructuring::build_pattern_type_check_mode(state, &ctxt, pat)?;
            let src = UnifType::Concrete(TypeF::Record(src_rows_ty.clone()));
            let trg = state.table.fresh_type_uvar();
            let arr = mk_uty_arrow!(src.clone(), trg.clone());

            if let Some(x) = x {
                linearizer.retype_ident(lin, x, src.clone());
                ctxt.type_env.insert(*x, src);
            }

            destructuring::inject_pattern_variables(state, &mut ctxt.type_env, pat, src_rows_ty);
            unify(state, &ctxt, ty, arr).map_err(|err| err.into_typecheck_err(state, rt.pos))?;
            check(state, ctxt, lin, linearizer, t, trg)
        }
        Term::Array(terms, _) => {
            let ty_elts = state.table.fresh_type_uvar();

            unify(state, &ctxt, ty, mk_uniftype::array(ty_elts.clone()))
                .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            terms
                .iter()
                .try_for_each(|t| -> Result<(), TypecheckError> {
                    check(
                        state,
                        ctxt.clone(),
                        lin,
                        linearizer.scope(),
                        t,
                        ty_elts.clone(),
                    )
                })
        }
        Term::Lbl(_) => {
            // TODO implement lbl type
            unify(state, &ctxt, ty, mk_uniftype::dynamic())
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Let(x, re, rt, attrs) => {
            let ty_let = binding_type(state, re.as_ref(), &ctxt, true);

            // We don't support recursive binding when checking for contract equality. See the
            // `Let` case in `walk`.
            ctxt.term_env
                .0
                .insert(*x, (re.clone(), ctxt.term_env.clone()));

            if attrs.rec {
                ctxt.type_env.insert(*x, ty_let.clone());
            }

            linearizer.retype_ident(lin, x, ty_let.clone());
            check(
                state,
                ctxt.clone(),
                lin,
                linearizer.scope(),
                re,
                ty_let.clone(),
            )?;

            if !attrs.rec {
                ctxt.type_env.insert(*x, ty_let);
            }
            check(state, ctxt, lin, linearizer, rt, ty)
        }
        Term::LetPattern(x, pat, re, rt) => {
            // The inferred type of the pattern w/ unification vars
            let pattern_rows_type =
                destructuring::build_pattern_type_check_mode(state, &ctxt, pat)?;
            let pattern_type = UnifType::Concrete(TypeF::Record(pattern_rows_type.clone()));
            // The inferred type of the expr being bound
            let ty_let = binding_type(state, re.as_ref(), &ctxt, true);

            unify(state, &ctxt, ty_let.clone(), pattern_type)
                .map_err(|e| e.into_typecheck_err(state, re.pos))?;

            check(
                state,
                ctxt.clone(),
                lin,
                linearizer.scope(),
                re,
                ty_let.clone(),
            )?;

            if let Some(x) = x {
                linearizer.retype_ident(lin, x, ty_let.clone());
                ctxt.type_env.insert(*x, ty_let);
            }

            destructuring::inject_pattern_variables(
                state,
                &mut ctxt.type_env,
                pat,
                pattern_rows_type,
            );

            check(state, ctxt, lin, linearizer, rt, ty)
        }
        Term::App(e, t) => {
            // This part corresponds to the infer rule for application.
            let function_type = infer(state, ctxt.clone(), lin, linearizer.scope(), e)?;

            let src = state.table.fresh_type_uvar();
            let tgt = state.table.fresh_type_uvar();
            let arr = mk_uty_arrow!(src.clone(), tgt.clone());

            unify(state, &ctxt, arr, function_type)
                .map_err(|err| err.into_typecheck_err(state, e.pos))?;

            check(state, ctxt.clone(), lin, linearizer, t, src)?;

            subsumption(state, &ctxt, tgt, ty).map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Match { cases, default } => {
            // Currently, if it has a default value, we typecheck the whole thing as
            // taking ANY enum, since it's more permissive and there's no loss of information.

            // A match expression is a special kind of function. Thus it's typed as `a -> b`, where
            // `a` is a enum type determined by the matched tags and `b` is the type of each match
            // arm.
            let arg_type = state.table.fresh_type_uvar();
            let return_type = state.table.fresh_type_uvar();

            // We unify the expected type of the match expression with `arg_type -> return_type`
            unify(
                state,
                &ctxt,
                ty,
                mk_uty_arrow!(arg_type.clone(), return_type.clone()),
            )
            .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            for case in cases.values() {
                check(
                    state,
                    ctxt.clone(),
                    lin,
                    linearizer.scope(),
                    case,
                    return_type.clone(),
                )?;
            }

            let erows = match default {
                Some(t) => {
                    check(state, ctxt.clone(), lin, linearizer.scope(), t, return_type)?;
                    state.table.fresh_erows_uvar()
                }
                None => cases.iter().try_fold(
                    EnumRowsF::Empty.into(),
                    |acc, x| -> Result<UnifEnumRows, TypecheckError> {
                        Ok(mk_uty_enum_row!(*x.0; acc))
                    },
                )?,
            };

            unify(state, &ctxt, arg_type, mk_uty_enum!(; erows))
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Var(x) => {
            let x_ty = ctxt
                .type_env
                .get(x)
                .cloned()
                .ok_or(TypecheckError::UnboundIdentifier(*x, *pos))?;

            let instantiated = instantiate_foralls(state, x_ty, ForallInst::Ptr);
            unify(state, &ctxt, ty, instantiated)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Enum(id) => {
            let row = state.table.fresh_erows_uvar();
            unify(state, &ctxt, ty, mk_uty_enum!(*id; row))
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        // If some fields are defined dynamically, the only potential type that works is `{_ : a}`
        // for some `a`. In other words, the checking rule is not the same depending on the target
        // type: if the target type is a dictionary type, we simply check each field against the
        // element type.
        Term::RecRecord(record, dynamic, ..) if !dynamic.is_empty() => {
            let ty_dict = state.table.fresh_type_uvar();
            unify(state, &ctxt, ty, mk_uniftype::dict(ty_dict.clone()))
                .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            //TODO: should we insert in the environment the checked type, or the actual type?
            for id in record.fields.keys() {
                ctxt.type_env.insert(*id, ty_dict.clone());
                linearizer.retype_ident(lin, id, ty_dict.clone())
            }

            // We don't bind recursive fields in the term environment used to check for contract.
            // See the recursive let case in `walk`.
            record
                .fields
                .iter()
                .try_for_each(|(id, field)| -> Result<(), TypecheckError> {
                    check_field(
                        state,
                        ctxt.clone(),
                        lin,
                        linearizer.scope(),
                        *id,
                        field,
                        ty_dict.clone(),
                    )
                })
        }
        Term::Record(record) | Term::RecRecord(record, ..) => {
            // For recursive records, we look at the apparent type of each field and bind it in
            // ctxt before actually typechecking the content of fields.
            //
            // Fields defined by interpolation are ignored, because they can't be referred to
            // recursively.
            if let Term::RecRecord(..) = t.as_ref() {
                for (id, field) in &record.fields {
                    let uty = field_type(state, field, &ctxt, true);
                    ctxt.type_env.insert(*id, uty.clone());
                    linearizer.retype_ident(lin, id, uty);
                }
            }

            let root_ty = ty.clone().into_root(state.table);

            if let UnifType::Concrete(TypeF::Dict {
                type_fields: rec_ty,
                ..
            }) = root_ty
            {
                // Checking for a dictionary
                record
                    .fields
                    .iter()
                    .try_for_each(|(id, field)| -> Result<(), TypecheckError> {
                        check_field(
                            state,
                            ctxt.clone(),
                            lin,
                            linearizer.scope(),
                            *id,
                            field,
                            (*rec_ty).clone(),
                        )
                    })
            } else {
                // Building the type {id1 : ?a1, id2: ?a2, .., idn: ?an}
                let mut field_types: HashMap<Ident, UnifType> = record
                    .fields
                    .keys()
                    .map(|id| (*id, state.table.fresh_type_uvar()))
                    .collect();

                let rows = field_types.iter().fold(
                    mk_uty_row!(),
                    |acc, (id, row_ty)| mk_uty_row!((*id, row_ty.clone()); acc),
                );

                unify(state, &ctxt, ty, mk_uty_record!(; rows))
                    .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

                for (id, field) in record.fields.iter() {
                    if let Term::RecRecord(..) = t.as_ref() {
                        let affected_type = ctxt.type_env.get(id).cloned().unwrap();
                        unify(
                            state,
                            &ctxt,
                            field_types.get(id).cloned().unwrap(),
                            affected_type,
                        )
                        .map_err(|err| {
                            err.into_typecheck_err(
                                state,
                                field.value.as_ref().map(|v| v.pos).unwrap_or_default(),
                            )
                        })?;
                    }

                    check_field(
                        state,
                        ctxt.clone(),
                        lin,
                        linearizer.scope(),
                        *id,
                        field,
                        // expect(): we've built `rows` in this very function from
                        // record.fields.keys(), so it must contain `id`
                        field_types
                            .remove(id)
                            .expect("inserted `id` inside the `field_types` hashmap previously; expected it to be there"),
                    )?;
                }

                Ok(())
            }
        }
        // Primitive operator application follows the inference discipline, like function
        // application, because it's an elimination rule (as far as typechecking is concerned,
        // primitive operator application is strictly the same as normal function application).
        //
        // Note that the order of checking an argument against an inferred type or unifying the
        // target type with an expected value doesn't matter (those operation commutes). We usually
        // `unify` first, because it doesn't consume the context, while `check` needs an owned
        // value.
        Term::Op1(op, t) => {
            let (ty_arg, ty_res) = get_uop_type(state, op)?;

            //TODO: this should go after the call to `subsumption`, to avoid context cloning, once
            //we get rid of the special casing of type instantation below. For the time being, the
            //instantiation is depending on this check having happened before, so we have to leave
            //it as it is.
            check(state, ctxt.clone(), lin, linearizer.scope(), t, ty_arg)?;

            let instantiated = instantiate_foralls(state, ty_res, ForallInst::Ptr);
            subsumption(state, &ctxt, instantiated, ty)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Op2(op, t1, t2) => {
            let (ty_arg1, ty_arg2, ty_res) = get_bop_type(state, op)?;

            subsumption(state, &ctxt, ty_res, ty)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            check(state, ctxt.clone(), lin, linearizer.scope(), t1, ty_arg1)?;
            check(state, ctxt, lin, linearizer, t2, ty_arg2)
        }
        Term::OpN(op, args) => {
            let (tys_op, ty_ret) = get_nop_type(state, op)?;

            unify(state, &ctxt, ty, ty_ret).map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            tys_op.into_iter().zip(args.iter()).try_for_each(
                |(ty_t, t)| -> Result<_, TypecheckError> {
                    check(state, ctxt.clone(), lin, linearizer.scope(), t, ty_t)?;
                    Ok(())
                },
            )?;

            Ok(())
        }
        Term::Annotated(annot, rt) => check_annotated(state, ctxt, lin, linearizer, annot, rt, ty),
        Term::SealingKey(_) => unify(state, &ctxt, ty, mk_uniftype::sym())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Sealed(_, t, _) => check(state, ctxt, lin, linearizer, t, ty),
        Term::Import(_) => unify(state, &ctxt, ty, mk_uniftype::dynamic())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        // We use the apparent type of the import for checking. This function doesn't recursively
        // typecheck imports: this is the responsibility of the caller.
        Term::ResolvedImport(file_id) => {
            let t = state
                .resolver
                .get(*file_id)
                .expect("Internal error: resolved import not found during typechecking.");
            let ty_import: UnifType = UnifType::from_apparent_type(
                apparent_type(t.as_ref(), Some(&ctxt.type_env), Some(state.resolver)),
                &ctxt.term_env,
            );
            unify(state, &ctxt, ty, ty_import).map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
    }
}

/// Change from inference mode to checking mode, and apply a potential subsumption rule.
///
/// Currently, there is no subtyping (until RFC004 is implemented), hence this function simply
/// performs unification (put differently, the subtyping relation is the equality relation). In the
/// future, this function might implement a non-trivial subsumption rule.
pub fn subsumption(
    state: &mut State,
    ctxt: &Context,
    inferred: UnifType,
    checked: UnifType,
) -> Result<(), UnifError> {
    unify(state, ctxt, checked, inferred)
}

fn check_field<L: Linearizer>(
    state: &mut State,
    ctxt: Context,
    lin: &mut Linearization<L::Building>,
    mut linearizer: L,
    id: Ident,
    field: &Field,
    ty: UnifType,
) -> Result<(), TypecheckError> {
    linearizer.add_field_metadata(lin, field);

    check_with_annot(
        state,
        ctxt,
        lin,
        linearizer,
        &field.metadata.annotation,
        field.value.as_ref(),
        ty,
        field.value.as_ref().map(|v| v.pos).unwrap_or(id.pos),
    )
}

fn check_annotated<L: Linearizer>(
    state: &mut State,
    ctxt: Context,
    lin: &mut Linearization<L::Building>,
    linearizer: L,
    annot: &TypeAnnotation,
    rt: &RichTerm,
    ty: UnifType,
) -> Result<(), TypecheckError> {
    check_with_annot(state, ctxt, lin, linearizer, annot, Some(rt), ty, rt.pos)
}

/// Function handling the common part of typechecking terms with type or contract annotation, with
/// or without definitions. This encompasses both standalone type annotation (where `value` is
/// always `Some(_)`) as well as field definiitions (where `value` may or may not be defined).
///
/// The last argument is a position to use for error reporting when `value` is `None`.
#[allow(clippy::too_many_arguments)] // TODO: Is it worth doing something about it?
fn check_with_annot<L: Linearizer>(
    state: &mut State,
    ctxt: Context,
    lin: &mut Linearization<L::Building>,
    mut linearizer: L,
    annot: &TypeAnnotation,
    value: Option<&RichTerm>,
    ty: UnifType,
    pos: TermPos,
) -> Result<(), TypecheckError> {
    annot.iter().try_for_each(|ty| {
        walk_type(state, ctxt.clone(), lin, linearizer.scope_meta(), &ty.types)
    })?;

    match (annot, value) {
        (
            TypeAnnotation {
                types: Some(LabeledType { types: ty2, .. }),
                ..
            },
            Some(value),
        ) => {
            let uty2 = UnifType::from_type(ty2.clone(), &ctxt.term_env);
            let instantiated = instantiate_foralls(state, uty2.clone(), ForallInst::Constant);

            unify(state, &ctxt, uty2, ty).map_err(|err| err.into_typecheck_err(state, pos))?;
            check(state, ctxt, lin, linearizer, value, instantiated)
        }
        // A annotation without a type but with a contract switches the typechecker back to walk
        // mode. If there are several contracts, we arbitrarily chose the first one as the apparent
        // type (the most precise type would be the intersection of all contracts, but Nickel's
        // type system doesn't feature intersection types).
        (
            TypeAnnotation {
                types: None,
                contracts,
            },
            value_opt,
        ) if !contracts.is_empty() => {
            let ctr = contracts.get(0).unwrap();
            let LabeledType { types: ty2, .. } = ctr;

            unify(
                state,
                &ctxt,
                ty,
                UnifType::from_type(ty2.clone(), &ctxt.term_env),
            )
            .map_err(|err| err.into_typecheck_err(state, pos))?;

            // if there's an inner value, we still have to walk it, as it may contain
            // statically typed blocks.
            if let Some(value) = value_opt {
                walk(state, ctxt, lin, linearizer, value)
            } else {
                // TODO: we might have something to with the linearizer to clear the current
                // metadata. It looks like it may be unduly attached to the next field definition,
                // which is not critical, but still a bug.
                Ok(())
            }
        }
        // A non-empty value without a type or a contract annotation is typechecked in the same way
        // as its inner value
        (_, Some(value)) => check(state, ctxt, lin, linearizer, value, ty),
        // A empty value is a record field without definition. We don't check anything, and infer
        // its type to be either the first annotation defined if any, or `Dyn` otherwise.
        //
        // TODO: we might have something to with the linearizer to clear the current metadata.
        // It looks like it may be unduly attached to the next field definition, which is not
        // critical, but still a bug.
        _ => {
            let inferred = annot
                .first()
                .map(|labeled_ty| UnifType::from_type(labeled_ty.types.clone(), &ctxt.term_env))
                .unwrap_or_else(mk_uniftype::dynamic);
            unify(state, &ctxt, ty, inferred).map_err(|err| err.into_typecheck_err(state, pos))
        }
    }
}

/// Infer a type for an expression.
///
/// `infer` corresponds to the inference mode of bidirectional typechecking. Nickel uses a mix of
/// bidirectional typechecking together with traditional ML-like unification. In practice, to avoid
/// duplicating a lot of rules for both checking mode and inference mode, the current [`check`]
/// function mixes both and inference simply corresponds to checking against a free unification
/// variable.
///
/// Still, using this dedicated method - although it is a thin wrapper - helps making clear when
/// inference mode is used and when checking mode is used in the typechecking algorithm.
fn infer<L: Linearizer>(
    state: &mut State,
    ctxt: Context,
    lin: &mut Linearization<L::Building>,
    linearizer: L,
    rt: &RichTerm,
) -> Result<UnifType, TypecheckError> {
    let inferred = state.table.fresh_type_uvar();
    check(state, ctxt, lin, linearizer, rt, inferred.clone())?;
    Ok(inferred.into_root(state.table))
}

/// Determine the type of a let-bound expression.
///
/// Call [`apparent_type`] to see if the binding is annotated. If it is, return this type as a
/// [`UnifType`]. Otherwise:
///     * in non strict mode, we won't (and possibly can't) infer the type of `bound_exp`: just
///       return `Dyn`.
///     * in strict mode, we will typecheck `bound_exp`: return a new unification variable to be
///       associated to `bound_exp`.
/// As this function is always called in a context where an `ImportResolver` is present, expect
/// it passed in arguments.
///
/// If the annotated type contains any wildcard:
///     * in non strict mode, wildcards are assigned `Dyn`.
///     * in strict mode, the wildcard is typechecked, and we return the unification variable
///       corresponding to it.
fn binding_type(state: &mut State, t: &Term, ctxt: &Context, strict: bool) -> UnifType {
    apparent_or_infer(
        state,
        apparent_type(t, Some(&ctxt.type_env), Some(state.resolver)),
        ctxt,
        strict,
    )
}

/// Same as `binding_type` but for record field definition.
fn field_type(state: &mut State, field: &Field, ctxt: &Context, strict: bool) -> UnifType {
    apparent_or_infer(
        state,
        field_apparent_type(field, Some(&ctxt.type_env), Some(state.resolver)),
        ctxt,
        strict,
    )
}

/// Either returns the exact type annotation extracted as an apparent type, or return a fresh
/// unification variable, for the type to be inferred by the typechecker, in enforce mode.
///
/// In walk mode, returns the type as approximated by [`apparent_type`].
fn apparent_or_infer(
    state: &mut State,
    aty: ApparentType,
    ctxt: &Context,
    strict: bool,
) -> UnifType {
    match aty {
        ApparentType::Annotated(ty) if strict => {
            replace_wildcards_with_var(state.table, state.wildcard_vars, ty, &ctxt.term_env)
        }
        ApparentType::Approximated(_) if strict => state.table.fresh_type_uvar(),
        ty_apt => UnifType::from_apparent_type(ty_apt, &ctxt.term_env),
    }
}

/// Substitute wildcards in a type for their unification variable.
fn replace_wildcards_with_var(
    table: &mut UnifTable,
    wildcard_vars: &mut Vec<UnifType>,
    ty: Types,
    env: &SimpleTermEnvironment,
) -> UnifType {
    fn replace_rrows(
        table: &mut UnifTable,
        wildcard_vars: &mut Vec<UnifType>,
        rrows: RecordRows,
        env: &SimpleTermEnvironment,
    ) -> UnifRecordRows {
        UnifRecordRows::Concrete(rrows.0.map_state(
            |ty, (table, wildcard_vars)| {
                Box::new(replace_wildcards_with_var(table, wildcard_vars, *ty, env))
            },
            |rrows, (table, wildcard_vars)| {
                Box::new(replace_rrows(table, wildcard_vars, *rrows, env))
            },
            &mut (table, wildcard_vars),
        ))
    }

    match ty.types {
        TypeF::Wildcard(i) => get_wildcard_var(table, wildcard_vars, i),
        TypeF::Flat(t) => UnifType::Contract(t, env.clone()),
        _ => UnifType::Concrete(ty.types.map_state(
            |ty, (table, wildcard_vars)| {
                Box::new(replace_wildcards_with_var(table, wildcard_vars, *ty, env))
            },
            |rrows, (table, wildcard_vars)| replace_rrows(table, wildcard_vars, rrows, env),
            // Enum rows contain neither wildcards nor contracts
            |erows, _| UnifEnumRows::from(erows),
            &mut (table, wildcard_vars),
        )),
    }
}

/// Different kinds of apparent types (see [`apparent_type`]).
///
/// Indicate the nature of an apparent type. In particular, when in enforce mode, the typechecker
/// throws away approximations as it can do better and infer the actual type of an expression.  In
/// walk mode, however, the approximation is the best we can do. This type allows the caller of
/// `apparent_type` to determine which situation it is.
#[derive(Debug)]
pub enum ApparentType {
    /// The apparent type is given by a user-provided annotation.
    Annotated(Types),
    /// The apparent type has been inferred from a simple expression.
    Inferred(Types),
    /// The term is a variable and its type was retrieved from the typing environment.
    FromEnv(UnifType),
    /// The apparent type wasn't trivial to determine, and an approximation (most of the time,
    /// `Dyn`) has been returned.
    Approximated(Types),
}

impl From<ApparentType> for Types {
    fn from(at: ApparentType) -> Self {
        match at {
            ApparentType::Annotated(ty) if has_wildcards(&ty) => Types::from(TypeF::Dyn),
            ApparentType::Annotated(ty)
            | ApparentType::Inferred(ty)
            | ApparentType::Approximated(ty) => ty,
            ApparentType::FromEnv(uty) => uty.try_into().ok().unwrap_or(Types::from(TypeF::Dyn)),
        }
    }
}

/// Return the apparent type of a field, by first looking at the type annotation, if any, the at
/// the contracts annotation, and if there is none, fall back to the apparent type of the value. If
/// there is no value, `Approximated(Dyn)` is returned.
fn field_apparent_type(
    field: &Field,
    env: Option<&Environment>,
    resolver: Option<&dyn ImportResolver>,
) -> ApparentType {
    field
        .metadata
        .annotation
        .first()
        .cloned()
        .map(|labeled_ty| ApparentType::Annotated(labeled_ty.types))
        .or_else(|| {
            field
                .value
                .as_ref()
                .map(|v| apparent_type(v.as_ref(), env, resolver))
        })
        .unwrap_or(ApparentType::Approximated(Types::from(TypeF::Dyn)))
}

/// Determine the apparent type of a let-bound expression.
///
/// When a let-binding `let x = bound_exp in body` is processed, the type of `bound_exp` must be
/// determined in order to be bound to the variable `x` in the typing environment.
/// Then, future occurrences of `x` can be given this type when used in a `Promise` block.
///
/// The role of `apparent_type` is precisely to determine the type of `bound_exp`:
/// - if `bound_exp` is annotated by a type or contract annotation, return the user-provided type,
///   unless that type is a wildcard.
/// - if `bound_exp` is a constant (string, number, boolean or symbol) which type can be deduced
///   directly without unfolding the expression further, return the corresponding exact type.
/// - if `bound_exp` is an array, return `Array Dyn`.
/// - if `bound_exp` is a resolved import, return the apparent type of the imported term. Returns
///   `Dyn` if the resolver is not passed as a parameter to the function.
/// - Otherwise, return an approximation of the type (currently `Dyn`, but could be more precise in
///   the future, such as `Dyn -> Dyn` for functions, `{ | Dyn}` for records, and so on).
pub fn apparent_type(
    t: &Term,
    env: Option<&Environment>,
    resolver: Option<&dyn ImportResolver>,
) -> ApparentType {
    use codespan::FileId;

    // Check the apparent type while avoiding cycling through direct imports loops. Indeed,
    // `apparent_type` tries to see through imported terms. But doing so can lead to an infinite
    // loop, for example with the trivial program which imports itself:
    //
    // ```nickel
    // # foo.ncl
    // import "foo.ncl"
    // ```
    //
    // The following function thus remembers what imports have been seen already, and simply
    // returns `Dyn` if it detects a cycle.
    fn apparent_type_check_cycle(
        t: &Term,
        env: Option<&Environment>,
        resolver: Option<&dyn ImportResolver>,
        mut imports_seen: HashSet<FileId>,
    ) -> ApparentType {
        match t {
            Term::Annotated(annot, value) => annot
                .first()
                .map(|labeled_ty| ApparentType::Annotated(labeled_ty.types.clone()))
                .unwrap_or_else(|| apparent_type(value.as_ref(), env, resolver)),
            Term::Num(_) => ApparentType::Inferred(Types::from(TypeF::Number)),
            Term::Bool(_) => ApparentType::Inferred(Types::from(TypeF::Bool)),
            Term::SealingKey(_) => ApparentType::Inferred(Types::from(TypeF::Symbol)),
            Term::Str(_) | Term::StrChunks(_) => ApparentType::Inferred(Types::from(TypeF::String)),
            Term::Array(..) => ApparentType::Approximated(Types::from(TypeF::Array(Box::new(
                Types::from(TypeF::Dyn),
            )))),
            Term::Var(id) => env
                .and_then(|envs| envs.get(id).cloned())
                .map(ApparentType::FromEnv)
                .unwrap_or(ApparentType::Approximated(Types::from(TypeF::Dyn))),
            Term::ResolvedImport(file_id) => match resolver {
                Some(r) if !imports_seen.contains(file_id) => {
                    imports_seen.insert(*file_id);

                    let t = r
                        .get(*file_id)
                        .expect("Internal error: resolved import not found during typechecking.");
                    apparent_type_check_cycle(&t.term, env, Some(r), imports_seen)
                }
                _ => ApparentType::Approximated(Types::from(TypeF::Dyn)),
            },
            _ => ApparentType::Approximated(Types::from(TypeF::Dyn)),
        }
    }

    apparent_type_check_cycle(t, env, resolver, HashSet::new())
}

/// Infer the type of a non-annotated record by recursing inside gathering the apparent type of the
/// fields. It's currently used essentially to type the stdlib.
///
/// # Parameters
///
/// - `rt`: the term to infer a type for
/// - `term_env`: the current term environment, used for contracts equality
/// - `max_depth`: the max recursion depth. `infer_record_type` descends into sub-records, as long
///   as it only encounters nested record literals. `max_depth` is used to control this behavior
///   and cap the work that `infer_record_type` might do.
pub fn infer_record_type(
    rt: &RichTerm,
    term_env: &SimpleTermEnvironment,
    max_depth: u8,
) -> UnifType {
    match rt.as_ref() {
        Term::Record(record) | Term::RecRecord(record, ..) if max_depth > 0 => UnifType::from(
            TypeF::Record(UnifRecordRows::Concrete(record.fields.iter().fold(
                RecordRowsF::Empty,
                |r, (id, field)| {
                    let uty = match field_apparent_type(field, None, None) {
                        ApparentType::Annotated(ty) => UnifType::from_type(ty, term_env),
                        ApparentType::FromEnv(uty) => uty,
                        // If we haven't reached max_depth yet, and the type is only approximated,
                        // we try to recursively infer a better type.
                        ApparentType::Inferred(ty) | ApparentType::Approximated(ty)
                            if max_depth > 0 =>
                        {
                            field
                                .value
                                .as_ref()
                                .map(|v| infer_record_type(v, term_env, max_depth - 1))
                                .unwrap_or(UnifType::from_type(ty, term_env))
                        }
                        ApparentType::Inferred(ty) | ApparentType::Approximated(ty) => {
                            UnifType::from_type(ty, term_env)
                        }
                    };

                    RecordRowsF::Extend {
                        row: UnifRecordRow {
                            id: *id,
                            types: Box::new(uty),
                        },
                        tail: Box::new(r.into()),
                    }
                },
            ))),
        ),
        t => UnifType::from_apparent_type(
            apparent_type(t, None, None),
            &SimpleTermEnvironment::new(),
        ),
    }
}

/// Deeply check whether a type contains a wildcard.
fn has_wildcards(ty: &Types) -> bool {
    let mut has_wildcard = false;
    ty.clone()
        .traverse::<_, _, std::convert::Infallible>(
            &|ty: Types, has_wildcard| {
                if ty.types.is_wildcard() {
                    *has_wildcard = true;
                }
                Ok(ty)
            },
            &mut has_wildcard,
            TraverseOrder::TopDown,
        )
        .unwrap();
    has_wildcard
}

/// Try to find a specific row inside record rows, or add it if permitted.
///
/// If the row is present, this function returns the corresponding type together with the tail
/// corresponding to the other rows coming after the found one.
///
/// If the row is not present:
///
/// - If the given record rows are extensible, i.e. they end with a free unification variable, this
///   function adds a new row with the provided type `ty` (if allowed by [row
///   constraints][RowConstr]). Returns `ty` together with the new tail (a fresh unification
///   variable).
/// - Otherwise, raise a missing row error.
fn rrows_add(
    state: &mut State,
    id: &Ident,
    ty: Box<UnifType>,
    rrows: UnifRecordRows,
) -> Result<(Box<UnifType>, UnifRecordRows), RowUnifError> {
    let rrows = rrows.into_root(state.table);

    match rrows {
        UnifRecordRows::Concrete(erows) => match erows {
            RecordRowsF::Empty | RecordRowsF::TailDyn | RecordRowsF::TailVar(_) => {
                Err(RowUnifError::MissingRow(*id))
            }
            RecordRowsF::Extend { row, tail } => {
                if *id == row.id {
                    Ok((row.types, *tail))
                } else {
                    let (extracted_type, subrow) = rrows_add(state, id, ty, *tail)?;
                    Ok((
                        extracted_type,
                        UnifRecordRows::Concrete(RecordRowsF::Extend {
                            row,
                            tail: Box::new(subrow),
                        }),
                    ))
                }
            }
        },
        UnifRecordRows::UnifVar(uvar) => {
            if state
                .constr
                .get(&uvar)
                .map(|set| set.contains(id))
                .unwrap_or(false)
            {
                return Err(RowUnifError::UnsatConstr(*id, Some(*ty)));
            }
            let tail_var_id = state.table.fresh_rrows_var_id();
            let new_tail = UnifRecordRows::Concrete(RecordRowsF::Extend {
                row: RecordRowF {
                    id: *id,
                    types: ty.clone(),
                },
                tail: Box::new(UnifRecordRows::UnifVar(tail_var_id)),
            });

            new_tail.constrain_fresh_rrows_var(state, tail_var_id);

            state.table.assign_rrows(uvar, new_tail);

            Ok((ty, UnifRecordRows::UnifVar(tail_var_id)))
        }
        UnifRecordRows::Constant(_) => Err(RowUnifError::MissingRow(*id)),
    }
}

/// Try to find a specific row (ident) inside enum rows, or add it if permitted.
///
/// If the row is present, this function returns the tail corresponding to the remaining rows coming after
/// the found one.
///
/// If the row is not present:
///
/// - If the given enum rows are extensible, i.e. they end with a free unification variable, this
///   function adds a new row (if allowed by [row constraints][RowConstr]). Returns the new tail (a
///   fresh unification variable).
/// - Otherwise, raise a missing row error.
fn erows_add(
    state: &mut State,
    id: &Ident,
    uerows: UnifEnumRows,
) -> Result<UnifEnumRows, RowUnifError> {
    let uerows = uerows.into_root(state.table);

    match uerows {
        UnifEnumRows::Concrete(erows) => match erows {
            EnumRowsF::Empty | EnumRowsF::TailVar(_) => Err(RowUnifError::MissingRow(*id)),
            EnumRowsF::Extend { row, tail } => {
                if *id == row {
                    Ok(*tail)
                } else {
                    let subrow = erows_add(state, id, *tail)?;
                    Ok(UnifEnumRows::Concrete(EnumRowsF::Extend {
                        row,
                        tail: Box::new(subrow),
                    }))
                }
            }
        },
        UnifEnumRows::UnifVar(uvar) => {
            if state
                .constr
                .get(&uvar)
                .map(|set| set.contains(id))
                .unwrap_or(false)
            {
                return Err(RowUnifError::UnsatConstr(*id, None));
            }
            let tail_var_id = state.table.fresh_erows_var_id();
            let new_tail = UnifEnumRows::Concrete(EnumRowsF::Extend {
                row: *id,
                tail: Box::new(UnifEnumRows::UnifVar(tail_var_id)),
            });

            new_tail.constrain_fresh_erows_var(state, tail_var_id);

            state.table.assign_erows(uvar, new_tail);

            Ok(UnifEnumRows::UnifVar(tail_var_id))
        }
        UnifEnumRows::Constant(_) => Err(RowUnifError::MissingRow(*id)),
    }
}

/// Try to unify two types.
pub fn unify(
    state: &mut State,
    ctxt: &Context,
    t1: UnifType,
    t2: UnifType,
) -> Result<(), UnifError> {
    let t1 = t1.into_root(state.table);
    let t2 = t2.into_root(state.table);

    // t1 and t2 are roots of the type
    match (t1, t2) {
        // If either type is a wildcard, unify with the associated type var
        (UnifType::Concrete(TypeF::Wildcard(id)), ty2)
        | (ty2, UnifType::Concrete(TypeF::Wildcard(id))) => {
            let ty1 = get_wildcard_var(state.table, state.wildcard_vars, id);
            unify(state, ctxt, ty1, ty2)
        }
        (UnifType::Concrete(s1), UnifType::Concrete(s2)) => match (s1, s2) {
            (TypeF::Dyn, TypeF::Dyn)
            | (TypeF::Number, TypeF::Number)
            | (TypeF::Bool, TypeF::Bool)
            | (TypeF::String, TypeF::String)
            | (TypeF::Symbol, TypeF::Symbol) => Ok(()),
            (TypeF::Array(uty1), TypeF::Array(uty2)) => unify(state, ctxt, *uty1, *uty2),
            (TypeF::Arrow(s1s, s1t), TypeF::Arrow(s2s, s2t)) => {
                unify(state, ctxt, (*s1s).clone(), (*s2s).clone()).map_err(|err| {
                    UnifError::DomainMismatch(
                        UnifType::Concrete(TypeF::Arrow(s1s.clone(), s1t.clone())),
                        UnifType::Concrete(TypeF::Arrow(s2s.clone(), s2t.clone())),
                        Box::new(err),
                    )
                })?;
                unify(state, ctxt, (*s1t).clone(), (*s2t).clone()).map_err(|err| {
                    UnifError::CodomainMismatch(
                        UnifType::Concrete(TypeF::Arrow(s1s, s1t)),
                        UnifType::Concrete(TypeF::Arrow(s2s, s2t)),
                        Box::new(err),
                    )
                })
            }
            (TypeF::Flat(s), TypeF::Flat(t)) => Err(UnifError::IncomparableFlatTypes(s, t)),
            (TypeF::Enum(erows1), TypeF::Enum(erows2)) => {
                unify_erows(state, erows1.clone(), erows2.clone()).map_err(|err| {
                    err.into_unif_err(mk_uty_enum!(; erows1), mk_uty_enum!(; erows2))
                })
            }
            (TypeF::Record(rrows1), TypeF::Record(rrows2)) => {
                unify_rrows(state, ctxt, rrows1.clone(), rrows2.clone()).map_err(|err| {
                    err.into_unif_err(mk_uty_record!(; rrows1), mk_uty_record!(; rrows2))
                })
            }
            (
                TypeF::Dict {
                    type_fields: uty1, ..
                },
                TypeF::Dict {
                    type_fields: uty2, ..
                },
            ) => unify(state, ctxt, *uty1, *uty2),
            (
                TypeF::Forall {
                    var: var1,
                    var_kind: var_kind1,
                    body: body1,
                },
                TypeF::Forall {
                    var: var2,
                    var_kind: var_kind2,
                    body: body2,
                },
            ) if var_kind1 == var_kind2 => {
                // Very stupid (slow) implementation
                let (substd1, substd2) = match var_kind1 {
                    VarKind::Type => {
                        let constant_type = state.table.fresh_type_const();
                        (
                            body1.subst_type(&var1, &constant_type),
                            body2.subst_type(&var2, &constant_type),
                        )
                    }
                    VarKind::RecordRows => {
                        let constant_type = state.table.fresh_rrows_const();
                        (
                            body1.subst_rrows(&var1, &constant_type),
                            body2.subst_rrows(&var2, &constant_type),
                        )
                    }
                    VarKind::EnumRows => {
                        let constant_type = state.table.fresh_erows_const();
                        (
                            body1.subst_erows(&var1, &constant_type),
                            body2.subst_erows(&var2, &constant_type),
                        )
                    }
                };

                unify(state, ctxt, substd1, substd2)
            }
            (TypeF::Var(ident), _) | (_, TypeF::Var(ident)) => {
                Err(UnifError::UnboundTypeVariable(ident))
            }
            (ty1, ty2) => Err(UnifError::TypeMismatch(
                UnifType::Concrete(ty1),
                UnifType::Concrete(ty2),
            )),
        },
        (UnifType::UnifVar(p1), UnifType::UnifVar(p2)) if p1 == p2 => Ok(()),
        (UnifType::UnifVar(p), uty) | (uty, UnifType::UnifVar(p)) => {
            state.table.assign_type(p, uty);
            Ok(())
        }
        (UnifType::Constant(i1), UnifType::Constant(i2)) if i1 == i2 => Ok(()),
        (UnifType::Constant(i1), UnifType::Constant(i2)) => Err(UnifError::ConstMismatch(i1, i2)),
        (ty, UnifType::Constant(i)) | (UnifType::Constant(i), ty) => {
            Err(UnifError::WithConst(i, ty))
        }
        (UnifType::Contract(t1, env1), UnifType::Contract(t2, env2))
            if eq::contract_eq(state.table.max_uvars_count(), &t1, &env1, &t2, &env2) =>
        {
            Ok(())
        }
        (uty1 @ UnifType::Contract(..), uty2) | (uty1, uty2 @ UnifType::Contract(..)) => {
            Err(UnifError::TypeMismatch(uty1, uty2))
        }
    }
}

/// Try to unify two record row types.
pub fn unify_rrows(
    state: &mut State,
    ctxt: &Context,
    urrows1: UnifRecordRows,
    urrows2: UnifRecordRows,
) -> Result<(), RowUnifError> {
    let urrows1 = urrows1.into_root(state.table);
    let urrows2 = urrows2.into_root(state.table);

    match (urrows1, urrows2) {
        (UnifRecordRows::Concrete(rrows1), UnifRecordRows::Concrete(rrows2)) => {
            match (rrows1, rrows2) {
                (RecordRowsF::TailVar(id), _) | (_, RecordRowsF::TailVar(id)) => {
                    Err(RowUnifError::UnboundTypeVariable(id))
                }
                (RecordRowsF::Empty, RecordRowsF::Empty)
                | (RecordRowsF::TailDyn, RecordRowsF::TailDyn) => Ok(()),
                (RecordRowsF::Empty, RecordRowsF::TailDyn) => Err(RowUnifError::ExtraDynTail()),
                (RecordRowsF::TailDyn, RecordRowsF::Empty) => Err(RowUnifError::MissingDynTail()),
                (
                    RecordRowsF::Empty,
                    RecordRowsF::Extend {
                        row: UnifRecordRow { id, .. },
                        ..
                    },
                )
                | (
                    RecordRowsF::TailDyn,
                    RecordRowsF::Extend {
                        row: UnifRecordRow { id, .. },
                        ..
                    },
                ) => Err(RowUnifError::ExtraRow(id)),
                (
                    RecordRowsF::Extend {
                        row: UnifRecordRow { id, .. },
                        ..
                    },
                    RecordRowsF::TailDyn,
                )
                | (
                    RecordRowsF::Extend {
                        row: UnifRecordRow { id, .. },
                        ..
                    },
                    RecordRowsF::Empty,
                ) => Err(RowUnifError::MissingRow(id)),
                (
                    RecordRowsF::Extend {
                        row: UnifRecordRow { id, types },
                        tail,
                    },
                    r2 @ RecordRowsF::Extend { .. },
                ) => {
                    let (ty2, t2_tail) =
                        rrows_add(state, &id, types.clone(), UnifRecordRows::Concrete(r2))?;
                    unify(state, ctxt, *types, *ty2)
                        .map_err(|err| RowUnifError::RowMismatch(id, Box::new(err)))?;
                    unify_rrows(state, ctxt, *tail, t2_tail)
                }
            }
        }
        (UnifRecordRows::UnifVar(p), urrows) | (urrows, UnifRecordRows::UnifVar(p)) => {
            constr_unify_rrows(state.constr, p, &urrows)?;
            state.table.assign_rrows(p, urrows);
            Ok(())
        }
        (UnifRecordRows::Constant(i1), UnifRecordRows::Constant(i2)) if i1 == i2 => Ok(()),
        (UnifRecordRows::Constant(i1), UnifRecordRows::Constant(i2)) => {
            Err(RowUnifError::ConstMismatch(i1, i2))
        }
        (urrows, UnifRecordRows::Constant(i)) | (UnifRecordRows::Constant(i), urrows) => {
            //TODO ROWS: should we refactor RowUnifError as well?
            Err(RowUnifError::WithConst(
                i,
                UnifType::Concrete(TypeF::Record(urrows)),
            ))
        }
    }
}

/// Try to unify two enum row types.
pub fn unify_erows(
    state: &mut State,
    uerows1: UnifEnumRows,
    uerows2: UnifEnumRows,
) -> Result<(), RowUnifError> {
    let uerows1 = uerows1.into_root(state.table);
    let uerows2 = uerows2.into_root(state.table);

    match (uerows1, uerows2) {
        (UnifEnumRows::Concrete(erows1), UnifEnumRows::Concrete(erows2)) => {
            match (erows1, erows2) {
                (EnumRowsF::TailVar(id), _) | (_, EnumRowsF::TailVar(id)) => {
                    Err(RowUnifError::UnboundTypeVariable(id))
                }
                (EnumRowsF::Empty, EnumRowsF::Empty) => Ok(()),
                (EnumRowsF::Empty, EnumRowsF::Extend { row: ident, .. }) => {
                    Err(RowUnifError::ExtraRow(ident))
                }
                (EnumRowsF::Extend { row: ident, .. }, EnumRowsF::Empty) => {
                    Err(RowUnifError::MissingRow(ident))
                }
                (EnumRowsF::Extend { row: id, tail }, erows2 @ EnumRowsF::Extend { .. }) => {
                    let t2_tail = erows_add(state, &id, UnifEnumRows::Concrete(erows2))?;
                    unify_erows(state, *tail, t2_tail)
                }
            }
        }
        (UnifEnumRows::UnifVar(p), uerows) | (uerows, UnifEnumRows::UnifVar(p)) => {
            constr_unify_erows(state.constr, p, &uerows)?;
            state.table.assign_erows(p, uerows);
            Ok(())
        }
        (UnifEnumRows::Constant(i1), UnifEnumRows::Constant(i2)) if i1 == i2 => Ok(()),
        (UnifEnumRows::Constant(i1), UnifEnumRows::Constant(i2)) => {
            Err(RowUnifError::ConstMismatch(i1, i2))
        }
        (uerows, UnifEnumRows::Constant(i)) | (UnifEnumRows::Constant(i), uerows) => {
            //TODO ROWS: should we refactor RowUnifError as well?
            Err(RowUnifError::WithConst(
                i,
                UnifType::Concrete(TypeF::Enum(uerows)),
            ))
        }
    }
}

/// Type of the parameter controlling instantiation of foralls.
///
/// See [`instantiate_foralls`].
#[derive(Copy, Clone, Debug, PartialEq)]
enum ForallInst {
    Constant,
    Ptr,
}

/// Instantiate the type variables which are quantified in head position with either unification
/// variables or type constants.
///
/// For example, if `inst` is `Constant`, `forall a. forall b. a -> (forall c. b -> c)` is
/// transformed to `cst1 -> (forall c. cst2 -> c)` where `cst1` and `cst2` are fresh type
/// constants.  This is used when typechecking `forall`s: all quantified type variables in head
/// position are replaced by rigid type constants, and the term is then typechecked normally. As
/// these constants cannot be unified with anything, this forces all the occurrences of a type
/// variable to be the same type.
///
/// # Parameters
///
/// - `state`: the unification state
/// - `ty`: the polymorphic type to instantiate
/// - `inst`: the type of instantiation, either by a type constant or by a unification variable
fn instantiate_foralls(state: &mut State, mut ty: UnifType, inst: ForallInst) -> UnifType {
    ty = ty.into_root(state.table);

    while let UnifType::Concrete(TypeF::Forall {
        var,
        var_kind,
        body,
    }) = ty
    {
        match var_kind {
            VarKind::Type => {
                let fresh_uid = state.table.fresh_type_var_id();
                let uvar = match inst {
                    ForallInst::Constant => UnifType::Constant(fresh_uid),
                    ForallInst::Ptr => UnifType::UnifVar(fresh_uid),
                };
                state.names.insert(fresh_uid, var);
                ty = body.subst_type(&var, &uvar);
            }
            VarKind::RecordRows => {
                let fresh_uid = state.table.fresh_rrows_var_id();
                let uvar = match inst {
                    ForallInst::Constant => UnifRecordRows::Constant(fresh_uid),
                    ForallInst::Ptr => UnifRecordRows::UnifVar(fresh_uid),
                };
                state.names.insert(fresh_uid, var);
                ty = body.subst_rrows(&var, &uvar);

                if inst == ForallInst::Ptr {
                    ty.constrain_fresh_rrows_var(state, fresh_uid)
                }
            }
            VarKind::EnumRows => {
                let fresh_uid = state.table.fresh_erows_var_id();
                let uvar = match inst {
                    ForallInst::Constant => UnifEnumRows::Constant(fresh_uid),
                    ForallInst::Ptr => UnifEnumRows::UnifVar(fresh_uid),
                };
                state.names.insert(fresh_uid, var);
                ty = body.subst_erows(&var, &uvar);

                if inst == ForallInst::Ptr {
                    ty.constrain_fresh_erows_var(state, fresh_uid)
                }
            }
        };
    }

    ty
}

/// The unification table.
///
/// Map each unification variable to either another type variable or a concrete type it has been
/// unified with. Each binding `(ty, var)` in this map should be thought of an edge in a
/// unification graph.
///
/// The unification table is really three separate tables, corresponding to the different kinds of
/// types: standard types, record rows, and enum rows.
#[derive(Default)]
pub struct UnifTable {
    types: Vec<Option<UnifType>>,
    rrows: Vec<Option<UnifRecordRows>>,
    erows: Vec<Option<UnifEnumRows>>,
}

impl UnifTable {
    pub fn new() -> Self {
        UnifTable::default()
    }

    /// Assign a type to a type unification variable.
    pub fn assign_type(&mut self, var: VarId, uty: UnifType) {
        debug_assert!(self.types[var].is_none());
        self.types[var] = Some(uty);
    }

    /// Assign record rows to a record rows unification variable.
    pub fn assign_rrows(&mut self, var: VarId, rrows: UnifRecordRows) {
        debug_assert!(self.rrows[var].is_none());
        self.rrows[var] = Some(rrows);
    }

    /// Assign enum rows to an enum rows unification variable.
    pub fn assign_erows(&mut self, var: VarId, erows: UnifEnumRows) {
        debug_assert!(self.erows[var].is_none());
        self.erows[var] = Some(erows);
    }

    /// Retrieve the current assignment of a type unification variable.
    pub fn get_type(&self, var: VarId) -> Option<&UnifType> {
        self.types[var].as_ref()
    }

    /// Retrieve the current assignment of a record rows unification variable.
    pub fn get_rrows(&self, var: VarId) -> Option<&UnifRecordRows> {
        self.rrows[var].as_ref()
    }

    /// Retrieve the current assignment of an enum rows unification variable.
    pub fn get_erows(&self, var: VarId) -> Option<&UnifEnumRows> {
        self.erows[var].as_ref()
    }

    /// Create a fresh type unification variable (or constant) identifier and allocate a
    /// corresponding slot in the table.
    fn fresh_type_var_id(&mut self) -> VarId {
        let next = self.types.len();
        self.types.push(None);
        next
    }

    /// Create a fresh record rows variable (or constant) identifier and allocate a corresponding
    /// slot in the table.
    fn fresh_rrows_var_id(&mut self) -> VarId {
        let next = self.rrows.len();
        self.rrows.push(None);
        next
    }

    /// Create a fresh enum rows variable (or constant) identifier and allocate a corresponding
    /// slot in the table.
    fn fresh_erows_var_id(&mut self) -> VarId {
        let next = self.erows.len();
        self.erows.push(None);
        next
    }

    /// Create a fresh type unification variable and allocate a corresponding slot in the table.
    pub fn fresh_type_uvar(&mut self) -> UnifType {
        UnifType::UnifVar(self.fresh_type_var_id())
    }

    /// Create a fresh record rows unification variable and allocate a corresponding slot in the
    /// table.
    pub fn fresh_rrows_uvar(&mut self) -> UnifRecordRows {
        UnifRecordRows::UnifVar(self.fresh_rrows_var_id())
    }

    /// Create a fresh enum rows unification variable and allocate a corresponding slot in the
    /// table.
    pub fn fresh_erows_uvar(&mut self) -> UnifEnumRows {
        UnifEnumRows::UnifVar(self.fresh_erows_var_id())
    }

    /// Create a fresh type constant and allocate a corresponding slot in the table.
    pub fn fresh_type_const(&mut self) -> UnifType {
        UnifType::Constant(self.fresh_type_var_id())
    }

    /// Create a fresh record rows constant and allocate a corresponding slot in the table.
    pub fn fresh_rrows_const(&mut self) -> UnifRecordRows {
        UnifRecordRows::Constant(self.fresh_rrows_var_id())
    }

    /// Create a fresh enum rows constant and allocate a corresponding slot in the table.
    pub fn fresh_erows_const(&mut self) -> UnifEnumRows {
        UnifEnumRows::Constant(self.fresh_erows_var_id())
    }

    /// Follow the links in the unification table to find the representative of the equivalence
    /// class of the type unification variable `x`.
    ///
    /// This corresponds to the find in union-find.
    // TODO This should be a union find like algorithm
    pub fn root_type(&self, var_id: VarId) -> UnifType {
        // All queried variable must have been introduced by `new_var` and thus a corresponding entry
        // must always exist in `state`. If not, the typechecking algorithm is not correct, and we
        // panic.
        match &self.types[var_id] {
            None => UnifType::UnifVar(var_id),
            Some(UnifType::UnifVar(y)) => self.root_type(*y),
            Some(ty) => ty.clone(),
        }
    }

    /// Follow the links in the unification table to find the representative of the equivalence
    /// class of the record rows unification variable `x`.
    ///
    /// This corresponds to the find in union-find.
    // TODO This should be a union find like algorithm
    pub fn root_rrows(&self, var_id: VarId) -> UnifRecordRows {
        // All queried variable must have been introduced by `new_var` and thus a corresponding entry
        // must always exist in `state`. If not, the typechecking algorithm is not correct, and we
        // panic.
        match &self.rrows[var_id] {
            None => UnifRecordRows::UnifVar(var_id),
            Some(UnifRecordRows::UnifVar(y)) => self.root_rrows(*y),
            Some(ty) => ty.clone(),
        }
    }

    /// Follow the links in the unification table to find the representative of the equivalence
    /// class of the enum rows unification variable `x`.
    ///
    /// This corresponds to the find in union-find.
    // TODO This should be a union find like algorithm
    pub fn root_erows(&self, var_id: VarId) -> UnifEnumRows {
        // All queried variable must have been introduced by `new_var` and thus a corresponding entry
        // must always exist in `state`. If not, the typechecking algorithm is not correct, and we
        // panic.
        match &self.erows[var_id] {
            None => UnifEnumRows::UnifVar(var_id),
            Some(UnifEnumRows::UnifVar(y)) => self.root_erows(*y),
            Some(ty) => ty.clone(),
        }
    }

    /// Return a `VarId` greater than all of the variables currently allocated (unification and
    /// rigid type variables, of all kinds, rows or types). The returned UID is guaranteed to be
    /// different from all the currently live variables. This is currently simply the max of the
    /// length of the various unification tables.
    ///
    /// Used inside [self::eq] to generate temporary rigid type variables that are guaranteed to
    /// not conflict with existing variables.
    pub fn max_uvars_count(&self) -> VarId {
        use std::cmp::max;

        max(self.types.len(), max(self.rrows.len(), self.erows.len()))
    }
}

/// Row constraints.
///
/// A row constraint applies to a unification variable appearing inside a row type (such as `r` in
/// `{ someId: SomeType | r }`). It is a set of identifiers that said row must NOT contain, to
/// forbid ill-formed types with multiple declaration of the same id, for example `{ a: Num, a:
/// String}`.
pub type RowConstr = HashMap<VarId, HashSet<Ident>>;

trait ConstrainFreshRRowsVar {
    /// Add row constraints on a freshly instantiated type variable.
    ///
    /// When instantiating a quantified type variable with a unification variable, row constraints may
    /// apply. For example, if we instantiate `forall a. {x: Num | a} -> Num` by replacing `a` with a
    /// unification variable `UnifVar(p)`, this unification variable requires a constraint to avoid being
    /// unified with a row type containing another declaration for the field `x`.
    ///
    /// This function traverses the type or rows `self`, looking for all occurrences of the
    /// unification variable `p` in tail position of record rows to add the corresponding
    /// constraints.
    ///
    /// # Preconditions
    ///
    /// Because `constraint_fresh_rrows_var` should be called on a fresh unification variable `p`,
    /// the following precondition is assumed and required:
    ///
    /// - `state.table.root_rrows(p) == p`
    fn constrain_fresh_rrows_var(&self, state: &mut State, var_id: VarId);
}

trait ConstrainFreshERowsVar {
    /// Add row constraints on a freshly instantiated type variable.
    ///
    /// This function traverses the type or rows `self`, looking for all occurrences of the
    /// unification variable `p` in tail position of enum rows, to add the corresponding
    /// constraints.
    ///
    /// # Preconditions
    ///
    /// Because `constraint_fresh_erows_var` should be called on a fresh unification variable `p`,
    /// the following precondition is assumed and required:
    ///
    /// - `state.table.root_erows(p) == p`
    fn constrain_fresh_erows_var(&self, state: &mut State, var_id: VarId);
}

impl ConstrainFreshRRowsVar for UnifType {
    fn constrain_fresh_rrows_var(&self, state: &mut State, var_id: VarId) {
        match self {
            UnifType::UnifVar(u) => match state.table.root_type(*u) {
                UnifType::UnifVar(_) => (),
                ty => ty.constrain_fresh_rrows_var(state, var_id),
            },
            UnifType::Concrete(ty) => match ty {
                    TypeF::Arrow(uty1, uty2) => {
                        uty1.constrain_fresh_rrows_var(state, var_id);
                        uty2.constrain_fresh_rrows_var(state, var_id);
                    }
                    TypeF::Forall {body, ..} => body.constrain_fresh_rrows_var(state, var_id),
                    TypeF::Dyn
                    | TypeF::Number
                    | TypeF::Bool
                    | TypeF::String
                    | TypeF::Symbol
                    | TypeF::Flat(_)
                    | TypeF::Var(_)
                    // There can be no record rows unification variable inside an enum type
                    | TypeF::Enum(_) => (),
                    | TypeF::Wildcard(_) => (),
                    TypeF::Record(rrows) => rrows.constrain_fresh_rrows_var(state, var_id),
                    TypeF::Array(uty)
                    | TypeF::Dict { type_fields: uty, .. } => uty.constrain_fresh_rrows_var(state, var_id),
                },
            UnifType::Constant(_) | UnifType::Contract(..) => (),
        }
    }
}

impl ConstrainFreshRRowsVar for UnifRecordRows {
    fn constrain_fresh_rrows_var(&self, state: &mut State, var_id: VarId) {
        fn constrain_var(
            state: &mut State,
            mut constr: HashSet<Ident>,
            uty: &UnifRecordRows,
            var_id: VarId,
        ) {
            match uty {
                UnifRecordRows::UnifVar(u) if var_id == *u && !constr.is_empty() => {
                    state.constr.insert(var_id, constr);
                }
                UnifRecordRows::UnifVar(u) => match state.table.root_rrows(*u) {
                    UnifRecordRows::UnifVar(_) => (),
                    rrows => constrain_var(state, constr, &rrows, var_id),
                },
                UnifRecordRows::Concrete(ty) => match ty {
                    RecordRowsF::Empty | RecordRowsF::TailDyn | RecordRowsF::TailVar(_) => (),
                    RecordRowsF::Extend { row, tail } => {
                        constr.insert(row.id);
                        row.types.constrain_fresh_rrows_var(state, var_id);
                        constrain_var(state, constr, tail, var_id);
                    }
                },
                UnifRecordRows::Constant(_) => (),
            }
        }

        constrain_var(state, HashSet::new(), self, var_id);
    }
}

impl ConstrainFreshERowsVar for UnifType {
    fn constrain_fresh_erows_var(&self, state: &mut State, var_id: VarId) {
        match self {
            UnifType::UnifVar(u) => match state.table.root_type(*u) {
                UnifType::UnifVar(_) => (),
                ty => ty.constrain_fresh_erows_var(state, var_id),
            },
            UnifType::Concrete(ty) => match ty {
                TypeF::Arrow(uty1, uty2) => {
                    uty1.constrain_fresh_erows_var(state, var_id);
                    uty2.constrain_fresh_erows_var(state, var_id);
                }
                TypeF::Forall { body, .. } => body.constrain_fresh_erows_var(state, var_id),
                TypeF::Dyn
                | TypeF::Number
                | TypeF::Bool
                | TypeF::String
                | TypeF::Symbol
                | TypeF::Flat(_)
                | TypeF::Var(_)
                | TypeF::Wildcard(_) => (),
                TypeF::Enum(erows) => erows.constrain_fresh_erows_var(state, var_id),
                TypeF::Record(rrows) => rrows.constrain_fresh_erows_var(state, var_id),
                TypeF::Array(uty)
                | TypeF::Dict {
                    type_fields: uty, ..
                } => uty.constrain_fresh_erows_var(state, var_id),
            },
            UnifType::Constant(_) | UnifType::Contract(..) => (),
        }
    }
}

impl ConstrainFreshERowsVar for UnifRecordRows {
    fn constrain_fresh_erows_var(&self, state: &mut State, var_id: VarId) {
        match self {
            UnifRecordRows::UnifVar(u) => match state.table.root_rrows(*u) {
                UnifRecordRows::UnifVar(_) => (),
                rrows => rrows.constrain_fresh_erows_var(state, var_id),
            },
            UnifRecordRows::Concrete(ty) => match ty {
                RecordRowsF::Empty | RecordRowsF::TailDyn | RecordRowsF::TailVar(_) => (),
                RecordRowsF::Extend { row, tail } => {
                    row.types.constrain_fresh_erows_var(state, var_id);
                    tail.constrain_fresh_erows_var(state, var_id);
                }
            },
            UnifRecordRows::Constant(_) => (),
        }
    }
}

impl ConstrainFreshERowsVar for UnifEnumRows {
    fn constrain_fresh_erows_var(&self, state: &mut State, var_id: VarId) {
        fn constrain_var(
            state: &mut State,
            mut constr: HashSet<Ident>,
            uty: &UnifEnumRows,
            var_id: VarId,
        ) {
            match uty {
                UnifEnumRows::UnifVar(u) if var_id == *u && !constr.is_empty() => {
                    state.constr.insert(var_id, constr);
                }
                UnifEnumRows::UnifVar(u) => match state.table.root_erows(*u) {
                    UnifEnumRows::UnifVar(_) => (),
                    erows => constrain_var(state, constr, &erows, var_id),
                },
                UnifEnumRows::Concrete(ty) => match ty {
                    EnumRowsF::Empty | EnumRowsF::TailVar(_) => (),
                    EnumRowsF::Extend { row, tail } => {
                        constr.insert(*row);
                        constrain_var(state, constr, tail, var_id);
                    }
                },
                UnifEnumRows::Constant(_) => (),
            }
        }

        constrain_var(state, HashSet::new(), self, var_id);
    }
}

/// Check that unifying a variable with a type doesn't violate record rows constraints, and update
/// the row constraints of the unified type accordingly if needed.
///
/// When a unification variable `UnifVar(p)` is unified with a type `uty` which is either a row type or
/// another unification variable which could be later unified with a row type itself, the following
/// operations are required:
///
/// 1. If `uty` is a concrete row, check that it doesn't contain an identifier which is forbidden
///    by a row constraint on `p`.
/// 2. If the type is either a unification variable or a row type ending with a unification
///    variable `u`, we must add the constraints of `p` to the constraints of `u`. Indeed, take the
///    following situation: `p` appears in a row type `{a: Num | p}`, hence has a constraint that
///    it must not contain a field `a`. Then `p` is unified with a fresh type variable `u`. If we
///    don't constrain `u`, `u` could be unified later with a row type `{a : Str}` which violates
///    the original constraint on `p`. Thus, when unifying `p` with `u` or a row ending with `u`,
///    `u` must inherit all the constraints of `p`.
pub fn constr_unify_rrows(
    constr: &mut RowConstr,
    var_id: VarId,
    rrows: &UnifRecordRows,
) -> Result<(), RowUnifError> {
    if let Some(p_constr) = constr.remove(&var_id) {
        match rrows {
            UnifRecordRows::Concrete(RecordRowsF::Extend { row, .. })
                if p_constr.contains(&row.id) =>
            {
                Err(RowUnifError::UnsatConstr(
                    row.id,
                    Some(UnifType::Concrete(TypeF::Record(rrows.clone()))),
                ))
            }
            UnifRecordRows::Concrete(RecordRowsF::Extend { tail, .. }) => {
                constr_unify_rrows(constr, var_id, tail)
            }
            UnifRecordRows::UnifVar(u) if *u != var_id => {
                if let Some(u_constr) = constr.get_mut(u) {
                    u_constr.extend(p_constr.into_iter());
                } else {
                    constr.insert(*u, p_constr);
                }

                Ok(())
            }
            _ => Ok(()),
        }
    } else {
        Ok(())
    }
}

/// Check that unifying a variable with a type doesn't violate enum rows constraints, and update
/// the row constraints of the unified type accordingly if needed.
///
/// Same as `constr_unify_rrows`, but for enum rows.
pub fn constr_unify_erows(
    constr: &mut RowConstr,
    var_id: VarId,
    erows: &UnifEnumRows,
) -> Result<(), RowUnifError> {
    if let Some(p_constr) = constr.remove(&var_id) {
        match erows {
            UnifEnumRows::Concrete(EnumRowsF::Extend { row, .. }) if p_constr.contains(row) => {
                Err(RowUnifError::UnsatConstr(
                    *row,
                    Some(UnifType::Concrete(TypeF::Enum(erows.clone()))),
                ))
            }
            UnifEnumRows::Concrete(EnumRowsF::Extend { tail, .. }) => {
                constr_unify_erows(constr, var_id, tail)
            }
            UnifEnumRows::UnifVar(u) if *u != var_id => {
                if let Some(u_constr) = constr.get_mut(u) {
                    u_constr.extend(p_constr.into_iter());
                } else {
                    constr.insert(*u, p_constr);
                }

                Ok(())
            }
            _ => Ok(()),
        }
    } else {
        Ok(())
    }
}

/// Get the type unification variable associated with a given wildcard ID.
fn get_wildcard_var(
    table: &mut UnifTable,
    wildcard_vars: &mut Vec<UnifType>,
    id: VarId,
) -> UnifType {
    // If `id` is not in `wildcard_vars`, populate it with fresh vars up to `id`
    if id >= wildcard_vars.len() {
        wildcard_vars.extend((wildcard_vars.len()..=id).map(|_| table.fresh_type_uvar()));
    }
    wildcard_vars[id].clone()
}

/// Convert a mapping from wildcard ID to type var, into a mapping from wildcard ID to concrete type.
fn wildcard_vars_to_type(wildcard_vars: Vec<UnifType>, table: &UnifTable) -> Wildcards {
    wildcard_vars
        .into_iter()
        .map(|var| var.into_type(table))
        .collect()
}
