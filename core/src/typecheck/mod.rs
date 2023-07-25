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
//!   is implemented by the `walk` function.
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
    typ::{
        EnumRow, EnumRows, EnumRowsF, EnumRowsIterator, RecordRowF, RecordRows, RecordRowsF,
        RecordRowsIterator, Type, TypeF, VarKind, VarKindDiscriminant,
    },
    {mk_uty_arrow, mk_uty_enum, mk_uty_enum_row, mk_uty_record, mk_uty_row},
};

use std::{
    cmp::max,
    collections::{HashMap, HashSet},
    convert::TryInto,
    num::NonZeroU16,
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
pub mod unif;

use eq::{SimpleTermEnvironment, TermEnvironment};
use error::*;
use operation::{get_bop_type, get_nop_type, get_uop_type};
use unif::*;

/// The max depth parameter used to limit the work performed when inferring the type of the stdlib.
const INFER_RECORD_MAX_DEPTH: u8 = 4;

/// The typing environment.
pub type Environment = GenericEnvironment<Ident, UnifType>;

/// Mapping from wildcard ID to inferred type
pub type Wildcards = Vec<Type>;

/// A table mapping variable IDs with their kind to names.
pub type NameTable = HashMap<(VarId, VarKindDiscriminant), Ident>;

/// A unifiable record row.
pub type GenericUnifRecordRow<E> = RecordRowF<Box<GenericUnifType<E>>>;
pub type GenericUnifRecordRowsUnrolling<E> =
    RecordRowsF<Box<GenericUnifType<E>>, Box<GenericUnifRecordRows<E>>>;

/// Unifiable record rows. Same shape as [`crate::typ::RecordRows`], but where each type is
/// unifiable, and each tail may be a unification variable (or a constant).
#[derive(Clone, PartialEq, Debug)]
pub enum GenericUnifRecordRows<E: TermEnvironment + Clone> {
    Concrete {
        rrows: GenericUnifRecordRowsUnrolling<E>,
        /// Additional metadata related to unification variable levels update. See [VarLevelsData].
        var_levels_data: VarLevelsData,
    },
    Constant(VarId),
    /// A unification variable.
    UnifVar {
        /// The unique identifier of this variable in the unification table.
        id: VarId,
        /// The initial variable level at which the variable was created. See
        /// [GenericUnifType::UnifVar].
        init_level: VarLevel,
    },
}

pub type UnifEnumRowsUnrolling = EnumRowsF<Box<UnifEnumRows>>;

/// Unifiable enum rows. Same shape as [`crate::typ::EnumRows`] but where each tail may be a
/// unification variable (or a constant).
///
/// Enum rows don't store any type (they are just a sequence of identifiers), so there is no
/// `GenericUnifEnumRows` taking an additional `E` parameter.
#[derive(Clone, PartialEq, Debug)]
pub enum UnifEnumRows {
    Concrete {
        erows: UnifEnumRowsUnrolling,
        /// Additional metadata related to unification variable levels update. See [VarLevelsData].
        var_levels_data: VarLevelsData,
    },
    Constant(VarId),
    UnifVar {
        /// The unique identifier of this variable in the unification table.
        id: VarId,
        /// The initial variable level at which the variable was created. See
        /// [GenericUnifType::UnifVar].
        init_level: VarLevel,
    },
}

/// Metadata attached to unification types, which are used to delay and optimize potentially costly
/// type traversals when updating the levels of the free unification variables of a type. Based on
/// Didier Remy's algorithm for the OCaml typechecker, see [Efficient and insightful
/// generalization](http://web.archive.org/web/20230525023637/https://okmij.org/ftp/ML/generalization.html).
///
/// When unifying a variable with a composite type, we have to update the levels of all the free
/// unification variables contained in that type, which naively incurs a full traversal of the
/// type. The idea behind Didier Remy's algorithm is to delay such traversals, and use the values
/// of [VarLevelsData] to group traversals and avoid unneeded ones. This make variable unification
/// run in constant time again, as long as we don't unify with a rigid type variable.
///
/// Variable levels data might correspond to different variable kinds (type, record rows and enum
/// rows) depending on where they appear (in a [UnifType], [UnifRecordRows] or [UnifEnumRows])
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct VarLevelsData {
    /// Upper bound on the variable levels of free unification variables contained in this type.
    upper_bound: VarLevel,
    /// Pending variable level update, which must satisfy `pending_level <= old_level`.
    pending: Option<VarLevel>,
}

impl Default for VarLevelsData {
    fn default() -> Self {
        VarLevelsData::new_from_bound(VarLevel::MAX_LEVEL)
    }
}

impl VarLevelsData {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create new variable levels data with the given upper bound and no pending level update.
    pub fn new_from_bound(upper_bound: VarLevel) -> Self {
        VarLevelsData {
            upper_bound,
            pending: None,
        }
    }

    /// Create new variable levels data with an upper bound that there is no unification variable
    /// in the attached type and no pending level update.
    pub fn new_no_uvars() -> Self {
        Self::new_from_bound(VarLevel::NO_VAR)
    }
}

// Unification types and variants that store an upper bound on the level of the unification
// variables they contain, or for which an upper bound can be computed quickly (in constant time).
trait VarLevelUpperBound {
    // Return an upper bound on the level of the unification variables contained in `self`.
    // Depending on the implementer, the level might refer to different kind of unification
    // variables (type, record rows or enum rows).
    fn var_level_upper_bound(&self) -> VarLevel;
}

impl<E: TermEnvironment> VarLevelUpperBound for GenericUnifType<E> {
    fn var_level_upper_bound(&self) -> VarLevel {
        match self {
            GenericUnifType::Concrete {
                var_levels_data, ..
            } => var_levels_data.upper_bound,
            GenericUnifType::UnifVar { init_level, .. } => *init_level,
            GenericUnifType::Contract(..) | GenericUnifType::Constant(_) => VarLevel::NO_VAR,
        }
    }
}

impl<E: TermEnvironment> VarLevelUpperBound for GenericUnifTypeUnrolling<E> {
    fn var_level_upper_bound(&self) -> VarLevel {
        match self {
            TypeF::Dyn | TypeF::Bool | TypeF::Number | TypeF::String | TypeF::Symbol => {
                VarLevel::NO_VAR
            }
            TypeF::Arrow(domain, codomain) => max(domain.var_level_upper_bound(), codomain.var_level_upper_bound()),
            TypeF::Forall { body, .. } => body.var_level_upper_bound(),
            TypeF::Enum(erows) => erows.var_level_upper_bound(),
            TypeF::Record(rrows) => rrows.var_level_upper_bound(),
            TypeF::Dict { type_fields, .. } => type_fields.var_level_upper_bound(),
            TypeF::Array(ty_elts) => ty_elts.var_level_upper_bound(),
            TypeF::Wildcard(_)
            | TypeF::Var(_)
            // This should be unreachable, but let's not panic in release mode nonetheless
            | TypeF::Flat(_) => VarLevel::NO_VAR,
        }
    }
}

impl VarLevelUpperBound for UnifEnumRows {
    fn var_level_upper_bound(&self) -> VarLevel {
        match self {
            UnifEnumRows::Concrete {
                var_levels_data, ..
            } => var_levels_data.upper_bound,
            UnifEnumRows::UnifVar { init_level, .. } => *init_level,
            UnifEnumRows::Constant(_) => VarLevel::NO_VAR,
        }
    }
}

impl VarLevelUpperBound for UnifEnumRowsUnrolling {
    fn var_level_upper_bound(&self) -> VarLevel {
        match self {
            // A var that hasn't be instantiated yet isn't a unification variable
            EnumRowsF::Empty | EnumRowsF::TailVar(_) => VarLevel::NO_VAR,
            EnumRowsF::Extend { row: _, tail } => tail.var_level_upper_bound(),
        }
    }
}

impl<E: TermEnvironment> VarLevelUpperBound for GenericUnifRecordRows<E> {
    fn var_level_upper_bound(&self) -> VarLevel {
        match self {
            GenericUnifRecordRows::Concrete {
                var_levels_data, ..
            } => var_levels_data.upper_bound,
            GenericUnifRecordRows::UnifVar { init_level, .. } => *init_level,
            GenericUnifRecordRows::Constant(_) => VarLevel::NO_VAR,
        }
    }
}

impl<E: TermEnvironment> VarLevelUpperBound for GenericUnifRecordRowsUnrolling<E> {
    fn var_level_upper_bound(&self) -> VarLevel {
        match self {
            // A var that hasn't be instantiated yet isn't a unification variable
            RecordRowsF::Empty | RecordRowsF::TailVar(_) | RecordRowsF::TailDyn => VarLevel::NO_VAR,
            RecordRowsF::Extend {
                row: RecordRowF { id: _, typ },
                tail,
            } => max(tail.var_level_upper_bound(), typ.var_level_upper_bound()),
        }
    }
}

/// The types on which the unification algorithm operates, which may be either a concrete type, a
/// type constant or a unification variable.
///
/// Contracts store an additional term environment for contract equality checking, which is
/// represented by `E`. The typechecker always uses the same type for `E`. However, the evaluation
/// phase may also resort to checking contract equality, using a different environment
/// representation, hence the parametrization.
///
/// # Invariants
///
/// **Important**: the following invariant must always be satisfied: for any free unification
/// variable[^free-unif-var] part of a concrete unification type, the level of this variable must
/// be smaller or equal to `var_levels_data.upper_bound`. Otherwise, the typechecking algorithm
/// might not be correct. Be careful when creating new concrete [GenericUnifType] or [UnifType]
/// values manually. All `from` and `try_from` implementations, the `concrete` method as well as
/// builders from the [mk_uniftype] module all correctly compute the upper bound (given that the
/// upper bounds of the subcomponents are correct).
///
/// The default value for `var_levels_data`, although it can incur more work, is at least always
/// correct (by setting `upper_bound = VarLevel::MAX`).
///
/// [^free-unif-var]: A free unification variable is a unification variable that isn't assigned to
/// any type yet, i.e. verifying  `uty.root_type(..) == uty` (adapt with the corresponding
/// `root_xxx` method for rows).
#[derive(Clone, PartialEq, Debug)]
pub enum GenericUnifType<E: TermEnvironment> {
    /// A concrete type (like `Number` or `String -> String`).
    Concrete {
        typ: GenericUnifTypeUnrolling<E>,
        /// Additional metadata related to unification variable levels update. See [VarLevelsData].
        var_levels_data: VarLevelsData,
    },
    /// A contract, seen as an opaque type. In order to compute type equality between contracts or
    /// between a contract and a type, we need to carry an additional environment. This is why we
    /// don't reuse the variant from [`crate::typ::TypeF`].
    Contract(RichTerm, E),
    /// A rigid type constant which cannot be unified with anything but itself.
    Constant(VarId),
    /// A unification variable.
    UnifVar {
        /// The unique identifier of this variable in the unification table.
        id: VarId,
        /// An upper bound of this variable level, which usually correspond to the initial level at
        /// which the variable was allocated, although this value might be bumped for some
        /// variables by level updates.
        ///
        /// In a model where unification variables directly store a mutable level attribute, we
        /// wouldn't need to duplicate this level information both here at the variable level and
        /// in the unification table. `init_level` is used to compute upper bounds without having
        /// to thread the unification table around (in the `from`/`try_from` implementation for
        /// unification types, typically).
        ///
        /// Note that the actual level of this variable is stored in the unification table, which
        /// is the source of truth. The actual level must satisfy `current_level <= init_level`
        /// (the level of a variable can only decrease with time).
        init_level: VarLevel,
    },
}

type GenericUnifTypeUnrolling<E> =
    TypeF<Box<GenericUnifType<E>>, GenericUnifRecordRows<E>, UnifEnumRows>;

impl<E: TermEnvironment> GenericUnifType<E> {
    /// Create a concrete generic unification type. Compute the variable levels data from the
    /// subcomponents.
    pub fn concrete(typ: GenericUnifTypeUnrolling<E>) -> Self {
        let upper_bound = typ.var_level_upper_bound();

        GenericUnifType::Concrete {
            typ,
            var_levels_data: VarLevelsData::new_from_bound(upper_bound),
        }
    }
}

impl<E: TermEnvironment> GenericUnifRecordRows<E> {
    /// Create concrete generic record rows. Compute the variable levels data from the
    /// subcomponents.
    pub fn concrete(typ: GenericUnifRecordRowsUnrolling<E>) -> Self {
        let upper_bound = typ.var_level_upper_bound();

        GenericUnifRecordRows::Concrete {
            rrows: typ,
            var_levels_data: VarLevelsData::new_from_bound(upper_bound),
        }
    }
}

impl<E: TermEnvironment + Clone> std::convert::TryInto<RecordRows> for GenericUnifRecordRows<E> {
    type Error = ();

    fn try_into(self) -> Result<RecordRows, ()> {
        match self {
            GenericUnifRecordRows::Concrete { rrows, .. } => {
                let converted: RecordRowsF<Box<Type>, Box<RecordRows>> = rrows.try_map(
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
            UnifEnumRows::Concrete { erows, .. } => {
                let converted: EnumRowsF<Box<EnumRows>> =
                    erows.try_map(|erows| Ok(Box::new(UnifEnumRows::try_into(*erows)?)))?;
                Ok(EnumRows(converted))
            }
            _ => Err(()),
        }
    }
}

impl<E: TermEnvironment + Clone> std::convert::TryInto<Type> for GenericUnifType<E> {
    type Error = ();

    fn try_into(self) -> Result<Type, ()> {
        match self {
            GenericUnifType::Concrete { typ, .. } => {
                let converted: TypeF<Box<Type>, RecordRows, EnumRows> = typ.try_map(
                    |uty_boxed| {
                        let ty: Type = (*uty_boxed).try_into()?;
                        Ok(Box::new(ty))
                    },
                    GenericUnifRecordRows::try_into,
                    UnifEnumRows::try_into,
                )?;
                Ok(Type::from(converted))
            }
            GenericUnifType::Contract(t, _) => {
                let pos = t.pos;
                Ok(Type {
                    typ: TypeF::Flat(t),
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
        UnifEnumRows::concrete(erows.0.map(|erows| Box::new(UnifEnumRows::from(*erows))))
    }
}

impl UnifEnumRows {
    /// Return an iterator producing immutable references to individual rows.
    pub fn iter(&self) -> EnumRowsIterator<UnifEnumRows> {
        EnumRowsIterator { erows: Some(self) }
    }

    /// Create concrete generic unification enum rows. Compute the variable levels data from the
    /// subcomponents.
    pub fn concrete(erows: UnifEnumRowsUnrolling) -> Self {
        let upper_bound = erows.var_level_upper_bound();

        UnifEnumRows::Concrete {
            erows,
            var_levels_data: VarLevelsData::new_from_bound(upper_bound),
        }
    }
}

impl<E: TermEnvironment + Clone> GenericUnifRecordRows<E> {
    /// Create `GenericUnifRecordRows` from `RecordRows`. Contracts are represented as the separate
    /// variant [`GenericUnifType::Contract`] which also stores a term environment, required for
    /// checking type equality involving contracts.
    pub fn from_record_rows(rrows: RecordRows, env: &E) -> Self {
        let f_rrow = |ty: Box<Type>| Box::new(GenericUnifType::from_type(*ty, env));
        let f_rrows =
            |rrows: Box<RecordRows>| Box::new(GenericUnifRecordRows::from_record_rows(*rrows, env));

        GenericUnifRecordRows::concrete(rrows.0.map(f_rrow, f_rrows))
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

// A type which contains variables that can be substituted with values of type `T`.
trait Subst<T: Clone>: Sized {
    // Substitute all variables of identifier `id` with `to`.
    fn subst(self, id: &Ident, to: &T) -> Self {
        self.subst_levels(id, to).0
    }

    // Must be implemented by implementers of this trait.
    // In addition to performing substitution, this method threads variable levels upper bounds to
    // compute new upper bounds efficiently.
    fn subst_levels(self, id: &Ident, to: &T) -> (Self, VarLevel);
}

impl<E: TermEnvironment> Subst<GenericUnifType<E>> for GenericUnifType<E> {
    fn subst_levels(self, id: &Ident, to: &GenericUnifType<E>) -> (Self, VarLevel) {
        match self {
            GenericUnifType::Concrete {
                typ: TypeF::Var(var_id),
                var_levels_data,
            } if var_id == *id => {
                debug_assert!(var_levels_data.upper_bound == VarLevel::NO_VAR);
                (to.clone(), to.var_level_upper_bound())
            }
            GenericUnifType::Concrete {
                typ,
                var_levels_data,
            } => {
                let mut upper_bound = VarLevel::NO_VAR;

                let new_ty = GenericUnifType::Concrete {
                    typ: typ.map_state(
                        |ty, upper_bound| {
                            let (new_type, new_ub) = ty.subst_levels(id, to);
                            *upper_bound = max(*upper_bound, new_ub);
                            Box::new(new_type)
                        },
                        |rrows, upper_bound| {
                            let (new_rrows, new_ub) = rrows.subst_levels(id, to);
                            *upper_bound = max(*upper_bound, new_ub);
                            new_rrows
                        },
                        |erows, _| erows,
                        &mut upper_bound,
                    ),
                    var_levels_data: VarLevelsData {
                        upper_bound,
                        ..var_levels_data
                    },
                };

                (new_ty, upper_bound)
            }
            _ => {
                let upper_bound = self.var_level_upper_bound();
                (self, upper_bound)
            }
        }
    }
}

impl<E: TermEnvironment> Subst<GenericUnifType<E>> for GenericUnifRecordRows<E> {
    fn subst_levels(self, id: &Ident, to: &GenericUnifType<E>) -> (Self, VarLevel) {
        match self {
            GenericUnifRecordRows::Concrete {
                rrows,
                var_levels_data,
            } => {
                let mut upper_bound = VarLevel::NO_VAR;

                let new_rrows = rrows.map_state(
                    |ty, upper_bound| {
                        let (new_ty, new_ub) = ty.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        Box::new(new_ty)
                    },
                    |rrows, upper_bound| {
                        let (new_rrows, new_ub) = rrows.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        Box::new(new_rrows)
                    },
                    &mut upper_bound,
                );

                let new_urrows = GenericUnifRecordRows::Concrete {
                    rrows: new_rrows,
                    var_levels_data: VarLevelsData {
                        upper_bound,
                        ..var_levels_data
                    },
                };

                (new_urrows, upper_bound)
            }
            _ => {
                let upper_bound = self.var_level_upper_bound();
                (self, upper_bound)
            }
        }
    }
}

impl<E: TermEnvironment> Subst<GenericUnifRecordRows<E>> for GenericUnifType<E> {
    fn subst_levels(self, id: &Ident, to: &GenericUnifRecordRows<E>) -> (Self, VarLevel) {
        match self {
            GenericUnifType::Concrete {
                typ,
                var_levels_data,
            } => {
                let mut upper_bound = VarLevel::NO_VAR;

                let new_ty = typ.map_state(
                    |ty, upper_bound| {
                        let (new_ty, new_ub) = ty.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        Box::new(new_ty)
                    },
                    |rrows, upper_bound| {
                        let (new_rrows, new_ub) = rrows.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        new_rrows
                    },
                    |erows, _| erows,
                    &mut upper_bound,
                );

                let new_uty = GenericUnifType::Concrete {
                    typ: new_ty,
                    var_levels_data,
                };

                (new_uty, upper_bound)
            }
            _ => {
                let upper_bound = self.var_level_upper_bound();
                (self, upper_bound)
            }
        }
    }
}

impl<E: TermEnvironment> Subst<GenericUnifRecordRows<E>> for GenericUnifRecordRows<E> {
    fn subst_levels(self, id: &Ident, to: &GenericUnifRecordRows<E>) -> (Self, VarLevel) {
        match self {
            GenericUnifRecordRows::Concrete {
                rrows: RecordRowsF::TailVar(var_id),
                var_levels_data,
            } if var_id == *id => {
                debug_assert!(var_levels_data.upper_bound == VarLevel::NO_VAR);
                (to.clone(), to.var_level_upper_bound())
            }
            GenericUnifRecordRows::Concrete {
                rrows,
                var_levels_data,
            } => {
                let mut upper_bound = VarLevel::NO_VAR;

                let new_rrows = rrows.map_state(
                    |ty, upper_bound| {
                        let (new_ty, new_ub) = ty.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        Box::new(new_ty)
                    },
                    |rrows, upper_bound| {
                        let (new_rrows, new_ub) = rrows.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        Box::new(new_rrows)
                    },
                    &mut upper_bound,
                );

                let new_urrows = GenericUnifRecordRows::Concrete {
                    rrows: new_rrows,
                    var_levels_data: VarLevelsData {
                        upper_bound,
                        ..var_levels_data
                    },
                };

                (new_urrows, upper_bound)
            }
            _ => {
                let upper_bound = self.var_level_upper_bound();
                (self, upper_bound)
            }
        }
    }
}

impl<E: TermEnvironment> Subst<UnifEnumRows> for GenericUnifType<E> {
    fn subst_levels(self, id: &Ident, to: &UnifEnumRows) -> (Self, VarLevel) {
        match self {
            GenericUnifType::Concrete {
                typ,
                var_levels_data,
            } => {
                let mut upper_bound = VarLevel::NO_VAR;

                let new_ty = typ.map_state(
                    |ty, upper_bound| {
                        let (new_ty, new_ub) = ty.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        Box::new(new_ty)
                    },
                    |rrows, upper_bound| {
                        let (new_rrows, new_ub) = rrows.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        new_rrows
                    },
                    |erows, upper_bound| {
                        let (new_erows, new_ub) = erows.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        new_erows
                    },
                    &mut upper_bound,
                );

                let new_uty = GenericUnifType::Concrete {
                    typ: new_ty,
                    var_levels_data: VarLevelsData {
                        upper_bound,
                        ..var_levels_data
                    },
                };

                (new_uty, upper_bound)
            }
            _ => {
                let upper_bound = self.var_level_upper_bound();
                (self, upper_bound)
            }
        }
    }
}

impl<E: TermEnvironment> Subst<UnifEnumRows> for GenericUnifRecordRows<E> {
    fn subst_levels(self, id: &Ident, to: &UnifEnumRows) -> (Self, VarLevel) {
        match self {
            GenericUnifRecordRows::Concrete {
                rrows,
                var_levels_data,
            } => {
                let mut upper_bound = VarLevel::NO_VAR;

                let new_rrows = rrows.map_state(
                    |ty, upper_bound| {
                        let (new_ty, new_ub) = ty.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        Box::new(new_ty)
                    },
                    |rrows, upper_bound| {
                        let (new_rrows, new_ub) = rrows.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        Box::new(new_rrows)
                    },
                    &mut upper_bound,
                );

                let new_urrows = GenericUnifRecordRows::Concrete {
                    rrows: new_rrows,
                    var_levels_data: VarLevelsData {
                        upper_bound,
                        ..var_levels_data
                    },
                };

                (new_urrows, upper_bound)
            }

            _ => {
                let upper_bound = self.var_level_upper_bound();
                (self, upper_bound)
            }
        }
    }
}

impl Subst<UnifEnumRows> for UnifEnumRows {
    fn subst_levels(self, id: &Ident, to: &UnifEnumRows) -> (Self, VarLevel) {
        match self {
            UnifEnumRows::Concrete {
                erows: EnumRowsF::TailVar(var_id),
                var_levels_data,
            } if var_id == *id => {
                debug_assert!(var_levels_data.upper_bound == VarLevel::NO_VAR);
                (to.clone(), to.var_level_upper_bound())
            }
            UnifEnumRows::Concrete {
                erows,
                var_levels_data,
            } => {
                let mut upper_bound = VarLevel::NO_VAR;

                let new_erows = erows.map_state(
                    |erows, upper_bound| {
                        let (new_erows, new_ub) = erows.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        Box::new(new_erows)
                    },
                    &mut upper_bound,
                );

                let new_uerows = UnifEnumRows::Concrete {
                    erows: new_erows,
                    var_levels_data: VarLevelsData {
                        upper_bound,
                        ..var_levels_data
                    },
                };

                (new_uerows, upper_bound)
            }

            _ => {
                let upper_bound = self.var_level_upper_bound();
                (self, upper_bound)
            }
        }
    }
}

impl<E: TermEnvironment + Clone> GenericUnifType<E> {
    /// Create a [`GenericUnifType`] from a [`Type`]. Contracts are represented as the separate variant
    /// [`GenericUnifType::Contract`] which also stores a term environment, required for checking type
    /// equality involving contracts.
    pub fn from_type(ty: Type, env: &E) -> Self {
        match ty.typ {
            TypeF::Flat(t) => GenericUnifType::Contract(t, env.clone()),
            ty => GenericUnifType::concrete(ty.map(
                |ty_| Box::new(GenericUnifType::from_type(*ty_, env)),
                |rrows| GenericUnifRecordRows::from_record_rows(rrows, env),
                UnifEnumRows::from,
            )),
        }
    }
}

type UnifTypeUnrolling = GenericUnifTypeUnrolling<SimpleTermEnvironment>;
type UnifRecordRowsUnrolling = GenericUnifRecordRowsUnrolling<SimpleTermEnvironment>;

pub type UnifRecordRow = GenericUnifRecordRow<SimpleTermEnvironment>;
pub type UnifRecordRows = GenericUnifRecordRows<SimpleTermEnvironment>;
pub type UnifType = GenericUnifType<SimpleTermEnvironment>;

impl UnifRecordRows {
    /// Extract the concrete [`RecordRows`] corresponding to a [`UnifRecordRows`]. Free unification variables as well
    /// as type constants are replaced with the empty row.
    fn into_rrows(self, table: &UnifTable) -> RecordRows {
        match self {
            UnifRecordRows::UnifVar { id, init_level } => match table.root_rrows(id, init_level) {
                t @ UnifRecordRows::Concrete { .. } => t.into_rrows(table),
                _ => RecordRows(RecordRowsF::Empty),
            },
            UnifRecordRows::Constant(_) => RecordRows(RecordRowsF::Empty),
            UnifRecordRows::Concrete { rrows, .. } => {
                let mapped = rrows.map(
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
            UnifRecordRows::UnifVar { id, init_level } => table.root_rrows(id, init_level),
            urrows => urrows,
        }
    }
}

impl UnifEnumRows {
    /// Extract the concrete [`EnumRows`] corresponding to a [`UnifEnumRows`]. Free unification variables as well
    /// as type constants are replaced with the empty row.
    fn into_erows(self, table: &UnifTable) -> EnumRows {
        match self {
            UnifEnumRows::UnifVar { id, init_level } => match table.root_erows(id, init_level) {
                t @ UnifEnumRows::Concrete { .. } => t.into_erows(table),
                _ => EnumRows(EnumRowsF::Empty),
            },
            UnifEnumRows::Constant(_) => EnumRows(EnumRowsF::Empty),
            UnifEnumRows::Concrete { erows, .. } => {
                let mapped = erows.map(|erows| Box::new(erows.into_erows(table)));
                EnumRows(mapped)
            }
        }
    }

    /// Return the unification root associated with these enum rows. If the rows are a unification
    /// variable, return the result of `table.root_erows`. Return `self` otherwise.
    fn into_root(self, table: &UnifTable) -> Self {
        match self {
            UnifEnumRows::UnifVar { id, init_level } => table.root_erows(id, init_level),
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
                GenericUnifType::concrete(TypeF::Dyn)
            }
            ApparentType::Annotated(ty)
            | ApparentType::Inferred(ty)
            | ApparentType::Approximated(ty) => GenericUnifType::from_type(ty, env),
            ApparentType::FromEnv(uty) => uty,
        }
    }

    pub fn from_constant_of_kind(c: usize, k: VarKindDiscriminant) -> Self {
        match k {
            VarKindDiscriminant::Type => UnifType::Constant(c),
            VarKindDiscriminant::EnumRows => UnifType::Concrete {
                typ: TypeF::Enum(UnifEnumRows::Constant(c)),
                var_levels_data: VarLevelsData::new_no_uvars(),
            },
            VarKindDiscriminant::RecordRows => UnifType::Concrete {
                typ: TypeF::Record(UnifRecordRows::Constant(c)),
                var_levels_data: VarLevelsData::new_no_uvars(),
            },
        }
    }

    /// Extract the concrete type corresponding to a unifiable type. Free unification variables as well
    /// as type constants are replaced with the type `Dyn`.
    fn into_type(self, table: &UnifTable) -> Type {
        match self {
            UnifType::UnifVar { id, init_level } => match table.root_type(id, init_level) {
                t @ UnifType::Concrete { .. } => t.into_type(table),
                _ => Type::from(TypeF::Dyn),
            },
            UnifType::Constant(_) => Type::from(TypeF::Dyn),
            UnifType::Concrete { typ, .. } => {
                let mapped = typ.map(
                    |btyp| Box::new(btyp.into_type(table)),
                    |urrows| urrows.into_rrows(table),
                    |uerows| uerows.into_erows(table),
                );
                Type::from(mapped)
            }
            UnifType::Contract(t, _) => Type::from(TypeF::Flat(t)),
        }
    }

    /// Return the unification root associated with this type. If the type is a unification variable,
    /// return the result of `table.root_type`. Return `self` otherwise.
    fn into_root(self, table: &UnifTable) -> Self {
        match self {
            UnifType::UnifVar { id, init_level } => table.root_type(id, init_level),
            uty => uty,
        }
    }
}

// This implementation assumes that `TypeF::Flat` is not possible. If a [`UnifType`] has been
// correctly created from a type using `from_type`, this must be the case.
impl From<UnifTypeUnrolling> for UnifType {
    fn from(typ: UnifTypeUnrolling) -> Self {
        debug_assert!(!matches!(typ, TypeF::Flat(_)));

        let var_level_max = typ.var_level_upper_bound();

        UnifType::Concrete {
            typ,
            var_levels_data: VarLevelsData::new_from_bound(var_level_max),
        }
    }
}

impl From<RecordRowsF<Box<UnifType>, Box<UnifRecordRows>>> for UnifRecordRows {
    fn from(rrows: RecordRowsF<Box<UnifType>, Box<UnifRecordRows>>) -> Self {
        let var_level_max = rrows.var_level_upper_bound();

        UnifRecordRows::Concrete {
            rrows,
            var_levels_data: VarLevelsData::new_from_bound(var_level_max),
        }
    }
}

impl From<EnumRowsF<Box<UnifEnumRows>>> for UnifEnumRows {
    fn from(erows: EnumRowsF<Box<UnifEnumRows>>) -> Self {
        UnifEnumRows::concrete(erows)
    }
}

/// Iterator items produced by [RecordRowsIterator] on [GenericUnifRecordRows].
pub enum GenericUnifRecordRowsIteratorItem<'a, E: TermEnvironment> {
    TailDyn,
    TailVar(&'a Ident),
    TailUnifVar { id: VarId, init_level: VarLevel },
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
            GenericUnifRecordRows::Concrete { rrows, .. } => match rrows {
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
                        typ: row.typ.as_ref(),
                    }))
                }
            },
            GenericUnifRecordRows::UnifVar { id, init_level } => {
                self.rrows = None;
                Some(GenericUnifRecordRowsIteratorItem::TailUnifVar {
                    id: *id,
                    init_level: *init_level,
                })
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
    TailUnifVar { id: VarId, init_level: VarLevel },
    TailConstant(VarId),
    Row(&'a EnumRow),
}

impl<'a> Iterator for EnumRowsIterator<'a, UnifEnumRows> {
    type Item = UnifEnumRowsIteratorItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.erows.and_then(|next| match next {
            UnifEnumRows::Concrete { erows, .. } => match erows {
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
            UnifEnumRows::UnifVar { id, init_level } => {
                self.erows = None;
                Some(UnifEnumRowsIteratorItem::TailUnifVar {
                    id: *id,
                    init_level: *init_level,
                })
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
#[derive(Debug, PartialEq, Clone)]
pub struct Context {
    /// The typing environment, counterpart of the eval environment for typechecking
    pub type_env: Environment,
    /// The term environment, used to decide type equality over contracts.
    pub term_env: SimpleTermEnvironment,
    /// The current variable level, incremented each time we instantiate a polymorphic type and
    /// thus introduce a new block of variables (either unification variables or rigid type
    /// variables).
    pub var_level: VarLevel,
}

impl Context {
    pub fn new() -> Self {
        Context {
            type_env: Environment::new(),
            term_env: SimpleTermEnvironment::new(),
            var_level: VarLevel::MIN_LEVEL,
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

    Ok(Context {
        type_env,
        term_env,
        var_level: VarLevel::MIN_LEVEL,
    })
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
    /// A mapping from unification variables or constants together with their
    /// kind to the name of the corresponding type variable which introduced it,
    /// if any.
    ///
    /// Used for error reporting.
    names: &'a mut NameTable,
    /// A mapping from wildcard ID to unification variable.
    wildcard_vars: &'a mut Vec<UnifType>,
}

/// Immutable and owned data, required by the LSP to carry out specific analysis.
/// It is basically an owned-subset of the typechecking state.
pub struct Extra {
    pub table: UnifTable,
    pub names: NameTable,
    pub wildcards: Vec<Type>,
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
        Term::Sealed(_, t, _) => walk(state, ctxt, lin, linearizer, t),
        Term::Type(ty) => walk_type(state, ctxt, lin, linearizer, ty),
   }
}

/// Same as [`walk`] but operate on a type, which can contain terms as contracts (`TypeF::Flat`),
/// instead of a term.
fn walk_type<L: Linearizer>(
    state: &mut State,
    ctxt: Context,
    lin: &mut Linearization<L::Building>,
    mut linearizer: L,
    ty: &Type,
) -> Result<(), TypecheckError> {
    match &ty.typ {
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
            walk_type(state, ctxt.clone(), lin, linearizer.scope(), &row.typ)?;
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
    mut ctxt: Context,
    lin: &mut Linearization<L::Building>,
    mut linearizer: L,
    annot: &TypeAnnotation,
    value: Option<&RichTerm>,
) -> Result<(), TypecheckError> {
    annot
        .iter()
        .try_for_each(|ty| walk_type(state, ctxt.clone(), lin, linearizer.scope_meta(), &ty.typ))?;

    match (annot, value) {
        (
            TypeAnnotation {
                typ: Some(LabeledType { typ: ty2, .. }),
                ..
            },
            Some(value),
        ) => {
            let uty2 = UnifType::from_type(ty2.clone(), &ctxt.term_env);
            let instantiated = instantiate_foralls(state, &mut ctxt, uty2, ForallInst::Constant);
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
        Term::Null => ty
            .unify(mk_uniftype::dynamic(), state, &ctxt)
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Bool(_) => ty
            .unify(mk_uniftype::bool(), state, &ctxt)
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Num(_) => ty
            .unify(mk_uniftype::num(), state, &ctxt)
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Str(_) => ty
            .unify(mk_uniftype::str(), state, &ctxt)
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::StrChunks(chunks) => {
            ty.unify(mk_uniftype::str(), state, &ctxt)
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
            let src = state.table.fresh_type_uvar(ctxt.var_level);
            let trg = state.table.fresh_type_uvar(ctxt.var_level);
            let arr = mk_uty_arrow!(src.clone(), trg.clone());

            linearizer.retype_ident(lin, x, src.clone());

            ty.unify(arr, state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            ctxt.type_env.insert(*x, src);
            check(state, ctxt, lin, linearizer, t, trg)
        }
        Term::FunPattern(x, pat, t) => {
            let src_rows_ty = destructuring::build_pattern_type_check_mode(state, &ctxt, pat)?;
            let src = UnifType::concrete(TypeF::Record(src_rows_ty.clone()));
            let trg = state.table.fresh_type_uvar(ctxt.var_level);
            let arr = mk_uty_arrow!(src.clone(), trg.clone());

            if let Some(x) = x {
                linearizer.retype_ident(lin, x, src.clone());
                ctxt.type_env.insert(*x, src);
            }

            destructuring::inject_pattern_variables(state, &mut ctxt.type_env, pat, src_rows_ty);
            ty.unify(arr, state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))?;
            check(state, ctxt, lin, linearizer, t, trg)
        }
        Term::Array(terms, _) => {
            let ty_elts = state.table.fresh_type_uvar(ctxt.var_level);

            ty.unify(mk_uniftype::array(ty_elts.clone()), state, &ctxt)
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
            ty.unify(mk_uniftype::dynamic(), state, &ctxt)
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
            let pattern_type = UnifType::concrete(TypeF::Record(pattern_rows_type.clone()));
            // The inferred type of the expr being bound
            let ty_let = binding_type(state, re.as_ref(), &ctxt, true);

            ty_let
                .clone()
                .unify(pattern_type, state, &ctxt)
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

            let src = state.table.fresh_type_uvar(ctxt.var_level);
            let tgt = state.table.fresh_type_uvar(ctxt.var_level);
            let arr = mk_uty_arrow!(src.clone(), tgt.clone());

            arr.unify(function_type, state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, e.pos))?;

            check(state, ctxt.clone(), lin, linearizer, t, src)?;

            subsumption(state, &ctxt, tgt, ty).map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Match { cases, default } => {
            // Currently, if the match has a default value, we typecheck the whole thing as taking
            // ANY enum, since it's more permissive and there's no loss of information.

            // A match expression is a special kind of function. Thus it's typed as `a -> b`, where
            // `a` is a enum type determined by the matched tags and `b` is the type of each match
            // arm.
            let arg_type = state.table.fresh_type_uvar(ctxt.var_level);
            let return_type = state.table.fresh_type_uvar(ctxt.var_level);

            // We unify the expected type of the match expression with `arg_type -> return_type`
            ty.unify(
                mk_uty_arrow!(arg_type.clone(), return_type.clone()),
                state,
                &ctxt,
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
                    state.table.fresh_erows_uvar(ctxt.var_level)
                }
                None => cases.iter().try_fold(
                    EnumRowsF::Empty.into(),
                    |acc, x| -> Result<UnifEnumRows, TypecheckError> {
                        Ok(mk_uty_enum_row!(*x.0; acc))
                    },
                )?,
            };

            arg_type
                .unify(mk_uty_enum!(; erows), state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Var(x) => {
            let x_ty = ctxt
                .type_env
                .get(x)
                .cloned()
                .ok_or(TypecheckError::UnboundIdentifier(*x, *pos))?;

            let instantiated = instantiate_foralls(state, &mut ctxt, x_ty, ForallInst::Ptr);
            ty.unify(instantiated, state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Enum(id) => {
            let row = state.table.fresh_erows_uvar(ctxt.var_level);
            ty.unify(mk_uty_enum!(*id; row), state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        // If some fields are defined dynamically, the only potential type that works is `{_ : a}`
        // for some `a`. In other words, the checking rule is not the same depending on the target
        // type: if the target type is a dictionary type, we simply check each field against the
        // element type.
        Term::RecRecord(record, dynamic, ..) if !dynamic.is_empty() => {
            let ty_dict = state.table.fresh_type_uvar(ctxt.var_level);
            ty.unify(mk_uniftype::dict(ty_dict.clone()), state, &ctxt)
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

            if let UnifType::Concrete {
                typ:
                    TypeF::Dict {
                        type_fields: rec_ty,
                        ..
                    },
                ..
            } = root_ty
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
                    .map(|id| (*id, state.table.fresh_type_uvar(ctxt.var_level)))
                    .collect();

                let rows = field_types.iter().fold(
                    mk_uty_row!(),
                    |acc, (id, row_ty)| mk_uty_row!((*id, row_ty.clone()); acc),
                );

                ty.unify(mk_uty_record!(; rows), state, &ctxt)
                    .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

                for (id, field) in record.fields.iter() {
                    if let Term::RecRecord(..) = t.as_ref() {
                        let affected_type = ctxt.type_env.get(id).cloned().unwrap();

                        field_types
                            .get(id)
                            .cloned()
                            .unwrap()
                            .unify(affected_type, state, &ctxt)
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
            let (ty_arg, ty_res) = get_uop_type(state, ctxt.var_level, op)?;

            //TODO: this should go after the call to `subsumption`, to avoid context cloning, once
            //we get rid of the special casing of type instantation below. For the time being, the
            //instantiation is depending on this check having happened before, so we have to leave
            //it as it is.
            check(state, ctxt.clone(), lin, linearizer.scope(), t, ty_arg)?;

            let instantiated = instantiate_foralls(state, &mut ctxt, ty_res, ForallInst::Ptr);
            subsumption(state, &ctxt, instantiated, ty)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Op2(op, t1, t2) => {
            let (ty_arg1, ty_arg2, ty_res) = get_bop_type(state, ctxt.var_level, op)?;

            subsumption(state, &ctxt, ty_res, ty)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            check(state, ctxt.clone(), lin, linearizer.scope(), t1, ty_arg1)?;
            check(state, ctxt, lin, linearizer, t2, ty_arg2)
        }
        Term::OpN(op, args) => {
            let (tys_op, ty_ret) = get_nop_type(state, ctxt.var_level, op)?;

            ty.unify(ty_ret, state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            tys_op.into_iter().zip(args.iter()).try_for_each(
                |(ty_t, t)| -> Result<_, TypecheckError> {
                    check(state, ctxt.clone(), lin, linearizer.scope(), t, ty_t)?;
                    Ok(())
                },
            )?;

            Ok(())
        }
        Term::Annotated(annot, rt) => check_annotated(state, ctxt, lin, linearizer, annot, rt, ty),
        Term::SealingKey(_) => ty
            .unify(mk_uniftype::sym(), state, &ctxt)
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Sealed(_, t, _) => check(state, ctxt, lin, linearizer, t, ty),
        Term::Import(_) => ty
            .unify(mk_uniftype::dynamic(), state, &ctxt)
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
            ty.unify(ty_import, state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Type(typ) => {
            if let Some(flat) = typ.find_flat() {
                Err(TypecheckError::FlatTypeInTermPosition { flat, pos: *pos })
            } else {
                Ok(())
            }
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
    checked.unify(inferred, state, ctxt)
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
/// always `Some(_)`) as well as field definitions (where `value` may or may not be defined).
///
/// The last argument is a position to use for error reporting when `value` is `None`.
#[allow(clippy::too_many_arguments)] // TODO: Is it worth doing something about it?
fn check_with_annot<L: Linearizer>(
    state: &mut State,
    mut ctxt: Context,
    lin: &mut Linearization<L::Building>,
    mut linearizer: L,
    annot: &TypeAnnotation,
    value: Option<&RichTerm>,
    ty: UnifType,
    pos: TermPos,
) -> Result<(), TypecheckError> {
    annot
        .iter()
        .try_for_each(|ty| walk_type(state, ctxt.clone(), lin, linearizer.scope_meta(), &ty.typ))?;

    match (annot, value) {
        (
            TypeAnnotation {
                typ: Some(LabeledType { typ: ty2, .. }),
                ..
            },
            Some(value),
        ) => {
            let uty2 = UnifType::from_type(ty2.clone(), &ctxt.term_env);
            let instantiated =
                instantiate_foralls(state, &mut ctxt, uty2.clone(), ForallInst::Constant);

            uty2.unify(ty, state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, pos))?;
            check(state, ctxt, lin, linearizer, value, instantiated)
        }
        // A annotation without a type but with a contract switches the typechecker back to walk
        // mode. If there are several contracts, we arbitrarily chose the first one as the apparent
        // type (the most precise type would be the intersection of all contracts, but Nickel's
        // type system doesn't feature intersection types).
        (
            TypeAnnotation {
                typ: None,
                contracts,
            },
            value_opt,
        ) if !contracts.is_empty() => {
            let ctr = contracts.get(0).unwrap();
            let LabeledType { typ: ty2, .. } = ctr;

            ty.unify(
                UnifType::from_type(ty2.clone(), &ctxt.term_env),
                state,
                &ctxt,
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
                .map(|labeled_ty| UnifType::from_type(labeled_ty.typ.clone(), &ctxt.term_env))
                .unwrap_or_else(mk_uniftype::dynamic);
            ty.unify(inferred, state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, pos))
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
    let inferred = state.table.fresh_type_uvar(ctxt.var_level);
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
            replace_wildcards_with_var(state.table, ctxt, state.wildcard_vars, ty)
        }
        ApparentType::Approximated(_) if strict => state.table.fresh_type_uvar(ctxt.var_level),
        ty_apt => UnifType::from_apparent_type(ty_apt, &ctxt.term_env),
    }
}

/// Substitute wildcards in a type for their unification variable.
fn replace_wildcards_with_var(
    table: &mut UnifTable,
    ctxt: &Context,
    wildcard_vars: &mut Vec<UnifType>,
    ty: Type,
) -> UnifType {
    fn replace_rrows(
        table: &mut UnifTable,
        ctxt: &Context,
        wildcard_vars: &mut Vec<UnifType>,
        rrows: RecordRows,
    ) -> UnifRecordRows {
        UnifRecordRows::concrete(rrows.0.map_state(
            |ty, (table, wildcard_vars)| {
                Box::new(replace_wildcards_with_var(table, ctxt, wildcard_vars, *ty))
            },
            |rrows, (table, wildcard_vars)| {
                Box::new(replace_rrows(table, ctxt, wildcard_vars, *rrows))
            },
            &mut (table, wildcard_vars),
        ))
    }

    match ty.typ {
        TypeF::Wildcard(i) => get_wildcard_var(table, ctxt.var_level, wildcard_vars, i),
        TypeF::Flat(t) => UnifType::Contract(t, ctxt.term_env.clone()),
        _ => UnifType::concrete(ty.typ.map_state(
            |ty, (table, wildcard_vars)| {
                Box::new(replace_wildcards_with_var(table, ctxt, wildcard_vars, *ty))
            },
            |rrows, (table, wildcard_vars)| replace_rrows(table, ctxt, wildcard_vars, rrows),
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
    Annotated(Type),
    /// The apparent type has been inferred from a simple expression.
    Inferred(Type),
    /// The term is a variable and its type was retrieved from the typing environment.
    FromEnv(UnifType),
    /// The apparent type wasn't trivial to determine, and an approximation (most of the time,
    /// `Dyn`) has been returned.
    Approximated(Type),
}

impl From<ApparentType> for Type {
    fn from(at: ApparentType) -> Self {
        match at {
            ApparentType::Annotated(ty) if has_wildcards(&ty) => Type::from(TypeF::Dyn),
            ApparentType::Annotated(ty)
            | ApparentType::Inferred(ty)
            | ApparentType::Approximated(ty) => ty,
            ApparentType::FromEnv(uty) => uty.try_into().ok().unwrap_or(Type::from(TypeF::Dyn)),
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
        .map(|labeled_ty| ApparentType::Annotated(labeled_ty.typ))
        .or_else(|| {
            field
                .value
                .as_ref()
                .map(|v| apparent_type(v.as_ref(), env, resolver))
        })
        .unwrap_or(ApparentType::Approximated(Type::from(TypeF::Dyn)))
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
                .map(|labeled_ty| ApparentType::Annotated(labeled_ty.typ.clone()))
                .unwrap_or_else(|| apparent_type(value.as_ref(), env, resolver)),
            Term::Num(_) => ApparentType::Inferred(Type::from(TypeF::Number)),
            Term::Bool(_) => ApparentType::Inferred(Type::from(TypeF::Bool)),
            Term::SealingKey(_) => ApparentType::Inferred(Type::from(TypeF::Symbol)),
            Term::Str(_) | Term::StrChunks(_) => ApparentType::Inferred(Type::from(TypeF::String)),
            Term::Array(..) => ApparentType::Approximated(Type::from(TypeF::Array(Box::new(
                Type::from(TypeF::Dyn),
            )))),
            Term::Var(id) => env
                .and_then(|envs| envs.get(id).cloned())
                .map(ApparentType::FromEnv)
                .unwrap_or(ApparentType::Approximated(Type::from(TypeF::Dyn))),
            Term::ResolvedImport(file_id) => match resolver {
                Some(r) if !imports_seen.contains(file_id) => {
                    imports_seen.insert(*file_id);

                    let t = r
                        .get(*file_id)
                        .expect("Internal error: resolved import not found during typechecking.");
                    apparent_type_check_cycle(&t.term, env, Some(r), imports_seen)
                }
                _ => ApparentType::Approximated(Type::from(TypeF::Dyn)),
            },
            _ => ApparentType::Approximated(Type::from(TypeF::Dyn)),
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
            TypeF::Record(UnifRecordRows::concrete(record.fields.iter().fold(
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
                            typ: Box::new(uty),
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
fn has_wildcards(ty: &Type) -> bool {
    let mut has_wildcard = false;
    ty.clone()
        .traverse::<_, _, std::convert::Infallible>(
            &|ty: Type, has_wildcard| {
                if ty.typ.is_wildcard() {
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
fn instantiate_foralls(
    state: &mut State,
    ctxt: &mut Context,
    mut ty: UnifType,
    inst: ForallInst,
) -> UnifType {
    ty = ty.into_root(state.table);

    // We are instantiating a polymorphic type: it's precisely the place where we have to increment
    // the variable level, to prevent already existing unification variables to unify with the
    // rigid type variables introduced here.
    ctxt.var_level.incr();

    while let UnifType::Concrete {
        typ: TypeF::Forall {
            var,
            var_kind,
            body,
        },
        ..
    } = ty
    {
        let kind = (&var_kind).into();
        match var_kind {
            VarKind::Type => {
                let fresh_uid = state.table.fresh_type_var_id(ctxt.var_level);
                let uvar = match inst {
                    ForallInst::Constant => UnifType::Constant(fresh_uid),
                    ForallInst::Ptr => UnifType::UnifVar {
                        id: fresh_uid,
                        init_level: ctxt.var_level,
                    },
                };
                state.names.insert((fresh_uid, kind), var);
                ty = body.subst(&var, &uvar);
            }
            VarKind::RecordRows { excluded } => {
                let fresh_uid = state.table.fresh_rrows_var_id(ctxt.var_level);
                let uvar = match inst {
                    ForallInst::Constant => UnifRecordRows::Constant(fresh_uid),
                    ForallInst::Ptr => UnifRecordRows::UnifVar {
                        id: fresh_uid,
                        init_level: ctxt.var_level,
                    },
                };
                state.names.insert((fresh_uid, kind), var);
                ty = body.subst(&var, &uvar);

                if inst == ForallInst::Ptr {
                    state.constr.insert(fresh_uid, excluded);
                }
            }
            VarKind::EnumRows => {
                let fresh_uid = state.table.fresh_erows_var_id(ctxt.var_level);
                let uvar = match inst {
                    ForallInst::Constant => UnifEnumRows::Constant(fresh_uid),
                    ForallInst::Ptr => UnifEnumRows::UnifVar {
                        id: fresh_uid,
                        init_level: ctxt.var_level,
                    },
                };
                state.names.insert((fresh_uid, kind), var);
                ty = body.subst(&var, &uvar);
            }
        };
    }

    ty
}

/// Get the type unification variable associated with a given wildcard ID.
fn get_wildcard_var(
    table: &mut UnifTable,
    var_level: VarLevel,
    wildcard_vars: &mut Vec<UnifType>,
    id: VarId,
) -> UnifType {
    // If `id` is not in `wildcard_vars`, populate it with fresh vars up to `id`
    if id >= wildcard_vars.len() {
        wildcard_vars.extend((wildcard_vars.len()..=id).map(|_| table.fresh_type_uvar(var_level)));
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
