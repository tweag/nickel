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
//!   operator `:`, as in `1 + 1 : Number` or `let f : Number -> Number = fun x => x + 1 in ..`.
//!   Enforce mode is implemented by [`type_check`] and variants.
//! - **walk** doesn't enforce any typing but traverses the AST looking for typed blocks to
//!   typecheck. Walk mode also stores the annotations of bound identifiers in the environment. This
//!   is implemented by the `walk` function.
//!
//! The algorithm usually starts in walk mode, although this can be configured. A typed block
//! (an expression annotated with a type) switches to enforce mode, and is switched back to walk
//! mode when entering an expression annotated with a contract. Type and contract annotations thus
//! serve as a switch for the typechecking mode.
//!
//! Note that the static typing part (enforce mode) is based on the bidirectional typing framework,
//! which defines two different modes. Thus, the enforce mode is itself divided again into
//! **checking** mode and **inference** mode.
//!
//! # Type inference
//!
//! Type inference is done via a form of bidirectional typechecking coupled with unification, in the
//! same spirit as GHC (Haskell), although the type system of Nickel is simpler. The type of
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
//! generalized to `forall a. a -> a`. At the first call site, `_a` is unified with `String`, and at
//! the second call site the typechecker complains that `5` is not of type `String`.
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
use super::ast::{
    pattern::bindings::Bindings as _, record::FieldDef, typ::*, Annotation, Ast, AstAlloc,
    MatchBranch, Node, StringChunk, TryConvert,
};

use crate::{
    cache::ImportResolver,
    environment::Environment,
    error::TypecheckError,
    identifier::{Ident, LocIdent},
    mk_buty_arrow, mk_buty_enum, mk_buty_record, mk_buty_record_row, stdlib as nickel_stdlib,
    traverse::TraverseAlloc,
    typ::{EnumRowsIterator, RecordRowsIterator, VarKind, VarKindDiscriminant},
};

use std::{
    cmp::max,
    collections::{HashMap, HashSet},
    convert::TryInto,
    num::NonZeroU16,
};

pub mod error;
pub mod operation;
mod pattern;
pub mod reporting;
#[macro_use]
pub mod mk_uniftype;
pub mod eq;
pub mod record;
pub mod subtyping;
pub mod unif;

use error::*;
use operation::PrimOpType;
use pattern::{PatternTypeData, PatternTypes};
use record::Resolve;
use unif::*;

use self::subtyping::SubsumedBy;

/// The max depth parameter used to limit the work performed when inferring the type of the stdlib.
const INFER_RECORD_MAX_DEPTH: u8 = 4;

/// The typechecker has two modes, one for statically typed code and one for dynamically type code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypecheckMode {
    /// In `Walk` mode, the typechecker traverses the AST looking for typed blocks.
    Walk,
    /// In `Enforce` mode, the typechecker checks types.
    Enforce,
}

/// The typing environment.
pub type TypeEnv<'ast> = Environment<Ident, UnifType<'ast>>;

/// A term environment defined as a mapping from identifiers to a tuple of a term and an
/// environment (i.e. a closure). Used to compute contract equality.
#[derive(PartialEq, Clone, Debug)]
pub struct TermEnv<'ast>(pub Environment<Ident, (Ast<'ast>, TermEnv<'ast>)>);

impl TermEnv<'_> {
    pub fn new() -> Self {
        TermEnv(Environment::new())
    }
}

impl Default for TermEnv<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ast> std::iter::FromIterator<(Ident, (Ast<'ast>, TermEnv<'ast>))> for TermEnv<'ast> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (Ident, (Ast<'ast>, TermEnv<'ast>))>,
    {
        TermEnv(Environment::<Ident, (Ast<'ast>, TermEnv<'ast>)>::from_iter(
            iter,
        ))
    }
}

/// Mapping from wildcard IDs to inferred types
pub type Wildcards<'ast> = Vec<Type<'ast>>;

/// A table mapping type variables and their kind to names. Used for reporting.
pub type NameTable = HashMap<(VarId, VarKindDiscriminant), Ident>;

/// A unifiable record row.
pub type UnifRecordRow<'ast> = RecordRowF<Box<UnifType<'ast>>>;
pub type UnifRecordRowsUnr<'ast> = RecordRowsF<Box<UnifType<'ast>>, Box<UnifRecordRows<'ast>>>;

/// Unifiable record rows. Same shape as [`crate::bytecode::ast::typ::RecordRows`], but where each
/// type is unifiable, and each tail may be a unification variable (or a constant).
#[derive(Clone, PartialEq, Debug)]
pub enum UnifRecordRows<'ast> {
    Concrete {
        rrows: UnifRecordRowsUnr<'ast>,
        /// Additional metadata related to unification variable levels update. See [VarLevelsData].
        var_levels_data: VarLevelsData,
    },
    Constant(VarId),
    /// A unification variable.
    UnifVar {
        /// The unique identifier of this variable in the unification table.
        id: VarId,
        /// The initial variable level at which the variable was created. See
        /// [UnifType::UnifVar].
        init_level: VarLevel,
    },
}

pub type UnifEnumRow<'ast> = EnumRowF<Box<UnifType<'ast>>>;
pub type UnifEnumRowsUnr<'ast> = EnumRowsF<Box<UnifType<'ast>>, Box<UnifEnumRows<'ast>>>;

/// Unifiable enum rows. Same shape as [`crate::typ::EnumRows`] but where each tail may be a
/// unification variable (or a constant).
#[derive(Clone, PartialEq, Debug)]
pub enum UnifEnumRows<'ast> {
    Concrete {
        erows: UnifEnumRowsUnr<'ast>,
        /// Additional metadata related to unification variable levels update. See [VarLevelsData].
        var_levels_data: VarLevelsData,
    },
    Constant(VarId),
    UnifVar {
        /// The unique identifier of this variable in the unification table.
        id: VarId,
        /// The initial variable level at which the variable was created. See
        /// [UnifType::UnifVar].
        init_level: VarLevel,
    },
}

/// Metadata attached to unification types, which are used to delay and optimize potentially costly
/// type traversals when updating the levels of the free unification variables of a type. Based on
/// Didier Remy's algorithm for the OCaml typechecker, see [Efficient and insightful
/// generalization](http://web.archive.org/web/20230525023637/https://okmij.org/ftp/ML/generalization.html).
///
/// When unifying a variable with a composite type, we have to update the levels of all the free
/// unification variables contained in that type, which naively incurs a full traversal of the type.
/// The idea behind Didier Remy's algorithm is to delay such traversals, and use the values of
/// [VarLevelsData] to group traversals and avoid unneeded ones. This make variable unification run
/// in constant time again, as long as we don't unify with a rigid type variable.
///
/// Variable levels data might correspond to different variable kinds (type, record rows and enum
/// rows) depending on where they appear (in a [UnifType<'ast>], [UnifRecordRows<'ast>] or [UnifEnumRows<'ast>])
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

    /// Create new variable levels data with an upper bound which indicates that there is no
    /// unification variable in the attached type and no pending level update.
    pub fn new_no_uvars() -> Self {
        Self::new_from_bound(VarLevel::NO_VAR)
    }
}

/// Unification types and variants that store an upper bound on the level of the unification
/// variables they contain, or for which an upper bound can be computed quickly (in constant time).
trait VarLevelUpperBound {
    // Return an upper bound on the level of the unification variables contained in `self`.
    // Depending on the implementer, the level might refer to different kind of unification
    // variables (type, record rows or enum rows).
    fn var_level_upper_bound(&self) -> VarLevel;
}

impl VarLevelUpperBound for UnifType<'_> {
    fn var_level_upper_bound(&self) -> VarLevel {
        match self {
            UnifType::Concrete {
                var_levels_data, ..
            } => var_levels_data.upper_bound,
            UnifType::UnifVar { init_level, .. } => *init_level,
            UnifType::Constant(_) => VarLevel::NO_VAR,
        }
    }
}

impl VarLevelUpperBound for UnifTypeUnr<'_> {
    fn var_level_upper_bound(&self) -> VarLevel {
        match self {
            TypeF::Dyn
            | TypeF::Bool
            | TypeF::Number
            | TypeF::String
            | TypeF::ForeignId
            | TypeF::Symbol => VarLevel::NO_VAR,
            TypeF::Arrow(domain, codomain) => max(
                domain.var_level_upper_bound(),
                codomain.var_level_upper_bound(),
            ),
            TypeF::Forall { body, .. } => body.var_level_upper_bound(),
            TypeF::Enum(erows) => erows.var_level_upper_bound(),
            TypeF::Record(rrows) => rrows.var_level_upper_bound(),
            TypeF::Dict { type_fields, .. } => type_fields.var_level_upper_bound(),
            TypeF::Array(ty_elts) => ty_elts.var_level_upper_bound(),
            TypeF::Wildcard(_) | TypeF::Var(_) | TypeF::Contract(_) => VarLevel::NO_VAR,
        }
    }
}

impl VarLevelUpperBound for UnifEnumRows<'_> {
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

impl VarLevelUpperBound for UnifEnumRowsUnr<'_> {
    fn var_level_upper_bound(&self) -> VarLevel {
        match self {
            // A var that hasn't be instantiated yet isn't a unification variable
            EnumRowsF::Empty | EnumRowsF::TailVar(_) => VarLevel::NO_VAR,
            EnumRowsF::Extend { row: _, tail } => tail.var_level_upper_bound(),
        }
    }
}

impl VarLevelUpperBound for UnifRecordRows<'_> {
    fn var_level_upper_bound(&self) -> VarLevel {
        match self {
            UnifRecordRows::Concrete {
                var_levels_data, ..
            } => var_levels_data.upper_bound,
            UnifRecordRows::UnifVar { init_level, .. } => *init_level,
            UnifRecordRows::Constant(_) => VarLevel::NO_VAR,
        }
    }
}

impl VarLevelUpperBound for UnifRecordRowsUnr<'_> {
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
/// Contracts store an additional term environment for contract equality checking.
///
/// # Invariants
///
/// **Important**: the following invariant must always be satisfied: for any free unification
/// variable[^free-unif-var] part of a concrete unification type, the level of this variable must
/// be smaller or equal to `var_levels_data.upper_bound`. Otherwise, the typechecking algorithm
/// might not be correct. Be careful when creating new concrete [UnifType<'ast>] or [UnifType]
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
pub enum UnifType<'ast> {
    /// A concrete type (like `Number` or `String -> String`). Note that subcomponents of a
    /// concrete type can still be free unification variables, such as the type `a -> a`, but the
    /// top-level node is a concrete type constructor.
    Concrete {
        typ: UnifTypeUnr<'ast>,
        /// Additional metadata related to unification variable levels update. See [VarLevelsData].
        var_levels_data: VarLevelsData,
    },
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

type UnifTypeUnr<'ast> = TypeF<
    Box<UnifType<'ast>>,
    UnifRecordRows<'ast>,
    UnifEnumRows<'ast>,
    (&'ast Ast<'ast>, TermEnv<'ast>),
>;

impl<'ast> UnifType<'ast> {
    /// Create a concrete generic unification type. Compute the variable levels data from the
    /// subcomponents.
    pub fn concrete(typ: UnifTypeUnr<'ast>) -> Self {
        let upper_bound = typ.var_level_upper_bound();

        UnifType::Concrete {
            typ,
            var_levels_data: VarLevelsData::new_from_bound(upper_bound),
        }
    }

    /// Create a [`UnifType<'ast>`] from a [`Type`].
    pub fn from_type(ty: Type<'ast>, env: &TermEnv<'ast>) -> Self {
        UnifType::concrete(ty.typ.map(
            |ty| Box::new(UnifType::from_type(ty.clone(), env)),
            |rrows| UnifRecordRows::from_record_rows(rrows, env),
            |erows| UnifEnumRows::from_enum_rows(erows, env),
            |term| (term, env.clone()),
        ))
    }

    /// Create a [`UnifType<'ast>`] from an [`ApparentType`]. As for [`UnifType::from_type`], this
    /// function requires the current term environment.
    pub fn from_apparent_type(at: ApparentType<'ast>, env: &TermEnv<'ast>) -> Self {
        match at {
            ApparentType::Annotated(ty) if has_wildcards(&ty) => UnifType::concrete(TypeF::Dyn),
            ApparentType::Annotated(ty)
            | ApparentType::Inferred(ty)
            | ApparentType::Approximated(ty) => UnifType::from_type(ty, env),
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

    /// Extract the concrete type corresponding to a unifiable type. Free unification variables as
    /// well as type constants are replaced with the type `Dyn`.
    fn into_type(self, alloc: &'ast AstAlloc, table: &UnifTable<'ast>) -> Type<'ast> {
        match self {
            UnifType::UnifVar { id, init_level } => match table.root_type(id, init_level) {
                t @ UnifType::Concrete { .. } => t.into_type(alloc, table),
                _ => Type::from(TypeF::Dyn),
            },
            UnifType::Constant(_) => Type::from(TypeF::Dyn),
            UnifType::Concrete { typ, .. } => {
                let mapped = typ.map(
                    |btyp| alloc.alloc(btyp.into_type(alloc, table)),
                    |urrows| urrows.into_rrows(alloc, table),
                    |uerows| uerows.into_erows(alloc, table),
                    |(term, _env)| term,
                );
                Type::from(mapped)
            }
        }
    }

    /// Return the unification root associated with this type. If the type is a unification
    /// variable, return the result of `table.root_type`. Return `self` otherwise.
    fn into_root(self, table: &UnifTable<'ast>) -> Self {
        match self {
            UnifType::UnifVar { id, init_level } => table.root_type(id, init_level),
            uty => uty,
        }
    }
}

impl<'ast> UnifRecordRows<'ast> {
    /// Create concrete generic record rows. Compute the variable levels data from the
    /// subcomponents.
    pub fn concrete(typ: UnifRecordRowsUnr<'ast>) -> Self {
        let upper_bound = typ.var_level_upper_bound();

        UnifRecordRows::Concrete {
            rrows: typ,
            var_levels_data: VarLevelsData::new_from_bound(upper_bound),
        }
    }

    /// Extract the concrete [`RecordRows`] corresponding to a [`UnifRecordRows<'ast>`]. Free unification
    /// variables as well as type constants are replaced with the empty row.
    fn into_rrows(self, alloc: &'ast AstAlloc, table: &UnifTable<'ast>) -> RecordRows<'ast> {
        match self {
            UnifRecordRows::UnifVar { id, init_level } => match table.root_rrows(id, init_level) {
                t @ UnifRecordRows::Concrete { .. } => t.into_rrows(alloc, table),
                _ => RecordRows(RecordRowsF::Empty),
            },
            UnifRecordRows::Constant(_) => RecordRows(RecordRowsF::Empty),
            UnifRecordRows::Concrete { rrows, .. } => {
                let mapped = rrows.map(
                    |ty| alloc.alloc(ty.into_type(alloc, table)),
                    |rrows| alloc.alloc(rrows.into_rrows(alloc, table)),
                );
                RecordRows(mapped)
            }
        }
    }

    /// Return the unification root associated with these record rows. If the rows are a unification
    /// variable, return the result of `table.root_rrows`. Return `self` otherwise.
    fn into_root(self, table: &UnifTable<'ast>) -> Self {
        match self {
            UnifRecordRows::UnifVar { id, init_level } => table.root_rrows(id, init_level),
            urrows => urrows,
        }
    }
}

impl<'ast> UnifEnumRows<'ast> {
    /// Create concrete generic enum rows. Compute the variable levels data from the subcomponents.
    pub fn concrete(typ: UnifEnumRowsUnr<'ast>) -> Self {
        let upper_bound = typ.var_level_upper_bound();

        UnifEnumRows::Concrete {
            erows: typ,
            var_levels_data: VarLevelsData::new_from_bound(upper_bound),
        }
    }

    /// Extract the concrete [`EnumRows`] corresponding to a [`UnifEnumRows<'ast>`]. Free unification
    /// variables as well as type constants are replaced with the empty row.
    fn into_erows(self, alloc: &'ast AstAlloc, table: &UnifTable<'ast>) -> EnumRows<'ast> {
        match self {
            UnifEnumRows::UnifVar { id, init_level } => match table.root_erows(id, init_level) {
                t @ UnifEnumRows::Concrete { .. } => t.into_erows(alloc, table),
                _ => EnumRows(EnumRowsF::Empty),
            },
            UnifEnumRows::Constant(_) => EnumRows(EnumRowsF::Empty),
            UnifEnumRows::Concrete { erows, .. } => {
                let mapped = erows.map(
                    |ty| alloc.alloc(ty.into_type(alloc, table)),
                    |erows| alloc.alloc(erows.into_erows(alloc, table)),
                );
                EnumRows(mapped)
            }
        }
    }

    /// Return the unification root associated with these enum rows. If the rows are a unification
    /// variable, return the result of `table.root_erows`. Return `self` otherwise.
    fn into_root(self, table: &UnifTable<'ast>) -> Self {
        match self {
            UnifEnumRows::UnifVar { id, init_level } => table.root_erows(id, init_level),
            uerows => uerows,
        }
    }
}

impl<'ast> TryConvert<'ast, UnifRecordRows<'ast>> for RecordRows<'ast> {
    type Error = ();

    fn try_convert(
        alloc: &'ast AstAlloc,
        urrows: UnifRecordRows<'ast>,
    ) -> Result<RecordRows<'ast>, ()> {
        match urrows {
            UnifRecordRows::Concrete { rrows, .. } => {
                let converted: RecordRowsF<&'ast Type<'ast>, &'ast RecordRows<'ast>> = rrows
                    .try_map(
                        |uty| Ok(alloc.alloc(Type::try_convert(alloc, *uty)?)),
                        |urrows| Ok(alloc.alloc(RecordRows::try_convert(alloc, *urrows)?)),
                    )?;
                Ok(RecordRows(converted))
            }
            _ => Err(()),
        }
    }
}

impl<'ast> TryConvert<'ast, UnifEnumRows<'ast>> for EnumRows<'ast> {
    type Error = ();

    fn try_convert(
        alloc: &'ast AstAlloc,
        uerows: UnifEnumRows<'ast>,
    ) -> Result<EnumRows<'ast>, ()> {
        match uerows {
            UnifEnumRows::Concrete { erows, .. } => {
                let converted: EnumRowsF<&'ast Type<'ast>, &'ast EnumRows<'ast>> = erows.try_map(
                    |uty| Ok(alloc.alloc(Type::try_convert(alloc, *uty)?)),
                    |uerows| Ok(alloc.alloc(EnumRows::try_convert(alloc, *uerows)?)),
                )?;
                Ok(EnumRows(converted))
            }
            _ => Err(()),
        }
    }
}

impl<'ast> TryConvert<'ast, UnifType<'ast>> for Type<'ast> {
    type Error = ();

    fn try_convert(alloc: &'ast AstAlloc, utype: UnifType<'ast>) -> Result<Type<'ast>, ()> {
        match utype {
            UnifType::Concrete { typ, .. } => {
                let converted: TypeF<
                    &'ast Type<'ast>,
                    RecordRows<'ast>,
                    EnumRows<'ast>,
                    &'ast Ast<'ast>,
                > = typ.try_map(
                    |uty_boxed| {
                        let ty = Type::try_convert(alloc, *uty_boxed)?;
                        Ok(alloc.alloc(ty))
                    },
                    |urrows| RecordRows::try_convert(alloc, urrows),
                    |uerows| EnumRows::try_convert(alloc, uerows),
                    |(term, _env)| Ok(term),
                )?;
                Ok(Type::from(converted))
            }
            _ => Err(()),
        }
    }
}

impl<'ast> UnifEnumRows<'ast> {
    pub fn from_enum_rows(erows: EnumRows<'ast>, env: &TermEnv<'ast>) -> Self {
        let f_erow = |ty: &'ast Type<'ast>| Box::new(UnifType::from_type(ty.clone(), env));
        let f_erows = |erows: &'ast EnumRows<'ast>| {
            Box::new(UnifEnumRows::from_enum_rows(erows.clone(), env))
        };

        UnifEnumRows::concrete(erows.0.map(f_erow, f_erows))
    }
}

impl<'ast> UnifEnumRows<'ast> {
    /// Return an iterator producing immutable references to individual rows.
    pub(super) fn iter(&self) -> EnumRowsIterator<UnifType<'ast>, UnifEnumRows<'ast>> {
        EnumRowsIterator {
            erows: Some(self),
            ty: std::marker::PhantomData,
        }
    }
}

impl<'ast> UnifRecordRows<'ast> {
    /// Create [UnifRecordRows<'ast>] from [RecordRows].
    pub fn from_record_rows(rrows: RecordRows<'ast>, env: &TermEnv<'ast>) -> Self {
        let f_rrow = |ty: &'ast Type<'ast>| Box::new(UnifType::from_type(ty.clone(), env));
        let f_rrows = |rrows: &'ast RecordRows<'ast>| {
            Box::new(UnifRecordRows::from_record_rows(rrows.clone(), env))
        };

        UnifRecordRows::concrete(rrows.0.map(f_rrow, f_rrows))
    }
}

impl<'ast> UnifRecordRows<'ast> {
    pub(super) fn iter(&self) -> RecordRowsIterator<UnifType<'ast>, UnifRecordRows<'ast>> {
        RecordRowsIterator {
            rrows: Some(self),
            ty: std::marker::PhantomData,
        }
    }
}

/// A type which contains variables that can be substituted with values of type `T`.
trait Subst<T: Clone>: Sized {
    /// Substitute all variables of identifier `id` with `to`.
    fn subst(self, id: &LocIdent, to: &T) -> Self {
        self.subst_levels(id, to).0
    }

    /// Must be filled by implementers of this trait.
    /// In addition to performing substitution, this method threads variable levels upper bounds to
    /// compute new upper bounds efficiently.
    fn subst_levels(self, id: &LocIdent, to: &T) -> (Self, VarLevel);
}

impl<'ast> Subst<UnifType<'ast>> for UnifType<'ast> {
    fn subst_levels(self, id: &LocIdent, to: &UnifType<'ast>) -> (Self, VarLevel) {
        match self {
            UnifType::Concrete {
                typ: TypeF::Var(var_id),
                var_levels_data,
            } if var_id == id.ident() => {
                // A free type variable isn't (yet) a unification variable, so it shouldn't have a
                // level set at this point. During instantiation, it might be substituted for a
                // unification variable by this very function, and will then inherit this level.
                debug_assert!(var_levels_data.upper_bound == VarLevel::NO_VAR);

                (to.clone(), to.var_level_upper_bound())
            }
            UnifType::Concrete {
                typ,
                var_levels_data,
            } => {
                let mut upper_bound = VarLevel::NO_VAR;

                let new_ty = UnifType::Concrete {
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
                        |erows, upper_bound| {
                            let (new_erows, new_ub) = erows.subst_levels(id, to);
                            *upper_bound = max(*upper_bound, new_ub);
                            new_erows
                        },
                        // Substitution doesn't cross the contract boundaries
                        |ctr, _upper_bound| ctr,
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

impl<'ast> Subst<UnifType<'ast>> for UnifRecordRows<'ast> {
    fn subst_levels(self, id: &LocIdent, to: &UnifType<'ast>) -> (Self, VarLevel) {
        match self {
            UnifRecordRows::Concrete {
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

                let new_urrows = UnifRecordRows::Concrete {
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

impl<'ast> Subst<UnifType<'ast>> for UnifEnumRows<'ast> {
    fn subst_levels(self, id: &LocIdent, to: &UnifType<'ast>) -> (Self, VarLevel) {
        match self {
            UnifEnumRows::Concrete {
                erows,
                var_levels_data,
            } => {
                let mut upper_bound = VarLevel::NO_VAR;

                let new_erows = erows.map_state(
                    |ty, upper_bound| {
                        let (new_ty, new_ub) = ty.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        Box::new(new_ty)
                    },
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

impl<'ast> Subst<UnifRecordRows<'ast>> for UnifType<'ast> {
    fn subst_levels(self, id: &LocIdent, to: &UnifRecordRows<'ast>) -> (Self, VarLevel) {
        match self {
            UnifType::Concrete {
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
                    |ctr, _upper_bound| ctr,
                    &mut upper_bound,
                );

                let new_uty = UnifType::Concrete {
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

impl<'ast> Subst<UnifRecordRows<'ast>> for UnifRecordRows<'ast> {
    fn subst_levels(self, id: &LocIdent, to: &UnifRecordRows<'ast>) -> (Self, VarLevel) {
        match self {
            UnifRecordRows::Concrete {
                rrows: RecordRowsF::TailVar(var_id),
                var_levels_data,
            } if var_id == *id => {
                debug_assert!(var_levels_data.upper_bound == VarLevel::NO_VAR);
                (to.clone(), to.var_level_upper_bound())
            }
            UnifRecordRows::Concrete {
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

                let new_urrows = UnifRecordRows::Concrete {
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

impl<'ast> Subst<UnifRecordRows<'ast>> for UnifEnumRows<'ast> {
    fn subst_levels(self, id: &LocIdent, to: &UnifRecordRows<'ast>) -> (Self, VarLevel) {
        match self {
            UnifEnumRows::Concrete {
                erows,
                var_levels_data,
            } => {
                let mut upper_bound = VarLevel::NO_VAR;

                let new_erows = erows.map_state(
                    |ty, upper_bound| {
                        let (new_ty, new_ub) = ty.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        Box::new(new_ty)
                    },
                    |erows, upper_bound| {
                        let (new_erows, new_ub) = erows.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        Box::new(new_erows)
                    },
                    &mut upper_bound,
                );

                let new_uerows = UnifEnumRows::Concrete {
                    erows: new_erows,
                    var_levels_data,
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

impl<'ast> Subst<UnifEnumRows<'ast>> for UnifType<'ast> {
    fn subst_levels(self, id: &LocIdent, to: &UnifEnumRows<'ast>) -> (Self, VarLevel) {
        match self {
            UnifType::Concrete {
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
                    |ctr, _upper_bound| ctr,
                    &mut upper_bound,
                );

                let new_uty = UnifType::Concrete {
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

impl<'ast> Subst<UnifEnumRows<'ast>> for UnifRecordRows<'ast> {
    fn subst_levels(self, id: &LocIdent, to: &UnifEnumRows<'ast>) -> (Self, VarLevel) {
        match self {
            UnifRecordRows::Concrete {
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

                let new_urrows = UnifRecordRows::Concrete {
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

impl<'ast> Subst<UnifEnumRows<'ast>> for UnifEnumRows<'ast> {
    fn subst_levels(self, id: &LocIdent, to: &UnifEnumRows<'ast>) -> (Self, VarLevel) {
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
                    |ty, upper_bound| {
                        let (new_ty, new_ub) = ty.subst_levels(id, to);
                        *upper_bound = max(*upper_bound, new_ub);
                        Box::new(new_ty)
                    },
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

impl<'ast> From<UnifTypeUnr<'ast>> for UnifType<'ast> {
    fn from(typ: UnifTypeUnr<'ast>) -> Self {
        let var_level_max = typ.var_level_upper_bound();

        UnifType::Concrete {
            typ,
            var_levels_data: VarLevelsData::new_from_bound(var_level_max),
        }
    }
}

impl<'ast> From<RecordRowsF<Box<UnifType<'ast>>, Box<UnifRecordRows<'ast>>>>
    for UnifRecordRows<'ast>
{
    fn from(rrows: RecordRowsF<Box<UnifType<'ast>>, Box<UnifRecordRows<'ast>>>) -> Self {
        let var_level_max = rrows.var_level_upper_bound();

        UnifRecordRows::Concrete {
            rrows,
            var_levels_data: VarLevelsData::new_from_bound(var_level_max),
        }
    }
}

impl<'ast> From<EnumRowsF<Box<UnifType<'ast>>, Box<UnifEnumRows<'ast>>>> for UnifEnumRows<'ast> {
    fn from(erows: EnumRowsF<Box<UnifType<'ast>>, Box<UnifEnumRows<'ast>>>) -> Self {
        UnifEnumRows::concrete(erows)
    }
}

/// Iterator items produced by [RecordRowsIterator] on [UnifRecordRows<'ast>].
pub enum RecordRowsElt<'a, 'ast> {
    TailDyn,
    TailVar(&'a LocIdent),
    TailUnifVar { id: VarId, init_level: VarLevel },
    TailConstant(VarId),
    Row(RecordRowF<&'a UnifType<'ast>>),
}

impl<'a, 'ast> Iterator for RecordRowsIterator<'a, UnifType<'ast>, UnifRecordRows<'ast>> {
    type Item = RecordRowsElt<'a, 'ast>;

    fn next(&mut self) -> Option<Self::Item> {
        self.rrows.and_then(|next| match next {
            UnifRecordRows::Concrete { rrows, .. } => match rrows {
                RecordRowsF::Empty => {
                    self.rrows = None;
                    None
                }
                RecordRowsF::TailDyn => {
                    self.rrows = None;
                    Some(RecordRowsElt::TailDyn)
                }
                RecordRowsF::TailVar(id) => {
                    self.rrows = None;
                    Some(RecordRowsElt::TailVar(&id))
                }
                RecordRowsF::Extend { row, tail } => {
                    self.rrows = Some(tail);
                    Some(RecordRowsElt::Row(RecordRowF {
                        id: row.id,
                        typ: row.typ.as_ref(),
                    }))
                }
            },
            UnifRecordRows::UnifVar { id, init_level } => {
                self.rrows = None;
                Some(RecordRowsElt::TailUnifVar {
                    id: *id,
                    init_level: *init_level,
                })
            }
            UnifRecordRows::Constant(var_id) => {
                self.rrows = None;
                Some(RecordRowsElt::TailConstant(*var_id))
            }
        })
    }
}

/// Iterator items produced by [`EnumRowsIterator`].
pub enum EnumRowsElt<'a, 'ast> {
    TailVar(&'a LocIdent),
    TailUnifVar { id: VarId, init_level: VarLevel },
    TailConstant(VarId),
    Row(EnumRowF<&'a UnifType<'ast>>),
}

impl<'a, 'ast> Iterator for EnumRowsIterator<'a, UnifType<'ast>, UnifEnumRows<'ast>> {
    type Item = EnumRowsElt<'a, 'ast>;

    fn next(&mut self) -> Option<Self::Item> {
        self.erows.and_then(|next| match next {
            UnifEnumRows::Concrete { erows, .. } => match erows {
                EnumRowsF::Empty => {
                    self.erows = None;
                    None
                }
                EnumRowsF::TailVar(id) => {
                    self.erows = None;
                    Some(EnumRowsElt::TailVar(&id))
                }
                EnumRowsF::Extend { row, tail } => {
                    self.erows = Some(tail);
                    Some(EnumRowsElt::Row(EnumRowF {
                        id: row.id,
                        typ: row.typ.as_ref().map(|ty| ty.as_ref()),
                    }))
                }
            },
            UnifEnumRows::UnifVar { id, init_level } => {
                self.erows = None;
                Some(EnumRowsElt::TailUnifVar {
                    id: *id,
                    init_level: *init_level,
                })
            }
            UnifEnumRows::Constant(var_id) => {
                self.erows = None;
                Some(EnumRowsElt::TailConstant(*var_id))
            }
        })
    }
}

pub trait ReifyAsUnifType<'ast> {
    fn unif_type() -> UnifType<'ast>;
}

impl<'ast> ReifyAsUnifType<'ast> for crate::label::TypeVarData {
    fn unif_type() -> UnifType<'ast> {
        mk_buty_record!(("polarity", crate::label::Polarity::unif_type()))
    }
}

impl<'ast> ReifyAsUnifType<'ast> for crate::label::Polarity {
    fn unif_type() -> UnifType<'ast> {
        mk_buty_enum!("Positive", "Negative")
    }
}

/// The typing context is a structure holding the scoped, environment-like data structures required
/// to perform typechecking.
///
#[derive(Debug, PartialEq, Clone)]
pub struct Context<'ast> {
    /// The typing environment.
    pub type_env: TypeEnv<'ast>,
    /// The term environment, used to decide type equality over contracts.
    pub term_env: TermEnv<'ast>,
    /// The current variable level, incremented each time we instantiate a polymorphic type and
    /// thus introduce a new block of variables (either unification variables or rigid type
    /// variables).
    pub var_level: VarLevel,
}

impl<'ast> Context<'ast> {
    pub fn new() -> Self {
        Context {
            type_env: TypeEnv::new(),
            term_env: TermEnv::new(),
            var_level: VarLevel::MIN_LEVEL,
        }
    }
}

impl<'ast> Default for Context<'ast> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
pub enum EnvBuildError<'ast> {
    NotARecord(Ast<'ast>),
}

/// Populate the initial typing environment from a `Vec` of parsed files.
pub fn mk_initial_ctxt<'ast>(
    ast_alloc: &'ast AstAlloc,
    initial_env: &[(nickel_stdlib::StdlibModule, Ast<'ast>)],
) -> Result<Context<'ast>, EnvBuildError<'ast>> {
    // Collect the bindings for each module, clone them and flatten the result to a single list.
    let mut bindings = Vec::new();

    for (module, ast) in initial_env {
        match (module, &ast.node) {
            // The internals module is special: it is required to be syntactically a record,
            // and is added directly to the top-level environment.
            (nickel_stdlib::StdlibModule::Internals, Node::Record(record)) => {
                // We reject fields without a value (that would be a stdlib module without
                // defintion). We also assume that the top-level modules of the stdlib aren't
                // defined piecewise, so that all path have length exactly one, and that those are
                // static.
                bindings.extend(record.field_defs.iter().map(|field_def| {
                    // unwrap(s)(): see assumptions above about the structure of the stdlib.
                    debug_assert!(
                        field_def.path.len() == 1,
                        "unexpected piecewise definition in stdlib internals module"
                    );

                    let id = field_def.path.first().unwrap().try_as_ident().unwrap();

                    (
                        id,
                        field_def
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
                return Err(EnvBuildError::NotARecord(ast.clone()));
            }
            // Otherwise, we insert a value in the environment bound to the name of the module
            (module, _) => bindings.push((module.name().into(), ast.clone())),
        }
    }

    let term_env = bindings
        .iter()
        .cloned()
        .map(|(id, ast)| (id.ident(), (ast, TermEnv::new())))
        .collect();

    let type_env = bindings
        .into_iter()
        .map(|(id, ast)| {
            (
                id.ident(),
                infer_record_type(ast_alloc, &ast, &term_env, INFER_RECORD_MAX_DEPTH),
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
pub fn env_add_term<'ast>(
    ast_alloc: &'ast AstAlloc,
    env: &mut TypeEnv<'ast>,
    ast: &Ast<'ast>,
    term_env: &TermEnv<'ast>,
    resolver: &dyn ImportResolver,
) -> Result<(), EnvBuildError<'ast>> {
    match &ast.node {
        Node::Record(record) => {
            for field_def in record.field_defs.iter() {
                if let Some(id) = field_def.path.first().unwrap().try_as_ident() {
                    let uty = UnifType::from_apparent_type(
                        field_def.apparent_type(ast_alloc, Some(env), Some(resolver)),
                        term_env,
                    );

                    env.insert(id.ident(), uty);
                }
            }

            Ok(())
        }
        _ => Err(EnvBuildError::NotARecord(ast.clone())),
    }
}

/// Bind one term in a typing environment.
pub fn env_add<'ast>(
    ast_alloc: &'ast AstAlloc,
    env: &mut TypeEnv<'ast>,
    id: LocIdent,
    ast: &Ast<'ast>,
    term_env: &TermEnv<'ast>,
    resolver: &dyn ImportResolver,
) {
    env.insert(
        id.ident(),
        UnifType::from_apparent_type(
            apparent_type(ast_alloc, &ast.node, Some(env), Some(resolver)),
            term_env,
        ),
    );
}

/// The shared state of unification.
pub struct State<'ast, 'tc> {
    /// The import resolver, to retrieve and typecheck imports.
    resolver: &'tc dyn ImportResolver,
    /// The unification table.
    table: &'tc mut UnifTable<'ast>,
    /// Row constraints.
    constr: &'tc mut RowConstrs,
    /// A mapping from unification variables or constants together with their
    /// kind to the name of the corresponding type variable which introduced it,
    /// if any.
    ///
    /// Used for error reporting.
    names: &'tc mut NameTable,
    /// A mapping from wildcard ID to unification variable.
    wildcard_vars: &'tc mut Vec<UnifType<'ast>>,
    /// The AST allocator.
    ast_alloc: &'ast AstAlloc,
}

/// Immutable and owned data, required by the LSP to carry out specific analysis.
/// It is basically an owned-subset of the typechecking state.
pub struct TypeTables<'ast> {
    pub table: UnifTable<'ast>,
    pub names: NameTable,
    pub wildcards: Vec<Type<'ast>>,
}

/// Typecheck a term.
///
/// Return the inferred type in case of success. This is just a wrapper that calls
/// `type_check_with_visitor` with a blanket implementation for the visitor.
///
/// Note that this function doesn't recursively typecheck imports (anymore), but just the current
/// file. It however still needs the resolver to get the apparent type of imports.
///
/// Return the type inferred for type wildcards.
pub fn type_check<'ast>(
    alloc: &'ast AstAlloc,
    ast: &Ast<'ast>,
    initial_ctxt: Context<'ast>,
    resolver: &impl ImportResolver,
    initial_mode: TypecheckMode,
) -> Result<Wildcards<'ast>, TypecheckError> {
    type_check_with_visitor(alloc, ast, initial_ctxt, resolver, &mut (), initial_mode)
        .map(|tables| tables.wildcards)
}

/// Typecheck a term while providing the type information to a visitor.
pub fn type_check_with_visitor<'ast, V>(
    ast_alloc: &'ast AstAlloc,
    ast: &Ast<'ast>,
    initial_ctxt: Context<'ast>,
    resolver: &impl ImportResolver,
    visitor: &mut V,
    initial_mode: TypecheckMode,
) -> Result<TypeTables<'ast>, TypecheckError>
where
    V: TypecheckVisitor<'ast>,
{
    let (mut table, mut names) = (UnifTable::new(), HashMap::new());
    let mut wildcard_vars = Vec::new();

    {
        let mut state = State {
            resolver,
            table: &mut table,
            constr: &mut RowConstrs::new(),
            names: &mut names,
            wildcard_vars: &mut wildcard_vars,
            ast_alloc,
        };

        if initial_mode == TypecheckMode::Enforce {
            let uty = state.table.fresh_type_uvar(initial_ctxt.var_level);
            check(&mut state, initial_ctxt, visitor, ast, uty)?;
        } else {
            walk(&mut state, initial_ctxt, visitor, ast)?;
        }
    }

    let wildcards = wildcard_vars_to_type(ast_alloc, wildcard_vars.clone(), &table);

    Ok(TypeTables {
        table,
        names,
        wildcards,
    })
}

/// Walk the AST of a term looking for statically typed block to check. Fill the linearization
/// alongside and store the apparent type of variable inside the typing environment.
fn walk<'ast, V: TypecheckVisitor<'ast>>(
    state: &mut State<'ast, '_>,
    mut ctxt: Context<'ast>,
    visitor: &mut V,
    ast: &Ast<'ast>,
) -> Result<(), TypecheckError> {
    let Ast { node, pos } = ast;

    visitor.visit_term(
        ast,
        UnifType::from_apparent_type(
            apparent_type(
                &state.ast_alloc,
                node,
                Some(&ctxt.type_env),
                Some(state.resolver),
            ),
            &ctxt.term_env,
        ),
    );

    match node {
        Node::ParseError(_)
        | Node::Null
        | Node::Bool(_)
        | Node::Number(_)
        | Node::String(_)
        | Node::EnumVariant {arg: None, ..}
        // This function doesn't recursively typecheck imports: this is the responsibility of the
        // caller.
        | Node::Import(_) => Ok(()),
        Node::Var(x) => ctxt.type_env
            .get(&x.ident())
            .ok_or(TypecheckError::UnboundIdentifier { id: *x, pos: *pos })
            .map(|_| ()),
        Node::StringChunks(chunks) => {
            for chunk in chunks.iter() {
                if let StringChunk::Expr(t, _) = chunk {
                    walk(state, ctxt.clone(), visitor, t)?;
                }
            }

            Ok(())
        }
        Node::Fun { args, body } => {
            // The parameter of an unannotated function is always assigned type `Dyn`, unless the
            // function is directly annotated with a function contract (see the special casing in
            // `walk_with_annot`).
            for arg in args.iter() {
                let PatternTypeData { bindings: pat_bindings, ..} = arg.pattern_types(state, &ctxt, TypecheckMode::Walk)?;
                ctxt.type_env.extend(pat_bindings.into_iter().map(|(id, typ)| (id.ident(), typ)));
            }

            walk(state, ctxt, visitor, body)
        }
        Node::Array(array) => {
            for elt in array.iter() {
                walk(state, ctxt.clone(), visitor, elt)?;
            }

            Ok(())
        }
        Node::Let { bindings, body, rec } => {
            // For a recursive let block, shadow all the names we're about to bind, so
            // we aren't influenced by variables defined in an outer scope.
            if *rec {
                for binding in bindings.iter() {
                    for pat_binding in binding.pattern.bindings() {
                        ctxt.type_env
                            .insert(pat_binding.id.ident(), mk_uniftype::dynamic());
                    }
                }
            }

            let start_ctxt = ctxt.clone();

            for binding in bindings.iter() {
                let ty_let = binding_type(state, &binding.value.node, &start_ctxt, false);

                // In the case of a let-binding, we want to guess a better type than `Dyn` when we can
                // do so cheaply for the whole pattern.
                if let Some(alias) = &binding.pattern.alias {
                    visitor.visit_ident(alias, ty_let.clone());
                    ctxt.type_env.insert(alias.ident(), ty_let);
                    ctxt.term_env.0.insert(alias.ident(), (binding.value.clone(), start_ctxt.term_env.clone()));
                }

                // [^separate-alias-treatment]: Note that we call `pattern_types` on the inner pattern
                // data, which doesn't take into account the potential heading alias `x @ <pattern>`.
                // This is on purpose, as the alias has been treated separately, so we don't want to
                // shadow it with a less precise type.
                //
                // The use of start_ctxt here looks like it might be wrong for let rec, but in fact
                // the context is unused in mode `TypecheckMode::Walk` anyway.
                let PatternTypeData {bindings: pat_bindings, ..} = binding.pattern.data.pattern_types(state, &start_ctxt, TypecheckMode::Walk)?;

                for (id, typ) in pat_bindings {
                    visitor.visit_ident(&id, typ.clone());
                    ctxt.type_env.insert(id.ident(), typ);
                    // [^term-env-rec-bindings]: we don't support recursive binding when checking
                    // for contract equality.
                    //
                    // This would quickly lead to `Rc` cycles, which are hard to deal with without
                    // leaking memory. The best way out would be to allocate all the term
                    // environments inside an arena, local to each statically typed block, and use
                    // bare references to represent cycles. Everything would be cleaned at the end
                    // of the block.
                    ctxt.term_env.0.insert(id.ident(), (binding.value.clone(), start_ctxt.term_env.clone()));
                }
            }

            let value_ctxt = if *rec { ctxt.clone() } else { start_ctxt.clone() };

            for binding in bindings.iter() {
                walk(state, value_ctxt.clone(), visitor, &binding.value)?;
            }

            walk(state, ctxt, visitor, body)
        }
        Node::App { head, args } => {
            walk(state, ctxt.clone(), visitor, head)?;

            for arg in args.iter() {
                walk(state, ctxt.clone(), visitor, arg)?;
            }

            Ok(())
        }
        Node::Match(match_data) => {
            for MatchBranch { pattern, guard, body } in match_data.branches.iter() {
                let mut local_ctxt = ctxt.clone();
                let PatternTypeData { bindings: pat_bindings, .. } = pattern.data.pattern_types(state, &ctxt, TypecheckMode::Walk)?;

                if let Some(alias) = &pattern.alias {
                    visitor.visit_ident(alias, mk_uniftype::dynamic());
                    local_ctxt.type_env.insert(alias.ident(), mk_uniftype::dynamic());
                }

                for (id, typ) in pat_bindings {
                    visitor.visit_ident(&id, typ.clone());
                    local_ctxt.type_env.insert(id.ident(), typ);
                }

                if let Some(guard) = guard {
                    walk(state, local_ctxt.clone(), visitor, guard)?;
                }

                walk(state, local_ctxt, visitor, body)?;
            }

            Ok(())
        }
        Node::IfThenElse {
            cond,
            then_branch,
            else_branch,
        } => {
            walk(state, ctxt.clone(), visitor, cond)?;
            walk(state, ctxt.clone(), visitor, then_branch)?;
            walk(state, ctxt, visitor, else_branch)
        }
        Node::Record(record) => {
            for field_def in record.field_defs.iter() {
                let field_type = field_type(
                    state,
                    field_def,
                    &ctxt,
                    false,
                );

                if let Some(id) = field_def.name_as_ident() {
                    ctxt.type_env.insert(id.ident(), field_type.clone());
                    visitor.visit_ident(&id, field_type);
                }
            }

            // Walk the type and contract annotations

            // We don't bind the fields in the term environment used to check for contract
            // equality. See [^term-env-rec-bindings].
            record.field_defs
                .iter()
                .try_for_each(|field_def| -> Result<(), TypecheckError> {
                    walk_field(state, ctxt.clone(), visitor, field_def)
                })
        }
        Node::EnumVariant { arg: Some(arg), ..} => walk(state, ctxt, visitor, arg),
        Node::PrimOpApp { args, .. } => {
            args.iter().try_for_each(|arg| -> Result<(), TypecheckError> {
                walk(state, ctxt.clone(), visitor, arg)
            })?;

            Ok(())
        }
        Node::Annotated { annot, inner } => {
            walk_annotated(state, ctxt, visitor, annot, inner)
        }
        Node::Type(typ) => walk_type(state, ctxt, visitor, typ),
   }
}

/// Same as [`walk`] but operate on a type, which can contain terms as contracts
/// ([crate::typ::TypeF::Contract]), instead of a term.
fn walk_type<'ast, V: TypecheckVisitor<'ast>>(
    state: &mut State<'ast, '_>,
    ctxt: Context<'ast>,
    visitor: &mut V,
    ty: &Type<'ast>,
) -> Result<(), TypecheckError> {
    match &ty.typ {
       TypeF::Dyn
       | TypeF::Number
       | TypeF::Bool
       | TypeF::String
       | TypeF::ForeignId
       | TypeF::Symbol
       // Currently, the parser can't generate unbound type variables by construction. Thus we
       // don't check here for unbound type variables again.
       | TypeF::Var(_)
       // An enum type can't contain a contract.
       // TODO: the assertion above isn't true anymore (ADTs). Need fixing?
       | TypeF::Enum(_)
       | TypeF::Wildcard(_) => Ok(()),
       TypeF::Arrow(ty1, ty2) => {
           walk_type(state, ctxt.clone(), visitor, ty1)?;
           walk_type(state, ctxt, visitor, ty2)
       }
       TypeF::Record(rrows) => walk_rrows(state, ctxt, visitor, rrows),
       TypeF::Contract(t) => walk(state, ctxt, visitor, t),
       TypeF::Dict { type_fields: ty2, .. }
       | TypeF::Array(ty2)
       | TypeF::Forall {body: ty2, ..} => walk_type(state, ctxt, visitor, ty2),
    }
}

/// Same as [`walk_type`] but operate on record rows.
fn walk_rrows<'ast, V: TypecheckVisitor<'ast>>(
    state: &mut State<'ast, '_>,
    ctxt: Context<'ast>,
    visitor: &mut V,
    rrows: &RecordRows<'ast>,
) -> Result<(), TypecheckError> {
    match rrows.0 {
        RecordRowsF::Empty
        // Currently, the parser can't generate unbound type variables by construction. Thus we
        // don't check here for unbound type variables again.
        | RecordRowsF::TailVar(_)
        | RecordRowsF::TailDyn => Ok(()),
        RecordRowsF::Extend { ref row, ref tail } => {
            walk_type(state, ctxt.clone(), visitor, &row.typ)?;
            walk_rrows(state, ctxt, visitor, tail)
        }
    }
}

fn walk_field<'ast, V: TypecheckVisitor<'ast>>(
    state: &mut State<'ast, '_>,
    ctxt: Context<'ast>,
    visitor: &mut V,
    field_def: &FieldDef<'ast>,
) -> Result<(), TypecheckError> {
    walk_with_annot(
        state,
        ctxt,
        visitor,
        &field_def.metadata.annotation,
        field_def.value.as_ref(),
    )
}

fn walk_annotated<'ast, V: TypecheckVisitor<'ast>>(
    state: &mut State<'ast, '_>,
    ctxt: Context<'ast>,
    visitor: &mut V,
    annot: &Annotation<'ast>,
    ast: &Ast<'ast>,
) -> Result<(), TypecheckError> {
    walk_with_annot(state, ctxt, visitor, annot, Some(ast))
}

/// Walk an annotated term, either via [crate::term::record::FieldMetadata], or via a standalone
/// type or contract annotation. A type annotation switches the typechecking mode to _enforce_.
fn walk_with_annot<'ast, V: TypecheckVisitor<'ast>>(
    state: &mut State<'ast, '_>,
    mut ctxt: Context<'ast>,
    visitor: &mut V,
    annot: &Annotation<'ast>,
    value: Option<&Ast<'ast>>,
) -> Result<(), TypecheckError> {
    annot
        .iter()
        .try_for_each(|ty| walk_type(state, ctxt.clone(), visitor, &ty))?;

    match (annot, value) {
        (Annotation { typ: Some(ty2), .. }, Some(value)) => {
            let uty2 = UnifType::from_type(ty2.clone(), &ctxt.term_env);
            check(state, ctxt, visitor, value, uty2)
        }
        (
            Annotation {
                typ: None,
                contracts,
            },
            Some(value),
        ) => {
            // If we see a function annotated with a function contract, we can get the type of the
            // arguments for free. We use this information both for typechecking (you could see it
            // as an extension of the philosophy of apparent types, but for function arguments
            // instead of let-bindings) and for the LSP, to provide better type information and
            // completion.
            if let Node::Fun { args, body } = value.node {
                // We look for the first contract of the list that is a function contract.
                let domains = contracts.iter().find_map(|c| {
                    if let TypeF::Arrow(mut domain, _) = &c.typ {
                        let mut domains =
                            vec![UnifType::from_type((*domain).clone(), &ctxt.term_env)];

                        while let TypeF::Arrow(next_domain, _) = &domain.typ {
                            domains.push(UnifType::from_type((*domain).clone(), &ctxt.term_env));
                            domain = next_domain;
                        }

                        Some(domains)
                    } else {
                        None
                    }
                });

                if let Some(domains) = domains {
                    for (arg, uty) in args.iter().zip(domains) {
                        // Because the normal code path in `walk` sets the function argument to `Dyn`,
                        // we need to short-circuit it. We manually visit the argument, augment the
                        // typing environment and walk the body of the function.
                        if let Some(id) = arg.try_as_any() {
                            visitor.visit_ident(&id, uty.clone());
                            ctxt.type_env.insert(id.ident(), uty);
                        }
                    }

                    return walk(state, ctxt, visitor, body);
                }
            }

            walk(state, ctxt, visitor, value)
        }
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
/// Following Pfenning's recipe (see [Bidirectional Typing][bidirectional-typing]), introduction
/// rules (e.g. typechecking a record) are checking. `check` follows the same logic here: it uses
/// unification to "match" on the expected type (for example in the case of records, a record type
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
/// - `visitor`: A visitor that can modify the linearization
/// - `t`: the term to check.
/// - `ty`: the type to check the term against.
///
/// # Linearization (LSP)
///
/// `check` is in charge of registering every term with the `visitor` and makes sure to scope
/// the visitor accordingly
///
/// [bidirectional-typing]: (https://arxiv.org/abs/1908.05839)
fn check<'ast, V: TypecheckVisitor<'ast>>(
    state: &mut State<'ast, '_>,
    mut ctxt: Context<'ast>,
    visitor: &mut V,
    ast: &Ast<'ast>,
    ty: UnifType<'ast>,
) -> Result<(), TypecheckError> {
    let Ast { node, pos } = ast;

    visitor.visit_term(ast, ty.clone());

    // When checking against a polymorphic type, we immediatly instantiate potential heading
    // foralls. Otherwise, this polymorphic type wouldn't unify much with other types. If we infer
    // a polymorphic type for `ast`, the subsumption rule will take care of instantiating this type
    // with unification variables, such that terms like `(fun x => x : forall a. a -> a) : forall
    // b. b -> b` typecheck correctly.
    let ty = instantiate_foralls(state, &mut ctxt, ty, ForallInst::Constant);

    match node {
        Node::ParseError(_) => Ok(()),
        // null is inferred to be of type Dyn
        Node::Null => ty
            .unify(mk_uniftype::dynamic(), state, &ctxt)
            .map_err(|err| err.into_typecheck_err(state, ast.pos)),
        Node::Bool(_) => ty
            .unify(mk_uniftype::bool(), state, &ctxt)
            .map_err(|err| err.into_typecheck_err(state, ast.pos)),
        Node::Number(_) => ty
            .unify(mk_uniftype::num(), state, &ctxt)
            .map_err(|err| err.into_typecheck_err(state, ast.pos)),
        Node::String(_) => ty
            .unify(mk_uniftype::str(), state, &ctxt)
            .map_err(|err| err.into_typecheck_err(state, ast.pos)),
        Node::StringChunks(chunks) => {
            ty.unify(mk_uniftype::str(), state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, ast.pos))?;

            chunks
                .iter()
                .try_for_each(|chunk| -> Result<(), TypecheckError> {
                    match chunk {
                        StringChunk::Literal(_) => Ok(()),
                        StringChunk::Expr(t, _) => {
                            check(state, ctxt.clone(), visitor, t, mk_uniftype::str())
                        }
                    }
                })
        }
        Node::IfThenElse {
            cond,
            then_branch,
            else_branch,
        } => {
            check(state, ctxt.clone(), visitor, cond, mk_uniftype::bool())?;
            check(state, ctxt.clone(), visitor, then_branch, ty.clone())?;
            check(state, ctxt, visitor, else_branch, ty)
        }
        // Fun is an introduction rule for the arrow type. The target type is thus expected to be
        // of the form `T1 -> ... -> Tn -> U`, which we enforce by unification. We then check the
        // body of the function against `U`, after adding the relevant argument types in the
        // environment.
        Node::Fun { args, body } => {
            let codomain = state.table.fresh_type_uvar(ctxt.var_level);
            let fun_type = args.iter().rev().try_fold(
                codomain.clone(),
                |fun_type, arg| -> Result<_, TypecheckError> {
                    // See [^separate-alias-treatment].
                    let pat_types = arg
                        .data
                        .pattern_types(state, &ctxt, TypecheckMode::Enforce)?;
                    // In the destructuring case, there's no alternative pattern, and we must thus
                    // immediately close all the row types.
                    pattern::close_all_enums(pat_types.enum_open_tails, state);
                    let arg_type = pat_types.typ;

                    if let Some(id) = arg.alias {
                        visitor.visit_ident(&id, arg_type.clone());
                        ctxt.type_env.insert(id.ident(), arg_type.clone());
                    }

                    for (id, typ) in pat_types.bindings {
                        visitor.visit_ident(&id, typ.clone());
                        ctxt.type_env.insert(id.ident(), typ);
                    }

                    Ok(mk_buty_arrow!(arg_type, fun_type))
                },
            )?;

            ty.unify(fun_type, state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, ast.pos))?;

            check(state, ctxt, visitor, body, codomain)
        }
        Node::Array(elts) => {
            let ty_elts = state.table.fresh_type_uvar(ctxt.var_level);

            ty.unify(mk_uniftype::array(ty_elts.clone()), state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, ast.pos))?;

            elts.iter()
                .try_for_each(|elt| -> Result<(), TypecheckError> {
                    check(state, ctxt.clone(), visitor, elt, ty_elts.clone())
                })
        }
        Node::Let {
            bindings,
            body,
            rec,
        } => {
            // For a recursive let block, shadow all the names we're about to bind, so
            // we aren't influenced by variables defined in an outer scope.
            if *rec {
                for binding in bindings.iter() {
                    for pat_binding in binding.pattern.bindings() {
                        ctxt.type_env.insert(
                            pat_binding.id.ident(),
                            state.table.fresh_type_uvar(ctxt.var_level),
                        );
                    }
                }
            }

            let start_ctxt = ctxt.clone();

            let typed_bindings: Result<Vec<_>, _> = bindings
                .iter()
                .map(|binding| -> Result<_, TypecheckError> {
                    // See [^separate-alias-treatment].
                    let pat_types = binding.pattern.pattern_types(
                        state,
                        &start_ctxt,
                        TypecheckMode::Enforce,
                    )?;

                    // In the destructuring case, there's no alternative pattern, and we must thus
                    // immediatly close all the row types.
                    pattern::close_all_enums(pat_types.enum_open_tails, state);

                    // The inferred type of the expr being bound
                    let ty_let = binding_type(state, &binding.value.node, &start_ctxt, true);

                    pat_types
                        .typ
                        .unify(ty_let.clone(), state, &start_ctxt)
                        .map_err(|e| e.into_typecheck_err(state, binding.value.pos))?;

                    if let Some(alias) = &binding.pattern.alias {
                        visitor.visit_ident(alias, ty_let.clone());
                        ctxt.type_env.insert(alias.ident(), ty_let.clone());
                        ctxt.term_env.0.insert(
                            alias.ident(),
                            (binding.value.clone(), start_ctxt.term_env.clone()),
                        );
                    }

                    for (id, typ) in pat_types.bindings {
                        visitor.visit_ident(&id, typ.clone());
                        ctxt.type_env.insert(id.ident(), typ);
                        // See [^term-env-rec-bindings] for why we use `start_ctxt` independently
                        // from `rec`.
                        ctxt.term_env.0.insert(
                            id.ident(),
                            (binding.value.clone(), start_ctxt.term_env.clone()),
                        );
                    }

                    Ok((&binding.value, ty_let))
                })
                .collect();

            let re_ctxt = if *rec { &ctxt } else { &start_ctxt };

            for (value, ty_let) in typed_bindings? {
                check(state, re_ctxt.clone(), visitor, value, ty_let)?;
            }

            check(state, ctxt, visitor, body, ty)
        }
        Node::Match(data) => {
            // [^typechecking-match-expression]: We can associate a type to each pattern of each
            // case of the match expression. From there, the type of a valid argument for the match
            // expression is ideally the union of each pattern type.
            //
            // For record types, we don't have a good way to express union: for example, what could
            // be the type of something that is either `{x : a}` or `{y : a}`? In the case of
            // record types, we thus just take the intersection of the types, which amounts to
            // unify all pattern types together. While it might fail most of the time (including
            // for the `{x}` and `{y}` example), it can still typecheck interesting expressions
            // when the record pattern are similar enough:
            //
            // ```nickel
            // x |> match {
            //  {foo, bar: 'Baz} => <branch1>
            //  {foo, bar: 'Qux} => <branch2>
            // }
            // ```
            //
            // We can definitely find a type for `x`: `{foo: a, bar: [| 'Baz, 'Qux |]}`.
            //
            // For enum types, we can express union: for example, the union of `[|'Foo, 'Bar|]` and
            // `[|'Bar, 'Baz|]` is `[|'Foo, 'Bar, 'Baz|]`. We can even turn this into a unification
            // problem: "open" the initial row types as `[| 'Foo, 'Bar; ?a |]` and `[|'Bar, 'Baz;
            // ?b |]`, unify them together, and close the result (unify the tail with an empty row
            // tail). The advantage of this approach is that unification takes care of descending
            // into record types and sub-patterns to perform this operation, and we're back to the
            // same procedure (almost) than for record patterns: simply unify all pattern types.
            // Although we have additional bookkeeping to perform (remember the tail variables
            // introduced to open enum rows and close the corresponding rows at the end of the
            // procedure).

            // We zip the pattern types with each branch
            let with_pat_types = data
                .branches
                .iter()
                .map(|branch| -> Result<_, TypecheckError> {
                    Ok((
                        branch,
                        branch
                            .pattern
                            .pattern_types(state, &ctxt, TypecheckMode::Enforce)?,
                    ))
                })
                .collect::<Result<Vec<(&MatchBranch, PatternTypeData<_>)>, _>>()?;

            // A match expression is a special kind of function. Thus it's typed as `a -> b`, where
            // `a` is a type determined by the patterns and `b` is the type of each match arm.
            let arg_type = state.table.fresh_type_uvar(ctxt.var_level);
            let return_type = state.table.fresh_type_uvar(ctxt.var_level);

            // Express the constraint that all the arms of the match expression should have a
            // compatible type and that each guard must be a boolean.
            for (
                MatchBranch {
                    pattern,
                    guard,
                    body,
                },
                pat_types,
            ) in with_pat_types.iter()
            {
                if let Some(alias) = &pattern.alias {
                    visitor.visit_ident(alias, return_type.clone());
                    ctxt.type_env.insert(alias.ident(), return_type.clone());
                }

                for (id, typ) in pat_types.bindings.iter() {
                    visitor.visit_ident(id, typ.clone());
                    ctxt.type_env.insert(id.ident(), typ.clone());
                }

                if let Some(guard) = guard {
                    check(state, ctxt.clone(), visitor, guard, mk_uniftype::bool())?;
                }

                check(state, ctxt.clone(), visitor, body, return_type.clone())?;
            }

            let pat_types = with_pat_types.into_iter().map(|(_, pat_types)| pat_types);

            // Unify all the pattern types with the argument's type, and build the list of all open
            // tail vars
            let mut enum_open_tails = Vec::with_capacity(
                pat_types
                    .clone()
                    .map(|pat_type| pat_type.enum_open_tails.len())
                    .sum(),
            );

            // Build the list of all wildcard pattern occurrences
            let mut wildcard_occurrences = HashSet::with_capacity(
                pat_types
                    .clone()
                    .map(|pat_type| pat_type.wildcard_occurrences.len())
                    .sum(),
            );

            // We don't immediately return if an error occurs while unifying the patterns together.
            // For error reporting purposes, it's best to first close the tail variables (if
            // needed), to avoid cluttering the reported types with free unification variables
            // which are mostly an artifact of our implementation of typechecking pattern matching.
            let pat_unif_result: Result<(), UnifError> =
                pat_types.into_iter().try_for_each(|pat_type| {
                    arg_type.clone().unify(pat_type.typ, state, &ctxt)?;

                    for (id, typ) in pat_type.bindings {
                        visitor.visit_ident(&id, typ.clone());
                        ctxt.type_env.insert(id.ident(), typ);
                    }

                    enum_open_tails.extend(pat_type.enum_open_tails);
                    wildcard_occurrences.extend(pat_type.wildcard_occurrences);

                    Ok(())
                });

            // Once we have accumulated all the information about enum rows and wildcard
            // occurrences, we can finally close the tails that need to be.
            pattern::close_enums(enum_open_tails, &wildcard_occurrences, state);

            // And finally fail if there was an error.
            pat_unif_result.map_err(|err| err.into_typecheck_err(state, ast.pos))?;

            // We unify the expected type of the match expression with `arg_type -> return_type`.
            //
            // This must happen last, or at least after having closed the tails: otherwise, the
            // enum type inferred for the argument could be unduly generalized. For example, take:
            //
            // ```
            // let exp : forall r. [| 'Foo; r |] -> Dyn = match { 'Foo => null }
            // ```
            //
            // This must not typecheck, as the match expression doesn't have a default case, and
            // its type is thus `[| 'Foo |] -> Dyn`. However, during the typechecking of the match
            // expression, before tails are closed, the working type is `[| 'Foo; _erows_a |]`,
            // which can definitely unify with `[| 'Foo; r |]` while the tail is still open. If we
            // close the tail first, then the type becomes [| 'Foo |] and the generalization fails
            // as desired.
            //
            // As a safety net, the tail closing code panics (in debug mode) if it finds a rigid
            // type variable at the end of the tail of a pattern type, which would happen if we
            // somehow generalized an enum row type variable before properly closing the tails
            // before.
            ty.unify(
                mk_buty_arrow!(arg_type.clone(), return_type.clone()),
                state,
                &ctxt,
            )
            .map_err(|err| err.into_typecheck_err(state, ast.pos))?;

            Ok(())
        }
        // Elimination forms (variable, function application and primitive operator application)
        // follow the inference discipline, following the Pfennig recipe and the current type
        // system specification (as far as typechecking is concerned, primitive operator
        // application is the same as function application).
        Node::Var(_) | Node::App { .. } | Node::PrimOpApp { .. } | Node::Annotated { .. } => {
            let inferred = infer(state, ctxt.clone(), visitor, ast)?;

            // We apply the subsumption rule when switching from infer mode to checking mode.
            inferred
                .subsumed_by(ty, state, ctxt)
                .map_err(|err| err.into_typecheck_err(state, ast.pos))
        }
        Node::EnumVariant { tag, arg: None } => {
            let row = state.table.fresh_erows_uvar(ctxt.var_level);
            ty.unify(mk_buty_enum!(*tag; row), state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, ast.pos))
        }
        Node::EnumVariant {
            tag,
            arg: Some(arg),
        } => {
            let tail = state.table.fresh_erows_uvar(ctxt.var_level);
            let ty_arg = state.table.fresh_type_uvar(ctxt.var_level);

            // We match the expected type against `[| 'id ty_arg; row_tail |]`, where `row_tail` is
            // a free unification variable, to ensure it has the right shape and extract the
            // components.
            ty.unify(mk_buty_enum!((*tag, ty_arg.clone()); tail), state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, ast.pos))?;

            // Once we have a type for the argument, we check the variant's data against it.
            check(state, ctxt, visitor, arg, ty_arg)
        }
        Node::Record(record) => record
            .resolve()
            .with_pos(*pos)
            .check(state, ctxt, visitor, ty),
        Node::Import(_) => todo!("need to figure out import resolution with the  new AST first"),
        // Node::Import(_) => ty
        //     .unify(mk_uniftype::dynamic(), state, &ctxt)
        //     .map_err(|err| err.into_typecheck_err(state, ast.pos)),
        // We use the apparent type of the import for checking. This function doesn't recursively
        // typecheck imports: this is the responsibility of the caller.
        // Term::ResolvedImport(file_id) => {
        //     let t = state
        //         .resolver
        //         .get(*file_id)
        //         .expect("Internal error: resolved import not found during typechecking.");
        //     let ty_import: UnifType<'ast> = UnifType::from_apparent_type(
        //         apparent_type(t.as_ref(), Some(&ctxt.type_env), Some(state.resolver)),
        //         &ctxt.term_env,
        //     );
        //     ty.unify(ty_import, state, &ctxt)
        //         .map_err(|err| err.into_typecheck_err(state, ast.pos))
        // }
        Node::Type(typ) => {
            if let Some(contract) = typ.find_contract() {
                todo!("needs to update `error::TypecheckError` first, but not ready to switch to the new typechecker yet")
                // Err(TypecheckError::CtrTypeInTermPos {
                //     contract,
                //     pos: *pos,
                // })
            } else {
                Ok(())
            }
        }
    }
}

fn check_field<'ast, V: TypecheckVisitor<'ast>>(
    state: &mut State<'ast, '_>,
    ctxt: Context<'ast>,
    visitor: &mut V,
    def: &FieldDef<'ast>,
    ty: UnifType<'ast>,
) -> Result<(), TypecheckError> {
    //unwrap(): a field path is always assumed to be non-empty
    let pos_id = def.path.last().unwrap().pos();

    // If there's no annotation, we simply check the underlying value, if any.
    if def.metadata.annotation.is_empty() {
        if let Some(value) = def.value.as_ref() {
            check(state, ctxt, visitor, value, ty)
        } else {
            // It might make sense to accept any type for a value without definition (which would
            // act a bit like a function parameter). But for now, we play safe and implement a more
            // restrictive rule, which is that a value without a definition has type `Dyn`
            ty.unify(mk_uniftype::dynamic(), state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, pos_id))
        }
    } else {
        let pos = def.value.as_ref().map(|v| v.pos).unwrap_or(pos_id);

        let inferred = infer_with_annot(
            state,
            ctxt.clone(),
            visitor,
            &def.metadata.annotation,
            def.value.as_ref(),
        )?;

        inferred
            .subsumed_by(ty, state, ctxt)
            .map_err(|err| err.into_typecheck_err(state, pos))
    }
}

fn infer_annotated<'ast, V: TypecheckVisitor<'ast>>(
    state: &mut State<'ast, '_>,
    ctxt: Context<'ast>,
    visitor: &mut V,
    annot: &Annotation<'ast>,
    ast: &Ast<'ast>,
) -> Result<UnifType<'ast>, TypecheckError> {
    infer_with_annot(state, ctxt, visitor, annot, Some(ast))
}

/// Function handling the common part of inferring the type of terms with type or contract
/// annotation, with or without definitions. This encompasses both standalone type annotation
/// (where `value` is always `Some(_)`) as well as field definitions (where `value` may or may not
/// be defined).
///
/// As for [check_visited] and [infer_visited], the additional `item_id` is provided when the term
/// has been added to the visitor before but can still benefit from updating its information
/// with the inferred type.
fn infer_with_annot<'ast, V: TypecheckVisitor<'ast>>(
    state: &mut State<'ast, '_>,
    ctxt: Context<'ast>,
    visitor: &mut V,
    annot: &Annotation<'ast>,
    value: Option<&Ast<'ast>>,
) -> Result<UnifType<'ast>, TypecheckError> {
    for ty in annot.iter() {
        walk_type(state, ctxt.clone(), visitor, ty)?;
    }

    match (annot, value) {
        (Annotation { typ: Some(ty2), .. }, Some(value)) => {
            let uty2 = UnifType::from_type(ty2.clone(), &ctxt.term_env);

            visitor.visit_term(value, uty2.clone());

            check(state, ctxt, visitor, value, uty2.clone())?;
            Ok(uty2)
        }
        // An annotation without a type but with a contract switches the typechecker back to walk
        // mode. If there are several contracts, we arbitrarily chose the first one as the apparent
        // type (the most precise type would be the intersection of all contracts, but Nickel's
        // type system doesn't feature intersection types).
        (
            Annotation {
                typ: None,
                contracts,
            },
            value_opt,
        ) if !contracts.is_empty() => {
            let ty2 = contracts.first().unwrap();
            let uty2 = UnifType::from_type(ty2.clone(), &ctxt.term_env);

            if let Some(value) = &value_opt {
                visitor.visit_term(value, uty2.clone());
            }

            // If there's an inner value, we have to walk it, as it may contain statically typed
            // blocks.
            if let Some(value) = value_opt {
                walk(state, ctxt, visitor, value)?;
            }

            Ok(uty2)
        }
        // A non-empty value without a type or a contract annotation is typechecked in the same way
        // as its inner value. This case should only happen for record fields, as the parser can't
        // produce an annotated term without an actual annotation. Still, such terms could be
        // produced programmatically, and aren't necessarily an issue.
        (_, Some(value)) => infer(state, ctxt, visitor, value),
        // An empty value is a record field without definition. We don't check anything, and infer
        // its type to be either the first annotation defined if any, or `Dyn` otherwise.
        // We can only hit this case for record fields.
        _ => {
            let inferred = annot
                .first()
                .map(|ty| UnifType::from_type(ty.clone(), &ctxt.term_env))
                .unwrap_or_else(mk_uniftype::dynamic);
            Ok(inferred)
        }
    }
}

/// Infer a type for an expression.
///
/// `infer` corresponds to the inference mode of bidirectional typechecking. Nickel uses a mix of
/// bidirectional typechecking and traditional ML-like unification.
fn infer<'ast, V: TypecheckVisitor<'ast>>(
    state: &mut State<'ast, '_>,
    mut ctxt: Context<'ast>,
    visitor: &mut V,
    ast: &Ast<'ast>,
) -> Result<UnifType<'ast>, TypecheckError> {
    let Ast { node, pos } = ast;

    match node {
        Node::Var(x) => {
            let x_ty = ctxt
                .type_env
                .get(&x.ident())
                .cloned()
                .ok_or(TypecheckError::UnboundIdentifier { id: *x, pos: *pos })?;

            visitor.visit_term(ast, x_ty.clone());

            Ok(x_ty)
        }
        // Theoretically, we need to instantiate the type of the head of the primop application,
        // that is, the primop itself. In practice,
        // [crate::bytecode::typecheck::operation::PrimOpType::primop_type] returns types that are
        // already instantiated with free unification variables, to save building a polymorphic
        // type that would be instantiated immediately. Thus, the type of a primop is currently
        // always monomorphic.
        Node::PrimOpApp { op, args } => {
            let (tys_args, ty_res) = op.primop_type(state, ctxt.var_level)?;

            visitor.visit_term(ast, ty_res.clone());

            for (ty_arg, arg) in tys_args.into_iter().zip(args.iter()) {
                check(state, ctxt.clone(), visitor, arg, ty_arg)?;
            }

            Ok(ty_res)
        }
        Node::App { head, args } => {
            // If we go the full Quick Look route (cf [quick-look] and the Nickel type system
            // specification), we will have a more advanced and specific rule to guess the
            // instantiation of the potentially polymorphic type of the head of the application.
            // Currently, we limit ourselves to predicative instantiation, and we can get away
            // with eagerly instantiating heading `foralls` with fresh unification variables.
            let head_poly = infer(state, ctxt.clone(), visitor, head)?;
            let head_type = instantiate_foralls(state, &mut ctxt, head_poly, ForallInst::UnifVar);

            let arg_types: Vec<_> =
                std::iter::repeat_with(|| state.table.fresh_type_uvar(ctxt.var_level))
                    .take(args.len())
                    .collect();
            let codomain = state.table.fresh_type_uvar(ctxt.var_level);
            let fun_type = mk_uniftype::nary_arrow(arg_types.clone(), codomain.clone());

            // "Match" the type of the head with `dom -> codom`
            fun_type
                .unify(head_type, state, &ctxt)
                .map_err(|err| err.into_typecheck_err(state, head.pos))?;

            visitor.visit_term(ast, codomain.clone());

            for (arg, arg_type) in args.iter().zip(arg_types.into_iter()) {
                check(state, ctxt.clone(), visitor, arg, arg_type)?;
            }

            Ok(codomain)
        }
        Node::Annotated { annot, inner } => infer_annotated(state, ctxt, visitor, annot, inner),
        _ => {
            // The remaining cases can't produce polymorphic types, and thus we can reuse the
            // checking code. Inferring the type for those rules is equivalent to checking against
            // a free unification variable. This saves use from duplicating all the remaining
            // cases.
            let inferred = state.table.fresh_type_uvar(ctxt.var_level);

            visitor.visit_term(ast, inferred.clone());

            check(state, ctxt, visitor, ast, inferred.clone())?;
            Ok(inferred.into_root(state.table))
        }
    }
}

/// Determine the type of a let-bound expression.
///
/// Call [`apparent_type`] to see if the binding is annotated. If it is, return this type as a
/// [`UnifType<'ast>`]. Otherwise:
///
/// - in walk mode, we won't (and possibly can't) infer the type of `bound_exp`: just return `Dyn`.
/// - in typecheck mode, we will typecheck `bound_exp`: return a new unification variable to be
///   associated to `bound_exp`.
///
/// As this function is always called in a context where an `ImportResolver` is present, expect it
/// passed in arguments.
///
/// If the annotated type contains any wildcard:
///
/// - in non strict mode, wildcards are assigned `Dyn`.
/// - in strict mode, the wildcard is typechecked, and we return the unification variable
///   corresponding to it.
fn binding_type<'ast>(
    state: &mut State<'ast, '_>,
    ast: &Node<'ast>,
    ctxt: &Context<'ast>,
    strict: bool,
) -> UnifType<'ast> {
    apparent_or_infer(
        state,
        apparent_type(
            &state.ast_alloc,
            ast,
            Some(&ctxt.type_env),
            Some(state.resolver),
        ),
        ctxt,
        strict,
    )
}

/// Same as `binding_type` but for record field definition.
fn field_type<'ast>(
    state: &mut State<'ast, '_>,
    field_def: &FieldDef<'ast>,
    ctxt: &Context<'ast>,
    strict: bool,
) -> UnifType<'ast> {
    apparent_or_infer(
        state,
        field_def.apparent_type(&state.ast_alloc, Some(&ctxt.type_env), Some(state.resolver)),
        ctxt,
        strict,
    )
}

/// Either returns the exact type annotation extracted as an apparent type, or return a fresh
/// unification variable, for the type to be inferred by the typechecker, in enforce mode.
///
/// In walk mode, returns the type as approximated by [`apparent_type`].
fn apparent_or_infer<'ast>(
    state: &mut State<'ast, '_>,
    aty: ApparentType<'ast>,
    ctxt: &Context<'ast>,
    strict: bool,
) -> UnifType<'ast> {
    match aty {
        ApparentType::Annotated(ty) if strict => {
            replace_wildcards_with_var(state.table, ctxt, state.wildcard_vars, ty)
        }
        ApparentType::Approximated(_) if strict => state.table.fresh_type_uvar(ctxt.var_level),
        ty_apt => UnifType::from_apparent_type(ty_apt, &ctxt.term_env),
    }
}

/// Substitute wildcards in a type for their unification variable.
fn replace_wildcards_with_var<'ast>(
    table: &mut UnifTable<'ast>,
    ctxt: &Context<'ast>,
    wildcard_vars: &mut Vec<UnifType<'ast>>,
    ty: Type<'ast>,
) -> UnifType<'ast> {
    fn replace_rrows<'ast>(
        table: &mut UnifTable<'ast>,
        ctxt: &Context<'ast>,
        wildcard_vars: &mut Vec<UnifType<'ast>>,
        rrows: RecordRows<'ast>,
    ) -> UnifRecordRows<'ast> {
        UnifRecordRows::concrete(rrows.0.map_state(
            |ty, (table, wildcard_vars)| {
                Box::new(replace_wildcards_with_var(
                    table,
                    ctxt,
                    wildcard_vars,
                    ty.clone(),
                ))
            },
            |rrows, (table, wildcard_vars)| {
                Box::new(replace_rrows(table, ctxt, wildcard_vars, rrows.clone()))
            },
            &mut (table, wildcard_vars),
        ))
    }

    fn replace_erows<'ast>(
        table: &mut UnifTable<'ast>,
        ctxt: &Context<'ast>,
        wildcard_vars: &mut Vec<UnifType<'ast>>,
        erows: EnumRows<'ast>,
    ) -> UnifEnumRows<'ast> {
        UnifEnumRows::concrete(erows.0.map_state(
            |ty, (table, wildcard_vars)| {
                Box::new(replace_wildcards_with_var(
                    table,
                    ctxt,
                    wildcard_vars,
                    ty.clone(),
                ))
            },
            |erows, (table, wildcard_vars)| {
                Box::new(replace_erows(table, ctxt, wildcard_vars, erows.clone()))
            },
            &mut (table, wildcard_vars),
        ))
    }

    match ty.typ {
        TypeF::Wildcard(i) => get_wildcard_var(table, ctxt.var_level, wildcard_vars, i),
        _ => UnifType::concrete(ty.typ.map_state(
            |ty, (table, wildcard_vars)| {
                Box::new(replace_wildcards_with_var(
                    table,
                    ctxt,
                    wildcard_vars,
                    ty.clone(),
                ))
            },
            |rrows, (table, wildcard_vars)| replace_rrows(table, ctxt, wildcard_vars, rrows),
            // Enum rows contain neither wildcards nor contracts
            |erows, (table, wildcard_vars)| replace_erows(table, ctxt, wildcard_vars, erows),
            |ctr, _| (ctr, ctxt.term_env.clone()),
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
pub enum ApparentType<'ast> {
    /// The apparent type is given by a user-provided annotation.
    Annotated(Type<'ast>),
    /// The apparent type has been inferred from a simple expression.
    Inferred(Type<'ast>),
    /// The term is a variable and its type was retrieved from the typing environment.
    FromEnv(UnifType<'ast>),
    /// The apparent type wasn't trivial to determine, and an approximation (most of the time,
    /// `Dyn`) has been returned.
    Approximated(Type<'ast>),
}

impl<'ast> TryConvert<'ast, ApparentType<'ast>> for Type<'ast> {
    type Error = std::convert::Infallible;

    fn try_convert(alloc: &'ast AstAlloc, at: ApparentType<'ast>) -> Result<Self, Self::Error> {
        Ok(match at {
            ApparentType::Annotated(ty) if has_wildcards(&ty) => Type::from(TypeF::Dyn),
            ApparentType::Annotated(ty)
            | ApparentType::Inferred(ty)
            | ApparentType::Approximated(ty) => ty,
            ApparentType::FromEnv(uty) => Type::try_convert(alloc, uty)
                .ok()
                .unwrap_or(Type::from(TypeF::Dyn)),
        })
    }
}

// Since there's already an enum named `ApparentType`, we can't use it as a trait name.
trait HasApparentType<'ast> {
    fn apparent_type(
        &self,
        ast_alloc: &'ast AstAlloc,
        env: Option<&TypeEnv<'ast>>,
        resolver: Option<&dyn ImportResolver>,
    ) -> ApparentType<'ast>;
}

impl<'ast> HasApparentType<'ast> for FieldDef<'ast> {
    // Return the apparent type of a field, by first looking at the type annotation, if any, then at
    // the contracts annotation, and if there is none, fall back to the apparent type of the value. If
    // there is no value, `Approximated(Dyn)` is returned.
    fn apparent_type(
        &self,
        ast_alloc: &'ast AstAlloc,
        env: Option<&TypeEnv<'ast>>,
        resolver: Option<&dyn ImportResolver>,
    ) -> ApparentType<'ast> {
        self.metadata
            .annotation
            .first()
            .cloned()
            .map(|ty| ApparentType::Annotated(ty))
            .or_else(|| {
                self.value
                    .as_ref()
                    .map(|v| apparent_type(ast_alloc, &v.node, env, resolver))
            })
            .unwrap_or(ApparentType::Approximated(Type::from(TypeF::Dyn)))
    }
}

/// Determine the apparent type of a let-bound expression.
///
/// When a let-binding `let x = bound_exp in body` is processed, the type of `bound_exp` must be
/// determined in order to be bound to the variable `x` in the typing environment.
/// Then, future occurrences of `x` can be given this type when used in a statically typed block.
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
pub fn apparent_type<'ast>(
    ast_alloc: &'ast AstAlloc,
    node: &Node<'ast>,
    env: Option<&TypeEnv<'ast>>,
    resolver: Option<&dyn ImportResolver>,
) -> ApparentType<'ast> {
    use crate::files::FileId;

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
    fn apparent_type_check_cycle<'ast>(
        ast_alloc: &'ast AstAlloc,
        node: &Node<'ast>,
        env: Option<&TypeEnv<'ast>>,
        resolver: Option<&dyn ImportResolver>,
        mut imports_seen: HashSet<FileId>,
    ) -> ApparentType<'ast> {
        match node {
            Node::Annotated { annot, inner } => annot
                .first()
                .map(|typ| ApparentType::Annotated(typ.clone()))
                .unwrap_or_else(|| apparent_type(ast_alloc, &inner.node, env, resolver)),
            Node::Number(_) => ApparentType::Inferred(Type::from(TypeF::Number)),
            Node::Bool(_) => ApparentType::Inferred(Type::from(TypeF::Bool)),
            Node::String(_) | Node::StringChunks(_) => {
                ApparentType::Inferred(Type::from(TypeF::String))
            }
            Node::Array(_) => ApparentType::Approximated(Type::from(TypeF::Array(
                ast_alloc.alloc(Type::from(TypeF::Dyn)),
            ))),
            Node::Var(id) => env
                .and_then(|envs| envs.get(&id.ident()).cloned())
                .map(ApparentType::FromEnv)
                .unwrap_or(ApparentType::Approximated(Type::from(TypeF::Dyn))),
            //TODO: import
            // Node::ResolvedImport(file_id) => match resolver {
            //     Some(r) if !imports_seen.contains(file_id) => {
            //         imports_seen.insert(*file_id);
            //
            //         let t = r
            //             .get(*file_id)
            //             .expect("Internal error: resolved import not found during typechecking.");
            //         apparent_type_check_cycle(&t.term, env, Some(r), imports_seen)
            //     }
            //     _ => ApparentType::Approximated(Type::from(TypeF::Dyn)),
            // },
            _ => ApparentType::Approximated(Type::from(TypeF::Dyn)),
        }
    }

    apparent_type_check_cycle(ast_alloc, node, env, resolver, HashSet::new())
}

/// Infer the type of a non-annotated record by recursing inside gathering the apparent type of the
/// fields. It's currently used essentially to type the stdlib.
///
/// # Parameters
///
/// - `ast`: the term to infer a type for
/// - `term_env`: the current term environment, used for contracts equality
/// - `max_depth`: the max recursion depth. `infer_record_type` descends into sub-records, as long
///   as it only encounters nested record literals. `max_depth` is used to control this behavior
///   and cap the work that `infer_record_type` might do.
///
/// # Preconditions
///
/// The recourd shouldn't have any dynamic fields. They are ignored anyway, so if the record has
/// some, the inferred type could be wrong.
pub fn infer_record_type<'ast>(
    ast_alloc: &'ast AstAlloc,
    ast: &Ast<'ast>,
    term_env: &TermEnv<'ast>,
    max_depth: u8,
) -> UnifType<'ast> {
    match &ast.node {
        Node::Record(record) if max_depth > 0 => UnifType::from(TypeF::Record(
            UnifRecordRows::concrete(record.field_defs.iter().fold(
                RecordRowsF::Empty,
                |rtype, field_def| {
                    if let Some(id) = field_def.path.first().unwrap().try_as_ident() {
                        let uty = match field_def.apparent_type(ast_alloc, None, None) {
                            ApparentType::Annotated(ty) => UnifType::from_type(ty, term_env),
                            ApparentType::FromEnv(uty) => uty,
                            // Since we haven't reached max_depth yet, and the type is only
                            // approximated, we try to recursively infer a better type.
                            ApparentType::Inferred(ty) | ApparentType::Approximated(ty) => {
                                field_def
                                    .value
                                    .as_ref()
                                    .map(|v| {
                                        infer_record_type(ast_alloc, v, term_env, max_depth - 1)
                                    })
                                    .unwrap_or(UnifType::from_type(ty, term_env))
                            }
                        };

                        RecordRowsF::Extend {
                            row: UnifRecordRow {
                                id,
                                typ: Box::new(uty),
                            },
                            tail: Box::new(rtype.into()),
                        }
                    } else {
                        rtype
                    }
                },
            )),
        )),
        node => UnifType::from_apparent_type(
            apparent_type(ast_alloc, node, None, None),
            &TermEnv::new(),
        ),
    }
}

/// Deeply check whether a type contains a wildcard.
fn has_wildcards(ty: &Type<'_>) -> bool {
    ty.find_map(&mut |ty: &Type| ty.typ.is_wildcard().then_some(()))
        .is_some()
}

/// Type of the parameter controlling instantiation of foralls.
///
/// See [`instantiate_foralls`].
#[derive(Copy, Clone, Debug, PartialEq)]
enum ForallInst {
    Constant,
    UnifVar,
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
fn instantiate_foralls<'ast>(
    state: &mut State<'ast, '_>,
    ctxt: &mut Context<'ast>,
    mut ty: UnifType<'ast>,
    inst: ForallInst,
) -> UnifType<'ast> {
    ty = ty.into_root(state.table);

    // We are instantiating a polymorphic type: it's precisely the place where we have to increment
    // the variable level, to prevent already existing unification variables to unify with the
    // rigid type variables introduced here.
    //
    // As this function can be called on monomorphic types, we only increment the level when we
    // really introduce a new block of rigid type variables.
    if matches!(
        ty,
        UnifType::Concrete {
            typ: TypeF::Forall { .. },
            ..
        }
    ) {
        ctxt.var_level.incr();
    }

    while let UnifType::Concrete {
        typ: TypeF::Forall {
            var,
            var_kind,
            body,
        },
        ..
    } = ty
    {
        let kind: VarKindDiscriminant = (&var_kind).into();

        match var_kind {
            VarKind::Type => {
                let fresh_uid = state.table.fresh_type_var_id(ctxt.var_level);
                let uvar = match inst {
                    ForallInst::Constant => UnifType::Constant(fresh_uid),
                    ForallInst::UnifVar => UnifType::UnifVar {
                        id: fresh_uid,
                        init_level: ctxt.var_level,
                    },
                };
                state.names.insert((fresh_uid, kind), var.ident());
                ty = body.subst(&var, &uvar);
            }
            VarKind::RecordRows { excluded } => {
                let fresh_uid = state.table.fresh_rrows_var_id(ctxt.var_level);
                let uvar = match inst {
                    ForallInst::Constant => UnifRecordRows::Constant(fresh_uid),
                    ForallInst::UnifVar => UnifRecordRows::UnifVar {
                        id: fresh_uid,
                        init_level: ctxt.var_level,
                    },
                };
                state.names.insert((fresh_uid, kind), var.ident());
                ty = body.subst(&var, &uvar);

                if inst == ForallInst::UnifVar {
                    state.constr.insert(fresh_uid, excluded);
                }
            }
            VarKind::EnumRows { excluded } => {
                let fresh_uid = state.table.fresh_erows_var_id(ctxt.var_level);
                let uvar = match inst {
                    ForallInst::Constant => UnifEnumRows::Constant(fresh_uid),
                    ForallInst::UnifVar => UnifEnumRows::UnifVar {
                        id: fresh_uid,
                        init_level: ctxt.var_level,
                    },
                };
                state.names.insert((fresh_uid, kind), var.ident());
                ty = body.subst(&var, &uvar);

                if inst == ForallInst::UnifVar {
                    state.constr.insert(fresh_uid, excluded);
                }
            }
        };
    }

    ty
}

/// Get the type unification variable associated with a given wildcard ID.
fn get_wildcard_var<'ast>(
    table: &mut UnifTable<'ast>,
    var_level: VarLevel,
    wildcard_vars: &mut Vec<UnifType<'ast>>,
    id: VarId,
) -> UnifType<'ast> {
    // If `id` is not in `wildcard_vars`, populate it with fresh vars up to `id`
    if id >= wildcard_vars.len() {
        wildcard_vars.extend((wildcard_vars.len()..=id).map(|_| table.fresh_type_uvar(var_level)));
    }
    wildcard_vars[id].clone()
}

/// Convert a mapping from wildcard ID to type var, into a mapping from wildcard ID to concrete
/// type.
fn wildcard_vars_to_type<'ast>(
    alloc: &'ast AstAlloc,
    wildcard_vars: Vec<UnifType<'ast>>,
    table: &UnifTable<'ast>,
) -> Wildcards<'ast> {
    wildcard_vars
        .into_iter()
        .map(|var| var.into_type(alloc, table))
        .collect()
}

/// A visitor trait for receiving callbacks during typechecking.
pub trait TypecheckVisitor<'ast> {
    /// Record the type of a term.
    ///
    /// It's possible for a single term to be visited multiple times, for example, if type
    /// inference kicks in.
    fn visit_term(&mut self, _ast: &Ast<'ast>, _ty: UnifType<'ast>) {}

    /// Record the type of a bound identifier.
    fn visit_ident(&mut self, _ident: &LocIdent, _new_type: UnifType<'ast>) {}
}

/// A do-nothing `TypeCheckVisitor` for when you don't want one.
impl<'ast> TypecheckVisitor<'ast> for () {}
