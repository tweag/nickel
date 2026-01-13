//! Nickel static types.
//!
//! The type system of Nickel is comprised of primitive types, arrays, records, functions, and
//! opaque types (contracts). This is a structural type system with row polymorphism for both
//! records and enums.
//!
//! ## Record types (rows)
//!
//! A row type for a record is represented as a linked list of pairs `(id, type)` indicating the
//! name and the type of each field. Row-polymorphism means that the tail of this list can be a
//! type variable which can be abstracted over, leaving the row open for future extension. A simple
//! illustration is record field access:
//!
//! ```nickel
//! let f : forall a. { some_field : Number; a} -> Number =
//!   fun record => record.some_field
//! ```
//!
//! The type `{ some_field: Number; a }` indicates that an argument to this function must have at
//! least the field `some_field` of type `Number`, but may contain other fields (or not).
//!
//! ## Dictionaries
//!
//! A dictionary type `{ _ : Type }` represents a record whose fields all have the type `Type`. The
//! count and the name of the fields aren't constrained. Dictionaries can be mapped over, extended,
//! shrinked and accessed in a type-safe manner.
//!
//! # Enum types
//!
//! An enum type is also a row type where each element is a tag, such as `[| 'foo, 'bar, 'baz |]`.
//! This type represent values that can be either `'foo`, `'bar` or `'baz`. Enums support row
//! polymorphism as well.
//!
//! # Contracts
//!
//! To each type corresponds a contract, which is equivalent to a Nickel function which checks at
//! runtime that its argument is of the given type. Contract checks are introduced by a contract
//! annotation or propagated via merging. They ensure sane interaction between typed and untyped
//! parts.
//!
//! Conversely, any Nickel term seen as a contract corresponds to a type, which is opaque and can
//! only be equated with itself.

// Some doc links point to, e.g. Type::traverse. rustdoc doesn't support fully-qualified syntax,
// so this import was the only way I could figure out to have `Type::traverse` in scope.
#[cfg(doc)]
use crate::traverse::TraverseAlloc;

use crate::{
    error::{ParseError, ParseErrors},
    identifier::{Ident, LocIdent},
};

use std::{collections::HashSet, convert::Infallible};

/// A record row, mapping an identifier to a type. A record type is a dictionary mapping
/// identifiers to Nickel type. Record types are represented as sequences of `RecordRowF`, ending
/// potentially with a type variable or `Dyn` in tail position.
///
/// # Type parameters
///
/// As other types with the `F` suffix, this type is parametrized by one or more recursive
/// unfoldings (here, `Ty` for `TypeF`). See [`TypeF`] for more details.
#[derive(Clone, PartialEq, Eq, Debug, rkyv::Archive)]
pub struct RecordRowF<Ty> {
    pub id: LocIdent,
    pub typ: Ty,
}

/// An enum row, mapping an identifier to an optional type. If the associated type is `None`, this
/// represent a bare (unapplied) enum tag such as `'foo`. If the enum is applied (a variant), the
/// type is store in `typ`. An enum type is a set of enum rows, represented as a sequence of
/// `EnumRow`s, ending potentially with a type variable tail position.
///
/// # Type parameters
///
/// As other types with the `F` suffix, this type is parametrized by one or more recursive
/// unfoldings (here, `Ty` for `TypeF`). See [`TypeF`] for more details.
#[derive(Clone, PartialEq, Eq, Debug, rkyv::Archive)]
pub struct EnumRowF<Ty> {
    pub id: LocIdent,
    pub typ: Option<Ty>,
}

/// Generic sequence of record rows potentially with a type variable or `Dyn` in tail position.
///
/// As other types with the `F` suffix, this type is parametrized by one or more recursive
/// unfoldings. See [`TypeF`] for more details.
///
/// # Type parameters
///
/// - `Ty` is the recursive unfolding of a Nickel type stored inside one row. In practice, a
///   wrapper around an instantiation of `TypeF`.
/// - `RRows` is the recursive unfolding of record rows (the tail of this row sequence). In
///   practice, a wrapper around an instantiation of `RecordRowsF`.
#[derive(Clone, PartialEq, Eq, Debug, rkyv::Archive)]
pub enum RecordRowsF<Ty, RRows> {
    Empty,
    Extend { row: RecordRowF<Ty>, tail: RRows },
    TailVar(LocIdent),
    TailDyn,
}

/// Generic sequence of enum rows potentially with a type variable in tail position.
///
/// As other types with the `F` suffix, this type is parametrized by one or more recursive
/// unfoldings. See [`TypeF`] for more details.
///
/// # Type parameters
///
/// - `ERows` is the recursive unfolding of enum rows (the tail of this row sequence). In practice,
///   a wrapper around `EnumRowsF`.
#[derive(Clone, PartialEq, Eq, Debug, rkyv::Archive)]
pub enum EnumRowsF<Ty, ERows> {
    Empty,
    Extend { row: EnumRowF<Ty>, tail: ERows },
    TailVar(LocIdent),
}

/// The kind of a quantified type variable.
///
/// Nickel uses several forms of polymorphism. A type variable can be substituted for a type, as in
/// `id : forall a. a -> a`, for record rows as in `access_foo : forall a . {foo : Number; a} ->
/// Number}`, or for enum rows. This information is implicit in the source syntax: we don't require
/// users to write e.g. `forall a :: Type` or `forall a :: Rows`. But the kind of a variable is
/// required for the typechecker. It is thus determined during parsing and stored as `VarKind` where
/// type variables are introduced, that is, on forall quantifiers.
#[derive(Clone, PartialEq, Eq, Debug, Default, rkyv::Archive)]
pub enum VarKind {
    #[default]
    Type,
    /// `excluded` keeps track of which rows appear somewhere alongside the tail, and therefore
    /// cannot appear in the tail. For instance `forall r. { ; r } -> { x : Number ; r }` assumes
    EnumRows { excluded: HashSet<Ident> },
    /// Same as for [Self::EnumRows].
    RecordRows { excluded: HashSet<Ident> },
}

/// Equivalent to `std::mem::Discriminant<VarKind>`, but we can do things like match on it
// TODO: this seems overly complicated, and it's anyways more space-efficient to store the
// `excluded` information separately like we do in the `State` field constr. Probably we can store
// it that way during parsing too.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, rkyv::Archive)]
pub enum VarKindDiscriminant {
    Type,
    EnumRows,
    RecordRows,
}

impl From<&VarKind> for VarKindDiscriminant {
    fn from(vk: &VarKind) -> Self {
        match vk {
            VarKind::Type => VarKindDiscriminant::Type,
            VarKind::EnumRows { .. } => VarKindDiscriminant::EnumRows,
            VarKind::RecordRows { .. } => VarKindDiscriminant::RecordRows,
        }
    }
}

/// Flavour of a dictionary type. There are currently two way of writing a dictionary type-ish
/// object: as a dictionary contract `{_ | T}` or as a dictionary type `{_ : T}`. Ideally, the
/// former wouldn't even be a type but mostly syntactic sugar for a builtin contract application,
/// or maybe a proper AST node.
///
/// However, the LSP needs to handle both dictionary types and contracts specifically in order to
/// provide good completion. As we added dictionary contract just before 1.0 to fix a non trivial
/// issue with respect to polymorphic contracts ([GitHub
/// issue](https://github.com/tweag/nickel/issues/1228)), the solution to just tweak dictionary
/// types to be able to hold both kinds - generating a different contract  - seemed to be the
/// simplest to preserve the user experience (LSP, handling of dictionary when reporting a contract
/// blame, etc.).
///
/// Dictionary contracts might get a proper AST node later on.
#[derive(Clone, Debug, Copy, Eq, PartialEq, rkyv::Archive)]
pub enum DictTypeFlavour {
    /// Dictionary type (`{_ : T}`)
    Type,
    /// Dictionary contract (`{_ | T}`)
    Contract,
}

/// A Nickel type.
///
/// # Generic representation (functor)
///
/// A Nickel type is represented by a tree and is naturally defined in a recursive manner: for
/// example, one would expect the constructor for function types (`Arrow`) to look like:
///
/// ```rust
/// pub enum Type {
///      Arrow(Box<Type>, Box<Type>),
///      // ...
/// }
/// ```
///
/// However, `TypeF` is slightly different, in that it is parametrized by a generic type instead of
/// using a concrete definition like `Box<Types>` (forget about rows for now):
///
/// ```rust
/// pub enum TypeF<Ty /* , .. */> {
///      Arrow(Ty, Ty),
///      // ...
/// }
/// ```
///
/// `Ty` is also called a recursive unfolding throughout the documentation. By defining `struct
/// Type(TypeF<Box<Types>>)`, we get back the original, natural definition.
///
/// ## Motivation 1: variation on `Types`
///
/// Having a generic definition makes it possible to easily create other types with the _same shape_
/// as `Type` (seen as trees), but with enriched nodes. The typical use-case in Nickel is the
/// variation on types used by the typechecker. During type inference, the typechecker operates on
/// trees where each node can either be a concrete type, or a unification variable (a unknown type
/// to be inferred). Instead of duplicating the whole definition of `Type` as well as all basic
/// methods, we can simply have a different recursive definition:
///
/// ```
/// # // phony declarations to make this example pass the tests
/// # type VarId = ();
/// # type TypeF<T> = T;
///
/// pub enum UnifType {
///    UnifVar(VarId),
///    Concrete(TypeF<Box<UnifType> /*, .. */>),
///    // ..
///  }
/// ```
///
/// We get something that looks like normal Nickel types, except that each node can also be a
/// unification variable as well.
///
/// ## Motivation 2: recursion schemes
///
/// This definition is actually in the style of recursion schemes. Pedantically, `TypeF` (hence the
/// `F` suffix) is a functor, but the formal details aren't so important: keep in mind that the `F`
/// suffix means that the recursive occurrences of subtrees (and enum rows and record rows as well)
/// are replaced by generic parameters.
///
/// The usual motivation for recursion schemes is that they allow for elegant and simple definitions
/// of recursive transformation over trees (here, `TypeF`, and more generally anything with an `F`
/// suffix) in terms of simple appropriate chaining of `map` and folding/unfolding operations. A
/// good example is the definition of [crate::ast::typ::Type::traverse]. Although [crate::ast::Node] isn't currently
/// defined using functors per se, the way program transformations are written is in the same style
/// as recursion schemes: we simply define the action of a transformation as a mapping on the
/// current node, and let the traversal take care of the plumbing of recursion and reconstruction.
///
/// ## Type parameters
///
/// - `Ty`: the recursive unfolding of Nickel types
/// - `RRows`: the recursive unfolding of record rows
/// - `ERows`: the recursive unfolding of enum rows
/// - `Te`: the type of a term (used to store contracts)
#[derive(Clone, PartialEq, Eq, Debug, rkyv::Archive)]
pub enum TypeF<Ty, RRows, ERows, Te> {
    /// The dynamic type, or unitype. Assigned to values whose actual type is not statically known
    /// or checked.
    Dyn,
    /// A floating point number.
    Number,
    /// A boolean.
    Bool,
    /// A string literal.
    String,
    /// A symbol.
    ///
    /// See `Term::Sealed` in `nickel_lang_core`.
    Symbol,
    /// The type of `Term::ForeignId`.
    ForeignId,
    /// A type created from a user-defined contract.
    Contract(Te),
    /// A function.
    Arrow(Ty, Ty),
    /// A type variable.
    Var(Ident),
    /// A forall binder.
    Forall {
        var: LocIdent,
        var_kind: VarKind,
        body: Ty,
    },

    /// An enum type, composed of a sequence of enum rows.
    Enum(ERows),
    /// A record type, composed of a sequence of record rows.
    Record(RRows),
    /// A dictionary type.
    Dict {
        type_fields: Ty,
        flavour: DictTypeFlavour,
    },
    /// A parametrized array.
    Array(Ty),
    /// A type wildcard, wrapping an ID unique within a given file.
    Wildcard(usize),
}

impl<Ty, RRows> RecordRowsF<Ty, RRows> {
    /// Map functions over the children nodes of record rows, when seen as a tree. The mutable
    /// state ( `S`) is threaded through the calls to the mapped functions. Functions are fallible
    /// and may return an error `E`, which causes `try_map_state` to return early with the same
    /// error.
    ///
    /// If we put aside the state and the error (see [RecordRowsF::map), this function makes
    /// `RecordRowsF` a functor (of arity 2). As hinted by the type signature, this function just
    /// maps on "one-level" of recursion, so to speak. Take the instantiated version `RecordRows`,
    /// and record rows of the form `{foo : T, bar: U, baz: V}`. Then, calling `try_map_state(f_ty,
    /// f_rrows, state)` on these rows will map `f_ty` onto `T` and `f_rrows` onto `{bar : U, baz:
    /// V}`.
    ///
    /// Note that `f_ty` isn't mapped onto `U` and `V` recursively: map isn't a recursive
    /// operation. It's however a building block to express recursive operations: as an example,
    /// see [crate::ast::typ::RecordRows::traverse].
    pub fn try_map_state<TyO, RRowsO, FTy, FRRows, S, E>(
        self,
        mut f_ty: FTy,
        mut f_rrows: FRRows,
        state: &mut S,
    ) -> Result<RecordRowsF<TyO, RRowsO>, E>
    where
        FTy: FnMut(Ty, &mut S) -> Result<TyO, E>,
        FRRows: FnMut(RRows, &mut S) -> Result<RRowsO, E>,
    {
        match self {
            RecordRowsF::Empty => Ok(RecordRowsF::Empty),
            RecordRowsF::Extend {
                row: RecordRowF { id, typ },
                tail,
            } => Ok(RecordRowsF::Extend {
                row: RecordRowF {
                    id,
                    typ: f_ty(typ, state)?,
                },
                tail: f_rrows(tail, state)?,
            }),
            RecordRowsF::TailDyn => Ok(RecordRowsF::TailDyn),
            RecordRowsF::TailVar(id) => Ok(RecordRowsF::TailVar(id)),
        }
    }

    /// Variant of `try_map_state` without threaded state.
    pub fn try_map<TyO, RRowsO, FTy, FRRows, E>(
        self,
        mut f_ty: FTy,
        mut f_rrows: FRRows,
    ) -> Result<RecordRowsF<TyO, RRowsO>, E>
    where
        FTy: FnMut(Ty) -> Result<TyO, E>,
        FRRows: FnMut(RRows) -> Result<RRowsO, E>,
    {
        let f_ty_lifted = |rrow: Ty, _: &mut ()| -> Result<TyO, E> { f_ty(rrow) };
        let f_rrows_lifted = |rrows: RRows, _: &mut ()| -> Result<RRowsO, E> { f_rrows(rrows) };

        self.try_map_state(f_ty_lifted, f_rrows_lifted, &mut ())
    }

    /// Variant of `try_map_state` with infallible functions.
    pub fn map_state<TyO, RRowsO, FTy, FRRows, S>(
        self,
        mut f_ty: FTy,
        mut f_rrows: FRRows,
        state: &mut S,
    ) -> RecordRowsF<TyO, RRowsO>
    where
        FTy: FnMut(Ty, &mut S) -> TyO,
        FRRows: FnMut(RRows, &mut S) -> RRowsO,
    {
        let f_ty_lifted = |rrow: Ty, state: &mut S| -> Result<TyO, ()> { Ok(f_ty(rrow, state)) };
        let f_rrows_lifted =
            |rrows: RRows, state: &mut S| -> Result<RRowsO, ()> { Ok(f_rrows(rrows, state)) };

        self.try_map_state(f_ty_lifted, f_rrows_lifted, state)
            .unwrap()
    }

    /// Variant of `try_map_state` without threaded state and with infallible functions.
    pub fn map<TyO, RRowsO, FTy, FRRows>(
        self,
        mut f_ty: FTy,
        mut f_rrows: FRRows,
    ) -> RecordRowsF<TyO, RRowsO>
    where
        FTy: FnMut(Ty) -> TyO,
        FRRows: FnMut(RRows) -> RRowsO,
    {
        let f_ty_lifted = |rrow: Ty| -> Result<TyO, Infallible> { Ok(f_ty(rrow)) };
        let f_rrows_lifted = |rrows: RRows| -> Result<RRowsO, Infallible> { Ok(f_rrows(rrows)) };
        self.try_map(f_ty_lifted, f_rrows_lifted).unwrap()
    }
}

impl<Ty, ERows> EnumRowsF<Ty, ERows> {
    /// Map functions over the tail of enum rows. The mutable state ( `S`) is threaded through the
    /// calls to the mapped function. The function is fallible and may return an error `E`, which
    /// causes `try_map_state` to return early with the same error.
    ///
    /// If we put aside the state and the error (see [EnumRowsF::map), this function makes
    /// `EnumRowsF` a functor. As hinted by the type signature, this function just maps on
    /// "one-level" of recursion, so to speak. Take the instantiated version `EnumRows`, and
    /// enum rows of the form ``[| 'foo, 'bar, 'baz |]``. Then, calling `try_map_state(f_erows,
    /// state)` on these rows will map `f_erows` onto ``[| 'bar, 'baz |]``.
    ///
    /// Note that `f_erows` is just mapped once. Map isn't a recursive operation. It's however a
    /// building block to express recursive operations: as an example, see
    /// [crate::ast::typ::RecordRows::traverse].
    pub fn try_map_state<TyO, ERowsO, FTy, FERows, S, E>(
        self,
        mut f_ty: FTy,
        f_erows: FERows,
        state: &mut S,
    ) -> Result<EnumRowsF<TyO, ERowsO>, E>
    where
        FTy: FnMut(Ty, &mut S) -> Result<TyO, E>,
        FERows: FnOnce(ERows, &mut S) -> Result<ERowsO, E>,
    {
        match self {
            EnumRowsF::Empty => Ok(EnumRowsF::Empty),
            EnumRowsF::Extend {
                row: EnumRowF { id, typ },
                tail,
            } => Ok(EnumRowsF::Extend {
                row: EnumRowF {
                    id,
                    typ: typ.map(|ty| f_ty(ty, state)).transpose()?,
                },
                tail: f_erows(tail, state)?,
            }),
            EnumRowsF::TailVar(id) => Ok(EnumRowsF::TailVar(id)),
        }
    }

    /// Variant of `try_map_state` without threaded state.
    pub fn try_map<TyO, ERowsO, FTy, FERows, E>(
        self,
        mut f_ty: FTy,
        mut f_erows: FERows,
    ) -> Result<EnumRowsF<TyO, ERowsO>, E>
    where
        FTy: FnMut(Ty) -> Result<TyO, E>,
        FERows: FnMut(ERows) -> Result<ERowsO, E>,
    {
        let f_ty_lifted = |erow: Ty, _: &mut ()| -> Result<TyO, E> { f_ty(erow) };
        let f_erows_lifted = |erows: ERows, _: &mut ()| -> Result<ERowsO, E> { f_erows(erows) };

        self.try_map_state(f_ty_lifted, f_erows_lifted, &mut ())
    }

    /// Variant of `try_map_state` with infallible functions.
    pub fn map_state<TyO, ERowsO, FTy, FERows, S>(
        self,
        mut f_ty: FTy,
        mut f_erows: FERows,
        state: &mut S,
    ) -> EnumRowsF<TyO, ERowsO>
    where
        FTy: FnMut(Ty, &mut S) -> TyO,
        FERows: FnMut(ERows, &mut S) -> ERowsO,
    {
        let f_ty_lifted = |erow: Ty, state: &mut S| -> Result<TyO, ()> { Ok(f_ty(erow, state)) };
        let f_erows_lifted =
            |erows: ERows, state: &mut S| -> Result<ERowsO, ()> { Ok(f_erows(erows, state)) };
        self.try_map_state(f_ty_lifted, f_erows_lifted, state)
            .unwrap()
    }

    /// Variant of `try_map_state` without threaded state and with infallible functions.
    pub fn map<TyO, ERowsO, FTy, FERows>(
        self,
        mut f_ty: FTy,
        mut f_erows: FERows,
    ) -> EnumRowsF<TyO, ERowsO>
    where
        FTy: FnMut(Ty) -> TyO,
        FERows: FnMut(ERows) -> ERowsO,
    {
        let f_ty_lifted = |erow: Ty| -> Result<TyO, Infallible> { Ok(f_ty(erow)) };
        let f_erows_lifted = |erows: ERows| -> Result<ERowsO, Infallible> { Ok(f_erows(erows)) };

        self.try_map(f_ty_lifted, f_erows_lifted).unwrap()
    }
}

impl<Ty, RRows, ERows, Te> TypeF<Ty, RRows, ERows, Te> {
    /// Map functions over the children nodes of a type, when seen as a tree. The mutable state (
    /// `S`) is threaded through the calls to the mapped functions. Functions are fallible and may
    /// return an error `E`, which causes `try_map_state` to return early with the same error.
    ///
    /// If we put aside the state and the error (see [RecordRowsF::map), this function makes
    /// `TypeF` a functor (of arity 3). As hinted by the type signature, this function just
    /// maps on "one-level" of recursion, so to speak.
    ///
    /// Take the instantiated version `Type`, and a type of the form `(Dyn -> Dyn) -> (Number ->
    /// Dyn)`. Then, calling `try_map_state(f_ty, ..)` on this type rows will map `f_ty` onto `(Dyn
    /// -> Dyn)` and `Number -> Dyn` because they are direct children of the root `Arrow` node.
    ///
    /// Note that `f_ty` isn't mapped onto `Dyn` and `Number` recursively: map isn't a recursive
    /// operation. It's however a building block to express recursive operations: as an example,
    /// see [crate::ast::typ::RecordRows::traverse].
    ///
    /// Since `TypeF` may contain record rows and enum rows as well, `f_rrows` and `f_erows` are
    /// required to know how to map on record and enum types respectively.
    pub fn try_map_state<TyO, RRowsO, ERowsO, TeO, FTy, FRRows, FERows, FTe, S, E>(
        self,
        mut f: FTy,
        mut f_rrows: FRRows,
        mut f_erows: FERows,
        mut f_ctr: FTe,
        state: &mut S,
    ) -> Result<TypeF<TyO, RRowsO, ERowsO, TeO>, E>
    where
        FTy: FnMut(Ty, &mut S) -> Result<TyO, E>,
        FRRows: FnMut(RRows, &mut S) -> Result<RRowsO, E>,
        FERows: FnMut(ERows, &mut S) -> Result<ERowsO, E>,
        FTe: FnMut(Te, &mut S) -> Result<TeO, E>,
    {
        match self {
            TypeF::Dyn => Ok(TypeF::Dyn),
            TypeF::Number => Ok(TypeF::Number),
            TypeF::Bool => Ok(TypeF::Bool),
            TypeF::String => Ok(TypeF::String),
            TypeF::ForeignId => Ok(TypeF::ForeignId),
            TypeF::Symbol => Ok(TypeF::Symbol),
            TypeF::Contract(t) => Ok(TypeF::Contract(f_ctr(t, state)?)),
            TypeF::Arrow(dom, codom) => Ok(TypeF::Arrow(f(dom, state)?, f(codom, state)?)),
            TypeF::Var(i) => Ok(TypeF::Var(i)),
            TypeF::Forall {
                var,
                var_kind,
                body,
            } => Ok(TypeF::Forall {
                var,
                var_kind,
                body: f(body, state)?,
            }),
            TypeF::Enum(erows) => Ok(TypeF::Enum(f_erows(erows, state)?)),
            TypeF::Record(rrows) => Ok(TypeF::Record(f_rrows(rrows, state)?)),
            TypeF::Dict {
                type_fields,
                flavour: attrs,
            } => Ok(TypeF::Dict {
                type_fields: f(type_fields, state)?,
                flavour: attrs,
            }),
            TypeF::Array(t) => Ok(TypeF::Array(f(t, state)?)),
            TypeF::Wildcard(i) => Ok(TypeF::Wildcard(i)),
        }
    }

    /// Variant of `try_map_state` without threaded state.
    pub fn try_map<TyO, RRowsO, ERowsO, TeO, FTy, FRRows, FERows, FTe, E>(
        self,
        mut f: FTy,
        mut f_rrows: FRRows,
        mut f_erows: FERows,
        mut f_ctr: FTe,
    ) -> Result<TypeF<TyO, RRowsO, ERowsO, TeO>, E>
    where
        FTy: FnMut(Ty) -> Result<TyO, E>,
        FRRows: FnMut(RRows) -> Result<RRowsO, E>,
        FERows: FnMut(ERows) -> Result<ERowsO, E>,
        FTe: FnMut(Te) -> Result<TeO, E>,
    {
        let f_lifted = |ty: Ty, _: &mut ()| -> Result<TyO, E> { f(ty) };
        let f_rrows_lifted = |rrows: RRows, _: &mut ()| -> Result<RRowsO, E> { f_rrows(rrows) };
        let f_erows_lifted = |erows: ERows, _: &mut ()| -> Result<ERowsO, E> { f_erows(erows) };
        let f_ctr_lifted = |ctr: Te, _: &mut ()| -> Result<TeO, E> { f_ctr(ctr) };

        self.try_map_state(
            f_lifted,
            f_rrows_lifted,
            f_erows_lifted,
            f_ctr_lifted,
            &mut (),
        )
    }

    /// Variant of `try_map_state` with infallible functions.
    pub fn map_state<TyO, RRowsO, ERowsO, TeO, FTy, FRRows, FERows, FTe, S>(
        self,
        mut f: FTy,
        mut f_rrows: FRRows,
        mut f_erows: FERows,
        mut f_ctr: FTe,
        state: &mut S,
    ) -> TypeF<TyO, RRowsO, ERowsO, TeO>
    where
        FTy: FnMut(Ty, &mut S) -> TyO,
        FRRows: FnMut(RRows, &mut S) -> RRowsO,
        FERows: FnMut(ERows, &mut S) -> ERowsO,
        FTe: FnMut(Te, &mut S) -> TeO,
    {
        let f_lifted = |ty: Ty, state: &mut S| -> Result<TyO, Infallible> { Ok(f(ty, state)) };
        let f_rrows_lifted = |rrows: RRows, state: &mut S| -> Result<RRowsO, Infallible> {
            Ok(f_rrows(rrows, state))
        };
        let f_erows_lifted = |erows: ERows, state: &mut S| -> Result<ERowsO, Infallible> {
            Ok(f_erows(erows, state))
        };
        let f_ctr_lifted =
            |ctr: Te, state: &mut S| -> Result<TeO, Infallible> { Ok(f_ctr(ctr, state)) };

        self.try_map_state(
            f_lifted,
            f_rrows_lifted,
            f_erows_lifted,
            f_ctr_lifted,
            state,
        )
        .unwrap()
    }

    /// Variant of `try_map_state` without threaded state and with infallible functions.
    pub fn map<TyO, RRowsO, ERowsO, TeO, FTy, FRRows, FERows, FTe>(
        self,
        mut f: FTy,
        mut f_rrows: FRRows,
        mut f_erows: FERows,
        mut f_ctr: FTe,
    ) -> TypeF<TyO, RRowsO, ERowsO, TeO>
    where
        FTy: FnMut(Ty) -> TyO,
        FRRows: FnMut(RRows) -> RRowsO,
        FERows: FnMut(ERows) -> ERowsO,
        FTe: FnMut(Te) -> TeO,
    {
        let f_lifted = |ty: Ty, _: &mut ()| -> TyO { f(ty) };
        let f_rrows_lifted = |rrows: RRows, _: &mut ()| -> RRowsO { f_rrows(rrows) };
        let f_erows_lifted = |erows: ERows, _: &mut ()| -> ERowsO { f_erows(erows) };
        let f_ctr_lifted = |ctr: Te, _: &mut ()| -> TeO { f_ctr(ctr) };

        self.map_state(
            f_lifted,
            f_rrows_lifted,
            f_erows_lifted,
            f_ctr_lifted,
            &mut (),
        )
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self, TypeF::Wildcard(_))
    }

    pub fn is_contract(&self) -> bool {
        matches!(self, TypeF::Contract(_))
    }
}

#[derive(Clone, Debug)]
pub struct UnboundTypeVariableError(pub LocIdent);

impl From<UnboundTypeVariableError> for ParseError {
    fn from(err: UnboundTypeVariableError) -> Self {
        ParseError::UnboundTypeVariables(vec![err.0])
    }
}

impl From<UnboundTypeVariableError> for ParseErrors {
    fn from(err: UnboundTypeVariableError) -> Self {
        ParseErrors::from(ParseError::from(err))
    }
}
