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
use crate::{
    error::{EvalError, ParseError, ParseErrors, TypecheckError},
    identifier::{Ident, LocIdent},
    label::Polarity,
    mk_app, mk_fun,
    position::TermPos,
    term::{
        array::Array, make as mk_term, record::RecordData, string::NickelString, IndexMap,
        RichTerm, Term, Traverse, TraverseControl, TraverseOrder,
    },
};

use std::{
    collections::{HashMap, HashSet},
    convert::Infallible,
    fmt::{self, Display},
};

/// A record row, mapping an identifier to a type. A record type is a dictionary mapping
/// identifiers to Nickel type. Record types are represented as sequences of `RecordRowF`, ending
/// potentially with a type variable or `Dyn` in tail position.
///
/// # Type parameters
///
/// As other types with the `F` suffix, this type is parametrized by one or more recursive
/// unfoldings (here, `Ty` for `TypeF`). See [`TypeF`] for more details.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RecordRowF<Ty> {
    pub id: LocIdent,
    pub typ: Ty,
}

/// An enum row, which is just an identifier. An enum type is a set of identifiers, represented as
/// a sequence of `EnumRow`s, ending potentially with a type variable tail position.
///
/// `EnumRowF` is the same as `EnumRow` and doesn't have any type parameter. We introduce the alias
/// nonetheless for consistency with other parametrized type definitions. See [`TypeF`] for more
/// details.
pub type EnumRowF = LocIdent;
pub type EnumRow = EnumRowF;

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
#[derive(Clone, PartialEq, Eq, Debug)]
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
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum EnumRowsF<ERows> {
    Empty,
    Extend { row: EnumRowF, tail: ERows },
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
#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub enum VarKind {
    #[default]
    Type,
    EnumRows,
    /// `excluded` keeps track of which rows appear somewhere alongside the tail, and therefore
    /// cannot appear in the tail. For instance `forall r. { ; r } -> { x : Number ; r }` assumes
    /// `r` does not already contain an `x` field.
    RecordRows {
        excluded: HashSet<Ident>,
    },
}

/// Equivalent to `std::mem::Discriminant<VarKind>`, but we can do things like match on it
// TODO: this seems overly complicated, and it's anyways more space-efficient to store the
// `excluded` information separately like we do in the `State` field constr. Probably we can store
// it that way during parsing too.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum VarKindDiscriminant {
    Type,
    EnumRows,
    RecordRows,
}

impl From<&VarKind> for VarKindDiscriminant {
    fn from(vk: &VarKind) -> Self {
        match vk {
            VarKind::Type => VarKindDiscriminant::Type,
            VarKind::EnumRows => VarKindDiscriminant::EnumRows,
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
#[derive(Clone, Debug, Copy, Eq, PartialEq)]
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
/// good example is the definition of [Type::traverse]. Although [crate::term::Term] isn't currently
/// defined using functors per se, the way program transformations are written is in the same style
/// as recursion schemes: we simply define the action of a transformation as a mapping on the
/// current node, and let the traversal take care of the plumbing of recursion and reconstruction.
///
/// ## Type parameters
///
/// Here, `TypeF` also takes two additional type parameters for the recursive unfolding of record
/// rows and enum rows. We have other, distinct recursive subcomponents (or subtrees) in our
/// complete definition, but the approach is unchanged.
#[derive(Clone, PartialEq, Debug)]
pub enum TypeF<Ty, RRows, ERows> {
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
    /// See [`crate::term::Term::Sealed`].
    Symbol,
    /// A type created from a user-defined contract.
    Flat(RichTerm),
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

// Concrete, recursive definition of Nickel types from the generic `XxxF` definitions. This is
// "tying" the knot. We have to put `Box` in the appropriate positions (otherwise, Rust will
// complain that the type has an infinite size), but also avoid putting in more than necessary.
//
// For example, `RecordRows` contains a `RecordRow`. The latter doesn't need to be boxed, because a
// `RecordRow` itself potentially contains occurrences of `Type` and `RecordRows`, which need to
// be boxed. Hence, we don't need to additionally box `RecordRow`.

/// Concrete, recursive definition for enum rows.
#[derive(Clone, PartialEq, Debug)]
pub struct EnumRows(pub EnumRowsF<Box<EnumRows>>);
/// Concrete, recursive definition for a record row.
pub type RecordRow = RecordRowF<Box<Type>>;
#[derive(Clone, PartialEq, Debug)]
/// Concrete, recursive definition for record rows.
pub struct RecordRows(pub RecordRowsF<Box<Type>, Box<RecordRows>>);

/// Concrete, recursive type for a Nickel type.
#[derive(Clone, PartialEq, Debug)]
pub struct Type {
    pub typ: TypeF<Box<Type>, RecordRows, EnumRows>,
    pub pos: TermPos,
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
    /// see [RecordRows::traverse].
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
        let f_ty_lifted = |rrow: Ty| -> Result<TyO, ()> { Ok(f_ty(rrow)) };
        let f_rrows_lifted = |rrows: RRows| -> Result<RRowsO, ()> { Ok(f_rrows(rrows)) };
        self.try_map(f_ty_lifted, f_rrows_lifted).unwrap()
    }
}

impl<ERows> EnumRowsF<ERows> {
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
    /// building block to express recursive operations: as an example, see [RecordRows::traverse].
    pub fn try_map_state<ERowsO, FERows, S, E>(
        self,
        f_erows: FERows,
        state: &mut S,
    ) -> Result<EnumRowsF<ERowsO>, E>
    where
        FERows: FnOnce(ERows, &mut S) -> Result<ERowsO, E>,
    {
        match self {
            EnumRowsF::Empty => Ok(EnumRowsF::Empty),
            EnumRowsF::Extend { row, tail } => Ok(EnumRowsF::Extend {
                row,
                tail: f_erows(tail, state)?,
            }),
            EnumRowsF::TailVar(id) => Ok(EnumRowsF::TailVar(id)),
        }
    }

    /// Variant of `try_map_state` without threaded state.
    pub fn try_map<ERowsO, FERows, E>(self, mut f_erows: FERows) -> Result<EnumRowsF<ERowsO>, E>
    where
        FERows: FnMut(ERows) -> Result<ERowsO, E>,
    {
        let f_erows_lifted = |erows: ERows, _: &mut ()| -> Result<ERowsO, E> { f_erows(erows) };
        self.try_map_state(f_erows_lifted, &mut ())
    }

    /// Variant of `try_map_state` with infallible functions.
    pub fn map_state<ERowsO, FERows, S>(
        self,
        mut f_erows: FERows,
        state: &mut S,
    ) -> EnumRowsF<ERowsO>
    where
        FERows: FnMut(ERows, &mut S) -> ERowsO,
    {
        let f_erows_lifted =
            |erows: ERows, state: &mut S| -> Result<ERowsO, ()> { Ok(f_erows(erows, state)) };
        self.try_map_state(f_erows_lifted, state).unwrap()
    }

    /// Variant of `try_map_state` without threaded state and with infallible functions.
    pub fn map<ERowsO, FERows>(self, mut f_erows: FERows) -> EnumRowsF<ERowsO>
    where
        FERows: FnMut(ERows) -> ERowsO,
    {
        let f_erows_lifted = |erows: ERows, _: &mut ()| -> ERowsO { f_erows(erows) };
        self.map_state(f_erows_lifted, &mut ())
    }
}

impl<Ty, RRows, ERows> TypeF<Ty, RRows, ERows> {
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
    /// see [RecordRows::traverse].
    ///
    /// Since `TypeF` may contain record rows and enum rows as well, `f_rrows` and `f_erows` are
    /// required to know how to map on record and enum types respectively.
    pub fn try_map_state<TyO, RRowsO, ERowsO, FTy, FRRows, FERows, S, E>(
        self,
        mut f: FTy,
        mut f_rrows: FRRows,
        mut f_erows: FERows,
        state: &mut S,
    ) -> Result<TypeF<TyO, RRowsO, ERowsO>, E>
    where
        FTy: FnMut(Ty, &mut S) -> Result<TyO, E>,
        FRRows: FnMut(RRows, &mut S) -> Result<RRowsO, E>,
        FERows: FnMut(ERows, &mut S) -> Result<ERowsO, E>,
    {
        match self {
            TypeF::Dyn => Ok(TypeF::Dyn),
            TypeF::Number => Ok(TypeF::Number),
            TypeF::Bool => Ok(TypeF::Bool),
            TypeF::String => Ok(TypeF::String),
            TypeF::Symbol => Ok(TypeF::Symbol),
            TypeF::Flat(t) => Ok(TypeF::Flat(t)),
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
    pub fn try_map<TyO, RRowsO, ERowsO, FTy, FRRows, FERows, E>(
        self,
        mut f: FTy,
        mut f_rrows: FRRows,
        mut f_erows: FERows,
    ) -> Result<TypeF<TyO, RRowsO, ERowsO>, E>
    where
        FTy: FnMut(Ty) -> Result<TyO, E>,
        FRRows: FnMut(RRows) -> Result<RRowsO, E>,
        FERows: FnMut(ERows) -> Result<ERowsO, E>,
    {
        let f_lifted = |ty: Ty, _: &mut ()| -> Result<TyO, E> { f(ty) };
        let f_rrows_lifted = |rrows: RRows, _: &mut ()| -> Result<RRowsO, E> { f_rrows(rrows) };
        let f_erows_lifted = |erows: ERows, _: &mut ()| -> Result<ERowsO, E> { f_erows(erows) };

        self.try_map_state(f_lifted, f_rrows_lifted, f_erows_lifted, &mut ())
    }

    /// Variant of `try_map_state` with infallible functions.
    pub fn map_state<TyO, RRowsO, ERowsO, FTy, FRRows, FERows, S>(
        self,
        mut f: FTy,
        mut f_rrows: FRRows,
        mut f_erows: FERows,
        state: &mut S,
    ) -> TypeF<TyO, RRowsO, ERowsO>
    where
        FTy: FnMut(Ty, &mut S) -> TyO,
        FRRows: FnMut(RRows, &mut S) -> RRowsO,
        FERows: FnMut(ERows, &mut S) -> ERowsO,
    {
        let f_lifted = |ty: Ty, state: &mut S| -> Result<TyO, ()> { Ok(f(ty, state)) };
        let f_rrows_lifted =
            |rrows: RRows, state: &mut S| -> Result<RRowsO, ()> { Ok(f_rrows(rrows, state)) };
        let f_erows_lifted =
            |erows: ERows, state: &mut S| -> Result<ERowsO, ()> { Ok(f_erows(erows, state)) };
        self.try_map_state(f_lifted, f_rrows_lifted, f_erows_lifted, state)
            .unwrap()
    }

    /// Variant of `try_map_state` without threaded state and with infallible functions.
    pub fn map<TyO, RRowsO, ERowsO, FTy, FRRows, FERows>(
        self,
        mut f: FTy,
        mut f_rrows: FRRows,
        mut f_erows: FERows,
    ) -> TypeF<TyO, RRowsO, ERowsO>
    where
        FTy: FnMut(Ty) -> TyO,
        FRRows: FnMut(RRows) -> RRowsO,
        FERows: FnMut(ERows) -> ERowsO,
    {
        let f_lifted = |ty: Ty, _: &mut ()| -> TyO { f(ty) };
        let f_rrows_lifted = |rrows: RRows, _: &mut ()| -> RRowsO { f_rrows(rrows) };
        let f_erows_lifted = |erows: ERows, _: &mut ()| -> ERowsO { f_erows(erows) };
        self.map_state(f_lifted, f_rrows_lifted, f_erows_lifted, &mut ())
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self, TypeF::Wildcard(_))
    }

    pub fn is_flat(&self) -> bool {
        matches!(self, TypeF::Flat(_))
    }
}

impl Traverse<Type> for RecordRows {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<RecordRows, E>
    where
        F: FnMut(Type) -> Result<Type, E>,
    {
        // traverse keeps track of state in the FnMut function. try_map_state
        // keeps track of it in a separate state variable. we can pass the
        // former into the latter by treating the function itself as the state
        let rows = self.0.try_map_state(
            |ty, f| Ok(Box::new(ty.traverse(f, order)?)),
            |rrows, f| Ok(Box::new(rrows.traverse(f, order)?)),
            f,
        )?;

        Ok(RecordRows(rows))
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&Type, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        match &self.0 {
            RecordRowsF::Extend { row, tail } => row
                .typ
                .traverse_ref(f, state)
                .or_else(|| tail.traverse_ref(f, state)),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct UnboundTypeVariableError(pub LocIdent);

impl From<UnboundTypeVariableError> for EvalError {
    fn from(err: UnboundTypeVariableError) -> Self {
        let UnboundTypeVariableError(id) = err;
        let pos = id.pos;
        EvalError::UnboundIdentifier(id, pos)
    }
}

impl From<UnboundTypeVariableError> for TypecheckError {
    fn from(err: UnboundTypeVariableError) -> Self {
        TypecheckError::UnboundTypeVariable(err.0)
    }
}

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

pub struct RecordRowsIterator<'a, Ty, RRows> {
    pub(crate) rrows: Option<&'a RRows>,
    pub(crate) ty: std::marker::PhantomData<Ty>,
}

pub enum RecordRowsIteratorItem<'a, Ty> {
    TailDyn,
    TailVar(&'a LocIdent),
    Row(RecordRowF<&'a Ty>),
}

impl<'a> Iterator for RecordRowsIterator<'a, Type, RecordRows> {
    type Item = RecordRowsIteratorItem<'a, Type>;

    fn next(&mut self) -> Option<Self::Item> {
        self.rrows.and_then(|next| match next.0 {
            RecordRowsF::Empty => {
                self.rrows = None;
                None
            }
            RecordRowsF::TailDyn => {
                self.rrows = None;
                Some(RecordRowsIteratorItem::TailDyn)
            }
            RecordRowsF::TailVar(ref id) => {
                self.rrows = None;
                Some(RecordRowsIteratorItem::TailVar(id))
            }
            RecordRowsF::Extend { ref row, ref tail } => {
                self.rrows = Some(tail);
                Some(RecordRowsIteratorItem::Row(RecordRowF {
                    id: row.id,
                    typ: row.typ.as_ref(),
                }))
            }
        })
    }
}

pub struct EnumRowsIterator<'a, ERows> {
    pub(crate) erows: Option<&'a ERows>,
}

pub enum EnumRowsIteratorItem<'a> {
    TailVar(&'a LocIdent),
    Row(&'a EnumRowF),
}

impl<'a> Iterator for EnumRowsIterator<'a, EnumRows> {
    type Item = EnumRowsIteratorItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.erows.and_then(|next| match next.0 {
            EnumRowsF::Empty => {
                self.erows = None;
                None
            }
            EnumRowsF::TailVar(ref id) => {
                self.erows = None;
                Some(EnumRowsIteratorItem::TailVar(id))
            }
            EnumRowsF::Extend { ref row, ref tail } => {
                self.erows = Some(tail);
                Some(EnumRowsIteratorItem::Row(row))
            }
        })
    }
}

/// Retrieve the contract corresponding to a type variable occurrence in a type as a `RichTerm`.
/// Helper used by the `subcontract` functions.
fn get_var_contract(
    vars: &HashMap<Ident, RichTerm>,
    sym: Ident,
    pos: TermPos,
) -> Result<RichTerm, UnboundTypeVariableError> {
    Ok(vars
        .get(&sym)
        .ok_or(UnboundTypeVariableError(LocIdent::from(sym).with_pos(pos)))?
        .clone())
}

impl EnumRows {
    fn subcontract(&self) -> Result<RichTerm, UnboundTypeVariableError> {
        use crate::stdlib::internals;

        let mut cases = IndexMap::new();
        let mut has_tail = false;
        let value_arg = LocIdent::from("x");
        let label_arg = LocIdent::from("l");

        for row in self.iter() {
            match row {
                EnumRowsIteratorItem::Row(id) => {
                    cases.insert(*id, mk_term::var(value_arg));
                }
                EnumRowsIteratorItem::TailVar(_) => {
                    has_tail = true;
                    break;
                }
            }
        }

        // If the enum type has a tail, the tail must be a universally quantified variable,
        // and this means that the tag can be anything.
        let case_body = if has_tail {
            mk_term::var(value_arg)
        }
        // Otherwise, we build a match with all the tags as cases, which just returns the
        // original argument, and a default case that blames.
        //
        // For example, for an enum type [| 'foo, 'bar, 'baz |], the `case` function looks
        // like:
        //
        // ```
        // fun l x =>
        //   match {
        //     'foo => x,
        //     'bar => x,
        //     'baz => x,
        //     _ => $enum_fail l
        //   } x
        // ```
        else {
            mk_app!(
                Term::Match {
                    cases,
                    default: Some(mk_app!(internals::enum_fail(), mk_term::var(label_arg))),
                },
                mk_term::var(value_arg)
            )
        };
        let case = mk_fun!(label_arg, value_arg, case_body);

        Ok(mk_app!(internals::enums(), case))
    }

    pub fn iter(&self) -> EnumRowsIterator<EnumRows> {
        EnumRowsIterator { erows: Some(self) }
    }
}

impl RecordRows {
    /// Construct the subcontract corresponding to a record type
    fn subcontract(
        &self,
        vars: HashMap<Ident, RichTerm>,
        pol: Polarity,
        sy: &mut i32,
    ) -> Result<RichTerm, UnboundTypeVariableError> {
        use crate::stdlib::internals;

        // We begin by building a record whose arguments are contracts
        // derived from the types of the statically known fields.
        let mut rrows = self;
        let mut fcs = IndexMap::new();

        while let RecordRowsF::Extend {
            row: RecordRowF { id, typ: ty },
            tail,
        } = &rrows.0
        {
            fcs.insert(*id, ty.subcontract(vars.clone(), pol, sy)?);
            rrows = tail
        }

        // Now that we've dealt with the row extends, we just need to
        // work out the tail.
        let tail = match &rrows.0 {
            RecordRowsF::Empty => internals::empty_tail(),
            RecordRowsF::TailDyn => internals::dyn_tail(),
            RecordRowsF::TailVar(id) => get_var_contract(&vars, id.ident(), id.pos)?,
            // Safety: the while above excludes that `tail` can have the form `Extend`.
            RecordRowsF::Extend { .. } => unreachable!(),
        };

        let rec = RichTerm::from(Term::Record(RecordData::with_field_values(fcs)));

        Ok(mk_app!(internals::record(), rec, tail))
    }

    /// Find a nested binding in a record row type. The nested field is given as a list of
    /// successive fields, that is, as a path. Return `None` if there is no such binding.
    ///
    /// # Example
    ///
    /// - self: ` {a : {b : Number }}`
    /// - path: `["a", "b"]`
    /// - result: `Some(Number)`
    pub fn find_path(&self, path: &[Ident]) -> Option<RecordRowF<&Type>> {
        if path.is_empty() {
            return None;
        }

        let next = self.iter().find_map(|item| match item {
            RecordRowsIteratorItem::Row(row) if row.id.ident() == path[0] => Some(row.clone()),
            _ => None,
        });

        if path.len() == 1 {
            next
        } else {
            match next.map(|row| &row.typ.typ) {
                Some(TypeF::Record(rrows)) => rrows.find_path(&path[1..]),
                _ => None,
            }
        }
    }

    pub fn iter(&self) -> RecordRowsIterator<Type, RecordRows> {
        RecordRowsIterator {
            rrows: Some(self),
            ty: std::marker::PhantomData,
        }
    }
}

impl From<TypeF<Box<Type>, RecordRows, EnumRows>> for Type {
    fn from(typ: TypeF<Box<Type>, RecordRows, EnumRows>) -> Self {
        Type {
            typ,
            pos: TermPos::None,
        }
    }
}

impl Type {
    /// Creates a `Type` with the specified position
    pub fn with_pos(self, pos: TermPos) -> Type {
        Type { pos, ..self }
    }

    /// Returns the same type with the position cleared (set to `None`).
    ///
    /// This is currently only used in test code, but because it's used from integration
    /// tests we cannot hide it behind cfg(test).
    pub fn without_pos(self) -> Type {
        self.traverse(
            &mut |t: Type| {
                Ok::<_, Infallible>(Type {
                    pos: TermPos::None,
                    ..t
                })
            },
            TraverseOrder::BottomUp,
        )
        .unwrap()
        .traverse(
            &mut |t: RichTerm| {
                Ok::<_, Infallible>(RichTerm {
                    pos: TermPos::None,
                    ..t
                })
            },
            TraverseOrder::BottomUp,
        )
        .unwrap()
    }

    /// Return the contract corresponding to a type, either as a function or a record. Said
    /// contract must then be applied using the `ApplyContract` primitive operation.
    pub fn contract(&self) -> Result<RichTerm, UnboundTypeVariableError> {
        let mut sy = 0;
        self.subcontract(HashMap::new(), Polarity::Positive, &mut sy)
    }

    /// Returns true if this type is a function type, false otherwise.
    pub fn is_function_type(&self) -> bool {
        match &self.typ {
            TypeF::Forall { body, .. } => body.is_function_type(),
            TypeF::Arrow(..) => true,
            _ => false,
        }
    }

    /// Return the contract corresponding to a subtype.
    ///
    /// # Arguments
    ///
    /// - `h` is an environment mapping type variables to contracts. Type variables are introduced
    ///   locally when opening a `forall`.
    /// - `pol` is the current polarity, which is toggled when generating a contract for the
    ///   argument of an arrow type (see [`crate::label::Label`]).
    /// - `sy` is a counter used to generate fresh symbols for `forall` contracts (see
    ///   [`crate::term::Term::Sealed`]).
    fn subcontract(
        &self,
        mut vars: HashMap<Ident, RichTerm>,
        pol: Polarity,
        sy: &mut i32,
    ) -> Result<RichTerm, UnboundTypeVariableError> {
        use crate::stdlib::internals;

        let ctr = match self.typ {
            TypeF::Dyn => internals::dynamic(),
            TypeF::Number => internals::num(),
            TypeF::Bool => internals::bool(),
            TypeF::String => internals::string(),
            //TODO: optimization: have a specialized contract for `Array Dyn`, to avoid mapping an
            //always successful contract on each element.
            TypeF::Array(ref ty) => mk_app!(internals::array(), ty.subcontract(vars, pol, sy)?),
            TypeF::Symbol => panic!("Are you trying to check a Sym at runtime?"),
            TypeF::Arrow(ref s, ref t) => mk_app!(
                internals::func(),
                s.subcontract(vars.clone(), pol.flip(), sy)?,
                t.subcontract(vars, pol, sy)?
            ),
            TypeF::Flat(ref t) => t.clone(),
            TypeF::Var(id) => get_var_contract(&vars, id, self.pos)?,
            TypeF::Forall {
                ref var,
                ref body,
                ref var_kind,
            } => {
                let sealing_key = Term::SealingKey(*sy);
                let contract = match var_kind {
                    VarKind::Type => mk_app!(internals::forall_var(), sealing_key.clone()),
                    VarKind::EnumRows => {
                        // Enums do not need to exclude any rows, so we pass the empty array
                        let excluded_ncl: RichTerm =
                            Term::Array(Default::default(), Default::default()).into();
                        mk_app!(internals::forall_tail(), sealing_key.clone(), excluded_ncl)
                    }
                    VarKind::RecordRows { excluded } => {
                        let excluded_ncl: RichTerm = Term::Array(
                            Array::from_iter(
                                excluded
                                    .iter()
                                    .map(|id| Term::Str(NickelString::from(*id)).into()),
                            ),
                            Default::default(),
                        )
                        .into();
                        mk_app!(internals::forall_tail(), sealing_key.clone(), excluded_ncl)
                    }
                };
                vars.insert(var.ident(), contract);

                *sy += 1;
                mk_app!(
                    internals::forall(),
                    sealing_key,
                    Term::from(pol),
                    body.subcontract(vars, pol, sy)?
                )
            }
            TypeF::Enum(ref erows) => erows.subcontract()?,
            TypeF::Record(ref rrows) => rrows.subcontract(vars, pol, sy)?,
            TypeF::Dict {
                ref type_fields,
                flavour: DictTypeFlavour::Contract,
            } => {
                mk_app!(
                    internals::dict_contract(),
                    type_fields.subcontract(vars, pol, sy)?
                )
            }
            TypeF::Dict {
                ref type_fields,
                flavour: DictTypeFlavour::Type,
            } => {
                mk_app!(
                    internals::dict_type(),
                    type_fields.subcontract(vars, pol, sy)?
                )
            }
            TypeF::Wildcard(_) => internals::dynamic(),
        };

        Ok(ctr)
    }

    /// Determine if a type is an atom, that is a either a primitive type (`Dyn`, `Number`, etc.) or
    /// a type delimited by specific markers (such as a row type). Used in formatting to decide if
    /// parentheses need to be inserted during pretty pretting.
    pub fn fmt_is_atom(&self) -> bool {
        match &self.typ {
            TypeF::Dyn
            | TypeF::Number
            | TypeF::Bool
            | TypeF::String
            | TypeF::Var(_)
            | TypeF::Record(_)
            | TypeF::Enum(_) => true,
            TypeF::Flat(rt) if rt.as_ref().is_atom() => true,
            _ => false,
        }
    }

    /// Searches for a `TypeF::Flat`. If one is found, returns the term it contains.
    pub fn find_flat(&self) -> Option<RichTerm> {
        self.find_map(|ty: &Type| match &ty.typ {
            TypeF::Flat(f) => Some(f.clone()),
            _ => None,
        })
    }
}

impl Traverse<Type> for Type {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(Type) -> Result<Type, E>,
    {
        let pre_map = match order {
            TraverseOrder::TopDown => f(self)?,
            TraverseOrder::BottomUp => self,
        };

        // traverse keeps track of state in the FnMut function. try_map_state
        // keeps track of it in a separate state variable. we can pass the
        // former into the latter by treating the function itself as the state
        let typ = pre_map.typ.try_map_state(
            |ty, f| Ok(Box::new(ty.traverse(f, order)?)),
            |rrows, f| rrows.traverse(f, order),
            |erows, _| Ok(erows),
            f,
        )?;

        let post_map = Type { typ, ..pre_map };

        match order {
            TraverseOrder::TopDown => Ok(post_map),
            TraverseOrder::BottomUp => f(post_map),
        }
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&Type, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        let child_state = match f(self, state) {
            TraverseControl::Continue => None,
            TraverseControl::ContinueWithScope(s) => Some(s),
            TraverseControl::SkipBranch => {
                return None;
            }
            TraverseControl::Return(ret) => {
                return Some(ret);
            }
        };
        let state = child_state.as_ref().unwrap_or(state);

        match &self.typ {
            TypeF::Dyn
            | TypeF::Number
            | TypeF::Bool
            | TypeF::String
            | TypeF::Symbol
            | TypeF::Var(_)
            | TypeF::Enum(_)
            | TypeF::Wildcard(_) => None,
            TypeF::Flat(rt) => rt.traverse_ref(f, state),
            TypeF::Arrow(t1, t2) => t1
                .traverse_ref(f, state)
                .or_else(|| t2.traverse_ref(f, state)),
            TypeF::Forall { body: t, .. }
            | TypeF::Dict { type_fields: t, .. }
            | TypeF::Array(t) => t.traverse_ref(f, state),
            TypeF::Record(rrows) => rrows.traverse_ref(f, state),
        }
    }
}

impl Traverse<RichTerm> for Type {
    fn traverse<F, E>(self, f: &mut F, order: TraverseOrder) -> Result<Self, E>
    where
        F: FnMut(RichTerm) -> Result<RichTerm, E>,
    {
        self.traverse(
            &mut |ty: Type| match ty.typ {
                TypeF::Flat(t) => t
                    .traverse(f, order)
                    .map(|t| Type::from(TypeF::Flat(t)).with_pos(ty.pos)),
                _ => Ok(ty),
            },
            order,
        )
    }

    fn traverse_ref<S, U>(
        &self,
        f: &mut dyn FnMut(&RichTerm, &S) -> TraverseControl<S, U>,
        state: &S,
    ) -> Option<U> {
        self.traverse_ref(
            &mut |ty: &Type, s: &S| match &ty.typ {
                TypeF::Flat(t) => {
                    if let Some(ret) = t.traverse_ref(f, s) {
                        TraverseControl::Return(ret)
                    } else {
                        TraverseControl::SkipBranch
                    }
                }
                _ => TraverseControl::Continue,
            },
            state,
        )
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        crate::pretty::fmt_pretty(&self, f)
    }
}
