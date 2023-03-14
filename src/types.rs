//! Define the Nickel type system.
//!
//! # Base types
//!
//! - Number: a floating-point number
//! - Bool: a boolean
//! - String: a string literal
//! - Sym: a symbol, used by contracts when checking polymorphic types
//! - Array: an (heterogeneous) array
//!
//! # Higher-order types
//!
//! - `->`: the function type, or arrow
//! - `forall a. type`: polymorphic type
//! - `#customContract`: an opaque type created from an user-defined contract
//!
//! # Record types
//!
//! The type systems feature structural records with row-polymorphism.
//!
//! ## Records (row types)
//!
//! A row type for a record is a linked list of pairs `(id, type)` indicating the name and the type
//! of each field. Row-polymorphism means that the tail of this list can be a type variable which
//! can be abstracted over, leaving the row open for future extension. A simple and demonstrative
//! example is field access:
//!
//! ```text
//! let f = Promise(forall a. { myField : Number, a} -> Number, fun rec => rec.myField)
//! ```
//!
//! The type `{ myField : Number, a }` indicates that any argument must have at least the field
//! `myField` of type `Number`, but may contain any other fields (or no additional field at all).
//!
//! ## Dictionaries
//!
//! A dictionary type: `{ _ : Type }`. It has string keys and `Type` values. It can be mapped
//! over and accessed in a type-safe manner.
//!
//! # Enum types
//!
//! An enum type is also a row type, but each list element only contains an identifier without an
//! associated type. It indicates which tag the enum can contain.
//!
//! # Contracts
//!
//! To each type corresponds a contract, which is a Nickel function which checks at runtime that
//! its argument is of the given type and either returns it if it passes or raise a blame
//! otherwise.  Contract checks are introduced by `Promise` and `Assume` blocks or alternatively by
//! enriched values `Contract` or `ContractDefault`. They ensure sane interaction between typed and
//! untyped parts.
use crate::{
    error::{EvalError, ParseError, ParseErrors, TypecheckError},
    identifier::Ident,
    mk_app, mk_fun,
    position::TermPos,
    term::make as mk_term,
    term::{record::RecordData, RichTerm, Term, Traverse, TraverseOrder},
};

use std::{
    collections::HashMap,
    fmt::{self, Display},
    ops::Range,
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
    pub id: Ident,
    pub types: Ty,
}

/// An enum row, which is just an identifier. An enum type is a set of identifiers, represented as
/// a sequence of `EnumRow`s, ending potentially with a type variable tail position.
///
/// `EnumRowF` is the same as `EnumRow` and doesn't have any type parameter. We introduce the alias
/// nonetheless for consistency with other parametrized type definitions. See [`TypeF`] for more
/// details.
pub type EnumRowF = Ident;
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
    TailVar(Ident),
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
    TailVar(Ident),
}

/// The kind of a quantified type variable.
///
/// Nickel uses several forms of polymorphism. A type variable can be substituted for a type, as in
/// `id : forall a. a -> a`, for record rows as in `access_foo : forall a . {foo : Number; a} -> Number}`,
/// or for enum rows. This information is implicit in the source syntax: we don't require users to
/// write e.g. `forall a :: Type` or `forall a :: Rows`. But the kind of a variable is required for
/// the typechecker. It is thus determined during parsing and stored as `VarKind` where type
/// variables are introduced, that is, on forall quantifiers.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum VarKind {
    Type,
    EnumRows,
    RecordRows,
}

/// A Nickel type.
///
/// # Generic representation (functor)
///
/// A Nickel type is represented by a tree and is naturally defined in a recursive manner: for
/// example, one would expect the constructor for function types (`Arrow`) to look like:
///
/// ```rust
/// pub enum Types {
///      Arrow(Box<Types>, Box<Types>),
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
/// Types(TypeF<Box<Types>>)`, we get back the original, natural definition.
///
/// ## Motivation 1: variation on `Types`
///
/// Having a generic definition makes it possible to easily create other types with the _same
/// shape_ as `Types` (seen as trees), but with enriched nodes. The typical use-case in Nickel is
/// the variation on types used by the typechecker. During type inference, the typechecker operates
/// on trees where each node can either be a concrete type, or a unification variable (a unknown
/// type to be inferred). Instead of duplicating the whole definition of `Types` as well as all
/// basic methods, we can simply have a different recursive definition:
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
/// good example is the definition of `[Types::traverse]`. Although [`crate::term::Term`] isn't
/// currently defined using functors per se, the way program transformations are written is in the same
/// style as recursion schemes: we simply define the action of a transformation as a mapping on the
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
        var: Ident,
        var_kind: VarKind,
        body: Ty,
    },

    /// An enum type, composed of a sequence of enum rows.
    Enum(ERows),
    /// A record type, composed of a sequence of record rows.
    Record(RRows),
    /// A dictionary type.
    Dict(Ty),
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
// `RecordRow` itself potentially contains occurrences of `Types` and `RecordRows`, which need to
// be boxed. Hence, we don't need to additionally box `RecordRow`.

/// Concrete, recursive definition for enum rows.
#[derive(Clone, PartialEq, Debug)]
pub struct EnumRows(pub EnumRowsF<Box<EnumRows>>);
/// Concrete, recursive definition for a record row.
pub type RecordRow = RecordRowF<Box<Types>>;
#[derive(Clone, PartialEq, Debug)]
/// Concrete, recursive definition for record rows.
pub struct RecordRows(pub RecordRowsF<Box<Types>, Box<RecordRows>>);

/// Concrete, recursive type for a Nickel type.
#[derive(Clone, PartialEq, Debug)]
pub struct Types {
    pub types: TypeF<Box<Types>, RecordRows, EnumRows>,
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
                row: RecordRowF { id, types },
                tail,
            } => Ok(RecordRowsF::Extend {
                row: RecordRowF {
                    id,
                    types: f_ty(types, state)?,
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
    /// enum rows of the form ``[| `foo, `bar, `baz |]``. Then, calling `try_map_state(f_erows,
    /// state)` on these rows will map `f_erows` onto ``[| `bar, `baz |]``.
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
    /// Take the instantiated version `Types`, and a type of the form `(Dyn -> Dyn) -> (Number ->
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
            TypeF::Dict(t) => Ok(TypeF::Dict(f(t, state)?)),
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

impl Traverse<Types> for RecordRows {
    fn traverse<FTy, S, E>(
        self,
        f: &FTy,
        state: &mut S,
        order: TraverseOrder,
    ) -> Result<RecordRows, E>
    where
        FTy: Fn(Types, &mut S) -> Result<Types, E>,
    {
        let inner = self.0.try_map_state(
            |ty, state| Ok(Box::new(ty.traverse(f, state, order)?)),
            |rrows, state| Ok(Box::new(rrows.traverse(f, state, order)?)),
            state,
        )?;

        Ok(RecordRows(inner))
    }
}

#[derive(Clone, Debug)]
pub struct UnboundTypeVariableError(pub Ident);

impl From<UnboundTypeVariableError> for EvalError {
    fn from(err: UnboundTypeVariableError) -> Self {
        let UnboundTypeVariableError(id) = err;
        let pos = id.pos;
        EvalError::UnboundIdentifier(id, pos)
    }
}

impl From<UnboundTypeVariableError> for TypecheckError {
    fn from(err: UnboundTypeVariableError) -> Self {
        let pos = err.0.pos;
        TypecheckError::UnboundTypeVariable(err.0, pos)
    }
}

impl From<UnboundTypeVariableError> for ParseError {
    fn from(err: UnboundTypeVariableError) -> Self {
        let pos = err.0.pos;
        ParseError::UnboundTypeVariables(vec![err.0], pos.unwrap())
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
    TailVar(&'a Ident),
    Row(RecordRowF<&'a Ty>),
}

impl<'a> Iterator for RecordRowsIterator<'a, Types, RecordRows> {
    type Item = RecordRowsIteratorItem<'a, Types>;

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
                    types: row.types.as_ref(),
                }))
            }
        })
    }
}

pub struct EnumRowsIterator<'a, ERows> {
    pub(crate) erows: Option<&'a ERows>,
}

pub enum EnumRowsIteratorItem<'a> {
    TailVar(&'a Ident),
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
/// Helper used by the `subcontract` functions. `pol` is the polarity of the variable occurrence
/// inside the original type.
fn get_var_contract(
    vars: &HashMap<Ident, (RichTerm, RichTerm)>,
    id: &Ident,
    pol: bool,
) -> Result<RichTerm, UnboundTypeVariableError> {
    let (pos, neg) = vars.get(id).ok_or(UnboundTypeVariableError(*id))?;
    if pol {
        Ok(pos.clone())
    } else {
        Ok(neg.clone())
    }
}

impl EnumRows {
    fn subcontract(&self) -> Result<RichTerm, UnboundTypeVariableError> {
        use crate::stdlib::contract;

        let mut cases = HashMap::new();
        let mut has_tail = false;
        let value_arg = Ident::from("x");
        let label_arg = Ident::from("l");

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
        // For example, for an enum type [| `foo, `bar, `baz |], the `case` function looks
        // like:
        //
        // ```
        // fun l x =>
        //   match {
        //     `foo => x,
        //     `bar => x,
        //     `baz => x,
        //     _ => $enum_fail l
        //   } x
        // ```
        else {
            mk_app!(
                Term::Match {
                    cases,
                    default: Some(mk_app!(contract::enum_fail(), mk_term::var(label_arg))),
                },
                mk_term::var(value_arg)
            )
        };
        let case = mk_fun!(label_arg, value_arg, case_body);

        Ok(mk_app!(contract::enums(), case))
    }

    pub fn iter(&self) -> EnumRowsIterator<EnumRows> {
        EnumRowsIterator { erows: Some(self) }
    }
}

impl RecordRows {
    // TODO: doc
    fn subcontract(
        &self,
        h: HashMap<Ident, (RichTerm, RichTerm)>,
        pol: bool,
        sy: &mut i32,
    ) -> Result<RichTerm, UnboundTypeVariableError> {
        use crate::stdlib::contract;

        // We begin by building a record whose arguments are contracts
        // derived from the types of the statically known fields.
        let mut rrows = self;
        let mut fcs = HashMap::new();

        while let RecordRowsF::Extend {
            row: RecordRowF { id, types: ty },
            tail,
        } = &rrows.0
        {
            fcs.insert(*id, ty.subcontract(h.clone(), pol, sy)?);
            rrows = tail
        }

        // Now that we've dealt with the row extends, we just need to
        // work out the tail.
        let tail = match &rrows.0 {
            RecordRowsF::Empty => contract::empty_tail(),
            RecordRowsF::TailDyn => contract::dyn_tail(),
            RecordRowsF::TailVar(id) => get_var_contract(&h, id, false)?,
            // Safety: the while above excludes that `tail` can have the form `Extend`.
            RecordRowsF::Extend { .. } => unreachable!(),
        };

        let rec = RichTerm::from(Term::Record(RecordData::with_field_values(fcs)));

        Ok(mk_app!(contract::record(), rec, tail))
    }

    /// Find a nested binding in a record row type. The nested field is given as a list of
    /// successive fields, that is, as a path. Return `None` if there is no such binding.
    ///
    /// # Example
    ///
    /// - self: ` {a : {b : Number }}`
    /// - path: `["a", "b"]`
    /// - result: `Some(Number)`
    pub fn row_find_path(&self, path: &[Ident]) -> Option<Types> {
        if path.is_empty() {
            return None;
        }

        let next = self.iter().find_map(|item| match item {
            RecordRowsIteratorItem::Row(row) if row.id == path[0] => Some(row.types.clone()),
            _ => None,
        });

        if path.len() == 1 {
            next
        } else {
            match next.map(|ty| ty.types) {
                Some(TypeF::Record(rrows)) => rrows.row_find_path(&path[1..]),
                _ => None,
            }
        }
    }

    pub fn iter(&self) -> RecordRowsIterator<Types, RecordRows> {
        RecordRowsIterator {
            rrows: Some(self),
            ty: std::marker::PhantomData,
        }
    }
}

impl From<TypeF<Box<Types>, RecordRows, EnumRows>> for Types {
    fn from(types: TypeF<Box<Types>, RecordRows, EnumRows>) -> Self {
        Types {
            types,
            pos: TermPos::None,
        }
    }
}

impl Types {
    /// Creates a `Type` with the specified position
    pub fn with_pos(self, pos: TermPos) -> Types {
        Types { pos, ..self }
    }

    pub fn range(&self) -> Option<Range<usize>> {
        self.pos
            .into_opt()
            .map(|span| span.start.to_usize()..span.end.to_usize())
    }

    /// Return the contract corresponding to a type, either as a function or a record. Said
    /// contract must then be applied using the `Assume` primitive operation.
    pub fn contract(&self) -> Result<RichTerm, UnboundTypeVariableError> {
        let mut sy = 0;
        self.subcontract(HashMap::new(), true, &mut sy)
    }

    /// Returns true if this type is a function type, false otherwise.
    pub fn is_function_type(&self) -> bool {
        match &self.types {
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
    /// - `pol` is the current polarity, which is toggled when generating a contract for the argument
    ///   of an arrow type (see [`crate::label::Label`]).
    /// - `sy` is a counter used to generate fresh symbols for `forall` contracts (see [`crate::term::Term::Sealed`]).
    fn subcontract(
        &self,
        mut h: HashMap<Ident, (RichTerm, RichTerm)>,
        pol: bool,
        sy: &mut i32,
    ) -> Result<RichTerm, UnboundTypeVariableError> {
        use crate::stdlib::contract;

        let ctr = match self.types {
            TypeF::Dyn => contract::dynamic(),
            TypeF::Number => contract::num(),
            TypeF::Bool => contract::bool(),
            TypeF::String => contract::string(),
            //TODO: optimization: have a specialized contract for `Array Dyn`, to avoid mapping an
            //always successful contract on each element.
            TypeF::Array(ref ty) => mk_app!(contract::array(), ty.subcontract(h, pol, sy)?),
            TypeF::Symbol => panic!("Are you trying to check a Sym at runtime?"),
            TypeF::Arrow(ref s, ref t) => mk_app!(
                contract::func(),
                s.subcontract(h.clone(), !pol, sy)?,
                t.subcontract(h, pol, sy)?
            ),
            TypeF::Flat(ref t) => t.clone(),
            TypeF::Var(ref id) => get_var_contract(&h, id, true)?,
            TypeF::Forall {
                ref var, ref body, ..
            } => {
                let inst_var = mk_app!(
                    contract::forall_var(),
                    Term::SealingKey(*sy),
                    Term::Bool(pol)
                );

                let inst_tail = mk_app!(
                    contract::forall_tail(),
                    Term::SealingKey(*sy),
                    Term::Bool(pol)
                );

                h.insert(*var, (inst_var, inst_tail));
                *sy += 1;
                body.subcontract(h, pol, sy)?
            }
            TypeF::Enum(ref erows) => erows.subcontract()?,
            TypeF::Record(ref rrows) => rrows.subcontract(h, pol, sy)?,
            TypeF::Dict(ref ty) => {
                mk_app!(contract::dyn_record(), ty.subcontract(h, pol, sy)?)
            }
            TypeF::Wildcard(_) => contract::dynamic(),
        };

        Ok(ctr)
    }

    /// Determine if a type is an atom, that is a either a primitive type (`Dyn`, `Number`, etc.) or a
    /// type delimited by specific markers (such as a row type). Used in formatting to decide if
    /// parentheses need to be inserted during pretty pretting.
    pub fn fmt_is_atom(&self) -> bool {
        match &self.types {
            TypeF::Dyn
            | TypeF::Number
            | TypeF::Bool
            | TypeF::String
            | TypeF::Var(_)
            | TypeF::Record(_)
            | TypeF::Enum(_) => true,
            TypeF::Flat(rt) if matches!(*rt.term, Term::Var(_)) => true,
            _ => false,
        }
    }
}

impl Traverse<Types> for Types {
    fn traverse<FTy, S, E>(self, f: &FTy, state: &mut S, order: TraverseOrder) -> Result<Self, E>
    where
        FTy: Fn(Types, &mut S) -> Result<Types, E>,
    {
        match order {
            TraverseOrder::TopDown => {
                let inner = f(self, state)?.types.try_map_state(
                    |ty, state| Ok(Box::new(ty.traverse(f, state, order)?)),
                    |rrows, state| rrows.traverse(f, state, order),
                    |erows, _| Ok(erows),
                    state,
                )?;

                Ok(Types::from(inner))
            }
            TraverseOrder::BottomUp => {
                let traversed_depth_first = self.types.try_map_state(
                    |ty, state| Ok(Box::new(ty.traverse(f, state, order)?)),
                    |rrows, state| rrows.traverse(f, state, order),
                    |erows, _| Ok(erows),
                    state,
                )?;

                f(Types::from(traversed_depth_first), state)
            }
        }
    }
}

impl Traverse<RichTerm> for Types {
    fn traverse<F, S, E>(self, f: &F, state: &mut S, order: TraverseOrder) -> Result<Self, E>
    where
        F: Fn(RichTerm, &mut S) -> Result<RichTerm, E>,
    {
        let f_on_type = |ty: Types, s: &mut S| match ty.types {
            TypeF::Flat(t) => t.traverse(f, s, order).map(|t| Types::from(TypeF::Flat(t))),
            _ => Ok(ty),
        };

        self.traverse(&f_on_type, state, order)
    }
}

impl Display for RecordRows {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            RecordRowsF::Extend { ref row, ref tail } => {
                write!(f, "{}: {}", row.id, row.types)?;

                match tail.0 {
                    RecordRowsF::Extend { .. } => write!(f, ", {tail}"),
                    _ => write!(f, "{tail}"),
                }
            }
            RecordRowsF::Empty => Ok(()),
            RecordRowsF::TailVar(id) => write!(f, " ; {id}"),
            RecordRowsF::TailDyn => write!(f, " ; Dyn"),
        }
    }
}

impl Display for EnumRows {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            EnumRowsF::Extend { ref row, ref tail } => {
                write!(f, "`{row}")?;

                match tail.0 {
                    EnumRowsF::Extend { .. } => write!(f, ", {tail}"),
                    _ => write!(f, "{tail}"),
                }
            }
            EnumRowsF::Empty => Ok(()),
            EnumRowsF::TailVar(id) => write!(f, " ; {id}"),
        }
    }
}

impl Display for Types {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.types {
            TypeF::Dyn => write!(f, "Dyn"),
            TypeF::Number => write!(f, "Number"),
            TypeF::Bool => write!(f, "Bool"),
            TypeF::String => write!(f, "String"),
            TypeF::Array(ty) => {
                write!(f, "Array ")?;

                if ty.fmt_is_atom() {
                    write!(f, "{ty}")
                } else {
                    write!(f, "({ty})")
                }
            }
            TypeF::Symbol => write!(f, "Sym"),
            TypeF::Flat(ref t) => write!(f, "{}", t.pretty_print_cap(32)),
            TypeF::Var(var) => write!(f, "{var}"),
            TypeF::Forall { var, ref body, .. } => {
                let mut curr: &Types = body.as_ref();
                write!(f, "forall {var}")?;
                while let Types {
                    types: TypeF::Forall { var, ref body, .. },
                    ..
                } = curr
                {
                    write!(f, " {var}")?;
                    curr = body;
                }
                write!(f, ". {curr}")
            }
            TypeF::Enum(row) => write!(f, "[|{row}|]"),
            TypeF::Record(row) => write!(f, "{{{row}}}"),
            TypeF::Dict(ty) => write!(f, "{{_: {ty}}}"),
            TypeF::Arrow(dom, codom) => match dom.types {
                TypeF::Arrow(_, _) | TypeF::Forall { .. } => write!(f, "({dom}) -> {codom}"),
                _ => write!(f, "{dom} -> {codom}"),
            },
            TypeF::Wildcard(_) => write!(f, "_"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Types;
    use crate::parser::grammar::TermParser;
    use crate::parser::lexer::Lexer;
    use crate::term::Term;
    use codespan::Files;

    /// Parse a type represented as a string.
    fn parse_type(s: &str) -> Types {
        use crate::term::TypeAnnotation;

        // Wrap the type in a contract to have it accepted by the parser.
        let wrapper = format!("null | {s}");
        println!("{wrapper}");
        let id = Files::new().add("<test>", wrapper.clone());

        let rt = TermParser::new()
            .parse_term(id, Lexer::new(&wrapper))
            .unwrap();

        match rt.term.into_owned() {
            Term::Annotated(TypeAnnotation { mut contracts, .. }, _) if contracts.len() == 1 => {
                contracts.remove(0).types
            }
            _ => panic!("types::test::parse_type(): expected contract"),
        }
    }

    /// Take a string representation of a type, parse it, and assert that formatting it gives the
    /// same string as the original argument.
    ///
    /// Note that there are infinitely many string representations of the same type since, for
    /// example, spaces are ignored: for the outcome of this function to be meaningful, the
    /// original type must be written in the same way as types are formatted.
    fn assert_format_eq(s: &str) {
        let ty = parse_type(s);
        assert_eq!(s, &format!("{ty}"));
    }

    #[test]
    fn types_pretty_printing() {
        assert_format_eq("Number");
        assert_format_eq("Number -> Number");
        assert_format_eq("(Number -> Number) -> (Number -> Number) -> Number -> Number");
        assert_format_eq("((Number -> Number) -> Number) -> Number");
        assert_format_eq("Number -> (forall a. a -> String) -> String");

        assert_format_eq("{_: String}");
        assert_format_eq("{_: (String -> String) -> String}");

        assert_format_eq("{x: (Bool -> Bool) -> Bool, y: Bool}");
        assert_format_eq("forall r. {x: Bool, y: Bool, z: Bool ; r}");
        assert_format_eq("{x: Bool, y: Bool, z: Bool}");

        assert_format_eq("[|`a, `b, `c, `d|]");
        assert_format_eq("forall r. [|`tag1, `tag2, `tag3 ; r|]");

        assert_format_eq("Array Number");
        assert_format_eq("Array (Array Number)");
        assert_format_eq("Number -> Array (Array String) -> Number");
        assert_format_eq("Array (Number -> Number)");
        assert_format_eq("Array (Array (Array Dyn) -> Number)");

        assert_format_eq("_");
        assert_format_eq("_ -> _");
        assert_format_eq("{x: _, y: Bool}");
        assert_format_eq("{_: _}");
    }
}
