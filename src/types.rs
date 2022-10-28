//! Define the Nickel type system.
//!
//! # Base types
//!
//! - Num: a floating-point number
//! - Bool: a boolean
//! - Str: a string literal
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
//! let f = Promise(forall a. { myField : Num, a} -> Num, fun rec => rec.myField)
//! ```
//!
//! The type `{ myField : Num, a }` indicates that any argument must have at least the field
//! `myField` of type `Num`, but may contain any other fields (or no additional field at all).
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
    error::{ParseError, ParseErrors, TypecheckError},
    identifier::Ident,
    mk_app, mk_fun,
    term::make as mk_term,
    term::{record::RecordData, RichTerm, Term, TraverseOrder},
};

use std::{
    collections::HashMap,
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
#[derive(Clone, PartialEq, Debug)]
pub struct RecordRowF<Ty> {
    pub id: Ident,
    pub types: Ty,
}

/// An enum row, which is just an identifier. An enum type is a set of identifiers, represented as
/// a sequence of `EnumRow`s, ending potentially with a type variable tail position.
///
/// `EnumRowF` is the same as `EnumRow` and doesn't have any type parameter. We introduce the alias
/// nontheless for consistency with other parametrized type definitions. See [`TypeF`] for more
/// details.
pub type EnumRowF = Ident;
pub type EnumRow = EnumRowF;

/// Generic sequence of record rows potentially with a type variable or `Dyn` in tail position.
/// Depending on the instantiation of `R` and `R`s, `RowsF` can represent both enum rows or records
/// rows.
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
#[derive(Clone, PartialEq, Debug)]
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
///   a wrapper around of `EnumRowsF`.
#[derive(Clone, PartialEq, Debug)]
pub enum EnumRowsF<ERows> {
    Empty,
    Extend { row: EnumRowF, tail: ERows },
    TailVar(Ident),
}

/// The kind of a quantified type variable.
///
/// Nickel uses several form of polymorphism. A type variable can be substituted for a type, as in
/// `id : forall a. a -> a`, for record rows as in `acess_foo : forall a . {foo : Num; a} -> Num}`,
/// or for enum rows. This information is implicit in the source syntax: we don't require users to
/// write e.g. `forall a :: Type` or `forall a :: Rows`. But the kind of variable is required for
/// the typechecker. It is thus determined during parsing and stored as `VarKind` where type
/// variable are introduced, that is, on forall quantifiers.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum VarKind {
    Type,
    EnumRow,
    RecordRow,
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
/// `Ty` is also called recursive unfolding throughout the documentation. By defining `struct
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
/// ```rust
///  pub enum UnifType {
///    UnifVar(VarId),
///    Concrete(TypeF<Box<UnifType> /*, .. */>),
///    // ..
///  }
/// ```
///
/// We get something that look like normal Nickel types, except that each node can also be a
/// unification variable as well.
///
/// ## Motivation 2: recursion schemes
///
/// This definition is actually in the style of recursion schemes. Pedantically, `TypeF` (hence the
/// `F` suffix), but the formal details aren't so important: keep in mind that the `F` suffix means
/// that the corresponding type definition where the recursive occurrences of subtrees (and enum
/// rows and records rows as well) are replaced by generic parameters.
///
/// The usual motivation for recursion schemes is that they allow for elegant and simple definition
/// of recursive transformation over trees (here, `TypeF`, and more general anything with `F`
/// suffix) in term of simple appropriate chaining of `map` and folding/unfolding operations. A
/// good example is the definition of `[Types::traverse]`. Although [`crate::term::Term`] isn't
/// currently defined as functors per se, the way program transformation are written is in the same
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
    /// The dynamic type, or unitype. Affected to values which actual type is not statically known
    /// or checked.
    Dyn,
    /// A floating point number.
    Num,
    /// A boolean.
    Bool,
    /// A string literal.
    Str,
    /// A symbol.
    ///
    /// See [`crate::term::Term::Sealed`].
    Sym,
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
// "tying" the note. We have to put `Box` in the appropriate positions (otherwise, Rust will
// complain that the type has an infinite size), but also avoid putting more than necessary.
//
// For example, `RecordRows` contains a `RecordRow`. The latter doesn't need to be boxed, because a
// `RecordRow` itself potentially contains occurrences of `Types` and `RecordRows`, which need to
// be boxed. Hence, we don't need to additionally box `RecordRow`.

/// Concrete, recursive definition for enum rows.
pub struct EnumRows(pub EnumRowsF<Box<EnumRows>>);
/// Concrete, recursive definition for a record row.
pub type RecordRow = RecordRowF<Box<Types>>;
#[derive(Clone, PartialEq, Debug)]
/// Concrete, recursive definition for record rows.
pub struct RecordRows(pub RecordRowsF<RecordRow, Box<RecordRows>>);
/// Concrete, recursive type for a Nickel type.
#[derive(Clone, PartialEq, Debug)]
pub struct Types(pub TypeF<Box<Types>, RecordRows, EnumRows>);

impl<Ty, RRows> RecordRowsF<Ty, RRows> {
    // TODO: doc
    pub fn try_map<TyO, RRowsO, FTy, FRRows, E>(
        self,
        mut f_ty: FTy,
        mut f_rrows: FRRows,
    ) -> Result<RecordRowsF<TyO, RRowsO>, E>
    where
        FTy: FnMut(Ty) -> Result<TyO, E>,
        FRRows: FnMut(RRows) -> Result<RRowsO, E>,
    {
        match self {
            RecordRowsF::Empty => Ok(RecordRowsF::Empty),
            RecordRowsF::Extend { row: RecordRowF { id, types }, tail } => Ok(RecordRowsF::Extend {
                row: RecordRowF { id, types: f_ty(types)?},
                tail: f_rrows(tail)?,
            }),
            RecordRowsF::TailDyn => Ok(RecordRowsF::TailDyn),
            RecordRowsF::TailVar(id) => Ok(RecordRowsF::TailVar(id)),
        }
    }

    // TODO: doc
    pub fn map<TyO, RRowsO, FTy, FRRows>(self, mut f_rrow: FR, mut f_rrows: FRRows) -> RecordRowsF<RO, RRowsO>
    where
        FR: FnMut(Ty) -> RO,
        FRRows: FnMut(RRows) -> RRowsO,
    {
        let f_rrow_lifted = |rrow: Ty| -> Result<RO, ()> { Ok(f_rrow(rrow)) };
        let f_rrows_lifted = |rrows: RRows| -> Result<RRowsO, ()> { Ok(f_rrows(rrows)) };
        self.try_map(f_rrow_lifted, f_rrows_lifted).unwrap()
    }
}

impl<ERows> EnumRowsF<ERows> {
    // We would like to express that RRows and ERows are of the form RRows<Ty> and ERows<Ty>,
    // because we lack higher-kinded types. Thus, we need to provide three mapping functions, which
    // is a bit painful :(
    pub fn try_map<ERowsO, FERows, E>(
        self,
        mut f_erows: FERows,
    ) -> Result<EnumRowsF<ERowsO>, E>
    where
        FERows: FnMut(ERows) -> Result<ERowsO, E>,
    {
        match self {
            EnumRowsF::Empty => Ok(EnumRowsF::Empty),
            EnumRowsF::Extend { row, tail } => Ok(EnumRowsF::Extend {
                row,
                tail: f_erows(tail)?,
            }),
            EnumRowsF::TailVar(id) => Ok(EnumRowsF::TailVar(id)),
        }
    }

    // TODO: doc
    pub fn map<ERowsO, FERows>(self, mut f_erows: ERows) -> EnumRowsF<ERowsO>
    where
        FERows: FnMut(ERows) -> ERowsO,
    {
        let f_erows_lifted = |erows: ERows| -> Result<ERowsO, ()> { Ok(f_erows(erows)) };
        self.try_map(f_erows_lifted).unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct UnboundTypeVariableError(pub Ident);

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

// /// Concrete, recursive type for a Nickel type.
// // #[derive(Clone, PartialEq, Debug)]
// // pub struct Types(pub TypeF<Box<Types>>);
//
// /// An item as returned by an iterator over a row type.
// ///
// /// The parameter `T` is there because row iterators are implemented for different flavours of
// /// [`TypeF`] (currently for [`Types`] and [`crate::typewrapper::TypeWrapper`]).
// pub enum RowIteratorItem<'a, T> {
//     /// A non-empty tail.
//     Tail(&'a T),
//     /// A row binding.
//     Row(&'a Ident, Option<&'a T>),
// }
//
// /// An iterator over a row type, returning items by reference.
// pub struct RowIterator<'a, T> {
//     pub(crate) next: Option<&'a T>,
// }
//
// impl<'a> Iterator for RowIterator<'a, Types> {
//     type Item = RowIteratorItem<'a, Types>;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         self.next.and_then(|next| match next {
//             Types(TypeF::RowEmpty) => None,
//             Types(TypeF::RowExtend(id, ty_row, tail)) => {
//                 self.next = Some(tail);
//                 Some(RowIteratorItem::Row(id, ty_row.as_ref().map(Box::as_ref)))
//             }
//             ty => {
//                 self.next = None;
//                 Some(RowIteratorItem::Tail(ty))
//             }
//         })
//     }
// }

impl EnumRows {
    fn subcontract(
        &self,
        mut h: HashMap<Ident, (RichTerm, RichTerm)>,
        pol: bool,
        sy: &mut i32,
    ) -> Result<RichTerm, UnboundTypeVariableError> {
        todo!()
    }

    /// Determine if a type is an atom, that is a either an atom or a type delimited by specific
    /// markers (such as a row type). Used in formatting to decide if parentheses need to be
    /// inserted during pretty pretting.
    pub fn fmt_is_atom(&self) -> bool {
        matches!(self.0, EnumRowsF::TailVar(_))
    }

    /// Apply a transformation on a whole type by mapping a faillible function `f` on each node in
    /// manner as prescribed by the order.
    /// `f` may return a generic error `E` and use the state `S` which is passed around.
    pub fn traverse<FTy, S, E>(
        self,
        f: &mut FTy,
        state: &mut S,
        order: TraverseOrder,
    ) -> Result<Self, E> {
        todo!()
    }
}

impl RecordRows {
    // TODO: doc
    fn subcontract(
        &self,
        mut h: HashMap<Ident, (RichTerm, RichTerm)>,
        pol: bool,
        sy: &mut i32,
    ) -> Result<RichTerm, UnboundTypeVariableError> {
        todo!()
    }

    /// Find a binding in a record row type. Return `None` if there is no such binding, if the type
    /// is not a row type, or if the row is an enum row.
    pub fn row_find(&self, ident: &Ident) -> Option<Self> {
        todo!()
        //TODO: just use iterator stuff?
        // match &self.0 {
        //     TypeF::RowExtend(id, Some(ty), _) if *id == *ident => Some((**ty).clone()),
        //     TypeF::RowExtend(_, _, tail) => tail.row_find(ident),
        //     _ => None,
        // }
    }

    /// Find a nested binding in a record row type. The nested field is given as a list of
    /// successive fields, that is, as a path. Return `None` if there is no such binding, if the
    /// type is not a row type, or if the final row is an enum row.
    ///
    /// # Example
    ///
    /// - self: ` { {| a : { {| b : Num |} } |} }`
    /// - path: `["a", "b"]`
    /// - result: `Some(Num)`
    pub fn row_find_path(&self, path: &[Ident]) -> Option<Self> {
        todo!()
        //TODO: just use iterator stuff?
        // if path.is_empty() {
        //     return None;
        // }
        //
        // let next = self.row_find(&path[0]);
        //
        // if path.len() == 1 {
        //     next
        // } else {
        //     match next {
        //         Some(ty) => ty.row_find_path(&path[1..]),
        //         _ => None,
        //     }
        // }
    }

    /// Determine if a type is an atom, that is a either an atom or a type delimited by specific
    /// markers (such as a row type). Used in formatting to decide if parentheses need to be
    /// inserted during pretty pretting.
    pub fn fmt_is_atom(&self) -> bool {
        matches!(self.0, RecordRowsF::TailDyn | RecordRowsF::TailVar(_))
    }

    /// Apply a transformation on a whole type by mapping a faillible function `f` on each node in
    /// manner as prescribed by the order.
    /// `f` may return a generic error `E` and use the state `S` which is passed around.
    pub fn traverse<FTy, S, E>(
        self,
        f: &mut FTy,
        state: &mut S,
        order: TraverseOrder,
    ) -> Result<Self, E> {
        todo!()
    }
}

impl Types {
    /// Return the contract corresponding to a type, either as a function or a record. Said
    /// contract must then be applied using the `Assume` primitive operation.
    pub fn contract(&self) -> Result<RichTerm, UnboundTypeVariableError> {
        let mut sy = 0;
        self.subcontract(HashMap::new(), true, &mut sy)
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

        fn get_var(
            vars: &HashMap<Ident, (RichTerm, RichTerm)>,
            id: &Ident,
            pol: bool,
        ) -> Result<RichTerm, UnboundTypeVariableError> {
            let (pos, neg) = vars
                .get(id)
                .ok_or_else(|| UnboundTypeVariableError(id.clone()))?;
            if pol {
                Ok(pos.clone())
            } else {
                Ok(neg.clone())
            }
        }

        let ctr = match self.0 {
            TypeF::Dyn => contract::dynamic(),
            TypeF::Num => contract::num(),
            TypeF::Bool => contract::bool(),
            TypeF::Str => contract::string(),
            //TODO: optimization: have a specialized contract for `Array Dyn`, to avoid mapping an
            //always successful contract on each element.
            TypeF::Array(ref ty) => mk_app!(contract::array(), ty.subcontract(h, pol, sy)?),
            TypeF::Sym => panic!("Are you trying to check a Sym at runtime?"),
            TypeF::Arrow(ref s, ref t) => mk_app!(
                contract::func(),
                s.subcontract(h.clone(), !pol, sy)?,
                t.subcontract(h, pol, sy)?
            ),
            TypeF::Flat(ref t) => t.clone(),
            TypeF::Var(ref id) => get_var(&h, id, true)?,
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

                h.insert(var.clone(), (inst_var, inst_tail));
                *sy += 1;
                body.subcontract(h, pol, sy)?
            }
            //TODO: IMPLEMENT THAT
            // AbsType::RowEmpty() | AbsType::RowExtend(..) => contract::fail(),
            // AbsType::Enum(ref r) => {
            //     let mut cases = HashMap::new();
            //     let mut has_tail = false;
            //     let value_arg = Ident::from("x");
            //     let label_arg = Ident::from("l");
            //
            //     for row in r.iter_as_rows() {
            //         match row {
            //             RowIteratorItem::Row(id, _ty) => {
            //                 debug_assert!(_ty.is_none());
            //                 cases.insert(id.clone(), mk_term::var(value_arg.clone()));
            //             }
            //             RowIteratorItem::Tail(tail) => {
            //                 // We only expect a type variable in tail position
            //                 debug_assert!(matches!(tail, Types(AbsType::Var(_))));
            //                 has_tail = true;
            //                 break;
            //             }
            //         }
            //     }
            //
            //     // If the enum type has a tail, the tail must be a universally quantified variable,
            //     // and this means that the tag can be anything.
            //     let case_body = if has_tail {
            //         mk_term::var(value_arg.clone())
            //     }
            //     // Otherwise, we build a switch with all the tags as cases, which just returns the
            //     // original argument, and a default case that blames.
            //     //
            //     // For example, for an enum type [| `foo, `bar, `baz |], the `case` function looks
            //     // like:
            //     //
            //     // ```
            //     // fun l x =>
            //     //   switch {
            //     //     `foo => x,
            //     //     `bar => x,
            //     //     `baz => x,
            //     //     _ => $enum_fail l
            //     //   } x
            //     // ```
            //     else {
            //         RichTerm::from(Term::Switch(
            //             mk_term::var(value_arg.clone()),
            //             cases,
            //             Some(mk_app!(
            //                 contract::enum_fail(),
            //                 mk_term::var(label_arg.clone())
            //             )),
            //         ))
            //     };
            //     let case = mk_fun!(label_arg, value_arg, case_body);
            //
            //     mk_app!(contract::enums(), case)
            // }
            // AbsType::Record(ref ty) => {
            //     // We begin by building a record whose arguments are contracts
            //     // derived from the types of the statically known fields.
            //     let mut row = ty;
            //     let mut fcs = HashMap::new();
            //
            //     while let AbsType::RowExtend(id, Some(ty), rest) = &row.0 {
            //         fcs.insert(*id, ty.subcontract(h.clone(), pol, sy)?);
            //         row = rest
            //     }
            //
            //     // Now that we've dealt with the row extends, we just need to
            //     // work out the tail.
            //     let tail = match &row.0 {
            //         AbsType::RowEmpty() => contract::empty_tail(),
            //         AbsType::Dyn() => contract::dyn_tail(),
            //         AbsType::Var(id) => get_var(&h, id, false)?,
            //         ty => panic!(
            //             "types::subcontract(): invalid row tail {:?}",
            //             Types(ty.clone())
            //         ),
            //     };
            //
            //     let rec = RichTerm::from(Term::Record(RecordData::with_fields(fcs)));
            //
            //     mk_app!(contract::record(), rec, tail)
            // }
            // AbsType::Dict(ref ty) => {
            TypeF::Enum(ref erows) => erows.subcontract(h, pol, sy)?,
            // AbsType::Enum(ref r) => {
            //     let mut cases = HashMap::new();
            //     let mut has_tail = false;
            //     let value_arg = Ident::from("x");
            //     let label_arg = Ident::from("l");
            //
            //     for row in r.iter_as_rows() {
            //         match row {
            //             RowIteratorItem::Row(id, _ty) => {
            //                 debug_assert!(_ty.is_none());
            //                 cases.insert(id.clone(), mk_term::var(value_arg.clone()));
            //             }
            //             RowIteratorItem::Tail(tail) => {
            //                 // We only expect a type variable in tail position
            //                 debug_assert!(matches!(tail, Types(AbsType::Var(_))));
            //                 has_tail = true;
            //                 break;
            //             }
            //         }
            //     }
            //
            //     // If the enum type has a tail, the tail must be a universally quantified variable,
            //     // and this means that the tag can be anything.
            //     let case_body = if has_tail {
            //         mk_term::var(value_arg.clone())
            //     }
            //     // Otherwise, we build a switch with all the tags as cases, which just returns the
            //     // original argument, and a default case that blames.
            //     //
            //     // For example, for an enum type [| `foo, `bar, `baz |], the `case` function looks
            //     // like:
            //     //
            //     // ```
            //     // fun l x =>
            //     //   switch {
            //     //     `foo => x,
            //     //     `bar => x,
            //     //     `baz => x,
            //     //     _ => $enum_fail l
            //     //   } x
            //     // ```
            //     else {
            //         RichTerm::from(Term::Switch(
            //             mk_term::var(value_arg.clone()),
            //             cases,
            //             Some(mk_app!(
            //                 contract::enum_fail(),
            //                 mk_term::var(label_arg.clone())
            //             )),
            //         ))
            //     };
            //     let case = mk_fun!(label_arg, value_arg, case_body);
            //
            //     mk_app!(contract::enums(), case)
            // }
            TypeF::Record(ref rrows) => rrows.subcontract(h, pol, sy)?,
            // AbsType::Record(ref ty) => {
            //     // We begin by building a record whose arguments are contracts
            //     // derived from the types of the statically known fields.
            //     let mut row = ty;
            //     let mut fcs = HashMap::new();
            //
            //     while let AbsType::RowExtend(id, Some(ty), rest) = &row.0 {
            //         fcs.insert(*id, ty.subcontract(h.clone(), pol, sy)?);
            //         row = rest
            //     }
            //
            //     // Now that we've dealt with the row extends, we just need to
            //     // work out the tail.
            //     let tail = match &row.0 {
            //         AbsType::RowEmpty() => contract::empty_tail(),
            //         AbsType::Dyn() => contract::dyn_tail(),
            //         AbsType::Var(id) => get_var(&h, id, false)?,
            //         ty => panic!(
            //             "types::subcontract(): invalid row tail {:?}",
            //             Types(ty.clone())
            //         ),
            //     };
            //
            //     let rec = RichTerm::from(Term::Record(fcs, Default::default()));
            //
            //     mk_app!(contract::record(), rec, tail)
            // }
            TypeF::Dict(ref ty) => {
                mk_app!(contract::dyn_record(), ty.subcontract(h, pol, sy)?)
            }
            TypeF::Wildcard(_) => contract::dynamic(),
        };

        Ok(ctr)
    }

    /// markers (such as a row type). Used in formatting to decide if parentheses need to be
    /// inserted during pretty pretting.
    pub fn fmt_is_atom(&self) -> bool {
        use TypeF::*;

        match &self.0 {
            Dyn | Num | Bool | Str | Var(_) => true,
            Flat(rt) if matches!(*rt.term, Term::Var(_)) => true,
            _ => false,
        }
    }

    /// Apply a transformation on a whole type by mapping a faillible function `f` on each node in
    /// manner as prescribed by the order.
    /// `f` may return a generic error `E` and use the state `S` which is passed around.
    pub fn traverse<FTy, S, E>(
        self,
        f: &mut FTy,
        state: &mut S,
        order: TraverseOrder,
    ) -> Result<Self, E>
    where
        FTy: FnMut(Types, &mut S) -> Result<Types, E>,
    {
        let ty = match order {
            TraverseOrder::TopDown => f(self, state)?,
            TraverseOrder::BottomUp => self,
        };

        let abs_ty = match ty.0 {
            TypeF::Dyn
            | TypeF::Num
            | TypeF::Bool
            | TypeF::Str
            | TypeF::Sym
            | TypeF::Var(_)
            | TypeF::Flat(_)
            | TypeF::Wildcard(_) => Ok(ty.0),
            TypeF::Forall {
                var,
                var_kind,
                body,
            } => (*body).traverse(f, state, order).map(|ty| TypeF::Forall {
                var,
                var_kind,
                body: Box::new(ty),
            }),
            TypeF::Enum(erows) => erows.traverse(f, state, order).map(TypeF::Enum),
            TypeF::Record(rrows) => rrows.traverse(f, state, order).map(TypeF::Record),
            TypeF::Dict(ty_inner) => (*ty_inner)
                .traverse(f, state, order)
                .map(Box::new)
                .map(TypeF::Dict),
            TypeF::Array(ty_inner) => (*ty_inner)
                .traverse(f, state, order)
                .map(Box::new)
                .map(TypeF::Array),
            TypeF::Arrow(ty1, ty2) => {
                let ty1 = (*ty1).traverse(f, state, order)?;
                let ty2 = (*ty2).traverse(f, state, order)?;
                Ok(TypeF::Arrow(Box::new(ty1), Box::new(ty2)))
            } // TypeF::RowExtend(id, ty_row, tail) => {
              //     let ty_row = ty_row
              //         .map(|ty| (*ty).traverse(f, state, order))
              //         .transpose()?;
              //     let tail = (*tail).traverse(f, state, order)?;
              //     Ok(TypeF::RowExtend(id, ty_row.map(Box::new), Box::new(tail)))
              // }
        }?;

        let result = Types(abs_ty);

        match order {
            TraverseOrder::TopDown => Ok(result),
            TraverseOrder::BottomUp => f(result, state),
        }
    }

    // /// Create an iterator on rows represented by this type.
    // ///
    // /// The iterator continues as long as the next item is of the form `RowExtend(..)`, and
    // /// stops once it reaches `RowEmpty` (which ends iteration), or something else which is not a
    // /// `RowExtend` (which produces a last item [`typecheck::RowIteratorItem::Tail`]).
    // pub fn iter_as_rows(&self) -> RowIterator<'_, Types> {
    //     RowIterator { next: Some(self) }
    // }
}

impl Display for RecordRows {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
    }
}

impl Display for EnumRows {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
    }
}

impl Display for Types {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            TypeF::Dyn => write!(f, "Dyn"),
            TypeF::Num => write!(f, "Num"),
            TypeF::Bool => write!(f, "Bool"),
            TypeF::Str => write!(f, "Str"),
            TypeF::Array(ty) => {
                write!(f, "Array ")?;

                if ty.fmt_is_atom() {
                    write!(f, "{}", ty)
                } else {
                    write!(f, "({})", ty)
                }
            }
            TypeF::Sym => write!(f, "Sym"),
            TypeF::Flat(ref t) => write!(f, "{}", t.pretty_print_cap(32)),
            TypeF::Var(var) => write!(f, "{}", var),
            TypeF::Forall { var, ref body, .. } => {
                let mut curr: &Types = body.as_ref();
                write!(f, "forall {}", var)?;
                while let Types(TypeF::Forall { var, ref body, .. }) = curr {
                    write!(f, " {}", var)?;
                    curr = body;
                }
                write!(f, ". {}", curr)
            }
            TypeF::Enum(row) => write!(f, "[|{}|]", row),
            TypeF::Record(row) => write!(f, "{{{}}}", row),
            TypeF::Dict(ty) => write!(f, "{{_: {}}}", ty),
            // TypeF::RowExtend(id, ty_opt, tail) => {
            //     if let Some(ty) = ty_opt {
            //         write!(f, "{}: {}", id, ty)?;
            //     } else {
            //         write!(f, "`{}", id)?;
            //     }
            //
            //     match tail.0 {
            //         TypeF::RowEmpty => write!(f, "{}", tail),
            //         TypeF::Var(_) => write!(f, " ; {}", tail),
            //         TypeF::Dyn => write!(f, " ; Dyn"),
            //         _ => write!(f, ", {}", tail),
            //     }
            // }
            TypeF::Arrow(dom, codom) => match dom.0 {
                TypeF::Arrow(_, _) => write!(f, "({}) -> {}", dom, codom),
                _ => write!(f, "{} -> {}", dom, codom),
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
        use crate::term::MetaValue;

        // Wrap the type in a contract to have it accepted by the parser.
        let wrapper = format!("null | {}", s);
        println!("{}", wrapper);
        let id = Files::new().add("<test>", wrapper.clone());

        let rt = TermParser::new()
            .parse_term(id, Lexer::new(&wrapper))
            .unwrap();

        match rt.term.into_owned() {
            Term::MetaValue(MetaValue { mut contracts, .. }) if contracts.len() == 1 => {
                contracts.remove(0).types
            }
            _ => panic!("types::test::parse_type(): expected contract"),
        }
    }

    /// Take a string representation of a type, parse it, and assert that formatting it gives the
    /// same string as the original argument.
    ///
    /// Note that their are infintely many string representations of the same type since, for
    /// example, spaces are ignored: for the outcome of this function to be meaningful, the
    /// original type must be written in the same way as types are formatted.
    fn assert_format_eq(s: &str) {
        let ty = parse_type(s);
        assert_eq!(s, &format!("{}", ty));
    }

    #[test]
    fn types_pretty_printing() {
        assert_format_eq("Num");
        assert_format_eq("Num -> Num");
        assert_format_eq("(Num -> Num) -> (Num -> Num) -> Num -> Num");
        assert_format_eq("((Num -> Num) -> Num) -> Num");

        assert_format_eq("{_: Str}");
        assert_format_eq("{_: (Str -> Str) -> Str}");

        assert_format_eq("{x: (Bool -> Bool) -> Bool, y: Bool}");
        assert_format_eq("forall r. {x: Bool, y: Bool, z: Bool ; r}");
        assert_format_eq("{x: Bool, y: Bool, z: Bool}");

        assert_format_eq("[|`a, `b, `c, `d|]");
        assert_format_eq("forall r. [|`tag1, `tag2, `tag3 ; r|]");

        assert_format_eq("Array Num");
        assert_format_eq("Array (Array Num)");
        assert_format_eq("Num -> Array (Array Str) -> Num");
        assert_format_eq("Array (Num -> Num)");
        assert_format_eq("Array (Array (Array Dyn) -> Num)");

        assert_format_eq("_");
        assert_format_eq("_ -> _");
        assert_format_eq("{x: _, y: Bool}");
        assert_format_eq("{_: _}");
    }
}
