//! Implementation of the typechecker.
//!
//! # Mode
//!
//! Typechecking can be made in to different modes:
//! - **Strict**: correspond to traditional typechecking in statically typed languages. This
//!   happens inside a statically typed block. Statically typed blocks are introduced by the type
//!   ascription operator `:`, as in `1 + 1 : Num` or `let f : Num -> Num = fun x => x + 1 in ..`. This is
//!   implemented by [`type_check_`] and variants.
//! - **Non strict**: do not enforce any typing but continue to traverse the AST looking for other
//!   typed blocks to typecheck and store the annotations of let bindings in the environment. This is
//!   implemented by the [`walk`] function.
//!
//! The algorithm starts in non strict mode. It is switched to strict mode when entering a
//! statically typed block (an expression annotated with a type), and is switched back to
//! non-strict mode when entering an expression annotated with a contract. Type and contract
//! annotations thus serve both another purpose beside enforcing a type or a contract, which is to
//! switch the typechecking mode.
//!
//! # Type inference
//!
//! Type inference is done via a form of bidirectional typechecking coupled with unification, in
//! the same spirit as GHC (Haskell), albeit the type system of Nickel is much simpler. The type of
//! un-annotated let-bound expressions (the type of `bound_exp` in `let x = bound_exp in body`) is
//! inferred in strict mode, but it is never implicitly generalized. For example, the following
//! program is rejected:
//!
//! ```nickel
//! # Rejected
//! let id = fun x => x in seq (id "a") (id 5) : Num
//! ```
//!
//! Indeed, `id` is given the type `_a -> _a`, where `_a` is a unification variable, but is not
//! generalized to `forall a. a -> a`. At the first call site, `_a` is unified with `Str`, and at the second
//! call site the typechecker complains that `5` is not of type `Str`.
//!
//! This restriction is on purpose, as generalization is not trivial to implement efficiently and
//! more importantly can interact with other components of the type system and type inference. If
//! polymorphism is required, the user can simply add annotation:
//!
//! ```nickel
//! # Accepted
//! let id : forall a. a -> a = fun x => x in seq (id "a") (id 5) : Num
//! ```
//!
//! In non-strict mode, the type of let-bound expressions is inferred in a shallow way (see
//! [`apparent_type`]).
use crate::types::EnumRowsIterator;
use crate::types::RecordRowsIterator;
use crate::{
    cache::ImportResolver,
    destruct::*,
    environment::Environment as GenericEnvironment,
    error::TypecheckError,
    identifier::Ident,
    term::{Contract, MetaValue, RichTerm, StrChunk, Term, TraverseOrder},
    types::{TypeF, RecordRowsF, EnumRowsF, RecordRowF, RecordRow, EnumRow, Types, RecordRows, EnumRows, VarKind, TraverseType, TraverseEnumRows, TraverseRecordRows},
    {mk_tyw_arrow, mk_tyw_enum, mk_tyw_enum_row, mk_tyw_record, mk_tyw_row},
};

use std::{
    collections::{HashMap, HashSet},
    convert::TryInto,
};

use self::linearization::{Linearization, Linearizer, StubHost};

pub mod error;
pub mod linearization;
pub mod operation;
pub mod reporting;
#[macro_use]
pub mod mk_typewrapper;
pub mod eq;

use eq::{SimpleTermEnvironment, TermEnvironment};
use error::*;
use operation::{get_bop_type, get_nop_type, get_uop_type};

/// The typing environment.
pub type Environment = GenericEnvironment<Ident, UnifType>;

/// Mapping from wildcard ID to inferred type
pub type Wildcards = Vec<Types>;

pub type VarId = usize;

pub type GenericUnifRecordRow<E : TermEnvironment + Clone> = RecordRowF<Box<GenericUnifType<E>>>;

#[derive(Clone, PartialEq, Debug)]
pub enum GenericUnifRecordRows<E : TermEnvironment + Clone> {
    Concrete(RecordRowsF<Box<GenericUnifType<E>>, Box<GenericUnifRecordRows<E>>>),
    Constant(VarId),
    UnifVar(VarId),
}

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
    /// A concrete type (like `Num` or `Str -> Str`).
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
                let converted: RecordRowsF<Box<Types>, Box<RecordRows>> = rrows.try_map(|uty| Ok(Box::new(GenericUnifType::try_into(*uty)?)), |urrows| {
                    let rrows: RecordRows = (*urrows).try_into()?;
                    Ok(Box::new(rrows))
                })?;
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
                let converted: EnumRowsF<Box<EnumRows>> = erows.try_map(|erows| Ok(Box::new(UnifEnumRows::try_into(*erows)?)))?;
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
                let converted: TypeF<Box<Types>, RecordRows, EnumRows> = ty.try_map(|uty_boxed| {
                    let ty: Types = (*uty_boxed).try_into()?;
                    Ok(Box::new(ty))
                }, GenericUnifRecordRows::try_into, UnifEnumRows::try_into)?;
                Ok(Types(converted))
            }
            GenericUnifType::Contract(t, _) => Ok(Types(TypeF::Flat(t))),
            _ => Err(()),
        }
    }
}

// As opposed to `UnifType` and `UnifRecordRows` which can contain contract, and thus need the
// additional environment parameter (see e.g. `from_record_rows`), we can convert enum rows
// directly to unif enum rows without additional data: instead of implementing a function
// `from_enum_rows`, we rather implement the more natural trait `From<EnumRows>`.
impl From<EnumRows> for UnifEnumRows {
    fn from(erows: EnumRows) -> Self {
        UnifEnumRows::Concrete(
            erows.0.map(|erows| Box::new(UnifEnumRows::from(*erows)))
        )
    }
}

impl UnifEnumRows {
    pub fn iter<'a>(&'a self) -> EnumRowsIterator<'a, UnifEnumRows> {
        EnumRowsIterator {
            erows: Some(self),
        }
    }
}

impl<E: TermEnvironment + Clone> GenericUnifRecordRows<E> {
    /// Create a TypeWrapper from a Types. Contracts are represented as the separate variant
    /// [`TypeWrapper::Contract`] which also stores a term environment, required for checking type
    /// equality involving contracts.
    pub fn from_record_rows(rrows: RecordRows, env: &E) -> Self {
        let f_rrow = |ty : Box<Types>| Box::new(GenericUnifType::from_type(*ty, env));
        let f_rrows = |rrows: Box<RecordRows>| { Box::new(GenericUnifRecordRows::from_record_rows(*rrows,env)) };

        GenericUnifRecordRows::Concrete(
            rrows.0.map(f_rrow, f_rrows)
        )
    }
}

impl<E: TermEnvironment> GenericUnifRecordRows<E> {
    pub fn iter<'a>(&'a self) -> RecordRowsIterator<'a, GenericUnifType<E>, GenericUnifRecordRows<E>> {
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
            GenericUnifType::Concrete(t) => GenericUnifType::Concrete(t.map(|ty| Box::new(ty.subst_type(id, to)), |rrows| rrows.subst_type(id, to), |erows| erows)),
            _ => self
        }
    }
}

impl<E: TermEnvironment> SubstType<E> for GenericUnifRecordRows<E> {
    fn subst_type(self, id: &Ident, to: &GenericUnifType<E>) -> Self {
        match self {
            GenericUnifRecordRows::Concrete(rrows) => GenericUnifRecordRows::Concrete(rrows.map(|ty| Box::new(ty.subst_type(id, to)), |rrows| Box::new(rrows.subst_type(id, to)))),
            _ => self
        }
    }
}

impl<E: TermEnvironment> SubstRRows<E> for GenericUnifType<E> {
    fn subst_rrows(self, id: &Ident, to: &GenericUnifRecordRows<E>) -> Self {
        match self {
            GenericUnifType::Concrete(t) => GenericUnifType::Concrete(t.map(|ty| Box::new(ty.subst_rrows(id, to)), |rrows| rrows.subst_rrows(id, to), |erows| erows)),
            _ => self
        }
    }
}

impl<E: TermEnvironment> SubstRRows<E> for GenericUnifRecordRows<E> {
    fn subst_rrows(self, id: &Ident, to: &GenericUnifRecordRows<E>) -> Self {
        match self {
            GenericUnifRecordRows::Concrete(RecordRowsF::TailVar(var_id)) if var_id == *id => to.clone(),
            GenericUnifRecordRows::Concrete(rrows) => GenericUnifRecordRows::Concrete(rrows.map(|ty| Box::new(ty.subst_rrows(id, to)), |rrows| Box::new(rrows.subst_rrows(id, to)))),
            _ => self
        }
    }
}

impl<E: TermEnvironment> SubstERows for GenericUnifType<E> {
    fn subst_erows(self, id: &Ident, to: &UnifEnumRows) -> Self {
        match self {
            GenericUnifType::Concrete(t) => GenericUnifType::Concrete(t.map(|ty| Box::new(ty.subst_erows(id, to)), |rrows| rrows.subst_erows(id, to), |erows| erows.subst_erows(id, to))),
            _ => self
        }
    }
}

impl<E: TermEnvironment> SubstERows for GenericUnifRecordRows<E> {
    fn subst_erows(self, id: &Ident, to: &UnifEnumRows) -> Self {
        match self {
            GenericUnifRecordRows::Concrete(rrows) => GenericUnifRecordRows::Concrete(rrows.map(|ty| Box::new(ty.subst_erows(id, to)), |rrows| Box::new(rrows.subst_erows(id, to)))),
            _ => self
        }
    }
}

impl SubstERows for UnifEnumRows {
    fn subst_erows(self, id: &Ident, to: &UnifEnumRows) -> Self {
        match self {
            UnifEnumRows::Concrete(EnumRowsF::TailVar(var_id)) if var_id == *id => to.clone(),
            UnifEnumRows::Concrete(rrows) => UnifEnumRows::Concrete(rrows.map(|erows| Box::new(erows.subst_erows(id, to)))),
            _ => self
        }
    }
}

impl<E: TermEnvironment + Clone> GenericUnifType<E> {
    /// Create a TypeWrapper from a Types. Contracts are represented as the separate variant
    /// [`TypeWrapper::Contract`] which also stores a term environment, required for checking type
    /// equality involving contracts.
    pub fn from_type(ty: Types, env: &E) -> Self {
        match ty.0 {
            TypeF::Flat(t) => GenericUnifType::Contract(t.clone(), env.clone()),
            ty => GenericUnifType::Concrete(
                ty.map(|ty_| Box::new(GenericUnifType::from_type(*ty_, env)),
                       |rrows| GenericUnifRecordRows::from_record_rows(rrows, env),
                       |erows| UnifEnumRows::from(erows),
            )),
        }
    }
}

pub type UnifRecordRow = GenericUnifRecordRow<SimpleTermEnvironment>;
pub type UnifRecordRows = GenericUnifRecordRows<SimpleTermEnvironment>;
pub type UnifType = GenericUnifType<SimpleTermEnvironment>;

impl UnifRecordRows {
    /// Extract the concrete type corresponding to a type wrapper. Free unification variables as well
    /// as type constants are replaced with the empty row.
    fn into_rrows(self, table: &UnifTable) -> RecordRows {
        match self {
            UnifRecordRows::UnifVar(p) => match table.root_rrows(p) {
                t @ UnifRecordRows::Concrete(_) => t.into_rrows(table),
                _ => RecordRows(RecordRowsF::Empty),
            },
            UnifRecordRows::Constant(_) => RecordRows(RecordRowsF::Empty),
            UnifRecordRows::Concrete(t) => {
                let mapped = t.map(|ty| Box::new(ty.into_type(table)), |rrows| Box::new(rrows.into_rrows(table)));
                RecordRows(mapped)
            }
        }
    }

}

impl UnifEnumRows {
    /// Extract the concrete type corresponding to a type wrapper. Free unification variables as well
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
}


impl UnifType {
    /// Create a TypeWrapper from an apparent type. As for [`from_type`], this function requires
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

    /// Extract the concrete type corresponding to a type wrapper. Free unification variables as well
    /// as type constants are replaced with the type `Dyn`.
    fn into_type(self, table: &UnifTable) -> Types {
        match self {
            UnifType::UnifVar(p) => match table.root_type(p) {
                t @ UnifType::Concrete(_) => t.into_type(table),
                _ => Types(TypeF::Dyn),
            },
            UnifType::Constant(_) => Types(TypeF::Dyn),
            UnifType::Concrete(t) => {
                let mapped = t.map(|btyp| Box::new(btyp.into_type(table)), |urrows| urrows.into_rrows(table), |uerows| uerows.into_erows(table));
                Types(mapped)
            }
            UnifType::Contract(t, _) => Types(TypeF::Flat(t)),
        }
    }
}

// This implementation assumes that `TypeF::Flat` is not possible. If TypeWrappers have been
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

pub enum GenericUnifRecordRowsIteratorItem<'a, E : TermEnvironment> {
    TailDyn,
    TailVar(&'a Ident),
    TailUnifVar(VarId),
    TailConstant(VarId),
    Row(RecordRowF<&'a GenericUnifType<E>>),
}
pub type UnifRecordRowsIteratorItem<'a> = GenericUnifRecordRowsIteratorItem<'a, SimpleTermEnvironment>;

impl<'a, E: TermEnvironment> Iterator for RecordRowsIterator<'a, GenericUnifType<E>, GenericUnifRecordRows<E>> {
    type Item = GenericUnifRecordRowsIteratorItem<'a, E>;

    fn next(&mut self) -> Option<Self::Item> {
        self.rrows.and_then(|next| match next {
            GenericUnifRecordRows::Concrete(rrows) => {
                match rrows {
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
                        Some(GenericUnifRecordRowsIteratorItem::TailVar(&id))
                    }
                    RecordRowsF::Extend { row, tail } => {
                        self.rrows = Some(&*tail);
                        Some(GenericUnifRecordRowsIteratorItem::Row(RecordRowF {
                            id: row.id.clone(),
                            types: row.types.as_ref(),
                        }))
                    }
                }
            }
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
            UnifEnumRows::Concrete(erows) => {
                match erows {
                    EnumRowsF::Empty => {
                        self.erows = None;
                        None
                    }
                    EnumRowsF::TailVar(id) => {
                        self.erows = None;
                        Some(UnifEnumRowsIteratorItem::TailVar(&id))
                    }
                    EnumRowsF::Extend { row, tail } => {
                        self.erows = Some(&*tail);
                        Some(UnifEnumRowsIteratorItem::Row(&row))
                    }
                }
            }
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

// impl<'a, E: TermEnvironment> Iterator for RowIterator<'a, GenericUnifType<E>> {
//     type Item = RowIteratorItem<'a, GenericUnifType<E>>;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         self.next.and_then(|next| match next {
//             GenericUnifType::Concrete(TypeF::Empty) => None,
//             GenericUnifType::Concrete(TypeF::RowExtend(id, ty_row, tail)) => {
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
pub fn mk_initial_ctxt(initial_env: &Vec<RichTerm>) -> Result<Context, EnvBuildError> {
    // Collect the bindings for each module, clone them and flatten the result to a single list.
    let bindings = initial_env
        .iter()
        .map(|rt| {
            if let Term::RecRecord(record, ..) = rt.as_ref() {
                Ok(record
                    .fields
                    .iter()
                    .map(|(id, rt)| (id.clone(), rt.clone())))
            } else {
                Err(EnvBuildError::NotARecord(rt.clone()))
            }
        })
        .collect::<Result<Vec<_>, EnvBuildError>>()?
        .into_iter()
        .flatten();

    let term_env = bindings
        .clone()
        .map(|(id, rt)| (id, (rt, SimpleTermEnvironment::new())))
        .collect();

    let type_env = bindings
        .map(|(id, rt)| (id.clone(), infer_record_type(rt.as_ref(), &term_env)))
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
            for (id, t) in &record.fields {
                let uty: UnifType = UnifType::from_apparent_type(
                    apparent_type(t.as_ref(), Some(env), Some(resolver)),
                    term_env,
                );
                env.insert(id.clone(), uty);
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
    names: &'a mut HashMap<usize, Ident>,
    /// A mapping from wildcard ID to unification variable.
    wildcard_vars: &'a mut Vec<UnifType>,
}

/// Immutable and owned data, required by the LSP to carry out specific analysis.
/// It is basically an owned-subset of the typecheking state.
pub struct Extra {
    pub table: UnifTable,
    pub names: HashMap<usize, Ident>,
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
    type_check_linearize(t, initial_ctxt, resolver, StubHost::<(), (), _>::new())
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
) -> Result<(Wildcards, LL::Completed), TypecheckError>
where
    LL: Linearizer<CompletionExtra = Extra>,
{
    let (mut table, mut names) = (UnifTable::new(), HashMap::new());
    let mut building = Linearization::new(LL::Building::default());
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
            .ok_or_else(|| TypecheckError::UnboundIdentifier(x.clone(), *pos))
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
            // The parameter of an un-annotated function is assigned the type `Dyn`.
            ctxt.type_env.insert(id.clone(), mk_typewrapper::dynamic());
            walk(state, ctxt, lin, linearizer, t)
        }
        Term::FunPattern(id, pat, t) => {
            if let Some(id) = id {
                ctxt.type_env.insert(id.clone(), binding_type(state, t.as_ref(), &ctxt, false));
            }

            inject_pat_vars(pat, &mut ctxt.type_env);
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
            ctxt.term_env.0.insert(x.clone(), (re.clone(), ctxt.term_env.clone()));

            if attrs.rec {
                ctxt.type_env.insert(x.clone(), ty_let.clone());
            }

            linearizer.retype_ident(lin, x, ty_let.clone());
            walk(state, ctxt.clone(), lin, linearizer.scope(), re)?;

            if !attrs.rec {
                ctxt.type_env.insert(x.clone(), ty_let);
            }

            walk(state, ctxt, lin, linearizer, rt)
        }
        Term::LetPattern(x, pat, re, rt) => {
            let ty_let = binding_type(state, re.as_ref(), &ctxt, false);
            walk(state, ctxt.clone(), lin, linearizer.scope(), re)?;

            if let Some(x) = x {
                linearizer.retype_ident(lin, x, ty_let.clone());
                ctxt.type_env.insert(x.clone(), ty_let);
            }

            inject_pat_vars(pat, &mut ctxt.type_env);

            walk(state, ctxt, lin, linearizer, rt)
        }
        Term::App(e, t) => {
            walk(state, ctxt.clone(), lin, linearizer.scope(), e)?;
            walk(state, ctxt, lin, linearizer, t)
        }
        Term::Switch(exp, cases, default) => {
            cases.values().chain(default.iter()).try_for_each(|case| {
                walk(state, ctxt.clone(), lin, linearizer.scope(), case)
            })?;

            walk(state, ctxt, lin, linearizer, exp)
        }
        Term::RecRecord(record, dynamic, ..) => {
            for (id, field) in record.fields.iter() {
                let binding_type = binding_type(
                    state,
                    field.as_ref(),
                    &ctxt,
                    false,
                );
                ctxt.type_env.insert(id.clone(), binding_type.clone());
                linearizer.retype_ident(lin, id, binding_type);
            }

            // We don't bind the fields in the term environment used to check for contract
            // equality. See the `Let` case above for more details on why such recursive bindings
            // are currently ignored.
            record.fields
                .iter()
                .map(|(_, t)| t)
                .chain(dynamic.iter().map(|(_, t)| t))
                .try_for_each(|t| -> Result<(), TypecheckError> {
                    walk(state, ctxt.clone(), lin, linearizer.scope(), t)
                })
        }
        Term::Record(record) => {
            record.fields
                .iter()
                .try_for_each(|(_, t)| -> Result<(), TypecheckError> {
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
        // An type annotation switches mode to check.
        Term::MetaValue(meta) => {
            meta.contracts.iter().chain(meta.types.iter()).try_for_each(|ty| walk_type(state, ctxt.clone(), lin, linearizer.scope_meta(), &ty.types))?;

            match meta {
                MetaValue {
                types: Some(Contract { types: ty2, .. }),
                value: Some(t),
                ..
                } => {
                    let uty2 = UnifType::from_type(ty2.clone(), &ctxt.term_env);
                    let instantiated = instantiate_foralls(state, uty2, ForallInst::Constant);
                    type_check_(state, ctxt, lin, linearizer, t, instantiated)
                }
                MetaValue {value: Some(t), .. } =>  walk(state, ctxt, lin, linearizer, t),
                // A metavalue without a body nor a type annotation is a record field without definition.
                // TODO: we might have something to with the linearizer to clear the current
                // metadata. It looks like it may be unduly attached to the next field definition,
                // which is not critical, but still a bug.
                _ => Ok(()),
            }
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
    match &ty.0 {
       TypeF::Dyn
       | TypeF::Num
       | TypeF::Bool
       | TypeF::Str
       | TypeF::Sym
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
       TypeF::Dict(ty2)
       | TypeF::Array(ty2)
       | TypeF::Forall {body: ty2, ..} => walk_type(state, ctxt, lin, linearizer, ty2),
    }
}

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
        RecordRowsF::Extend { row, tail } => {
            walk_type(state, ctxt, lin, linearizer, &*row.types)?;
            walk_rrows(state, ctxt, lin, linearizer, &*tail)
        }
    }
}

// TODO: The insertion of values in the type environment is done but everything is
// typed as `Dyn`.
fn inject_pat_vars(pat: &Destruct, env: &mut Environment) {
    if let Destruct::Record { matches, rest, .. } = pat {
        if let Some(id) = rest {
            env.insert(id.clone(), UnifType::Concrete(TypeF::Dyn));
        }
        matches.iter().for_each(|m| match m {
            Match::Simple(id, ..) => env.insert(id.clone(), UnifType::Concrete(TypeF::Dyn)),
            Match::Assign(id, _, (bind_id, pat)) => {
                let id = bind_id.as_ref().unwrap_or(id);
                env.insert(id.clone(), UnifType::Concrete(TypeF::Dyn));
                if !pat.is_empty() {
                    inject_pat_vars(pat, env);
                }
            }
        });
    }
}

/// Typecheck a term against a specific type.
///
/// # Arguments
///
/// - `state`: the unification state (see [`State`]).
/// - `env`: the typing environment, mapping free variable to types.
/// - `lin`: The current building linearization of building state `S`
/// - `linearizer`: A linearizer that can modify the linearization
/// - `t`: the term to check.
/// - `ty`: the type to check the term against.
///
/// Registers every term with the `linearizer` and makes sure to scope the
/// liearizer accordingly
fn type_check_<L: Linearizer>(
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
        // null is inferred to be of type Dyn
        Term::Null => unify(state, &ctxt, ty, mk_typewrapper::dynamic())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Bool(_) => unify(state, &ctxt, ty, mk_typewrapper::bool())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Num(_) => unify(state, &ctxt, ty, mk_typewrapper::num())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Str(_) => unify(state, &ctxt, ty, mk_typewrapper::str())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::StrChunks(chunks) => {
            unify(state, &ctxt, ty, mk_typewrapper::str())
                .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            chunks
                .iter()
                .try_for_each(|chunk| -> Result<(), TypecheckError> {
                    match chunk {
                        StrChunk::Literal(_) => Ok(()),
                        StrChunk::Expr(t, _) => type_check_(
                            state,
                            ctxt.clone(),
                            lin,
                            linearizer.scope(),
                            t,
                            mk_typewrapper::str(),
                        ),
                    }
                })
        }
        Term::Fun(x, t) => {
            let src = state.table.fresh_type_uvar();
            // TODO what to do here, this makes more sense to me, but it means let x = foo in bar
            // behaves quite different to (\x.bar) foo, worth considering if it's ok to type these two differently
            let trg = state.table.fresh_type_uvar();
            let arr = mk_tyw_arrow!(src.clone(), trg.clone());
            linearizer.retype_ident(lin, x, src.clone());

            unify(state, &ctxt, ty, arr).map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            ctxt.type_env.insert(x.clone(), src);
            type_check_(state, ctxt, lin, linearizer, t, trg)
        }
        Term::FunPattern(x, pat, t) => {
            let src = state.table.fresh_type_uvar();
            // TODO what to do here, this makes more sense to me, but it means let x = foo in bar
            // behaves quite different to (\x.bar) foo, worth considering if it's ok to type these two differently
            let trg = state.table.fresh_type_uvar();
            let arr = mk_tyw_arrow!(src.clone(), trg.clone());
            if let Some(x) = x {
                linearizer.retype_ident(lin, x, src.clone());
                ctxt.type_env.insert(x.clone(), src);
            }
            inject_pat_vars(pat, &mut ctxt.type_env);
            unify(state, &ctxt, ty, arr).map_err(|err| err.into_typecheck_err(state, rt.pos))?;
            type_check_(state, ctxt, lin, linearizer, t, trg)
        }
        Term::Array(terms, _) => {
            let ty_elts = state.table.fresh_type_uvar();

            unify(state, &ctxt, ty, mk_typewrapper::array(ty_elts.clone()))
                .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            terms
                .iter()
                .try_for_each(|t| -> Result<(), TypecheckError> {
                    type_check_(
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
            unify(state, &ctxt, ty, mk_typewrapper::dynamic())
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Let(x, re, rt, attrs) => {
            let ty_let = binding_type(state, re.as_ref(), &ctxt, true);

            // We don't support recursive binding when checking for contract equality. See the
            // `Let` case in `walk`.
            ctxt.term_env
                .0
                .insert(x.clone(), (re.clone(), ctxt.term_env.clone()));

            if attrs.rec {
                ctxt.type_env.insert(x.clone(), ty_let.clone());
            }

            linearizer.retype_ident(lin, x, ty_let.clone());
            type_check_(
                state,
                ctxt.clone(),
                lin,
                linearizer.scope(),
                re,
                ty_let.clone(),
            )?;

            if !attrs.rec {
                ctxt.type_env.insert(x.clone(), ty_let);
            }
            type_check_(state, ctxt, lin, linearizer, rt, ty)
        }
        Term::LetPattern(x, pat, re, rt) => {
            let ty_let = binding_type(state, re.as_ref(), &ctxt, true);
            type_check_(
                state,
                ctxt.clone(),
                lin,
                linearizer.scope(),
                re,
                ty_let.clone(),
            )?;

            if let Some(x) = x {
                linearizer.retype_ident(lin, x, ty_let.clone());
                ctxt.type_env.insert(x.clone(), ty_let);
            }
            inject_pat_vars(pat, &mut ctxt.type_env);
            type_check_(state, ctxt, lin, linearizer, rt, ty)
        }
        Term::App(e, t) => {
            let src = state.table.fresh_type_uvar();
            let arr = mk_tyw_arrow!(src.clone(), ty);

            type_check_(state, ctxt.clone(), lin, linearizer.scope(), e, arr)?;
            type_check_(state, ctxt, lin, linearizer, t, src)
        }
        Term::Switch(exp, cases, default) => {
            // Currently, if it has a default value, we typecheck the whole thing as
            // taking ANY enum, since it's more permissive and there's no loss of information
            let res = state.table.fresh_type_uvar();

            for case in cases.values() {
                type_check_(
                    state,
                    ctxt.clone(),
                    lin,
                    linearizer.scope(),
                    case,
                    res.clone(),
                )?;
            }

            let erows = match default {
                Some(t) => {
                    type_check_(state, ctxt.clone(), lin, linearizer.scope(), t, res.clone())?;
                    state.table.fresh_erows_uvar()
                }
                None => cases.iter().try_fold(
                    EnumRowsF::Empty.into(),
                    |acc, x| -> Result<UnifEnumRows, TypecheckError> {
                        Ok(mk_tyw_enum_row!(x.0.clone(); acc))
                    },
                )?,
            };

            unify(state, &ctxt, ty, res).map_err(|err| err.into_typecheck_err(state, rt.pos))?;
            type_check_(state, ctxt, lin, linearizer, exp, mk_tyw_enum!(; erows))
        }
        Term::Var(x) => {
            let x_ty = ctxt
                .type_env
                .get(x)
                .cloned()
                .ok_or_else(|| TypecheckError::UnboundIdentifier(x.clone(), *pos))?;

            let instantiated = instantiate_foralls(state, x_ty, ForallInst::Ptr);
            unify(state, &ctxt, ty, instantiated)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Enum(id) => {
            let row = state.table.fresh_erows_uvar();
            unify(state, &ctxt, ty, mk_tyw_enum!(id.clone(); row))
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        // If some fields are defined dynamically, the only potential type that works is `{_ : a}`
        // for some `a`
        Term::RecRecord(record, dynamic, ..) if !dynamic.is_empty() => {
            let ty_dict = state.table.fresh_type_uvar();

            for id in record.fields.keys() {
                ctxt.type_env.insert(id.clone(), ty_dict.clone());
                linearizer.retype_ident(lin, id, ty_dict.clone())
            }

            // We don't bind the fields in the term environment used to check for contract. See
            // `Let` case in `walk`.
            record
                .fields
                .iter()
                .try_for_each(|(_, t)| -> Result<(), TypecheckError> {
                    type_check_(
                        state,
                        ctxt.clone(),
                        lin,
                        linearizer.scope(),
                        t,
                        ty_dict.clone(),
                    )
                })?;

            unify(state, &ctxt, ty, mk_typewrapper::dyn_record(ty_dict))
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Record(record) | Term::RecRecord(record, ..) => {
            // For recursive records, we look at the apparent type of each field and bind it in
            // ctxt before actually typechecking the content of fields.
            // Fields defined by interpolation are ignored.
            if let Term::RecRecord(..) = t.as_ref() {
                for (id, rt) in &record.fields {
                    let uty = binding_type(state, rt.as_ref(), &ctxt, true);
                    ctxt.type_env.insert(id.clone(), uty.clone());
                    linearizer.retype_ident(lin, id, uty);
                }
            }

            let root_ty = if let UnifType::UnifVar(p) = ty {
                state.table.root_type(p)
            } else {
                ty.clone()
            };

            if let UnifType::Concrete(TypeF::Dict(rec_ty)) = root_ty {
                // Checking for a dictionary
                record
                    .fields
                    .iter()
                    .try_for_each(|(_, t)| -> Result<(), TypecheckError> {
                        type_check_(
                            state,
                            ctxt.clone(),
                            lin,
                            linearizer.scope(),
                            t,
                            (*rec_ty).clone(),
                        )
                    })
            } else {
                let rows = record.fields.iter().try_fold(
                    mk_tyw_row!(),
                    |acc, (id, field)| -> Result<UnifRecordRows, TypecheckError> {
                        // In the case of a recursive record, new types (either type variables or
                        // annotations) have already be determined and put in the typing context,
                        // and we need to use the same.
                        let ty = if let Term::RecRecord(..) = t.as_ref() {
                            ctxt.type_env.get(id).cloned().unwrap()
                        } else {
                            state.table.fresh_type_uvar()
                        };

                        type_check_(
                            state,
                            ctxt.clone(),
                            lin,
                            linearizer.scope(),
                            field,
                            ty.clone(),
                        )?;

                        Ok(mk_tyw_row!((id.clone(), ty); acc))
                    },
                )?;

                unify(state, &ctxt, ty, mk_tyw_record!(; rows))
                    .map_err(|err| err.into_typecheck_err(state, rt.pos))
            }
        }
        Term::Op1(op, t) => {
            let (ty_arg, ty_res) = get_uop_type(state, op)?;

            type_check_(state, ctxt.clone(), lin, linearizer.scope(), t, ty_arg)?;

            let instantiated = instantiate_foralls(state, ty_res, ForallInst::Ptr);
            unify(state, &ctxt, ty, instantiated)
                .map_err(|err| err.into_typecheck_err(state, rt.pos))
        }
        Term::Op2(op, t1, t2) => {
            let (ty_arg1, ty_arg2, ty_res) = get_bop_type(state, op)?;

            unify(state, &ctxt, ty, ty_res).map_err(|err| err.into_typecheck_err(state, rt.pos))?;
            type_check_(state, ctxt.clone(), lin, linearizer.scope(), t1, ty_arg1)?;
            type_check_(state, ctxt, lin, linearizer, t2, ty_arg2)
        }
        Term::OpN(op, args) => {
            let (tys_op, ty_ret) = get_nop_type(state, op)?;

            unify(state, &ctxt, ty, ty_ret).map_err(|err| err.into_typecheck_err(state, rt.pos))?;

            tys_op.into_iter().zip(args.iter()).try_for_each(
                |(ty_t, t)| -> Result<_, TypecheckError> {
                    type_check_(state, ctxt.clone(), lin, linearizer.scope(), t, ty_t)?;
                    Ok(())
                },
            )?;

            Ok(())
        }
        Term::MetaValue(meta) => {
            meta.contracts
                .iter()
                .chain(meta.types.iter())
                .try_for_each(|ty| {
                    walk_type(state, ctxt.clone(), lin, linearizer.scope_meta(), &ty.types)
                })?;

            match meta {
                MetaValue {
                    types: Some(Contract { types: ty2, .. }),
                    value: Some(t),
                    ..
                } => {
                    let uty2 = UnifType::from_type(ty2.clone(), &ctxt.term_env);
                    let instantiated =
                        instantiate_foralls(state, uty2.clone(), ForallInst::Constant);

                    unify(state, &ctxt, uty2, ty)
                        .map_err(|err| err.into_typecheck_err(state, rt.pos))?;
                    type_check_(state, ctxt, lin, linearizer, t, instantiated)
                }
                // A metavalue without a type annotation but with a contract annotation switches
                // the typechecker back to walk mode. If there are several contracts, we
                // arbitrarily chose the first one as the apparent type.
                MetaValue {
                    contracts, value, ..
                } if !contracts.is_empty() => {
                    let ctr = contracts.get(0).unwrap();
                    let Contract { types: ty2, .. } = ctr;

                    unify(
                        state,
                        &ctxt,
                        ty,
                        UnifType::from_type(ty2.clone(), &ctxt.term_env),
                    )
                    .map_err(|err| err.into_typecheck_err(state, rt.pos))?;

                    // if there's an inner value, we still have to walk it, as it may contain
                    // statically typed blocks.
                    if let Some(t) = value {
                        walk(state, ctxt, lin, linearizer, t)
                    } else {
                        // TODO: we might have something to with the linearizer to clear the current
                        // metadata. It looks like it may be unduly attached to the next field definition,
                        // which is not critical, but still a bug.
                        Ok(())
                    }
                }
                // A non-empty metavalue without a type or a contract annotation is typechecked in
                // the same way as its inner value
                MetaValue { value: Some(t), .. } => {
                    type_check_(state, ctxt, lin, linearizer, t, ty)
                }
                // A metavalue without a body nor a type annotation is a record field without definition.
                // We infer it to be of type `Dyn` for now.
                // TODO: we might have something to with the linearizer to clear the current
                // metadata. It looks like it may be unduly attached to the next field definition,
                // which is not critical, but still a bug.
                _ => unify(state, &ctxt, ty, mk_typewrapper::dynamic())
                    .map_err(|err| err.into_typecheck_err(state, rt.pos)),
            }
        }
        Term::SealingKey(_) => unify(state, &ctxt, ty, mk_typewrapper::sym())
            .map_err(|err| err.into_typecheck_err(state, rt.pos)),
        Term::Sealed(_, t, _) => type_check_(state, ctxt, lin, linearizer, t, ty),
        Term::Import(_) => unify(state, &ctxt, ty, mk_typewrapper::dynamic())
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

/// Determine the type of a let-bound expression, or more generally of any binding (e.g. fields)
/// that may be stored in a typing environment at some point.
///
/// Call [`apparent_type`] to see if the binding is annotated. If it is, return this type as a
/// [`TypeWrapper`]. Otherwise:
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
    let ty_apt = apparent_type(t, Some(&ctxt.type_env), Some(state.resolver));

    match ty_apt {
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
    fn replace_rrows(rrows: RecordRows) -> UnifRecordRows { todo!() }
    fn replace_erows(erows: EnumRows) -> UnifEnumRows { todo!() }

    match ty.0 {
        TypeF::Wildcard(i) => get_wildcard_var(table, wildcard_vars, i),
        TypeF::Flat(t) => UnifType::Contract(t, env.clone()),
        _ => UnifType::Concrete(
            ty.0.map(|ty| Box::new(replace_wildcards_with_var(table, wildcard_vars, *ty, env)), replace_rrows, replace_erows),
        ),
    }
}

/// Different kinds of apparent types (see [`apparent_type`]).
///
/// Indicate the nature of an apparent type. In particular, when in strict mode, the typechecker
/// throws away approximations as it can do better and infer the actual type of an expression by
/// generating a fresh unification variable.  In non-strict mode, however, the approximation is the
/// best we can do. This type allows the caller of `apparent_type` to determine which situation it
/// is.
#[derive(Debug)]
pub enum ApparentType {
    /// The apparent type is given by a user-provided annotation, such as an `Assume`, a `Promise`,
    /// or a metavalue.
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
            ApparentType::Annotated(ty) if has_wildcards(&ty) => Types(TypeF::Dyn),
            ApparentType::Annotated(ty)
            | ApparentType::Inferred(ty)
            | ApparentType::Approximated(ty) => ty,
            ApparentType::FromEnv(uty) => uty.try_into().ok().unwrap_or(Types(TypeF::Dyn)),
        }
    }
}

/// Determine the apparent type of a let-bound expression.
///
/// When a let-binding `let x = bound_exp in body` is processed, the type of `bound_exp` must be
/// determined in order to be bound to the variable `x` in the typing environment.
/// Then, future occurrences of `x` can be given this type when used in a `Promise` block.
///
/// The role of `apparent_type` is precisely to determine the type of `bound_exp`:
/// - if `bound_exp` is annotated by an `Assume`, a `Promise` or a metavalue, return the
///   user-provided type, unless that type is a wildcard.
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
    match t {
        Term::MetaValue(MetaValue {
            types: Some(Contract { types: ty, .. }),
            ..
        }) => ApparentType::Annotated(ty.clone()),
        // For metavalues, if there's no type annotation, choose the first contract appearing.
        Term::MetaValue(MetaValue { contracts, .. }) if !contracts.is_empty() => {
            ApparentType::Annotated(contracts.get(0).unwrap().types.clone())
        }
        Term::MetaValue(MetaValue { value: Some(v), .. }) => {
            apparent_type(v.as_ref(), env, resolver)
        }
        Term::Num(_) => ApparentType::Inferred(Types(TypeF::Num)),
        Term::Bool(_) => ApparentType::Inferred(Types(TypeF::Bool)),
        Term::SealingKey(_) => ApparentType::Inferred(Types(TypeF::Sym)),
        Term::Str(_) | Term::StrChunks(_) => ApparentType::Inferred(Types(TypeF::Str)),
        Term::Array(..) => {
            ApparentType::Approximated(Types(TypeF::Array(Box::new(Types(TypeF::Dyn)))))
        }
        Term::Var(id) => env
            .and_then(|envs| envs.get(id).cloned())
            .map(ApparentType::FromEnv)
            .unwrap_or(ApparentType::Approximated(Types(TypeF::Dyn))),
        Term::ResolvedImport(f) => {
            if let Some(r) = resolver {
                let t = r
                    .get(*f)
                    .expect("Internal error: resolved import not found during typechecking.");
                apparent_type(&t.term, env, Some(r))
            } else {
                ApparentType::Approximated(Types(TypeF::Dyn))
            }
        }
        _ => ApparentType::Approximated(Types(TypeF::Dyn)),
    }
}

/// Infer the type of a non annotated record by gathering the apparent type of the fields. It's
/// currently used essentially to type the stdlib.
pub fn infer_record_type(t: &Term, term_env: &SimpleTermEnvironment) -> UnifType {
    match t {
        // An explicit annotation must take precedence over this inference, so let's use it.
        t @ Term::MetaValue(MetaValue {
            types, contracts, ..
        }) if (types.is_some() || !contracts.is_empty()) => {
            UnifType::from_apparent_type(apparent_type(t, None, None), term_env)
        }
        // We unwrap meta-values without any type information. Otherwise, something like a
        // top-level module documentation would stop this function from doing its job.
        Term::MetaValue(MetaValue {
            value: Some(rt),
            types: None,
            contracts,
            ..
        }) if contracts.is_empty() => infer_record_type(rt.as_ref(), term_env),
        Term::Record(record) | Term::RecRecord(record, ..) => UnifType::from(TypeF::Record(
            UnifRecordRows::Concrete(record.fields.iter().fold(RecordRowsF::Empty, |r, (id, rt)| {
                RecordRowsF::Extend {
                    row : UnifRecordRow { id: id.clone(), types: Box::new(infer_record_type(rt.term.as_ref(), term_env)) },
                    tail: Box::new(r.into()),
                }}
            ))
        )),
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
            &mut |ty, has_wildcard| {
                if ty.0.is_wildcard() {
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

/// Look for a binding in a row, or add a new one if it is not present and if allowed by [row
/// constraints][RowConstr].
///
/// The row may be given as a concrete type or as a unification variable.
///
/// # Return
///
/// The type newly bound to `id` in the row together with the tail of the new row. If `id` was
/// already in `r`, it does not change the binding and return the corresponding type instead as a
/// first component.
fn rrow_add(
    state: &mut State,
    id: &Ident,
    ty: Box<UnifType>,
    mut r: UnifRecordRows,
) -> Result<(Box<UnifType>, UnifRecordRows), RowUnifError> {
    todo!()
    // if let UnifType::UnifVar(p) = r {
    //     r = state.table.root(p);
    // }
    // match r {
    //     UnifType::Concrete(TypeF::Empty) | UnifType::Concrete(TypeF::Dyn) => {
    //         Err(RowUnifError::MissingRow(id.clone()))
    //     }
    //     UnifType::Concrete(TypeF::RowExtend(id2, ty2, r2)) => {
    //         if *id == id2 {
    //             Ok((ty2, *r2))
    //         } else {
    //             let (extracted_type, subrow) = row_add(state, id, ty, *r2)?;
    //             Ok((
    //                 extracted_type,
    //                 UnifType::Concrete(TypeF::RowExtend(id2, ty2, Box::new(subrow))),
    //             ))
    //         }
    //     }
    //     UnifType::UnifVar(root) => {
    //         if let Some(set) = state.constr.get(&root) {
    //             if set.contains(id) {
    //                 return Err(RowUnifError::UnsatConstr(id.clone(), ty.map(|uty| *uty)));
    //             }
    //         }
    //         let new_row = state.table.fresh_unif_var();
    //         constraint(state, new_row.clone(), id.clone())?;
    //         state.table.assign(
    //             root,
    //             UnifType::Concrete(TypeF::RowExtend(
    //                 id.clone(),
    //                 ty.clone(),
    //                 Box::new(new_row.clone()),
    //             )),
    //         );
    //         Ok((ty, new_row))
    //     }
    //     other => Err(RowUnifError::IllformedRow(other)),
    // }
}

fn erow_add(
    state: &mut State,
    id: &Ident,
    mut r: UnifEnumRows,
) -> Result<UnifEnumRows, RowUnifError> {
    todo!()
    // if let UnifType::UnifVar(p) = r {
    //     r = state.table.root(p);
    // }
    // match r {
    //     UnifType::Concrete(TypeF::Empty) | UnifType::Concrete(TypeF::Dyn) => {
    //         Err(RowUnifError::MissingRow(id.clone()))
    //     }
    //     UnifType::Concrete(TypeF::RowExtend(id2, ty2, r2)) => {
    //         if *id == id2 {
    //             Ok((ty2, *r2))
    //         } else {
    //             let (extracted_type, subrow) = row_add(state, id, ty, *r2)?;
    //             Ok((
    //                 extracted_type,
    //                 UnifType::Concrete(TypeF::RowExtend(id2, ty2, Box::new(subrow))),
    //             ))
    //         }
    //     }
    //     UnifType::UnifVar(root) => {
    //         if let Some(set) = state.constr.get(&root) {
    //             if set.contains(id) {
    //                 return Err(RowUnifError::UnsatConstr(id.clone(), ty.map(|uty| *uty)));
    //             }
    //         }
    //         let new_row = state.table.fresh_unif_var();
    //         constraint(state, new_row.clone(), id.clone())?;
    //         state.table.assign(
    //             root,
    //             UnifType::Concrete(TypeF::RowExtend(
    //                 id.clone(),
    //                 ty.clone(),
    //                 Box::new(new_row.clone()),
    //             )),
    //         );
    //         Ok((ty, new_row))
    //     }
    //     other => Err(RowUnifError::IllformedRow(other)),
    // }
}

/// Try to unify two types.
pub fn unify(
    state: &mut State,
    ctxt: &Context,
    mut t1: UnifType,
    mut t2: UnifType,
) -> Result<(), UnifError> {
    if let UnifType::UnifVar(pt1) = t1 {
        t1 = state.table.root_type(pt1);
    }
    if let UnifType::UnifVar(pt2) = t2 {
        t2 = state.table.root_type(pt2);
    }

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
            | (TypeF::Num, TypeF::Num)
            | (TypeF::Bool, TypeF::Bool)
            | (TypeF::Str, TypeF::Str)
            | (TypeF::Sym, TypeF::Sym) => Ok(()),
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
            // (r1, r2) if r1.is_row_type() && r2.is_row_type() => {
            //     unify_rows(state, &ctxt, r1.clone(), r2.clone()).map_err(|err| {
            //         err.into_unif_err(UnifType::Concrete(r1), UnifType::Concrete(r2))
            //     })
            // }
            (TypeF::Enum(erows1), TypeF::Enum(erows2)) => unify_erows(state, ctxt, erows1, erows2).map_err(|err| err.into_unif_err(mk_tyw_enum!(; erows1), mk_tyw_enum!(; erows2))),
            // match (*uty1, *uty2) {
            //     (UnifType::Concrete(r1), UnifType::Concrete(r2))
            //         if r1.is_row_type() && r2.is_row_type() =>
            //     {
            //         unify_rows(state, &ctxt, r1.clone(), r2.clone())
            //             .map_err(|err| err.into_unif_err(mk_tyw_enum!(; r1), mk_tyw_enum!(; r2)))
            //     }
            //     (UnifType::Concrete(r), _) if !r.is_row_type() => {
            //         Err(UnifError::IllformedType(mk_tyw_enum!(; r)))
            //     }
            //     (_, UnifType::Concrete(r)) if !r.is_row_type() => {
            //         Err(UnifError::IllformedType(mk_tyw_enum!(; r)))
            //     }
            //     (uty1, uty2) => unify(state, ctxt, uty1, uty2),
            // },
            (TypeF::Record(rrows1), TypeF::Record(rrows2)) => unify_rrows(state, ctxt, rrows1, rrows2).map_err(|err| {
                         err.into_unif_err(mk_tyw_record!(; rrows1), mk_tyw_record!(; rrows2))
                     }),
                // (UnifType::Concrete(r1), UnifType::Concrete(r2))
                //     if r1.is_row_type() && r2.is_row_type() =>
                // {
                //     unify_rows(state, &ctxt, r1.clone(), r2.clone()).map_err(|err| {
                //         err.into_unif_err(mk_tyw_record!(; r1), mk_tyw_record!(; r2))
                //     })
                // }
                // (UnifType::Concrete(TypeF::Var(id)), _)
                // | (_, UnifType::Concrete(TypeF::Var(id))) => {
                //     Err(UnifError::UnboundTypeVariable(id))
                // }
                // (UnifType::Concrete(r), _) | (_, UnifType::Concrete(r))
                //     if !r.is_row_type() =>
                // {
                //     Err(UnifError::IllformedType(mk_tyw_record!(; r)))
                // }
                // (uty1, uty2) => unify(state, ctxt, uty1, uty2),
            // },
            (TypeF::Dict(t1), TypeF::Dict(t2)) => unify(state, &ctxt, *t1, *t2),
            (TypeF::Forall {var: var1, var_kind: var_kind1, body: body1}, TypeF::Forall {var: var2, var_kind: var_kind2, body: body2}) if var_kind1 == var_kind2 => {

                // Very stupid (slow) implementation
                let (substd1, substd2) = match var_kind1 {
                    VarKind::Type => {
                        let constant_type = state.table.fresh_type_const();
                        (body1.subst_type(&var1, &constant_type), body2.subst_type(&var2, &constant_type))
                    },
                    VarKind::RecordRows => {
                        let constant_type = state.table.fresh_rrows_const();
                        (body1.subst_rrows(&var1, &constant_type), body2.subst_rrows(&var2, &constant_type))
                    }
                    VarKind::EnumRows => {
                        let constant_type = state.table.fresh_erows_const();
                        (body1.subst_erows(&var1, &constant_type), body2.subst_erows(&var2, &constant_type))
                    },
                };

                unify(
                    state,
                    &ctxt,
                    substd1,
                    substd2,
                )
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
        // The two following cases are not merged just to correctly distinguish between the
        // expected type (first component of the tuple) and the inferred type when reporting a row
        // unification error.
        (UnifType::UnifVar(p), uty) | (uty, UnifType::UnifVar(p))  => {
            state.table.assign_type(p, uty);
            Ok(())
        }
        (UnifType::Constant(i1), UnifType::Constant(i2)) if i1 == i2 => Ok(()),
        (UnifType::Constant(i1), UnifType::Constant(i2)) => {
            Err(UnifError::ConstMismatch(i1, i2))
        }
        (ty, UnifType::Constant(i)) | (UnifType::Constant(i), ty) => {
            Err(UnifError::WithConst(i, ty))
        }
        (UnifType::Contract(t1, env1), UnifType::Contract(t2, env2))
            if eq::contract_eq(state.table.type_uvars_count(), &t1, &env1, &t2, &env2) =>
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
    match (urrows1, urrows2) {
        (UnifRecordRows::Concrete(rrows1), UnifRecordRows::Concrete(rrows2)) => match (rrows1, rrows2) {
            (RecordRowsF::TailVar(id), _) | (_, RecordRowsF::TailVar(id)) => Err(RowUnifError::UnboundTypeVariable(id)),
            (RecordRowsF::Empty, RecordRowsF::Empty) | (RecordRowsF::TailDyn, RecordRowsF::TailDyn) => Ok(()),
            (RecordRowsF::Empty, RecordRowsF::TailDyn) => Err(RowUnifError::ExtraDynTail()),
            (RecordRowsF::TailDyn, RecordRowsF::Empty) => Err(RowUnifError::MissingDynTail()),
            (RecordRowsF::Empty, RecordRowsF::Extend { row: UnifRecordRow {id, ..}, .. })
            | (RecordRowsF::TailDyn, RecordRowsF::Extend { row: UnifRecordRow {id, ..}, .. }) => Err(RowUnifError::ExtraRow(id)),
            (RecordRowsF::Extend { row: UnifRecordRow {id, ..}, ..}, RecordRowsF::TailDyn)
            | (RecordRowsF::Extend { row: UnifRecordRow {id, ..}, ..}, RecordRowsF::Empty) => {
                Err(RowUnifError::MissingRow(id))
            }
            (RecordRowsF::Extend {row: UnifRecordRow {id, types}, tail }, r2 @ RecordRowsF::Extend {..}) => {
                let (ty2, t2_tail) = rrow_add(state, &id, types.clone(), UnifRecordRows::Concrete(r2))?;
                unify(state, ctxt, *types, *ty2).map_err(|err| RowUnifError::RowMismatch(id.clone(), Box::new(err)))?;
                unify_rrows(state, ctxt, *tail, t2_tail)
            }
        }
        (UnifRecordRows::UnifVar(p), urrows) | (urrows, UnifRecordRows::UnifVar(p))  => {
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
            Err(RowUnifError::WithConst(i, UnifType::Concrete(TypeF::Record(urrows))))
        }
    }
}

/// Try to unify two row types. Return an [`RowUnifError::IllformedRow`] error if one of the given
/// type is not a row type.
pub fn unify_erows(
    state: &mut State,
    ctxt: &Context,
    uerows1 : UnifEnumRows,
    uerows2 : UnifEnumRows,
) -> Result<(), RowUnifError> {
    match (uerows1, uerows2) {
        (UnifEnumRows::Concrete(erows1), UnifEnumRows::Concrete(erows2)) => match (erows1, erows2) {
            (EnumRowsF::TailVar(id), _) | (_, EnumRowsF::TailVar(id)) => Err(RowUnifError::UnboundTypeVariable(id)),
            (EnumRowsF::Empty, EnumRowsF::Empty)  => Ok(()),
            (EnumRowsF::Empty, EnumRowsF::Extend { row: ident, ..}) => Err(RowUnifError::ExtraRow(ident)),
            (EnumRowsF::Extend { row: ident, ..}, EnumRowsF::Empty) => Err(RowUnifError::MissingRow(ident)),
            (EnumRowsF::Extend {row: id, tail }, r2 @ EnumRowsF::Extend {..}) => {
                let t2_tail = erow_add(state, &id, UnifEnumRows::Concrete(r2))?;
                unify_erows(state, ctxt, *tail, t2_tail)
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
            Err(RowUnifError::WithConst(i, UnifType::Concrete(TypeF::Enum(uerows))))
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
/// - `state`: the unification state
/// - `ty`: the polymorphic type to instantiate
/// - `inst`: the type of instantiation, either by a type constant or by a unification variable
fn instantiate_foralls(state: &mut State, mut ty: UnifType, inst: ForallInst) -> UnifType {
    if let UnifType::UnifVar(p) = ty {
        ty = state.table.root_type(p);
    }

    while let UnifType::Concrete(TypeF::Forall {var, var_kind, body }) = ty {
        match var_kind {
            VarKind::Type => {
                let fresh_uid = state.table.fresh_type_var_id();
                let uvar = match inst {
                    ForallInst::Constant => UnifType::Constant(fresh_uid),
                    ForallInst::Ptr => UnifType::UnifVar(fresh_uid),
                };
                state.names.insert(fresh_uid, var.clone());
                ty = body.subst_type(&var, &uvar);
            },
            VarKind::RecordRows => {
                let fresh_uid = state.table.fresh_rrows_var_id();
                let uvar = match inst {
                    ForallInst::Constant => UnifRecordRows::Constant(fresh_uid),
                    ForallInst::Ptr => UnifRecordRows::UnifVar(fresh_uid),
                };
                state.names.insert(fresh_uid, var.clone());
                ty = body.subst_rrows(&var, &uvar);

                if inst == ForallInst::Ptr {
                    ty.constrain_fresh_rrows_var(state, fresh_uid)
                }
            },
            VarKind::EnumRows => {
                let fresh_uid = state.table.fresh_rrows_var_id();
                let uvar = match inst {
                    ForallInst::Constant => UnifEnumRows::Constant(fresh_uid),
                    ForallInst::Ptr => UnifEnumRows::UnifVar(fresh_uid),
                };
                state.names.insert(fresh_uid, var.clone());
                ty = body.subst_erows(&var, &uvar);

                if inst == ForallInst::Ptr {
                    ty.constrain_fresh_erows_var(state, fresh_uid)
                }
            },
        };
    }

    ty
}

/// The unification table.
///
/// Map each unification variable to either another type variable or a concrete type it has been
/// unified with. Each binding `(ty, var)` in this map should be thought of an edge in a
/// unification graph.
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

    /// Assign a type to a unification variable.
    pub fn assign_type(&mut self, var: usize, uty: UnifType) {
        debug_assert!(self.types[var].is_none());
        self.types[var] = Some(uty);
    }

    /// Assign a type to a unification variable.
    pub fn assign_rrows(&mut self, var: usize, rrows: UnifRecordRows) {
        debug_assert!(self.rrows[var].is_none());
        self.rrows[var] = Some(rrows);
    }

    /// Assign a type to a unification variable.
    pub fn assign_erows(&mut self, var: usize, erows: UnifEnumRows) {
        debug_assert!(self.erows[var].is_none());
        self.erows[var] = Some(erows);
    }

    /// Retrieve the current assignment of a unification variable.
    pub fn get_type(&self, var: usize) -> Option<&UnifType> {
        self.types[var].as_ref()
    }

    /// Retrieve the current assignment of a unification variable.
    pub fn get_rrows(&self, var: usize) -> Option<&UnifRecordRows> {
        self.rrows[var].as_ref()
    }

    /// Retrieve the current assignment of a unification variable.
    pub fn get_erows(&self, var: usize) -> Option<&UnifEnumRows> {
        self.erows[var].as_ref()
    }

    /// Create a fresh variable identifier and allocate a corresponding slot in the table.
    fn fresh_type_var_id(&mut self) -> usize {
        let next = self.types.len();
        self.types.push(None);
        next
    }

    /// Create a fresh variable identifier and allocate a corresponding slot in the table.
    fn fresh_rrows_var_id(&mut self) -> usize {
        let next = self.rrows.len();
        self.rrows.push(None);
        next
    }

    /// Create a fresh variable identifier and allocate a corresponding slot in the table.
    fn fresh_erows_var_id(&mut self) -> usize {
        let next = self.erows.len();
        self.erows.push(None);
        next
    }

    /// Create a fresh unification variable and allocate a corresponding slot in the table.
    pub fn fresh_type_uvar(&mut self) -> UnifType {
        UnifType::UnifVar(self.fresh_type_var_id())
    }

    /// Create a fresh unification variable and allocate a corresponding slot in the table.
    pub fn fresh_rrows_uvar(&mut self) -> UnifRecordRows {
        UnifRecordRows::UnifVar(self.fresh_rrows_var_id())
    }

    /// Create a fresh unification variable and allocate a corresponding slot in the table.
    pub fn fresh_erows_uvar(&mut self) -> UnifEnumRows {
        UnifEnumRows::UnifVar(self.fresh_erows_var_id())
    }

    /// Create a fresh unification variable and allocate a corresponding slot in the table.
    pub fn fresh_type_const(&mut self) -> UnifType {
        UnifType::Constant(self.fresh_type_var_id())
    }

    /// Create a fresh unification variable and allocate a corresponding slot in the table.
    pub fn fresh_rrows_const(&mut self) -> UnifRecordRows {
        UnifRecordRows::Constant(self.fresh_rrows_var_id())
    }

    /// Create a fresh unification variable and allocate a corresponding slot in the table.
    pub fn fresh_erows_const(&mut self) -> UnifEnumRows {
        UnifEnumRows::Constant(self.fresh_erows_var_id())
    }

    /// Follow the links in the unification table to find the representative of the equivalence class
    /// of unification variable `x`.
    ///
    /// This corresponds to the find in union-find.
    // TODO This should be a union find like algorithm
    pub fn root_type(&self, x: usize) -> UnifType {
        // All queried variable must have been introduced by `new_var` and thus a corresponding entry
        // must always exist in `state`. If not, the typechecking algorithm is not correct, and we
        // panic.
        match &self.types[x] {
            None => UnifType::UnifVar(x),
            Some(UnifType::UnifVar(y)) => self.root_type(*y),
            Some(ty) => ty.clone(),
        }
    }

    /// Follow the links in the unification table to find the representative of the equivalence class
    /// of unification variable `x`.
    ///
    /// This corresponds to the find in union-find.
    // TODO This should be a union find like algorithm
    pub fn root_rrows(&self, x: usize) -> UnifRecordRows {
        // All queried variable must have been introduced by `new_var` and thus a corresponding entry
        // must always exist in `state`. If not, the typechecking algorithm is not correct, and we
        // panic.
        match &self.rrows[x] {
            None => UnifRecordRows::UnifVar(x),
            Some(UnifRecordRows::UnifVar(y)) => self.root_rrows(*y),
            Some(ty) => ty.clone(),
        }
    }

    /// Follow the links in the unification table to find the representative of the equivalence class
    /// of unification variable `x`.
    ///
    /// This corresponds to the find in union-find.
    // TODO This should be a union find like algorithm
    pub fn root_erows(&self, x: usize) -> UnifEnumRows {
        // All queried variable must have been introduced by `new_var` and thus a corresponding entry
        // must always exist in `state`. If not, the typechecking algorithm is not correct, and we
        // panic.
        match &self.erows[x] {
            None => UnifEnumRows::UnifVar(x),
            Some(UnifEnumRows::UnifVar(y)) => self.root_erows(*y),
            Some(ty) => ty.clone(),
        }
    }

    /// Return the number of all variables currently allocated (unification and rigid type
    /// variables). The returned UID is guaranteed to be different from all the currently live
    /// variables. This is also the value that will be returned by the next call to `fresh_var()`,
    /// and is currently simply the length of the unification table.
    pub fn type_uvars_count(&self) -> usize {
        self.types.len()
    }

    /// Return the number of all variables currently allocated (unification and rigid type
    /// variables). The returned UID is guaranteed to be different from all the currently live
    /// variables. This is also the value that will be returned by the next call to `fresh_var()`,
    /// and is currently simply the length of the unification table.
    pub fn rrows_uvar_count(&self) -> usize {
        self.rrows.len()
    }

    /// Return the number of all variables currently allocated (unification and rigid type
    /// variables). The returned UID is guaranteed to be different from all the currently live
    /// variables. This is also the value that will be returned by the next call to `fresh_var()`,
    /// and is currently simply the length of the unification table.
    pub fn erows_uvar_count(&self) -> usize {
        self.erows.len()
    }
}

/// Row constraints.
///
/// A row constraint applies to a unification variable appearing inside a row type (such as `r` in
/// `{ someId: SomeType | r }`). It is a set of identifiers that said row must NOT contain, to
/// forbid ill-formed types with multiple declaration of the same id, for example `{ a: Num, a:
/// String}`.
pub type RowConstr = HashMap<usize, HashSet<Ident>>;

/// Constrain record rows `rrows` to not contain a row declaration for `id`. Propagae those
/// constraint to potential record rows unification variable. Return an error if a row declaration
/// with label `id` is encountered.
fn constrain_rrows(state: &mut State, rrows: UnifRecordRows, id: Ident) -> Result<(), RowUnifError> {
    match rrows {
        UnifRecordRows::UnifVar(p) => match state.table.root_rrows(p) {
            ty @ UnifRecordRows::Concrete(_) => constrain_rrows(state, ty, id),
            // Constraints only applies to row unification variables
            UnifRecordRows::UnifVar(root) => {
                if let Some(v) = state.constr.get_mut(&root) {
                    v.insert(id);
                } else {
                    state.constr.insert(root, vec![id].into_iter().collect());
                }
                Ok(())
            }
            //TODO: previously ill formed row exception. Now that can't happen, but is it really ok
            //to constrain on a existential variable without doing anything?
            UnifRecordRows::Constant(_) => Ok(()),
        }
        UnifRecordRows::Concrete(RecordRowsF::Empty) => Ok(()),
        UnifRecordRows::Concrete(RecordRowsF::Extend {row, tail}) => {
            if row.id == id {
                Err(RowUnifError::UnsatConstr(id, Some(*row.types)))
            } else {
                constrain_rrows(state, *tail, id)
            }
        }
        UnifRecordRows::Concrete(RecordRowsF::TailVar(id)) => Err(RowUnifError::UnboundTypeVariable(id)),
        //TODO: previously ill formed row exception. Now that can't happen, but is it really ok
        //to constrain on a existential variable without doing anything?
        UnifRecordRows::Concrete(RecordRowsF::TailDyn)
        | UnifRecordRows::Constant(_) => Ok(()),
    }
}

trait ConstrainFreshRRowsVar {
    /// Add row constraints on a freshly instantiated type variable.
    ///
    /// When instantiating a quantified type variable with a unification variable, row constraints may
    /// apply. For example, if we instantiate `forall a. {x: Num | a} -> Num` by replacing `a` with a
    /// unification variable `Ptr(p)`, this unification variable requires a constraint to avoid being
    /// unified with a row type containing another declaration for the field `x`.
    ///
    /// This function traverses the type `ty`, looking for all occurrences of the unification variable
    /// `p` inside a row type, to add the corresponding constraints.
    ///
    /// # Preconditions
    ///
    /// Because `constraint_var` should be called on a fresh unification variable `p`, the following
    /// preconditions are assumed:
    ///
    /// - `state.table.root_rrows(p) == p`
    fn constrain_fresh_rrows_var(&self, state: &mut State, p: usize);
}

trait ConstrainFreshERowsVar {
    /// Add row constraints on a freshly instantiated type variable.
    ///
    /// When instantiating a quantified type variable with a unification variable, row constraints may
    /// apply. For example, if we instantiate `forall a. {x: Num | a} -> Num` by replacing `a` with a
    /// unification variable `Ptr(p)`, this unification variable requires a constraint to avoid being
    /// unified with a row type containing another declaration for the field `x`.
    ///
    /// This function traverses the type `ty`, looking for all occurrences of the unification variable
    /// `p` inside a row type, to add the corresponding constraints.
    ///
    /// # Preconditions
    ///
    /// Because `constraint_var` should be called on a fresh unification variable `p`, the following
    /// preconditions are assumed:
    ///
    /// - `state.table.root_rrows(p) == p`
    fn constrain_fresh_erows_var(&self, state: &mut State, p: usize);
}

impl ConstrainFreshRRowsVar for UnifType {
    fn constrain_fresh_rrows_var(&self, state: &mut State, p: usize) {
        match self {
                UnifType::UnifVar(u) => match state.table.root_type(*u) {
                    UnifType::UnifVar(_) => (),
                    ty => ty.constrain_fresh_rrows_var(state, p),
                },
                UnifType::Concrete(ty) => match ty {
                    TypeF::Arrow(uty1, uty2) => {
                        uty1.constrain_fresh_rrows_var(state, p);
                        uty2.constrain_fresh_rrows_var(state, p);
                    }
                    TypeF::Forall {body, ..} => body.constrain_fresh_rrows_var(state, p),
                    TypeF::Dyn
                    | TypeF::Num
                    | TypeF::Bool
                    | TypeF::Str
                    | TypeF::Sym
                    | TypeF::Flat(_)
                    | TypeF::Var(_)
                    // There can be no record rows unification variable inside an enum type
                    | TypeF::Enum(_) => (),
                    | TypeF::Wildcard(_) => (),
                    TypeF::Record(rrows) => rrows.constrain_fresh_rrows_var(state, p),
                    TypeF::Array(uty)
                    | TypeF::Dict(uty) => uty.constrain_fresh_rrows_var(state, p),
                },
                UnifType::Constant(_) | UnifType::Contract(..) => (),
            }
        }
}

impl ConstrainFreshRRowsVar for UnifRecordRows {
    fn constrain_fresh_rrows_var(&self, state: &mut State, p: usize) {
        fn constrain_var(state: &mut State, mut constr: HashSet<Ident>, uty: &UnifRecordRows, p: usize) {
            match uty {
                UnifRecordRows::UnifVar(u) if p == *u && !constr.is_empty() => {
                    state.constr.insert(p, constr);
                }
                UnifRecordRows::UnifVar(u) => match state.table.root_rrows(*u) {
                    UnifRecordRows::UnifVar(_) => (),
                    rrows => constrain_var(state, constr, &rrows, p),
                },
                UnifRecordRows::Concrete(ty) => match ty {
                    RecordRowsF::Empty
                    | RecordRowsF::TailDyn
                    | RecordRowsF::TailVar(_) => (),
                    RecordRowsF::Extend {row, tail} => {
                        constr.insert(row.id.clone());
                        row.types.constrain_fresh_rrows_var(state, p);
                        constrain_var(state, constr, &*tail, p);
                    }
                },
                UnifRecordRows::Constant(_) => (),
            }
        }

        constrain_var(state, HashSet::new(), self, p);
    }
}

impl ConstrainFreshERowsVar for UnifType {
    fn constrain_fresh_erows_var(&self, state: &mut State, p: usize) {
        match self {
                UnifType::UnifVar(u) => match state.table.root_type(*u) {
                    UnifType::UnifVar(_) => (),
                    ty => ty.constrain_fresh_erows_var(state, p),
                },
                UnifType::Concrete(ty) => match ty {
                    TypeF::Arrow(uty1, uty2) => {
                        uty1.constrain_fresh_erows_var(state, p);
                        uty2.constrain_fresh_erows_var(state, p);
                    }
                    TypeF::Forall {body, ..} => body.constrain_fresh_erows_var(state, p),
                    TypeF::Dyn
                    | TypeF::Num
                    | TypeF::Bool
                    | TypeF::Str
                    | TypeF::Sym
                    | TypeF::Flat(_)
                    | TypeF::Var(_)
                    | TypeF::Wildcard(_) => (),
                    TypeF::Enum(erows) => erows.constrain_fresh_erows_var(state, p),
                    TypeF::Record(rrows) => rrows.constrain_fresh_erows_var(state, p),
                    TypeF::Array(uty)
                    | TypeF::Dict(uty) => uty.constrain_fresh_erows_var(state, p),
                },
                UnifType::Constant(_) | UnifType::Contract(..) => (),
            }
        }
}

impl ConstrainFreshERowsVar for UnifRecordRows {
    fn constrain_fresh_erows_var(&self, state: &mut State, p: usize) {
            match self {
                UnifRecordRows::UnifVar(u) => match state.table.root_rrows(*u) {
                    UnifRecordRows::UnifVar(_) => (),
                    rrows => rrows.constrain_fresh_erows_var(state, p),
                },
                UnifRecordRows::Concrete(ty) => match ty {
                    RecordRowsF::Empty
                    | RecordRowsF::TailDyn
                    | RecordRowsF::TailVar(_) => (),
                    RecordRowsF::Extend {row, tail} => {
                        row.types.constrain_fresh_erows_var(state, p);
                        tail.constrain_fresh_erows_var(state, p);
                    }
                },
                UnifRecordRows::Constant(_) => (),
            }
    }
}

impl ConstrainFreshERowsVar for UnifEnumRows {
    fn constrain_fresh_erows_var(&self, state: &mut State, p: usize) {
        fn constrain_var(state: &mut State, mut constr: HashSet<Ident>, uty: &UnifEnumRows, p: usize) {
            match uty {
                UnifEnumRows::UnifVar(u) if p == *u && !constr.is_empty() => {
                    state.constr.insert(p, constr);
                }
                UnifEnumRows::UnifVar(u) => match state.table.root_erows(*u) {
                    UnifEnumRows::UnifVar(_) => (),
                    erows => constrain_var(state, constr, &erows, p),
                },
                UnifEnumRows::Concrete(ty) => match ty {
                    EnumRowsF::Empty
                    | EnumRowsF::TailVar(_) => (),
                    EnumRowsF::Extend {row, tail} => {
                        constr.insert(row.clone());
                        constrain_var(state, constr, &*tail, p);
                    }
                },
                UnifEnumRows::Constant(_) => (),
            }
        }

        constrain_var(state, HashSet::new(), self, p);
    }
}

/// Check that unifying a variable with a type doesn't violate record rows constraints, and update
/// the row constraints of the unified type accordingly if needed.
///
/// When a unification variable `Ptr(p)` is unified with a type `uty` which is either a row type or
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
    p: usize,
    mut rrows: &UnifRecordRows,
) -> Result<(), RowUnifError> {
    if let Some(p_constr) = constr.remove(&p) {
            match rrows {
                UnifRecordRows::Concrete(RecordRowsF::Extend {row, ..})
                    if p_constr.contains(&row.id) =>
                {
                    Err(RowUnifError::UnsatConstr(
                        row.id.clone(),
                        Some(UnifType::Concrete(TypeF::Record(rrows.clone()))),
                    ))
                }
                UnifRecordRows::Concrete(RecordRowsF::Extend {tail, ..}) => constr_unify_rrows(constr, p, tail),
                UnifRecordRows::UnifVar(u) if *u != p => {
                    if let Some(u_constr) = constr.get_mut(u) {
                        u_constr.extend(p_constr.into_iter());
                    } else {
                        constr.insert(*u, p_constr);
                    }

                    Ok(())
                }
                _ => Ok(()),
            }
        }
    else {
        Ok(())
    }
}

/// Check that unifying a variable with a type doesn't violate enum rows constraints, and update
/// the row constraints of the unified type accordingly if needed.
///
/// Same as `constr_unify_rrows`, but for enum rows.
pub fn constr_unify_erows(
    constr: &mut RowConstr,
    p: usize,
    mut erows: &UnifEnumRows,
) -> Result<(), RowUnifError> {
    if let Some(p_constr) = constr.remove(&p) {
            match erows {
                UnifEnumRows::Concrete(EnumRowsF::Extend {row, ..})
                    if p_constr.contains(&row) =>
                {
                    Err(RowUnifError::UnsatConstr(
                        row.clone(),
                        Some(UnifType::Concrete(TypeF::Enum(erows.clone()))),
                    ))
                }
                UnifEnumRows::Concrete(EnumRowsF::Extend {tail, ..}) => constr_unify_erows(constr, p, tail),
                UnifEnumRows::UnifVar(u) if *u != p => {
                    if let Some(u_constr) = constr.get_mut(u) {
                        u_constr.extend(p_constr.into_iter());
                    } else {
                        constr.insert(*u, p_constr);
                    }

                    Ok(())
                }
                _ => Ok(()),
            }
        }
    else {
        Ok(())
    }
}

/// Get the typevar associated with a given wildcard ID.
fn get_wildcard_var(
    table: &mut UnifTable,
    wildcard_vars: &mut Vec<UnifType>,
    id: usize,
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
