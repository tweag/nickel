//! Define the Nickel type system.
//!
//! # Base types
//!
//! - Num: a floating-point number
//! - Bool: a boolean
//! - Str: a string literal
//! - Sym: a symbol, used by contracts when checking polymorphic types
//! - List: an (heterogeneous) list
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
//! ## Static records (row types)
//!
//! A row type for a record is a linked list of pairs `(id, type)` indicating the name and the type
//! of each field. Row-polymorphism means that the tail of this list can be a type variable which
//! can be abstracted over, leaving the row open for future extension. A simple and demonstrative
//! example is field access:
//!
//! ```
//! let f = Promise(forall a. { myField : Num, a} -> Num, fun rec => rec.myField)
//! ```
//!
//! The type `{ myField : Num, a }` indicates that any argument must have at least the field
//! `myField` of type `Num`, but may contain any other fields (or no additional field at all).
//!
//! ## Dynamic records
//!
//! A second type available for records is the dynamic record type `{ _ : Type }`. A record of this
//! type may have any number of fields with any names, but they must all be of the same `Type`.
//! This is useful when using record as dictionaries, as such record is indeed a dictionary with
//! string keys and `Type` values. It can be mapped over and accessed in a type-safe manner.
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
use crate::identifier::Ident;
use crate::term::{RichTerm, Term, UnaryOp};
use std::collections::HashMap;

/// A Nickel type.
#[derive(Clone, PartialEq, Debug)]
pub enum AbsType<Ty> {
    /// The dynamic unitype, affected to values which type is not statically known or enforced.
    Dyn(),
    /// A floating point number.
    Num(),
    /// A boolean.
    Bool(),
    /// A string literal.
    Str(),
    /// A symbol.
    ///
    /// See `Wrapped` in [term](../term/enum.Term.html).
    Sym(),
    /// A type created from a user-defined contract.
    Flat(RichTerm),
    /// A function.
    Arrow(Ty, Ty),
    /// A type variable.
    Var(Ident),
    /// A forall binder.
    Forall(Ident, Ty),

    /// An empty row, terminating a row type.
    RowEmpty(),
    /// A row type.
    RowExtend(
        Ident,
        Option<Ty>, /* Type of the field, or None for enums */
        Ty,         /* Tail (another row) */
    ),
    /// An enum type, wrapping a row type for enums.
    Enum(Ty /* Row */),
    /// A record type, wrapping a row type for records.
    StaticRecord(Ty /* Row */),
    /// A dynamic record type, where all fields must have the same type.
    // DynRecord will only have a default type, this is simpler for now, I don't think we lose much
    DynRecord(Ty /*, Ty  Row */),
    /// An heterogeneous list.
    List(),
}

impl<Ty> AbsType<Ty> {
    pub fn map<To, F: Fn(Ty) -> To>(self, f: F) -> AbsType<To> {
        match self {
            AbsType::Dyn() => AbsType::Dyn(),
            AbsType::Num() => AbsType::Num(),
            AbsType::Bool() => AbsType::Bool(),
            AbsType::Str() => AbsType::Str(),
            AbsType::Sym() => AbsType::Sym(),
            AbsType::Flat(t) => AbsType::Flat(t),
            AbsType::Arrow(s, t) => {
                let fs = f(s);
                let ft = f(t);

                AbsType::Arrow(fs, ft)
            }
            AbsType::Var(i) => AbsType::Var(i),
            AbsType::Forall(i, t) => {
                let ft = f(t);

                AbsType::Forall(i, ft)
            }
            AbsType::RowEmpty() => AbsType::RowEmpty(),
            AbsType::RowExtend(id, t1, t2) => AbsType::RowExtend(id, t1.map(&f), f(t2)),
            AbsType::Enum(t) => AbsType::Enum(f(t)),
            AbsType::StaticRecord(t) => AbsType::StaticRecord(f(t)),
            AbsType::DynRecord(t) => AbsType::DynRecord(f(t)),
            AbsType::List() => AbsType::List(),
        }
    }

    pub fn arrow(s: Ty, t: Ty) -> Self {
        AbsType::Arrow(s, t)
    }
}

/// Concrete, recursive type for a Nickel type.
#[derive(Clone, PartialEq, Debug)]
pub struct Types(pub AbsType<Box<Types>>);

impl Types {
    /// Return the contract corresponding to a type.
    ///
    /// Wrapper for [`contract_open`](fn.contract_open.html).
    pub fn contract(&self) -> RichTerm {
        let mut sy = 0;
        self.contract_open(HashMap::new(), true, &mut sy)
    }

    /// Return the contract corresponding to a type.
    ///
    /// # Arguments
    ///
    /// - `h` is an environment mapping type variables to contracts. Type variables are introduced
    /// locally when opening a `forall`.
    /// - `pol` is the current polarity, which is toggled when generating a contract for the argument
    /// of an arrow type (see [`Label`](../label/struct.label.html)).
    /// - `sy` is a counter used to generate fresh symbols for `forall` contracts (see `Wrapped` in
    /// [terms](../term/enum.Term.html).
    pub fn contract_open(
        &self,
        mut h: HashMap<Ident, RichTerm>,
        pol: bool,
        sy: &mut i32,
    ) -> RichTerm {
        match self.0 {
            AbsType::Dyn() => RichTerm::var("dyn".to_string()),
            AbsType::Num() => RichTerm::var("num".to_string()),
            AbsType::Bool() => RichTerm::var("bool".to_string()),
            AbsType::Str() => RichTerm::var("string".to_string()),
            AbsType::List() => RichTerm::var("list".to_string()),
            AbsType::Sym() => panic!("Are you trying to check a Sym at runtime?"),
            AbsType::Arrow(ref s, ref t) => RichTerm::app(
                RichTerm::app(
                    RichTerm::var("func".to_string()),
                    s.contract_open(h.clone(), !pol, sy),
                ),
                t.contract_open(h, pol, sy),
            ),
            AbsType::Flat(ref t) => t.clone(),
            AbsType::Var(ref i) => {
                let rt = h
                    .get(i)
                    .unwrap_or_else(|| panic!("Unbound type variable {:?}", i));
                rt.clone()
            }
            AbsType::Forall(ref i, ref t) => {
                let inst_var = RichTerm::app(
                    RichTerm::app(
                        RichTerm::var("forall_var".to_string()),
                        Term::Sym(*sy).into(),
                    ),
                    Term::Bool(pol).into(),
                );

                h.insert(i.clone(), inst_var);
                *sy += 1;
                t.contract_open(h, pol, sy)
            }
            AbsType::RowEmpty() | AbsType::RowExtend(_, _, _) => RichTerm::var("fail".to_string()),
            AbsType::Enum(ref r) => {
                fn form(ty: Types, h: HashMap<Ident, RichTerm>) -> RichTerm {
                    match ty.0 {
                        AbsType::RowEmpty() => RichTerm::var("fail".to_string()),
                        AbsType::RowExtend(_, Some(_), _) => {
                            panic!("It should be a row without type")
                        }
                        AbsType::RowExtend(id, None, rest) => {
                            let rest_contract = form(*rest, h);
                            let mut map = HashMap::new();
                            map.insert(id, Term::Bool(true).into());

                            RichTerm::app(
                                RichTerm::app(
                                    RichTerm::var("row_extend".to_string()),
                                    rest_contract,
                                ),
                                Term::Fun(
                                    Ident("x".to_string()),
                                    Term::Op1(
                                        UnaryOp::Switch(map, Some(Term::Bool(false).into())),
                                        Term::Var(Ident("x".to_string())).into(),
                                    )
                                    .into(),
                                )
                                .into(),
                            )
                        }
                        AbsType::Var(ref i) => {
                            let rt = h
                                .get(i)
                                .unwrap_or_else(|| panic!("Unbound type variable {:?}", i));
                            rt.clone()
                        }
                        not_row => panic!("It should be a row!! {:?}", not_row),
                    }
                }

                form(*r.clone(), h)
            }
            AbsType::StaticRecord(_) => panic!("TODO implement"),
            AbsType::DynRecord(_) => panic!("TODO implement"),
        }
    }
}
