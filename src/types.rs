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

use std::{collections::HashMap, fmt};

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
    /// See [`crate::term::Term::Sealed`].
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
    Record(Ty /* Row */),
    /// A dictionary type.
    // Dict will only have a default type, this is simpler for now, I don't think we lose much
    Dict(Ty),
    /// A parametrized array.
    Array(Ty),
    /// A type wildcard, wrapping an ID unique within a given file.
    Wildcard(usize),
}

impl<Ty> AbsType<Ty> {
    pub fn try_map<To, F, E>(self, mut f: F) -> Result<AbsType<To>, E>
    where
        F: FnMut(Ty) -> Result<To, E>,
    {
        match self {
            AbsType::Dyn() => Ok(AbsType::Dyn()),
            AbsType::Num() => Ok(AbsType::Num()),
            AbsType::Bool() => Ok(AbsType::Bool()),
            AbsType::Str() => Ok(AbsType::Str()),
            AbsType::Sym() => Ok(AbsType::Sym()),
            AbsType::Flat(t) => Ok(AbsType::Flat(t)),
            AbsType::Arrow(s, t) => Ok(AbsType::Arrow(f(s)?, f(t)?)),
            AbsType::Var(i) => Ok(AbsType::Var(i)),
            AbsType::Forall(i, t) => Ok(AbsType::Forall(i, f(t)?)),
            AbsType::RowEmpty() => Ok(AbsType::RowEmpty()),
            AbsType::RowExtend(id, t1, t2) => {
                let t1_mapped = match t1 {
                    Some(ty) => Some(f(ty)?),
                    None => None,
                };

                Ok(AbsType::RowExtend(id, t1_mapped, f(t2)?))
            }
            AbsType::Enum(t) => Ok(AbsType::Enum(f(t)?)),
            AbsType::Record(t) => Ok(AbsType::Record(f(t)?)),
            AbsType::Dict(t) => Ok(AbsType::Dict(f(t)?)),
            AbsType::Array(t) => Ok(AbsType::Array(f(t)?)),
            AbsType::Wildcard(i) => Ok(AbsType::Wildcard(i)),
        }
    }

    pub fn map<To, F>(self, mut f: F) -> AbsType<To>
    where
        F: FnMut(Ty) -> To,
    {
        let f_lift = |ty: Ty| -> Result<To, ()> { Ok(f(ty)) };
        self.try_map(f_lift).unwrap()
    }

    /// Determine if a type is a row type.
    pub fn is_row_type(&self) -> bool {
        matches!(
            self,
            AbsType::RowExtend(..) | AbsType::RowEmpty() | AbsType::Dyn()
        )
    }

    /// Determine if a type is a type defined by a Nickel term.
    pub fn is_flat(&self) -> bool {
        matches!(self, AbsType::Flat(_))
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self, AbsType::Wildcard(_))
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

/// Concrete, recursive type for a Nickel type.
#[derive(Clone, PartialEq, Debug)]
pub struct Types(pub AbsType<Box<Types>>);

/// An item as returned by an iterator over a row type.
///
/// The parameter `T` is there because row iterators are implemented for different flavours of
/// [`AbsType`] (currently for [`Types`] and [`crate::typewrapper::TypeWrapper`]).
pub enum RowIteratorItem<'a, T> {
    /// A non-empty tail.
    Tail(&'a T),
    /// A row binding.
    Row(&'a Ident, Option<&'a T>),
}

/// An iterator over a row type, returning items by reference.
pub struct RowIterator<'a, T> {
    pub(crate) next: Option<&'a T>,
}

impl<'a> Iterator for RowIterator<'a, Types> {
    type Item = RowIteratorItem<'a, Types>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.and_then(|next| match next {
            Types(AbsType::RowEmpty()) => None,
            Types(AbsType::RowExtend(id, ty_row, tail)) => {
                self.next = Some(tail);
                Some(RowIteratorItem::Row(id, ty_row.as_ref().map(Box::as_ref)))
            }
            ty => {
                self.next = None;
                Some(RowIteratorItem::Tail(ty))
            }
        })
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
            AbsType::Dyn() => contract::dynamic(),
            AbsType::Num() => contract::num(),
            AbsType::Bool() => contract::bool(),
            AbsType::Str() => contract::string(),
            //TODO: optimization: have a specialized contract for `Array Dyn`, to avoid mapping an
            //always successful contract on each element.
            AbsType::Array(ref ty) => mk_app!(contract::array(), ty.subcontract(h, pol, sy)?),
            AbsType::Sym() => panic!("Are you trying to check a Sym at runtime?"),
            AbsType::Arrow(ref s, ref t) => mk_app!(
                contract::func(),
                s.subcontract(h.clone(), !pol, sy)?,
                t.subcontract(h, pol, sy)?
            ),
            AbsType::Flat(ref t) => t.clone(),
            AbsType::Var(ref id) => get_var(&h, id, true)?,
            AbsType::Forall(ref i, ref t) => {
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

                h.insert(i.clone(), (inst_var, inst_tail));
                *sy += 1;
                t.subcontract(h, pol, sy)?
            }
            AbsType::RowEmpty() | AbsType::RowExtend(..) => contract::fail(),
            AbsType::Enum(ref r) => {
                let mut cases = HashMap::new();
                let mut has_tail = false;
                let value_arg = Ident::from("x");
                let label_arg = Ident::from("l");

                for row in r.iter_as_rows() {
                    match row {
                        RowIteratorItem::Row(id, _ty) => {
                            debug_assert!(_ty.is_none());
                            cases.insert(id.clone(), mk_term::var(value_arg.clone()));
                        }
                        RowIteratorItem::Tail(tail) => {
                            // We only expect a type variable in tail position
                            debug_assert!(matches!(tail, Types(AbsType::Var(_))));
                            has_tail = true;
                            break;
                        }
                    }
                }

                // If the enum type has a tail, the tail must be a universally quantified variable,
                // and this means that the tag can be anything.
                let case_body = if has_tail {
                    mk_term::var(value_arg.clone())
                }
                // Otherwise, we build a switch with all the tags as cases, which just returns the
                // original argument, and a default case that blames.
                //
                // For example, for an enum type [| `foo, `bar, `baz |], the `case` function looks
                // like:
                //
                // ```
                // fun l x =>
                //   switch {
                //     `foo => x,
                //     `bar => x,
                //     `baz => x,
                //     _ => $enum_fail l
                //   } x
                // ```
                else {
                    RichTerm::from(Term::Switch(
                        mk_term::var(value_arg.clone()),
                        cases,
                        Some(mk_app!(
                            contract::enum_fail(),
                            mk_term::var(label_arg.clone())
                        )),
                    ))
                };
                let case = mk_fun!(label_arg, value_arg, case_body);

                mk_app!(contract::enums(), case)
            }
            AbsType::Record(ref ty) => {
                // We begin by building a record whose arguments are contracts
                // derived from the types of the statically known fields.
                let mut row = ty;
                let mut fcs = HashMap::new();

                while let AbsType::RowExtend(id, Some(ty), rest) = &row.0 {
                    fcs.insert(*id, ty.subcontract(h.clone(), pol, sy)?);
                    row = rest
                }

                // Now that we've dealt with the row extends, we just need to
                // work out the tail.
                let tail = match &row.0 {
                    AbsType::RowEmpty() => contract::empty_tail(),
                    AbsType::Dyn() => contract::dyn_tail(),
                    AbsType::Var(id) => get_var(&h, id, false)?,
                    ty => panic!(
                        "types::subcontract(): invalid row tail {:?}",
                        Types(ty.clone())
                    ),
                };

                let rec = RichTerm::from(Term::Record(RecordData {
                    fields: fcs,
                    attrs: Default::default(),
                }));

                mk_app!(contract::record(), rec, tail)
            }
            AbsType::Dict(ref ty) => {
                mk_app!(contract::dyn_record(), ty.subcontract(h, pol, sy)?)
            }
            AbsType::Wildcard(_) => contract::dynamic(),
        };

        Ok(ctr)
    }

    /// Find a binding in a record row type. Return `None` if there is no such binding, if the type
    /// is not a row type, or if the row is an enum row.
    pub fn row_find(&self, ident: &Ident) -> Option<Self> {
        match &self.0 {
            AbsType::RowExtend(id, Some(ty), _) if *id == *ident => Some((**ty).clone()),
            AbsType::RowExtend(_, _, tail) => tail.row_find(ident),
            _ => None,
        }
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
        if path.is_empty() {
            return None;
        }

        let next = self.row_find(&path[0]);

        if path.len() == 1 {
            next
        } else {
            match next {
                Some(ty) => ty.row_find_path(&path[1..]),
                _ => None,
            }
        }
    }

    /// Determine if a type is an atom, that is a either an atom or a type delimited by specific
    /// markers (such as a row type). Used in formatting to decide if parentheses need to be
    /// inserted during pretty pretting.
    pub fn fmt_is_atom(&self) -> bool {
        use AbsType::*;

        match &self.0 {
            Dyn() | Num() | Bool() | Str() | Var(_) => true,
            Flat(rt) if matches!(*rt.term, Term::Var(_)) => true,
            _ => false,
        }
    }

    /// Apply a transformation on a whole type by mapping a faillible function `f` on each node in
    /// manner as prescribed by the order.
    /// `f` may return a generic error `E` and use the state `S` which is passed around.
    pub fn traverse<F, S, E>(
        self: Types,
        f: &mut F,
        state: &mut S,
        order: TraverseOrder,
    ) -> Result<Types, E>
    where
        F: FnMut(Types, &mut S) -> Result<Types, E>,
    {
        let ty = match order {
            TraverseOrder::TopDown => f(self, state)?,
            TraverseOrder::BottomUp => self,
        };

        let abs_ty = match ty.0 {
            AbsType::Dyn()
            | AbsType::Num()
            | AbsType::Bool()
            | AbsType::Str()
            | AbsType::Sym()
            | AbsType::Var(_)
            | AbsType::RowEmpty()
            | AbsType::Flat(_)
            | AbsType::Wildcard(_) => Ok(ty.0),
            AbsType::Forall(id, ty_inner) => (*ty_inner)
                .traverse(f, state, order)
                .map(|ty| AbsType::Forall(id, Box::new(ty))),
            AbsType::Enum(ty_inner) => (*ty_inner)
                .traverse(f, state, order)
                .map(Box::new)
                .map(AbsType::Enum),
            AbsType::Record(ty_inner) => (*ty_inner)
                .traverse(f, state, order)
                .map(Box::new)
                .map(AbsType::Record),
            AbsType::Dict(ty_inner) => (*ty_inner)
                .traverse(f, state, order)
                .map(Box::new)
                .map(AbsType::Dict),
            AbsType::Array(ty_inner) => (*ty_inner)
                .traverse(f, state, order)
                .map(Box::new)
                .map(AbsType::Array),
            AbsType::Arrow(ty1, ty2) => {
                let ty1 = (*ty1).traverse(f, state, order)?;
                let ty2 = (*ty2).traverse(f, state, order)?;
                Ok(AbsType::Arrow(Box::new(ty1), Box::new(ty2)))
            }
            AbsType::RowExtend(id, ty_row, tail) => {
                let ty_row = ty_row
                    .map(|ty| (*ty).traverse(f, state, order))
                    .transpose()?;
                let tail = (*tail).traverse(f, state, order)?;
                Ok(AbsType::RowExtend(id, ty_row.map(Box::new), Box::new(tail)))
            }
        }?;

        let result = Types(abs_ty);

        match order {
            TraverseOrder::TopDown => Ok(result),
            TraverseOrder::BottomUp => f(result, state),
        }
    }

    /// Create an iterator on rows represented by this type.
    ///
    /// The iterator continues as long as the next item is of the form `RowExtend(..)`, and
    /// stops once it reaches `RowEmpty` (which ends iteration), or something else which is not a
    /// `RowExtend` (which produces a last item [`typecheck::RowIteratorItem::Tail`]).
    pub fn iter_as_rows(&self) -> RowIterator<'_, Types> {
        RowIterator { next: Some(self) }
    }
}

impl fmt::Display for Types {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            AbsType::Dyn() => write!(f, "Dyn"),
            AbsType::Num() => write!(f, "Num"),
            AbsType::Bool() => write!(f, "Bool"),
            AbsType::Str() => write!(f, "Str"),
            AbsType::Array(ty) => {
                write!(f, "Array ")?;

                if ty.fmt_is_atom() {
                    write!(f, "{}", ty)
                } else {
                    write!(f, "({})", ty)
                }
            }
            AbsType::Sym() => write!(f, "Sym"),
            AbsType::Flat(ref t) => write!(f, "{}", t.pretty_print_cap(32)),
            AbsType::Var(var) => write!(f, "{}", var),
            AbsType::Forall(i, ref ty) => {
                let mut curr: &Types = ty.as_ref();
                write!(f, "forall {}", i)?;
                while let Types(AbsType::Forall(i, ref ty)) = curr {
                    write!(f, " {}", i)?;
                    curr = ty;
                }
                write!(f, ". {}", curr)
            }
            AbsType::Enum(row) => write!(f, "[|{}|]", row),
            AbsType::Record(row) => write!(f, "{{{}}}", row),
            AbsType::Dict(ty) => write!(f, "{{_: {}}}", ty),
            AbsType::RowEmpty() => Ok(()),
            AbsType::RowExtend(id, ty_opt, tail) => {
                if let Some(ty) = ty_opt {
                    write!(f, "{}: {}", id, ty)?;
                } else {
                    write!(f, "`{}", id)?;
                }

                match tail.0 {
                    AbsType::RowEmpty() => write!(f, "{}", tail),
                    AbsType::Var(_) => write!(f, " ; {}", tail),
                    AbsType::Dyn() => write!(f, " ; Dyn"),
                    _ => write!(f, ", {}", tail),
                }
            }
            AbsType::Arrow(dom, codom) => match dom.0 {
                AbsType::Arrow(_, _) => write!(f, "({}) -> {}", dom, codom),
                _ => write!(f, "{} -> {}", dom, codom),
            },
            AbsType::Wildcard(_) => write!(f, "_"),
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
