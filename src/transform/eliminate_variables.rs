//! Enrich variable terms with De Bruijn indices.
//!
//! During evaluation, we clone the environment at each let-binding or function call.
//! This means that every envrionment is a node in a tree. When resolving a variable,
//! we use the `layer` field in `VarAttrs` as an index through our view of the graph,
//! i.e a linked list.
//!
//! ```nickel
//! let even = fun x =>
//!   let x = x % 2
//!   in x
//! in even 42
//! ```
//! The evaluator reduces a series of closures in the following fashion:
//!
//! > let even = fun x =>          |
//!     let x = x + 2              |
//!     in x                       |
//!   in even 42                   | {}
//! > even 42                      | e0 = { even = fun x => .. }
//! > even                         | e0
//! > fun x => let x = x % 2 in x  | { }
//! > let x = x % 2 in x           | e1 = { x = (42,    e0)    }
//! > x                            | e2 = { x = (x % 2, e1)    }
//! > x % 2                        | e1
//! > x                            | e1
//! > 42                           | e0
//! > 2                            | e1
//! > 0                            | {}
//!
//! With the nameless representation, we can write the program as:
//!
//! ```nickel
//! let = fun =>
//!   let = #0 % 2
//!   in #0
//! in #0 42
//! ```
//!
//! The number following `#` represents how many layers we need to
//! traverse up the chain of envrionments in order to resolve the
//! variable. Binders either correspond to let-expressions or
//! closure expressions.
//!
//! In the above example we can see three distinct binders, each of
//! which will generate an environment during evaluation. These
//! environments generally make up a tree structure.
//!
//!         { fun => .. } <- { 42 } <- { #1 % 2 }

use fral::rc::Fral;

use crate::{
    identifier::Ident,
    term::{RichTerm, SharedTerm, StrChunk, Symbol, Term},
    types::{AbsType, Types, UnboundTypeVariableError},
};

/// Apply the full transfomation on a term.
pub fn transform(rt: &mut RichTerm) -> Result<(), UnboundTypeVariableError> {
    eliminate_variables(rt, Fral::new())
}

/// Add a De Bruijn index to all variable terms in the syntax tree.
fn eliminate_variables(
    rt: &mut RichTerm,
    mut layers: Fral<Ident>,
) -> Result<(), UnboundTypeVariableError> {
    match SharedTerm::make_mut(&mut rt.term) {
        Term::Null
        | Term::Bool(_)
        | Term::Num(_)
        | Term::Str(_)
        | Term::Lbl(_)
        | Term::Symbol(_)
        | Term::Enum(_)
        | Term::SealingKey(_)
        | Term::Import(_)
        | Term::ResolvedImport(_)
        | Term::ParseError => Ok(()),
        Term::Let(x, t1, t2, attrs) => {
            // TODO: handle recursive-bindings.
            if attrs.rec {
                layers = layers.cons(x.clone());
                eliminate_variables(t1, layers.clone())?;
            } else {
                eliminate_variables(t1, layers.clone())?;
                layers = layers.cons(x.clone());
            }

            eliminate_variables(t2, layers)
        }
        Term::Fun(x, t) => {
            layers = layers.cons(x.clone());
            eliminate_variables(t, layers)
        }
        // TODO: This will require sharing code with `free_vars`.
        Term::FunPattern(_, _, _) => todo!(),
        Term::LetPattern(_, _, _, _) => todo!(),
        Term::App(t0, t1) => {
            eliminate_variables(t0, layers.clone())?;
            eliminate_variables(t1, layers)
        }
        Term::Record(map, _) | Term::RecRecord(map, _, _, _) => {
            for t in map.values_mut() {
                eliminate_variables(t, layers.clone())?;
            }

            Ok(())
        }
        Term::Switch(t0, ts, t1) => {
            eliminate_variables(t0, layers.clone())?;
            for t in ts.values_mut() {
                eliminate_variables(t, layers.clone())?;
            }

            if let Some(t) = t1 {
                eliminate_variables(t, layers)?;
            }

            Ok(())
        }
        Term::Array(ts, _) => {
            for t in ts.iter_mut() {
                eliminate_variables(t, layers.clone())?;
            }

            Ok(())
        }
        Term::Op1(_, t) => eliminate_variables(t, layers),
        Term::Op2(_, t0, t1) => {
            eliminate_variables(t0, layers.clone())?;
            eliminate_variables(t1, layers)
        }
        Term::OpN(_, ts) => {
            for t in ts.iter_mut() {
                eliminate_variables(t, layers.clone())?;
            }

            Ok(())
        }
        Term::Sealed(_, t) => eliminate_variables(t, layers),
        Term::StrChunks(chunks) => {
            for chunk in chunks {
                if let StrChunk::Expr(t, _) = chunk {
                    eliminate_variables(t, layers.clone())?;
                }
            }

            Ok(())
        }
        Term::MetaValue(mv) => {
            for ctr in &mut mv.contracts {
                eliminate_contract_variables(&mut ctr.types, layers.clone())?;
            }

            if let Some(t) = &mut mv.value {
                eliminate_variables(t, layers)?;
            }

            Ok(())
        }
        t => {
            // NOTE: We cannot put the `Term::Var` pattern in the match arm because
            // it will leader to a mutable borrow of `t` and an immutable borrow of `x`.
            // Instead, we reborrow `t` as immutable for a clone and then mutate it at the end.
            if let Term::Var(ident) = t.clone() {
                *t = if let Some(index) = layers.into_iter().position(|x| *x == ident) {
                    Term::Symbol(Symbol::new(index, ident))
                } else {
                    // FIXME: the symbol isn't in our environment, so it must be a global (?)

                    Term::Symbol(Symbol::local(ident))
                };
            }

            Ok(())
        }
    }
}

/// Add a De Bruijn index to all variable terms in the contract.
fn eliminate_contract_variables(
    ty: &mut Types,
    layers: Fral<Ident>,
) -> Result<(), UnboundTypeVariableError> {
    match &mut ty.0 {
        AbsType::Dyn()
        | AbsType::Num()
        | AbsType::Bool()
        | AbsType::Str()
        | AbsType::Sym()
        | AbsType::Var(..)
        | AbsType::RowEmpty()
        | AbsType::Wildcard(_) => Ok(()),
        AbsType::Flat(rt) => eliminate_variables(rt, layers),
        AbsType::Arrow(ty0, ty1) => {
            eliminate_contract_variables(ty0, layers.clone())?;
            eliminate_contract_variables(ty1, layers.clone())
        }
        AbsType::Forall(_, ty) => eliminate_contract_variables(ty, layers),
        AbsType::RowExtend(_, row, tail) => {
            if let Some(ty) = row {
                eliminate_contract_variables(ty, layers.clone())?;
            }

            eliminate_contract_variables(tail, layers)
        }
        AbsType::Enum(ty)
        | AbsType::StaticRecord(ty)
        | AbsType::DynRecord(ty)
        | AbsType::Array(ty) => eliminate_contract_variables(ty, layers),
    }
}
