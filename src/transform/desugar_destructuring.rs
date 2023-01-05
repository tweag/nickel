//! Desugar destructuring
//!
//! Replace a let-binding with destructuring by a classical let-binding.
//! It will first destruct the pattern and create a new var for each field of the pattern.
//! After that, it will construct a new Record/Array from the extracted fields.
//!
//! # Example
//!
//! ## The let pattern:
//! ```text
//! let x @ {a, b=d, ..} = {a=1,b=2,c="ignored"} in ...
//! ```
//! will be transformed to:
//! ```text
//! let x = {a=1,b=2,c="ignored"} in
//! let a = x.a in
//! let d = x.b in
//! ...
//! ```
//!
//! ## The function pattern
//! ```text
//! let f = fun x@{a, b=c} {d ? 2, ..w} => <do_something> in ...
//! ```
//! will be transformed to:
//! ```text
//! let f = fun x %unnamed% => (
//!     let {a, b=c} = x in
//!     let {d ? 2, ..w} = %unnamed% in
//!     <do_something>
//! ) in ...
//! ```
use crate::destruct::{Destruct, Match};
use crate::identifier::Ident;
use crate::match_sharedterm;
use crate::term::make::{op1, op2};
use crate::term::{BinaryOp::DynRemove, RichTerm, Term, TypeAnnotation, UnaryOp::StaticAccess};

/// Entry point of the patterns desugaring.
/// It desugar a `RichTerm` if possible (the term is a let pattern or a function with patterns in
/// its arguments).
/// ## Warning:
/// The transformation is generally not recursive. The result can contain patterns itself.
pub fn transform_one(rt: RichTerm) -> RichTerm {
    match *rt.term {
        Term::LetPattern(..) => desugar_with_contract(rt),
        Term::FunPattern(..) => desugar_fun(rt),
        _ => rt,
    }
}

/// Desugar a function with patterns as arguments.
/// This function does not perform nested transformation because internally it's only used in a top
/// down traversal. This means that the return value is a normal `Term::Fun` but it can contain
/// `Term::FunPattern` and `Term::LetPattern` inside.
pub fn desugar_fun(rt: RichTerm) -> RichTerm {
    match_sharedterm! {rt.term, with {
        Term::FunPattern(x, pat, t_) if !pat.is_empty() => {
            let x = x.unwrap_or_else(Ident::fresh);
            let t_pos = t_.pos;
            RichTerm::new(
                Term::Fun(
                    x,
                    RichTerm::new(Term::LetPattern(None, pat, Term::Var(x).into(), t_), t_pos /* TODO: should we use rt.pos? */),
                ),
                rt.pos,
            )
        },
        Term::FunPattern(Some(x), Destruct::Empty, t_) => RichTerm::new(Term::Fun(x, t_), rt.pos),
        t@Term::FunPattern(..) => panic!(
            "A function can not have empty pattern without name in {:?}",
            t
        ),
    } else rt
    }
}

/// Wrap the desugar `LetPattern` in a meta value containing the "Record contract" needed to check
/// the pattern exhaustively and also fill the default values (`?` operator) if not presents in the
/// record. This function should be, in the general case, considered as the entry point of the let
/// patterns transformation.
pub fn desugar_with_contract(rt: RichTerm) -> RichTerm {
    match_sharedterm!(rt.term,
        with {
            Term::LetPattern(x, pat, bound, body) => {
                let pos = body.pos;
                let contract = pat.clone().into_contract();
                let annotated = {
                    let t_pos = bound.pos;
                    RichTerm::new(
                        Term::Annotated(
                            TypeAnnotation { contracts: vec![contract], ..Default::default() },
                            bound
                        ),
                        t_pos,
                    )
                };
                desugar(RichTerm::new(Term::LetPattern(x, pat, annotated, body), pos))
            }
        } else rt
    )
}

/// Main transformation function to desugar let patterns. WARNING: In a real usage case, you will
/// want to generate also the metavalue associated to this pattern destructuring. Do not consider
/// this function as the entry point of the transformation. For that, use `desugar_with_contract`.
pub fn desugar(rt: RichTerm) -> RichTerm {
    match_sharedterm!(rt.term,
        with {
            Term::LetPattern(x, pat, t_, body) => {
                let pos = body.pos;
                let x = x.unwrap_or_else(Ident::fresh);
                RichTerm::new(
                    Term::Let(
                        x,
                        t_,
                        destruct_term(x, &pat, bind_open_field(x, &pat, body)),
                        Default::default(),
                    ),
                    pos,
                )
            }
        } else rt
    )
}

/// Wrap `body` in a let construct binding the open part of the pattern to the required value.
/// Having `let {a,..y} = {a=1, b=2, c=3} in <BODY>` will bind `y` to `{b=2,c=3}` in `BODY`. Here,
/// `x` is the identifier pointing to the full record. If having `val @ {...} = ... in ...` the
/// variable x should be `Ident("val")` but if we have a `@` binding less form, you will probably
/// generate a fresh variable.
fn bind_open_field(x: Ident, pat: &Destruct, body: RichTerm) -> RichTerm {
    let (matches, var) = match pat {
        Destruct::Record {
            matches,
            open: true,
            rest: Some(x),
            ..
        } => (matches, *x),
        Destruct::Record {
            matches,
            open: true,
            rest: None,
            ..
        } => (matches, Ident::fresh()),
        Destruct::Record {
            open: false,
            rest: None,
            ..
        }
        | Destruct::Empty => return body,
        _ => panic!("A closed pattern can not have a rest binding"),
    };
    Term::Let(
        var,
        matches.iter().fold(Term::Var(x).into(), |x, m| match m {
            Match::Simple(i, _) | Match::Assign(i, _, _) => {
                op2(DynRemove(), Term::Str(i.to_string()), x)
            }
        }),
        body,
        Default::default(),
    )
    .into()
}

/// Core of the destructuring. Bind all the variables of the pattern except the "open" (`..y`)
/// part. For that, see `bind_open_field`.
fn destruct_term(x: Ident, pat: &Destruct, body: RichTerm) -> RichTerm {
    let pos = body.pos;
    match pat {
        Destruct::Record { matches, .. } => matches.iter().fold(body, move |t, m| match m {
            Match::Simple(id, _) => RichTerm::new(
                Term::Let(
                    *id,
                    op1(StaticAccess(*id), Term::Var(x)),
                    t,
                    Default::default(),
                ),
                pos,
            ),
            Match::Assign(f, _, (id, pat)) => desugar(RichTerm::new(
                Term::LetPattern(*id, pat.clone(), op1(StaticAccess(*f), Term::Var(x)), t),
                pos,
            )),
        }),
        _ => body,
    }
}
