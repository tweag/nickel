//! Desugar destructuring
//!
//! Replace a let-binding with destructuring by a classical let-binding.
//! It will first destruct the pattern and create a new var for each field of the pattern.
//! After that, it will construct a new Record/List from the extracted fields.
//!
//! # Example
//!
//! The let pattern:
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
use crate::destruct::{Destruct, Match};
use crate::identifier::Ident;
use crate::match_sharedterm;
use crate::term::make::{op1, op2};
use crate::term::{
    BinaryOp::DynRemove, BindingType, MetaValue, RichTerm, Term, UnaryOp::StaticAccess,
};

/// Wrap the desugar term in a meta value containing the "Record contract" needed to check the
/// pattern exhaustively and also fill the default values (`?` operator) if not presents in the
/// record. This function should be, in the general case, considered as the entry point of this
/// transformation.
pub fn desugar_with_contract(rt: RichTerm) -> RichTerm {
    match_sharedterm!(rt.term,
        with {
            Term::LetPattern(x, pat, t_, body) => {
                let pos = body.pos;
                let meta = pat.clone().as_contract();
                let t_ = {
                    let t_pos = t_.pos;
                    RichTerm::new(
                        Term::MetaValue(MetaValue {
                            value: Some(t_),
                            ..meta
                        }),
                        t_pos,
                    )
                };
                desugar(RichTerm::new(Term::LetPattern(x, pat, t_, body), pos))
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
                let x = x.unwrap_or_else(super::fresh_var);
                RichTerm::new(
                    Term::Let(
                        x.clone(),
                        t_,
                        destruct_term(x.clone(), &pat, bind_open_field(x, &pat, body)),
                        BindingType::Normal,
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
        Destruct::Record(matches, true, Some(x)) => (matches, x.clone()),
        Destruct::Record(matches, true, None) => (matches, super::fresh_var()),
        Destruct::Record(_, false, None) | Destruct::Empty => return body,
        _ => panic!("A closed pattern can not have a rest binding"),
    };
    Term::Let(
        var.clone(),
        matches.iter().fold(Term::Var(x).into(), |x, m| match m {
            Match::Simple(i, _) | Match::Assign(i, _, _) => {
                op2(DynRemove(), Term::Str(i.to_string()), x)
            }
        }),
        body,
        BindingType::Normal,
    )
    .into()
}

/// Core of the destructuring. Bind all the variables of the pattern except the "open" (`..y`)
/// part. For that, see `bind_open_field`.
fn destruct_term(x: Ident, pat: &Destruct, body: RichTerm) -> RichTerm {
    let pos = body.pos;
    match pat {
        Destruct::Record(matches, ..) => matches.iter().fold(body, move |t, m| match m {
            Match::Simple(id, _) => RichTerm::new(
                Term::Let(
                    id.clone(),
                    op1(StaticAccess(id.clone()), Term::Var(x.clone())),
                    t,
                    BindingType::Normal,
                ),
                pos,
            ),
            Match::Assign(f, _, (id, pat)) => desugar(RichTerm::new(
                Term::LetPattern(
                    id.clone(),
                    pat.clone(),
                    op1(StaticAccess(f.clone()), Term::Var(x.clone())),
                    t,
                ),
                pos,
            )),
        }),
        _ => body,
    }
}
