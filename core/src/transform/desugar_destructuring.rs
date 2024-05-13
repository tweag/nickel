//! Destructuring desugaring
//!
//! Replace a let-binding with destructuring by simpler constructs.
//!
//! A destructuring `let pat> = <destr> in <body>` is rewritten to the pattern matching
//! `<destr> |> match { <pat> => <body> }`.
//!
//! A destructuring function `fun <pat> => <body>` is rewritten to
//! `fun x => let <pat> = x in <body>`, and then the inner let is recursively desugared.
use crate::{
    identifier::LocIdent,
    match_sharedterm,
    term::{pattern::*, MatchBranch, MatchData, RichTerm, Term},
};

/// Entry point of the destructuring desugaring transformation.
///
/// As other `transform_one` variants, this transformation is not recursive and only desugars the
/// top-level constructor of the pattern. It might return a term which still contains simpler
/// destructuring patterns to be desugared in children nodes.
pub fn transform_one(rt: RichTerm) -> RichTerm {
    match_sharedterm!(match (rt.term) {
        Term::LetPattern(pat, bound, body) => RichTerm::new(desugar_let(pat, bound, body), rt.pos),
        Term::FunPattern(pat, body) => RichTerm::new(desugar_fun(pat, body), rt.pos),
        _ => rt,
    })
}

/// Desugar a destructuring function.
///
/// A function `fun <pat> => body` is desugared to `fun x => let <pat> = x in body`. The inner
/// destructuring let isn't desugared further, as the general program transformation machinery will
/// take care of transforming the body of the function in a second step.
pub fn desugar_fun(mut pat: Pattern, body: RichTerm) -> Term {
    let id = pat.alias.take().unwrap_or_else(LocIdent::fresh);
    let pos_body = body.pos;

    Term::Fun(
        id,
        RichTerm::new(
            Term::LetPattern(pat, Term::Var(id).into(), body),
            // TODO: should we use rt.pos?
            pos_body,
        ),
    )
}

/// Desugar a destructuring let-binding.
///
/// A let-binding `let <pat> = bound in body` is desugared to `<bound> |> match { <pat> => body }`.
pub fn desugar_let(pattern: Pattern, bound: RichTerm, body: RichTerm) -> Term {
    // the position of the match expression is used during error reporting, so we try to provide a
    // sensible one.
    let match_expr_pos = pattern.pos.fuse(bound.pos);

    // `(match { <pat> => <body> }) <bound>`
    Term::App(
        RichTerm::new(
            Term::Match(MatchData {
                branches: vec![MatchBranch {
                    pattern,
                    guard: None,
                    body,
                }],
            }),
            match_expr_pos,
        ),
        bound,
    )
}
