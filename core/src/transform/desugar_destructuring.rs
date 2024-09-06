//! Destructuring desugaring
//!
//! Replace a let and function bindings with destructuring by simpler constructs.
use std::rc::Rc;

use crate::{
    identifier::LocIdent,
    match_sharedterm,
    position::TermPos,
    term::{
        array::{Array, ArrayAttrs},
        pattern::*,
        BindingType, LetAttrs, MatchBranch, MatchData, RichTerm, Term,
    },
};

/// Entry point of the destructuring desugaring transformation.
///
/// As other `transform_one` variants, this transformation is not recursive and only desugars the
/// top-level constructor of the pattern. It might return a term which still contains simpler
/// destructuring patterns to be desugared in children nodes.
pub fn transform_one(rt: RichTerm) -> RichTerm {
    match_sharedterm!(match (rt.term) {
        Term::LetPattern(bindings, body, attrs) =>
            RichTerm::new(desugar_let(bindings, body), rt.pos),
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
    let attrs = LetAttrs {
        binding_type: BindingType::Normal,
        rec: false,
    };

    Term::Fun(
        id,
        RichTerm::new(
            Term::LetPattern(
                std::iter::once((pat, Term::Var(id).into())).collect(),
                body,
                attrs,
            ),
            // TODO: should we use rt.pos?
            pos_body,
        ),
    )
}

/// Desugar a destructuring let-binding.
///
/// A let-binding `let <pat1> = <bound1>, <pat2> = <bound2> in body` is desugared to
/// `[<bound1>, <bound2>] |> match { [<pat1>, <pat2>] => body }`. If there is only one
/// pattern, we drop the arrays in the pattern and the bound (so, `<bound1> |> match { <pat1> => body }`).
pub fn desugar_let(
    bindings: impl IntoIterator<Item = (Pattern, RichTerm)>,
    body: RichTerm,
) -> Term {
    // the position of the match expression is used during error reporting, so we try to provide a
    // sensible one.
    let mut bounds_pos = TermPos::None;
    let mut patterns_pos = TermPos::None;
    let mut patterns = Vec::new();
    let mut bound = Vec::new();
    for (pat, rt) in bindings {
        bounds_pos = bounds_pos.fuse(rt.pos);
        patterns_pos = patterns_pos.fuse(pat.pos);
        patterns.push(pat);
        bound.push(rt);
    }

    let (pattern, bound) = if patterns.len() == 1 {
        // unwrap: pattern and bound have the same length (1)
        (
            patterns.into_iter().next().unwrap(),
            bound.into_iter().next().unwrap(),
        )
    } else {
        (
            Pattern {
                data: PatternData::Array(ArrayPattern {
                    patterns,
                    tail: TailPattern::Empty,
                    pos: patterns_pos,
                }),
                alias: None,
                pos: patterns_pos,
            },
            RichTerm::from(Term::Array(
                Array::new(Rc::from(bound)),
                ArrayAttrs::default(),
            ))
            .with_pos(bounds_pos),
        )
    };

    // `(match { [<pat1>, ..., <patn>] => <body> }) [<bound1>, ..., <boundn>]`
    Term::App(
        RichTerm::new(
            Term::Match(MatchData {
                branches: vec![MatchBranch {
                    pattern,
                    guard: None,
                    body,
                }],
            }),
            bounds_pos.fuse(patterns_pos),
        ),
        bound,
    )
}
