//! Destructuring desugaring
//!
//! Replace a let and function bindings with destructuring by simpler constructs.
use smallvec::SmallVec;

use crate::{
    error::EvalError,
    identifier::LocIdent,
    match_sharedterm,
    term::{make, pattern::*, record::RecordData, BinaryOp, BindingType, LetAttrs, RichTerm, Term},
};

use self::{bindings::Bindings, compile::CompilePart};

/// Entry point of the destructuring desugaring transformation.
///
/// As other `transform_one` variants, this transformation is not recursive and only desugars the
/// top-level constructor of the pattern. It might return a term which still contains simpler
/// destructuring patterns to be desugared in children nodes.
pub fn transform_one(rt: RichTerm) -> RichTerm {
    match_sharedterm!(match (rt.term) {
        Term::LetPattern(bindings, body, attrs) =>
            RichTerm::new(desugar_let(bindings, body, attrs.rec), rt.pos),
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
/// A let-binding
///
/// ```text
/// let
///   <pat1> = <bound1>,
///   <pat2> = <bound2>
/// in body
/// ```
///
/// is desugared to
///
/// ```text
/// let
///   %b1 = <bound1>,
///   %b2 = <bound2>,
///   %empty_record = {},
/// in
/// let
///   %r1 = <pat1.compile_part(%b1, %empty_record)>,
///   %r2 = <pat2.compile_part(%b2, %empty_record)>,
/// in
/// let
///   foo = %r1.foo,
///   ...
///   baz = %r2.baz,
/// in
///   if %r1 == null then <error1> else if %r2 == null then <error2> else body
/// ```
/// where `foo` and `baz` are names bound in `<pat1>` and `<pat2>`.
///
/// There's some ambiguity about where to put the error-checking. It might be natural
/// to put it before trying to access `%r1.foo`, but that would only raise the error
/// if someone tries to evaluate `foo`. The place we've put it above puts the error
/// check raises an error, for example, in `let 'Foo = 'Bar in true`.
///
/// A recursive let-binding is desugared almost the same way, except that everything is
/// shoved into a single let-rec block instead of three nested blocks.
pub fn desugar_let(
    bindings: SmallVec<[(Pattern, RichTerm); 1]>,
    body: RichTerm,
    rec: bool,
) -> Term {
    // Outer bindings are the ones we called %b1 and %b2, and %empty_record_id in the doc above.
    let mut outer_bindings = SmallVec::new();
    // Mid bindings are the ones we called %r1 and %r2 above.
    let mut mid_bindings = SmallVec::new();
    // Inner bindings are the ones that bind the actual variables defined in the patterns.
    let mut inner_bindings = SmallVec::new();
    let mut error_tests = Vec::new();

    let empty_record_id = LocIdent::fresh();
    outer_bindings.push((empty_record_id, Term::Record(RecordData::empty()).into()));
    for (pat, rhs) in bindings {
        let pos = pat.pos.fuse(rhs.pos);
        let outer_id = LocIdent::fresh();
        outer_bindings.push((outer_id, rhs.clone()));

        let mid_id = LocIdent::fresh();
        mid_bindings.push((mid_id, pat.compile_part(outer_id, empty_record_id)));

        let error_case = RichTerm::new(
            Term::RuntimeError(EvalError::FailedDestructuring {
                value: rhs.clone(),
                pattern: pat.clone(),
            }),
            pos,
        );

        let is_record_null = make::op2(BinaryOp::Eq, Term::Var(mid_id), Term::Null);
        error_tests.push((is_record_null, error_case));

        for (_path, id, _field) in pat.bindings() {
            inner_bindings.push((
                id,
                make::static_access(Term::Var(mid_id), std::iter::once(id)),
            ));
        }
    }

    let checked_body = error_tests
        .into_iter()
        .rev()
        .fold(body, |acc, (check, error)| {
            make::if_then_else(check, error, acc)
        });

    let attrs = LetAttrs {
        binding_type: BindingType::Normal,
        rec,
    };
    if rec {
        Term::Let(
            outer_bindings
                .into_iter()
                .chain(mid_bindings)
                .chain(inner_bindings)
                .collect(),
            checked_body,
            attrs,
        )
    } else {
        Term::Let(
            outer_bindings,
            Term::Let(
                mid_bindings,
                Term::Let(inner_bindings, checked_body, attrs.clone()).into(),
                attrs.clone(),
            )
            .into(),
            attrs,
        )
    }
}
