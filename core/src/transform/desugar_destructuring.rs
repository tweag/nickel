//! Destructuring desugaring
//!
//! Replace a let and function bindings with destructuring by simpler constructs.
use smallvec::SmallVec;

use crate::{
    error::EvalErrorData,
    eval::value::{NickelValue, ValueContent, lens::TermContent},
    identifier::LocIdent,
    position::PosTable,
    term::{BinaryOp, BindingType, FunPatternData, LetAttrs, Term, make, pattern::*},
};

use self::{bindings::Bindings, compile::CompilePart};

/// Entry point of the destructuring desugaring transformation.
///
/// As other `transform_one` variants, this transformation is not recursive and only desugars the
/// top-level constructor of the pattern. It might return a term which still contains simpler
/// destructuring patterns to be desugared in children nodes.
pub fn transform_one(pos_table: &mut PosTable, value: NickelValue) -> NickelValue {
    let pos_idx = value.pos_idx();

    match value.content() {
        ValueContent::Term(term) => match term {
            TermContent::LetPattern(lens) => {
                let (bindings, body, attrs) = lens.take();
                NickelValue::term(desugar_let(pos_table, bindings, body, attrs.rec), pos_idx)
            }
            TermContent::FunPattern(lens) => NickelValue::term(desugar_fun(*lens.take()), pos_idx),
            lens => lens.restore(),
        },
        lens => lens.restore(),
    }
}

/// Desugar a destructuring function.
///
/// A function `fun <pat> => body` is desugared to `fun x => let <pat> = x in body`. The inner
/// destructuring let isn't desugared further, as the general program transformation machinery will
/// take care of transforming the body of the function in a second step.
pub fn desugar_fun(FunPatternData { mut pattern, body }: FunPatternData) -> Term {
    let id = pattern.alias.take().unwrap_or_else(LocIdent::fresh);
    let pos_idx_body = body.pos_idx();

    Term::fun(
        id,
        NickelValue::term(
            Term::LetPattern(
                std::iter::once((pattern, Term::Var(id).into())).collect(),
                body,
                LetAttrs::default(),
            ),
            pos_idx_body,
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
/// if someone tries to evaluate `foo`. Putting it in the body as above raises
/// an error, for example, in `let 'Foo = 'Bar in true`.
///
/// A recursive let-binding is desugared almost the same way, except that everything is
/// shoved into a single let-rec block instead of three nested blocks.
pub fn desugar_let(
    pos_table: &mut PosTable,
    bindings: SmallVec<[(Pattern, NickelValue); 1]>,
    body: NickelValue,
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
    outer_bindings.push((empty_record_id, NickelValue::empty_record()));
    for (pat, rhs) in bindings {
        let pos = pos_table.get(pat.pos).fuse(pos_table.get(rhs.pos_idx()));
        let outer_id = LocIdent::fresh();
        outer_bindings.push((outer_id, rhs.clone()));

        let mid_id = LocIdent::fresh();
        mid_bindings.push((
            mid_id,
            pat.compile_part(pos_table, outer_id, empty_record_id),
        ));

        let error_case = NickelValue::term(
            Term::RuntimeError(EvalErrorData::FailedDestructuring {
                value: rhs.clone(),
                pattern: pat.clone(),
            }),
            pos_table.push(pos),
        );

        let is_record_null = make::op2(BinaryOp::Eq, Term::Var(mid_id), NickelValue::null());
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
