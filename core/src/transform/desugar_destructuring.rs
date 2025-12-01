//! Destructuring desugaring
//!
//! Replace a let and function bindings with destructuring by simpler constructs.
use std::collections::HashMap;

use smallvec::SmallVec;

use crate::{
    error::EvalErrorKind,
    eval::value::{NickelValue, RecordData, ValueContent, lens::TermContent},
    identifier::LocIdent,
    mk_app,
    position::PosTable,
    term::{
        BindingType, FunPatternData, LetAttrs, LetPatternData, Term, UnaryOp, make, pattern::*,
    },
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
                let data = lens.take_unboxed();
                NickelValue::term(desugar_let(pos_table, data), pos_idx)
            }
            TermContent::FunPattern(lens) => {
                NickelValue::term(desugar_fun(lens.take_unboxed()), pos_idx)
            }
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
            Term::let_pattern(
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
/// in which `<pat1>` binds `foo` and `<pat2>` binds `bar` is desugared to
///
/// ```text
/// let
///   %b1 = <bound1>,
///   %b2 = <bound2>,
/// in
/// let
///   %r1 = <pat1.compile_part(%b1, { foo = %c1 }, <error>, <{ foo => %c1 }>)>,
///   %r2 = <pat2.compile_part(%b2, { baz = %c2 }, <error>), <{ bar => %c2 }>)>,
/// in
/// let
///   foo = %r1.foo,
///   ...
///   baz = %r2.baz,
/// in (%seq% %r1) (%seq% %r2) body
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
    LetPatternData {
        bindings,
        body,
        attrs,
    }: LetPatternData,
) -> Term {
    // Outer bindings are the ones we called %b1 and %b2, and %empty_record_id in the doc above.
    let mut outer_bindings = SmallVec::new();
    // Mid bindings are the ones we called %r1 and %r2 above.
    let mut mid_bindings = SmallVec::new();
    // Inner bindings are the ones that bind the actual variables defined in the patterns.
    let mut inner_bindings = SmallVec::new();

    for (pat, rhs) in bindings {
        let pos = pos_table.get(pat.pos).fuse(pos_table.get(rhs.pos_idx()));
        let error_case = NickelValue::term(
            Term::RuntimeError(Box::new(EvalErrorKind::FailedDestructuring {
                value: rhs.clone(),
                pattern: pat.clone(),
            })),
            pos_table.push(pos),
        );

        let outer_id = LocIdent::fresh();
        outer_bindings.push((outer_id, rhs.clone()));

        let mid_id = LocIdent::fresh();

        let mut pattern_bindings = HashMap::new();
        for (_path, id, _field) in pat.bindings() {
            inner_bindings.push((
                id,
                make::static_access(Term::Var(mid_id), std::iter::once(id)),
            ));
            pattern_bindings.insert(id, LocIdent::fresh());
        }

        // Build the mapping from pattern variables to fresh variables
        // (see `CompilePart::compile_part` for details). This corresponds
        // to the <{ foo => %c1 }> part of the documentation above.
        let bindings_record = NickelValue::record_posless(RecordData {
            fields: pattern_bindings
                .iter()
                .map(|(bound_id, fresh_id)| {
                    (*bound_id, NickelValue::from(Term::Var(*fresh_id)).into())
                })
                .collect(),
            attrs: Default::default(),
            sealed_tail: None,
        });

        mid_bindings.push((
            mid_id,
            pat.compile_part(
                pos_table,
                outer_id,
                bindings_record,
                error_case,
                &pattern_bindings,
            ),
        ));
    }

    // Force all the "mid" ids, to make pattern failures more eager.
    // Without this, `let 'Foo = 'Bar in 1` wouldn't fail.
    let checked_body = mid_bindings.iter().rev().fold(body, |acc, (id, _)| {
        mk_app!(make::op1(UnaryOp::Seq, Term::Var(*id)), acc)
    });

    let attrs = LetAttrs {
        binding_type: BindingType::Normal,
        rec: attrs.rec,
    };
    if attrs.rec {
        Term::let_in(
            outer_bindings
                .into_iter()
                .chain(mid_bindings)
                .chain(inner_bindings)
                .collect(),
            checked_body,
            attrs,
        )
    } else {
        Term::let_in(
            outer_bindings,
            Term::let_in(
                mid_bindings,
                Term::let_in(inner_bindings, checked_body, attrs.clone()).into(),
                attrs.clone(),
            )
            .into(),
            attrs,
        )
    }
}
