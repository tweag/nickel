//! Destructuring desugaring
//!
//! Replace a let-binding with destructuring by a sequence of normal let-binding.
//!
//! # Example
//!
//! ## Let-binding
//!
//! The following destruring let-binding:
//!
//! ```text
//! let x @ {a, b=d, ..} = {a=1,b=2,c="ignored"} in ...
//! ```
//!
//! will be transformed to:
//!
//! ```text
//! let x = {a=1,b=2,c="ignored"} in
//! let a = x.a in
//! let d = x.b in
//! ...
//! ```
//!
//! ## Function
//!
//! The following desctructuring function:
//!
//! ```text
//! let f = fun x@{a, b=c} {d ? 2, ..w} => <do_something> in ...
//! ```
//!
//! will be transformed to:
//!
//! ```text
//! let f = fun x %unnamed% => (
//!     let {a, b=c} = x in
//!     let {d ? 2, ..w} = %unnamed% in
//!     <do_something>
//! ) in ...
//! ```
use crate::identifier::LocIdent;
use crate::match_sharedterm;
use crate::term::pattern::*;
use crate::term::{
    make::{op1, op2},
    BinaryOp::DynRemove,
    LetAttrs, RecordOpKind, RichTerm, Term, TypeAnnotation,
    UnaryOp::StaticAccess,
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

/// Elaborate a contract from the pattern if it is a record pattern and applies to the value before
/// actually destructuring it. Then convert the let pattern to a sequence of normal let-bindings.
pub fn desugar_let(pat: Pattern, bound: RichTerm, body: RichTerm) -> Term {
    let contract = pat.elaborate_contract();

    let annotated = {
        let t_pos = bound.pos;
        RichTerm::new(
            Term::Annotated(
                TypeAnnotation {
                    contracts: contract.into_iter().collect(),
                    ..Default::default()
                },
                bound,
            ),
            t_pos,
        )
    };

    pat.desugar(annotated, body)
}

trait Desugar {
    /// Elaborate a destructuring let-binding matching a pattern `self` against a value `destr` to
    /// a sequence of normal let-bindings and primitive operations.
    ///
    /// This function ignores the user-supplied contracts of the pattern and doesn't generate a
    /// safety contract to check that the value has the expected shape: this guarding contract must
    /// be introduced separately and prior to calling to [Desugar::desugar]. In practice, this
    /// contract is introduced by [desugar_let]. [Desugar::desugar] is only concerned with
    /// destructuring the value and binding its parts to appropriate variables.
    fn desugar(self, destr: RichTerm, body: RichTerm) -> Term;
}

impl Desugar for Pattern {
    fn desugar(self, destr: RichTerm, body: RichTerm) -> Term {
        // If the pattern is aliased, `x @ <pat>` matching `destr`, we introduce a heading
        // let-binding `let x = destruct in <elaborated>`, where `<elaborated>` is the desugaring
        // of `<pat>` matching `x` followed by the original `body`.
        if let Some(alias) = self.alias {
            let pos = body.pos;
            let inner = RichTerm::new(
                self.data
                    .desugar(RichTerm::new(Term::Var(alias), alias.pos), body),
                pos,
            );

            Term::Let(alias, destr, inner, LetAttrs::default())
        } else {
            self.data.desugar(destr, body)
        }
    }
}

impl Desugar for PatternData {
    fn desugar(self, destr: RichTerm, body: RichTerm) -> Term {
        match self {
            // If the pattern is an unconstrained identifier, we just bind it to the value.
            PatternData::Any(id) => Term::Let(id, destr, body, LetAttrs::default()),
            PatternData::Record(pat) => pat.desugar(destr, body),
        }
    }
}

impl Desugar for FieldPattern {
    // For a filed pattern, we assume that the `destr` argument is the whole record being
    // destructured. We extract the field from `destr` and desugar the rest of the pattern against
    // `destr.matched_id`.
    fn desugar(self, destr: RichTerm, body: RichTerm) -> Term {
        let extracted = op1(StaticAccess(self.matched_id), destr.clone());
        self.pattern.desugar(extracted, body)
    }
}

impl Desugar for RecordPattern {
    fn desugar(self, destr: RichTerm, body: RichTerm) -> Term {
        let pos = body.pos;
        // The body is the rest of the term being transformed, which contains the code that uses
        // the bindings introduced by the pattern. After having extracting all fields from the
        // value, we potentially need to capture the rest in a variable for patterns with a
        // capturing tail. For example, `let {foo, bar, ..rest} = destr in body` should be
        // desugared as `let foo = destr.foo in let bar = destr.bar in let rest = <rest> in body`
        // (where `<rest>` is some expression removing `foo` and `bar` from `destr`).
        //
        // Because body is the continuation, we need to first append the rest capture to the
        // original body before passing it to the [Desugar::desugar] of each individual field.
        let body_with_rest = bind_rest(&self, destr.clone(), body);

        self.patterns
            .into_iter()
            .fold(body_with_rest, |acc, field_pat| {
                RichTerm::new(field_pat.desugar(destr.clone(), acc), pos)
            })
            .term
            .into_owned()
    }
}

fn bind_rest(pat: &RecordPattern, destr: RichTerm, body: RichTerm) -> RichTerm {
    let capture_var = match pat {
        RecordPattern {
            tail: RecordPatternTail::Capture(x),
            ..
        } => *x,
        _ => return body,
    };

    Term::Let(
        capture_var,
        pat.patterns.iter().fold(destr, |acc, field_pat| {
            op2(
                DynRemove(RecordOpKind::default()),
                Term::Str(field_pat.matched_id.ident().into()),
                acc,
            )
        }),
        body,
        Default::default(),
    )
    .into()
}
