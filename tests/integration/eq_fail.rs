use assert_matches::assert_matches;
use nickel_lang::error::{Error, EvalError};
use nickel_lang_utilities::eval;

#[test]
fn functions_cannot_be_compared_for_equality() {
    for (name, src) in [
        (
            "lhs is function",
            r#"
                let f = fun x => x in
                f == 1
            "#,
        ),
        (
            "rhs is function",
            r#"
                let g = fun x => x + 1 in
                "a" == g
            "#,
        ),
        (
            "both sides are functions",
            r#"
                let f = fun x => x in
                f == f
            "#,
        ),
    ] {
        assert_matches!(
            eval(src),
            Err(Error::EvalError(EvalError::EqError { .. })),
            "failed on test: {}",
            name
        )
    }
}
