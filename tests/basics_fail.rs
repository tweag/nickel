use assert_matches::assert_matches;
use nickel::error::{Error, EvalError};

use utilities::eval;

#[test]
fn div_by_zero() {
    assert_matches!(
        eval("1 + 1 / (1 - 1)"),
        Err(Error::EvalError(EvalError::Other(..)))
    );
}

#[test]
fn comparisons() {
    assert_matches!(
        eval("1 < 2 < 3"),
        Err(Error::EvalError(EvalError::TypeError(..)))
    );
    assert_matches!(
        eval("1 < 2 > 3"),
        Err(Error::EvalError(EvalError::TypeError(..)))
    );
    assert_matches!(
        eval("\"a\" < 2"),
        Err(Error::EvalError(EvalError::TypeError(..)))
    );
    assert_matches!(
        eval("true <= []"),
        Err(Error::EvalError(EvalError::TypeError(..)))
    );
    assert_matches!(
        eval("\"a\" > \"b\""),
        Err(Error::EvalError(EvalError::TypeError(..)))
    );
    assert_matches!(
        eval("\"a\" >= \"b\""),
        Err(Error::EvalError(EvalError::TypeError(..)))
    );
}

#[test]
fn boolean_ops() {
    assert_matches!(
        eval("let throw | (fun l _v => %blame% l) = null in false || true && throw"),
        Err(Error::EvalError(EvalError::BlameError(..)))
    );
    assert_matches!(
        eval("0 && true"),
        Err(Error::EvalError(EvalError::TypeError(..)))
    );
    assert_matches!(
        eval("\"a\" || false"),
        Err(Error::EvalError(EvalError::TypeError(..)))
    );
}

#[test]
fn string_chunks() {
    assert_matches!(
        eval(r##""bad type %{1 + 1}""##),
        Err(Error::EvalError(EvalError::TypeError(..)))
    );
}
