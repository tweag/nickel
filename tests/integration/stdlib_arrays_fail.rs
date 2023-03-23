use assert_matches::assert_matches;
use nickel_lang::error::{Error, EvalError};

use nickel_lang_utilities::eval;

#[test]
fn elem_at() {
    assert_matches!(
        eval("%elem_at% [] 0"),
        Err(Error::EvalError(EvalError::Other(..)))
    );
    assert_matches!(
        eval("%elem_at% [1,2,3] (-1)"),
        Err(Error::EvalError(EvalError::Other(..)))
    );
    assert_matches!(
        eval("%elem_at% [true, false, true] 3"),
        Err(Error::EvalError(EvalError::Other(..)))
    );
    assert_matches!(
        eval("%elem_at% {} 0"),
        Err(Error::EvalError(EvalError::TypeError(..)))
    );
    assert_matches!(
        eval("%elem_at% [1, 2, 3] 0.5"),
        Err(Error::EvalError(EvalError::Other(..)))
    );

    assert_matches!(
        eval("array.at 0 {} 0"),
        Err(Error::EvalError(EvalError::BlameError { .. }))
    );
    assert_matches!(
        eval("array.at \"a\" []"),
        Err(Error::EvalError(EvalError::BlameError { .. }))
    );
}

#[test]
fn head_tail() {
    assert_matches!(
        eval("%elem_at% [] 0"),
        Err(Error::EvalError(EvalError::Other(..)))
    );
    assert_matches!(
        eval("%array_slice% 1 1 []"),
        Err(Error::EvalError(EvalError::Other(..)))
    );
    assert_matches!(
        eval("%elem_at% {} 0"),
        Err(Error::EvalError(EvalError::TypeError(..)))
    );
    assert_matches!(
        eval("%array_slice% 0 1 {}"),
        Err(Error::EvalError(EvalError::TypeError(..)))
    );

    // TODO: add non-empty contract to the input of array.first and array.tail
    // assert_matches!(
    //     eval("array.first []"),
    //     Err(Error::EvalError(EvalError::BlameError {..}))
    // );
    // assert_matches!(
    //     eval("array.tail []"),
    //     Err(Error::EvalError(EvalError::BlameError {..}))
    // );
    assert_matches!(
        eval("array.first false"),
        Err(Error::EvalError(EvalError::BlameError { .. }))
    );
    assert_matches!(
        eval("array.drop_first 2"),
        Err(Error::EvalError(EvalError::BlameError { .. }))
    );
}
