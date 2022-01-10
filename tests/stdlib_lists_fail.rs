use assert_matches::assert_matches;
use nickel::error::{Error, EvalError};

use utilities::eval;

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
        eval("lists.elem_at 0 {} 0"),
        Err(Error::EvalError(EvalError::BlameError(..)))
    );
    assert_matches!(
        eval("lists.elem_at \"a\" []"),
        Err(Error::EvalError(EvalError::BlameError(..)))
    );
}

#[test]
fn head_tail() {
    assert_matches!(
        eval("%head% []"),
        Err(Error::EvalError(EvalError::Other(..)))
    );
    assert_matches!(
        eval("%tail% []"),
        Err(Error::EvalError(EvalError::Other(..)))
    );
    assert_matches!(
        eval("%head% {}"),
        Err(Error::EvalError(EvalError::TypeError(..)))
    );
    assert_matches!(
        eval("%tail% {}"),
        Err(Error::EvalError(EvalError::TypeError(..)))
    );

    // TODO: add non-empty contract to the input of lists.head and lists.tail
    // assert_matches!(
    //     eval("lists.head []"),
    //     Err(Error::EvalError(EvalError::BlameError(..)))
    // );
    // assert_matches!(
    //     eval("lists.tail []"),
    //     Err(Error::EvalError(EvalError::BlameError(..)))
    // );
    assert_matches!(
        eval("lists.head false"),
        Err(Error::EvalError(EvalError::BlameError(..)))
    );
    assert_matches!(
        eval("lists.tail 2"),
        Err(Error::EvalError(EvalError::BlameError(..)))
    );
}
