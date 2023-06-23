use assert_matches::assert_matches;
use nickel_lang_core::error::{Error, EvalError};

use nickel_lang_utils::test_program::eval;

#[test]
fn infinite_loops() {
    assert_matches!(
        eval("{x = x}.x"),
        Err(Error::EvalError(EvalError::InfiniteRecursion(..)))
    );
    assert_matches!(
        eval("{x = y, y = z, z = x }.x"),
        Err(Error::EvalError(EvalError::InfiniteRecursion(..)))
    );
    assert_matches!(
        eval("{x = y + z, y = z + x, z = 1}.x"),
        Err(Error::EvalError(EvalError::InfiniteRecursion(..)))
    );
    assert_matches!(
        eval("{x = (fun a => a + y) 0, y = (fun a => a + x) 0}.x"),
        Err(Error::EvalError(EvalError::InfiniteRecursion(..)))
    );
}
