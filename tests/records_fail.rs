use assert_matches::assert_matches;
use nickel::error::{Error, EvalError};

mod common;
use common::eval;

#[test]
fn records_access() {
    assert_matches!(
        eval("{foo = true}.bar"),
        Err(Error::EvalError(EvalError::FieldMissing(..)))
    );
    assert_matches!(
        eval("({ \"#{(if false then \"foo\" else \"bar\")}\" = false; bar = true; }).foo"),
        Err(Error::EvalError(EvalError::Other(msg, ..))) if msg.starts_with("$[ .. ]"));
}

#[test]
fn non_mergeable() {
    assert_matches!(
        eval("({a=1} & {a=2}).a"),
        Err(Error::EvalError(EvalError::MergeIncompatibleArgs(..)))
    );
    assert_matches!(
        eval("({a | default = false;} & {a | default = true}).a"),
        Err(Error::EvalError(EvalError::MergeIncompatibleArgs(..)))
    );
    assert_matches!(
        eval("({a.b = {}} & {a.b.c = []} & {a.b.c = {}}).a.b.c"),
        Err(Error::EvalError(EvalError::MergeIncompatibleArgs(..)))
    );
}
