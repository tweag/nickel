use assert_matches::assert_matches;
use nickel::error::{Error, EvalError, TypecheckError};

mod common;
use common::eval;

#[test]
fn records_access() {
    assert_matches!(
        eval("{foo = true}.bar"),
        Err(Error::EvalError(EvalError::FieldMissing(..)))
    );
    assert_matches!(
        eval("({ \"#{(if false then \"foo\" else \"bar\")}\" = false, bar = true, }).foo"),
        Err(Error::EvalError(EvalError::Other(msg, ..))) if msg.starts_with("$[ .. ]"));
}

#[test]
fn non_mergeable() {
    assert_matches!(
        eval("({a=1} & {a=2}).a"),
        Err(Error::EvalError(EvalError::MergeIncompatibleArgs(..)))
    );
    assert_matches!(
        eval("({a | default = false} & {a | default = true}).a"),
        Err(Error::EvalError(EvalError::MergeIncompatibleArgs(..)))
    );
}

#[test]
fn non_mergeable_piecewise() {
    assert_matches!(
        eval("({a.b=1, a = {b = 2}}).a.b"),
        Err(Error::EvalError(EvalError::MergeIncompatibleArgs(..)))
    );
    assert_matches!(
        eval("({foo.bar | default = false, foo.bar | default = true}).foo.bar"),
        Err(Error::EvalError(EvalError::MergeIncompatibleArgs(..)))
    );
    assert_matches!(
        eval("({a.b = {}} & {a.b.c = []} & {a.b.c = {}}).a.b.c"),
        Err(Error::EvalError(EvalError::MergeIncompatibleArgs(..)))
    );
}

#[test]
fn dynamic_not_recursive() {
    assert_matches!(
        eval("let x = \"foo\" in {\"#{x}\" = 1, bar = foo}.bar"),
        Err(Error::TypecheckError(TypecheckError::UnboundIdentifier(..)))
    );
}
