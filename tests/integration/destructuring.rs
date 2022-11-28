use assert_matches::assert_matches;
use nickel_lang::error::{Error, EvalError, TypecheckError};
use nickel_lang::term::Term;

use nickel_lang_utilities::eval_file;

#[test]
fn simple() {
    assert_eq!(eval_file("destructuring/simple.ncl"), Ok(Term::Bool(true)));
}

#[test]
fn assign() {
    assert_eq!(eval_file("destructuring/assign.ncl"), Ok(Term::Bool(true)));
}

#[test]
fn atbind() {
    assert_eq!(eval_file("destructuring/atbind.ncl"), Ok(Term::Bool(true)));
}

#[test]
fn open() {
    assert_eq!(eval_file("destructuring/open.ncl"), Ok(Term::Bool(true)));
}

#[test]
fn nested() {
    assert_eq!(eval_file("destructuring/nested.ncl"), Ok(Term::Bool(true)));
}

#[test]
fn rest() {
    assert_eq!(eval_file("destructuring/rest.ncl"), Ok(Term::Bool(true)));
}

#[test]
fn default() {
    assert_eq!(eval_file("destructuring/default.ncl"), Ok(Term::Bool(true)));
}

#[test]
fn typecontract() {
    assert_eq!(
        eval_file("destructuring/typecontract.ncl"),
        Ok(Term::Bool(true))
    );
}

#[test]
fn mixed() {
    assert_eq!(eval_file("destructuring/mixed.ncl"), Ok(Term::Bool(true)));
}

#[test]
fn assign_fail() {
    assert_matches!(
        eval_file("destructuring/assign_fail.ncl"),
        Err(Error::TypecheckError(TypecheckError::UnboundIdentifier(..)))
    );
}

#[test]
fn closed_fail() {
    assert_matches!(
        eval_file("destructuring/closed_fail.ncl"),
        Err(Error::EvalError(EvalError::BlameError(..)))
    );
}

#[test]
fn rest_fail() {
    assert_matches!(
        eval_file("destructuring/rest_fail.ncl"),
        Err(Error::EvalError(EvalError::FieldMissing(..)))
    );
}

#[test]
fn typecontract_fail() {
    assert_matches!(
        eval_file("destructuring/typecontract_fail.ncl"),
        Err(Error::EvalError(EvalError::BlameError(..)))
    );
}

#[test]
fn fun() {
    assert_eq!(eval_file("destructuring/fun.ncl"), Ok(Term::Bool(true)));
}
