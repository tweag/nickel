use std::ffi::OsString;
use std::path::PathBuf;

use assert_matches::assert_matches;
use nickel_lang::error::{Error, EvalError, TypecheckError};
use nickel_lang::term::Term;

use nickel_lang_utilities::test_program::{eval_file, TestProgram};
use test_generator::test_resources;

#[test_resources("tests/integration/destructuring/pass/*.ncl")]
fn pass(file: &str) {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(file);

    let path: OsString = path.into();

    let mut p = TestProgram::new_from_file(path.clone()).expect("could not load file as program");
    assert_eq!(
        p.eval().map(Term::from),
        Ok(Term::Bool(true)),
        "error evaluating {}",
        path.to_string_lossy()
    )
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
        Err(Error::EvalError(EvalError::BlameError { .. }))
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
        Err(Error::EvalError(EvalError::BlameError { .. }))
    );
}

#[test]
fn type_mismatch_fail() {
    assert_matches!(
        eval_file("destructuring/type_mismatch_fail.ncl"),
        Err(Error::TypecheckError(TypecheckError::TypeMismatch(..)))
    )
}

#[test]
fn type_mismatch_field_pattern_fail() {
    assert_matches!(
        eval_file("destructuring/type_mismatch_field_pattern_fail.ncl"),
        Err(Error::TypecheckError(TypecheckError::TypeMismatch(..)))
    )
}

#[test]
fn type_mismatch_nested_destructuring_fail() {
    assert_matches!(
        eval_file("destructuring/type_mismatch_nested_destructuring_fail.ncl"),
        Err(Error::TypecheckError(TypecheckError::TypeMismatch(..)))
    )
}

#[test]
fn repeated_ident() {
    assert_matches!(
        eval_file("destructuring/repeated_ident.ncl"),
        // Note: currently a repeated identifier in a pattern isn't caught
        //       unless we're explicitly typechecking. There's an open issue
        //       (#1098) to handle this when we compile the pattern at which
        //       point this error will likely change.
        Err(Error::TypecheckError(TypecheckError::MissingRow(..)))
    )
}

#[test]
fn nonexistent_idents() {
    assert_matches!(
        eval_file("destructuring/nonexistent_idents.ncl"),
        Err(Error::TypecheckError(TypecheckError::MissingRow(..)))
    )
}
