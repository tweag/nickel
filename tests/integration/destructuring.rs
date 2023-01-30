use std::ffi::OsString;
use std::path::PathBuf;

use assert_matches::assert_matches;
use nickel_lang::error::{Error, EvalError, TypecheckError};
use nickel_lang::term::Term;

use nickel_lang_utilities::{eval_file, TestProgram};
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
        // Note: ideally this would be a typechecker error, but currently
        //       annotated types in destructuring patterns are just converted
        //       into contracts. This should be revisted once the metadata
        //       rework from RFC005 is implemented
        Err(Error::EvalError(EvalError::BlameError { .. }))
    )
}
