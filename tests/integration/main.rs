use std::{io::Cursor, thread};

use nickel_lang::{
    error::{Error, EvalError, TypecheckError},
    term::Term,
};
use nickel_lang_utilities::{
    annotated_test::{read_annotated_test_case, TestCase},
    test_program::TestProgram,
};
use serde::Deserialize;
use test_generator::test_resources;

mod contract_label_path;
mod free_vars;
mod imports;
mod merge_fail;
mod parse_fail;
mod pretty;
mod query;
mod records_fail;
mod stdlib_arrays_fail;
mod stdlib_typecheck;
mod typecheck_fail;
mod unbound_type_variables;

#[test_resources("./tests/integration/**/*.ncl")]
fn check_annotated_nickel_file(path: &str) {
    let test: TestCase<Test> =
        read_annotated_test_case(path).expect("Failed to parse annotated program");

    // By default, cargo runs tests with a 2MB stack, which we can overflow in
    // debug mode. To avoid this we run the tests with an increased stack size.
    const STACK_SIZE: usize = 4 * 1024 * 1024;
    let path = path.to_string();

    thread::Builder::new()
        .name(path.clone())
        .stack_size(STACK_SIZE)
        .spawn(move || run_test(test, path))
        .expect("Failed to spawn thread")
        .join()
        .expect("Failed to join thread")
}

fn run_test(test_case: TestCase<Test>, path: String) {
    let repeat = test_case.annotation.repeat.unwrap_or(1);
    let program = test_case.program;
    let test = test_case.annotation.test;

    for _ in 0..repeat {
        let mut p =
            TestProgram::new_from_source(Cursor::new(program.clone()), path.as_str()).expect("");
        match test.clone() {
            Expectation::Error(expected_err) => {
                let result = p.eval();
                let err = result.expect_err(
                    format!(
                        "Expected error: {}, but program evaluated successfully.",
                        expected_err
                    )
                    .as_str(),
                );
                assert_eq!(expected_err, err, "wrong error evaluating file {path}")
            }
            Expectation::Pass => {
                let result = p.eval();
                assert_eq!(
                    result.map(Term::from),
                    Ok(Term::Bool(true)),
                    "unexpected error evaluating file {path}",
                )
            }
            Expectation::Skip => (),
        }
    }
}

#[derive(Deserialize)]
struct Test {
    test: Expectation,
    repeat: Option<usize>,
}

#[derive(Clone, Deserialize)]
#[serde(tag = "type", content = "metadata")]
enum Expectation {
    #[serde(rename = "error")]
    Error(ErrorExpectation),
    #[serde(rename = "pass")]
    Pass,
    #[serde(rename = "skip")]
    Skip,
}

#[derive(Clone, Deserialize)]
#[serde(tag = "error", content = "expectation")]
enum ErrorExpectation {
    // TODO: can we somehow unify this with the `Display` impl below?
    #[serde(rename = "EvalError::EqError")]
    EvalEqError,
    #[serde(rename = "EvalError::Other")]
    EvalOther,
    #[serde(rename = "EvalError::NAryPrimopTypeError")]
    EvalNAryPrimopTypeError,
    #[serde(rename = "EvalError::BlameError")]
    EvalBlameError,
    #[serde(rename = "EvalError::IllegalPolymorphicTailAccess")]
    EvalIllegalPolymorphicTailAccess,
    #[serde(rename = "EvalError::TypeError")]
    EvalTypeError,
    #[serde(rename = "EvalError::InfiniteRecursion")]
    EvalInfiniteRecursion,
    #[serde(rename = "EvalError::FieldMissing")]
    EvalFieldMissing { field: String },
    #[serde(rename = "TypecheckError::UnboundIdentifier")]
    TypecheckUnboundIdentifier { identifier: String },
    #[serde(rename = "TypecheckError::TypeMismatch")]
    TypecheckTypeMismatch { expected: String, found: String },
    #[serde(rename = "TypecheckError::MissingRow")]
    TypecheckMissingRow { ident: String },
    #[serde(rename = "TypecheckError::RowMismatch")]
    TypecheckRowMismatch,
}

impl PartialEq<Error> for ErrorExpectation {
    fn eq(&self, other: &Error) -> bool {
        use ErrorExpectation::*;
        match (self, other) {
            (EvalBlameError, Error::EvalError(EvalError::BlameError { .. }))
            | (
                EvalIllegalPolymorphicTailAccess,
                Error::EvalError(EvalError::IllegalPolymorphicTailAccess { .. }),
            )
            | (EvalTypeError, Error::EvalError(EvalError::TypeError(..)))
            | (EvalEqError, Error::EvalError(EvalError::EqError { .. }))
            | (EvalNAryPrimopTypeError, Error::EvalError(EvalError::NAryPrimopTypeError { .. }))
            | (EvalInfiniteRecursion, Error::EvalError(EvalError::InfiniteRecursion(..)))
            | (EvalOther, Error::EvalError(EvalError::Other(..)))
            | (TypecheckRowMismatch, Error::TypecheckError(TypecheckError::RowMismatch(..))) => {
                true
            }
            (EvalFieldMissing { field }, Error::EvalError(EvalError::FieldMissing(ident, ..)))
                if field == ident =>
            {
                true
            }
            (
                TypecheckUnboundIdentifier { identifier },
                Error::TypecheckError(TypecheckError::UnboundIdentifier(ident, ..)),
            ) if ident.label() == identifier => true,
            (
                TypecheckTypeMismatch { expected, found },
                Error::TypecheckError(TypecheckError::TypeMismatch(expected1, found1, ..)),
            ) if expected == &expected1.to_string() && found == &found1.to_string() => true,
            (
                TypecheckMissingRow { ident },
                Error::TypecheckError(TypecheckError::MissingRow(row, ..)),
            ) if ident == row.label() => true,
            (_, _) => false,
        }
    }
}

impl std::fmt::Display for ErrorExpectation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ErrorExpectation::*;
        let name = match self {
            EvalBlameError => "EvalError::BlameError".to_owned(),
            EvalTypeError => "EvalError::TypeError".to_owned(),
            EvalEqError => "EvalError::EqError".to_owned(),
            EvalOther => "EvalError::Other".to_owned(),
            EvalNAryPrimopTypeError => "EvalError::NAryPrimopTypeError".to_owned(),
            EvalInfiniteRecursion => "EvalError::InfiniteRecursion".to_owned(),
            EvalIllegalPolymorphicTailAccess => {
                "EvalError::IllegalPolymorphicTailAccess".to_owned()
            }
            EvalFieldMissing { field } => {
                format!("EvalError::FieldMissing({field})")
            }
            TypecheckUnboundIdentifier { identifier } => {
                format!("TypecheckError::UnboundIdentifier({identifier})")
            }
            TypecheckTypeMismatch { expected, found } => {
                format!("TypecheckError::TypeMismatch({expected}, {found})")
            }
            TypecheckMissingRow { ident } => {
                format!("TypecheckError::MissingRow({ident})")
            }
            TypecheckRowMismatch => "TypecheckError::RowMismatch".to_owned(),
        };
        write!(f, "{}", name)
    }
}

impl std::fmt::Debug for ErrorExpectation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
