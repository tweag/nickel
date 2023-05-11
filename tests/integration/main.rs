use std::{
    fs::File,
    io::{BufRead, BufReader, Cursor},
    path::PathBuf,
    thread,
};

use nickel_lang::{
    error::{Error, EvalError},
    term::Term,
};
use nickel_lang_utilities::test_program::TestProgram;
use serde::Deserialize;
use test_generator::test_resources;

mod basics_fail;
mod contracts_fail;
mod destructuring;
mod examples;
mod free_vars;
mod imports;
mod infinite_rec;
mod merge_fail;
mod parse_fail;
mod pass;
mod pretty;
mod query;
mod records_fail;
mod stdlib_arrays_fail;
mod stdlib_typecheck;
mod typecheck_fail;
mod unbound_type_variables;

#[test_resources("./tests/integration/fail/*.ncl")]
fn check_annotated_nickel_file(path: &str) {
    let test = read_test_case(path).expect("Failed to parse annotated program");

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

fn run_test(test: TestCase, path: String) {
    let mut p = TestProgram::new_from_source(Cursor::new(test.program), path.as_str()).expect("");
    let result = p.eval();
    match test.expectation {
        Expectation::Error(expected_err) => {
            let err = result.expect_err(
                format!(
                    "Expected error: {}, but program evaluated successfully.",
                    expected_err
                )
                .as_str(),
            );
            assert_eq!(expected_err, err, "wrong error evaluating file {path}")
        }
        Expectation::Pass => assert_eq!(
            result.map(Term::from),
            Ok(Term::Bool(true)),
            "unexpected error evaluating file {path}",
        ),
        Expectation::Skip => (),
    }
}

struct TestCase {
    expectation: Expectation,
    program: String,
}

#[derive(Debug)]
enum AnnotatedProgramReadError {
    MissingAnnotation,
}

fn read_test_case(path: &str) -> Result<TestCase, AnnotatedProgramReadError> {
    let path = {
        let proj_root = env!("CARGO_MANIFEST_DIR");
        PathBuf::from(proj_root).join(path)
    };

    let file = File::open(path).expect("Failed to open file");
    let reader = BufReader::new(file);

    let mut lines = reader.lines();

    let mut expectation = String::new();
    let mut program = String::new();

    loop {
        let line = lines
            .next()
            .expect("Unexpected end of test file")
            .expect("Error reading line");
        if line.starts_with('#') {
            let annot_line = if line.len() > 1 { &line[2..] } else { "" };
            expectation.push_str(annot_line);
            expectation.push('\n');
        } else {
            // we've already consumed the line in order to check the first char
            // so we need to add it to the program string.
            program.push_str(&line);
            program.push('\n');
            break;
        }
    }

    if expectation.is_empty() {
        return Err(AnnotatedProgramReadError::MissingAnnotation);
    }

    for line in lines {
        let line = line.expect("Error reading line");
        program.push_str(&line);
        program.push('\n');
    }

    let expectation: Expectation =
        toml::from_str(expectation.as_str()).expect("Failed to parse expectation toml");

    Ok(TestCase {
        expectation,
        program,
    })
}

#[derive(Deserialize)]
#[serde(tag = "test", content = "metadata")]
enum Expectation {
    #[serde(rename = "error")]
    Error(ErrorExpectation),
    #[serde(rename = "pass")]
    Pass,
    #[serde(rename = "skip")]
    Skip,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "error", content = "expectation")]
enum ErrorExpectation {
    // TODO: can we somehow unify this with the `Display` impl below?
    #[serde(rename = "EvalError::EqError")]
    EvalEqError,
}

impl PartialEq<Error> for ErrorExpectation {
    fn eq(&self, other: &Error) -> bool {
        match (self, other) {
            (ErrorExpectation::EvalEqError, Error::EvalError(EvalError::EqError { .. })) => true,
            (_, _) => false,
        }
    }
}

impl std::fmt::Display for ErrorExpectation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            ErrorExpectation::EvalEqError => "EvalError::EqError",
        };
        write!(f, "{}", name)
    }
}
