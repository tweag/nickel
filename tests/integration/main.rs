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
use nickel_lang_utilities::TestProgram;
use serde::{Deserialize, Serialize};
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
    let AnnotatedProgram {
        annotation,
        program,
    } = read_annotated_program(path).expect("Failed to parse annotated program");

    let expectation: Expectation =
        toml::from_str(annotation.as_str()).expect("Failed to parse annotation toml");

    // By default, cargo runs tests with a 2MB stack, which we can overflow in
    // debug mode. To avoid this we run the tests with an increased stack size.
    const STACK_SIZE: usize = 4 * 1024 * 1024;

    let path_for_err = path.to_string();

    thread::Builder::new()
        .name(String::from("TODO: name thread"))
        .stack_size(STACK_SIZE)
        .spawn(move || {
            let mut p =
                TestProgram::new_from_source(Cursor::new(program), "TODO: program name from file")
                    .expect("");
            let result = p.eval();
            match expectation {
                Expectation::Error(expected_err) => {
                    let err = result.expect_err(
                        format!(
                            "Expected error: {}, but program evaluated successfully.",
                            expected_err
                        )
                        .as_str(),
                    );
                    assert_eq!(
                        expected_err, err,
                        "wrong error evaluating file {path_for_err}"
                    )
                }
                Expectation::Pass => assert_eq!(
                    result.map(Term::from),
                    Ok(Term::Bool(true)),
                    "unexpected error evaluating file {path_for_err}",
                ),
                Expectation::Skip => (),
            }
        })
        .expect("Failed to spawn thread")
        .join()
        .expect("Failed to join thread")
}

struct AnnotatedProgram {
    annotation: String,
    program: String,
}

#[derive(Debug)]
enum AnnotatedProgramReadError {
    MissingAnnotation,
}

fn read_annotated_program(path: &str) -> Result<AnnotatedProgram, AnnotatedProgramReadError> {
    let path = {
        let proj_root = env!("CARGO_MANIFEST_DIR");
        PathBuf::from(proj_root).join(path)
    };

    let file = File::open(path).expect("Failed to open file");
    let reader = BufReader::new(file);
    let mut annotation_lines = Vec::new();
    let mut program_lines = Vec::new();

    let mut was_annotation_encountered = false;
    for line in reader.lines() {
        let line = line.expect("Failed to read line");
        if line == "---" {
            was_annotation_encountered = true;
            continue;
        }

        if was_annotation_encountered {
            program_lines.push(line);
        } else {
            annotation_lines.push(line);
        }
    }

    if !was_annotation_encountered {
        std::mem::swap(&mut program_lines, &mut annotation_lines);
    }

    if annotation_lines.is_empty() {
        return Err(AnnotatedProgramReadError::MissingAnnotation);
    }

    let annotation = annotation_lines.join("\n");
    let program = program_lines.join("\n");

    Ok(AnnotatedProgram {
        annotation,
        program,
    })
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "test", content = "metadata")]
enum Expectation {
    #[serde(rename = "error")]
    Error(ErrorExpectation),
    #[serde(rename = "pass")]
    Pass,
    #[serde(rename = "skip")]
    Skip,
}

#[derive(Debug, Serialize, Deserialize)]
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
