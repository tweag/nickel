use assert_matches::assert_matches;
use nickel_lang::error::{Error, EvalError};
use nickel_lang_utilities::TestProgram;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::PathBuf;
use test_generator::test_resources;

#[test_resources("examples/**/*.ncl")]
fn eval_file(file: &str) {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(file);

    let annotation = extract_annotation(path.clone());

    match annotation {
        ExampleAnnotation::Ignore => (),
        ExampleAnnotation::Blame => {
            let mut p = TestProgram::new_from_file(path).expect("could not load file as a program");
            assert_matches!(
                p.eval_deep(),
                Err(Error::EvalError(EvalError::BlameError { .. }))
            );
        }
        ExampleAnnotation::Pass => {
            let mut p = TestProgram::new_from_file(path).expect("could not load file as a program");
            p.eval_deep().unwrap();
        }
    }
}

enum ExampleAnnotation {
    Pass,
    Blame,
    Ignore,
}

fn extract_annotation(path: PathBuf) -> ExampleAnnotation {
    let file = File::open(path).unwrap();
    let reader = io::BufReader::new(file);
    let first_line = reader
        .lines()
        .next()
        .expect("Found empty example file")
        .expect("Error reading example file");

    match first_line.strip_prefix("# test: ") {
        Some("pass") => ExampleAnnotation::Pass,
        Some("blame") => ExampleAnnotation::Blame,
        Some("ignore") => ExampleAnnotation::Ignore,
        _ => panic!("Invalid example file annotation. Example files must begin with `# test: (pass|blame|ignore)`")
    }
}
