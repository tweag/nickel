use nickel_lang::term::Term;
use nickel_lang_utilities::TestProgram;
use std::ffi::OsString;
use std::path::PathBuf;
use std::thread;
use test_generator::test_resources;

// By default, tests are run with 2MB stack size, which can overflow in debug mode. We run the
// tests with an increased stack size.
const STACK_SIZE: usize = 4 * 1024 * 1024;

fn run(path: impl Into<OsString>) {
    let path = path.into();
    let mut p = TestProgram::new_from_file(path.clone()).expect("could not load file as a program");
    assert_eq!(
        p.eval().map(Term::from),
        Ok(Term::Bool(true)),
        "error evaluating {}",
        path.to_string_lossy(),
    );
}

#[test_resources("tests/integration/pass/*.ncl")]
fn check_file(file: &str) {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(file);

    thread::Builder::new()
        .name(String::from(file))
        .stack_size(STACK_SIZE)
        .spawn(|| run(path))
        .unwrap()
        .join()
        .unwrap();
}
