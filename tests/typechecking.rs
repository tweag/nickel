use nickel::program::Program;
use nickel::term::Term;
use std::ffi::OsString;
use std::path::PathBuf;
use std::thread;

// The following code (run and check_file) is duplicated (common with `pass.rs`, maybe others).
// Once the benchmark PR lands, it will lie in a separate utilities crate. For now, we'll live with
// this duplication.

// By default, tests are run with 2MB stack size, which can overflow in debug mode. We run the
// tests with an increased stack size.
const STACK_SIZE: usize = 4 * 1024 * 1024;

fn run(path: impl Into<OsString>) {
    let path = path.into();
    let mut p = Program::new_from_file(path.clone()).expect("could not load file as a program");
    assert_eq!(
        p.eval(),
        Ok(Term::Bool(true)),
        "error evaluating {}",
        path.to_string_lossy(),
    );
}

fn check_file(file: &str) {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(format!("tests/typechecking/{}", file));

    thread::Builder::new()
        .name(String::from(file))
        .stack_size(STACK_SIZE)
        .spawn(|| run(path))
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn basics() {
    check_file("basics.ncl");
}

#[test]
fn subtyping() {
    check_file("subtyping.ncl");
}
