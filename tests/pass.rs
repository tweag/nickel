use nickel::program::Program;
use nickel::term::Term;
use std::ffi::OsString;
use std::path::PathBuf;
use std::thread;

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
    path.push(format!("tests/pass/{}", file));

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
fn builtins() {
    check_file("builtins.ncl");
}

#[test]
fn complete() {
    check_file("complete.ncl");
}

#[test]
fn contracts() {
    check_file("contracts.ncl");
}

#[test]
fn eq() {
    check_file("eq.ncl")
}

#[test]
fn functions() {
    check_file("functions.ncl");
}

#[test]
fn lists() {
    check_file("lists.ncl");
}

#[test]
fn metavalues() {
    check_file("metavalues.ncl");
}

#[test]
fn records() {
    check_file("records.ncl");
}

#[test]
fn record_defs() {
    check_file("record-defs.ncl");
}

#[test]
fn strings() {
    check_file("strings.ncl");
}

#[test]
fn typechecking() {
    check_file("typechecking.ncl");
}

#[test]
fn types() {
    check_file("types.ncl");
}

#[test]
fn serialize() {
    check_file("serialize.ncl");
}
