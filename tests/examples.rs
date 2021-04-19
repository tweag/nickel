use assert_matches::assert_matches;
use nickel::error::{Error, EvalError};
use nickel::program::Program;
use nickel::term::Term;
use std::path::PathBuf;

use std::sync::Once;

static INIT: Once = Once::new();

/// CD into the examples directory: otherwise, imports are not resolved as they should.
fn set_env() {
    INIT.call_once(|| {
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path.push("examples");
        std::env::set_current_dir(path).unwrap();
    });
}

fn eval_file(file: &str) -> Result<Term, Error> {
    set_env();
    let mut p =
        Program::new_from_file(PathBuf::from(file)).expect("could not load file as a program");
    p.eval()
}

#[test]
fn config_gcc() {
    eval_file("config-gcc.ncl").unwrap();
}

#[test]
fn fibonacci() {
    eval_file("fibonacci.ncl").unwrap();
}

#[test]
fn lists() {
    eval_file("lists.ncl").unwrap();
}

#[test]
fn merge_main() {
    eval_file("merge-main.ncl").unwrap();
}

#[test]
fn polymorphism() {
    eval_file("polymorphism.ncl").unwrap();
}

#[test]
fn record_contract() {
    eval_file("record-contract.ncl").unwrap();
}

#[test]
fn simple_contract_bool() {
    eval_file("simple-contract-bool.ncl").unwrap();
}

/// This example is expected to fail.
#[test]
fn simple_contract_div() {
    assert_matches!(
        eval_file("simple-contract-div.ncl"),
        Err(Error::EvalError(EvalError::BlameError(..)))
    );
}
