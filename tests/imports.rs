use assert_matches::assert_matches;
use nickel::error::{Error, EvalError, TypecheckError};
use nickel::program::Program;
use nickel::term::Term;
use std::io::BufReader;
use std::path::PathBuf;

fn mk_import(file: &str) -> String {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(format!("tests/imports/{}", file));
    format!(
        "import \"{}\"",
        path.into_os_string().into_string().unwrap()
    )
}

#[test]
fn nested() {
    let mut prog = Program::new_from_source(
        BufReader::new(mk_import("nested.ncl").as_bytes()),
        "should_be = 3",
    )
    .unwrap();
    assert_eq!(prog.eval().map(Term::from), Ok(Term::Num(3.)));
}

#[test]
fn root_path() {
    let mut prog = Program::new_from_source(
        BufReader::new(mk_import("root_path.ncl").as_bytes()),
        "should_be = 44",
    )
    .unwrap();
    assert_eq!(prog.eval().map(Term::from), Ok(Term::Num(44.)));
}

#[test]
fn multi_imports() {
    let mut prog = Program::new_from_source(
        BufReader::new(mk_import("multi_imports.ncl").as_bytes()),
        "should_be = 5",
    )
    .unwrap();
    assert_eq!(prog.eval().map(Term::from), Ok(Term::Num(5.)));
}

#[test]
fn contract_fail() {
    let mut prog = Program::new_from_source(
        BufReader::new(mk_import("contract-fail.ncl").as_bytes()),
        "should_fail",
    )
    .unwrap();
    assert_matches!(
        prog.eval(),
        Err(Error::EvalError(EvalError::BlameError(..)))
    );
}

#[test]
fn typecheck_fail() {
    let mut prog = Program::new_from_source(
        BufReader::new(mk_import("typecheck-fail.ncl").as_bytes()),
        "should_fail",
    )
    .unwrap();
    assert_matches!(
        prog.eval(),
        Err(Error::TypecheckError(TypecheckError::TypeMismatch(..)))
    );
}

#[test]
fn static_typing_fail() {
    let mut prog = Program::new_from_source(
        BufReader::new(format!("(let x = {} in x) : Str", mk_import("two.ncl")).as_bytes()),
        "should_fail",
    )
    .unwrap();
    assert_matches!(
        prog.eval(),
        Err(Error::TypecheckError(TypecheckError::TypeMismatch(..)))
    );
}

#[test]
fn serialize() {
    use nickel::term::Term;
    let mut prog = Program::new_from_source(
        BufReader::new(
            format!(
                "(builtins.serialize `Json ({})) == (builtins.serialize `Json ({{foo = \"ab\"}}))",
                mk_import("record.ncl")
            )
            .as_bytes(),
        ),
        "shoud success",
    )
    .unwrap();
    assert_eq!(prog.eval().map(Term::from), Ok(Term::Bool(true)));
}

#[test]
fn circular_imports_fail() {
    let mut prog = Program::new_from_source(
        BufReader::new(mk_import("cycle.ncl").as_bytes()),
        "should_fail",
    )
    .unwrap();
    assert_matches!(
        prog.eval().map(Term::from),
        Ok(Term::RecRecord(..)) | Ok(Term::Record(..))
    );
}
