use assert_matches::assert_matches;
use nickel_lang::error::{Error, EvalError, ImportError, TypecheckError};
use nickel_lang::term::{make as mk_term, RichTerm, Term};
use nickel_lang_utilities::test_program::TestProgram;
use std::io::BufReader;
use std::path::PathBuf;

fn mk_import(file: &str) -> String {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(format!("tests/integration/imports/{file}"));
    format!(
        "import \"{}\"",
        path.into_os_string().into_string().unwrap()
    )
}

#[test]
fn nested() {
    let mut prog = TestProgram::new_from_source(
        BufReader::new(mk_import("nested.ncl").as_bytes()),
        "should_be = 3",
    )
    .unwrap();
    assert_eq!(
        prog.eval().map(RichTerm::without_pos),
        Ok(mk_term::integer(3))
    );
}

#[test]
fn root_path() {
    let mut prog = TestProgram::new_from_source(
        BufReader::new(mk_import("root_path.ncl").as_bytes()),
        "should_be = 44",
    )
    .unwrap();
    assert_eq!(
        prog.eval().map(RichTerm::without_pos),
        Ok(mk_term::integer(44))
    );
}

#[test]
fn multi_imports() {
    let mut prog = TestProgram::new_from_source(
        BufReader::new(mk_import("multi_imports.ncl").as_bytes()),
        "should_be = 5",
    )
    .unwrap();
    assert_eq!(
        prog.eval().map(RichTerm::without_pos),
        Ok(mk_term::integer(5))
    );
}

#[test]
fn contract_fail() {
    let mut prog = TestProgram::new_from_source(
        BufReader::new(mk_import("contract-fail.ncl").as_bytes()),
        "should_fail",
    )
    .unwrap();
    assert_matches!(
        prog.eval(),
        Err(Error::EvalError(EvalError::BlameError { .. }))
    );
}

#[test]
fn typecheck_fail() {
    let mut prog = TestProgram::new_from_source(
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
    let mut prog = TestProgram::new_from_source(
        BufReader::new(format!("(let x = {} in x) : String", mk_import("two.ncl")).as_bytes()),
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
    use nickel_lang::term::Term;
    let mut prog = TestProgram::new_from_source(
        BufReader::new(
            format!(
                "(std.serialize 'Json ({})) == (std.serialize 'Json ({{foo = \"ab\"}}))",
                mk_import("record.ncl")
            )
            .as_bytes(),
        ),
        "should_succeed",
    )
    .unwrap();
    assert_eq!(prog.eval().map(Term::from), Ok(Term::Bool(true)));
}

#[test]
fn circular_imports_fail() {
    let mut prog = TestProgram::new_from_source(
        BufReader::new(mk_import("cycle.ncl").as_bytes()),
        "should_succeed",
    )
    .unwrap();
    assert_matches!(
        prog.eval().map(Term::from),
        Ok(Term::RecRecord(..)) | Ok(Term::Record(..))
    );
}

#[test]
fn import_unexpected_token_fail() {
    let mut prog = TestProgram::new_from_source(
        BufReader::new(mk_import("unexpected_token.ncl").as_bytes()),
        "should_fail",
    )
    .unwrap();
    assert_matches!(
        prog.eval(),
        Err(Error::ImportError(ImportError::ParseErrors(..)))
    );
}

#[test]
fn import_unexpected_token_in_record_fail() {
    let mut prog = TestProgram::new_from_source(
        BufReader::new(
            format!(
                "let x = {} in \"Hello, \" ++ x.name",
                mk_import("unexpected_token_in_record.ncl")
            )
            .as_bytes(),
        ),
        "should_fail",
    )
    .unwrap();
    assert_matches!(
        prog.eval(),
        Err(Error::ImportError(ImportError::ParseErrors(..)))
    );
}

#[test]
fn import_unexpected_token_buried_fail() {
    let mut prog = TestProgram::new_from_source(
        BufReader::new(
            format!(
                "let _ign = {} in 1",
                mk_import("unexpected_token_buried.ncl")
            )
            .as_bytes(),
        ),
        "should_fail",
    )
    .unwrap();
    assert_matches!(
        prog.eval(),
        Err(Error::ImportError(ImportError::ParseErrors(..)))
    );
}

// Regression test for #1090 (https://github.com/tweag/nickel/issues/1090)
#[test]
fn nested_syntax_error() {
    let mut prog = TestProgram::new_from_source(
        BufReader::new(mk_import("nested_syntax_error.ncl").as_bytes()),
        "should_fail",
    )
    .unwrap();

    assert_matches!(
        prog.eval(),
        Err(Error::ImportError(ImportError::ParseErrors(..)))
    );
}

// Regression test for simple import loop making the typechecker overflow
#[test]
fn direct_import_loop() {
    let mut prog = TestProgram::new_from_source(
        BufReader::new(mk_import("direct_import_loop.ncl").as_bytes()),
        "should_typecheck",
    )
    .unwrap();

    assert_matches!(prog.typecheck(), Ok(()));
}
