use nickel::error::{Error, EvalError, TypecheckError};
use nickel::program::Program;
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
    use nickel::term::Term;
    let mut prog = Program::new_from_source(
        BufReader::new(mk_import("nested.ncl").as_bytes()),
        "should_be = 3",
    )
    .unwrap();
    assert_eq!(prog.eval(), Ok(Term::Num(3.)));
}

#[test]
fn multi_imports() {
    use nickel::term::Term;
    let mut prog = Program::new_from_source(
        BufReader::new(mk_import("multi_imports.ncl").as_bytes()),
        "should_be = 5",
    )
    .unwrap();
    assert_eq!(prog.eval(), Ok(Term::Num(5.)));
}

#[test]
fn contract_fail() {
    let mut prog = Program::new_from_source(
        BufReader::new(mk_import("contract-fail.ncl").as_bytes()),
        "should_fail",
    )
    .unwrap();
    prog.eval()
        .map_err(|e| match e {
            Error::EvalError(EvalError::BlameError(_, _)) => Ok(()),
            r => Err(r),
        })
        .unwrap_err()
        .unwrap();
}

#[test]
fn typecheck_fail() {
    let mut prog = Program::new_from_source(
        BufReader::new(mk_import("typecheck-fail.ncl").as_bytes()),
        "should_fail",
    )
    .unwrap();
    prog.eval()
        .map_err(|e| match e {
            Error::TypecheckError(TypecheckError::TypeMismatch(..)) => Ok(()),
            r => Err(r),
        })
        .unwrap_err()
        .unwrap();
}

#[test]
fn static_typing_fail() {
    let mut prog = Program::new_from_source(
        BufReader::new(format!("(let x = {} in x) : Str", mk_import("two.ncl")).as_bytes()),
        "should_fail",
    )
    .unwrap();
    prog.eval()
        .map_err(|e| match e {
            Error::TypecheckError(TypecheckError::TypeMismatch(..)) => Ok(()),
            r => Err(r),
        })
        .unwrap_err()
        .unwrap();
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
    assert_eq!(prog.eval(), Ok(Term::Bool(true)));
}

// TODO produce a stack overflow
//#[test]
//fn circular_imports_fail() {
//    let mut prog = Program::new_from_source(
//        BufReader::new(mk_import("cycle.ncl").as_bytes()),
//        "should_fail",
//    )
//    .unwrap();
//    prog.eval()
//        .map_err(|e| match e {
//            Error::TypecheckError(TypecheckError::TypeMismatch(..)) => Ok(()),
//            r => Err(r),
//        })
//        .unwrap_err()
//        .unwrap();
//}
