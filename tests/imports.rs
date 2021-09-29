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

// produce a stack overflow
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
