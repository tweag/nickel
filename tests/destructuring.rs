use assert_matches::assert_matches;
use duplicate::duplicate;
use nickel::error::{Error, EvalError, TypecheckError};
use nickel::program::Program;
use nickel::term::Term;
use std::io::BufReader;
use std::path::PathBuf;

#[duplicate(
    name;
    [simple];
    [assign];
    [atbind];
    [open];
    [nested];
    [rest];
    [default];
    [typecontract];
    [mixed];
    )]
#[test]
fn name() {
    let mut prog = Program::new_from_file(format!(
        "{}/tests/destructuring/{}.ncl",
        env!("CARGO_MANIFEST_DIR"),
        stringify!(name)
    ))
    .unwrap();
    assert_eq!(prog.eval(), Ok(Term::Bool(true)));
}
