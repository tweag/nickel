use nickel::error::Error;
use nickel::program::Program;
use nickel::term::Term;
use std::io::Cursor;

pub fn eval(s: impl std::string::ToString) -> Result<Term, Error> {
    let mut p = Program::new_from_source(Cursor::new(s.to_string()), "test").unwrap();
    p.eval().map(Term::from)
}

pub fn eval_file(f: &str) -> Result<Term, Error> {
    let mut p =
        Program::new_from_file(format!("{}/tests/{}", env!("CARGO_MANIFEST_DIR"), f)).unwrap();
    p.eval().map(Term::from)
}
