use nickel::error::Error;
use nickel::program::Program;
use nickel::term::Term;
use std::io::Cursor;

pub fn eval(s: impl std::string::ToString) -> Result<Term, Error> {
    let mut p = Program::new_from_source(Cursor::new(s.to_string()), "test").unwrap();
    p.eval()
}
