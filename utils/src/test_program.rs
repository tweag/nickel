use codespan::Files;

use nickel_lang_core::{
    error::{Error, ParseError},
    eval::cache::CacheImpl,
    parser::{grammar, lexer, ErrorTolerantParser, ExtendedTerm},
    program::Program,
    term::{RichTerm, Term},
};

use std::io::Cursor;

pub type TestProgram = Program<CacheImpl>;

/// Create a program from a Nickel expression provided as a string.
pub fn program_from_expr(s: impl std::string::ToString) -> Program<CacheImpl> {
    Program::<CacheImpl>::new_from_source(Cursor::new(s.to_string()), "test", std::io::stderr())
        .unwrap()
}

pub fn eval(s: impl std::string::ToString) -> Result<Term, Error> {
    program_from_expr(s).eval().map(Term::from)
}

pub fn eval_file(f: &str) -> Result<Term, Error> {
    let mut p: Program<CacheImpl> = program_from_test_fixture(f);
    p.eval().map(Term::from)
}

pub fn parse(s: &str) -> Result<RichTerm, ParseError> {
    let id = Files::new().add("<test>", String::from(s));

    grammar::TermParser::new()
        .parse_strict(id, lexer::Lexer::new(s))
        .map_err(|errs| errs.errors.first().unwrap().clone())
}

pub fn parse_extended(s: &str) -> Result<ExtendedTerm, ParseError> {
    let id = Files::new().add("<test>", String::from(s));

    grammar::ExtendedTermParser::new()
        .parse_strict(id, lexer::Lexer::new(s))
        .map_err(|errs| errs.errors.first().unwrap().clone())
}

pub fn typecheck_fixture(f: &str) -> Result<(), Error> {
    let mut p: Program<CacheImpl> = program_from_test_fixture(f);
    p.typecheck()
}

fn program_from_test_fixture(f: &str) -> Program<CacheImpl> {
    let path = format!("{}/../tests/integration/{}", env!("CARGO_MANIFEST_DIR"), f);
    Program::new_from_file(&path, std::io::stderr())
        .unwrap_or_else(|e| panic!("Could not create program from `{}`\n {}", path, e))
}
