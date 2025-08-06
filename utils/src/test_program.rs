use nickel_lang_core::{
    bytecode::ast::{Ast, AstAlloc},
    error::{Error, NullReporter, ParseError},
    eval::cache::CacheImpl,
    files::Files,
    parser::{grammar, lexer, ErrorTolerantParser as _, ErrorTolerantParserCompat, ExtendedTerm},
    program::Program,
    term::{RichTerm, Term},
    typecheck::TypecheckMode,
};

use std::io::Cursor;

pub type TestProgram = Program<CacheImpl>;

/// Create a program from a Nickel expression provided as a string.
pub fn program_from_expr(s: impl std::string::ToString) -> TestProgram {
    TestProgram::new_from_source(
        Cursor::new(s.to_string()),
        "test",
        std::io::stderr(),
        NullReporter {},
    )
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
        .parse_strict_compat(id, lexer::Lexer::new(s))
        .map_err(|errs| errs.errors.first().unwrap().clone())
}

pub fn parse_bytecode_ast<'a>(alloc: &'a AstAlloc, s: &str) -> Result<Ast<'a>, ParseError> {
    let id = Files::new().add("<test>", String::from(s));

    grammar::TermParser::new()
        .parse_strict(alloc, id, lexer::Lexer::new(s))
        .map_err(|errs| errs.errors.first().unwrap().clone())
}

pub fn parse_extended(s: &str) -> Result<ExtendedTerm<RichTerm>, ParseError> {
    let id = Files::new().add("<test>", String::from(s));

    grammar::ExtendedTermParser::new()
        .parse_strict_compat(id, lexer::Lexer::new(s))
        .map_err(|errs| errs.errors.first().unwrap().clone())
}

pub fn typecheck_fixture(f: &str) -> Result<(), Error> {
    let mut p: Program<CacheImpl> = program_from_test_fixture(f);
    p.typecheck(TypecheckMode::Walk)
}

fn program_from_test_fixture(f: &str) -> Program<CacheImpl> {
    let path = format!("{}/../tests/integration/{}", env!("CARGO_MANIFEST_DIR"), f);
    Program::new_from_file(&path, std::io::stderr(), NullReporter {})
        .unwrap_or_else(|e| panic!("Could not create program from `{path}`\n {e}"))
}
