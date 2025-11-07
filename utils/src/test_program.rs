use nickel_lang_core::{
    ast::{Ast, AstAlloc},
    error::{Error, NullReporter, ParseError},
    eval::{cache::CacheImpl, value::NickelValue},
    files::Files,
    parser::{ErrorTolerantParser as _, ErrorTolerantParserCompat, ExtendedTerm, grammar, lexer},
    position::PosTable,
    program::Program,
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

pub fn eval(s: impl std::string::ToString) -> Result<NickelValue, Error> {
    program_from_expr(s).eval()
}

pub fn eval_file(f: &str) -> Result<NickelValue, Error> {
    let mut p: Program<CacheImpl> = program_from_test_fixture(f);
    p.eval()
}

pub fn parse(pos_table: &mut PosTable, s: &str) -> Result<NickelValue, ParseError> {
    let id = Files::empty().add("<test>", String::from(s));

    grammar::TermParser::new()
        .parse_strict_compat(pos_table, id, lexer::Lexer::new(s))
        .map_err(|errs| errs.errors.first().unwrap().clone())
}

pub fn parse_bytecode_ast<'a>(alloc: &'a AstAlloc, s: &str) -> Result<Ast<'a>, ParseError> {
    let id = Files::empty().add("<test>", String::from(s));

    grammar::TermParser::new()
        .parse_strict(alloc, id, lexer::Lexer::new(s))
        .map_err(|errs| errs.errors.first().unwrap().clone())
}

pub fn parse_extended(
    pos_table: &mut PosTable,
    s: &str,
) -> Result<ExtendedTerm<NickelValue>, ParseError> {
    let id = Files::empty().add("<test>", String::from(s));

    grammar::ExtendedTermParser::new()
        .parse_strict_compat(pos_table, id, lexer::Lexer::new(s))
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
