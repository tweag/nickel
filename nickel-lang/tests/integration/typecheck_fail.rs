use assert_matches::assert_matches;
use codespan::Files;
use nickel_lang::cache::resolvers::DummyResolver;
use nickel_lang::error::TypecheckError;
use nickel_lang::parser::{grammar, lexer, ErrorTolerantParser};
use nickel_lang::term::RichTerm;
use nickel_lang::types::{TypeF, Types};
use nickel_lang::{typecheck, typecheck::Context};

fn type_check(rt: &RichTerm) -> Result<(), TypecheckError> {
    typecheck::type_check(rt, Context::new(), &DummyResolver {}).map(|_| ())
}

#[track_caller]
fn type_check_expr(s: impl std::string::ToString) -> Result<(), TypecheckError> {
    let s = s.to_string();
    let id = Files::new().add("<test>", s.clone());
    type_check(
        &grammar::TermParser::new()
            .parse_strict(id, lexer::Lexer::new(&s))
            .unwrap(),
    )
}
