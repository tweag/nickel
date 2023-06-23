use assert_matches::assert_matches;
use codespan::Files;
use nickel_lang_lib::cache::resolvers::DummyResolver;
use nickel_lang_lib::error::TypecheckError;
use nickel_lang_lib::parser::{grammar, lexer, ErrorTolerantParser};
use nickel_lang_lib::term::RichTerm;
use nickel_lang_lib::types::{TypeF, Types};
use nickel_lang_lib::{typecheck, typecheck::Context};

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
