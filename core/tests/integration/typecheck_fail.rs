use assert_matches::assert_matches;
use codespan::Files;
use nickel_lang_core::cache::resolvers::DummyResolver;
use nickel_lang_core::error::TypecheckError;
use nickel_lang_core::parser::{grammar, lexer, ErrorTolerantParser};
use nickel_lang_core::term::RichTerm;
use nickel_lang_core::typ::{TypeF, Types};
use nickel_lang_core::{typecheck, typecheck::Context};

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
