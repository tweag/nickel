use assert_matches::assert_matches;
use nickel_lang::error::Error;

use nickel_lang_utilities::eval;

#[test]
fn unexpected_token() {
    assert_matches!(eval("\"Nickel\"$"), Err(Error::ParseErrors(..)));
}

#[test]
fn unexpected_token_in_record() {
    assert_matches!(eval("{ name = \"Nickel\",, }"), Err(Error::ParseErrors(..)));
}

#[test]
fn unexpected_token_buried() {
    assert_matches!(
        eval("{ foo = {bar = [2 @ ],}, baz = 2 }"),
        Err(Error::ParseErrors(..))
    );
}
