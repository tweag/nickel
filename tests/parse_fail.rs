use assert_matches::assert_matches;
use nickel_lang::error::Error;

use nickel_lang_utilities::eval;

#[test]
fn unexpected_token() {
    assert_matches!(eval("{ name = \"Nickel\",, }"), Err(Error::ParseErrors(..)));
}
