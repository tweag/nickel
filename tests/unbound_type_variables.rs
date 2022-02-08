use assert_matches::assert_matches;
use nickel::{
    error::{Error, ParseError, ParseErrors},
    identifier::Ident,
};

use utilities::eval;

macro_rules! assert_unbound {
    ( $term:expr, $var:expr ) => {
        let res = eval($term);
        assert_matches!(res, Err(Error::ParseErrors(_)));

        match res {
            Err(Error::ParseErrors(ParseErrors {errors})) => {
                assert_matches!(errors.as_slice(), [ParseError::UnboundTypeVariables(vars, _)] if vars.contains(&Ident::from($var)) && vars.len() == 1)
            }
            _ => unreachable!(),
        };
    }
}

#[test]
fn unbound_type_variables() {
    assert_unbound!("1 | a", "a");
    assert_unbound!(
        "null | forall a b c. a -> (b -> List c) -> {foo : List {_ : d}, bar: b; Dyn}",
        "d"
    );
    assert_unbound!(
        "null | forall a b c. a -> (b -> List c) -> {foo : List {_ : a}, bar: b; e}",
        "e"
    );
    assert_unbound!("null | a -> (forall a. a -> a)", "a");
}
