// Since [RFC002](../rfcs/002-merge-types-terms-syntax.md), the following tests don't raise an
// unbound type variable error anymore. Instead, the variable is interpreted as a term variable:
// the tests should still fail, but they can now fail with a standard unbound identifier error.
use assert_matches::assert_matches;
use nickel::{
    error::{Error, EvalError, ParseError, ParseErrors, TypecheckError},
    identifier::Ident,
};

use utilities::eval;

macro_rules! assert_unbound {
    ( $term:expr, $var:expr ) => {
        let res = eval($term);
        // Because expressions inside user-defined contracts are not currently typechecked, the
        // error should be an `EvalError::UnboundIdentifier` at the time of writing this comment.
        // Since it may not be the case forever, we accept alternative errors that make sense.
        assert_matches!(res, Err(Error::ParseErrors(_)) | Err(Error::TypecheckError(_)) | Err(Error::EvalError(_)));

        match res {
            Err(Error::ParseErrors(ParseErrors {errors})) =>
                assert_matches!(errors.as_slice(),
                    [ParseError::UnboundTypeVariables(vars, _)]
                    if vars.contains(&Ident::from($var)) && vars.len() == 1),
            Err(Error::TypecheckError(err)) =>
                assert_matches!(err,
                    TypecheckError::UnboundIdentifier(id, ..)
                    | TypecheckError::UnboundTypeVariable(id, ..)
                    if id.label.as_str() == $var),
            Err(Error::EvalError(err)) =>
                assert_matches!(err,
                    EvalError::UnboundIdentifier(id, ..)
                    if id.label.as_str() == $var),
            _ => unreachable!(),
        };
    }
}

#[test]
fn unbound_type_variables() {
    assert_unbound!("1 | a", "a");
    assert_unbound!(
        "let f | forall a b c. a -> (b -> List c) -> {foo : List {_ : d}, bar: a; Dyn}
           = fun bar_ _g => {foo = [{field = 1}], bar = bar_} in
         (f 1 (fun _x => [])).foo == [{field = 1}]",
        "d"
    );
    assert_unbound!(
        "let f : forall a b c. a -> (b -> List c) -> {foo : List {field : a}, bar: a; e}
           = fun bar_ _g => {foo = [{field = bar_}], bar = bar_} in
         null",
        "e"
    );
    assert_unbound!(
        "let f | a -> (forall a. a -> a)
                       = fun x => builtin.seq x null in
                       f null",
        "a"
    );
}
