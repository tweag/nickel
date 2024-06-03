use assert_matches::assert_matches;
use codespan::Files;
use nickel_lang_core::error::{Error, EvalError, IntoDiagnostics};

use nickel_lang_utils::test_program::eval;

#[test]
fn array_contracts_label_path_is_set_correctly() {
    use nickel_lang_core::label::ty_path::Elem;

    let res = eval("%force% ([{a = [1]}] | Array {a: Array String}) false");
    match &res {
        Err(Error::EvalError(EvalError::BlameError {
            evaluated_arg: _,
            ref label,
            call_stack: _,
        })) => assert_matches!(
            label.path.as_slice(),
            [Elem::Array, Elem::Field(id), Elem::Array] if &id.to_string() == "a"
        ),
        err => panic!("expected blame error, got {err:?}"),
    }
    // Check that reporting doesn't panic. Provide a dummy file database, as we won't report
    // the error message but just check that it can be built.
    let mut files = Files::new();
    res.unwrap_err().into_diagnostics(&mut files, None);

    let res = eval(
        "(%array/at% (\
            ({foo = [(fun x => \"a\")]} | {foo: Array (forall a. a -> Number)}).foo\
        ) 0) false",
    );
    match &res {
        Err(Error::EvalError(EvalError::BlameError {
            evaluated_arg: _,
            ref label,
            call_stack: _,
        })) => assert_matches!(
            label.path.as_slice(),
            [Elem::Field(id), Elem::Array, Elem::Codomain] if &id.to_string() == "foo"
        ),
        err => panic!("expected blame error, got {err:?}"),
    }
    res.unwrap_err().into_diagnostics(&mut files, None);
}

#[test]
fn dictionary_contracts_label_path_is_set_correctly() {
    use nickel_lang_core::label::ty_path::Elem;

    let res = eval("%force% ({foo = 1} | {_ | String}) false");
    match &res {
        Err(Error::EvalError(EvalError::BlameError {
            evaluated_arg: _,
            ref label,
            call_stack: _,
        })) => {
            assert_matches!(label.path.as_slice(), [Elem::Dict])
        }
        err => panic!("expected blame error, got {err:#?}"),
    }
}
