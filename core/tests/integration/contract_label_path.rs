use assert_matches::assert_matches;
use nickel_lang_core::{
    error::{Error, EvalErrorKind, IntoDiagnostics},
    eval::value::NickelValue,
    files::Files,
    label::ty_path::Elem,
};

use nickel_lang_utils::test_program::eval;

fn extract_blame_label_path(eval_result: &Result<NickelValue, Error>) -> Option<Vec<Elem>> {
    if let Err(Error::EvalError(data)) = &eval_result
        && let EvalErrorKind::BlameError {
            evaluated_arg: _,
            label,
        } = &data.error
    {
        Some(label.path.iter().copied().collect())
    } else {
        None
    }
}

#[test]
fn array_contracts_label_path_is_set_correctly() {
    let res = eval("%force% ([{a = [1]}] | Array {a: Array String}) false");

    assert_matches!(
        extract_blame_label_path(&res).unwrap().as_slice(),
        [Elem::Array, Elem::Field(id), Elem::Array] if &id.to_string() == "a"
    );

    // Check that reporting doesn't panic. Provide a dummy file database, as we won't report
    // the error message but just check that it can be built.
    let mut files = Files::empty();
    res.unwrap_err().into_diagnostics(&mut files);

    let res = eval(
        "(%array/at% (\
            ({foo = [(fun x => \"a\")]} | {foo: Array (forall a. a -> Number)}).foo\
        ) 0) false",
    );

    assert_matches!(
        extract_blame_label_path(&res).unwrap().as_slice(),
        [Elem::Field(id), Elem::Array, Elem::Codomain] if &id.to_string() == "foo"
    );

    res.unwrap_err().into_diagnostics(&mut files);
}

#[test]
fn dictionary_contracts_label_path_is_set_correctly() {
    let res = eval("%force% ({foo = 1} | {_ | String}) false");

    assert_matches!(
        extract_blame_label_path(&res).unwrap().as_slice(),
        [Elem::Dict]
    );
    res.unwrap_err().into_diagnostics(&mut Files::empty());
}
