use assert_matches::assert_matches;
use codespan::Files;
use nickel_lang::error::{Error, EvalError, ToDiagnostic};

use nickel_lang_utilities::eval;

macro_rules! assert_raise_blame {
    ($term:expr) => {{
        assert_matches!(
            eval($term),
            Err(Error::EvalError(EvalError::BlameError(..)))
        )
    }};
}

#[test]
fn flat_contract_fail() {
    assert_raise_blame!(
        "let AlwaysTrue = fun l t => let boolT | Bool = t in
        if boolT then boolT else %blame% l in
        (false | AlwaysTrue)"
    );
}

#[test]
fn id_fail() {
    assert_raise_blame!(
        "let id | forall a. a -> a = fun x => false in
            id false && false"
    );
}

#[test]
fn enum_simple() {
    assert_raise_blame!("`far | [|foo, bar|]");
    assert_raise_blame!("123 | [|foo, bar|]");
    assert_raise_blame!("`foo | [| |]");
}

#[test]
fn metavalue_contract_default_fail() {
    assert_raise_blame!("true | default | Num");
}

#[test]
fn merge_contract() {
    assert_raise_blame!("let r = {a=2} & {a | Bool} in r.a");
}

#[test]
fn merge_default_contract() {
    assert_raise_blame!("({a=2} & {b | Num} & {b | default = true}).b");
}

#[test]
fn merge_compose_contract() {
    let mk_composed = |arg| {
        format!(
            "let Even = fun l x => if x % 2 == 0 then x else %blame% l in
        let DivBy3 = fun l x => if x % 3 ==  0 then x else %blame% l in
        let composed = {{a | Even}} & {{a | DivBy3}} in
        (({{a = {},}}) & composed).a",
            arg
        )
    };

    assert_raise_blame!(mk_composed("1"));
    assert_raise_blame!(mk_composed("14"));
    assert_raise_blame!(mk_composed("27"));
    assert_raise_blame!(mk_composed("10"));
    assert_raise_blame!(mk_composed("35"));
}

#[test]
fn records_contracts_simple() {
    assert_raise_blame!("{a=1} | {}");

    assert_raise_blame!("let x | {a: Num, s: Str} = {a = 1, s = 2} in %deep_seq% x x");
    assert_raise_blame!("let x | {a: Num, s: Str} = {a = \"a\", s = \"b\"} in %deep_seq% x x");
    assert_raise_blame!("let x | {a: Num, s: Str} = {a = 1} in %deep_seq% x x");
    assert_raise_blame!("let x | {a: Num, s: Str} = {s = \"a\"} in %deep_seq% x x");
    assert_raise_blame!(
        "let x | {a: Num, s: Str} = {a = 1, s = \"a\", extra = 1} in %deep_seq% x x"
    );

    assert_raise_blame!(
        "let x | {a: Num, s: {foo: Bool}} = {a = 1, s = {foo = 2}} in %deep_seq% x x"
    );
    assert_raise_blame!(
        "let x | {a: Num, s: {foo: Bool}} = {a = 1, s = {foo = true, extra = 1}} in %deep_seq% x x"
    );
    assert_raise_blame!("let x | {a: Num, s: {foo: Bool}} = {a = 1, s = {}} in %deep_seq% x x");
}

#[test]
fn records_contracts_poly() {
    // TODO: this test should ultimately pass (i.e., the program should be rejected)
    // but this is not yet the case.
    // eval_string(&format!("{} {{foo = 2}}", extd)).unwrap_err();

    let remv =
        "let f | forall a. {foo: Num ; a} -> { ; a} = fun x => %record_remove% \"foo\" x in f";
    assert_raise_blame!(&format!("{} {{}}", remv));

    let bad_cst = "let f | forall a. { ; a} -> { ; a} = fun x => {a=1} in f";
    let bad_acc = "let f | forall a. { ; a} -> { ; a} = fun x => %seq% x.a x in f";
    let bad_extd =
        "let f | forall a. { ; a} -> {foo: Num ; a} = fun x => %record_remove% \"foo\" x in f";
    let bad_rmv =
        "let f | forall a. {foo: Num ; a} -> { ; a} = fun x => %record_insert% \"foo\" x 1 in f";

    assert_raise_blame!(&format!("{} {{}}", bad_cst));
    // The polymorphic parts is protected by hiding it. Thus, trying to access the protected field
    // gives a field missing error, instead of a blame error.
    assert_matches!(
        eval(&format!("{} {{a=1}}", bad_acc)),
        Err(Error::EvalError(EvalError::FieldMissing(..)))
    );
    assert_matches!(
        eval(&format!("{} {{foo=1}}", bad_extd)),
        Err(Error::EvalError(EvalError::FieldMissing(..)))
    );
    assert_raise_blame!(&format!("{} {{}}", bad_rmv));
    assert_raise_blame!(
        "let f |
            forall a.
                (forall b. {a: Num, b: Num ; b} -> { a: Num ; b})
                -> {a: Num ; a}
                -> { ; a}
            = fun f r => %record_remove% \"b\" (%record_remove% \"a\" (f r)) in
        f (fun x => x) {a = 1, b = true, c = 3}"
    );
}

#[test]
fn lists_contracts() {
    use nickel_lang::label::ty_path::Elem;

    assert_matches!(
        eval("%force% ([1, \"a\"] | Array Num) 0"),
        Err(Error::EvalError(EvalError::BlameError(..)))
    );
    assert_matches!(
        eval("1 | Array Dyn"),
        Err(Error::EvalError(EvalError::BlameError(..)))
    );
    assert_matches!(
        eval("(fun x => x) | Array Dyn"),
        Err(Error::EvalError(EvalError::BlameError(..)))
    );

    let res = eval("%force% ([{a = [1]}] | Array {a: Array Str}) false");
    match &res {
        Err(Error::EvalError(EvalError::BlameError(ref l, _))) => {
            assert_matches!(l.path.as_slice(), [Elem::Array, Elem::Field(id), Elem::Array] if &id.to_string() == "a")
        }
        err => panic!("expected blame error, got {:?}", err),
    }
    // Check that reporting doesn't panic. Provide a dummy file database, as we won't report
    // the error message but just check that it can be built.
    let mut files = Files::new();
    res.unwrap_err().to_diagnostic(&mut files, None);

    let res = eval(
        "(%elem_at% (({foo = [(fun x => \"a\")]} | {foo: Array (forall a. a -> Num)}).foo) 0) false",
    );
    match &res {
        Err(Error::EvalError(EvalError::BlameError(ref l, _))) => {
            assert_matches!(l.path.as_slice(), [Elem::Field(id), Elem::Array, Elem::Codomain] if &id.to_string() == "foo")
        }
        err => panic!("expected blame error, got {:?}", err),
    }
    res.unwrap_err().to_diagnostic(&mut files, None);
}

#[test]
fn records_contracts_closed() {
    assert_raise_blame!("{a=1} | {}");
    assert_raise_blame!("{a=1, b=2} | {a | Num}");
    assert_raise_blame!("let Contract = {a | Num} & {b | Num} in ({a=1, b=2, c=3} | Contract)");
}

// #[test]
// fn enum_complex() {
//     eval(
//         "let f : <foo, bar> -> Num =
//             fun x => switch { `foo => 1, `bar => 2, } x in
//             f `boo",
//     )
//     .unwrap_err();
// }
