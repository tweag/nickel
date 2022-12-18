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
    ($term:expr, $($arg:tt)+) => {{
        assert_matches!(
            eval($term),
            Err(Error::EvalError(EvalError::BlameError(..))),
            $($arg)+
        )
    }}
}

macro_rules! assert_raise_tail_blame {
    ($term:expr) => {{
        assert_matches!(
            eval($term),
            Err(Error::EvalError(EvalError::IllegalPolymorphicTailAccess { .. }))
        )
    }};
    ($term:expr, $($arg:tt)+) => {{
        assert_matches!(
            eval($term),
            Err(Error::EvalError(EvalError::IllegalPolymorphicTailAccess{ .. })),
            $($arg)+
        )
    }}
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
    assert_raise_blame!("`far | [|`foo, `bar|]");
    assert_raise_blame!("123 | [|`foo, `bar|]");
    assert_raise_blame!("`foo | [| |]");
}

#[test]
fn metavalue_contract_default_fail() {
    assert_raise_blame!("{val | default | Num = true}.val");
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
    for (name, input) in [
        (
            "record argument missing all required fields",
            r#"
            let f | forall a. { foo: Num; a } -> Num = fun r => r.foo in
            f { }
            "#,
        ),
        (
            "record argument missing one required field",
            r#"
            let f | forall r. { x: Num, y: Num; r } -> { x: Num, y: Num; r } =
                fun r => r
            in
            f { x = 1 }
            "#,
        ),
        (
            "function arg which does not satisfy higher-order contract",
            r#"
            let f | (forall r. { x: Num; r } -> { ; r }) -> { x: Num, y: Num } -> { y : Num } =
                fun g r => g r
            in
            f (fun r => r) { x = 1, y = 2 }
            "#,
        ),
        (
            "invalid record argument to function arg",
            r#"
            let f | (forall r. { x: Num; r } -> { x: Num ; r }) -> { x: Str, y: Num } -> { x: Num, y: Num } =
                fun g r => g r
            in
            # We need to evaluate x here to cause the error.
            (f (fun r => r) { x = "", y = 1 }).x
            "#,
        ),
        (
            "return value without tail",
            r#"
            let f | forall r. { foo: Num; r } -> { foo: Num; r } =
                fun r => { foo = 1 }
            in
            f { foo = 1, other = 2 }
            "#,
        ),
        (
            "return value with wrong tail",
            r#"
            let f | forall r r'. { a: Num; r } -> { a: Num; r' } -> { a: Num; r } =
                fun r r' => r'
            in
            f { a = 1, b = "yes" } { a = 1, b = "no" }
            "#,
        ),
    ] {
        assert_raise_blame!(input, "failed on case: {}", name);
    }

    // These cases raise a custom error to give the user more information
    // about what went wrong.
    for (name, input) in [
        (
            "mapping over a record violates parametricity",
            r#"
            let f | forall r. { a: Num; r } -> { a: Str; r } =
                fun r => %record_map% r (fun x => %to_str% x)
            in
            f { a = 1, b = 2 }
            "#,
        ),
        (
            "merging a record violates parametricity, lhs arg",
            r#"
            let f | forall r. { a : Num; r } -> { a: Num; r } =
                fun r => { a | force = 0 } & r
            in
            f { a | default = 100, b = 1 }
            "#,
        ),
        (
            "merging a record violates parametricity, rhs arg",
            r#"
            let f | forall r. { a : Num; r } -> { a: Num; r } =
                fun r => r & { a | force = 0 }
            in
            f { a | default = 100, b = 1 }
            "#,
        ),
    ] {
        assert_raise_tail_blame!(input, "failed on case: {}", name);
    }

    // These are currently FieldMissing errors rather than BlameErrors as we
    // don't yet inspect the sealed tail to see if the requested field is inside.
    // Once we do, these tests will start failing, and if you're currently here
    // for that reason, you can just move the examples into the array above.
    let bad_acc = "let f | forall a. { ; a} -> { ; a} = fun x => %seq% x.a x in f";
    assert_matches!(
        eval(format!("{} {{a=1}}", bad_acc)),
        Err(Error::EvalError(EvalError::FieldMissing(..)))
    );

    let remove_sealed_field = r#"
        let remove_x | forall r. { ; r } -> { ; r } = fun r => %record_remove% "x" r in
        remove_x { x = 1 }
    "#;
    assert_matches!(
        eval(remove_sealed_field),
        Err(Error::EvalError(EvalError::FieldMissing(..)))
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

#[test]
fn enum_complex() {
    eval(
        "let f : [| `foo, `bar |] -> Num = match { `foo => 1, `bar => 2, } in
         f `boo",
    )
    .unwrap_err();
}
