use assert_matches::assert_matches;
use codespan::Files;
use nickel_lang::cache::resolvers::DummyResolver;
use nickel_lang::error::TypecheckError;
use nickel_lang::parser::{grammar, lexer};
use nickel_lang::term::RichTerm;
use nickel_lang::types::{TypeF, Types};
use nickel_lang::{typecheck, typecheck::Context};
use nickel_lang_utilities::typecheck_fixture;

fn type_check(rt: &RichTerm) -> Result<(), TypecheckError> {
    typecheck::type_check(rt, Context::new(), &DummyResolver {}).map(|_| ())
}

fn type_check_expr(s: impl std::string::ToString) -> Result<(), TypecheckError> {
    let s = s.to_string();
    let id = Files::new().add("<test>", s.clone());
    type_check(
        &grammar::TermParser::new()
            .parse_term(id, lexer::Lexer::new(&s))
            .unwrap(),
    )
}

macro_rules! assert_typecheck_fails {
    ($term:expr) => {{
        assert_matches!(type_check_expr($term), Err(..))
    }};
}

#[test]
fn unbound_variable_always_throws() {
    assert_matches!(
        type_check_expr("x"),
        Err(TypecheckError::UnboundIdentifier(..))
    )
}

#[test]
fn promise_simple_checks() {
    assert_typecheck_fails!("true : Num");
    assert_typecheck_fails!("34.5 : Bool");
    assert_typecheck_fails!("(34 | Bool) : Num");
    assert_typecheck_fails!("\"hello\" : Num");
}

#[test]
fn promise_complicated() {
    // Inside Promises we typecheck strictly
    assert_typecheck_fails!("let f : Bool -> Num = fun x => if x then x + 1 else 34 in f false");
    // not annotated, non trivial let bindings type to Dyn
    assert_typecheck_fails!("let id = fun x => x in (id 4 : Num)");
    // no implicit polymorphism
    assert_typecheck_fails!("(fun id => (id 4 : Num) + (id true : Bool)) (fun x => x)");
    // contract equality (to be fair, the current implementation is full of issues: to be reworked)
    assert_typecheck_fails!("(fun x => x) : (fun l t => t) -> (fun l t => t)");
}

#[test]
fn simple_forall() {
    assert_typecheck_fails!(
        "let f : forall a. (forall b. a -> b -> a) = fun x y => y in
         f"
    );
    assert_typecheck_fails!(
        "((fun f =>
            let g : forall b. b -> b = fun y => y in
            f g)
            : ((forall a. a -> a) -> Num) -> Num)
            (fun x => 3)"
    );
    assert_typecheck_fails!(
        "let g : Num -> Num = fun x => x in
         let f : forall a. a -> a = fun x => g x in
         f"
    );
}

#[test]
fn enum_simple() {
    assert_typecheck_fails!("`foo : [| `bar |]");
    assert_typecheck_fails!("match { `foo => 3} `bar : Num");
    assert_typecheck_fails!("match { `foo => 3, `bar => true} `bar : Num");
}

#[test]
fn enum_complex() {
    assert_typecheck_fails!("(match {`bla => 1, `ble => 2, `bli => 4}) : [| `bla, `ble |] -> Num");
    // TODO typecheck this, I'm not sure how to do it with row variables
    // LATER NOTE: this requires row subtyping, not easy
    assert_typecheck_fails!(
        "(fun x =>
            (x |> match {`bla => 3, `bli => 2}) +
            (x |> match {`bla => 6, `blo => 20})) `bla : Num"
    );
    assert_typecheck_fails!(
        "let f : forall r. [| `blo, `ble ; r |] -> Num =
            match {`blo => 1, `ble => 2, `bli => 3} in
        f"
    );
    assert_typecheck_fails!(
        "let f : forall r. (forall p. [| `blo, `ble ; r |] -> [| `bla, `bli ; p |]) =
            match {`blo => `bla, `ble => `bli, _ => `blo} in
        f `bli"
    );
}

#[test]
fn static_record_simple() {
    assert_typecheck_fails!("{bla = true} : {bla : Num}");
    assert_typecheck_fails!("{blo = 1} : {bla : Num}");

    assert_typecheck_fails!("{blo = 1}.blo : Bool");

    assert_typecheck_fails!(
        "let f : forall a. (forall r. {bla : Bool, blo : a, ble : a ; r} -> a) =
            fun r => if r.bla then r.blo else r.ble in
         (f {bla = true, blo = 1, ble = true, blip = `blip} : Num)"
    );
    assert_typecheck_fails!(
        "let f : forall a. (forall r. {bla : Bool, blo : a, ble : a ; r} -> a) =
            fun r => if r.bla then (r.blo + 1) else r.ble in
         (f {bla = true, blo = 1, ble = 2, blip = `blip} : Num)"
    );
}

#[test]
fn dynamic_record_simple() {
    assert_typecheck_fails!(
        "({ \"%{if true then \"foo\" else \"bar\"}\" = 2, \"foo\" = true, }.\"bla\") : Num"
    );
}

#[test]
fn simple_array() {
    assert_typecheck_fails!("[1, 2, false] : Array Num");
    assert_typecheck_fails!("[(1 : Str), true, \"b\"] : Array Dyn");
    assert_typecheck_fails!("[1, 2, \"3\"] : Array Str");
}

#[test]
fn arrays_operations() {
    assert_typecheck_fails!("(fun l => %head% l) : forall a b. (Array a -> b)");
    assert_typecheck_fails!(
        "(fun f l => %elem_at% (%map% l f) 0) : forall a. (forall b. (a -> b) -> Array Dyn -> b)"
    );
}

#[test]
fn recursive_records() {
    assert_typecheck_fails!("{a : Num = true, b = a + 1} : {a : Num, b : Num}");
    assert_typecheck_fails!("{a = 1, b : Bool = a} : {a : Num, b : Bool}");
}

#[test]
fn let_inference() {
    assert_typecheck_fails!("(let x = 1 + 2 in let f = fun x => x ++ \"a\" in f x) : Num");

    // Fields in recursive records are treated in the type environment in the same way as let-bound expressions
    assert_typecheck_fails!(
        "{ f = fun x => if x == 0 then false else 1 + (f (x + (-1)))} : {f : Num -> Num}"
    );
}

/// Regression test following [#144](https://github.com/tweag/nickel/issues/144). Check that
/// polymorphic type variables appearing inside a row type are correctly constrained at
/// instantiation.
#[test]
fn polymorphic_row_constraints() {
    // Assert that the result of evaluation is either directly a `RowConflict` error, or a
    // `RowConflict` wrapped in an `ArrowTypeMismatch`.
    fn assert_row_conflict(res: Result<(), TypecheckError>) {
        assert!(match res.unwrap_err() {
            TypecheckError::RowConflict(_, _, _, _, _) => true,
            TypecheckError::ArrowTypeMismatch(_, _, _, err_boxed, _) => {
                matches!(
                    err_boxed.as_ref(),
                    TypecheckError::RowConflict(_, _, _, _, _)
                )
            }
            _ => true,
        })
    }

    let mut res = type_check_expr(
        "let extend | forall c. { ; c} -> {a: Str ; c} = null in
           (let bad = extend {a = 1} in 0) : Num",
    );
    assert_row_conflict(res);

    res = type_check_expr(
        "let remove | forall c. {a: Str ; c} -> { ; c} = nul in
           (let bad = remove (remove {a = \"a\"}) in 0) : Num",
    );
    assert_row_conflict(res);
}

/// Regression test following [#841](https://github.com/tweag/nickel/issues/841).
/// Checks that type mismatches occurring in the tail of a row type with
/// unification variables don't cause a panic.
#[test]
fn row_type_unification_variable_mismatch() {
    use nickel_lang::error::Error;

    // We run the example multiple times as the order in which row types
    // are checked is not deterministic, and failures are handled differently
    // depending on whether they occur in the row head or tail.
    for _ in 0..10 {
        assert_matches!(
            typecheck_fixture("typecheck_fail/row_type_unification_variable_mismatch.ncl"),
            Err(Error::TypecheckError(TypecheckError::RowMismatch(..)))
        )
    }
}

#[test]
fn dynamic_row_tail() {
    // Currently, typechecking is conservative wrt the dynamic row type, meaning it can't
    // convert to a less precise type with a dynamic tail.
    assert_typecheck_fails!("{a = 1, b = 2} : {a: Num ; Dyn}");
    assert_typecheck_fails!("{a = 1} : {a: Num ; Dyn}");
    assert_typecheck_fails!("({a = 1} | {a: Num ; Dyn}) : {a: Num}");
    assert_typecheck_fails!("{a = 1} : {a: Num ; Dyn}");
}

#[test]
fn shallow_type_inference() {
    assert_matches!(
        type_check_expr("let x = (1 + 1) in (x + 1 : Num)"),
        Err(TypecheckError::TypeMismatch(..))
    );
}

#[test]
fn dynamic_record_field() {
    assert_matches!(
        type_check_expr("let x = \"foo\" in {\"%{x}\" = 1} : {foo: Num}"),
        Err(TypecheckError::TypeMismatch(..))
    );
}

#[test]
fn piecewise_signature() {
    assert_matches!(
        type_check_expr("{foo : Num, foo = \"bar\"}"),
        Err(TypecheckError::TypeMismatch(..))
    );
}

#[test]
fn recursive_let() {
    assert_matches!(
        type_check_expr("let rec f : Num -> Num = fun x => f \"hoi\" in null"),
        Err(TypecheckError::TypeMismatch(..))
    );
}

#[test]
fn fails_only_with_wildcard() {
    // Without a wildcard annotation, this type checks
    assert_matches!(
        type_check_expr("let head = fun l => %head% l in head 10"),
        Ok(_)
    );
    // However, with one, we get a type error
    assert_matches!(
        type_check_expr("(let head = fun l => %head% l in (head 10)) : _"),
        Err(TypecheckError::TypeMismatch(..))
    );
    // With an actual array, this passes
    assert_matches!(
        type_check_expr("(let head = fun l => %head% l in (head [10])) : _"),
        Ok(_)
    );
}

#[test]
fn wildcards_apparent_type_is_dyn() {
    // Wildcard-inferred types don't leak outside of the strict block.
    // Therefore, even though the body of `g` could type check, `f` has type
    // `Dyn`, exactly as if there were no wildcards, and the type checker
    // rejects the expression.
    assert_matches!(
        type_check_expr(
            r#"let f : _ -> _ = fun x => x + 1 in
let g : Num = f 0 in
g"#
        ),
        Err(TypecheckError::TypeMismatch(
            Types { ty: TypeF::Arrow(_, _), .. },
            Types { ty:TypeF::Dyn, .. },
            _
        ))
    );
}

#[test]
fn locally_different_flat_types() {
    assert_matches!(
        type_check_expr(
            "let lib = {Contract = fun label value => value} in
             let foo | lib.Contract = null in
             let lib = {Contract = fun label value => value} in
             foo : lib.Contract"
        ),
        Err(TypecheckError::TypeMismatch(
            Types { ty:TypeF::Flat(..), .. },
            Types { ty:TypeF::Flat(..), .. },
            _
        ))
    );
}
