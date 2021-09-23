use assert_matches::assert_matches;
use codespan::Files;
use nickel::cache::resolvers::DummyResolver;
use nickel::error::TypecheckError;
use nickel::parser::{grammar, lexer};
use nickel::term::RichTerm;
use nickel::typecheck::{type_check_in_env, Environment};
use nickel::types::Types;

fn type_check(rt: &RichTerm) -> Result<Types, TypecheckError> {
    type_check_in_env(rt, &Environment::new(), &mut DummyResolver {})
}

fn type_check_expr(s: impl std::string::ToString) -> Result<Types, TypecheckError> {
    let s = s.to_string();
    let id = Files::new().add("<test>", s.clone());
    type_check(
        &grammar::TermParser::new()
            .parse(id, lexer::Lexer::new(&s))
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
    assert_typecheck_fails!("(fun x => x) : #(fun l t => t) -> #(fun l t => t)");
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
    assert_typecheck_fails!("`foo : <bar>");
    assert_typecheck_fails!("switch { foo => 3} `bar : Num");
    assert_typecheck_fails!("switch { foo => 3, bar => true} `bar : Num");
}

#[test]
fn enum_complex() {
    assert_typecheck_fails!(
        "(fun x => switch {bla => 1, ble => 2, bli => 4} x) : <bla, ble> -> Num"
    );
    // TODO typecheck this, I'm not sure how to do it with row variables
    // LATER NOTE: this requires row subtyping, not easy
    assert_typecheck_fails!(
        "(fun x =>
            (switch {bla => 3, bli => 2} x) +
            (switch {bla => 6, blo => 20} x)) `bla : Num"
    );
    assert_typecheck_fails!(
        "let f : forall r. <blo, ble | r> -> Num =
            fun x => (switch {blo => 1, ble => 2, bli => 3} x) in
        f"
    );
    assert_typecheck_fails!(
        "let f : forall r. (forall p. <blo, ble | r> -> <bla, bli | p>) =
            fun x => (switch {blo => `bla, ble => `bli, _ => `blo} x) in
        f `bli"
    );
}

#[test]
fn static_record_simple() {
    assert_typecheck_fails!("{bla = true} : {bla : Num}");
    assert_typecheck_fails!("{blo = 1} : {bla : Num}");

    assert_typecheck_fails!("{blo = 1}.blo : Bool");

    assert_typecheck_fails!(
        "let f : forall a. (forall r. {bla : Bool, blo : a, ble : a | r} -> a) =
            fun r => if r.bla then r.blo else r.ble in
         (f {bla = true, blo = 1, ble = true, blip = `blip} : Num)"
    );
    assert_typecheck_fails!(
        "let f : forall a. (forall r. {bla : Bool, blo : a, ble : a | r} -> a) =
            fun r => if r.bla then (r.blo + 1) else r.ble in
         (f {bla = true, blo = 1, ble = 2, blip = `blip} : Num)"
    );
}

#[test]
fn dynamic_record_simple() {
    assert_typecheck_fails!(
        "({ \"#{if true then \"foo\" else \"bar\"}\" = 2, \"foo\" = true, }.\"bla\") : Num"
    );
}

#[test]
fn simple_list() {
    assert_typecheck_fails!("[1, 2, false] : List Num");
    assert_typecheck_fails!("[(1 : Str), true, \"b\"] : List");
    assert_typecheck_fails!("[1, 2, \"3\"] : List Str");
}

#[test]
fn lists_operations() {
    assert_typecheck_fails!("(fun l => %head% l) : forall a b. (List a -> b)");
    assert_typecheck_fails!(
        "(fun f l => %elemAt% (%map% l f) 0) : forall a. (forall b. (a -> b) -> List -> b)"
    );
}

#[test]
fn imports() {
    use nickel::cache::{resolvers::SimpleResolver, ImportResolver};
    use nickel::error::ImportError;
    use nickel::term::{make as mk_term, RichTerm};
    use nickel::transformations::resolve_imports;

    let mut resolver = SimpleResolver::new();
    resolver.add_source(String::from("good"), String::from("1 + 1 : Num"));
    resolver.add_source(String::from("bad"), String::from("false : Num"));
    resolver.add_source(
        String::from("proxy-bad"),
        String::from("let x = import \"bad\" in x"),
    );
    resolver.add_source(
        String::from("proxy-wrong-type"),
        String::from("let x : Str = import \"good\" in x"),
    );

    fn mk_import<R>(import: &str, resolver: &mut R) -> Result<RichTerm, ImportError>
    where
        R: ImportResolver,
    {
        resolve_imports(
            mk_term::let_in("x", mk_term::import(import), mk_term::var("x")),
            resolver,
        )
    }

    assert_matches!(
        type_check_in_env(
            &mk_import("proxy-wrong-type", &mut resolver).unwrap(),
            &Environment::new(),
            &mut resolver,
        ),
        Err(TypecheckError::TypeMismatch(..))
    );

    assert_matches!(
        type_check_in_env(
            &mk_import("proxy-bad", &mut resolver).unwrap(),
            &Environment::new(),
            &mut resolver,
        ),
        Err(TypecheckError::TypeMismatch(..))
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
    fn assert_row_conflict(res: Result<Types, TypecheckError>) {
        assert!(match res.unwrap_err() {
            TypecheckError::RowConflict(_, _, _, _, _) => true,
            TypecheckError::ArrowTypeMismatch(_, _, _, err_boxed, _) => {
                if let TypecheckError::RowConflict(_, _, _, _, _) = err_boxed.as_ref() {
                    true
                } else {
                    false
                }
            }
            _ => true,
        })
    }

    let mut res = type_check_expr(
        "let extend | forall c. { | c} -> {a: Str | c} = null in
           (let bad = extend {a = 1} in 0) : Num",
    );
    assert_row_conflict(res);

    res = type_check_expr(
        "let remove | forall c. {a: Str | c} -> { | c} = nul in
           (let bad = remove (remove {a = \"a\"}) in 0) : Num",
    );
    assert_row_conflict(res);
}

#[test]
fn dynamic_row_tail() {
    assert_typecheck_fails!("({a = 1} : {a: Num | Dyn}) : {a: Num}");
}

#[test]
fn shallow_type_inference() {
    assert_matches!(
        type_check_expr("let x = (1 + 1) in (x + 1 : Num)"),
        Err(TypecheckError::TypeMismatch(..))
    );
}

#[test]
fn dynamic_record_subtyping() {
    assert_matches!(
        type_check_expr("({a = 1} | {a: Num | Dyn}) : {_: Num}"),
        Err(TypecheckError::RowTailMismatch(..))
    );
    assert_matches!(
        type_check_expr("({a = \"a\"} | {a: Str | Num}) : {_: Num}"),
        Err(TypecheckError::RowTailMismatch(..))
    );
    assert_typecheck_fails!("({a = 1} : {_ : Dyn}) : {_: Num}");
    assert_typecheck_fails!("({a = \"a\"} : {_ : Str}) : {_: Num}");
}
