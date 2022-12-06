use nickel_lang::term::{MetaValue, SharedTerm, Term};
use nickel_lang_utilities::TestProgram;

#[test]
pub fn test_query_metadata_basic() {
    let mut program = TestProgram::new_from_source(
        "{val | doc \"Test basic\" = (1 + 1)}".as_bytes(),
        "regr_tests",
    )
    .unwrap();
    let result = program.query(Some(String::from("val"))).unwrap();

    if let Term::MetaValue(meta) = result {
        assert_eq!(meta.doc, Some(String::from("Test basic")));
        assert_eq!(meta.value.unwrap().term, SharedTerm::new(Term::Num(2.0)));
    } else {
        panic!();
    }
}

#[test]
pub fn test_query_with_wildcard() {
    /// Checks whether `lhs` and `rhs` both evaluate to terms with the same static type
    fn assert_types_eq(lhs: &str, rhs: &str) {
        let term1 = TestProgram::new_from_source(lhs.as_bytes(), "regr_tests")
            .unwrap()
            .query(None)
            .unwrap();
        let term2 = TestProgram::new_from_source(rhs.as_bytes(), "regr_tests")
            .unwrap()
            .query(None)
            .unwrap();
        if let (
            Term::MetaValue(MetaValue {
                types: Some(contract1),
                ..
            }),
            Term::MetaValue(MetaValue {
                types: Some(contract2),
                ..
            }),
        ) = (term1, term2)
        {
            assert_eq!(contract1.types, contract2.types);
            assert_eq!(
                contract1.label.types.as_ref(),
                contract2.label.types.as_ref()
            );
        } else {
            panic!();
        }
    }

    // Without wildcard, the result has no type annotation
    let mut program = TestProgram::new_from_source("10".as_bytes(), "regr_tests").unwrap();
    let result = program.query(None).unwrap();
    assert!(!matches!(result, Term::MetaValue(_)));

    // With a wildcard, there is a type annotation, inferred to be Num
    assert_types_eq("10 : _", "10 : Num");

    // Wildcard infers record type
    assert_types_eq(
        r#"{foo: Str = "quux"} : _"#,
        r#"{foo: Str = "quux"} : {foo: Str}"#,
    );

    // Wildcard infers function type, infers inside `let`
    assert_types_eq(
        r#"let f : _ = fun x => x + 1 in f"#,
        r#"(fun x => x + 1) : Num -> Num"#,
    );
}
