use nickel_lang::term::{
    record::{Field, FieldMetadata},
    SharedTerm, Term, TypeAnnotation,
};
use nickel_lang_utilities::TestProgram;

#[test]
pub fn test_query_metadata_basic() {
    let mut program = TestProgram::new_from_source(
        "{val | doc \"Test basic\" = (1 + 1)}".as_bytes(),
        "regr_tests",
    )
    .unwrap();
    let result = program.query(Some(String::from("val"))).unwrap();

    assert_eq!(result.metadata.doc, Some(String::from("Test basic")));
    assert_eq!(result.value.unwrap().term, SharedTerm::new(Term::Num(2.0)));
}

#[test]
pub fn test_query_with_wildcard() {
    let path = Some(String::from("value"));

    /// Checks whether `lhs` and `rhs` both evaluate to terms with the same static type
    fn assert_types_eq(lhs: &str, rhs: &str, path: Option<String>) {
        let term1 = TestProgram::new_from_source(lhs.as_bytes(), "regr_tests")
            .unwrap()
            .query(path.clone())
            .unwrap();
        let term2 = TestProgram::new_from_source(rhs.as_bytes(), "regr_tests")
            .unwrap()
            .query(path)
            .unwrap();
        if let (
            Field {
                metadata:
                    FieldMetadata {
                        annotation:
                            TypeAnnotation {
                                types: Some(contract1),
                                ..
                            },
                        ..
                    },
                ..
            },
            Field {
                metadata:
                    FieldMetadata {
                        annotation:
                            TypeAnnotation {
                                types: Some(contract2),
                                ..
                            },
                        ..
                    },
                ..
            },
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
    let mut program =
        TestProgram::new_from_source("{value = 10}".as_bytes(), "regr_tests").unwrap();
    let result = program.query(path.clone()).unwrap();
    assert!(!matches!(
        result,
        Field {
            metadata: FieldMetadata {
                annotation: TypeAnnotation { types: None, .. },
                ..
            },
            ..
        }
    ));

    // With a wildcard, there is a type annotation, inferred to be Num
    assert_types_eq("{value : _ = 10}", "{value : Num = 10}", path.clone());

    // Wildcard infers record type
    assert_types_eq(
        r#"{val : _ = {foo = "quux"}}"#,
        r#"{val : {foo: Str} = {foo = "quux"}}"#,
        path.clone(),
    );

    // Wildcard infers function type, infers inside `let`
    assert_types_eq(
        r#"{val : _ = let f = fun x => x + 1 in f}"#,
        r#"{val : Num -> Num = (fun x => x + 1)}"#,
        path.clone(),
    );
}
