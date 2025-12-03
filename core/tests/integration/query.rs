use nickel_lang_core::{
    error::NullReporter,
    eval::cache::lazy::CBNCache,
    program::Program,
    term::{TypeAnnotation, make as mk_term, record::FieldMetadata},
};

use nickel_lang_utils::test_program::TestProgram;

trait ProgramExt {
    fn with_field_path(self, path: &str) -> Self;
}

impl ProgramExt for Program<CBNCache> {
    fn with_field_path(mut self, path: &str) -> Self {
        self.field = self.parse_field_path(path.to_owned()).unwrap();
        self
    }
}

#[test]
pub fn test_query_metadata_basic() {
    let result = TestProgram::new_from_source(
        "{val | doc \"Test basic\" = (1 + 1)}".as_bytes(),
        "regr_tests",
        std::io::stderr(),
        NullReporter {},
    )
    .unwrap()
    .with_field_path("val")
    .query()
    .unwrap();

    assert_eq!(result.metadata.doc(), Some("Test basic"));
    assert_eq!(result.value.unwrap().without_pos(), mk_term::integer(2));
}

#[test]
pub fn test_query_with_wildcard() {
    let path = "value";

    /// Checks whether `lhs` and `rhs` both evaluate to terms with the same static type
    #[track_caller]
    fn assert_types_eq(lhs: &str, rhs: &str, path: &str) {
        let term1 = TestProgram::new_from_source(
            lhs.as_bytes(),
            "regr_tests",
            std::io::stderr(),
            NullReporter {},
        )
        .unwrap()
        .with_field_path(path)
        .query()
        .unwrap();
        let term2 = TestProgram::new_from_source(
            rhs.as_bytes(),
            "regr_tests",
            std::io::stderr(),
            NullReporter {},
        )
        .unwrap()
        .with_field_path(path)
        .query()
        .unwrap();
        let (
            Some(FieldMetadata {
                annotation:
                    TypeAnnotation {
                        typ: Some(contract1),
                        ..
                    },
                ..
            }),
            Some(FieldMetadata {
                annotation:
                    TypeAnnotation {
                        typ: Some(contract2),
                        ..
                    },
                ..
            }),
        ) = (term1.metadata.as_ref(), term2.metadata.as_ref())
        else {
            panic!();
        };

        // Since RFC005, we closurize the contracts attached to fields, which makes comparing
        // them after transformation quite hard. We can only rely on the original types as
        // stored inside the label, which is the one used for reporting anyway.
        // assert_eq!(contract1.types, contract2.types);
        assert_eq!(
            contract1.label.typ.as_ref().clone().without_pos(),
            contract2.label.typ.as_ref().clone().without_pos()
        );
    }

    // Without wildcard, the result has no type annotation
    let result = TestProgram::new_from_source(
        "{value = 10}".as_bytes(),
        "regr_tests",
        std::io::stderr(),
        NullReporter {},
    )
    .unwrap()
    .with_field_path(path)
    .query()
    .unwrap();

    assert!(result.metadata.is_empty());
    // assert!(matches!(
    //     result.metadata.as_ref(),
    //     Some(FieldMetadata {
    //         annotation: TypeAnnotation { typ: None, .. },
    //         ..
    //     }),
    // ));

    // With a wildcard, there is a type annotation, inferred to be Number
    assert_types_eq("{value : _ = 10}", "{value : Number = 10}", path);

    // Wildcard infers record type
    assert_types_eq(
        r#"{value : _ = {foo = "quux"}}"#,
        r#"{value : {foo: String} = {foo = "quux"}}"#,
        path,
    );

    // Wildcard infers function type, infers inside `let`
    assert_types_eq(
        r#"{value : _ = let f = fun x => x + 1 in f}"#,
        r#"{value : Number -> Number = (fun x => x + 1)}"#,
        path,
    );
}
