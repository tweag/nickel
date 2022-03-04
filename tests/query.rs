use nickel_lang::program::Program;
use nickel_lang::term::{SharedTerm, Term};

#[test]
pub fn test_query_metadata_basic() {
    let mut program =
        Program::new_from_source("(1+1) | doc \"Test basic\"".as_bytes(), "regr_tests").unwrap();
    let result = program.query(None).unwrap();

    if let Term::MetaValue(meta) = result {
        assert_eq!(meta.doc, Some(String::from("Test basic")));
        assert_eq!(meta.value.unwrap().term, SharedTerm::new(Term::Num(2.0)));
    } else {
        panic!();
    }
}

#[test]
pub fn test_query_metadata_from_func() {
    let mut program = Program::new_from_source(
        "builtin.seq 2 ((3+1) | doc \"Test from func\")".as_bytes(),
        "regr_tests",
    )
    .unwrap();
    let result = program.query(None).unwrap();

    if let Term::MetaValue(meta) = result {
        assert_eq!(meta.doc, Some(String::from("Test from func")));
        assert_eq!(meta.value.unwrap().term, SharedTerm::new(Term::Num(4.0)));
    } else {
        panic!();
    }
}
