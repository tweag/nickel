use nickel_lang_core::{
    ast::AstAlloc,
    eval::value::NickelValue,
    mk_app,
    term::{StrChunk, Term, UnaryOp, make},
};

use nickel_lang_utils::{
    project_root::project_root, test_program::eval, test_program::parse_bytecode_ast,
};

use std::io::Read;
use test_generator::test_resources;

#[track_caller]
fn diff(s1: &str, s2: &str) {
    use similar::*;
    let diff = TextDiff::from_lines(s1, s2);
    let mut nb_diff = 0;

    for change in diff.iter_all_changes() {
        match change.tag() {
            ChangeTag::Delete => nb_diff += 1,
            ChangeTag::Insert => nb_diff += 1,
            ChangeTag::Equal => (),
        };
    }
    if nb_diff != 0 {
        println!("{s1}");
        println!("{s2}");
        println!("{}", diff.unified_diff());
        panic!();
    }
}

/// Check idempotence of `Ast -> pretty output -> Ast`.
///
/// We don't check idempotence for the runtime value representation, because
/// `NickelValue`'s pretty-printing doesn't always produce parseable outputs
/// (intentionally, because of fresh identifiers).
#[track_caller]
fn check_idempotent(path: &str) {
    let mut buffer = String::new();
    let mut file = std::fs::File::open(project_root().join(path)).expect("Failed to open file");

    file.read_to_string(&mut buffer)
        .expect("Fail to read content of test file");

    let alloc = AstAlloc::new();
    // Some test samples don't even parse (on purpose, as they are test for parse errors), so we
    // only proceed with samples that do.
    if let Ok(ast) = parse_bytecode_ast(&alloc, &buffer) {
        let pretty_ast = format!("{}", &ast);
        let double_pretty = format!("{}", &parse_bytecode_ast(&alloc, &pretty_ast).unwrap());

        diff(&pretty_ast, &double_pretty);
    }
}

#[test_resources("core/tests/integration/inputs/**/*.ncl")]
fn pretty_integration_tests(path: &str) {
    check_idempotent(path);
}

#[test_resources("core/stdlib/*.ncl")]
fn pretty_standard_library(path: &str) {
    check_idempotent(path);
}

#[test]
fn str_vs_strchunks() {
    assert_eq!(
        format!("{}", NickelValue::string_posless("string")),
        format!(
            "{}",
            Term::StrChunks(vec![StrChunk::Literal("string".to_string())])
        )
    );
}

#[test]
fn negative_numbers() {
    eval(format!(
        "{}",
        mk_app!(
            Term::op1(UnaryOp::RecordAccess("is_number".into()), make::var("std")),
            NickelValue::number_posless(-5)
        )
    ))
    .unwrap();
}
