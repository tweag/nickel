use nickel_lang_core::mk_app;
use nickel_lang_core::term::{make, StrChunk, Term, UnaryOp};
use nickel_lang_utils::{
    project_root::project_root,
    test_program::{eval, parse},
};

use std::io::Read;
use test_generator::test_resources;

// Exclude list for tests that aren't yet handled correctly by the pretty printer. The pretty
// printer should ideally be fixed to make them pass, but since we automatically try the pretty
// printer on each and every passing test, we need an escape hatch to make the CI pass in the
// meantime (also, the pretty printer isn't a critical part of the interpreter).
const EXCLUDE_LIST: &[&str] = &[
    "pass/merging/multiple_overrides.ncl",
    "pass/merging/priorities.ncl",
    "pass/contracts/contracts.ncl",
    "pass/merging/metavalues.ncl",
    "pass/contracts/types_dont_propagate.ncl",
];

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
    assert!(nb_diff == 0);
}

#[track_caller]
fn check_idempotent(path: &str) {
    if EXCLUDE_LIST.iter().any(|suffix| path.ends_with(suffix)) {
        return;
    }

    let mut buffer = String::new();
    let mut file = std::fs::File::open(project_root().join(path)).expect("Failed to open file");

    file.read_to_string(&mut buffer)
        .expect("Fail to read content of test file");

    let rt = parse(&buffer).unwrap();
    let pretty_rt = format!("{}", &rt);
    let double_pretty = format!("{}", &parse(&pretty_rt).unwrap());

    diff(&pretty_rt, &double_pretty);
}

#[test_resources("core/tests/integration/pass/**/*.ncl")]
fn pretty_integration_tests(path: &str) {
    check_idempotent(path)
}

#[test]
fn str_vs_strchunks() {
    assert_eq!(
        format!("{}", Term::Str("string".into())),
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
            Term::Op1(UnaryOp::StaticAccess("is_number".into()), make::var("std")),
            Term::Num((-5).into())
        )
    ))
    .unwrap();
}
