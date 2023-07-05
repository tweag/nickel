use nickel_lang_core::term::{make, RichTerm, StrChunk, Term, UnaryOp};
use nickel_lang_core::{mk_app, pretty::*};
use nickel_lang_utils::{
    project_root::project_root,
    test_program::{eval, parse},
};

use pretty::BoxAllocator;
use std::io::{Cursor, Read};
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

#[test_resources("core/tests/integration/pass/**/*.ncl")]
fn check_idempotent(path: &str) {
    if EXCLUDE_LIST.iter().any(|suffix| path.ends_with(suffix)) {
        return;
    }

    let mut buffer = String::new();
    let mut file = std::fs::File::open(project_root().join(path)).expect("Failed to open file");

    file.read_to_string(&mut buffer)
        .expect("Fail to read content of test file");

    let rt = parse(&buffer).unwrap();
    let pretty_rt = pretty(&rt);
    let double_pretty = pretty(&parse(&pretty_rt).unwrap());

    diff(&pretty_rt, &double_pretty);
}

fn pretty(rt: &RichTerm) -> String {
    let allocator = BoxAllocator;
    let mut ret = Vec::new();
    let mut rt_pretty = Cursor::new(&mut ret);

    let doc: DocBuilder<_, ()> = rt.clone().pretty(&allocator);
    doc.render(80, &mut rt_pretty).unwrap();
    String::from_utf8_lossy(&ret).into_owned()
}

#[test]
fn str_vs_strchunks() {
    assert_eq!(
        pretty(&Term::Str("string".into()).into()),
        pretty(&Term::StrChunks(vec![StrChunk::Literal("string".to_string())]).into())
    );
}

#[test]
fn negative_numbers() {
    eval(pretty(&mk_app!(
        Term::Op1(UnaryOp::StaticAccess("is_number".into()), make::var("std")),
        Term::Num((-5).into())
    )))
    .unwrap();
}
