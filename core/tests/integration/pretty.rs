use libtest_mimic::{Arguments, Failed, Trial};
use nickel_lang_core::mk_app;
use nickel_lang_core::term::{make, StrChunk, Term, UnaryOp};
use nickel_lang_utils::{
    project_root::project_root,
    test_program::{eval, parse},
};

use std::io::Read;
use std::path::Path;
use std::process::ExitCode;

pub fn main() -> ExitCode {
    let args = Arguments::from_args();
    let mut tests = nickel_lang_utils::path_tests::path_tests(
        "core/tests/integration/inputs/**/*.ncl",
        |_, path| check_idempotent(path),
    );
    tests.extend(nickel_lang_utils::path_tests::path_tests(
        "core/stdlib/*.ncl",
        |_, path| check_idempotent(path),
    ));
    tests.push(Trial::test("str_vs_strchunks", str_vs_strchunks));
    tests.push(Trial::test("negative_numbers", negative_numbers));
    libtest_mimic::run(&args, tests).exit_code()
}

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
fn check_idempotent(path: &Path) {
    let mut buffer = String::new();
    let mut file = std::fs::File::open(project_root().join(path)).expect("Failed to open file");

    file.read_to_string(&mut buffer)
        .expect("Fail to read content of test file");

    // Some test samples don't even parse (on purpose, as they are test for parse errors), so we
    // only proceed with samples that do.
    if let Ok(rt) = parse(&buffer) {
        let pretty_rt = format!("{}", &rt);
        let double_pretty = format!("{}", &parse(&pretty_rt).unwrap());

        diff(&pretty_rt, &double_pretty);
    }
}

fn str_vs_strchunks() -> Result<(), Failed> {
    assert_eq!(
        format!("{}", Term::Str("string".into())),
        format!(
            "{}",
            Term::StrChunks(vec![StrChunk::Literal("string".to_string())])
        )
    );
    Ok(())
}

fn negative_numbers() -> Result<(), Failed> {
    eval(format!(
        "{}",
        mk_app!(
            Term::Op1(UnaryOp::RecordAccess("is_number".into()), make::var("std")),
            Term::Num((-5).into())
        )
    ))
    .unwrap();
    Ok(())
}
