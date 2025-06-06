use std::{path::Path, process::ExitCode};

use assert_matches::assert_matches;
use libtest_mimic::Arguments;
use nickel_lang_core::error::{Error, EvalError, NullReporter};
use nickel_lang_utils::{
    annotated_test::{read_annotated_test_case, TestCase},
    project_root::project_root,
    test_program::TestProgram,
};
use serde::Deserialize;

pub fn main() -> ExitCode {
    let args = Arguments::from_args();
    let tests = nickel_lang_utils::path_tests::path_tests("examples/**/*.ncl", check_example_file);
    libtest_mimic::run(&args, tests).exit_code()
}

fn check_example_file(_name: &str, path: &Path) {
    let test: TestCase<Expectation> = read_annotated_test_case(path.as_os_str().to_str().unwrap())
        .expect("Failed to parse annotated program");

    // `test_resources` uses paths relative to the workspace manifesty
    let mut p = TestProgram::new_from_file(
        project_root().join(path),
        std::io::stderr(),
        NullReporter {},
    )
    .expect("Failed to load program from file");

    match test.annotation {
        Expectation::Pass => {
            p.eval_deep()
                .expect("Example is marked as 'pass' but evaluation failed");
        }
        Expectation::Blame => assert_matches!(
            p.eval_deep(),
            Err(Error::EvalError(EvalError::BlameError { .. }))
        ),
        Expectation::Ignore => (),
    }
}

#[derive(Deserialize)]
#[serde(tag = "test")]
enum Expectation {
    #[serde(rename = "pass")]
    Pass,
    #[serde(rename = "blame")]
    Blame,
    #[serde(rename = "ignore")]
    Ignore,
}
