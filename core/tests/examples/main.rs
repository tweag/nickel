use assert_matches::assert_matches;
use nickel_lang_core::error::{Error, EvalError};
use nickel_lang_utils::{
    annotated_test::{read_annotated_test_case, TestCase},
    project_root::project_root,
    test_program::TestProgram,
};
use serde::Deserialize;
use test_generator::test_resources;

#[test_resources("examples/**/*.ncl")]
fn check_example_file(path: &str) {
    let test: TestCase<Expectation> =
        read_annotated_test_case(path).expect("Failed to parse annotated program");

    // `test_resources` uses paths relative to the workspace manifesty
    let mut p = TestProgram::new_from_file(project_root().join(path), std::io::stderr())
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
