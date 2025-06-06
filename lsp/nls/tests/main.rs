use std::{path::Path, process::ExitCode};

use libtest_mimic::Arguments;
use lsp_harness::{TestFixture, TestHarness};
use nickel_lang_utils::project_root::project_root;

pub fn main() -> ExitCode {
    let args = Arguments::from_args();
    let tests =
        nickel_lang_utils::path_tests::path_tests("lsp/nls/tests/inputs/*.ncl", check_snapshot);
    libtest_mimic::run(&args, tests).exit_code()
}

fn check_snapshot(name: &str, path: &Path) {
    let _ = env_logger::try_init();

    let full_path = project_root().join(path);

    let contents = std::fs::read_to_string(full_path).unwrap();
    let fixture = TestFixture::parse(&contents).unwrap();
    let mut harness = TestHarness::new();

    harness.prepare_files(&fixture);
    for req in fixture.reqs {
        harness.request_dyn(req);
    }

    for url in fixture.expected_diags {
        harness.get_eval_diagnostics(url);
    }
    let output = String::from_utf8(harness.out).unwrap();

    insta::with_settings!(
        {filters => vec![("file:///C:", "file://")]},
        {
            insta::assert_snapshot!(name, output);
        }
    );
}
