use nickel_lang_utils::project_root::project_root;
use test_generator::test_resources;

use lsp_harness::{TestFixture, TestHarness};

#[test_resources("lsp/nls/tests/inputs/*.ncl")]
fn check_snapshots(path: &str) {
    let _ = env_logger::try_init();

    let full_path = project_root().join(path);

    let contents = std::fs::read_to_string(&full_path).unwrap();
    let fixture = TestFixture::parse(&contents).unwrap();
    let mut harness = TestHarness::new();

    harness.prepare_files(&fixture);
    for req in fixture.reqs {
        harness.request_dyn(req);
    }
    harness.drain_notifications();
    let output = String::from_utf8(harness.out).unwrap();

    insta::assert_snapshot!(path, output);
}
