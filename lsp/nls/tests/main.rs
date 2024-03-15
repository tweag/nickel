use nickel_lang_utils::project_root::project_root;
use test_generator::test_resources;

use lsp_harness::{TestFixture, TestHarness};

#[test_resources("lsp/nls/tests/inputs/*.ncl")]
fn check_snapshots(path: &str) {
    let _ = env_logger::try_init();

    let full_path = project_root().join(path);

    let contents = std::fs::read_to_string(full_path).unwrap();
    let fixture = TestFixture::parse(&contents).unwrap();
    let mut harness = TestHarness::new();

    harness.prepare_files(&fixture);
    for req in fixture.reqs {
        harness.request_dyn(req);
    }

    harness.drain_diagnostics(fixture.expected_diags.iter().cloned());
    let output = String::from_utf8(harness.out).unwrap();

    insta::assert_snapshot!(path, output);
}

#[test]
fn refresh_missing_imports() {
    let _ = env_logger::try_init();
    let mut harness = TestHarness::new();

    let url = |s: &str| lsp_types::Url::from_file_path(s).unwrap();
    harness.send_file(url("/test.ncl"), "import \"dep.ncl\"");
    let diags = harness.wait_for_diagnostics().diagnostics;
    assert_eq!(2, diags.len());
    assert!(diags[0].message.contains("import of dep.ncl failed"));

    // We expect another copy of the diagnostics (coming from background eval).
    let diags = harness.wait_for_diagnostics().diagnostics;
    assert_eq!(2, diags.len());
    assert!(diags[0].message.contains("import of dep.ncl failed"));

    // Now provide the import.
    harness.send_file(url("/dep.ncl"), "42");

    // Check that we get back clean diagnostics for both files.
    // (LSP doesn't define the order, but we happen to know it)
    // Loop because we can get back the diagnostics twice from each
    // file (once from synchronous typechecking, once from eval in the background).
    loop {
        let diags = harness.wait_for_diagnostics();
        assert!(diags.diagnostics.is_empty());
        if diags.uri.path() == "/dep.ncl" {
            break;
        }
    }

    loop {
        let diags = harness.wait_for_diagnostics();
        assert!(diags.diagnostics.is_empty());
        if diags.uri.path() == "/test.ncl" {
            break;
        }
    }
}
