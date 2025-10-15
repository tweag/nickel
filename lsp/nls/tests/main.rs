use lsp_types::Url;
use nickel_lang_utils::project_root::project_root;
use pretty_assertions::assert_eq;
use serde_json::json;
use test_generator::test_resources;

use lsp_harness::{file_url_from_path, TestFixture, TestHarness};

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

    for url in fixture.expected_diags {
        harness.get_eval_diagnostics(url);
    }
    let output = String::from_utf8(harness.out).unwrap();

    insta::with_settings!(
        {filters => vec![
            ("file:///C:", "file://"),
            ("C:\\\\\\\\", "/"), // This just matches "C:\\" but needs several of layers of escaping
        ]},
        {
            insta::assert_snapshot!(path, output);
        }
    );
}

// Tests the test fixtures in `inputs-in-place`.
//
// Whereas the tests in `inputs` are one file per test (and use annotations
// to tell the LSP about multiple "virtual" files), the text fixtures in
// `inputs-in-place` are one *directory* per test. We load each file in that
// directory and then ask for diagnostics.
//
// This allows for testing things that rely on configuration files, because
// these inputs-in-place test are provided to the LSP along with their actual
// physical filesystem location, which allows the LSP to look for nearby
// configuration files.
#[test_resources("lsp/nls/tests/inputs-in-place/*")]
fn check_snapshots_in_place(path: &str) {
    let _ = env_logger::try_init();

    let root = project_root();
    let full_path = root.join(path);

    let mut fixture = TestFixture::default();
    let mut file_paths = Vec::new();
    for file in std::fs::read_dir(&full_path).unwrap() {
        // We don't currently support subdirectories, so assume everything is just a file.
        let file = file.unwrap();
        let contents = std::fs::read_to_string(file.path()).unwrap();
        let url = Url::from_file_path(file.path()).unwrap();
        fixture.files.push(lsp_harness::TestFile {
            uri: url.clone(),
            contents,
        });
        file_paths.push(file.path());
    }

    // Make sure we ask for diagnostics in a predictable order.
    file_paths.sort();

    let mut harness = TestHarness::new();

    harness.prepare_files(&fixture);
    for diag_path in file_paths {
        // Put the file name before the diagnostics in the output, so the snapshot is
        // easier to read.
        harness.out.extend_from_slice(
            diag_path
                .strip_prefix(&root)
                .unwrap()
                .display()
                .to_string()
                .as_bytes(),
        );
        harness.out.extend_from_slice(b":\n");

        harness.get_eval_diagnostics(Url::from_file_path(diag_path).unwrap());
    }
    let output = String::from_utf8(harness.out).unwrap();

    insta::with_settings!(
        {filters => vec![
            ("file:///C:", "file://"),
            ("C:\\\\\\\\", "/"), // This just matches "C:\\" but needs several of layers of escaping
            ("\\\\", "/"),
        ]},
        {
            insta::assert_snapshot!(path, output);
        }
    );
}

#[test]
fn refresh_missing_imports() {
    let _ = env_logger::try_init();
    let mut harness = TestHarness::new();

    let test_uri = file_url_from_path("/test.ncl").unwrap();
    harness.send_file(test_uri.clone(), "import \"dep.ncl\"");
    let diags = harness.wait_for_diagnostics().diagnostics;
    assert_eq!(2, diags.len());
    assert!(diags[0].message.contains("import of dep.ncl failed"));

    // Now provide the import.
    let dep_uri = file_url_from_path("/dep.ncl").unwrap();
    harness.send_file(dep_uri.clone(), "42");

    // Check that we get back clean diagnostics for both files.
    // (LSP doesn't define the order, but we happen to know it)
    let diags = harness.wait_for_diagnostics();
    assert!(diags.diagnostics.is_empty());
    assert_eq!(diags.uri, dep_uri);

    let diags = harness.wait_for_diagnostics();
    assert!(diags.diagnostics.is_empty());
    assert_eq!(diags.uri, test_uri);
}

#[test]
fn apply_client_options() {
    let _ = env_logger::try_init();
    let lsp_options = json!({
        "eval_config": {
            "eval_limits": {
                "recursion_limit": 1
            }
        }
    });
    let mut harness = TestHarness::new_with_options(Some(lsp_options));
    let test_uri = file_url_from_path("/test.ncl").unwrap();
    harness.send_file(
        test_uri,
        "{ C = fun n => if n == 0 then String else C (n - 1), res = 2 | C 5 }",
    );

    // Typecheck diagnostics. Empty because there's nothing to error on
    let diags = harness.wait_for_diagnostics();
    assert!(diags.diagnostics.is_empty());

    // Evaluator diagnostics.
    // These shouldn't be empty (because `C 5 == String` so `2 | C 5` is a contract
    // violation), but they are because `recursion_limit` is too low for the evaluator to be able
    // to compute that.
    let diags = harness.wait_for_diagnostics();
    assert!(diags.diagnostics.is_empty());
}
