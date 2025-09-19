use lsp_types::Url;
use nickel_lang_utils::project_root::project_root;
use pretty_assertions::assert_eq;
use serde_json::json;
use tempfile::TempDir;
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

#[test]
fn local_config() {
    let dir = TempDir::new().unwrap();
    std::fs::write(
        dir.path().join("nls.ncl"),
        r#"{ "**/shallow*.ncl" = { eval_config.eval_limits.recursion_limit = 1 } }"#,
    )
    .unwrap();

    let mk_path = |s: &str| Url::from_file_path(dir.path().join(s)).unwrap();

    // Here's a little snippet that fails, but succeeds if the depth is restricted to 1.
    let code = "{ C = fun n => if n == 0 then String else C (n - 1), res = 2 | C 5 }";
    let _ = env_logger::try_init();
    let mut harness = TestHarness::new_with_options(None);

    harness.send_file(mk_path("shallow.ncl"), code);
    let diags = harness.wait_for_diagnostics();
    assert!(diags.diagnostics.is_empty());
    let diags = harness.wait_for_diagnostics();
    assert!(diags.diagnostics.is_empty());

    harness.send_file(mk_path("subdir/shallow_foo.ncl"), code);
    let diags = harness.wait_for_diagnostics();
    assert!(diags.diagnostics.is_empty());
    let diags = harness.wait_for_diagnostics();
    assert!(diags.diagnostics.is_empty());

    harness.send_file(mk_path("deep.ncl"), code);
    let diags = harness.wait_for_diagnostics();
    assert!(diags.diagnostics.is_empty());
    let diags = harness.wait_for_diagnostics();
    // This time we should get a failure from the background diagnostics.
    assert!(!diags.diagnostics.is_empty());

    // If we modify the config file, it should get picked up next time
    // the code is sent. The file watcher has some debounce, so
    // we need to wait. This test might have reliability issues because
    // of the timing (and also the file watching has known issues in
    // certain circumstances: https://docs.rs/notify/latest/notify/#known-problems
    std::fs::write(
        dir.path().join("nls.ncl"),
        r#"{ "**/shallow*.ncl" = { eval_config.eval_limits.recursion_limit = 5 } }"#,
    )
    .unwrap();
    std::thread::sleep(std::time::Duration::from_secs(1));

    harness.send_file(mk_path("shallow.ncl"), code);
    let diags = harness.wait_for_diagnostics();
    assert!(diags.diagnostics.is_empty());
    let diags = harness.wait_for_diagnostics();
    assert!(!diags.diagnostics.is_empty());
}
