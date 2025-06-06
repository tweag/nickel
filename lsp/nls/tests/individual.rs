use lsp_harness::{file_url_from_path, TestHarness};
use serde_json::json;

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
