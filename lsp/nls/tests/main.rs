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

    harness.drain_diagnostics(fixture.expected_diags.iter().cloned());
    let output = String::from_utf8(harness.out).unwrap();

    insta::with_settings!(
        {filters => vec![("file:///C:", "file://")]},
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

    // We expect another copy of the diagnostics (coming from background eval).
    let diags = harness.wait_for_diagnostics().diagnostics;
    assert_eq!(2, diags.len());
    assert!(diags[0].message.contains("import of dep.ncl failed"));

    // Now provide the import.
    let dep_uri = file_url_from_path("/dep.ncl").unwrap();
    harness.send_file(dep_uri.clone(), "42");

    // Check that we get back clean diagnostics for both files.
    // (LSP doesn't define the order, but we happen to know it)
    // Loop because we can get back the diagnostics twice from each
    // file (once from synchronous typechecking, once from eval in the background).
    loop {
        let diags = harness.wait_for_diagnostics();
        assert!(diags.diagnostics.is_empty());
        if diags.uri == dep_uri {
            break;
        }
    }

    loop {
        let diags = harness.wait_for_diagnostics();
        assert!(diags.diagnostics.is_empty());
        if diags.uri == test_uri {
            break;
        }
    }
}

// Regression test for #1943, in which the cache was not properly reset
// for imports that changed state from parseable to not-parseable.
#[test]
fn reload_broken_imports() {
    let _ = env_logger::try_init();
    let mut harness = TestHarness::new();

    let dep_uri = file_url_from_path("/dep.ncl").unwrap();
    harness.send_file(dep_uri.clone(), "{ x }");

    let test_uri = file_url_from_path("/test.ncl").unwrap();
    harness.send_file(test_uri.clone(), "import \"dep.ncl\"");
    let diags = harness.wait_for_diagnostics();

    assert_eq!(diags.uri, dep_uri);
    assert!(diags.diagnostics.is_empty());

    let diags = harness.wait_for_diagnostics();
    assert_eq!(diags.uri, test_uri);
    assert!(diags.diagnostics.is_empty());

    // We expect two more diagnostics coming from background eval.
    let _diags = harness.wait_for_diagnostics();
    let _diags = harness.wait_for_diagnostics();

    // Introduce an error in the import.
    harness.send_file(dep_uri.clone(), "{ `x = 1 }");

    // Check that we get back clean diagnostics for both files.
    // (LSP doesn't define the order, but we happen to know it)
    // Loop because we can get back the diagnostics twice from each
    // file (once from synchronous typechecking, once from eval in the background).
    loop {
        let diags = harness.wait_for_diagnostics();
        if diags.uri == dep_uri {
            assert_eq!(diags.diagnostics[0].message, "unexpected token");
            break;
        }
    }

    loop {
        let diags = harness.wait_for_diagnostics();
        if diags.uri == test_uri {
            assert_eq!(diags.diagnostics[0].message, "unexpected token");
            break;
        }
    }
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
