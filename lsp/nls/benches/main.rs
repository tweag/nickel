use std::{
    path::{Path, PathBuf},
    time::Duration,
};

use criterion::{criterion_group, criterion_main, Criterion};

use glob::glob;
use lsp_harness::{TestFixture, TestHarness};
use nickel_lang_core::cache;
use nickel_lang_utils::project_root::project_root;

criterion_main!(test_request_benches, test_init_benches);

criterion_group! {
    name = test_request_benches;
    // There are a lot of these and none is particularly high-value, so turn down
    // the default benchmark time.
    config = Criterion::default()
        .measurement_time(Duration::from_secs(1))
        .warm_up_time(Duration::from_secs_f64(0.5));
    targets = test_requests
}

criterion_group! {
    name = test_init_benches;
    // There are a lot of these and none is particularly high-value, so turn down
    // the default benchmark time.
    config = Criterion::default()
        .measurement_time(Duration::from_secs(1))
        .warm_up_time(Duration::from_secs_f64(0.5));
    targets = test_init
}

fn friendly_path(path: &Path) -> String {
    let path = cache::normalize_path(path).unwrap();
    let components: Vec<_> = path.components().rev().take(3).collect();
    let path: PathBuf = components.into_iter().rev().collect();
    path.to_str().unwrap().to_owned()
}

fn test_requests(c: &mut Criterion) {
    let files = project_root()
        .join("lsp/nls/tests/inputs/*.ncl")
        .to_str()
        .unwrap()
        .to_owned();
    for f in glob(&files).unwrap() {
        benchmark_one_test(c, f.unwrap().to_str().unwrap());
    }
}

fn benchmark_one_test(c: &mut Criterion, path: &str) {
    let full_path = project_root().join(path);
    let contents = std::fs::read_to_string(&full_path).unwrap();
    let fixture = TestFixture::parse(&contents).unwrap();
    let mut harness = TestHarness::new();

    harness.prepare_files(&fixture);

    for (i, req) in fixture.reqs.iter().enumerate() {
        let path = friendly_path(&full_path);
        let name = format!("requests-{path}-{i:03}");
        c.bench_function(&name, |b| b.iter(|| harness.request_dyn(req.clone())));
    }
}

fn test_init(c: &mut Criterion) {
    let files = project_root()
        .join("lsp/nls/tests/inputs/*.ncl")
        .to_str()
        .unwrap()
        .to_owned();
    for f in glob(&files).unwrap() {
        benchmark_diagnostics(c, f.unwrap().to_str().unwrap());
    }
}

// Measure how long it takes from the time the file is sent to the LSP
// until its diagnostics are received.
fn benchmark_diagnostics(c: &mut Criterion, path: &str) {
    let full_path = project_root().join(path);
    let contents = std::fs::read_to_string(&full_path).unwrap();
    let fixture = TestFixture::parse(&contents).unwrap();

    for (i, f) in fixture.files.into_iter().enumerate() {
        let path = friendly_path(&full_path);
        let name = format!("init-diagnostics-{path}-{i:03}");
        c.bench_function(&name, |b| {
            let mut harness = TestHarness::new();
            b.iter(|| {
                harness.send_file(f.uri.clone(), &f.contents);
                loop {
                    // Check the uri of the diagnostics, because in
                    // the presence of imports we want to wait until
                    // the main file sends its diagnostics back.
                    let diags = harness.wait_for_diagnostics();
                    if diags.uri == f.uri {
                        break;
                    }
                }
            });
        });
    }
}
