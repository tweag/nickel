use std::collections::{hash_map::Entry, HashMap};

use assert_cmd::cargo::CommandCargoExt;
use lsp_types::request::{
    Completion, Formatting, GotoDefinition, HoverRequest, Request as LspRequest,
};
use nickel_lang_utils::project_root::project_root;
use test_generator::test_resources;

use lsp_harness::{LspDebug, Request, Server, TestFixture};

struct TestHarness {
    srv: Server,
    out: Vec<u8>,
}

impl TestHarness {
    fn new() -> Self {
        let cmd = std::process::Command::cargo_bin("nls").unwrap();
        let srv = Server::new(cmd).unwrap();
        Self {
            srv,
            out: Vec::new(),
        }
    }

    fn request<T: LspRequest>(&mut self, params: T::Params)
    where
        T::Result: LspDebug,
    {
        let result = self.srv.send_request::<T>(params).unwrap();
        result.debug(&mut self.out).unwrap();
        self.out.push(b'\n');
    }

    fn request_dyn(&mut self, req: Request) {
        match req {
            Request::GotoDefinition(d) => self.request::<GotoDefinition>(d),
            Request::Completion(c) => self.request::<Completion>(c),
            Request::Formatting(f) => self.request::<Formatting>(f),
            Request::Hover(h) => self.request::<HoverRequest>(h),
        }
    }

    // For debug purposes, drain and print notifications.
    fn drain_notifications(&mut self) {
        // FIXME: nls doesn't report progress, so we have no way to check whether
        // it's finished sending notifications. We just retrieve any that we've already
        // received.
        // We should also have a better format for printing diagnostics and other
        // notifications.
        for msg in self.srv.pending_notifications() {
            eprintln!("{msg:?}");
        }
    }
}

#[test_resources("lsp/nls/tests/inputs/*.ncl")]
fn check_snapshots(path: &str) {
    let _ = env_logger::try_init();

    let full_path = project_root().join(path);

    let contents = std::fs::read_to_string(&full_path).unwrap();
    let fixture = TestFixture::parse(&contents).unwrap();
    let mut harness = TestHarness::new();

    let mut file_versions = HashMap::new();

    if fixture.files.is_empty() {
        panic!("no files");
    }

    for file in fixture.files {
        match file_versions.entry(file.uri.clone()) {
            Entry::Occupied(mut version) => {
                *version.get_mut() += 1;
                harness
                    .srv
                    .replace_file(file.uri, *version.get(), &file.contents)
                    .unwrap();
            }
            Entry::Vacant(entry) => {
                harness.srv.send_file(file.uri, &file.contents).unwrap();
                entry.insert(1);
            }
        }
    }
    for req in fixture.reqs {
        harness.request_dyn(req);
    }
    harness.drain_notifications();
    let output = String::from_utf8(harness.out).unwrap();

    insta::assert_snapshot!(path, output);
}
