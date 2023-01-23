use std::{
    ffi::OsStr,
    path::PathBuf,
    process::{Command, Output},
};
use test_generator::test_resources;

#[test_resources("tests/snapshot/inputs/pretty/*.ncl")]
fn check_pretty_print_snapshots(file: &str) {
    let file = TestFile::from_project_path(file);

    let snapshot = NickelInvocation::new()
        .subcommand("pprint-ast")
        .file(&file)
        .snapshot_stdout();

    insta::assert_snapshot!(file.prefixed_test_name("pretty"), snapshot)
}

#[test_resources("tests/snapshot/inputs/export/*.ncl")]
fn check_export_stdout_snapshots(file: &str) {
    let file = TestFile::from_project_path(file);

    let snapshot = NickelInvocation::new()
        .subcommand("export")
        .file(&file)
        .snapshot_stdout();

    insta::assert_snapshot!(file.prefixed_test_name("export_stdout"), snapshot);
}

#[test_resources("tests/snapshot/inputs/export/*.ncl")]
fn check_export_stderr_snapshots(file: &str) {
    let file = TestFile::from_project_path(file);

    let snapshot = NickelInvocation::new()
        .subcommand("export")
        .file(&file)
        .snapshot_stderr();

    insta::assert_snapshot!(file.prefixed_test_name("export_stderr"), snapshot);
}

#[test_resources("tests/snapshot/inputs/errors/*.ncl")]
fn check_error_snapshots(file: &str) {
    let file = TestFile::from_project_path(file);

    let snapshot = NickelInvocation::new().file(&file).snapshot_stderr();

    insta::with_settings!({filters => vec![
        // Since error output includes fully-qualified paths to the source file
        // we need to replace those with something static to avoid snapshots
        // differing across machines.
        (r"(?:/.+/tests/snapshot/inputs)", "[INPUTS_PATH]")
    ]},
    {
        insta::assert_snapshot!(file.prefixed_test_name("error"), snapshot)
    });
}

struct TestFile {
    path_buf: PathBuf,
}

impl TestFile {
    fn from_project_path(file: &str) -> Self {
        let mut path_buf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        path_buf.push(file);
        Self { path_buf }
    }

    fn prefixed_test_name(&self, prefix: &str) -> String {
        let file_name = self
            .path_buf
            .as_path()
            .file_name()
            .and_then(|f| f.to_str())
            .expect("Could not extract file name");
        format!("{}_{}", prefix, file_name)
    }

    fn as_nickel_argument(&self) -> &str {
        self.path_buf
            .to_str()
            .expect("Could not convert path arg to str")
    }
}

struct NickelInvocation {
    cmd: Command,
}

impl NickelInvocation {
    fn new() -> Self {
        let nickel_loc = env!("CARGO_BIN_EXE_nickel");
        let mut cmd = Command::new(nickel_loc);
        cmd.args(["--color", "never"]);
        Self { cmd }
    }

    fn subcommand<S: AsRef<OsStr>>(&mut self, s: S) -> &mut Self {
        self.cmd.arg(s);
        self
    }

    fn file(&mut self, f: &TestFile) -> &mut Self {
        self.cmd.args(["-f", f.as_nickel_argument()]);
        self
    }

    fn run(&mut self) -> Output {
        self.cmd.output().expect("Should be able to capture output")
    }

    fn snapshot_stderr(&mut self) -> String {
        String::from_utf8(self.run().stderr).expect("Output should be utf8")
    }

    fn snapshot_stdout(&mut self) -> String {
        String::from_utf8(self.run().stdout).expect("Output should be utf8")
    }
}
