use nickel_lang_utils::{
    annotated_test::{read_annotated_test_case, TestCase},
    project_root::project_root,
};
use serde::Deserialize;
use std::{
    ffi::{OsStr, OsString},
    path::PathBuf,
    process::{Command, Output},
};
use test_generator::test_resources;

macro_rules! assert_snapshot_filtered {
    { $name:expr, $snapshot:expr } => {
        insta::with_settings!({filters => vec![
            // Since error output includes fully-qualified paths to the source file
            // we need to replace those with something static to avoid snapshots
            // differing across machines.
            (r"(?:/.+/tests/snapshot/inputs)", "[INPUTS_PATH]")
        ]},
        {
            insta::assert_snapshot!($name, $snapshot);
        })
    }
}

#[test_resources("cli/tests/snapshot/inputs/**/*.ncl")]
fn check_snapshots(path: &str) {
    let file = TestFile::from_project_path(path);

    let TestCase { annotation, .. }: TestCase<SnapshotAnnotation> =
        read_annotated_test_case(path).expect("Failed to read test case");

    let subcommand = annotation
        .command
        .first()
        .map(|s| s.as_str())
        // TODO: er, maybe?
        .unwrap_or("no_subcommand")
        .to_string();

    // We set the `-f somefile.ncl` argument before the others, because it can't come after
    // customize mode's delimiter `--`.
    let invocation = NickelInvocation::new()
        .file(&file)
        .args(annotation.command)
        .extra_args(annotation.extra_args);

    match annotation.capture {
        SnapshotCapture::Stderr => {
            let err = invocation.snapshot_stderr();
            assert_snapshot_filtered!(
                file.prefixed_test_name(format!("{subcommand}_stderr").as_str()),
                err
            );
        }
        SnapshotCapture::Stdout => {
            let out = invocation.snapshot_stdout();
            assert_snapshot_filtered!(
                file.prefixed_test_name(format!("{subcommand}_stdout").as_str()),
                out
            );
        }
        SnapshotCapture::All => {
            let (out, err) = invocation.snapshot();
            assert_snapshot_filtered!(
                file.prefixed_test_name(format!("{subcommand}_stdout").as_str()),
                out
            );
            assert_snapshot_filtered!(
                file.prefixed_test_name(format!("{subcommand}_stderr").as_str()),
                err
            );
        }
    }
}

struct TestFile {
    path_buf: PathBuf,
}

impl TestFile {
    fn from_project_path(file: &str) -> Self {
        Self {
            path_buf: project_root().join(file),
        }
    }

    fn prefixed_test_name(&self, prefix: &str) -> String {
        let file_name = self
            .path_buf
            .as_path()
            .file_name()
            .and_then(|f| f.to_str())
            .expect("Could not extract file name");
        format!("{prefix}_{file_name}")
    }

    fn as_nickel_argument(&self) -> &OsStr {
        self.path_buf.as_os_str()
    }
}

struct NickelInvocation<'a> {
    cmd: Command,
    file: Option<&'a TestFile>,
    args: Vec<OsString>,
    extra_args: Vec<OsString>,
}

impl<'a> NickelInvocation<'a> {
    fn new() -> Self {
        let nickel_loc = env!("CARGO_BIN_EXE_nickel");
        let mut cmd = Command::new(nickel_loc);
        cmd.args(["--color", "never"]);
        Self {
            cmd,
            file: None,
            args: Vec::new(),
            extra_args: Vec::new(),
        }
    }

    fn file(mut self, f: &'a TestFile) -> Self {
        self.file = Some(f);
        self
    }

    fn args<I, S>(mut self, args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        self.args
            .extend(args.into_iter().map(|s| s.as_ref().into()));
        self
    }

    fn extra_args<I, S>(mut self, extra_args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        self.extra_args
            .extend(extra_args.into_iter().map(|s| s.as_ref().into()));
        self
    }

    fn run(mut self) -> Output {
        self.cmd.args(self.args);
        self.cmd
            .args(self.file.into_iter().map(|f| f.as_nickel_argument()));
        self.cmd.args(self.extra_args);
        self.cmd.output().expect("Should be able to capture output")
    }

    // TODO: named struct > tuple
    fn snapshot(self) -> (String, String) {
        let output = self.run();
        fn as_string(v: Vec<u8>) -> String {
            String::from_utf8(v).expect("Output should be utf8")
        }
        (as_string(output.stdout), as_string(output.stderr))
    }

    fn snapshot_stderr(self) -> String {
        String::from_utf8(self.run().stderr).expect("Output should be utf8")
    }

    fn snapshot_stdout(self) -> String {
        String::from_utf8(self.run().stdout).expect("Output should be utf8")
    }
}

#[derive(Deserialize)]
struct SnapshotAnnotation {
    capture: SnapshotCapture,
    command: Vec<String>,
    #[serde(default)]
    extra_args: Vec<String>,
}

#[derive(Deserialize)]
enum SnapshotCapture {
    #[serde(rename = "stderr")]
    Stderr,
    #[serde(rename = "stdout")]
    Stdout,
    #[serde(rename = "all")]
    All,
}
