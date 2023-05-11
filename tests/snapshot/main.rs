use serde::Deserialize;
use std::{
    ffi::OsStr,
    fs::File,
    io::{BufRead, BufReader},
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

#[test_resources("tests/snapshot/inputs/pretty/*.ncl")]
fn check_pretty_print_snapshots(file: &str) {
    let file = TestFile::from_project_path(file);

    let snapshot = NickelInvocation::new()
        .subcommand("pprint-ast")
        .file(&file)
        .snapshot_stdout();

    insta::assert_snapshot!(file.prefixed_test_name("pretty"), snapshot);
}

#[test_resources("tests/snapshot/inputs/export/*.ncl")]
fn check_export_snapshots(path: &str) {
    let file = TestFile::from_project_path(path);

    let TestCase { annotation, .. } = read_test_case(path).expect("Failed to read test case");

    let invocation = NickelInvocation::new().args(annotation.command).file(&file);

    match annotation.capture {
        SnapshotCapture::Stderr => {
            let err = invocation.snapshot_stderr();
            assert_snapshot_filtered!(file.prefixed_test_name("export_stderr"), err);
        }
        SnapshotCapture::Stdout => {
            let out = invocation.snapshot_stdout();
            assert_snapshot_filtered!(file.prefixed_test_name("export_stdout"), out);
        }
        SnapshotCapture::All => {
            let (out, err) = invocation.snapshot();
            assert_snapshot_filtered!(file.prefixed_test_name("export_stdout"), out);
            assert_snapshot_filtered!(file.prefixed_test_name("export_stderr"), err);
        }
    }
}

#[test_resources("tests/snapshot/inputs/errors/*.ncl")]
fn check_error_snapshots(file: &str) {
    let file = TestFile::from_project_path(file);

    let snapshot = NickelInvocation::new().file(&file).snapshot_stderr();
    assert_snapshot_filtered!(file.prefixed_test_name("error"), snapshot);
}

#[test_resources("tests/snapshot/inputs/docs/*.ncl")]
fn check_doc_stdout_snapshots(file: &str) {
    let file = TestFile::from_project_path(file);

    let snapshot = NickelInvocation::new()
        .subcommand("doc")
        .file(&file)
        .args(["--stdout"])
        .snapshot_stdout();

    insta::assert_snapshot!(file.prefixed_test_name("doc_stdout"), snapshot);
}

#[test_resources("tests/snapshot/inputs/docs/*.ncl")]
fn check_doc_stderr_snapshots(file: &str) {
    let file = TestFile::from_project_path(file);

    let snapshot = NickelInvocation::new()
        .subcommand("doc")
        .file(&file)
        .args(["--stdout"])
        .snapshot_stderr();

    assert_snapshot_filtered!(file.prefixed_test_name("doc_stderr"), snapshot);
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
        format!("{prefix}_{file_name}")
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

    fn subcommand<S: AsRef<OsStr>>(mut self, s: S) -> Self {
        self.cmd.arg(s);
        self
    }

    fn file(mut self, f: &TestFile) -> Self {
        self.cmd.args(["-f", f.as_nickel_argument()]);
        self
    }

    fn args<I, S>(mut self, args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        self.cmd.args(args);
        self
    }

    fn run(mut self) -> Output {
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

pub struct TestCase<Annot> {
    annotation: Annot,
    program: String,
}

#[derive(Deserialize)]
struct SnapshotAnnotation {
    capture: SnapshotCapture,
    command: Vec<String>,
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

#[derive(Debug)]
enum AnnotatedProgramReadError {
    MissingAnnotation,
}

fn read_test_case(path: &str) -> Result<TestCase<SnapshotAnnotation>, AnnotatedProgramReadError> {
    let path = {
        let proj_root = env!("CARGO_MANIFEST_DIR");
        PathBuf::from(proj_root).join(path)
    };

    let file = File::open(path).expect("Failed to open file");
    let reader = BufReader::new(file);

    let mut lines = reader.lines();

    let mut annotation = String::new();
    let mut program = String::new();

    loop {
        let line = lines
            .next()
            .expect("Unexpected end of test file")
            .expect("Error reading line");
        if line.starts_with('#') {
            let annot_line = if line.len() > 1 { &line[2..] } else { "" };
            annotation.push_str(annot_line);
            annotation.push('\n');
        } else {
            // we've already consumed the line in order to check the first char
            // so we need to add it to the program string.
            program.push_str(&line);
            program.push('\n');
            break;
        }
    }

    if annotation.is_empty() {
        return Err(AnnotatedProgramReadError::MissingAnnotation);
    }

    for line in lines {
        let line = line.expect("Error reading line");
        program.push_str(&line);
        program.push('\n');
    }

    let annotation: SnapshotAnnotation =
        toml::from_str(annotation.as_str()).expect("Failed to parse expectation toml");

    Ok(TestCase {
        annotation,
        program,
    })
}
