use std::{
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
};

use tempfile::tempdir;

fn test_creates_output_files(command: &[&str]) {
    let nickel_bin = env!("CARGO_BIN_EXE_nickel");
    let output = tempdir()
        .expect("should be able to make a temporary directory")
        .into_path()
        .join("output");
    let mut nickel = Command::new(nickel_bin)
        .args(command)
        .arg("-o")
        .arg(&output)
        .stdin(Stdio::piped())
        .spawn()
        .expect("Nickel should be runnable");
    let Some(mut stdin) = nickel.stdin.take() else {
        panic!("couldn't retrieve stdin handle to Nickel")
    };
    stdin
        .write_all(b"{foo=1}")
        .expect("writing into Nickel stdin should work");
    drop(stdin);

    nickel.wait().expect("Nickel should exit successfully");
    assert!(output.exists());
}

#[test]
fn export_creates_output_files() {
    test_creates_output_files(&["export"]);
}

#[test]
fn doc_creates_output_files() {
    test_creates_output_files(&["doc", "--format", "json"]);
}

#[test]
fn automatic_color_on_non_tty() {
    let nickel_bin = env!("CARGO_BIN_EXE_nickel");
    let mut nickel = Command::new(nickel_bin)
        .args(["export"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Nickel should be runnable");
    let mut stdin = nickel
        .stdin
        .take()
        .expect("couldn't retrieve stdin handle to Nickel");
    stdin
        .write_all(b"1+{}")
        .expect("writing into Nickel stdin should work");
    drop(stdin);
    let output = nickel
        .wait_with_output()
        .expect("couldn't retrieve stdout handle to Nickel");
    for raw_stream in [output.stdout, output.stderr] {
        let stream =
            String::from_utf8(raw_stream).expect("The result of Nickel should be valid utf8");
        // The prefix used for the ANSI escape codes used for terminal colors
        let ansi_code_prefix = "\x1b[";
        assert_eq!(
            stream.find(ansi_code_prefix),
            None,
            "The Nickel output shouldn't be colorized when stdout isn't a tty"
        );
    }
}

#[test]
// This test didn't fit the snapshot test specification very well. While it can be encoded as
// snapshot test, it didn't work on the CI (where the working directory doesn't seem to be properly
// set for some reason), so we are using a manual test for now.
fn merge_mixed_formats() {
    let mut input_base_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    // We don't push "tests/integration/inputs" at once because that wouldn't work on Windows.
    input_base_path.push("tests");
    input_base_path.push("integration");
    input_base_path.push("inputs");

    let nickel_bin = env!("CARGO_BIN_EXE_nickel");

    let nickel = Command::new(nickel_bin)
        .arg("export")
        .args([
            input_base_path.join("mixed.ncl").into_os_string(),
            input_base_path.join("mixed.json").into_os_string(),
            input_base_path.join("mixed.toml").into_os_string(),
        ])
        .arg("--apply-contract")
        .arg(input_base_path.join("mixed_contract.ncl").into_os_string())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Nickel should be runnable");

    let output = nickel
        .wait_with_output()
        .expect("couldn't retrieve stdout handle to Nickel");

    let stderr_output =
        String::from_utf8(output.stderr).expect("The result of Nickel should be valid utf8");
    let stdout_output =
        String::from_utf8(output.stdout).expect("The result of Nickel should be valid utf8");

    assert_eq!(stderr_output, "");
    assert_eq!(
        stdout_output,
        "\
{
  \"bar\": 123,
  \"extra\": {
    \"json\": true,
    \"nickel\": true,
    \"subfield\": \"here\",
    \"toml\": true
  },
  \"foo\": \"hello\"
}
"
    );
}
