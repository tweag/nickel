use std::{
    io::Write,
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
fn format_creates_output_files() {
    test_creates_output_files(&["format"]);
}
