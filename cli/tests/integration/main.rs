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
