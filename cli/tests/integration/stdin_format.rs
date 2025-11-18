use std::{
    io::Write,
    process::{Command, Stdio},
};

struct ProcessOutputs {
    stderr: String,
    stdout: String,
}

fn run_input_from_stdin(args: Vec<&str>, input: &str) -> ProcessOutputs {
    let nickel_bin = env!("CARGO_BIN_EXE_nickel");
    let mut nickel = Command::new(nickel_bin)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Nickel should be runnable");

    let stdin = nickel.stdin.as_mut().unwrap();
    stdin
        .write_all(input.as_bytes())
        .expect("Could not write to Nickel's stdin");

    let output = nickel
        .wait_with_output()
        .expect("couldn't retrieve stdout handle to Nickel");

    ProcessOutputs {
        stderr: String::from_utf8(output.stderr)
            .expect("The result of Nickel should be valid utf8"),
        stdout: String::from_utf8(output.stdout)
            .expect("The result of Nickel should be valid utf8"),
    }
}

#[test]
fn evaluates_json_from_stdin() {
    let output = run_input_from_stdin(
        vec!["eval", "--stdin-format", "json"],
        "{ \"foo\": \"hello\", \"bar\": 123 }",
    );
    assert_eq!(output.stderr, "");
    assert_eq!(output.stdout, "{ bar = 123, foo = \"hello\", }\n");
}

#[test]
fn evaluates_yaml_from_stdin() {
    let output = run_input_from_stdin(
        vec!["eval", "--stdin-format", "yaml"],
        "foo: hello\nbar: 123",
    );
    assert_eq!(output.stderr, "");
    assert_eq!(output.stdout, "{ bar = 123, foo = \"hello\", }\n");
}

#[test]
fn evaluates_toml_from_stdin() {
    let output = run_input_from_stdin(
        vec!["eval", "--stdin-format", "toml"],
        "foo = \"hello\"\nbar = 123",
    );
    assert_eq!(output.stderr, "");
    assert_eq!(output.stdout, "{ bar = 123, foo = \"hello\", }\n");
}

#[test]
fn evaluates_nickel_from_stdin() {
    let output = run_input_from_stdin(
        vec!["eval", "--stdin-format", "nickel"],
        "{ foo = \"hello\", bar = 123 }",
    );
    assert_eq!(output.stderr, "");
    assert_eq!(output.stdout, "{ bar = 123, foo = \"hello\", }\n");
}

#[test]
fn evaluates_text_from_stdin() {
    let output = run_input_from_stdin(vec!["eval", "--stdin-format", "text"], "hello");
    assert_eq!(output.stderr, "");
    assert_eq!(output.stdout, "\"hello\"\n");
}

#[cfg(feature = "nix-experimental")]
#[test]
fn evaluates_nix_from_stdin() {
    let output = run_input_from_stdin(
        vec!["eval", "--stdin-format", "nix"],
        "{\n  foo = \"hello\";\n  bar = 123;\n}",
    );
    assert_eq!(output.stderr, "");
    assert_eq!(output.stdout, "{ bar = 123, foo = \"hello\", }\n");
}

#[test]
fn export_supports_stdin_format_flag() {
    let output = run_input_from_stdin(
        vec!["export", "--stdin-format", "json", "-f", "toml"],
        "{ \"foo\": \"hello\", \"bar\": 123 }",
    );
    assert_eq!(output.stderr, "");
    assert_eq!(output.stdout, "bar = 123\nfoo = \"hello\"\n");
}

#[test]
fn query_supports_stdin_format_flag() {
    let output = run_input_from_stdin(
        vec!["query", "--stdin-format", "json", "--field", "bar"],
        "{ \"foo\": \"hello\", \"bar\": { \"a\": 1, \"b\": 2 }}",
    );
    // stderr will output some warnings because there's no actual metadata
    // in this case. That's fine, so I'm ignoring it and only checking that
    // available fields are listed in stdout.
    assert_eq!(
        output.stdout,
        "\n\u{1b}[4mAvailable fields\u{1b}[0m\n\u{1b}[38;5;240m•\u{1b}[39m a\n\u{1b}[38;5;240m•\u{1b}[39m b\n"
    );
}
