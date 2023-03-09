#![cfg(feature = "nix")]

use nickel_lang::term::Term;
use nickel_lang_utilities::eval;

fn run(path: &str) {
    eval(format!(
        "import \"{}/tests/nix/{path}\" |> array.all function.id",
        env!("CARGO_MANIFEST_DIR"),
    ))
    .map(|term| {
        assert_eq!(term, Term::Bool(true), "error in test {path}");
    })
    .unwrap();
}

#[test]
fn basics_nix() {
    run("basics.nix");
}

#[test]
fn lets_nix() {
    run("lets.nix");
}

#[test]
fn records_nix() {
    run("records.nix");
}

#[test]
fn with_nix() {
    run("with.nix");
}
