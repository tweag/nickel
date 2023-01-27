#![cfg(feature = "nix")]

use nickel_lang::term::Term;
use nickel_lang_utilities::eval;

fn run(path: &str) {
    eval(format!(
        "let t = import \"{}/tests/nix/{}\" in array.fold (fun x acc => acc && x) true t",
        env!("CARGO_MANIFEST_DIR"),
        path
    ))
    .map(|term| {
        assert_eq!(term, Term::Bool(true), "error in test {}", path,);
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
