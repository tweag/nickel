use nickel::program::Program;
use nickel::term::Term;
use std::ffi::{OsStr, OsString};
use std::fs::read_dir;
use std::path::PathBuf;
use std::thread;

// By default, tests are run with 2MB stack size, which can overflow in debug mode. We run the
// tests with an increased stack size.
const STACK_SIZE: usize = 4 * 1024 * 1024;

fn run(path: impl Into<OsString>) {
    let path = path.into();
    let mut p = Program::new_from_file(path.clone()).expect("could not load file as a program");
    assert_eq!(
        p.eval(),
        Ok(Term::Bool(true)),
        "error evaluating {}",
        path.to_string_lossy(),
    );
}

#[test]
fn tests() {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("tests/pass");

    let children: Vec<_> = read_dir(d)
        .unwrap()
        .into_iter()
        .map(|file| {
            let file = file.unwrap();
            let path = file.path();
            let name = String::from(
                path.file_name()
                    .map(OsStr::to_string_lossy)
                    .unwrap_or(path.to_string_lossy()),
            );
            thread::Builder::new()
                .name(format!("pass {}", name))
                .stack_size(STACK_SIZE)
                .spawn(|| run(path))
                .unwrap()
        })
        .collect();

    for thread in children {
        thread.join().unwrap();
    }
}
