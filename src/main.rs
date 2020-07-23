mod error;
mod eval;
mod identifier;
mod label;
mod merge;
mod operation;
mod parser;
mod position;
mod program;
mod stack;
mod term;
mod transformations;
mod typecheck;
mod types;

use crate::program::Program;

extern crate either;

fn main() {
    match Program::new_from_stdin() {
        Ok(mut p) => match p.eval() {
            Ok(t) => println!("Done: {:?}", t),
            Err(err) => p.report(&err),
        },
        Err(msg) => eprintln!("Error when reading the source: {}", msg),
    };
}
