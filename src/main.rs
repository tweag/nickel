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
    let mut p = Program::new_from_stdin();
    match p.eval() {
        Ok(t) => println!("Ok: {:?}", t),
        Err(s) => print!("{}", s),
    }
}
