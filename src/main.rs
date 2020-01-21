mod eval;
mod identifier;
mod label;
mod operation;
mod parser;
mod program;
mod stack;
mod term;
mod typecheck;
mod types;

use crate::program::Program;

extern crate either;

fn main() {
    let mut p = Program::new_from_stdin();
    match p.eval() {
        Ok(t) => println!("Evaluation finished with result:\n{:?}", t),
        Err(s) => println!("Evaluation didn't finished, found error:\n{}", s),
    }
}
