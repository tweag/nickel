mod continuation;
mod eval;
mod identifier;
mod label;
mod parser;
mod program;
mod stack;
mod term;

use program::Program;

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate lalrpop_util;

fn main() {
    let mut p = Program::new_from_stdin();
    match p.eval() {
        Ok(s) => println!("{:?}", s),
        Err(s) => panic!(s),
    }
}
