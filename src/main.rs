mod eval;
mod identifier;
mod parser;
mod term;

use eval::eval;
use std::io::{self, Read};

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate lalrpop_util;

fn main() {
    let mut buffer = String::new();
    io::stdin()
        .read_to_string(&mut buffer)
        .expect("This main doesnt handle Err for now.");

    if let Ok(parsed) = parser::grammar::TermParser::new().parse(&buffer) {
        println!("Parsed term {:?}", parsed);
        println!("Evaluated term {:?}", eval(parsed));
    }
}
