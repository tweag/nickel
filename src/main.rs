mod eval;
mod identifier;
mod label;
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

    match parser::grammar::TermParser::new().parse(&buffer) {
        Ok(parsed) => {
            println!("Parsed term {:?}", parsed);
            println!("Evaluated term {:?}", eval(parsed));
        }
        Err(error) => {
            println!("Error: {:?}", error);
            panic!("It didnt parsed correctly");
        }
    }
}
