mod eval;
mod identifier;
mod parser;
mod term;

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate lalrpop_util;

fn main() {
    println!("{:?}", parser::grammar::TermParser::new().parse("22"))
}
