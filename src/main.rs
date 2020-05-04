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
    let mut args = std::env::args();
    let command = args.next();

    let result = match args.next() {
        Some(s) if &s == "-h" || &s == "--help"  => {
            print_help(&command.unwrap_or(String::from("nickel")));
            return;
        }
        Some(file) => {
            match Program::new_from_file(std::path::Path::new(file.as_str())) {
                Ok(mut p) => p.eval(),
                Err(err) => {
                    eprintln!("Cannot read file {}: {}", file, err);
                    std::process::exit(1)
                }
            }
        }
        None => Program::new_from_stdin().eval()
    };

    match result {
        Ok(t) => println!("Evaluation finished with result:\n{:?}", t),
        Err(s) => println!("Evaluation didn't finished, found error:\n{}", s),
    }
}

fn print_help(command: &String) {
    println!("Usage: {} [FILE] [-h | --help]", command);
    println!("Execute a Nickel program. If no argument is provided, the program is read from the \
             standard input.");
}
