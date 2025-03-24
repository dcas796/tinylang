#![feature(let_chains)]
#![feature(string_remove_matches)]

use std::fs::read;
use crate::interpreter::{Interpreter, InterpreterError};
use crate::lexer::Lexer;
use crate::parser::Parser;

mod lexer;
mod token;
mod expression;
mod parser;
mod interpreter;

fn main() {
    let input = read("examples/fibonacci.tiny").unwrap();
    let input_str = String::from_utf8(input).unwrap();
    let lexer = Lexer::new(input_str.as_str());

    let parser = Parser::new(lexer);
    let mut interpreter = Interpreter::new(parser);

    loop {
        match interpreter.execute_next() {
            Ok(value) => {/*println!("{value:?}")*/},
            Err(InterpreterError::InterpreterConsumed) => {
                println!("Execution finalized.");
                break;
            }
            Err(error) => {
                println!("ERROR: {error} at character {}", interpreter.get_index());
                break;
            }
        }
    }
}
