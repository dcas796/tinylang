#![feature(let_chains)]
#![feature(string_remove_matches)]

use std::fs::read;
use crate::expression::Expr;
use crate::lexer::Lexer;
use crate::parser::Parser;

mod lexer;
mod token;
mod expression;
mod parser;

fn main() {
    let input = read("../examples/fibonacci.tiny").unwrap();
    let input_str = String::from_utf8(input).unwrap();
    let lexer = Lexer::new(input_str.as_str());

    let mut parser = Parser::new(lexer);
    loop {
        let expr = match parser.next_expression() {
            Ok(Expr::Eof) => break,
            Ok(expr) => expr,
            Err(err) => {
                println!("Error while parsing: {}", err);
                break;
            }
        };

        println!("{:#?}", expr);
    }
}
