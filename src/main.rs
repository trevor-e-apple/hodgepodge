mod code_gen;
mod expression_parser;
mod scanner;
mod statement_parser;
mod statements;
mod syntax_tree;

use statement_parser::parse_statement;
use std::{env, fs::File, io::Read, todo};

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    // TODO: handle errors
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    // TODO: handle errors
    file.read_to_string(&mut contents).unwrap();

    let tokens = match scanner::scan(&contents) {
        Ok(tokens) => tokens,
        Err(_) => todo!(),
    };

    let statements = match parse_statement(&tokens) {
        Ok(statements) => statements,
        Err(_) => todo!(), // print out the error(s)
    };

    match code_gen::generate(&statements) {
        Ok(_) => todo!(),
        Err(_) => todo!(),
    };
}
