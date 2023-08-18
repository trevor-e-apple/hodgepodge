mod code_gen;
mod expression_parser;
mod scanner;
mod statement_parser;
mod statements;
mod syntax_tree;

use scanner::scan;
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

    let tokens = match scan(&contents) {
        Ok(tokens) => tokens,
        Err(_) => todo!(),
    };

    let tree = match parse_statement(&tokens) {
        Ok(tree) => tree,
        Err(_) => todo!(), // print out the error(s)
    };
    todo!();
}
