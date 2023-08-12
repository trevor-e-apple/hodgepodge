mod code_gen;
mod parser;
mod scanner;
mod syntax_tree;

use parser::parse;
use scanner::scan;
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

    let tree = match parse(&tokens) {
        Ok(tree) => tree,
        Err(_) => todo!(), // print out the error(s)
    };

    let representation = code_gen::generate(tree);
}
