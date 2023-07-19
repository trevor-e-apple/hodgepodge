mod scanner;

use std::{env, fs::File, io::Read, todo};
use scanner::scanner;

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    // TODO: handle errors
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    // TODO: handle errors
    file.read_to_string(&mut contents).unwrap();

    match scanner(&contents) {
        Ok(_) => todo!(),
        Err(_) => todo!(),
    };
}
