use std::{env, fs::File, io::Read, todo};

enum Token {
    LScope,
    RScope,
    Assignment,
    Add,
    Minus,
    Multiply,
    Divide,
    Lparen,
    RParen,
    BlockComment,
    Number(i32),
    Variable(String),
}

// TODO: add more terminals (e.g. newline)
const TERMINAL: char = ' ';

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    // TODO: handle errors
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    // TODO: handle errors
    file.read_to_string(&mut contents).unwrap();

    // start scanning for tokens
    let mut tokens: Vec<Token> = Vec::new();
    let mut scan_iter = contents.chars().enumerate();
    loop {
        let (index, character) = match scan_iter.next() {
            Some(value) => value,
            None => break,
        };

        if character == TERMINAL {
            continue;
        }

        let token = if character == '+' {
            Token::Add
        } else if character == '-' {
            Token::Minus
        } else if character.is_digit(10) {
            Token::Number(character.to_digit(10).unwrap() as i32)
        } else if character.is_alphabetic() {
            // TODO: handle errors
            let (mut check_index, mut check_char) = scan_iter.next().unwrap();
            while check_char.is_alphabetic() {
                (check_index, check_char) = scan_iter.next().unwrap();
                // TODO: handle errors
            }
            Token::Variable(contents[index..check_index].to_string())
        } else {
            todo!("error handling");
            break;
        };
        tokens.push(token);
    }
}
