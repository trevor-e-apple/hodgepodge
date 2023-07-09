use std::{collections::VecDeque, env, fs::File, io::Read, todo};

#[derive(Debug)]
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

fn scanner(contents: &str) -> Vec<Token> {
    // start scanning for tokens
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars: VecDeque<char> = contents.chars().collect();

    loop {
        let character = match chars.pop_front() {
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
            let mut index = 0;
            loop {
                // TODO: handle errors
                let check_char = chars.pop_front().unwrap();
                if check_char.is_alphabetic() {
                    index += 1;
                } else {
                    chars.push_front(check_char);
                    break;
                }
            }
            let string = contents[0..index].to_string();
            Token::Variable(string)
        } else {
            todo!("error handling");
            break;
        };
        tokens.push(token);
    }

    return tokens;
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];

    // TODO: handle errors
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    // TODO: handle errors
    file.read_to_string(&mut contents).unwrap();

    scanner(&contents);
}

#[test]
fn one_char_var() {
    let contents = "a + b ";
    scanner(contents);
}

#[test]
fn multi_char_var() {
    let contents = "alice + bob ";
    scanner(contents);
}
