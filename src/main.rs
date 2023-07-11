use std::{collections::VecDeque, env, fs::File, io::Read, ops::Add, todo};

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

// TODO: documentation
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
            let mut token_chars = vec![character];
            loop {
                // TODO: handle errors
                let check_char = chars.pop_front().unwrap();
                if check_char.is_alphabetic() {
                    token_chars.push(check_char);
                } else {
                    chars.push_front(check_char);
                    break;
                }
            }
            let string: String = token_chars.iter().cloned().collect();
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

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use super::*;

    // TODO: documentation
    fn check_token(tokens: &Vec<Token>, index: usize, expected_token: Token) {
        match tokens.get(index) {
            Some(value) => match value {
                expected_token => {}
                _ => assert!(false),
            },
            None => assert!(false),
        }
    }

    // TODO: documentation
    fn check_variable_token(
        tokens: &Vec<Token>,
        index: usize,
        expected_string: &str,
    ) {
        match tokens.get(index) {
            Some(value) => match value {
                Token::Variable(token_string) => {
                    assert_eq!(token_string, expected_string)
                }
                _ => assert!(false),
            },
            None => assert!(false),
        };
    }

    #[test]
    fn one_char_var() {
        let contents = "a + b ";
        let tokens = scanner(contents);

        assert_eq!(tokens.len(), 3);

        check_variable_token(&tokens, 0, "a");

        check_token(&tokens, 1, Token::Add);

        check_variable_token(&tokens, 2, "b");
    }

    #[test]
    fn multi_char_var() {
        let contents = "alice + bob ";
        let tokens = scanner(contents);
        assert_eq!(tokens.len(), 3);

        check_variable_token(&tokens, 0, "alice");

        check_token(&tokens, 1, Token::Add);

        check_variable_token(&tokens, 2, "bob");
    }
}
