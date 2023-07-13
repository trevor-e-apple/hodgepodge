use std::{
    collections::VecDeque, env, fs::File, io::Read, ops::Add, todo, vec,
};

#[derive(Debug)]
enum Token {
    LScope,
    RScope,
    Assignment,
    Add,
    Minus,
    Multiply,
    Divide,
    Equivalence,
    LessThan,
    LessThanEqualTo,
    GreaterThan,
    GreatherThanEqualTo,
    EndStatement,
    LParen,
    RParen,
    Newline,
    LineComment,
    LBlockComment,
    RBlockComment,
    If,
    Else,
    For,
    While,
    StringLiteral(String),
    IntLiteral(i32),
    FloatLiteral(f32),
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

        let token = if character == '=' {
            match chars.pop_front() {
                Some(check_char) => {
                    if check_char == '=' {
                        Token::Equivalence
                    } else {
                        Token::Assignment
                    }
                }
                None => Token::Assignment,
            }
        } else if character == '+' {
            Token::Add
        } else if character == '-' {
            match chars.pop_front() {
                Some(value) => {
                    if value.is_digit(10) {
                        // TODO: handle floats
                        let mut num_chars = vec![value];
                        loop {
                            match chars.pop_front() {
                                Some(check_char) => {
                                    if check_char.is_digit(10) {
                                        num_chars.push(check_char);
                                    } else {
                                        chars.push_front(check_char);
                                        break;
                                    }
                                }
                                None => break,
                            }
                        }
                        let num_string: String =
                            num_chars.iter().cloned().collect();
                        match num_string.parse::<i32>() {
                            Ok(value) => Token::IntLiteral(-1 * value),
                            Err(_) => {
                                // TODO: error message
                                assert!(false);
                                Token::Minus
                            }
                        }
                    } else {
                        Token::Minus
                    }
                }
                None => Token::Minus,
            }
        } else if character == '*' {
            match chars.pop_front() {
                Some(check_char) => {
                    if check_char == '/' {
                        Token::RBlockComment
                    } else {
                        Token::Multiply
                    }
                }
                None => Token::Multiply,
            }
        } else if character == '/' {
            match chars.pop_front() {
                Some(check_char) => {
                    if check_char == '/' {
                        Token::LineComment
                    } else if check_char == '*' {
                        Token::LBlockComment
                    } else {
                        chars.push_front(check_char);
                        Token::Divide
                    }
                }
                None => Token::Divide,
            }
        } else if character == '<' {
            match chars.pop_front() {
                Some(check_char) => {
                    if check_char == '=' {
                        Token::LessThanEqualTo
                    } else {
                        Token::LessThan
                    }
                }
                None => Token::LessThan,
            }
        } else if character == '>' {
            match chars.pop_front() {
                Some(check_char) => {
                    if check_char == '=' {
                        Token::GreatherThanEqualTo
                    } else {
                        Token::GreaterThan
                    }
                }
                None => Token::GreaterThan,
            }
        } else if character == '(' {
            Token::LParen
        } else if character == ')' {
            Token::RParen
        } else if character == ';' {
            Token::EndStatement
        } else if character == '\n' {
            Token::Newline
        } else if character == '{' {
            Token::LScope
        } else if character == '}' {
            Token::RScope
        } else if character.is_digit(10) {
            // TODO: handle floating point
            Token::IntLiteral(character.to_digit(10).unwrap() as i32)
        } else if character.is_alphabetic() {
            // TODO: handle errors
            let mut token_chars = vec![character];
            loop {
                let check_char = match chars.pop_front() {
                    Some(value) => value,
                    None => break,
                };

                if check_char.is_alphabetic() {
                    token_chars.push(check_char);
                } else {
                    chars.push_front(check_char);
                    break;
                }
            }
            let string: String = token_chars.iter().cloned().collect();
            if string == "if" {
                Token::If
            } else if string == "else" {
                Token::Else
            } else if string == "while" {
                Token::While
            } else if string == "for" {
                Token::For
            } else {
                Token::Variable(string)
            }
        } else if character == '"' {
            let mut token_chars = vec![character];

            loop {
                let check_char = match chars.pop_front() {
                    Some(value) => value,
                    None => break,
                };

                if check_char != '"' {
                    token_chars.push(check_char)
                } else {
                    break;
                }
            }
            Token::StringLiteral(token_chars.iter().cloned().collect())
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

    // TODO: documentation
    fn check_int_literal_token(
        tokens: &Vec<Token>,
        index: usize,
        expected_int: i32,
    ) {
        match tokens.get(index) {
            Some(value) => match value {
                Token::IntLiteral(int_value) => {
                    assert_eq!(*int_value, expected_int);
                }
                _ => assert!(false),
            },
            None => assert!(false),
        }
    }

    fn check_float_literal_token(
        tokens: &Vec<Token>,
        index: usize,
        expected_float: f32,
    ) {
        match tokens.get(index) {
            Some(value) => match value {
                Token::FloatLiteral(float_value) => {
                    assert_eq!(*float_value, expected_float)
                }
                _ => assert!(false),
            },
            None => assert!(false),
        }
    }

    // TODO: documentation
    #[test]
    fn one_char_var() {
        let contents = "a + b";
        let tokens = scanner(contents);

        assert_eq!(tokens.len(), 3);

        check_variable_token(&tokens, 0, "a");

        check_token(&tokens, 1, Token::Add);

        check_variable_token(&tokens, 2, "b");
    }

    // TODO: documentation
    #[test]
    fn multi_char_var() {
        let contents = "alice + bob";
        let tokens = scanner(contents);

        assert_eq!(tokens.len(), 3);

        check_variable_token(&tokens, 0, "alice");

        check_token(&tokens, 1, Token::Add);

        check_variable_token(&tokens, 2, "bob");
    }

    // TODO: documentation
    #[test]
    fn trailing_space() {
        let contents = "alice + bob ";
        let tokens = scanner(contents);

        assert_eq!(tokens.len(), 3);

        check_variable_token(&tokens, 0, "alice");

        check_token(&tokens, 1, Token::Add);

        check_variable_token(&tokens, 2, "bob");
    }

    // TODO: documentation
    #[test]
    fn no_spaces() {
        let contents = "alice+bob";
        let tokens = scanner(contents);

        assert_eq!(tokens.len(), 3);

        check_variable_token(&tokens, 0, "alice");

        check_token(&tokens, 1, Token::Add);

        check_variable_token(&tokens, 2, "bob");
    }

    // TODO: documentation
    #[test]
    fn numeric_literals() {
        let contents = "1 +23";
        let tokens = scanner(contents);

        assert_eq!(tokens.len(), 3);

        check_variable_token(&tokens, 0, "alice");

        check_token(&tokens, 1, Token::Add);

        check_variable_token(&tokens, 2, "bob");
    }

    // TODO: documentation
    #[test]
    fn string_literals() {
        unimplemented!();
    }

    // TODO: documentation
    #[test]
    fn max_munch() {
        unimplemented!();
    }

    // TODO: documentation
    #[test]
    fn floating_point() {
        unimplemented!();
    }

    #[test]
    fn flow_control_scan() {
        unimplemented!();
    }

    #[test]
    fn negative_number() {
        unimplemented!();
    }

    #[test]
    fn negative_float() {
        unimplemented!();
    }

    #[test]
    fn newlines() {
        unimplemented!();
    }
}
