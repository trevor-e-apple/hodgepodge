use std::{collections::VecDeque, env, format, fs::File, io::Read, todo, vec};

#[derive(Debug, PartialEq)]
enum Token {
    LBrace,
    RBrace,
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
    True,
    False,
    And,
    Or,
    Comma,
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
    Return,
    Function,
    Procedure,
    DataStruct,
    StringLiteral(String),
    IntLiteral(i32),
    FloatLiteral(f32),
    Identifier(String),
}

// TODO: add more terminals (e.g. newline)
const TERMINAL: char = ' ';

// TODO: documentation
/// Returns either a vector of tokens or a tuple containing the error string and
/// the line number on which the error was found
fn scanner(contents: &str) -> Result<Vec<Token>, (String, i32)> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars: VecDeque<char> = contents.chars().collect();
    let mut line: i32 = 0;

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
                                    if check_char != TERMINAL {
                                        num_chars.push(check_char);
                                    } else {
                                        break;
                                    }
                                }
                                None => break,
                            }
                        }

                        // parse number string and make negative
                        let num_string: String =
                            num_chars.iter().cloned().collect();
                        match num_string.parse::<i32>() {
                            Ok(value) => Token::IntLiteral(-1 * value),
                            Err(_) => {
                                let err = format!(
                                    "Unable to parse expected int {}",
                                    num_string,
                                )
                                .to_string();
                                return Err((err, line));
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
        } else if character == ',' {
            Token::Comma
        } else if character == ';' {
            Token::EndStatement
        } else if character == '\n' {
            line += 1;
            Token::Newline
        } else if character == '{' {
            Token::LBrace
        } else if character == '}' {
            Token::RBrace
        } else if character.is_digit(10) {
            let mut token_chars = vec![character];

            let mut is_float = false;
            loop {
                let check_char = match chars.pop_front() {
                    Some(value) => value,
                    None => break,
                };

                if check_char.is_digit(10) {
                    token_chars.push(check_char);
                } else if check_char == '.' {
                    is_float = true;
                    token_chars.push(check_char);
                } else {
                    chars.push_front(check_char);
                    break;
                }
            }

            let string: String = token_chars.iter().cloned().collect();

            if is_float {
                match string.parse() {
                    Ok(value) => Token::FloatLiteral(value),
                    Err(_) => todo!(),
                }
            } else {
                match string.parse() {
                    Ok(value) => Token::IntLiteral(value),
                    Err(_) => todo!(),
                }
            }
        } else if character.is_alphabetic() {
            let mut token_chars = vec![character];
            loop {
                let check_char = match chars.pop_front() {
                    Some(value) => value,
                    None => break,
                };

                // first character of an identifier must be a alphabetic
                // -- character, but successive chracters can be numbers
                if check_char.is_alphanumeric() {
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
            } else if string == "return" {
                Token::Return
            } else if string == "or" {
                Token::Or
            } else if string == "and" {
                Token::And
            } else if string == "true" {
                Token::True
            } else if string == "false" {
                Token::False
            } else if string == "func" {
                Token::Function
            } else if string == "proc" {
                Token::Procedure
            } else if string == "struct" {
                Token::DataStruct
            } else {
                Token::Identifier(string)
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
            let err =
                format!("Unexpected lead character {}", character).to_string();
            return Err((err, line));
        };
        tokens.push(token);
    }

    return Ok(tokens);
}

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

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use super::*;

    // TODO: documentation
    fn check_token(
        tokens: &Vec<Token>,
        index: &mut usize,
        expected_token: Token,
    ) {
        match tokens.get(*index) {
            Some(value) => assert_eq!(*value, expected_token),
            None => assert!(false),
        };

        *index += 1;
    }

    // TODO: documentation
    fn check_identifier_token(
        tokens: &Vec<Token>,
        index: &mut usize,
        expected_string: &str,
    ) {
        match tokens.get(*index) {
            Some(value) => match value {
                Token::Identifier(token_string) => {
                    assert_eq!(token_string, expected_string)
                }
                _ => assert!(false),
            },
            None => assert!(false),
        };

        *index += 1;
    }

    // TODO: documentation
    fn check_int_literal_token(
        tokens: &Vec<Token>,
        index: &mut usize,
        expected_int: i32,
    ) {
        match tokens.get(*index) {
            Some(value) => match value {
                Token::IntLiteral(int_value) => {
                    assert_eq!(*int_value, expected_int);
                }
                _ => assert!(false),
            },
            None => assert!(false),
        };

        *index += 1;
    }

    fn check_float_literal_token(
        tokens: &Vec<Token>,
        index: &mut usize,
        expected_float: f32,
    ) {
        match tokens.get(*index) {
            Some(value) => match value {
                Token::FloatLiteral(float_value) => {
                    assert_eq!(*float_value, expected_float)
                }
                _ => assert!(false),
            },
            None => assert!(false),
        };

        *index += 1;
    }

    // TODO: documentation
    #[test]
    fn one_char_var() {
        let contents = "a + b";
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 3);

        let mut index = 0;
        let index = &mut index;

        check_identifier_token(&tokens, index, "a");

        check_token(&tokens, index, Token::Add);

        check_identifier_token(&tokens, index, "b");
    }

    // TODO: documentation
    #[test]
    fn multi_char_var() {
        let contents = "alice + bob";
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 3);

        let mut index = 0;
        let index = &mut index;

        check_identifier_token(&tokens, index, "alice");

        check_token(&tokens, index, Token::Add);

        check_identifier_token(&tokens, index, "bob");
    }

    // TODO: documentation
    #[test]
    fn trailing_space() {
        let contents = "alice + bob ";
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 3);

        let mut index = 0;
        let index = &mut index;

        check_identifier_token(&tokens, index, "alice");

        check_token(&tokens, index, Token::Add);

        check_identifier_token(&tokens, index, "bob");
    }

    // TODO: documentation
    #[test]
    fn no_spaces() {
        let contents = "alice+bob";
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 3);

        let mut index = 0;
        let index = &mut index;

        check_identifier_token(&tokens, index, "alice");

        check_token(&tokens, index, Token::Add);

        check_identifier_token(&tokens, index, "bob");
    }

    // TODO: documentation
    #[test]
    fn int_literal() {
        let contents = "1 +23";
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 3);

        let mut index = 0;
        let index = &mut index;

        check_int_literal_token(&tokens, index, 1);

        check_token(&tokens, index, Token::Add);

        check_int_literal_token(&tokens, index, 23);
    }

    // TODO: documentation
    #[test]
    fn float_literal() {
        let contents = "1.0 - 2.1";
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 3);

        let mut index = 0;
        let index = &mut index;

        check_float_literal_token(&tokens, index, 1.0);

        check_token(&tokens, index, Token::Minus);

        check_float_literal_token(&tokens, index, 2.1);
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

    #[test]
    fn flow_control() {
        unimplemented!();
    }

    #[test]
    fn negative_int() {
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

    // TODO: Documentation
    #[test]
    fn hex_literal() {
        unimplemented!();
    }

    #[test]
    fn binary_literal() {
        unimplemented!();
    }

    // TODO: Documentation
    #[test]
    fn multiline() {
        let contents = concat!("i32 a = b + c;\n", "f32 d = 2 * a;\n",);
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        let mut index = 0;
        let index = &mut index;

        check_identifier_token(&tokens, index, "i32");
        check_identifier_token(&tokens, index, "a");
        check_token(&tokens, index, Token::Assignment);
        check_identifier_token(&tokens, index, "b");
        check_token(&tokens, index, Token::Add);
        check_identifier_token(&tokens, index, "c");
        check_token(&tokens, index, Token::EndStatement);
        check_token(&tokens, index, Token::Newline);

        check_identifier_token(&tokens, index, "f32");
        check_identifier_token(&tokens, index, "d");
        check_token(&tokens, index, Token::Assignment);
        check_int_literal_token(&tokens, index, 2);
        check_token(&tokens, index, Token::Multiply);
        check_identifier_token(&tokens, index, "a");
        check_token(&tokens, index, Token::EndStatement);
    }

    #[test]
    fn multi_expression() {
        unimplemented!();
    }

    /// test handling errors when user provides multiple  
    #[test]
    fn multi_point_float_literal_error() {
        unimplemented!();
    }

    #[test]
    fn function_definition() {
        let contents = "func add(i32 a, f32 b) {";
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        let mut index = 0;
        let index = &mut index;

        check_token(&tokens, index, Token::Function);
        check_identifier_token(&tokens, index, "add");
        check_token(&tokens, index, Token::LParen);
        check_identifier_token(&tokens, index, "i32");
        check_identifier_token(&tokens, index, "a");
        check_token(&tokens, index, Token::Comma);
        check_identifier_token(&tokens, index, "f32");
        check_identifier_token(&tokens, index, "b");
        check_token(&tokens, index, Token::RParen);
        check_token(&tokens, index, Token::LBrace);
    }

    /// Test for verifying that the line number where the issue is found is
    /// correct
    #[test]
    fn error_on_line() {
        let contents = concat!("i32 andy = bella + craig;\n", "-1x\n");
        match scanner(contents) {
            Ok(_) => assert!(false),
            Err(err_data) => {
                let (_, line) = err_data;
                assert_eq!(line, 1);
            }
        };
    }

    // TODO: add test for something like -x. what should the tokens be?
}
