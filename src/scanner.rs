use std::{collections::VecDeque, format, vec};

/* TODO:
for/in loops
*/

#[derive(Debug, PartialEq)]
pub enum Token {
    LBrace,
    RBrace,
    Assignment,
    Plus,
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
    LBracket,
    RBracket,
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
    UintLiteral(u32),
    FloatLiteral(f32),
    Identifier(String),
}

const TERMINAL: char = ' ';

// TODO: documentation
/// Returns either a vector of tokens or a tuple containing the error string and
/// the line number on which the error was found
pub fn scanner(contents: &str) -> Result<Vec<Token>, (String, i32)> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars: VecDeque<char> = contents.chars().collect();
    let mut line: i32 = 0;

    'scanloop: loop {
        let character = match chars.pop_front() {
            Some(value) => value,
            None => break 'scanloop,
        };

        if character == TERMINAL {
            continue 'scanloop;
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
            Token::Plus
        } else if character == '-' {
            match chars.pop_front() {
                Some(first_digit) => {
                    if first_digit.is_digit(10) {
                        // scan number doesn't handle negatives, so we need to
                        // -- modify its return values
                        match scan_number(&mut chars, first_digit, 10, line) {
                            Ok(token) => match token {
                                Token::IntLiteral(int_value) => {
                                    Token::IntLiteral(-1 * int_value)
                                }
                                Token::FloatLiteral(float_value) => {
                                    Token::FloatLiteral(-1.0 * float_value)
                                }
                                _ => {
                                    let err = "Unexpected token".to_string();
                                    return Err((err, line));
                                }
                            },
                            Err(err) => {
                                return Err(err);
                            }
                        }
                    } else {
                        chars.push_front(first_digit);
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
        } else if character == '[' {
            Token::LBracket
        } else if character == ']' {
            Token::RBracket
        } else if character.is_digit(10) {
            // First, figure out what the base of the number is
            let base = if character == '0' {
                match chars.pop_front() {
                    Some(value) => {
                        let check_char = value;
                        if check_char == 'x' {
                            16
                        } else if check_char == 'b' {
                            2
                        } else {
                            10
                        }
                    }
                    None => 10,
                }
            } else {
                10
            };

            match scan_number(&mut chars, character, base, line) {
                Ok(token) => token,
                Err(err) => {
                    return Err(err);
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
            // no need to include first quotation mark in token chars
            let mut token_chars = vec![];

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

/// Helper function for scanning numbers, whether integer, unsigned integer, or
/// float
fn scan_number(
    chars: &mut VecDeque<char>,
    character: char,
    base: u32,
    line: i32,
) -> Result<Token, (String, i32)> {
    // then finish bringing in the rest of the token characters
    let mut token_chars = vec![character];

    let mut is_float = false;
    loop {
        let check_char = match chars.pop_front() {
            Some(value) => value,
            None => break,
        };

        if check_char.is_digit(base) {
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
            Ok(value) => Ok(Token::FloatLiteral(value)),
            Err(_) => {
                let err = format!(
                    "Unable to parse expected float literal: {}",
                    string
                );
                return Err((err, line));
            }
        }
    } else {
        if base == 10 {
            match string.parse() {
                Ok(value) => Ok(Token::IntLiteral(value)),
                Err(_) => {
                    let err = format!(
                        "Unable to parse expected int literal: {}",
                        string
                    );
                    return Err((err, line));
                }
            }
        } else {
            match u32::from_str_radix(&string, base) {
                Ok(value) => Ok(Token::UintLiteral(value)),
                Err(_) => {
                    let err = format!(
                        "Unable to parse expected uint literal with base {}: {}",
                        base,
                        string
                    );
                    return Err((err, line));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use super::*;

    /// Helper function for tests that need to check whether a token without a
    /// payload is at index. Increments index at the end of the function
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

    /// Helper function for tests that need to check whether a identifier token
    /// is at index. Increments index at the end of the function
    fn check_identifier_token(
        tokens: &Vec<Token>,
        index: &mut usize,
        expected_identifier: &str,
    ) {
        match tokens.get(*index) {
            Some(value) => match value {
                Token::Identifier(token_string) => {
                    assert_eq!(token_string, expected_identifier)
                }
                _ => assert!(false),
            },
            None => assert!(false),
        };

        *index += 1;
    }

    /// Helper function for tests that need to check whether a int literal
    /// token is at index. Increments index at the end of the function
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

    /// Helper function for tests that need to check whether a uint literal
    /// token is at index. Increments index at the end of the function
    fn check_uint_literal_token(
        tokens: &Vec<Token>,
        index: &mut usize,
        expected_value: u32,
    ) {
        match tokens.get(*index) {
            Some(value) => match value {
                Token::UintLiteral(uint_value) => {
                    assert_eq!(*uint_value, expected_value);
                }
                _ => assert!(false),
            },
            None => assert!(false),
        };

        *index += 1;
    }

    /// Helper function for tests that need to check whether a float literal
    /// token is at index. Increments index at the end of the function
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

    /// Helper function for tests that need to check whether a string literal
    /// token is at index. Increments index at the end of the function
    fn check_string_literal_token(
        tokens: &Vec<Token>,
        index: &mut usize,
        expected_string: &str,
    ) {
        match tokens.get(*index) {
            Some(value) => match value {
                Token::StringLiteral(string_value) => {
                    assert_eq!(*string_value, expected_string);
                }
                _ => assert!(false),
            },
            None => assert!(false),
        }

        *index += 1;
    }

    /// Test that one character identifier tokens can be scanned
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

        check_token(&tokens, index, Token::Plus);

        check_identifier_token(&tokens, index, "b");
    }

    /// Test that multi character identifier tokens can be scanned
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

        check_token(&tokens, index, Token::Plus);

        check_identifier_token(&tokens, index, "bob");
    }

    /// Test that the scanner is not troubled by trailing whitespace
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

        check_token(&tokens, index, Token::Plus);

        check_identifier_token(&tokens, index, "bob");
    }

    /// Test that the scanner is not troubled when tokens aren't separated by
    /// whitespace
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

        check_token(&tokens, index, Token::Plus);

        check_identifier_token(&tokens, index, "bob");
    }

    /// Test that int literal tokens can be sanned
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

        check_token(&tokens, index, Token::Plus);

        check_int_literal_token(&tokens, index, 23);
    }

    /// Test that float literal tokens can be scanned
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

    /// Test that string literal tokens can be scanned
    #[test]
    fn string_literal() {
        let contents = "char[] name = \"alice\";\n";
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 8);

        let mut index = 0;
        let index = &mut index;

        check_identifier_token(&tokens, index, "char");
        check_token(&tokens, index, Token::LBracket);
        check_token(&tokens, index, Token::RBracket);
        check_identifier_token(&tokens, index, "name");
        check_token(&tokens, index, Token::Assignment);
        check_string_literal_token(&tokens, index, "alice");
        check_token(&tokens, index, Token::EndStatement);
        check_token(&tokens, index, Token::Newline);
    }

    /// Test that max munch works as expected. That is, the longest matching
    /// token is parsed, not any of the shorter matching tokens
    #[test]
    fn max_munch() {
        let contents = "ifl";
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 1);

        let mut index = 0;
        let index = &mut index;

        check_identifier_token(&tokens, index, "ifl");
    }

    /// Test flow control tokens
    #[test]
    fn flow_control() {
        let contents = concat!(
            "if a == b {\n",
            "    c = d;\n",
            "} else {\n",
            "    d = c;\n",
            "}\n"
        );
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 22);

        let mut index = 0;
        let index = &mut index;

        check_token(&tokens, index, Token::If);
        check_identifier_token(&tokens, index, "a");
        check_token(&tokens, index, Token::Equivalence);
        check_identifier_token(&tokens, index, "b");
        check_token(&tokens, index, Token::LBrace);
        check_token(&tokens, index, Token::Newline);
        check_identifier_token(&tokens, index, "c");
        check_token(&tokens, index, Token::Assignment);
        check_identifier_token(&tokens, index, "d");
        check_token(&tokens, index, Token::EndStatement);
        check_token(&tokens, index, Token::Newline);
        check_token(&tokens, index, Token::RBrace);
        check_token(&tokens, index, Token::Else);
        check_token(&tokens, index, Token::LBrace);
        check_token(&tokens, index, Token::Newline);
        check_identifier_token(&tokens, index, "d");
        check_token(&tokens, index, Token::Assignment);
        check_identifier_token(&tokens, index, "c");
        check_token(&tokens, index, Token::EndStatement);
        check_token(&tokens, index, Token::Newline);
        check_token(&tokens, index, Token::RBrace);
        check_token(&tokens, index, Token::Newline);
    }

    /// Test that basic loops tokens can be parsed
    #[test]
    fn basic_loop() {
        let contents = concat!("while a < b {\n", "    a += 1;\n", "}\n",);
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 14);

        let mut index = 0;
        let index = &mut index;

        check_token(&tokens, index, Token::While);
        check_identifier_token(&tokens, index, "a");
        check_token(&tokens, index, Token::LessThan);
        check_identifier_token(&tokens, index, "b");
        check_token(&tokens, index, Token::LBrace);
        check_token(&tokens, index, Token::Newline);
        check_identifier_token(&tokens, index, "a");
        check_token(&tokens, index, Token::Plus);
        check_token(&tokens, index, Token::Assignment);
        check_int_literal_token(&tokens, index, 1);
        check_token(&tokens, index, Token::EndStatement);
        check_token(&tokens, index, Token::Newline);
        check_token(&tokens, index, Token::RBrace);
        check_token(&tokens, index, Token::Newline);
    }

    /// Test combination of if-else and loops
    #[test]
    fn flow_control_loop() {
        let contents =
            concat!("if a < b {\n", "    a = b;\n", "} else while a > b {\n",);

        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 19);

        let mut index = 0;
        let index = &mut index;

        check_token(&tokens, index, Token::If);
        check_identifier_token(&tokens, index, "a");
        check_token(&tokens, index, Token::LessThan);
        check_identifier_token(&tokens, index, "b");
        check_token(&tokens, index, Token::LBrace);
        check_token(&tokens, index, Token::Newline);
        check_identifier_token(&tokens, index, "a");
        check_token(&tokens, index, Token::Assignment);
        check_identifier_token(&tokens, index, "b");
        check_token(&tokens, index, Token::EndStatement);
        check_token(&tokens, index, Token::Newline);
        check_token(&tokens, index, Token::RBrace);
        check_token(&tokens, index, Token::Else);
        check_token(&tokens, index, Token::While);
        // the rest are covered by other test cases
    }

    #[test]
    fn negative_int() {
        let contents = "-425";
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 1);

        let mut index = 0;
        let index = &mut index;
        check_int_literal_token(&tokens, index, -425);
    }

    #[test]
    fn negative_float() {
        let contents = "-1.387";
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 1);

        let mut index = 0;
        let index = &mut index;
        check_float_literal_token(&tokens, index, -1.387);
    }

    #[test]
    fn newlines() {
        unimplemented!();
    }

    /// Test the tokenization of a hex literal
    #[test]
    fn hex_literal() {
        let contents = "0xABCDEF";
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 1);

        let mut index = 0;
        let index = &mut index;
        check_uint_literal_token(&tokens, index, 0xABCDEF);
    }

    #[test]
    fn binary_literal() {
        let contents = "0b1110";
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 1);

        let mut index = 0;
        let index = &mut index;
        check_uint_literal_token(&tokens, index, 0b1110);
    }

    #[test]
    fn zero_token() {
        let contents = "0";
        let tokens = match scanner(contents) {
            Ok(value) => value,
            Err(_) => {
                assert!(false);
                vec![]
            }
        };

        assert_eq!(tokens.len(), 1);

        let mut index = 0;
        let index = &mut index;
        check_int_literal_token(&tokens, index, 0);
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
        check_token(&tokens, index, Token::Plus);
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

    /// test handling errors when user provides multiple points in a floating
    /// point literal
    #[test]
    fn multi_point_float_literal_error() {
        let contents = "1.0.1";
        match scanner(contents) {
            Ok(_) => assert!(false),
            Err(_) => assert!(true),
        };
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
