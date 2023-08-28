/*
A parser that takes a Vec of Tokens and produces a "Statement" data structure
reflecting the following grammar

statement ->
    ((variable_type)* IDENTIFIER "=")*
        (scope ";") | (expression ";") | (variable_declaration ";");
scope -> "{" statement* expression? "}";

The expression grammar is defined in the expression parser module
*/

use core::panic;
use std::{todo, vec};

use crate::{
    expression_parser::{parse_expression, ParseError},
    scanner::Token,
    statements::{StatementHandle, Statements},
};

struct StackEntry {
    statement: StatementHandle,
    start_index: usize,
    end_index: usize,
}

pub fn parse_statement(
    tokens: &[Token],
) -> Result<Statements, Vec<ParseError>> {
    let mut statements = Statements::new();
    let mut parse_errors: Vec<ParseError> = vec![];

    let root_statement_handle = statements.add_root_statement();

    let mut stack: Vec<StackEntry> = vec![StackEntry {
        statement: root_statement_handle,
        start_index: 0,
        end_index: tokens.len(),
    }];
    while let Some(stack_entry) = stack.pop() {
        let start_expression = {
            let start_expression = parse_declaration_and_assignment(
                &mut statements,
                &mut stack,
                &mut parse_errors,
                tokens,
                &stack_entry,
            );
            if let Some(start_expression) = start_expression {
                start_expression
            } else {
                stack_entry.start_index
            }
        };

        // check if the expression is a full scope
        let full_scope: bool = {
            match tokens.get(start_expression) {
                Some(first_token) => {
                    match tokens.get(stack_entry.end_index - 2) {
                        Some(penultimate_token) => {
                            *first_token == Token::LBrace
                                && *penultimate_token == Token::RBrace
                        }
                        None => false,
                    }
                }
                None => false,
            }
        };

        if full_scope {
            parse_full_scope(
                &mut statements,
                &mut stack,
                &mut parse_errors,
                tokens,
                &stack_entry,
            );
        } else {
            // parse the expression
            let statement =
                match statements.get_statement_mut(stack_entry.statement) {
                    Some(statement) => statement,
                    None => todo!(),
                };
            let start_index = start_expression;
            // clip endstatement token from expression if necessary
            let end_index =
                if let Some(token) = tokens.get(stack_entry.end_index - 1) {
                    if *token == Token::EndStatement {
                        stack_entry.end_index - 1
                    } else {
                        stack_entry.end_index
                    }
                } else {
                    stack_entry.end_index
                };
            statement.expression =
                match parse_expression(&tokens[start_index..end_index]) {
                    Ok(tree) => Some(tree),
                    Err(err) => {
                        parse_errors.push(err);
                        None
                    }
                };
        }
    }

    if parse_errors.len() == 0 {
        Ok(statements)
    } else {
        Err(parse_errors)
    }
}

/// a helper function for the parse function that handles parsing declaration
/// and assignment
fn parse_declaration_and_assignment(
    statements: &mut Statements,
    stack: &mut Vec<StackEntry>,
    parse_errors: &mut Vec<ParseError>,
    tokens: &[Token],
    stack_entry: &StackEntry,
) -> Option<usize> {
    let first_token_index = stack_entry.start_index;
    let second_token_index = first_token_index + 1;
    let third_token_index = second_token_index + 1;

    let first_identifier: Option<Token> = match tokens.get(first_token_index) {
        Some(token) => match token {
            Token::Identifier(_) => Some(token.clone()),
            _ => None,
        },
        None => None,
    };

    let second_token: Option<Token> = match tokens.get(second_token_index) {
        Some(token) => match token {
            Token::Assignment => Some(token.clone()),
            Token::Identifier(_) => Some(token.clone()),
            _ => None,
        },
        None => None,
    };

    let assignment_token: Option<Token> = match tokens.get(third_token_index) {
        Some(token) => match token {
            Token::Assignment => Some(token.clone()),
            _ => None,
        },
        None => None,
    };

    let statement = match statements.get_statement_mut(stack_entry.statement) {
        Some(statement) => statement,
        None => todo!(),
    };

    if first_identifier == None {
        // empty statement
        parse_errors.push(ParseError::MissingToken);

        None
    } else {
        match second_token {
            Some(second_token) => {
                match second_token {
                    Token::Assignment => {
                        // expression starts after assignment
                        statement.variable = match first_identifier {
                            Some(first_token) => match first_token {
                                Token::Identifier(identifier) => {
                                    Some(identifier)
                                }
                                _ => panic!(),
                            },
                            None => panic!(),
                        };
                        Some(third_token_index)
                    }
                    Token::Identifier(second_token_identifier) => {
                        if assignment_token == None {
                            // should assign with declaration
                            parse_errors
                                .push(ParseError::UnassignedDeclaration);
                            None
                        } else {
                            statement.type_declaration = match first_identifier
                            {
                                Some(first_token) => match first_token {
                                    Token::Identifier(identifier) => {
                                        Some(identifier)
                                    }
                                    _ => panic!(),
                                },
                                None => panic!(),
                            };
                            statement.variable = Some(second_token_identifier);
                            Some(third_token_index + 1)
                        }
                    }
                    _ => panic!(),
                }
            }
            None => None, // id with no side effect should be ok
        }
    }
}

fn parse_full_scope(
    statements: &mut Statements,
    stack: &mut Vec<StackEntry>,
    parse_errors: &mut Vec<ParseError>,
    tokens: &[Token],
    stack_entry: &StackEntry,
) {
    // find all top level statements
    let start_index = stack_entry.start_index + 1;
    let end_index = stack_entry.end_index - 2;
    let mut lbraces_found = 0;
    let mut rbraces_found = 0;
    let mut current_statement_start = start_index;
    for index in start_index..end_index {
        let token = match tokens.get(index) {
            Some(token) => token,
            None => {
                // should never happen. need to handle error path
                todo!()
            }
        };

        if *token == Token::RBrace {
            rbraces_found += 1;
        } else if *token == Token::LBrace {
            lbraces_found += 1;
        } else if lbraces_found == rbraces_found {
            if *token == Token::EndStatement {
                let new_statement_handle =
                    statements.add_statement(stack_entry.statement);

                let one_past_end_statement_token = index + 1;
                // add new statement to the stack
                stack.push(StackEntry {
                    statement: new_statement_handle,
                    start_index: current_statement_start,
                    end_index: one_past_end_statement_token,
                });
                current_statement_start = one_past_end_statement_token;
            }
        }
    }

    let has_expression = match tokens.get(end_index - 1) {
        Some(token) => *token != Token::EndStatement && *token != Token::LBrace,
        None => false,
    };

    if has_expression {
        // find the final statement end
        let final_statement_end: Option<usize> = {
            let mut final_statement_end = None;
            for index in (start_index..end_index).rev() {
                if let Some(token) = tokens.get(index) {
                    if *token == Token::EndStatement {
                        final_statement_end = Some(index);
                        break;
                    }
                }
            }

            final_statement_end
        };

        // parse the expression
        if let Some(final_statement_end) = final_statement_end {
            let expression_start_index = final_statement_end + 1;
            let statement =
                match statements.get_statement_mut(stack_entry.statement) {
                    Some(statement) => statement,
                    None => todo!(),
                };
            let expression_end_index = stack_entry.end_index;
            let token_slice =
                &tokens[expression_start_index..expression_end_index];
            statement.expression = match parse_expression(&token_slice) {
                Ok(tree) => Some(tree),
                Err(err) => {
                    parse_errors.push(err);
                    None
                }
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use std::println;

    use super::*;
    use crate::statements::equivalent;

    fn debug_print(actual: &Statements, expected: &Statements) {
        println!("actual:");
        actual.pretty_print();
        println!("expected:");
        expected.pretty_print();
    }

    #[test]
    fn expression_only() {
        let tokens =
            vec![Token::IntLiteral(1), Token::Plus, Token::IntLiteral(2)];
        let statements = match parse_statement(&tokens) {
            Ok(statements) => statements,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let expected_statements = {
            let mut expected_statements = Statements::new();

            let root_handle = expected_statements.add_root_statement();
            let root_statement =
                match expected_statements.get_statement_mut(root_handle) {
                    Some(statement) => statement,
                    None => {
                        assert!(false);
                        return;
                    }
                };
            root_statement.expression = match parse_expression(&tokens) {
                Ok(tree) => Some(tree),
                Err(_) => {
                    assert!(false);
                    return;
                }
            };
            expected_statements
        };

        debug_print(&statements, &expected_statements);

        assert!(equivalent(&statements, &expected_statements));
    }

    #[test]
    fn statement_only() {
        let tokens = vec![
            Token::IntLiteral(1),
            Token::Plus,
            Token::IntLiteral(2),
            Token::EndStatement,
        ];
        let statements = match parse_statement(&tokens) {
            Ok(statements) => statements,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let expected_statements = {
            let mut expected_statements = Statements::new();

            let root_handle = expected_statements.add_root_statement();

            let root_statement =
                match expected_statements.get_statement_mut(root_handle) {
                    Some(statement) => statement,
                    None => {
                        assert!(false);
                        return;
                    }
                };

            root_statement.expression = match parse_expression(&tokens) {
                Ok(tree) => Some(tree),
                Err(_) => {
                    assert!(false);
                    return;
                }
            };
            expected_statements
        };

        debug_print(&statements, &expected_statements);
        assert!(equivalent(&statements, &expected_statements));
    }

    #[test]
    fn statement_and_expression() {
        let tokens = vec![
            Token::LBrace,
            // statement
            Token::IntLiteral(1),
            Token::Plus,
            Token::IntLiteral(2),
            Token::EndStatement,
            // expression
            Token::IntLiteral(3),
            Token::Plus,
            Token::IntLiteral(4),
            Token::RBrace,
            Token::EndStatement,
        ];
        let statements = match parse_statement(&tokens) {
            Ok(statements) => statements,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let expected_statements = {
            let mut expected_statements = Statements::new();

            let root_handle = expected_statements.add_root_statement();

            // add the statement
            {
                let handle = expected_statements.add_statement(root_handle);
                let statement =
                    match expected_statements.get_statement_mut(handle) {
                        Some(statement) => statement,
                        None => {
                            assert!(false);
                            return;
                        }
                    };

                statement.expression = match parse_expression(&tokens[1..4]) {
                    Ok(tree) => Some(tree),
                    Err(_) => {
                        assert!(false);
                        return;
                    }
                };
            }

            // add the expression
            {
                let root_statement =
                    match expected_statements.get_statement_mut(root_handle) {
                        Some(statement) => statement,
                        None => {
                            assert!(false);
                            return;
                        }
                    };
                root_statement.expression =
                    match parse_expression(&tokens[5..8]) {
                        Ok(tree) => Some(tree),
                        Err(_) => {
                            assert!(false);
                            return;
                        }
                    };
            }

            expected_statements
        };

        debug_print(&statements, &expected_statements);
        assert!(equivalent(&statements, &expected_statements));
    }

    #[test]
    fn scoped_statements() {
        let tokens = vec![
            Token::LBrace,
            // start scope
            Token::LBrace,
            // statement
            Token::IntLiteral(1),
            Token::Plus,
            Token::IntLiteral(2),
            Token::EndStatement,
            // statement
            Token::IntLiteral(3),
            Token::Multiply,
            Token::IntLiteral(4),
            Token::EndStatement,
            Token::RBrace,
            Token::EndStatement,
            // end scope
            // expression
            Token::IntLiteral(5),
            Token::Plus,
            Token::IntLiteral(6),
            Token::RBrace,
            Token::EndStatement,
        ];
        let statements = match parse_statement(&tokens) {
            Ok(statement) => statement,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let expected_statements = {
            let mut expected_statements = Statements::new();

            let root_handle = expected_statements.add_root_statement();

            // add scope statement to the root statement
            let scope_handle = expected_statements.add_statement(root_handle);
            // add first statement to scope
            {
                let handle = expected_statements.add_statement(scope_handle);
                let statement =
                    match expected_statements.get_statement_mut(handle) {
                        Some(statement) => statement,
                        None => {
                            assert!(false);
                            return;
                        }
                    };
                statement.expression = match parse_expression(&tokens[2..5]) {
                    Ok(tree) => Some(tree),
                    Err(_) => {
                        assert!(false);
                        return;
                    }
                };
            }
            // add second statement to the scope
            {
                let handle = expected_statements.add_statement(scope_handle);
                let statement =
                    match expected_statements.get_statement_mut(handle) {
                        Some(statement) => statement,
                        None => {
                            assert!(false);
                            return;
                        }
                    };
                statement.expression = match parse_expression(&tokens[6..9]) {
                    Ok(tree) => Some(tree),
                    Err(_) => {
                        assert!(false);
                        return;
                    }
                };
            }

            // add the expression
            {
                let statement =
                    match expected_statements.get_statement_mut(root_handle) {
                        Some(statement) => statement,
                        None => {
                            assert!(false);
                            return;
                        }
                    };
                statement.expression = match parse_expression(&tokens[12..15]) {
                    Ok(tree) => Some(tree),
                    Err(_) => {
                        assert!(false);
                        return;
                    }
                };
            }

            expected_statements
        };

        debug_print(&statements, &expected_statements);
        assert!(equivalent(&statements, &expected_statements));
    }

    #[test]
    fn scoped_expression() {
        let tokens = vec![
            Token::LBrace,
            // start scope
            Token::LBrace,
            // statement
            Token::IntLiteral(1),
            Token::Plus,
            Token::IntLiteral(2),
            Token::EndStatement,
            // statement
            Token::IntLiteral(3),
            Token::Multiply,
            Token::IntLiteral(4),
            Token::EndStatement,
            // scope expression
            Token::IntLiteral(5),
            Token::Multiply,
            Token::IntLiteral(6),
            Token::RBrace,
            Token::EndStatement,
            // end scope
            // expression
            Token::IntLiteral(5),
            Token::Plus,
            Token::IntLiteral(6),
            Token::RBrace,
            Token::EndStatement,
        ];
        let statements = match parse_statement(&tokens) {
            Ok(statement) => statement,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let expected_statements = {
            let mut expected_statements = Statements::new();

            let root_handle = expected_statements.add_root_statement();

            // add scope statement to the root statement
            let scope_handle = expected_statements.add_statement(root_handle);
            // add first statement to scope
            {
                let handle = expected_statements.add_statement(scope_handle);
                let statement =
                    match expected_statements.get_statement_mut(handle) {
                        Some(statement) => statement,
                        None => {
                            assert!(false);
                            return;
                        }
                    };
                statement.expression = match parse_expression(&tokens[2..5]) {
                    Ok(tree) => Some(tree),
                    Err(_) => {
                        assert!(false);
                        return;
                    }
                };
            }
            // add second statement to the scope
            {
                let handle = expected_statements.add_statement(scope_handle);
                let statement =
                    match expected_statements.get_statement_mut(handle) {
                        Some(statement) => statement,
                        None => {
                            assert!(false);
                            return;
                        }
                    };
                statement.expression = match parse_expression(&tokens[6..9]) {
                    Ok(tree) => Some(tree),
                    Err(_) => {
                        assert!(false);
                        return;
                    }
                };
            }
            // add expression to the scope
            {
                let statement =
                    match expected_statements.get_statement_mut(scope_handle) {
                        Some(statement) => statement,
                        None => {
                            assert!(false);
                            return;
                        }
                    };
                statement.expression = match parse_expression(&tokens[10..13]) {
                    Ok(tree) => Some(tree),
                    Err(_) => {
                        assert!(false);
                        return;
                    }
                };
            }

            // add the expression
            {
                let statement =
                    match expected_statements.get_statement_mut(root_handle) {
                        Some(statement) => statement,
                        None => {
                            assert!(false);
                            return;
                        }
                    };
                statement.expression = match parse_expression(&tokens[15..18]) {
                    Ok(tree) => Some(tree),
                    Err(_) => {
                        assert!(false);
                        return;
                    }
                };
            }

            expected_statements
        };

        debug_print(&statements, &expected_statements);
        assert!(equivalent(&statements, &expected_statements));
    }

    #[test]
    fn variable_declaration() {
        let tokens = vec![
            Token::Identifier("i32".to_string()),
            Token::Identifier("foo".to_string()),
            Token::Assignment,
            Token::Assignment,
            Token::IntLiteral(1),
            Token::EndStatement,
        ];

        let statements = match parse_statement(&tokens) {
            Ok(result) => result,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let expected_statements = {
            let mut expected_statements = Statements::new();

            let root_handle = expected_statements.add_root_statement();

            let statement =
                match expected_statements.get_statement_mut(root_handle) {
                    Some(statement) => statement,
                    None => {
                        assert!(false);
                        return;
                    }
                };

            statement.type_declaration = Some("i32".to_string());
            statement.variable = Some("foo".to_string());
            statement.expression = match parse_expression(&tokens[4..5]) {
                Ok(tree) => Some(tree),
                Err(_) => {
                    assert!(false);
                    return;
                },
            };

            expected_statements
        };

        debug_print(&statements, &expected_statements);
        assert!(equivalent(&statements, &expected_statements));
    }

    #[test]
    fn name() {
        unimplemented!();
    }

    #[test]
    fn variable_declaration_no_assignment() {
        unimplemented!();
    }

    #[test]
    fn variable_assignment() {
        unimplemented!();
    }

    #[test]
    fn missing_assignment_rhs() {
        unimplemented!();
    }

    #[test]
    fn error_reporting() {
        unimplemented!();
    }
}
