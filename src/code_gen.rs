/*
TODO:
documentation
collapse unary + primary into a new primary
collapse binary + two-primaries into a new primary
*/
use std::{collections::HashMap, format, todo};

use crate::{
    scanner::Token,
    statements::{Statements, StatementsDfs},
    syntax_tree::{SyntaxTree, SyntaxTreeNodeType},
};

pub enum CodeGenError {
    UndeclaredVariable,
}

pub fn generate(statements: &Statements) -> Result<String, CodeGenError> {
    // TODO: put type declaration on the value side
    let mut stack_data = HashMap::<VariableStackKey, VariableStackValue>::new();
    let mut stack_location = 0;

    let mut result = String::new();
    for search_entry in StatementsDfs::new(statements) {
        let statement = match statements.get_statement(search_entry.handle) {
            Some(statement) => statement,
            None => panic!(),
        };

        let (expression_string, expression_stored_at) =
            match &statement.expression {
                Some(tree) => match generate_expression_code(
                    tree,
                    &stack_data,
                    search_entry.depth,
                ) {
                    Ok(expression_code) => expression_code,
                    Err(err) => return Err(err),
                },
                None => ("".to_string(), Some(0)),
            };

        // store_data is a tuple with the type string first and the location
        // -- second
        let store_data = if let Some(variable) = &statement.variable {
            if let Some(type_declaration) = &statement.type_declaration {
                // track a new variable location on the stack
                let saved_location = stack_location;
                stack_data.insert(
                    VariableStackKey {
                        name: variable.clone(),
                        scope_depth: search_entry.depth,
                    },
                    VariableStackValue {
                        location: stack_location,
                        type_declaration: type_declaration.clone(),
                    },
                );
                stack_location += 1;

                Some((type_declaration.clone(), saved_location))
            } else {
                // find the variable's location in memory
                match stack_data.get(&VariableStackKey {
                    name: variable.clone(),
                    scope_depth: search_entry.depth,
                }) {
                    Some(stack_value) => Some((
                        stack_value.type_declaration.clone(),
                        stack_value.location,
                    )),
                    None => return Err(CodeGenError::UndeclaredVariable),
                }
            }
        } else {
            None
        };

        if let Some(store_data) = store_data {
            let store_type = store_data.0;
            let store_location = store_data.1;
            if let Some(expression_stored_at) = expression_stored_at {
                result.push_str(&expression_string);
                result.push_str(&format!(
                    "store {store_type}:{store_location}, ${expression_stored_at}\n"
                ));
            } else {
                result.push_str(&format!(
                    "store {store_type}:{store_location}, {expression_string}\n"
                ));
            }
        }
    }

    Ok(result)
}

fn generate_expression_code(
    tree: &SyntaxTree,
    stack_data: &HashMap<VariableStackKey, VariableStackValue>,
    depth: i32,
) -> Result<(String, Option<i32>), CodeGenError> {
    if tree.is_single_element() {
        // special case: must be primary
        let root_handle = match tree.get_root_handle() {
            Some(root_handle) => root_handle,
            None => panic!(),
        };

        let root = match tree.get_node(root_handle) {
            Some(root) => root,
            None => panic!(),
        };

        let expression = match &root.node_type {
            SyntaxTreeNodeType::Primary(token) => match token {
                Token::StringLiteral(string) => string.clone(),
                Token::IntLiteral(int) => int.to_string(),
                Token::UintLiteral(uint) => uint.to_string(),
                Token::FloatLiteral(float) => format!("{:.}", float),
                Token::Identifier(name) => {
                    let (store_type, store_location) =
                        match stack_data.get(&VariableStackKey {
                            name: name.clone(),
                            scope_depth: depth,
                        }) {
                            Some(stack_value) => (
                                stack_value.type_declaration.clone(),
                                stack_value.location,
                            ),
                            None => {
                                return Err(CodeGenError::UndeclaredVariable)
                            }
                        };
                    format!("{store_type}:{store_location}")
                }
                _ => panic!(),
            },
            _ => panic!(),
        };

        return Ok((expression, None));
    }

    let mut result = String::new();
    let mut tree = CodeGenTree::from_syntax_tree(tree);

    let root_handle = match tree.get_root_handle() {
        Some(root_handle) => root_handle,
        None => return Ok((result, Some(0))),
    };

    let mut stack: Vec<CodeGenTreeNodeHandle> = vec![root_handle];
    let mut store_at = 0;

    while let Some(node_handle) = stack.pop() {
        let (node_type, children) = match tree.get_node(node_handle) {
            Some(node) => {
                let node_type = node.node_type.clone();
                let children = &node.children;
                (node_type, children)
            }
            None => todo!(), // something has gone horribly wrong
        };

        let ready_to_evaluate: bool = {
            let mut ready_to_evaluate = true;
            for child_handle in children {
                let child = match tree.get_node(*child_handle) {
                    Some(child) => child,
                    None => {
                        // this should never happen
                        todo!();
                    }
                };

                match &child.node_type {
                    // most primaries are considered evaluated
                    SyntaxTreeNodeType::Primary(token) => {
                        match token {
                            Token::StringLiteral(_) => {},
                            Token::IntLiteral(_) => {},
                            Token::UintLiteral(_) => {},
                            Token::FloatLiteral(_) => {},
                            Token::Identifier(_) => {
                                if child.stored_at == None {
                                    ready_to_evaluate = false;
                                    break;
                                }
                            },
                            _ => panic!(),
                        }
                    }
                    _ => {
                        // children with stored result are also considered evaluated
                        if child.stored_at == None {
                            ready_to_evaluate = false;
                            break;
                        }
                    }
                };
            }

            ready_to_evaluate
        };

        if ready_to_evaluate {
            match node_type {
                SyntaxTreeNodeType::Expression => {
                    todo!();
                }
                SyntaxTreeNodeType::Equality(token) => {
                    binary_evaluate(
                        &mut tree,
                        node_handle,
                        token,
                        &mut store_at,
                        &mut result,
                    );
                }
                SyntaxTreeNodeType::Comparison(token) => {
                    binary_evaluate(
                        &mut tree,
                        node_handle,
                        token,
                        &mut store_at,
                        &mut result,
                    );
                }
                SyntaxTreeNodeType::Term(token) => {
                    binary_evaluate(
                        &mut tree,
                        node_handle,
                        token,
                        &mut store_at,
                        &mut result,
                    );
                }
                SyntaxTreeNodeType::Factor(token) => {
                    binary_evaluate(
                        &mut tree,
                        node_handle,
                        token,
                        &mut store_at,
                        &mut result,
                    );
                }
                SyntaxTreeNodeType::Unary(token) => {
                    unary_evaluate(
                        &mut tree,
                        node_handle,
                        token,
                        store_at,
                        &mut result,
                    );
                }
                SyntaxTreeNodeType::Primary(token) => {
                    match primary_evaluate(
                        &mut tree,
                        &stack_data,
                        depth,
                        node_handle,
                        token,
                        &mut store_at,
                        &mut result,
                    ) {
                        Ok(_) => {},
                        Err(err) => return Err(err),
                    };
                }
            }
        } else {
            // if not ready to evaluate, add node and children back onto the
            // -- stack (order matters)
            stack.push(node_handle);
            for child_handle in children {
                stack.push(*child_handle);
            }
        }
    }

    // store_at -1 b/c store_at variable retains the *next* position
    Ok((result, Some(store_at - 1)))
}

fn get_child_rep(tree: &CodeGenTree, child: CodeGenTreeNodeHandle) -> String {
    let node = match tree.get_node(child) {
        Some(node) => node,
        None => todo!(),
    };

    match node.stored_at {
        Some(stored_at) => format!("${:?}", stored_at),
        None => match &node.node_type {
            SyntaxTreeNodeType::Primary(token) => match token {
                Token::IntLiteral(value) => value.to_string(),
                Token::UintLiteral(value) => value.to_string(),
                Token::FloatLiteral(value) => value.to_string(),
                _ => panic!(),
            },
            _ => todo!(),
        },
    }
}

fn binary_evaluate(
    tree: &mut CodeGenTree,
    node_handle: CodeGenTreeNodeHandle,
    token: Token,
    store_at: &mut i32,
    result: &mut String,
) {
    let stored_at = {
        let node = match tree.get_node_mut(node_handle) {
            Some(node) => node,
            None => todo!(),
        };
        node.stored_at = Some(*store_at);
        *store_at
    };
    *store_at += 1;

    let node = match tree.get_node(node_handle) {
        Some(node) => node,
        None => todo!(),
    };

    let children = &node.children;
    let child_0_rep = get_child_rep(tree, children[0]);
    let child_1_rep = get_child_rep(tree, children[1]);

    let string = match token {
        Token::Equivalence => {
            format!("== ${stored_at}, {child_0_rep}, {child_1_rep}\n")
        }
        Token::NotEqual => {
            format!("!= ${stored_at}, {child_0_rep}, {child_1_rep}\n")
        }
        Token::LessThan => {
            format!("< ${stored_at}, {child_0_rep}, {child_1_rep}\n")
        }
        Token::LessThanEqualTo => {
            format!("<= ${stored_at}, {child_0_rep}, {child_1_rep}\n")
        }
        Token::GreaterThan => {
            format!("> ${stored_at}, {child_0_rep}, {child_1_rep}\n")
        }
        Token::GreaterThanEqualTo => {
            format!(">= ${stored_at}, {child_0_rep}, {child_1_rep}\n")
        }
        Token::Plus => {
            format!("+ ${stored_at}, {child_0_rep}, {child_1_rep}\n")
        }
        Token::Minus => {
            format!("- ${stored_at}, {child_0_rep}, {child_1_rep}\n")
        }
        Token::Multiply => {
            format!("* ${stored_at}, {child_0_rep}, {child_1_rep}\n")
        }
        Token::Divide => {
            format!("/ ${stored_at}, {child_0_rep}, {child_1_rep}\n")
        }
        _ => {
            // this should never happen
            todo!();
        }
    };

    result.push_str(&string);
}

fn unary_evaluate(
    tree: &mut CodeGenTree,
    node_handle: CodeGenTreeNodeHandle,
    token: Token,
    stored_at: i32,
    result: &mut String,
) {
    // no need to increment store_at since this is a unary operator
    // (saved in same place that child is saved in)

    let node = match tree.get_node(node_handle) {
        Some(node) => node,
        None => todo!(),
    };

    let child_rep = get_child_rep(tree, node.children[0]);

    let string = match token {
        Token::Minus => {
            format!("negate ${:?}, {}\n", stored_at, child_rep)
        }
        Token::Not => {
            format!("not ${:?}, {}\n", stored_at, child_rep)
        }
        _ => {
            // this should never happen
            todo!();
        }
    };

    result.push_str(&string);
}

fn primary_evaluate(
    tree: &mut CodeGenTree,
    stack_data: &HashMap<VariableStackKey, VariableStackValue>,
    scope_depth: i32,
    node_handle: CodeGenTreeNodeHandle,
    token: Token,
    store_at: &mut i32,
    result: &mut String,
) -> Result<(), CodeGenError> {
    match token {
        Token::Identifier(name) => {
            let stored_at = {
                let node = match tree.get_node_mut(node_handle) {
                    Some(node) => node,
                    None => todo!(),
                };
                node.stored_at = Some(*store_at);
                *store_at
            };
            *store_at += 1;

            let (store_type, store_location) =
                match stack_data.get(&VariableStackKey {
                    name: name.clone(),
                    scope_depth: scope_depth,
                }) {
                    Some(stack_value) => (
                        stack_value.type_declaration.clone(),
                        stack_value.location,
                    ),
                    None => {
                        return Err(CodeGenError::UndeclaredVariable)
                    }
                };

            let string = format!("load ${stored_at}, {store_type}:{store_location}\n");

            result.push_str(&string);
        },
        Token::IntLiteral(_) => {},
        Token::UintLiteral(_) => {},
        Token::FloatLiteral(_) => {},
        _ => {
            // this should never happen
            todo!();
        }
    };

    Ok(())
}

#[derive(Default, Debug, Copy, Clone, PartialEq)]
struct CodeGenTreeNodeHandle {
    index: usize,
}

struct CodeGenTreeNode {
    node_type: SyntaxTreeNodeType,
    stored_at: Option<i32>,
    children: Vec<CodeGenTreeNodeHandle>,
}

struct CodeGenTree {
    nodes: Vec<CodeGenTreeNode>,
}

impl CodeGenTree {
    pub fn from_syntax_tree(syntax_tree: &SyntaxTree) -> Self {
        let mut nodes = vec![];
        for node in syntax_tree.iter() {
            let mut children = vec![];
            for child in &node.children {
                children.push(CodeGenTreeNodeHandle { index: child.index })
            }
            nodes.push(CodeGenTreeNode {
                node_type: node.node_type.clone(),
                stored_at: None,
                children,
            });
        }

        Self { nodes }
    }

    pub fn get_root_handle(&self) -> Option<CodeGenTreeNodeHandle> {
        if self.nodes.is_empty() {
            None
        } else {
            Some(CodeGenTreeNodeHandle { index: 0 })
        }
    }

    pub fn get_node_mut(
        &mut self,
        handle: CodeGenTreeNodeHandle,
    ) -> Option<&mut CodeGenTreeNode> {
        self.nodes.get_mut(handle.index)
    }

    pub fn get_node(
        &self,
        handle: CodeGenTreeNodeHandle,
    ) -> Option<&CodeGenTreeNode> {
        self.nodes.get(handle.index)
    }
}

#[derive(Hash, PartialEq, Eq)]
struct VariableStackKey {
    name: String,
    scope_depth: i32,
}

struct VariableStackValue {
    location: i32,
    type_declaration: String,
}

#[cfg(test)]
mod tests {
    use std::{assert_eq, vec};

    use super::*;
    use crate::{
        expression_parser::parse_expression, statement_parser::parse_statement,
    };

    #[test]
    fn primary_only() {
        let tree = match parse_expression(&vec![Token::IntLiteral(1)]) {
            Ok(tree) => tree,
            Err(_) => {
                assert!(false);
                return;
            }
        };
        let (code, _) = match generate_expression_code(
            &tree,
            &HashMap::<VariableStackKey, VariableStackValue>::new(),
            0
        ) {
            Ok(expression_code) => expression_code,
            Err(_) => {
                assert!(false);
                return;
            },
        };

        let expected = "1";
        assert_eq!(code, expected);
    }

    #[test]
    fn add_two() {
        let tree = match parse_expression(&vec![
            Token::IntLiteral(1),
            Token::Plus,
            Token::IntLiteral(2),
        ]) {
            Ok(tree) => tree,
            Err(_) => {
                assert!(false);
                return;
            }
        };
        let (code, _) = match generate_expression_code(
            &tree,
            &HashMap::<VariableStackKey, VariableStackValue>::new(),
            0
        ) {
            Ok(expression_code) => expression_code,
            Err(_) => {
                assert!(false);
                return;
            },
        };

        let expected = concat!("+ $0, 1, 2\n");
        assert_eq!(code, expected);
    }

    #[test]
    fn negate() {
        let tree =
            match parse_expression(&vec![Token::Minus, Token::IntLiteral(1)]) {
                Ok(tree) => tree,
                Err(_) => {
                    assert!(false);
                    return;
                }
            };
        let (code, _) = match generate_expression_code(
            &tree,
            &HashMap::<VariableStackKey, VariableStackValue>::new(),
            0
        ) {
            Ok(expression_code) => expression_code,
            Err(_) => {
                assert!(false);
                return;
            },
        };

        let expected = concat!("negate $0, 1\n");
        assert_eq!(code, expected);
    }

    // multiple unary operators strung together should only use one register
    #[test]
    fn multiple_negations() {
        unimplemented!();
    }

    #[test]
    fn no_group_precedence() {
        let tree = match parse_expression(&vec![
            Token::IntLiteral(1),
            Token::Plus,
            Token::IntLiteral(2),
            Token::Multiply,
            Token::IntLiteral(3),
            Token::Minus,
            Token::IntLiteral(4),
        ]) {
            Ok(tree) => tree,
            Err(_) => {
                assert!(false);
                return;
            }
        };
        let (code, _) = match generate_expression_code(
            &tree,
            &HashMap::<VariableStackKey, VariableStackValue>::new(),
            0
        ) {
            Ok(expression_code) => expression_code,
            Err(_) => {
                assert!(false);
                return;
            },
        };

        let expected =
            concat!("* $0, 2, 3\n", "+ $1, 1, $0\n", "- $2, $1, 4\n",);
        assert_eq!(code, expected);
    }

    #[test]
    fn group_precedence() {
        let tree = match parse_expression(&vec![
            Token::LParen,
            Token::IntLiteral(1),
            Token::Plus,
            Token::IntLiteral(2),
            Token::RParen,
            Token::Multiply,
            Token::LParen,
            Token::IntLiteral(3),
            Token::Minus,
            Token::IntLiteral(4),
            Token::RParen,
        ]) {
            Ok(tree) => tree,
            Err(_) => {
                assert!(false);
                return;
            }
        };
        let (code, _) = match generate_expression_code(
            &tree,
            &HashMap::<VariableStackKey, VariableStackValue>::new(),
            0
        ) {
            Ok(expression_code) => expression_code,
            Err(_) => {
                assert!(false);
                return;
            },
        };

        let expected =
            concat!("- $0, 3, 4\n", "+ $1, 1, 2\n", "* $2, $1, $0\n",);
        assert_eq!(code, expected);
    }

    #[test]
    fn no_side_effect_statement() {
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

        let code = match generate(&statements) {
            Ok(code) => code,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let expected = concat!("");

        assert_eq!(code, expected);
    }

    #[test]
    fn declare_and_assign_literal() {
        let tokens = vec![
            Token::Identifier("i32".to_string()),
            Token::Identifier("foo".to_string()),
            Token::Assignment,
            Token::IntLiteral(1),
            Token::EndStatement,
        ];
        let statements = match parse_statement(&tokens) {
            Ok(statements) => statements,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let code = match generate(&statements) {
            Ok(code) => code,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let expected = concat!("store i32:0, 1\n");

        assert_eq!(code, expected);
    }

    #[test]
    fn declare_and_assign() {
        let tokens = vec![
            Token::Identifier("i32".to_string()),
            Token::Identifier("foo".to_string()),
            Token::Assignment,
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

        let code = match generate(&statements) {
            Ok(code) => code,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let expected = concat!("+ $0, 1, 2\n", "store i32:0, $0\n");

        assert_eq!(code, expected);
    }

    #[test]
    fn multiple_declarations() {
        let tokens = vec![
            Token::LBrace,
            // statement 1
            Token::Identifier("i32".to_string()),
            Token::Identifier("foo".to_string()),
            Token::Assignment,
            Token::IntLiteral(1),
            Token::EndStatement,
            // statement 2
            Token::Identifier("f32".to_string()),
            Token::Identifier("bar".to_string()),
            Token::Assignment,
            Token::FloatLiteral(1.0),
            Token::EndStatement,
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

        let code = match generate(&statements) {
            Ok(code) => code,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let expected = concat!("store i32:0, 1\n", "store f32:1, 1\n");

        assert_eq!(code, expected);
    }

    #[test]
    fn assign_after_declaration() {
        let tokens = vec![
            Token::LBrace,
            // statement 1
            Token::Identifier("i32".to_string()),
            Token::Identifier("foo".to_string()),
            Token::Assignment,
            Token::IntLiteral(1),
            Token::EndStatement,
            // statement 2
            Token::Identifier("foo".to_string()),
            Token::Assignment,
            Token::IntLiteral(2),
            Token::EndStatement,
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

        let code = match generate(&statements) {
            Ok(code) => code,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let expected = concat!("store i32:0, 1\n", "store i32:0, 2\n");

        assert_eq!(code, expected);
    }

    #[test]
    fn no_assignment_during_declaration() {
        let tokens = vec![
            Token::Identifier("foo".to_string()),
            Token::Assignment,
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

        match generate(&statements) {
            Ok(_) => assert!(false),
            Err(_) => {}
        };
    }

    #[test]
    fn use_undeclared_variable() {
        unimplemented!();
    }

    #[test]
    fn redeclare_variable() {
        unimplemented!();
    }

    #[test]
    fn assignment_to_identifier() {
        let tokens = vec![
            Token::LBrace,
            // statement 1
            Token::Identifier("i32".to_string()),
            Token::Identifier("foo".to_string()),
            Token::Assignment,
            Token::IntLiteral(1),
            Token::EndStatement,
            // statement 2
            Token::Identifier("i32".to_string()),
            Token::Identifier("bar".to_string()),
            Token::Assignment,
            Token::IntLiteral(2),
            Token::EndStatement,
            // statement 3
            Token::Identifier("bar".to_string()),
            Token::Assignment,
            Token::Identifier("foo".to_string()),
            Token::EndStatement,
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

        let code = match generate(&statements) {
            Ok(code) => code,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let expected = concat!(
            "store i32:0, 1\n",
            "store i32:1, 2\n",
            "store i32:1, i32:0\n"
        );

        assert_eq!(code, expected);
    }

    #[test]
    fn assignment_to_identifier_expression() {
        let tokens = vec![
            Token::LBrace,
            // statement 1
            Token::Identifier("i32".to_string()),
            Token::Identifier("foo".to_string()),
            Token::Assignment,
            Token::IntLiteral(1),
            Token::EndStatement,
            // statement 2
            Token::Identifier("i32".to_string()),
            Token::Identifier("bar".to_string()),
            Token::Assignment,
            Token::IntLiteral(2),
            Token::EndStatement,
            // statement 3
            Token::Identifier("bar".to_string()),
            Token::Assignment,
            Token::Identifier("foo".to_string()),
            Token::Plus,
            Token::IntLiteral(3),
            Token::EndStatement,
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

        let code = match generate(&statements) {
            Ok(code) => code,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let expected = concat!(
            "store i32:0, 1\n",
            "store i32:1, 2\n",
            "load $0, i32:0\n",
            "+ $1, $0, 3\n",
            "store i32:1, $1\n"
        );

        assert_eq!(code, expected);
    }

    #[test]
    fn lexical_scope_error() {
        unimplemented!();
    }
}
