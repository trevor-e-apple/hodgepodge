/* TODO:
    profiling
*/

use std::{todo, vec};

use crate::scanner::Token;
use crate::syntax_tree::{
    GrammarRule, SyntaxTree, SyntaxTreeNode, SyntaxTreeNodeHandle,
    SyntaxTreeNodeType,
};

struct StackEntry {
    rule: GrammarRule,
    parent_handle: Option<SyntaxTreeNodeHandle>,
    start_index: usize,
    end_index: usize, // one past the index of the final element
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    Success,
    MissingToken,
    MismatchedGrouping,
    UnexpectedToken,
}

const EQUALITY_MATCHING_OPS: [Token; 2] = [Token::Equivalence, Token::NotEqual];
const COMPARISON_MATCHING_OPS: [Token; 4] = [
    Token::GreaterThan,
    Token::GreaterThanEqualTo,
    Token::LessThan,
    Token::LessThanEqualTo,
];
const TERM_MATCHING_OPS: [Token; 2] = [Token::Minus, Token::Plus];
const FACTOR_MATCHING_OPS: [Token; 2] = [Token::Multiply, Token::Divide];
const OP_TOKENS: [Token; 14] = [
    Token::Assignment,
    Token::Plus,
    Token::Minus,
    Token::Multiply,
    Token::Divide,
    Token::Equivalence,
    Token::NotEqual,
    Token::LessThan,
    Token::LessThanEqualTo,
    Token::GreaterThan,
    Token::GreaterThanEqualTo,
    Token::Not,
    Token::And,
    Token::Or,
];

/// A parser that takes a Vec of Tokens and produces a tree reflecting
/// the following grammar
///
/// grammar rules are sorted from least to highest precedence
///
/// due to the non-associativity of the binary operators Minus and Divide,
/// The "match 0 or more" part of the term and factor rules are in the front
///
/// expression -> equality;
/// equality -> comparison ((!= | ==) comparison)*;
/// comparison -> term ((> | >= | < | <=) term)*;
/// term -> (factor ("-" | "+") )* factor ;
/// factor -> (unary ("*" | "/") )* unary  ;
/// unary -> (("!" | "-") unary) | primary;
/// primary -> NUMBER | STRING | "true" | "false" | ("(" expression ")");
pub fn parse(tokens: &Vec<Token>) -> Result<SyntaxTree, ParseError> {
    let mut syntax_tree = SyntaxTree::new();

    let mut stack = vec![StackEntry {
        rule: GrammarRule::Expression,
        parent_handle: None,
        start_index: 0,
        end_index: tokens.len(),
    }];

    loop {
        let stack_entry = match stack.pop() {
            Some(value) => value,
            None => break,
        };
        let current_rule = stack_entry.rule;

        match current_rule {
            GrammarRule::Expression => {
                expression_expansion(&mut stack, stack_entry);
            }
            GrammarRule::Equality => {
                match binary_op_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    stack_entry,
                    &tokens,
                    &EQUALITY_MATCHING_OPS,
                    &OP_TOKENS,
                ) {
                    Ok(_) => {}
                    Err(err) => return Err(err),
                };
            }
            GrammarRule::Comparison => {
                match binary_op_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    stack_entry,
                    &tokens,
                    &COMPARISON_MATCHING_OPS,
                    &OP_TOKENS,
                ) {
                    Ok(_) => {}
                    Err(err) => return Err(err),
                };
            }
            GrammarRule::Term => {
                match binary_op_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    stack_entry,
                    &tokens,
                    &TERM_MATCHING_OPS,
                    &OP_TOKENS,
                ) {
                    Ok(_) => {}
                    Err(err) => return Err(err),
                };
            }
            GrammarRule::Factor => {
                match binary_op_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    stack_entry,
                    &tokens,
                    &FACTOR_MATCHING_OPS,
                    &OP_TOKENS,
                ) {
                    Ok(_) => {}
                    Err(err) => return Err(err),
                };
            }
            GrammarRule::Unary => {
                // no need for matching ops vector. since unary operators don't
                // -- have precedence, we don't need to factor out the code,
                // -- which means we don't need this data to configure the
                // -- function behavior
                match unary_op_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    stack_entry,
                    &tokens,
                ) {
                    Ok(_) => {}
                    Err(err) => return Err(err),
                };
            }
            GrammarRule::Primary => {
                match primary_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    stack_entry,
                    &tokens,
                ) {
                    Ok(_) => {}
                    Err(err) => return Err(err),
                };
            }
        };
    }

    Ok(syntax_tree)
}

// TODO: documentation
fn expression_expansion(stack: &mut Vec<StackEntry>, stack_entry: StackEntry) {
    stack.push(StackEntry {
        rule: GrammarRule::Equality,
        parent_handle: stack_entry.parent_handle,
        start_index: stack_entry.start_index,
        end_index: stack_entry.end_index,
    });
}

fn binary_op_expansion(
    syntax_tree: &mut SyntaxTree,
    stack: &mut Vec<StackEntry>,
    stack_entry: StackEntry,
    tokens: &Vec<Token>,
    matching_op_tokens: &[Token],
    op_tokens: &[Token],
) -> Result<ParseError, ParseError> {
    let start_index = stack_entry.start_index;
    let end_index = stack_entry.end_index;

    let mut op_index_token: Option<(usize, Token)> = None;

    // since we don't want to have to "unroll" the tree we use this group depth
    // -- hack
    let mut group_depth = 0;
    // going in reverse order is equivalent to the approach where you match
    // 0 or more in the front
    for index in (start_index..end_index).into_iter().rev() {
        let token = match tokens.get(index) {
            Some(token) => token,
            None => todo!(),
        };

        if *token == Token::LParen {
            group_depth -= 1;
            if group_depth < 0 {
                return Err(ParseError::MismatchedGrouping);
            }
        } else if *token == Token::RParen {
            group_depth += 1;
        }

        // need to check what the previous token is. if it's an operation, then
        // -- the current token cannot be a binary operator
        // a bit hacky and it means we can't handle arbitrary grammars, but
        // -- this probably parses faster than implementing a whole scheme for
        // -- retrying different parsing trees until you find a valid one
        let prev_token_is_op = if index > 0 {
            match tokens.get(index - 1) {
                Some(prev_token) => {
                    op_tokens.into_iter().any(|x| *x == *prev_token)
                }
                None => false,
            }
        } else {
            false
        };

        if (group_depth == 0)
            && matching_op_tokens.into_iter().any(|x| *x == *token)
            && !prev_token_is_op
        {
            op_index_token = Some((index, token.clone()));
            break;
        }
    }

    if group_depth > 0 {
        return Err(ParseError::MismatchedGrouping);
    }

    match op_index_token {
        Some(op_index_token) => {
            let (op_index, token) = op_index_token;
            if op_index == start_index {
                // missing LHS, could be a unary op
                binary_op_next_rule(stack, &stack_entry);
            } else if (op_index + 1) == end_index {
                return Err(ParseError::MissingToken);
            } else {
                let parent_node_type = match stack_entry.rule {
                    GrammarRule::Equality => {
                        SyntaxTreeNodeType::Equality(token)
                    }
                    GrammarRule::Comparison => {
                        SyntaxTreeNodeType::Comparison(token)
                    }
                    GrammarRule::Term => SyntaxTreeNodeType::Term(token),
                    GrammarRule::Factor => SyntaxTreeNodeType::Factor(token),
                    _ => {
                        // not a binary op. should never happen
                        todo!()
                    }
                };
                let parent_handle = add_to_tree(
                    syntax_tree,
                    parent_node_type,
                    stack_entry.parent_handle,
                );

                // add LHS and RHS to queue

                // rhs added to stack first so that when they are added as 
                // -- children later, RHS will be added after LHS
                // RHS moves on to the next rule
                let rhs_rule = match stack_entry.rule {
                    GrammarRule::Equality => GrammarRule::Comparison,
                    GrammarRule::Comparison => GrammarRule::Term,
                    GrammarRule::Term => GrammarRule::Factor,
                    GrammarRule::Factor => GrammarRule::Unary,
                    _ => {
                        // not a binary expression. error here
                        todo!();
                    }
                };
                stack.push(StackEntry {
                    rule: rhs_rule,
                    parent_handle: Some(parent_handle),
                    start_index: op_index + 1,
                    end_index: end_index,
                });

                // LHS stays on the same rule
                let lhs_rule = match stack_entry.rule {
                    GrammarRule::Equality => GrammarRule::Equality,
                    GrammarRule::Comparison => GrammarRule::Comparison,
                    GrammarRule::Term => GrammarRule::Term,
                    GrammarRule::Factor => GrammarRule::Factor,
                    _ => {
                        // not a binary operation. error here
                        todo!();
                    }
                };

                stack.push(StackEntry {
                    rule: lhs_rule,
                    parent_handle: Some(parent_handle),
                    start_index: start_index,
                    end_index: op_index,
                });
            }
        }
        None => {
            // if op not found, you move on to the next rule
            binary_op_next_rule(stack, &stack_entry);
        }
    };

    Ok(ParseError::Success)
}

/// helper function for binary op expansion for moving onto the next expansion
/// rule
fn binary_op_next_rule(stack: &mut Vec<StackEntry>, stack_entry: &StackEntry) {
    let rule = match stack_entry.rule {
        GrammarRule::Equality => GrammarRule::Comparison,
        GrammarRule::Comparison => GrammarRule::Term,
        GrammarRule::Term => GrammarRule::Factor,
        GrammarRule::Factor => GrammarRule::Unary,
        _ => {
            // not a binary operation. error here
            todo!();
        }
    };
    stack.push(StackEntry { rule: rule, ..*stack_entry });
}

fn unary_op_expansion(
    syntax_tree: &mut SyntaxTree,
    stack: &mut Vec<StackEntry>,
    stack_entry: StackEntry,
    tokens: &Vec<Token>,
) -> Result<ParseError, ParseError> {
    let start_index = stack_entry.start_index;
    let end_index = stack_entry.end_index;

    let mut op_index_token: Option<(usize, Token)> = None;

    let mut group_depth = 0;
    for index in start_index..end_index {
        let token = match tokens.get(index) {
            Some(token) => token,
            None => todo!(),
        };
        if *token == Token::LParen {
            group_depth += 1;
        } else if *token == Token::RParen {
            group_depth -= 1;
            if group_depth < 0 {
                return Err(ParseError::MismatchedGrouping);
            }
        }

        if (group_depth == 0)
            && (*token == Token::Minus || *token == Token::Not)
        {
            op_index_token = Some((index, token.clone()));
            break;
        }
    }

    if group_depth > 0 {
        return Err(ParseError::MismatchedGrouping);
    }

    match op_index_token {
        Some(op_index_token) => {
            let (op_index, token) = op_index_token;
            // add parent to the syntax tree
            let parent_node_type = match stack_entry.rule {
                GrammarRule::Unary => SyntaxTreeNodeType::Unary(token),
                _ => {
                    // handle this error path, should never happen
                    todo!()
                }
            };
            let parent_handle = add_to_tree(
                syntax_tree,
                parent_node_type,
                stack_entry.parent_handle,
            );

            // add another unary node
            stack.push(StackEntry {
                rule: GrammarRule::Unary,
                parent_handle: Some(parent_handle),
                start_index: op_index + 1,
                end_index: stack_entry.end_index,
            });
        }
        None => {
            // if op not found, move on to the next rule
            stack
                .push(StackEntry { rule: GrammarRule::Primary, ..stack_entry });
        }
    };

    Ok(ParseError::Success)
}

fn primary_expansion(
    syntax_tree: &mut SyntaxTree,
    stack: &mut Vec<StackEntry>,
    stack_entry: StackEntry,
    tokens: &Vec<Token>,
) -> Result<ParseError, ParseError> {
    let start_index = stack_entry.start_index;
    let end_index = stack_entry.end_index;

    let first_token = match tokens.get(start_index) {
        Some(token) => token.clone(),
        None => return Err(ParseError::MissingToken),
    };

    if first_token == Token::LParen {
        // we are matching an expression group

        let mut lparens_found = 1;
        let mut rparens_found = 0;
        for index in (start_index + 1)..end_index {
            let token = match tokens.get(index) {
                Some(token) => token,
                None => return Err(ParseError::MismatchedGrouping),
            };

            match token {
                Token::LParen => {
                    lparens_found += 1;
                }
                Token::RParen => {
                    // we have found the end of expression group
                    rparens_found += 1;
                    if lparens_found == rparens_found {
                        // move on to the expression rule
                        // +1 for start index to remove the lparen
                        // no +1 for end_index b/c that's always one past
                        stack.push(StackEntry {
                            rule: GrammarRule::Expression,
                            start_index: stack_entry.start_index + 1,
                            end_index: index,
                            ..stack_entry
                        });

                        break;
                    }
                }
                _ => {}
            }
        }

        if lparens_found != rparens_found {
            return Err(ParseError::MismatchedGrouping);
        }
    } else {
        match first_token {
            Token::StringLiteral(_)
            | Token::IntLiteral(_)
            | Token::UintLiteral(_)
            | Token::FloatLiteral(_)
            | Token::Identifier(_)
            | Token::True
            | Token::False => {
                let node_type = SyntaxTreeNodeType::Primary(first_token);

                add_to_tree(syntax_tree, node_type, stack_entry.parent_handle);
            }
            _ => {
                // unexpected token error
                return Err(ParseError::UnexpectedToken);
            }
        }
    }

    Ok(ParseError::Success)
}

fn add_to_tree(
    syntax_tree: &mut SyntaxTree,
    node_type: SyntaxTreeNodeType,
    parent_handle: Option<SyntaxTreeNodeHandle>,
) -> SyntaxTreeNodeHandle {
    // first add the parent to the tree
    let new_node_handle = syntax_tree
        .add_node(SyntaxTreeNode { node_type: node_type, children: vec![] });

    // then add the parent to parent's children
    match parent_handle {
        Some(parent_handle) => {
            let parent_node = match syntax_tree.get_node_mut(parent_handle) {
                Some(parent_node) => parent_node,
                None => {
                    // handle error path, should never happen
                    todo!();
                }
            };
            parent_node.children.push(new_node_handle);
        }
        None => {} // root node
    }

    new_node_handle
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use crate::syntax_tree::equivalent;

    use super::*;

    fn debug_print(tree: &SyntaxTree, expected_tree: &SyntaxTree) {
        println!("Tree:");
        tree.pretty_print();
        println!("Expected tree:");
        expected_tree.pretty_print();
    }

    #[test]
    fn primary_only() {
        // 3
        let tokens = vec![Token::IntLiteral(3)];

        let tree = match parse(&tokens) {
            Ok(tree) => tree,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        // construct the expected tree
        let mut expected_tree = SyntaxTree::new();
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(3)),
            children: vec![],
        });

        debug_print(&tree, &expected_tree);
        assert!(equivalent(&tree, &expected_tree));
    }

    #[test]
    fn binary_op_only() {
        let tokens =
            vec![Token::IntLiteral(1), Token::Plus, Token::IntLiteral(2)];

        let tree = match parse(&tokens) {
            Ok(tree) => tree,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        // construct the expected tree
        let mut expected_tree = SyntaxTree::new();
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(Token::Plus),
            children: vec![
                SyntaxTreeNodeHandle::with_index(1),
                SyntaxTreeNodeHandle::with_index(2),
            ],
        });

        // LHS: 1
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(1)),
            children: vec![],
        });

        // RHS: 2
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(2)),
            children: vec![],
        });

        debug_print(&tree, &expected_tree);
        assert!(equivalent(&tree, &expected_tree));
    }

    /// test binary ops
    #[test]
    fn binary_ops() {
        // 1 + 2 - 3
        let tokens = vec![
            Token::IntLiteral(1),
            Token::Plus,
            Token::IntLiteral(2),
            Token::Minus,
            Token::IntLiteral(3),
        ];

        let tree = match parse(&tokens) {
            Ok(tree) => tree,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        // construct the expected tree
        let mut expected_tree = SyntaxTree::new();
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(Token::Minus),
            children: vec![
                SyntaxTreeNodeHandle::with_index(1),
                SyntaxTreeNodeHandle::with_index(4),
            ],
        });

        // LHS: 1 + 2
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Term(Token::Plus),
                children: vec![
                    SyntaxTreeNodeHandle::with_index(2),
                    SyntaxTreeNodeHandle::with_index(3),
                ],
            });

            // LHS: 1
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(1)),
                children: vec![],
            });

            // RHS: 2
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(2)),
                children: vec![],
            });
        }

        // RHS: 3
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(3)),
                children: vec![],
            });
        }

        debug_print(&tree, &expected_tree);
        assert!(equivalent(&tree, &expected_tree));
    }

    #[test]
    fn mixed_precedence_bin_ops() {
        // 1 + 2 * 3
        let tokens = vec![
            Token::IntLiteral(1),
            Token::Plus,
            Token::IntLiteral(2),
            Token::Multiply,
            Token::IntLiteral(3),
        ];

        let tree = match parse(&tokens) {
            Ok(tree) => tree,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        // construct the expected tree
        let mut expected_tree = SyntaxTree::new();
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(Token::Plus),
            children: vec![
                SyntaxTreeNodeHandle::with_index(1),
                SyntaxTreeNodeHandle::with_index(2),
            ],
        });

        // LHS: 1
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(1)),
                children: vec![],
            });
        }

        // RHS: 2 * 3
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Factor(Token::Multiply),
                children: vec![
                    SyntaxTreeNodeHandle::with_index(3),
                    SyntaxTreeNodeHandle::with_index(4),
                ],
            });

            // LHS: 2
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(2)),
                children: vec![],
            });

            // RHS: 3
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(3)),
                children: vec![],
            });
        }

        debug_print(&tree, &expected_tree);
        assert!(equivalent(&tree, &expected_tree));
    }

    #[test]
    fn group_precedence() {
        // 3 * (1 + 2)
        let tokens = vec![
            Token::IntLiteral(3),
            Token::Multiply,
            Token::LParen,
            Token::IntLiteral(1),
            Token::Plus,
            Token::IntLiteral(2),
            Token::RParen,
        ];

        let tree = match parse(&tokens) {
            Ok(tree) => tree,
            Err(err) => {
                println!("{:?}", err);
                assert!(false);
                return;
            }
        };

        let mut expected_tree = SyntaxTree::new();
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Factor(Token::Multiply),
            children: vec![
                SyntaxTreeNodeHandle::with_index(1),
                SyntaxTreeNodeHandle::with_index(2),
            ],
        });

        // LHS: 3
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(3)),
                children: vec![],
            });
        }

        // RHS: (1 + 2)
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Term(Token::Plus),
                children: vec![
                    SyntaxTreeNodeHandle::with_index(3),
                    SyntaxTreeNodeHandle::with_index(4),
                ],
            });

            // LHS: 1
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(1)),
                children: vec![],
            });

            // RHS: 2
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(2)),
                children: vec![],
            });
        }

        debug_print(&tree, &expected_tree);
        assert!(equivalent(&tree, &expected_tree));
    }

    #[test]
    fn missing_rparen() {
        let tokens = vec![
            Token::LParen,
            Token::LParen,
            Token::IntLiteral(1),
            Token::RParen,
        ];

        match parse(&tokens) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err, ParseError::MismatchedGrouping),
        };
    }

    #[test]
    fn missing_lparen() {
        let tokens = vec![
            Token::LParen,
            Token::IntLiteral(1),
            Token::RParen,
            Token::RParen,
        ];

        match parse(&tokens) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err, ParseError::MismatchedGrouping),
        };
    }

    #[test]
    fn solo_unary() {
        let tokens = vec![Token::Not];

        match parse(&tokens) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err, ParseError::MissingToken),
        };
    }

    #[test]
    fn binary_no_rhs() {
        let tokens = vec![Token::IntLiteral(1), Token::Plus];

        match parse(&tokens) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err, ParseError::MissingToken),
        };
    }

    #[test]
    fn binary_no_lhs() {
        let tokens = vec![Token::Plus, Token::IntLiteral(1)];

        match parse(&tokens) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err, ParseError::UnexpectedToken),
        };
    }

    #[test]
    fn double_binary_no_sides() {
        let tokens = vec![Token::Plus, Token::Multiply];
        match parse(&tokens) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err, ParseError::UnexpectedToken),
        };
    }

    #[test]
    fn empty() {
        let tokens = vec![];
        match parse(&tokens) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err, ParseError::MissingToken),
        }
    }

    #[test]
    fn double_unary() {
        // !!true
        let tokens = vec![Token::Not, Token::Not, Token::True];
        let tree = match parse(&tokens) {
            Ok(tree) => tree,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let mut expected_tree = SyntaxTree::new();

        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Unary(Token::Not),
            children: vec![SyntaxTreeNodeHandle::with_index(1)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Unary(Token::Not),
            children: vec![SyntaxTreeNodeHandle::with_index(2)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Primary(Token::True),
            children: vec![],
        });

        debug_print(&tree, &expected_tree);
        assert!(equivalent(&tree, &expected_tree));
    }

    #[test]
    fn groups_single_op() {
        // (1 + 2) * (3 + 4)
        let tokens = vec![
            Token::LParen,
            Token::IntLiteral(1),
            Token::Plus,
            Token::IntLiteral(2),
            Token::RParen,
            Token::Multiply,
            Token::LParen,
            Token::IntLiteral(3),
            Token::Plus,
            Token::IntLiteral(4),
            Token::RParen,
        ];

        let tree = match parse(&tokens) {
            Ok(tree) => tree,
            Err(err) => {
                println!("{:?}", err);
                assert!(false);
                return;
            }
        };

        let mut expected_tree = SyntaxTree::new();
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Factor(Token::Multiply),
            children: vec![
                SyntaxTreeNodeHandle::with_index(1),
                SyntaxTreeNodeHandle::with_index(4),
            ],
        });

        // LHS: 1 + 2
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Term(Token::Plus),
                children: vec![
                    SyntaxTreeNodeHandle::with_index(2),
                    SyntaxTreeNodeHandle::with_index(3),
                ],
            });

            // LHS: 1
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(1)),
                children: vec![],
            });

            // RHS: 2
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(2)),
                children: vec![],
            });
        }
        // RHS: 3 + 4
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Term(Token::Plus),
                children: vec![
                    SyntaxTreeNodeHandle::with_index(5),
                    SyntaxTreeNodeHandle::with_index(6),
                ],
            });

            // LHS: 1
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(3)),
                children: vec![],
            });

            // RHS: 2
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(4)),
                children: vec![],
            });
        }

        debug_print(&tree, &expected_tree);
        assert!(equivalent(&tree, &expected_tree));
    }

    #[test]
    fn left_associative() {
        // (3 - 2) - 1
        let tokens = vec![
            Token::LParen,
            Token::IntLiteral(3),
            Token::Minus,
            Token::IntLiteral(2),
            Token::RParen,
            Token::Minus,
            Token::IntLiteral(1),
        ];

        let tree = match parse(&tokens) {
            Ok(tree) => tree,
            Err(err) => {
                println!("{:?}", err);
                assert!(false);
                return;
            }
        };

        let mut expected_tree = SyntaxTree::new();
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(Token::Minus),
            children: vec![
                SyntaxTreeNodeHandle::with_index(1),
                SyntaxTreeNodeHandle::with_index(4),
            ],
        });

        // LHS: 3 - 2
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Term(Token::Minus),
                children: vec![
                    SyntaxTreeNodeHandle::with_index(2),
                    SyntaxTreeNodeHandle::with_index(3),
                ],
            });

            // LHS: 3
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(3)),
                children: vec![],
            });

            // RHS: 2
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(2)),
                children: vec![],
            });
        }
        // RHS: 1
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(1)),
                children: vec![],
            });
        }

        debug_print(&tree, &expected_tree);
        assert!(equivalent(&tree, &expected_tree));
    }

    #[test]
    fn double_group_same_op() {
        // (4 - 3) - (5 - 4)
        let tokens = vec![
            Token::LParen,
            Token::IntLiteral(4),
            Token::Minus,
            Token::IntLiteral(3),
            Token::RParen,
            Token::Minus,
            Token::LParen,
            Token::IntLiteral(5),
            Token::Minus,
            Token::IntLiteral(4),
            Token::RParen,
        ];

        let tree = match parse(&tokens) {
            Ok(tree) => tree,
            Err(err) => {
                println!("{:?}", err);
                assert!(false);
                return;
            }
        };

        let mut expected_tree = SyntaxTree::new();
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(Token::Minus),
            children: vec![
                SyntaxTreeNodeHandle::with_index(1),
                SyntaxTreeNodeHandle::with_index(4),
            ],
        });

        // LHS: 4 - 3
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Term(Token::Minus),
                children: vec![
                    SyntaxTreeNodeHandle::with_index(2),
                    SyntaxTreeNodeHandle::with_index(3),
                ],
            });

            // LHS: 4
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(4)),
                children: vec![],
            });

            // RHS: 3
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(3)),
                children: vec![],
            });
        }
        // RHS: 5 - 4
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Term(Token::Minus),
                children: vec![
                    SyntaxTreeNodeHandle::with_index(5),
                    SyntaxTreeNodeHandle::with_index(6),
                ],
            });

            // LHS: 5
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(5)),
                children: vec![],
            });

            // RHS: 4
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(4)),
                children: vec![],
            });
        }

        debug_print(&tree, &expected_tree);
        assert!(equivalent(&tree, &expected_tree));
    }

    #[test]
    fn deep_groups_right() {
        // (3 + (2 - 1))
        let tokens = vec![
            Token::LParen,
            Token::IntLiteral(3),
            Token::Plus,
            Token::LParen,
            Token::IntLiteral(2),
            Token::Minus,
            Token::IntLiteral(1),
            Token::RParen,
            Token::RParen,
        ];

        let tree = match parse(&tokens) {
            Ok(tree) => tree,
            Err(err) => {
                println!("{:?}", err);
                assert!(false);
                return;
            }
        };

        let mut expected_tree = SyntaxTree::new();
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(Token::Plus),
            children: vec![
                SyntaxTreeNodeHandle::with_index(1),
                SyntaxTreeNodeHandle::with_index(2),
            ],
        });

        // LHS: 3
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(3)),
                children: vec![],
            });
        }

        // RHS: (2 - 1)
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Term(Token::Minus),
                children: vec![
                    SyntaxTreeNodeHandle::with_index(3),
                    SyntaxTreeNodeHandle::with_index(4),
                ],
            });

            // LHS: 2
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(2)),
                children: vec![],
            });

            // RHS: 1
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(1)),
                children: vec![],
            });
        }

        debug_print(&tree, &expected_tree);
        assert!(equivalent(&tree, &expected_tree));
    }

    #[test]
    fn deep_groups_left() {
        // ((3 - 2) * 4)
        let tokens = vec![
            Token::LParen,
            Token::LParen,
            Token::IntLiteral(3),
            Token::Minus,
            Token::IntLiteral(2),
            Token::RParen,
            Token::Multiply,
            Token::IntLiteral(4),
            Token::RParen,
        ];

        let tree = match parse(&tokens) {
            Ok(tree) => tree,
            Err(err) => {
                println!("{:?}", err);
                assert!(false);
                return;
            }
        };

        let mut expected_tree = SyntaxTree::new();
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Factor(Token::Multiply),
            children: vec![
                SyntaxTreeNodeHandle::with_index(1),
                SyntaxTreeNodeHandle::with_index(4),
            ],
        });

        // LHS: (3 - 2)
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Term(Token::Minus),
                children: vec![
                    SyntaxTreeNodeHandle::with_index(2),
                    SyntaxTreeNodeHandle::with_index(3),
                ],
            });

            // LHS: 3
            {
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(
                        3,
                    )),
                    children: vec![],
                });
            }

            // RHS: 2
            {
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(
                        2,
                    )),
                    children: vec![],
                });
            }
        }

        // RHS: 4
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(4)),
                children: vec![],
            });
        }

        debug_print(&tree, &expected_tree);
        assert!(equivalent(&tree, &expected_tree));
    }

    #[test]
    fn negative_unary_and_minus() {
        // -1 - 2
        let tokens = vec![
            Token::Minus,
            Token::IntLiteral(1),
            Token::Minus,
            Token::IntLiteral(2),
        ];

        let tree = match parse(&tokens) {
            Ok(tree) => tree,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let mut expected_tree = SyntaxTree::new();

        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(Token::Minus),
            children: vec![
                SyntaxTreeNodeHandle::with_index(1),
                SyntaxTreeNodeHandle::with_index(3),
            ],
        });

        // LHS: -1
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Unary(Token::Minus),
            children: vec![SyntaxTreeNodeHandle::with_index(2)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(1)),
            children: vec![],
        });

        // RHS: 2
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(2)),
            children: vec![],
        });

        debug_print(&tree, &expected_tree);
        assert!(equivalent(&tree, &expected_tree));
    }

    #[test]
    fn minus_negative() {
        // 1 - -2
        let tokens = vec![
            Token::IntLiteral(1),
            Token::Minus,
            Token::Minus,
            Token::IntLiteral(2),
        ];

        let tree = match parse(&tokens) {
            Ok(tree) => tree,
            Err(_) => {
                assert!(false);
                return;
            }
        };

        let mut expected_tree = SyntaxTree::new();
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(Token::Minus),
            children: vec![
                SyntaxTreeNodeHandle::with_index(1),
                SyntaxTreeNodeHandle::with_index(2),
            ],
        });

        // LHS: 1
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(1)),
            children: vec![],
        });

        // RHS: -2
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Unary(Token::Minus),
            children: vec![SyntaxTreeNodeHandle::with_index(3)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Primary(Token::IntLiteral(2)),
            children: vec![],
        });

        debug_print(&tree, &expected_tree);
        assert!(equivalent(&tree, &expected_tree));
    }
}
