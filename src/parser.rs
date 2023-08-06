use std::{todo, vec};

use crate::scanner::Token;
use crate::syntax_tree::{
    SyntaxTree, SyntaxTreeNode, SyntaxTreeNodeHandle, SyntaxTreeNodeType,
};

struct StackEntry {
    node_handle: SyntaxTreeNodeHandle,
    start_index: usize,
    end_index: usize, // one past the index of the final element
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    Success,
    MissingToken,
    MissingLhs,
    MissingRhs,
    MismatchedGrouping,
    UnexpectedToken,
}

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

    let root_handle = syntax_tree.add_node(SyntaxTreeNode {
        node_type: SyntaxTreeNodeType::Expression,
        children: vec![],
    });

    let mut stack = vec![StackEntry {
        node_handle: root_handle,
        start_index: 0,
        end_index: tokens.len(),
    }];

    const EQUALITY_MATCHING_OPS: [Token; 2] =
        [Token::Equivalence, Token::NotEqual];
    const COMPARISON_MATCHING_OPS: [Token; 4] = [
        Token::GreaterThan,
        Token::GreaterThanEqualTo,
        Token::LessThan,
        Token::LessThanEqualTo,
    ];
    const TERM_MATCHING_OPS: [Token; 2] = [Token::Minus, Token::Plus];
    const FACTOR_MATCHING_OPS: [Token; 2] = [Token::Multiply, Token::Divide];
    loop {
        let node_entry = match stack.pop() {
            Some(value) => value,
            None => break,
        };
        let node_type = match syntax_tree.get_node(node_entry.node_handle) {
            Some(node) => &node.node_type,
            None => {
                // handle error path
                todo!();
            }
        };

        match node_type {
            SyntaxTreeNodeType::Expression => {
                expression_expansion(&mut syntax_tree, &mut stack, node_entry);
            }
            SyntaxTreeNodeType::Equality(_) => {
                match binary_op_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    node_entry,
                    &tokens,
                    &EQUALITY_MATCHING_OPS,
                ) {
                    Ok(_) => {}
                    Err(err) => return Err(err),
                };
            }
            SyntaxTreeNodeType::Comparison(_) => {
                match binary_op_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    node_entry,
                    &tokens,
                    &COMPARISON_MATCHING_OPS,
                ) {
                    Ok(_) => {}
                    Err(err) => return Err(err),
                };
            }
            SyntaxTreeNodeType::Term(_) => {
                match binary_op_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    node_entry,
                    &tokens,
                    &TERM_MATCHING_OPS,
                ) {
                    Ok(_) => {}
                    Err(err) => return Err(err),
                };
            }
            SyntaxTreeNodeType::Factor(_) => {
                match binary_op_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    node_entry,
                    &tokens,
                    &FACTOR_MATCHING_OPS,
                ) {
                    Ok(_) => {}
                    Err(err) => return Err(err),
                };
            }
            SyntaxTreeNodeType::Unary(_) => {
                // no need for matching ops vector. since unary operators don't
                // -- have precedence, we don't need to factor out the code,
                // -- which means we don't need this data to configure the
                // -- function behavior
                unary_op_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    node_entry,
                    &tokens,
                );
            }
            SyntaxTreeNodeType::Primary(_) => {
                match primary_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    node_entry,
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
fn expression_expansion(
    syntax_tree: &mut SyntaxTree,
    stack: &mut Vec<StackEntry>,
    node_entry: StackEntry,
) {
    let node_handle = node_entry.node_handle;

    let child_node_handle = syntax_tree.add_node(SyntaxTreeNode {
        node_type: SyntaxTreeNodeType::Equality(None),
        children: vec![],
    });

    stack.push(StackEntry {
        node_handle: child_node_handle,
        start_index: node_entry.start_index,
        end_index: node_entry.end_index,
    });

    match syntax_tree.get_node_mut(node_handle) {
        Some(node) => {
            node.children.push(child_node_handle);
        }
        None => {
            // handle error path
            todo!()
        }
    };
}

fn binary_op_expansion(
    syntax_tree: &mut SyntaxTree,
    stack: &mut Vec<StackEntry>,
    node_entry: StackEntry,
    tokens: &Vec<Token>,
    matching_op_tokens: &[Token],
) -> Result<ParseError, ParseError> {
    let start_index = node_entry.start_index;
    let end_index = node_entry.end_index;
    let node_handle = node_entry.node_handle;
    let node_type = {
        let node = match syntax_tree.get_node(node_handle) {
            Some(node) => node,
            None => {
                // handle error path
                todo!()
            }
        };
        node.node_type.clone()
    };

    let mut op_index: Option<usize> = None;

    // going in reverse order is equivalent to the approach where you match
    // 0 or more in the front
    let mut group_depth = 0;
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

        if (group_depth == 0)
            && matching_op_tokens.into_iter().any(|x| *x == *token)
        {
            op_index = Some(index);

            let node = match syntax_tree.get_node_mut(node_handle) {
                Some(node) => node,
                None => {
                    // handle error path
                    todo!()
                }
            };
            node.node_type = match node_type {
                SyntaxTreeNodeType::Equality(_) => {
                    SyntaxTreeNodeType::Equality(Some(token.clone()))
                }
                SyntaxTreeNodeType::Comparison(_) => {
                    SyntaxTreeNodeType::Comparison(Some(token.clone()))
                }
                SyntaxTreeNodeType::Term(_) => {
                    SyntaxTreeNodeType::Term(Some(token.clone()))
                }
                SyntaxTreeNodeType::Factor(_) => {
                    SyntaxTreeNodeType::Factor(Some(token.clone()))
                }
                _ => {
                    // not a binary operation. error here
                    todo!();
                }
            };
            break;
        }
    }

    if group_depth > 0 {
        return Err(ParseError::MismatchedGrouping);
    }

    match op_index {
        Some(op_index) => {
            if op_index == start_index {
                return Err(ParseError::MissingLhs);
            } else if (op_index + 1) == end_index {
                return Err(ParseError::MissingRhs);
            }

            // add LHS and RHS to queue
            // LHS stays on the same rule
            let lhs_handle = syntax_tree.add_node(SyntaxTreeNode {
                node_type: match node_type {
                    SyntaxTreeNodeType::Equality(_) => {
                        SyntaxTreeNodeType::Equality(None)
                    }
                    SyntaxTreeNodeType::Comparison(_) => {
                        SyntaxTreeNodeType::Comparison(None)
                    }
                    SyntaxTreeNodeType::Term(_) => {
                        SyntaxTreeNodeType::Term(None)
                    }
                    SyntaxTreeNodeType::Factor(_) => {
                        SyntaxTreeNodeType::Factor(None)
                    }
                    _ => {
                        // not a binary operation. error here
                        todo!();
                    }
                },
                children: vec![],
            });

            stack.push(StackEntry {
                node_handle: lhs_handle,
                start_index: start_index,
                end_index: op_index,
            });

            // RHS moves on to the next rule
            let rhs_handle = syntax_tree.add_node(SyntaxTreeNode {
                node_type: match node_type {
                    SyntaxTreeNodeType::Equality(_) => {
                        SyntaxTreeNodeType::Comparison(None)
                    }
                    SyntaxTreeNodeType::Comparison(_) => {
                        SyntaxTreeNodeType::Term(None)
                    }
                    SyntaxTreeNodeType::Term(_) => {
                        SyntaxTreeNodeType::Factor(None)
                    }
                    SyntaxTreeNodeType::Factor(_) => {
                        SyntaxTreeNodeType::Unary(None)
                    }
                    _ => {
                        // not a binary expression. error here
                        todo!();
                    }
                },
                children: vec![],
            });
            stack.push(StackEntry {
                node_handle: rhs_handle,
                start_index: op_index + 1,
                end_index: end_index,
            });

            // add lhs and rhs to node children
            let node = match syntax_tree.get_node_mut(node_handle) {
                Some(node) => node,
                None => {
                    // handle error path
                    todo!()
                }
            };
            node.children.push(lhs_handle);
            node.children.push(rhs_handle);
        }
        None => {
            // if op not found, you add a single child. its stack entry will
            // -- have the same bounds as the currently expanding node
            let child_handle = syntax_tree.add_node(SyntaxTreeNode {
                node_type: match node_type {
                    SyntaxTreeNodeType::Equality(_) => {
                        SyntaxTreeNodeType::Comparison(None)
                    }
                    SyntaxTreeNodeType::Comparison(_) => {
                        SyntaxTreeNodeType::Term(None)
                    }
                    SyntaxTreeNodeType::Term(_) => {
                        SyntaxTreeNodeType::Factor(None)
                    }
                    SyntaxTreeNodeType::Factor(_) => {
                        SyntaxTreeNodeType::Unary(None)
                    }
                    _ => {
                        // not a binary operation. error here
                        todo!();
                    }
                },
                children: vec![],
            });
            stack.push(StackEntry {
                node_handle: child_handle,
                start_index: start_index,
                end_index: end_index,
            });

            let node = match syntax_tree.get_node_mut(node_handle) {
                Some(node) => node,
                None => {
                    // handle error path
                    todo!()
                }
            };
            node.children.push(child_handle);
        }
    };

    Ok(ParseError::Success)
}

fn unary_op_expansion(
    syntax_tree: &mut SyntaxTree,
    stack: &mut Vec<StackEntry>,
    node_entry: StackEntry,
    tokens: &Vec<Token>,
) {
    let node_handle = node_entry.node_handle;
    let start_index = node_entry.start_index;
    let end_index = node_entry.end_index;

    let mut op_index: Option<usize> = None;

    for index in start_index..end_index {
        let token = match tokens.get(index) {
            Some(token) => token,
            None => todo!(),
        };
        match token {
            Token::Minus | Token::Not => {
                op_index = Some(index);

                let node = match syntax_tree.get_node_mut(node_handle) {
                    Some(node) => node,
                    None => {
                        // handle error path
                        todo!()
                    }
                };
                node.node_type = SyntaxTreeNodeType::Unary(Some(token.clone()));

                break;
            }
            _ => {}
        };
    }

    match op_index {
        Some(op_index) => {
            // add another unary node
            let child_handle = syntax_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Unary(None),
                children: vec![],
            });
            stack.push(StackEntry {
                node_handle: child_handle,
                start_index: op_index + 1,
                end_index: node_entry.end_index,
            });

            let node = match syntax_tree.get_node_mut(node_handle) {
                Some(node) => node,
                None => {
                    // handle error path
                    todo!()
                }
            };
            node.children.push(child_handle);
        }
        None => {
            // if not found, you add a single child. its stack entry will have
            // -- the same bounds as the currently expanding node
            let child_handle = syntax_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(None),
                children: vec![],
            });
            stack.push(StackEntry {
                node_handle: child_handle,
                start_index: node_entry.start_index,
                end_index: node_entry.end_index,
            });

            let node = match syntax_tree.get_node_mut(node_handle) {
                Some(node) => node,
                None => {
                    // handle error path
                    todo!()
                }
            };
            node.children.push(child_handle);
        }
    };
}

fn primary_expansion(
    syntax_tree: &mut SyntaxTree,
    stack: &mut Vec<StackEntry>,
    node_entry: StackEntry,
    tokens: &Vec<Token>,
) -> Result<ParseError, ParseError> {
    let start_index = node_entry.start_index;
    let end_index = node_entry.end_index;

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
                        let child_handle =
                            syntax_tree.add_node(SyntaxTreeNode {
                                node_type: SyntaxTreeNodeType::Expression,
                                children: vec![],
                            });

                        // +1 for start index to remove the lparen
                        // no +1 for end_index b/c that's always one past
                        stack.push(StackEntry {
                            node_handle: child_handle,
                            start_index: node_entry.start_index + 1,
                            end_index: index,
                        });

                        let node = match syntax_tree
                            .get_node_mut(node_entry.node_handle)
                        {
                            Some(node) => node,
                            None => {
                                // handle error path
                                todo!()
                            }
                        };
                        node.children.push(child_handle);
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
            | Token::Identifier(_) => {
                let mut node =
                    match syntax_tree.get_node_mut(node_entry.node_handle) {
                        Some(node) => node,
                        None => {
                            // handle error path
                            todo!()
                        }
                    };
                node.node_type = SyntaxTreeNodeType::Primary(Some(first_token));
            }
            _ => {
                // unexpected token error
                return Err(ParseError::UnexpectedToken);
            }
        }
    }

    Ok(ParseError::Success)
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use crate::syntax_tree::equivalent;

    use super::*;

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
            node_type: SyntaxTreeNodeType::Expression,
            children: vec![SyntaxTreeNodeHandle::with_index(1)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Equality(None),
            children: vec![SyntaxTreeNodeHandle::with_index(2)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Comparison(None),
            children: vec![SyntaxTreeNodeHandle::with_index(3)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(None),
            children: vec![SyntaxTreeNodeHandle::with_index(4)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Factor(None),
            children: vec![SyntaxTreeNodeHandle::with_index(5)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Unary(None),
            children: vec![SyntaxTreeNodeHandle::with_index(6)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Primary(Some(tokens[0].clone())),
            children: vec![],
        });

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
            node_type: SyntaxTreeNodeType::Expression,
            children: vec![SyntaxTreeNodeHandle::with_index(1)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Equality(None),
            children: vec![SyntaxTreeNodeHandle::with_index(2)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Comparison(None),
            children: vec![SyntaxTreeNodeHandle::with_index(3)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(Some(Token::Plus)),
            children: vec![
                SyntaxTreeNodeHandle::with_index(4),
                SyntaxTreeNodeHandle::with_index(8),
            ],
        });

        // LHS: 1
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(None),
            children: vec![SyntaxTreeNodeHandle::with_index(5)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Factor(None),
            children: vec![SyntaxTreeNodeHandle::with_index(6)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Unary(None),
            children: vec![SyntaxTreeNodeHandle::with_index(7)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Primary(Some(tokens[0].clone())),
            children: vec![],
        });

        // RHS: 2
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Factor(None),
            children: vec![SyntaxTreeNodeHandle::with_index(9)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Unary(None),
            children: vec![SyntaxTreeNodeHandle::with_index(10)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Primary(Some(tokens[2].clone())),
            children: vec![],
        });

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
            node_type: SyntaxTreeNodeType::Expression,
            children: vec![SyntaxTreeNodeHandle::with_index(1)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Equality(None),
            children: vec![SyntaxTreeNodeHandle::with_index(2)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Comparison(None),
            children: vec![SyntaxTreeNodeHandle::with_index(3)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(Some(Token::Minus)),
            children: vec![
                SyntaxTreeNodeHandle::with_index(4),
                SyntaxTreeNodeHandle::with_index(12),
            ],
        });

        // LHS: 1 + 2
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Term(Some(Token::Plus)),
                children: vec![
                    SyntaxTreeNodeHandle::with_index(5),
                    SyntaxTreeNodeHandle::with_index(9),
                ],
            });

            // LHS: 1
            {
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Term(None),
                    children: vec![SyntaxTreeNodeHandle::with_index(6)],
                });
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Factor(None),
                    children: vec![SyntaxTreeNodeHandle::with_index(7)],
                });
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Unary(None),
                    children: vec![SyntaxTreeNodeHandle::with_index(8)],
                });
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Primary(Some(
                        Token::IntLiteral(1),
                    )),
                    children: vec![],
                });
            }

            // RHS: 2
            {
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Factor(None),
                    children: vec![SyntaxTreeNodeHandle::with_index(10)],
                });
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Unary(None),
                    children: vec![SyntaxTreeNodeHandle::with_index(11)],
                });
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Primary(Some(
                        Token::IntLiteral(2),
                    )),
                    children: vec![],
                });
            }
        }

        // RHS: 3
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Factor(None),
                children: vec![SyntaxTreeNodeHandle::with_index(13)],
            });
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Unary(None),
                children: vec![SyntaxTreeNodeHandle::with_index(14)],
            });
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Some(
                    Token::IntLiteral(3),
                )),
                children: vec![],
            });
        }

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
            node_type: SyntaxTreeNodeType::Expression,
            children: vec![SyntaxTreeNodeHandle::with_index(1)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Equality(None),
            children: vec![SyntaxTreeNodeHandle::with_index(2)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Comparison(None),
            children: vec![SyntaxTreeNodeHandle::with_index(3)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(Some(Token::Plus)),
            children: vec![
                SyntaxTreeNodeHandle::with_index(4),
                SyntaxTreeNodeHandle::with_index(8),
            ],
        });

        // LHS: 1
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Term(None),
                children: vec![SyntaxTreeNodeHandle::with_index(5)],
            });
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Factor(None),
                children: vec![SyntaxTreeNodeHandle::with_index(6)],
            });
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Unary(None),
                children: vec![SyntaxTreeNodeHandle::with_index(7)],
            });
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Some(
                    Token::IntLiteral(1),
                )),
                children: vec![],
            });
        }

        // RHS: 2 * 3
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Factor(Some(Token::Multiply)),
                children: vec![
                    SyntaxTreeNodeHandle::with_index(9),
                    SyntaxTreeNodeHandle::with_index(12),
                ],
            });

            // LHS: 2
            {
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Factor(None),
                    children: vec![SyntaxTreeNodeHandle::with_index(10)],
                });
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Unary(None),
                    children: vec![SyntaxTreeNodeHandle::with_index(11)],
                });
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Primary(Some(
                        Token::IntLiteral(2),
                    )),
                    children: vec![],
                });
            }

            // RHS: 3
            {
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Unary(None),
                    children: vec![SyntaxTreeNodeHandle::with_index(13)],
                });
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Primary(Some(
                        Token::IntLiteral(3),
                    )),
                    children: vec![],
                });
            }
        }

        assert!(equivalent(&tree, &expected_tree));
    }

    #[test]
    fn unary_bin_mixed_ops() {
        unimplemented!();
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
            node_type: SyntaxTreeNodeType::Expression,
            children: vec![SyntaxTreeNodeHandle::with_index(1)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Equality(None),
            children: vec![SyntaxTreeNodeHandle::with_index(2)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Comparison(None),
            children: vec![SyntaxTreeNodeHandle::with_index(3)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(None),
            children: vec![SyntaxTreeNodeHandle::with_index(4)],
        });
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Factor(Some(Token::Multiply)),
            children: vec![
                SyntaxTreeNodeHandle::with_index(5),
                SyntaxTreeNodeHandle::with_index(8),
            ],
        });

        // LHS: 3
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Factor(None),
                children: vec![
                    SyntaxTreeNodeHandle::with_index(6),
                ],
            });
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Unary(None),
                children: vec![SyntaxTreeNodeHandle::with_index(7)],
            });
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Some(
                    Token::IntLiteral(3),
                )),
                children: vec![],
            });
        }

        // RHS: (1 + 2)
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Unary(None),
                children: vec![SyntaxTreeNodeHandle::with_index(9)],
            });
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(None),
                children: vec![SyntaxTreeNodeHandle::with_index(10)],
            });

            // Expression: 1 + 2
            {
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Expression,
                    children: vec![SyntaxTreeNodeHandle::with_index(11)],
                });
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Equality(None),
                    children: vec![SyntaxTreeNodeHandle::with_index(12)],
                });
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Comparison(None),
                    children: vec![SyntaxTreeNodeHandle::with_index(13)],
                });
                expected_tree.add_node(SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Term(Some(Token::Plus)),
                    children: vec![
                        SyntaxTreeNodeHandle::with_index(14),
                        SyntaxTreeNodeHandle::with_index(18),
                    ],
                });

                // LHS: 1
                {
                    expected_tree.add_node(SyntaxTreeNode {
                        node_type: SyntaxTreeNodeType::Term(None),
                        children: vec![
                            SyntaxTreeNodeHandle::with_index(15),
                        ],
                    });
                    expected_tree.add_node(SyntaxTreeNode {
                        node_type: SyntaxTreeNodeType::Factor(None),
                        children: vec![SyntaxTreeNodeHandle::with_index(16)],
                    });
                    expected_tree.add_node(SyntaxTreeNode {
                        node_type: SyntaxTreeNodeType::Unary(None),
                        children: vec![SyntaxTreeNodeHandle::with_index(17)],
                    });
                    expected_tree.add_node(SyntaxTreeNode {
                        node_type: SyntaxTreeNodeType::Primary(Some(
                            Token::IntLiteral(1),
                        )),
                        children: vec![],
                    });
                }

                // RHS: 2
                {
                    expected_tree.add_node(SyntaxTreeNode {
                        node_type: SyntaxTreeNodeType::Factor(None),
                        children: vec![SyntaxTreeNodeHandle::with_index(19)],
                    });
                    expected_tree.add_node(SyntaxTreeNode {
                        node_type: SyntaxTreeNodeType::Unary(None),
                        children: vec![SyntaxTreeNodeHandle::with_index(20)],
                    });
                    expected_tree.add_node(SyntaxTreeNode {
                        node_type: SyntaxTreeNodeType::Primary(Some(
                            Token::IntLiteral(2),
                        )),
                        children: vec![],
                    });
                }
            }
        }

        tree.pretty_print();
        expected_tree.pretty_print();
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
            Err(err) => assert_eq!(err, ParseError::MissingRhs),
        };
    }

    #[test]
    fn binary_no_lhs() {
        let tokens = vec![Token::Plus, Token::IntLiteral(1)];

        match parse(&tokens) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err, ParseError::MissingLhs),
        };
    }

    #[test]
    fn double_binary_no_sides() {
        let tokens = vec![Token::Plus, Token::Multiply];
        match parse(&tokens) {
            Ok(_) => assert!(false),
            Err(err) => assert_eq!(err, ParseError::MissingLhs),
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
        unimplemented!();
    }

    #[test]
    fn groups_single_op() {
        // (1 + 2) * (3 + 4)
        unimplemented!();
    }

    #[test]
    fn left_associative() {
        // (3 - 2) - 1
        unimplemented!();
    }

    #[test]
    fn double_group_same_op() {
        // (4 - 3) - (5 - 4)
        unimplemented!();
    }

    #[test]
    fn deep_groups_right() {
        // (3 + (2 - 1))
        unimplemented!();
    }

    #[test]
    fn deep_groups_left() {
        // ((3 - 2) * 4)
        unimplemented!();
    }
}
