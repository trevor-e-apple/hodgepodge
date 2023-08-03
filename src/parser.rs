use std::{todo, vec};

use crate::scanner::Token;
use crate::syntax_tree::{
    SyntaxTree, SyntaxTreeNode, SyntaxTreeNodeHandle, SyntaxTreeNodeType,
};

struct StackEntry {
    node_handle: SyntaxTreeNodeHandle,
    start_index: usize,
    end_index: usize,
}

/// A parser that takes a Vec of Tokens and produces a tree reflecting
/// the following grammar
///
/// grammar rules are sorted (for convenience) from least to highest precedence
///
/// expression -> equality;
/// equality -> comparison ((!= | ==) comparison)*;
/// comparison -> term ((> | >= | < | <=) term)*;
/// term -> factor (("-" | "+") factor)*;
/// factor -> unary (("*" | "/") unary)* ;
/// unary -> (("!" | "-") unary) | primary;
/// primary -> NUMBER | STRING | "true" | "false" | ("(" expression ")");
pub fn parse(tokens: &Vec<Token>) -> SyntaxTree {
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
                binary_op_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    node_entry,
                    &tokens,
                    &EQUALITY_MATCHING_OPS,
                );
            }
            SyntaxTreeNodeType::Comparison(_) => {
                binary_op_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    node_entry,
                    &tokens,
                    &COMPARISON_MATCHING_OPS,
                );
            }
            SyntaxTreeNodeType::Term(_) => {
                binary_op_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    node_entry,
                    &tokens,
                    &TERM_MATCHING_OPS,
                );
            }
            SyntaxTreeNodeType::Factor(_) => {
                binary_op_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    node_entry,
                    &tokens,
                    &FACTOR_MATCHING_OPS,
                );
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
                primary_expansion(
                    &mut syntax_tree,
                    &mut stack,
                    node_entry,
                    &tokens,
                );
            }
        };
    }

    syntax_tree
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
) {
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

    for index in start_index..end_index {
        let token = match tokens.get(index) {
            Some(token) => token,
            None => todo!(),
        };
        if matching_op_tokens.into_iter().any(|x| *x == *token) {
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

    match op_index {
        Some(op_index) => {
            // add LHS and RHS to queue
            // LHS moves on to the next rule
            let lhs_handle = syntax_tree.add_node(SyntaxTreeNode {
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
                node_handle: lhs_handle,
                start_index: start_index,
                end_index: op_index,
            });

            // RHS stays on the same rule
            let rhs_handle = syntax_tree.add_node(SyntaxTreeNode {
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
            // if not found, you add a single child. its stack entry will have
            // -- the same bounds as the currently expanding node
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
) {
    let start_index = node_entry.start_index;
    let end_index = node_entry.end_index;

    let first_token = match tokens.get(start_index) {
        Some(token) => token.clone(),
        None => todo!(),
    };

    if first_token == Token::LParen {
        let mut lparens_found = 1;
        let mut rparens_found = 0;
        for index in (start_index + 1)..end_index {
            let token = match tokens.get(index) {
                Some(token) => token,
                None => todo!(),
            };

            match token {
                Token::LParen => {
                    // we are matching an expression group
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
                        stack.push(StackEntry {
                            node_handle: child_handle,
                            start_index: node_entry.start_index,
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
                todo!();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::syntax_tree::equivalent;

    use super::*;

    #[test]
    fn primary_only() {
        let tokens = vec![Token::IntLiteral(3)];

        let tree = parse(&tokens);

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

        let tree = parse(&tokens);

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
                SyntaxTreeNodeHandle::with_index(7),
            ],
        });

        // 0th token branch
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

        // 2nd token branch
        expected_tree.add_node(SyntaxTreeNode {
            node_type: SyntaxTreeNodeType::Term(None),
            children: vec![SyntaxTreeNodeHandle::with_index(8)],
        });
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

    #[test]
    fn binary_ops() {
        let tokens = vec![
            Token::IntLiteral(1),
            Token::Plus,
            Token::IntLiteral(2),
            Token::Minus,
            Token::IntLiteral(3),
        ];

        let tree = parse(&tokens);

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
                SyntaxTreeNodeHandle::with_index(7),
            ],
        });

        // LHS
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

        // RHS
        {
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Term(Some(Token::Minus)),
                children: vec![
                    SyntaxTreeNodeHandle::with_index(8),
                    SyntaxTreeNodeHandle::with_index(11)
                ],
            });

            // LHS
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

            // RHS
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Term(None),
                children: vec![SyntaxTreeNodeHandle::with_index(12)],
            });
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Factor(None),
                children: vec![SyntaxTreeNodeHandle::with_index(13)],
            });
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Unary(None),
                children: vec![SyntaxTreeNodeHandle::with_index(14)],
            });
            expected_tree.add_node(SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(Some(tokens[4].clone())),
                children: vec![],
            });
        }

        assert!(equivalent(&tree, &expected_tree));
    }

    #[test]
    fn group_precedence() {
        unimplemented!();
    }
    // TODO: add test cases for error cases like a binary operator token with
    // -- no LHS or RHS. solo unary token.
}
