use std::{todo, vec};

use crate::scanner::Token;

/* TODO:
pretty print syntax tree
*/

#[derive(Default)]
pub enum SyntaxTreeNodeType {
    #[default]
    Expression,
    Equality(Option<Token>),
    Comparison(Option<Token>),
    Term(Option<Token>),
    Factor(Option<Token>),
    Unary(Option<Token>),
    Primary(Option<Token>),
}

#[derive(Default)]
pub struct SyntaxTreeNode {
    node_type: SyntaxTreeNodeType,
    children: Vec<SyntaxTreeNode>,
}

#[derive(Default)]
pub struct SyntaxTree {
    root: SyntaxTreeNode,
}

struct StackEntry<'a> {
    node: &'a SyntaxTreeNode,
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
pub fn parse(tokens: Vec<Token>) -> SyntaxTree {
    let mut syntax_tree = SyntaxTree { ..Default::default() };
    syntax_tree.root = SyntaxTreeNode {
        node_type: SyntaxTreeNodeType::Expression,
        children: vec![],
    };
    let root_ref = &syntax_tree.root;

    let mut stack = vec![StackEntry {
        node: root_ref,
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

        match node_entry.node.node_type {
            SyntaxTreeNodeType::Expression => {
                expression_expansion(&mut stack, node_entry);
            }
            SyntaxTreeNodeType::Equality(_) => {
                binary_op_expansion(
                    &mut stack,
                    node_entry,
                    &tokens,
                    &EQUALITY_MATCHING_OPS,
                );
            }
            SyntaxTreeNodeType::Comparison(_) => {
                binary_op_expansion(
                    &mut stack,
                    node_entry,
                    &tokens,
                    &COMPARISON_MATCHING_OPS,
                );
            }
            SyntaxTreeNodeType::Term(_) => {
                binary_op_expansion(
                    &mut stack,
                    node_entry,
                    &tokens,
                    &TERM_MATCHING_OPS,
                );
            }
            SyntaxTreeNodeType::Factor(_) => {
                binary_op_expansion(
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
                unary_op_expansion(&mut stack, node_entry, &tokens);
            }
            SyntaxTreeNodeType::Primary(_) => {
                primary_expansion(node_entry, &tokens);
            }
        };
    }

    syntax_tree
}

// TODO: documentation
fn expression_expansion(stack: &mut Vec<StackEntry>, node_entry: StackEntry) {
    let mut node = node_entry.node;

    let mut child_node = SyntaxTreeNode {
        node_type: SyntaxTreeNodeType::Equality(None),
        children: vec![],
    };

    stack.push(StackEntry {
        node: &child_node,
        start_index: node_entry.start_index,
        end_index: node_entry.end_index,
    });
    node.children.push(child_node);
}

fn binary_op_expansion(
    stack: &mut Vec<StackEntry>,
    node_entry: StackEntry,
    tokens: &Vec<Token>,
    matching_op_tokens: &[Token],
) {
    let mut node = node_entry.node;

    let mut op_index: Option<usize> = None;

    for index in node_entry.start_index..node_entry.end_index {
        let token = match tokens.get(index) {
            Some(token) => token,
            None => todo!(),
        };
        if matching_op_tokens.into_iter().any(|x| *x == *token) {
            op_index = Some(index);
            node.node_type = match node.node_type {
                SyntaxTreeNodeType::Equality(_) => {
                    SyntaxTreeNodeType::Equality(Some(*token))
                }
                SyntaxTreeNodeType::Comparison(_) => {
                    SyntaxTreeNodeType::Comparison(Some(*token))
                }
                SyntaxTreeNodeType::Term(_) => {
                    SyntaxTreeNodeType::Term(Some(*token))
                }
                SyntaxTreeNodeType::Factor(_) => {
                    SyntaxTreeNodeType::Factor(Some(*token))
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
            let lhs = SyntaxTreeNode {
                node_type: match node.node_type {
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
            };
            stack.push(StackEntry {
                node: &lhs,
                start_index: node_entry.start_index,
                end_index: op_index,
            });

            let rhs = SyntaxTreeNode {
                node_type: match node.node_type {
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
            };
            stack.push(StackEntry {
                node: &rhs,
                start_index: op_index + 1,
                end_index: node_entry.end_index,
            });

            // add lhs and rhs to node children
            node.children.push(lhs);
            node.children.push(rhs);
        }
        None => {
            // if not found, you add a single child. its stack entry will have
            // -- the same bounds as the currently expanding node
            let child = SyntaxTreeNode {
                node_type: match node.node_type {
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
            };
            stack.push(StackEntry {
                node: &child,
                start_index: node_entry.start_index,
                end_index: node_entry.end_index,
            });

            node.children.push(child);
        }
    };
}

fn unary_op_expansion(
    stack: &mut Vec<StackEntry>,
    node_entry: StackEntry,
    tokens: &Vec<Token>,
) {
    let mut node = node_entry.node;

    let mut op_index: Option<usize> = None;

    for index in node_entry.start_index..node_entry.end_index {
        let token = match tokens.get(index) {
            Some(token) => token,
            None => todo!(),
        };
        match token {
            Token::Minus | Token::Not => {
                op_index = Some(index);
                node.node_type = SyntaxTreeNodeType::Unary(Some(*token));
                break;
            }
            _ => {}
        };
    }

    match op_index {
        Some(op_index) => {
            // add another unary node
            let child = SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Unary(None),
                children: vec![],
            };
            stack.push(StackEntry {
                node: &child,
                start_index: op_index + 1,
                end_index: node_entry.end_index,
            });

            node.children.push(child);
        }
        None => {
            // if not found, you add a single child. its stack entry will have
            // -- the same bounds as the currently expanding node
            let child = SyntaxTreeNode {
                node_type: SyntaxTreeNodeType::Primary(None),
                children: vec![],
            };
            stack.push(StackEntry {
                node: &child,
                start_index: node_entry.start_index,
                end_index: node_entry.end_index,
            });

            node.children.push(child);
        }
    };
}

fn primary_expansion(
    stack: &mut Vec<StackEntry>,
    node_entry: StackEntry,
    tokens: &Vec<Token>,
) {
    let mut node = node_entry.node;

    let first_token = match tokens.get(0) {
        Some(token) => token,
        None => todo!(),
    };

    if *first_token == Token::LParen {
        let mut lparens_found = 1;
        let mut rparens_found = 0;
        for index in (node_entry.start_index + 1)..node_entry.end_index {
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
                        let child = SyntaxTreeNode {
                            node_type: SyntaxTreeNodeType::Expression,
                            children: vec![],
                        };
                        stack.push(StackEntry {
                            node: &child,
                            start_index: node_entry.start_index,
                            end_index: index,
                        });
                        node.children.push(child);
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
                node.node_type =
                    SyntaxTreeNodeType::Primary(Some(*first_token));
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
    use super::*;

    #[test]
    fn test_name() {}
}
