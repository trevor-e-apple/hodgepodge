use std::{todo, vec};

use crate::scanner::Token;

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

struct QueueEntry<'a> {
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

    let mut queue = vec![QueueEntry {
        node: root_ref,
        start_index: 0,
        end_index: tokens.len(),
    }];
    loop {
        /*
        nodes are popped from the queue. their child nodes are then found. Nodes
        are added to the parent's children when they are spawned,
        not when they are popped off the queue
        */

        let node_entry = match queue.pop() {
            Some(value) => value,
            None => break,
        };

        let mut node = node_entry.node;
        match node.node_type {
            SyntaxTreeNodeType::Expression => {
                let mut child_node = SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Equality(None),
                    children: vec![],
                };

                queue.push(QueueEntry { 
                    node: &child_node,
                    start_index: node_entry.start_index,
                    end_index: node_entry.end_index,
                });
                node.children.push(&child_node);
            }
            SyntaxTreeNodeType::Equality(_) => {
                let mut child_node = SyntaxTreeNode {
                    node_type: SyntaxTreeNodeType::Comparison(None),
                    children: vec![],
                };

                let mut equality_index: Option<usize> = None;

                for index in node_entry.start_index..node_entry.end_index {
                    let token = match tokens.get(index) {
                        Some(token) => token,
                        None => todo!(),
                    };
                    match token {
                        Token::Equivalence | Token::NotEqual => {
                            equality_index = Some(index);
                            node.node_type = SyntaxTreeNodeType::Equality(Some(*token));
                            break;
                        }
                        _ => {}
                    }
                }

                match node.node_type {
                    SyntaxTreeNodeType::Equality(_) => {
                        match equality_index {
                            Some(equality_index) => {
                                // add LHS and RHS to queue
                                let lhs = SyntaxTreeNode {
                                    node_type: SyntaxTreeNodeType::Comparison(None),
                                    children: vec![]
                                };
                                queue.push(QueueEntry { 
                                    node: &lhs,
                                    start_index: node_entry.start_index,
                                    end_index: equality_index }
                                );

                                let rhs = SyntaxTreeNode {
                                    node_type: SyntaxTreeNodeType::Equality(None),
                                    children: vec![]
                                };
                                queue.push(
                                    QueueEntry {
                                        node: &rhs,
                                        start_index: equality_index + 1,
                                        end_index: node_entry.end_index
                                    }
                                );

                                // add lhs and rhs to node children
                            },
                            None => todo!(),
                        };
                    },
                    _ => {
                        // error path
                        todo!()
                    },
                }
            }
            SyntaxTreeNodeType::Comparison(_) => todo!(),
            SyntaxTreeNodeType::Term(_) => todo!(),
            SyntaxTreeNodeType::Factor(_) => todo!(),
            SyntaxTreeNodeType::Unary(_) => todo!(),
            SyntaxTreeNodeType::Primary(_) => todo!(),
        };
    }

    syntax_tree
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_name() {}
}
