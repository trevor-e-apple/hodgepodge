use std::{todo, vec};

use crate::scanner::Token;

#[derive(Default)]
pub enum SyntaxTreeNodeType {
    #[default]
    Expression,
    Equality(Token),
    Comparison(Token),
    Term(Token),
    Factor(Token),
    Unary(Token),
    Primary(Token),
}

#[derive(Default)]
pub struct SyntaxTreeNode {
    node_type: SyntaxTreeNodeType,
    children: Vec<SyntaxTreeNode>,
}

#[derive(Default)]
pub struct SyntaxTree {
    tree_data: Vec<SyntaxTreeNode>,
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
    struct QueueEntry<'a> {
        node: &'a SyntaxTreeNode,
        start_index: usize,
        end_index: usize,
    }

    let mut syntax_tree = SyntaxTree { ..Default::default() };
    let root = SyntaxTreeNode {
        node_type: SyntaxTreeNodeType::Expression,
        children: vec![],
    };
    let root_ref = &root;
    syntax_tree.tree_data.push(root);

    let mut queue = vec![QueueEntry {
        node: root_ref,
        start_index: 0,
        end_index: tokens.len(),
    }];
    loop {
        let node_entry = match queue.pop() {
            Some(value) => value,
            None => break,
        };

        let node = node_entry.node;
        match node.node_type {
            SyntaxTreeNodeType::Expression => {
                // equality expansion
                for index in node_entry.start_index..node_entry.end_index {
                    let token = match tokens.get(index) {
                        Some(token) => token,
                        None => todo!(),
                    };
                    match token {
                        Token::Equivalence | Token::NotEqual => {
                            let child_node = SyntaxTreeNode {
                                node_type: SyntaxTreeNodeType::Equality(*token),
                                children: vec![],
                            };
                        }
                        _ => {}
                    }
                }

                // SyntaxTreeNode { node_type: SyntaxTreeNodeType::Equality(), children: todo!() };
                // queue.push(value);
            }
            SyntaxTreeNodeType::Equality(_) => {
                todo!();
            }
            SyntaxTreeNodeType::Comparison(_) => todo!(),
            SyntaxTreeNodeType::Term(_) => todo!(),
            SyntaxTreeNodeType::Factor(_) => todo!(),
            SyntaxTreeNodeType::Unary(_) => todo!(),
            SyntaxTreeNodeType::Primary(_) => todo!(),
        };

        todo!();
    }

    syntax_tree
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_name() {}
}
