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
    enum GrammarRule {
        Expression,
        Equality,
        Comparison,
        Term,
        Token,
        Factor,
        Unary,
        Primary,
    }
    struct QueueEntry {
        rule: GrammarRule,
        token_start_index: usize,
        token_end_index: usize,
    }

    let mut syntax_tree = SyntaxTree { ..Default::default() };
    let mut queue = vec![QueueEntry {
        rule: GrammarRule::Expression,
        token_start_index: 0,
        token_end_index: tokens.len(),
    }];
    loop {
        let node_entry = match queue.pop() {
            Some(value) => value,
            None => break,
        };

        // add children to queue
        // add children to tree node
        // add tree node to tree
        todo!();
    };

    syntax_tree
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_name() {}
}
