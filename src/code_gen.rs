use std::todo;

use crate::{
    scanner::Token,
    syntax_tree::{
        SyntaxTree, SyntaxTreeDfs, SyntaxTreeNodeHandle, SyntaxTreeNodeType,
    },
};

pub fn generate(tree: SyntaxTree) -> String {
    let result = String::new();

    let root_handle = match tree.get_root_handle() {
        Some(root_handle) => root_handle,
        None => return result,
    };

    // count the number of identifiers we need to allocate for
    let identifiers_found = {
        let mut identifiers_found = 0;
        for search_entry in SyntaxTreeDfs::new(&tree) {
            let node = match tree.get_node(search_entry.node_handle) {
                Some(node) => node,
                None => todo!(), // DFS is somehow returning invalid handle
            };

            match &node.node_type {
                SyntaxTreeNodeType::Primary(optional_token) => {
                    match optional_token {
                        Some(token) => match token {
                            Token::Identifier(_) => identifiers_found += 1,
                            _ => {}
                        },
                        None => todo!(), // syntax tree should no longer have empty nodes
                    }
                }
                _ => {}
            };
        }

        identifiers_found
    };

    let mut stack: Vec<SyntaxTreeNodeHandle> = vec![root_handle];

    loop {
        // pop off the top
        let node_handle = match stack.pop() {
            Some(node_handle) => node_handle,
            None => break,
        };
        let node = match tree.get_node(node_handle) {
            Some(node) => node,
            None => todo!(), // something has gone horribly wrong
        };

        match &node.node_type {
            SyntaxTreeNodeType::Expression => todo!(),
            SyntaxTreeNodeType::Equality(_) => todo!(),
            SyntaxTreeNodeType::Comparison(_) => todo!(),
            SyntaxTreeNodeType::Term(_) => todo!(),
            SyntaxTreeNodeType::Factor(optional_token) => {
                todo!();
            },
            SyntaxTreeNodeType::Unary(_) => todo!(),
            SyntaxTreeNodeType::Primary(_) => todo!(),
        };

        // check operator type, if it's ready to evaluate, then add the 
        // -- operation to the result string. 
        // if it's not ready to evaluate, add node and children back onto the 
        // -- stack
    }

    result
}
