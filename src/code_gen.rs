/*
TODO:
documentation
*/
use std::{format, todo};

use crate::{
    scanner::Token,
    syntax_tree::{SyntaxTree, SyntaxTreeNodeType},
};

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
                children: children,
            });
        }

        Self { nodes }
    }

    pub fn get_root_handle(&self) -> Option<CodeGenTreeNodeHandle> {
        if self.nodes.len() == 0 {
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

pub fn generate(tree: &SyntaxTree) -> String {
    let mut result = String::new();
    let mut tree = CodeGenTree::from_syntax_tree(tree);

    let root_handle = match tree.get_root_handle() {
        Some(root_handle) => root_handle,
        None => return result,
    };

    let mut stack: Vec<CodeGenTreeNodeHandle> = vec![root_handle];
    let mut store_at = 0;

    loop {
        // pop off the top
        let node_handle = match stack.pop() {
            Some(node_handle) => node_handle,
            None => break,
        };
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

                match child.node_type {
                    // primaries are considered evaluated
                    SyntaxTreeNodeType::Primary(_) => {}
                    _ => {
                        // children with stored result are also considered evaluated
                        match child.stored_at {
                            None => {
                                ready_to_evaluate = false;
                                break;
                            }
                            _ => {}
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
                    unary_evaluate();
                }
                SyntaxTreeNodeType::Primary(token) => {
                    todo!();
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

    result
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
                Token::Identifier(value) => value.clone(),
                _ => todo!(),
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
            format!(
                "== ${:?}, {}, {}\n",
                stored_at, child_0_rep, child_1_rep
            )
        }
        Token::NotEqual => {
            format!(
                "!= ${:?}, {}, {}\n",
                stored_at, child_0_rep, child_1_rep
            )
        }
        Token::LessThan => {
            format!(
                "< ${:?}, {}, {}\n",
                stored_at, child_0_rep, child_1_rep
            )
        }
        Token::LessThanEqualTo => {
            format!(
                "<= ${:?}, {}, {}\n",
                stored_at, child_0_rep, child_1_rep
            )
        }
        Token::GreaterThan => {
            format!(
                "> ${:?}, {}, {}\n",
                stored_at, child_0_rep, child_1_rep
            )
        }
        Token::GreaterThanEqualTo => {
            format!(
                ">= ${:?}, {}, {}\n",
                stored_at, child_0_rep, child_1_rep
            )
        }
        Token::Plus => {
            format!(
                "+ ${:?}, {}, {}\n",
                stored_at, child_0_rep, child_1_rep
            )
        }
        Token::Minus => {
            format!(
                "+ ${:?}, {}, {}\n",
                stored_at, child_0_rep, child_1_rep
            )
        }
        Token::Multiply => {
            format!(
                "* ${:?}, {}, {}\n",
                stored_at, child_0_rep, child_1_rep
            )
        }
        Token::Divide => {
            format!(
                "/ ${:?}, {}, {}\n",
                stored_at, child_0_rep, child_1_rep
            )
        }
        _ => {
            // this should never happen
            todo!();
        }
    };

    result.push_str(&string);
}

fn unary_evaluate() {
    todo!();
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::parser::parse;

    #[test]
    fn primary_only() {
        todo!();
    }

    #[test]
    fn add_two() {
        let tree = match parse(&vec![
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
        let code = generate(&tree);

        let expected = concat!("+ $0, 1, 2\n");
        assert_eq!(code, expected);
    }
}