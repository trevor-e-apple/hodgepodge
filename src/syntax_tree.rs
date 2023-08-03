/* TODO:
pretty print syntax tree
documentation
*/

use crate::scanner::Token;

#[derive(Default, Debug, Clone, PartialEq)]
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

#[derive(Default, Debug, Copy, Clone, PartialEq)]
pub struct SyntaxTreeNodeHandle {
    index: usize,
}

impl SyntaxTreeNodeHandle {
    #[allow(dead_code)]
    #[cfg(test)]
    pub fn new() -> Self {
        SyntaxTreeNodeHandle { ..Default::default() }
    }

    #[allow(dead_code)]
    #[cfg(test)]
    pub fn with_index(index: usize) -> Self {
        SyntaxTreeNodeHandle { index: index }
    }
}

#[derive(Default, Debug, PartialEq)]
pub struct SyntaxTreeNode {
    pub node_type: SyntaxTreeNodeType,
    pub children: Vec<SyntaxTreeNodeHandle>,
}

#[derive(Default, Debug, PartialEq)]
pub struct SyntaxTree {
    nodes: Vec<SyntaxTreeNode>,
}

impl SyntaxTree {
    pub fn new() -> Self {
        SyntaxTree { ..Default::default() }
    }

    pub fn add_node(
        &mut self,
        new_node: SyntaxTreeNode,
    ) -> SyntaxTreeNodeHandle {
        let new_node_index = self.nodes.len();
        self.nodes.push(new_node);
        SyntaxTreeNodeHandle { index: new_node_index }
    }

    pub fn get_node(
        &self,
        handle: SyntaxTreeNodeHandle,
    ) -> Option<&SyntaxTreeNode> {
        self.nodes.get(handle.index)
    }

    pub fn get_node_mut(
        &mut self,
        handle: SyntaxTreeNodeHandle,
    ) -> Option<&mut SyntaxTreeNode> {
        self.nodes.get_mut(handle.index)
    }

    #[allow(dead_code)]
    #[cfg(test)]
    pub fn get_root_handle(&self) -> Option<SyntaxTreeNodeHandle> {
        if self.nodes.len() == 0 {
            None
        } else {
            Some(SyntaxTreeNodeHandle { index: 0 })
        }
    }

    #[allow(dead_code)]
    #[cfg(test)]
    pub fn pretty_print(&self) {
        struct StackEntry {
            node_handle: SyntaxTreeNodeHandle,
            depth: i32,
        }

        let root_handle = match self.get_root_handle() {
            Some(handle) => handle,
            None => return,
        };
        let mut stack = vec![StackEntry { node_handle: root_handle, depth: 0 }];
        loop {
            let stack_entry = match stack.pop() {
                Some(entry) => entry,
                None => break,
            };
            // print current node info with the correct number of tabs
            for _ in 0..stack_entry.depth {
                print!("    ",);
            }
            let current_node = match self.get_node(stack_entry.node_handle) {
                Some(node) => node,
                None => {
                    assert!(false);
                    break;
                }
            };
            println!("{:?}", current_node.node_type);

            // add children to stack
            for child_handle in &current_node.children {
                stack.push(StackEntry {
                    node_handle: *child_handle,
                    depth: stack_entry.depth + 1,
                });
            }
        }
    }
}
