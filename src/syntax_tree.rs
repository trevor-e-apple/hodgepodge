/* TODO:
documentation
since no node ever has more than two children, maybe we can consider a static
array for the children instead of a vector (which is probably slower)
*/

use crate::scanner::Token;

#[derive(Default, Debug, Clone, PartialEq)]
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

#[derive(Default, Debug, Copy, Clone, PartialEq)]
pub enum GrammarRule {
    #[default]
    Expression,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Primary,
}

#[derive(Default, Debug, Copy, Clone, PartialEq)]
pub struct SyntaxTreeNodeHandle {
    pub index: usize,
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

#[derive(Default, Debug, Clone)]
pub struct SyntaxTreeNode {
    pub node_type: SyntaxTreeNodeType,
    pub children: Vec<SyntaxTreeNodeHandle>,
}

#[derive(Default, Debug, Clone)]
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

    pub fn get_root_handle(&self) -> Option<SyntaxTreeNodeHandle> {
        if self.nodes.len() == 0 {
            None
        } else {
            Some(SyntaxTreeNodeHandle { index: 0 })
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &SyntaxTreeNode> {
        self.nodes.iter()
    }
}

pub struct SearchEntry {
    pub node_handle: SyntaxTreeNodeHandle,
    pub depth: i32,
}

pub struct SyntaxTreeDfs<'a> {
    tree: &'a SyntaxTree,
    stack: Vec<SearchEntry>,
}

impl<'a> SyntaxTreeDfs<'a> {
    pub fn new(tree: &'a SyntaxTree) -> Self {
        match tree.get_root_handle() {
            Some(root_handle) => SyntaxTreeDfs {
                tree,
                stack: vec![SearchEntry { node_handle: root_handle, depth: 0 }],
            },
            None => SyntaxTreeDfs { tree, stack: vec![] },
        }
    }
}

impl Iterator for SyntaxTreeDfs<'_> {
    type Item = SearchEntry;

    fn next(&mut self) -> Option<Self::Item> {
        match self.stack.pop() {
            Some(stack_entry) => {
                // add children to stack
                match self.tree.get_node(stack_entry.node_handle) {
                    Some(tree_node) => {
                        // add children to stack in reverse order
                        // -- (so that LHS is on the left)
                        for child_handle in tree_node.children.iter().rev() {
                            self.stack.push(SearchEntry {
                                node_handle: *child_handle,
                                depth: stack_entry.depth + 1,
                            });
                        }
                        Some(stack_entry)
                    }
                    None => {
                        assert!(false);
                        None
                    }
                }
            }
            None => None,
        }
    }
}

impl SyntaxTree {
    #[allow(dead_code)]
    #[cfg(test)]
    pub fn pretty_print(&self) {
        for stack_entry in SyntaxTreeDfs::new(self) {
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
        }
    }
}

/// Returns a boolean indicating whether or not the two syntax trees are
/// equivalent. Since syntax tree nodes store their references internally,
/// we need a separate way to show equivalence between two trees (which may have
/// sorted their nodes differently)
#[allow(dead_code)]
#[cfg(test)]
pub fn equivalent(a: &SyntaxTree, b: &SyntaxTree) -> bool {
    if a.nodes.len() != b.nodes.len() {
        false
    } else {
        for (a_entry, b_entry) in
            SyntaxTreeDfs::new(a).zip(SyntaxTreeDfs::new(b))
        {
            let a_node = match a.get_node(a_entry.node_handle) {
                Some(node) => node,
                None => {
                    assert!(false);
                    return false;
                }
            };
            let b_node = match b.get_node(b_entry.node_handle) {
                Some(node) => node,
                None => {
                    assert!(false);
                    return false;
                }
            };

            if a_node.node_type != b_node.node_type {
                return false;
            }
        }

        true
    }
}
