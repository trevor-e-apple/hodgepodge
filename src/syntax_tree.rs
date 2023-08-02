/* TODO:
pretty print syntax tree
documentation
*/

use crate::scanner::Token;

#[derive(Default, Clone)]
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

#[derive(Default, Copy, Clone)]
pub struct SyntaxTreeNodeHandle {
    index: usize,
}

impl SyntaxTreeNodeHandle {
    #[cfg(test)]
    pub fn new() -> Self {
        SyntaxTreeNodeHandle {..Default::default()}
    }
}

#[derive(Default)]
pub struct SyntaxTreeNode {
    pub node_type: SyntaxTreeNodeType,
    pub children: Vec<SyntaxTreeNodeHandle>,
}

#[derive(Default)]
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

    #[cfg(test)]
    pub fn get_root_handle(&self) -> Option<SyntaxTreeNodeHandle> {
        if self.nodes.len() == 0 {
            None
        } else {
            Some(SyntaxTreeNodeHandle { index: 0 })
        }
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
}
