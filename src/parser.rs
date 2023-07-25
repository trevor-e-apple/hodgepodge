use crate::scanner::Token;

pub enum SyntaxTreeNodeType {
	Expression,
	Literal,
	Grouping,
	Unary,
	Binary,
	Operator,
	Terminal(Token),
}

pub struct SyntaxTreeNode {
	node_type: SyntaxTreeNodeType,
	children: Vec<SyntaxTreeNode>,
}

pub struct SyntaxTree {
	tree_data: Vec<SyntaxTreeNode>
}

/// A parser that takes a Vec of Tokens and produces a tree reflecting
/// the following grammar
/// 
/// expression -> literal | unary | binary | grouping;
/// literal -> NUMBER | STRING | "true" | "false";
/// grouping -> "(" expression ")";
/// unary -> ("!" | "-") expression;
/// binary -> expression operator expression
/// operator -> "==" | "!=" | "<" | "<=" | ">" | ">=" | "+"  | "-"  | "*" | "/" ;
pub fn parse() {

}