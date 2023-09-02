use std::{collections::HashSet, vec};

use crate::syntax_tree::SyntaxTree;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct StatementHandle {
    index: usize,
}

pub struct Statement {
    pub type_declaration: Option<String>,
    pub variable: Option<String>,
    statements: Vec<StatementHandle>,
    pub expression: Option<SyntaxTree>,
}

pub struct Statements {
    statements: Vec<Statement>,
}

impl Statements {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }

    pub fn get_root_statement_handle(&self) -> Option<StatementHandle> {
        if self.statements.is_empty() {
            None
        } else {
            Some(StatementHandle { index: 0 })
        }
    }

    pub fn get_statement_mut(
        &mut self,
        handle: StatementHandle,
    ) -> Option<&mut Statement> {
        self.statements.get_mut(handle.index)
    }

    pub fn get_statement(&self, handle: StatementHandle) -> Option<&Statement> {
        self.statements.get(handle.index)
    }

    /// adds
    pub fn add_root_statement(&mut self) -> StatementHandle {
        let index = self.statements.len();
        self.statements.push(Statement {
            type_declaration: None,
            variable: None,
            statements: vec![],
            expression: None,
        });
        StatementHandle { index }
    }

    pub fn add_statement(
        &mut self,
        parent_handle: StatementHandle,
    ) -> StatementHandle {
        let index = self.statements.len();
        self.statements.push(Statement {
            type_declaration: None,
            variable: None,
            statements: vec![],
            expression: None,
        });
        let handle = StatementHandle { index };

        let parent = match self.get_statement_mut(parent_handle) {
            Some(parent) => parent,
            None => todo!(),
        };
        parent.statements.push(handle);
        handle
    }
}

#[derive(Copy, Clone)]
pub struct SearchEntry {
    pub handle: StatementHandle,
    pub depth: i32,
}

pub struct StatementsDfs<'a> {
    statements: &'a Statements,
    stack: Vec<SearchEntry>,
    visited: HashSet<StatementHandle>,
}

impl<'a> StatementsDfs<'a> {
    pub fn new(statements: &'a Statements) -> Self {
        let mut stack = vec![];
        if let Some(handle) = statements.get_root_statement_handle() {
            stack.push(SearchEntry { handle, depth: 0 })
        }
        Self {
            statements,
            stack,
            visited: HashSet::with_capacity(statements.statements.len()),
        }
    }
}

impl<'a> Iterator for StatementsDfs<'a> {
    type Item = SearchEntry;

    fn next(&mut self) -> Option<Self::Item> {
        if self.stack.is_empty() {
            return None;
        }

        // check the top of the stack
        while let Some(search_entry) = self.stack.last() {
            let search_entry = *search_entry;

            let stack_handle = search_entry.handle;
            if let Some(statement) = self.statements.get_statement(stack_handle)
            {
                if !statement.statements.is_empty() {
                    // if it has statements, add them to the stack
                    let visited_size_before = self.visited.len();
                    for child_statement in statement.statements.iter().rev() {
                        if !self.visited.contains(child_statement) {
                            self.stack.push(SearchEntry {
                                handle: *child_statement,
                                depth: search_entry.depth + 1,
                            });
                        }
                    }

                    // no children were added
                    if self.visited.len() == visited_size_before {
                        break;
                    }
                } else {
                    // if it doesn't, exit the loop
                    break;
                }
            }
        }

        match self.stack.pop() {
            Some(result) => {
                self.visited.insert(result.handle);
                Some(result)
            }
            None => None,
        }
    }
}

impl Statements {
    #[cfg(test)]
    pub fn pretty_print(&self) {
        for search_entry in StatementsDfs::new(self) {
            if let Some(statement) = self.get_statement(search_entry.handle) {
                if let Some(expression) = &statement.expression {
                    expression.pretty_print();
                }
            }
        }
    }
}

#[cfg(test)]
pub fn equivalent(a: &Statements, b: &Statements) -> bool {
    use crate::syntax_tree;

    if a.statements.len() != b.statements.len() {
        return false;
    }

    for (a_search, b_search) in StatementsDfs::new(a).zip(StatementsDfs::new(b))
    {
        let a_handle = a_search.handle;
        let b_handle = b_search.handle;

        let a_statement = match a.get_statement(a_handle) {
            Some(statement) => statement,
            None => todo!(),
        };
        let b_statement = match b.get_statement(b_handle) {
            Some(statement) => statement,
            None => todo!(),
        };
        if let Some(a_expression) = &a_statement.expression {
            if let Some(b_expression) = &b_statement.expression {
                if !syntax_tree::equivalent(a_expression, b_expression) {
                    return false;
                }
            }
        }
    }

    true
}
