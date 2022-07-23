#[cfg(test)]
use pretty_assertions::assert_eq as pretty_assert_eq;

use super::{
    ast::{Node, NodeKind, Statement},
    lexer::{Lexer, Token},
};
use crate::util::SectorResult;

/// Data structure returned by the parser that represents a query AST.
/// This will be used throughout the project, most notably in the query planner and executor.
pub(crate) struct Query {
    nodes: Vec<Node>,
}

pub(super) struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub(super) fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    /// Parse the tokens into an AST.
    pub(super) fn parse(&mut self) -> SectorResult<Query> {
        let mut nodes: Vec<Node> = vec![];
        let tokens = &self.tokens;

        Ok(Query { nodes })
    }
}
