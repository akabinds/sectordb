//! SectorDB's query language

mod ast;
mod codegen;
pub mod lexer;
pub(crate) mod parser;

use crate::util::{SectorError, SectorResult};
use lexer::Lexer;
use parser::Parser;
use std::{fs, path::PathBuf};

#[allow(clippy::redundant_closure)]
pub fn mount_parser(path_to_source: &PathBuf) -> SectorResult<()> {
    let source = fs::read_to_string(path_to_source).map_err(|e| SectorError::IoError(e))?;

    let mut lexer = Lexer::new(&source);
    let tokens = lexer.lex();

    if tokens.is_ok() {
        println!("{:?}", tokens.as_ref().unwrap());
    } else {
        eprintln!("{}", tokens.as_ref().unwrap_err());
    }

    let mut parser = Parser::new(tokens?);

    Ok(())
}

const RESERVED_IDENTIFIERS: [&str; 48] = [
    "proc",
    "index",
    "on",
    "transaction",
    "exec",
    "return",
    "cursor",
    "drop",
    "from",
    "update",
    "where",
    "set",
    "match",
    "right",
    "left",
    "inner",
    "temp",
    "select",
    "insert",
    "into",
    "with",
    "bind",
    "within",
    "scan",
    "down",
    "up",
    "str",
    "int",
    "shortint",
    "tinyint",
    "bigint",
    "bool",
    "float",
    "bin",
    "hex",
    "oct",
    "scn",
    "guid",
    "arr",
    "obj",
    "char",
    "bit",
    "complex",
    "true",
    "false",
    "if",
    "elif",
    "else",
];
