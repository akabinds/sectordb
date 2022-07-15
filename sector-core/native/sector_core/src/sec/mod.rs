//! SectorDB's query language

pub(crate) mod ast;
mod codegen;
pub mod lexer;

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