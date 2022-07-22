#[cfg(test)]
use pretty_assertions::assert_eq as pretty_assert_eq;

use super::lexer::Lexer;
use crate::util::SectorResult;
use std::{fmt, string::String};
use {
    BinaryOperatorKind::*, Expr::*, LiteralKind::*, PunctuationKind::*, Statement::*,
    UnaryOperatorKind::*,
};

/// Literal kinds
#[derive(Debug, Clone, PartialEq)]
pub(super) enum LiteralKind {
    String(String),
    Character(char),
    Boolean(bool),
    Numeric(String),
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                String(s) => format!("String({})", s),
                Character(c) => format!("Char({})", c),
                Boolean(b) => format!("Bool({})", b),
                Numeric(s) => format!("Numeric({})", s),
            }
        )
    }
}

/// Punctuation in the Sec query language
#[derive(Debug, Clone, PartialEq)]
enum PunctuationKind {
    Comma,
    Period,
    Semicolon,
    LParen,
    RParen,
    LSq,
    RSq,
    LBrace,
    RBrace,
}

impl fmt::Display for PunctuationKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Comma => ",",
                Period => ".",
                Semicolon => ";",
                LParen => "(",
                RParen => ")",
                LSq => "[",
                RSq => "]",
                LBrace => "{",
                RBrace => "}",
            }
        )
    }
}

/// Unary operators
#[derive(Debug, Clone, PartialEq)]
enum UnaryOperatorKind {
    SqRoot,
    CbRoot,
    Abs,
    BwNot,
    Pos,
    Neg,
    Fact,
    This,
}

impl fmt::Display for UnaryOperatorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SqRoot => "-/",
                CbRoot => "--/",
                Abs => "<0>",
                BwNot => "~",
                Pos => "+",
                Neg => "-",
                Fact => "!",
                This => "@",
            }
        )
    }
}

/// Binary operators
#[derive(Debug, Clone, PartialEq)]
enum BinaryOperatorKind {
    LogAnd,
    LogOr,
    Add,
    Sub,
    Div,
    Mod,
    Pow,
    Lt,
    LtEq,
    Gt,
    GtEq,
    EqTo,
    NotEqTo,
    BwAnd,
    BwOr,
    BwXor,
    BwShiftR,
    BwShiftL,
    In,
    Bind,
}

impl fmt::Display for BinaryOperatorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LogAnd => "&&",
                LogOr => "||",
                Add => "+",
                Sub => "-",
                Div => "/",
                Mod => "%",
                Pow => "^",
                Lt => "<",
                LtEq => "<=",
                Gt => ">",
                GtEq => ">=",
                EqTo => "=",
                NotEqTo => "!=",
                BwAnd => "&",
                BwOr => "|",
                BwXor => "^",
                BwShiftR => ">>",
                BwShiftL => "<<",
                In => "=>",
                Bind => "->",
            }
        )
    }
}

/// Statements are the building block of the Sec query language.
/// All statements end with a semicolon.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum Statement {
    Expression(Expr),
    Procedure {
        ident: String,
        params: Vec<(String, String)>,
        body: Expr,
    },
    Binding {
        ident: String,
        value: Expr,
    },
    Return(Expr),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expression(e) => e.to_string(),
                Procedure {
                    ident,
                    params,
                    body,
                } => format!(
                    "proc {}({}) {{ {} }}",
                    ident,
                    params
                        .iter()
                        .map(|(ident, ty)| format!("{}: {}", ident, ty))
                        .collect::<Vec<String>>()
                        .join(", "),
                    body
                ),
                Self::Binding { ident, value } => format!("bind {} -> {}", ident, value),
                Return(e) => format!("return {}", e),
            }
        )
    }
}

/// Expressions are things that evaluate to some value
#[derive(Debug, Clone, PartialEq)]
pub(super) enum Expr {
    Literal(LiteralKind),
    Binding { ident: String, value: Box<Expr> },
    ProcCall { ident: String, args: Vec<Expr> },
    Block(Vec<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Literal(l) => l.to_string(),
                Self::Binding { ident, value } => format!("Binding[{} -> {}]", ident, value),
                ProcCall { ident, args } => format!(
                    "ProcCall[{}({})]",
                    ident,
                    args.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Block(c) => format!(
                    "Block[{}]",
                    c.iter()
                        .map(ToString::to_string)
                        .collect::<Vec<String>>()
                        .join("; ")
                ),
            }
        )
    }
}
