use super::KEYWORDS;
use crate::util::{SectorError, SectorResult};
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, fmt, iter::Peekable, ops::RangeInclusive, str::Chars};
use TokenKind::*;

#[cfg(test)]
use pretty_assertions::assert_eq as pretty_assert_eq;

/// A token is a lexical unit of the Sec query language.
/// It can be a punctuation mark, operator, keyword, user-defined identifier, or delimiter.
/// Delimeters like *whitespace* `(' ')`, *newlines* `('\n')`, and *tabs* `('\t')` are only
/// recognized as tokens to separate tokens from each other. Sec's lexer recognizes *EoF* `(End of File)` as a token
/// to show where the source code ends.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
enum TokenKind {
    // Punctuation
    Comma,
    Period,
    Semicolon,
    LParen,
    RParen,
    LSq,
    RSq,
    LBrace,
    RBrace,

    // Operators
    SqRoot,
    CbRoot,
    Abs,
    BwNot,
    Pos,
    Neg,
    Fact,
    LogAnd,
    LogOr,
    Add,
    Sub,
    Asterisk, // multiplication or wildcard
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
    Assign,
    This,
    // `XQ` stands for `XQuery`. These operators are used in the context of a predicate.
    // *This* `(@)` and *XqThis* `(_)` are the same but used in different contexts.
    XQThis,
    XQOr,
    XQIs,
    XQIn,
    XQAnd,

    // Keywords
    Keyword(String),

    // Literal
    StringLiteral(String),
    CharacterLiteral(char),
    BooleanLiteral(bool),
    NumericLiteral(String, bool),
    NullLiteral,

    // Delimiter
    Newline,
    Tab,
    Whitespace,

    // User defined identifiers
    Ident(String),

    // Other
    SingleLineComment(String),
    MultiLineComment(String),
    EoF,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Comma => write!(f, ","),
            Period => write!(f, "."),
            Semicolon => write!(f, ";"),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LSq => write!(f, "["),
            RSq => write!(f, "]"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            SqRoot => write!(f, "-/"),
            CbRoot => write!(f, "--/"),
            Abs => write!(f, "<0>"),
            BwNot => write!(f, "~"),
            Pos => write!(f, "+"),
            Neg => write!(f, "-"),
            Fact => write!(f, "!"),
            LogAnd => write!(f, "&&"),
            LogOr => write!(f, "||"),
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Asterisk => write!(f, "*"),
            Div => write!(f, "/"),
            Mod => write!(f, "%"),
            Pow => write!(f, "**"),
            Lt => write!(f, "<"),
            LtEq => write!(f, "<="),
            Gt => write!(f, ">"),
            GtEq => write!(f, ">="),
            EqTo => write!(f, "=="),
            NotEqTo => write!(f, "!="),
            BwAnd => write!(f, "&"),
            BwOr => write!(f, "|"),
            BwXor => write!(f, "^"),
            BwShiftR => write!(f, ">>"),
            BwShiftL => write!(f, "<<"),
            In => write!(f, "=>"),
            Assign => write!(f, "="),
            This => write!(f, "@"),
            XQThis => write!(f, "_"),
            XQOr => write!(f, "or"),
            XQIs => write!(f, "is"),
            XQIn => write!(f, "in"),
            XQAnd => write!(f, "and"),
            Keyword(s) => write!(f, "{}", s),
            StringLiteral(s) => write!(f, "{}", s),
            CharacterLiteral(c) => write!(f, "{}", c),
            BooleanLiteral(b) => write!(f, "{}", b),
            NumericLiteral(s, _) => write!(f, "{}", s),
            NullLiteral => write!(f, "null"),
            Newline => writeln!(f),
            Tab => write!(f, "\t"),
            Whitespace => write!(f, " "),
            Ident(s) => write!(f, "{}", s),
            SingleLineComment(s) => write!(f, "// {}", s),
            MultiLineComment(s) => write!(f, "/* {} */", s),
            EoF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Token(TokenKind, (usize, RangeInclusive<usize>));

/// Generate a stream of `Token`'s from some source string.
///
/// # Examples
///
/// ```
/// use sector_core::{util::SectorResult,sec::lexer::Lexer};
/// use sector_core::util::SectorResult;
///
/// fn test_lex() -> SectorResult<()> {
///     let source = "select * from users;";
///     let mut lexer = Lexer::new(source);
///     let tokens = lexer.lex()?;
///
///     Ok(())
/// }
/// ```
pub struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    cursor: (usize, usize),
}

impl<'a> Lexer<'a> {
    /// Create a new `Lexer` instance and pass in a string representing the source code to lex.
    ///
    /// # Examples
    ///
    /// ```
    /// use sector_core::sec::lexer::Lexer;
    ///
    /// let source = "select * from users;";
    /// let mut lexer = Lexer::new(source);
    /// ```
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source.chars().peekable(),
            cursor: (1, 1),
        }
    }

    fn new_token(&mut self, kind: TokenKind, len: usize) -> Token {
        Token(kind, (self.cursor.0, self.cursor.1 - len..=self.cursor.1))
    }

    /// Lex the source string and return a vector of `Token`'s.
    pub fn lex(&mut self) -> SectorResult<Vec<Token>> {
        let mut tokens: Vec<Token> = Vec::new();

        while !self.at_end() {
            let token = self.lex_token()?;
            tokens.push(token);
        }

        // need to fix `EoF` token not being pushed inside `lex_token` when `advance` returns `None`
        // this push below should be temporary
        if self.at_end() {
            tokens.push(self.new_token(EoF, 0));
        }

        Ok(tokens)
    }

    /// Wrapper around `Peekable.peek` to make peeking more ergonomic.
    #[rustfmt::skip]
    fn peek(&mut self) -> Option<&char> { self.source.peek() }

    #[rustfmt::skip]
    fn at_end(&mut self) -> bool { self.peek().is_none() }

    fn advance(&mut self) -> Option<char> {
        let next = self.source.next();

        if let Some(c) = next {
            match c {
                '\n' => {
                    self.cursor.0 += 1;
                    self.cursor.1 = 1;
                }
                '\t' => self.cursor.1 += 4,
                _ => self.cursor.1 += 1,
            }
        }

        next
    }

    /// Wrapper around `advance` to advance by `times` times.
    /// This is to prevent having to call `advance` multiple times in the case
    /// that we want to advance by more than one character. This method optionally return a `Vec` of the characters
    /// that were consumed while advancing `times` times, based on a boolean passed as `vec_out`.
    fn advance_by(&mut self, times: usize, vec_out: bool) -> Option<Vec<char>> {
        if vec_out {
            let mut consumed = Vec::with_capacity(times);

            for _ in 0..times {
                consumed.push(self.advance()?);
            }

            Some(consumed)
        } else {
            for _ in 0..times {
                self.advance();
            }

            None
        }
    }

    fn consume_until<P>(&mut self, predicate: P) -> String
    where
        P: Fn(char) -> bool,
    {
        let mut consumed = String::new();

        while let Some(&c) = self.peek() {
            if predicate(c) {
                break;
            }

            consumed.push(c);
            self.advance();
        }

        consumed
    }

    fn lex_token(&mut self) -> SectorResult<Token> {
        // TODO:
        // - finish implementing multi-line comments
        // - fix Tab (`\t`) lexing
        // - finish `lex_numeric_literal`
        // - implement lexing for special things like generics, procedure calls, this references, etc.
        // - think about edge cases

        if let Some(c) = self.advance() {
            return match c {
                '\n' => Ok(self.new_token(Newline, 1)),
                '\t' => Ok(self.new_token(Tab, 4)),
                ' ' => Ok(self.new_token(Whitespace, 1)),
                '(' => Ok(self.new_token(LParen, 1)),
                ')' => Ok(self.new_token(RParen, 1)),
                '[' => Ok(self.new_token(LSq, 1)),
                ']' => Ok(self.new_token(RSq, 1)),
                '{' => Ok(self.new_token(LBrace, 1)),
                '}' => Ok(self.new_token(RBrace, 1)),
                '.' => matches!(self.peek(), Some(c) if c.is_numeric())
                    .then(|| self.lex_numeric_literal('.'))
                    .unwrap_or_else(|| Ok(self.new_token(Period, 1))),
                ',' => Ok(self.new_token(Comma, 1)),
                '=' => matches!(self.peek(), Some(&'='))
                    .then(|| {
                        self.advance();
                        Ok(self.new_token(EqTo, 2))
                    })
                    .or_else(|| {
                        matches!(self.peek(), Some(&'>')).then(|| {
                            self.advance();
                            Ok(self.new_token(In, 2))
                        })
                    })
                    .unwrap_or_else(|| Ok(self.new_token(Assign, 1))),
                '+' => matches!(self.peek(), Some(c) if c.is_numeric())
                    .then(|| Ok(self.new_token(Pos, 1)))
                    .unwrap_or_else(|| Ok(self.new_token(Add, 1))),
                '-' => matches!(self.peek(), Some(c) if c.is_numeric())
                    .then(|| Ok(self.new_token(Neg, 1)))
                    .or_else(|| {
                        matches!(self.peek(), Some(&'/')).then(|| {
                            self.advance();
                            Ok(self.new_token(SqRoot, 2))
                        })
                    })
                    .or_else(|| {
                        matches!(self.peek(), Some(&'-')).then(|| {
                            self.advance_by(2, false);
                            Ok(self.new_token(CbRoot, 3))
                        })
                    })
                    .unwrap_or_else(|| Ok(self.new_token(Sub, 1))),
                '*' => matches!(self.peek(), Some(&'*'))
                    .then(|| {
                        self.advance();
                        Ok(self.new_token(Pow, 2))
                    })
                    .unwrap_or_else(|| Ok(self.new_token(Asterisk, 1))),
                '/' => matches!(self.peek(), Some(&'/'))
                    .then(|| {
                        self.advance();
                        let comment = Cow::from(self.consume_until(|c| c == '\n'));
                        Ok(self.new_token(
                            SingleLineComment(comment.trim_start().to_string()),
                            comment.len() + 2,
                        ))
                    })
                    .or_else(|| {
                        matches!(self.peek(), Some(&'*')).then(|| {
                            self.advance();
                            let mut comment = Cow::from(String::new());

                            loop {
                                if let Some(c) = self.advance() {
                                    if c == '*' && self.peek().map_or(false, |&c| c == '/') {
                                        self.advance();
                                        break;
                                    }

                                    comment.to_mut().push(c);
                                } else {
                                    return Err(SectorError::parse_error(self.cursor.0, self.cursor.1, "unexpected termination while lexing multiline comment"));
                                }
                            }

                            Ok(self.new_token(
                                MultiLineComment(comment.trim().to_string()),
                                comment.len(),
                            ))
                        })
                    })
                    .unwrap_or_else(|| Ok(self.new_token(Div, 1))),
                '%' => Ok(self.new_token(Mod, 1)),
                '<' => matches!(self.peek(), Some(&'='))
                    .then(|| {
                        self.advance();
                        Ok(self.new_token(LtEq, 2))
                    })
                    .or_else(|| {
                        matches!(self.peek(), Some(&'<')).then(|| {
                            self.advance();
                            Ok(self.new_token(BwShiftL, 2))
                        })
                    })
                    .or_else(|| {
                        matches!(self.peek(), Some(&'0')).then(|| {
                            self.advance();
                            matches!(self.peek(), Some(&'>'))
                                .then(|| {
                                    self.advance();
                                    Ok(self.new_token(Abs, 3))
                                })
                                .ok_or_else(|| SectorError::parse_error(self.cursor.0, self.cursor.1, "unexpected termination while lexing an operator starting with '<'. expected '>', found EoF"))?
                        })
                    })
                    .unwrap_or_else(|| Ok(self.new_token(Lt, 1))),
                '>' => matches!(self.peek(), Some(&'='))
                    .then(|| {
                        self.advance();
                        Ok(self.new_token(GtEq, 2))
                    })
                    .or_else(|| {
                        matches!(self.peek(), Some(&'>')).then(|| {
                            self.advance();
                            Ok(self.new_token(BwShiftR, 2))
                        })
                    })
                    .unwrap_or_else(|| Ok(self.new_token(Gt, 1))),
                '!' => matches!(self.peek(), Some(&'='))
                    .then(|| {
                        self.advance();
                        Ok(self.new_token(NotEqTo, 2))
                    })
                    .unwrap_or_else(|| Ok(self.new_token(Fact, 1))),
                '&' => matches!(self.peek(), Some(&'&'))
                    .then(|| {
                        self.advance();
                        Ok(self.new_token(LogAnd, 2))
                    })
                    .unwrap_or_else(|| Ok(self.new_token(BwAnd, 1))),
                '|' => matches!(self.peek(), Some(&'|'))
                    .then(|| {
                        self.advance();
                        Ok(self.new_token(LogOr, 2))
                    })
                    .unwrap_or_else(|| Ok(self.new_token(BwOr, 1))),
                '~' => Ok(self.new_token(BwNot, 1)),
                '^' => Ok(self.new_token(BwXor, 1)),
                '@' => Ok(self.new_token(This, 1)),
                '_' => matches!(self.peek(), Some(c) if c.is_alphanumeric())
                    .then(|| Ok(self.lex_ident('_')))
                    .unwrap_or_else(|| Ok(self.new_token(XQThis, 1))),
                'a'..='z' | 'A'..='Z' => Ok(self.lex_ident(c)),
                '0'..='9' => self.lex_numeric_literal(c),
                '\'' => self.lex_char_literal(),
                '"' => self.lex_string_literal(),
                _ => Err(SectorError::parse_error(self.cursor.0, self.cursor.1, "unexpected token: {c:?}")),
            };
        }

        Ok(self.new_token(EoF, 0))
    }

    /// Lex user-defined identifiers and reserved identifiers (keywords).
    fn lex_ident(&mut self, first: char) -> Token {
        let mut ident = Cow::from(String::from(first));

        if !self.at_end() {
            ident
                .to_mut()
                .push_str(&self.consume_until(|c| !c.is_alphanumeric() && c != '_'));
        }

        if KEYWORDS.contains(&&*ident) {
            return match ident.as_ref() {
                "or" => self.new_token(XQOr, 2),
                "is" => self.new_token(XQIs, 2),
                "in" => self.new_token(XQIn, 2),
                "and" => self.new_token(XQAnd, 3),
                "null" => self.new_token(NullLiteral, 4),
                "true" => self.new_token(BooleanLiteral(true), 4),
                "false" => self.new_token(BooleanLiteral(false), 5),
                _ => self.new_token(Keyword(ident.to_string()), ident.len()),
            };
        }

        self.new_token(Ident(ident.to_string()), ident.len())
    }

    fn lex_char_literal(&mut self) -> SectorResult<Token> {
        let consumed = self.advance();

        if let Some('\'') = consumed {
            return Err(SectorError::parse_error(self.cursor.0, self.cursor.1, "unexpected termination while lexing 'char' literal. expected codepoint, found closing quote"));
        } else if consumed.is_none() {
            return Err(SectorError::parse_error(
                self.cursor.0,
                self.cursor.1,
                "unexpected termination while lexing 'char' literal. expected codepoint, found EoF",
            ));
        }

        if let Some(&c) = self.peek() {
            if c != '\'' {
                return Err(SectorError::parse_error(
                    self.cursor.0,
                    self.cursor.1,
                    "'char' literal can only contain one codepoint",
                ));
            } else {
                self.advance();
            }
        } else {
            return Err(SectorError::parse_error(self.cursor.0, self.cursor.1, "unexpected termination while lexing 'char' literal. expected closing quote, found EoF"));
        }

        Ok(self.new_token(CharacterLiteral(consumed.unwrap()), 3))
    }

    fn lex_string_literal(&mut self) -> SectorResult<Token> {
        let mut string_literal = Cow::from(String::new());

        if !self.at_end() {
            string_literal
                .to_mut()
                .push_str(&self.consume_until(|c| c == '"'));
        } else {
            return Err(SectorError::parse_error(
                self.cursor.0,
                self.cursor.1,
                "unexpected termination while lexing 'string' literal. Expected '\"', found EoF",
            ));
        }

        self.advance();

        Ok(self.new_token(
            StringLiteral(string_literal.to_string()),
            string_literal.len() + 2,
        ))
    }

    #[allow(unused_must_use)]
    fn lex_numeric_literal(&mut self, first: char) -> SectorResult<Token> {
        // Temporary reference to help with integral checks
        //
        // Integral:
        // integers (1, 2, 10, 50, -20, -50, 0, etc)
        // hexadecimal (0xFF0, 0xFA2, etc)
        // octal (0o777, 0o0, etc)
        // binary (0b1010, 0b0, etc)
        //
        // Non-integral:
        // floating-point numbers/decimals (1.0, 2.0, 10.0, 50.0, -20.0, -50.0, 0.0, etc)

        let mut integral = true;
        let mut value = Cow::from(String::from(first));

        if !self.at_end() {
            value
                .to_mut()
                .push_str(&self.consume_until(|c| !c.is_alphanumeric() && c != '.'));
        }

        if value.contains('.') && value.matches('.').count() > 1 {
            return Err(SectorError::parse_error(
                self.cursor.0,
                self.cursor.1,
                "invalid numeric literal: multiple decimal points",
            ));
        }

        value.as_ref().chars(); // probably will use `Chars` iterator and methods for checking integrality

        Ok(self.new_token(NumericLiteral(value.to_string(), integral), value.len()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_punctuation() -> SectorResult<()> {
        let source = r#"( ) [ ] { } . ,"#;
        let mut lexer = Lexer::new(source);
        let lexed = lexer.lex()?;

        pretty_assert_eq!(lexed.len(), 16);
        pretty_assert_eq!(lexer.cursor, (1, 16));

        pretty_assert_eq!(
            lexed,
            vec![
                Token(LParen, (1, 1..=2)),
                Token(Whitespace, (1, 2..=3)),
                Token(RParen, (1, 3..=4)),
                Token(Whitespace, (1, 4..=5)),
                Token(LSq, (1, 5..=6)),
                Token(Whitespace, (1, 6..=7)),
                Token(RSq, (1, 7..=8)),
                Token(Whitespace, (1, 8..=9)),
                Token(LBrace, (1, 9..=10)),
                Token(Whitespace, (1, 10..=11)),
                Token(RBrace, (1, 11..=12)),
                Token(Whitespace, (1, 12..=13)),
                Token(Period, (1, 13..=14)),
                Token(Whitespace, (1, 14..=15)),
                Token(Comma, (1, 15..=16)),
                Token(EoF, (1, 16..=16))
            ]
        );

        Ok(())
    }

    #[test]
    fn test_lex_operators() -> SectorResult<()> {
        let source = "= == => + +1 +20 - -1 -20 * ** / % < <= << > >= >> != & && | || ~ ^ @ _ or is in and <0> -/ --/ 5! 20!";
        let mut lexer = Lexer::new(source);
        let lexed = lexer.lex()?;

        pretty_assert_eq!(lexed.len(), 80);
        pretty_assert_eq!(lexer.cursor, (1, 103));
        pretty_assert_eq!(
            lexed,
            vec![
                Token(Assign, (1, 1..=2)),
                Token(Whitespace, (1, 2..=3)),
                Token(EqTo, (1, 3..=5)),
                Token(Whitespace, (1, 5..=6)),
                Token(In, (1, 6..=8)),
                Token(Whitespace, (1, 8..=9)),
                Token(Add, (1, 9..=10)),
                Token(Whitespace, (1, 10..=11)),
                Token(Pos, (1, 11..=12)),
                Token(NumericLiteral("1".to_owned(), true), (1, 12..=13)),
                Token(Whitespace, (1, 13..=14)),
                Token(Pos, (1, 14..=15)),
                Token(NumericLiteral("20".to_owned(), true), (1, 15..=17)),
                Token(Whitespace, (1, 17..=18)),
                Token(Sub, (1, 18..=19)),
                Token(Whitespace, (1, 19..=20)),
                Token(Neg, (1, 20..=21)),
                Token(NumericLiteral("1".to_owned(), true), (1, 21..=22)),
                Token(Whitespace, (1, 22..=23)),
                Token(Neg, (1, 23..=24)),
                Token(NumericLiteral("20".to_owned(), true), (1, 24..=26)),
                Token(Whitespace, (1, 26..=27)),
                Token(Asterisk, (1, 27..=28)),
                Token(Whitespace, (1, 28..=29)),
                Token(Pow, (1, 29..=31)),
                Token(Whitespace, (1, 31..=32)),
                Token(Div, (1, 32..=33)),
                Token(Whitespace, (1, 33..=34)),
                Token(Mod, (1, 34..=35)),
                Token(Whitespace, (1, 35..=36)),
                Token(Lt, (1, 36..=37)),
                Token(Whitespace, (1, 37..=38)),
                Token(LtEq, (1, 38..=40)),
                Token(Whitespace, (1, 40..=41)),
                Token(BwShiftL, (1, 41..=43)),
                Token(Whitespace, (1, 43..=44)),
                Token(Gt, (1, 44..=45)),
                Token(Whitespace, (1, 45..=46)),
                Token(GtEq, (1, 46..=48)),
                Token(Whitespace, (1, 48..=49)),
                Token(BwShiftR, (1, 49..=51)),
                Token(Whitespace, (1, 51..=52)),
                Token(NotEqTo, (1, 52..=54)),
                Token(Whitespace, (1, 54..=55)),
                Token(BwAnd, (1, 55..=56)),
                Token(Whitespace, (1, 56..=57)),
                Token(LogAnd, (1, 57..=59)),
                Token(Whitespace, (1, 59..=60)),
                Token(BwOr, (1, 60..=61)),
                Token(Whitespace, (1, 61..=62)),
                Token(LogOr, (1, 62..=64)),
                Token(Whitespace, (1, 64..=65)),
                Token(BwNot, (1, 65..=66)),
                Token(Whitespace, (1, 66..=67)),
                Token(BwXor, (1, 67..=68)),
                Token(Whitespace, (1, 68..=69)),
                Token(This, (1, 69..=70)),
                Token(Whitespace, (1, 70..=71)),
                Token(XQThis, (1, 71..=72)),
                Token(Whitespace, (1, 72..=73)),
                Token(XQOr, (1, 73..=75)),
                Token(Whitespace, (1, 75..=76)),
                Token(XQIs, (1, 76..=78)),
                Token(Whitespace, (1, 78..=79)),
                Token(XQIn, (1, 79..=81)),
                Token(Whitespace, (1, 81..=82)),
                Token(XQAnd, (1, 82..=85)),
                Token(Whitespace, (1, 85..=86)),
                Token(Abs, (1, 86..=89)),
                Token(Whitespace, (1, 89..=90)),
                Token(SqRoot, (1, 90..=92)),
                Token(Whitespace, (1, 92..=93)),
                Token(CbRoot, (1, 93..=96)),
                Token(Whitespace, (1, 96..=97)),
                Token(NumericLiteral("5".to_owned(), true), (1, 97..=98)),
                Token(Fact, (1, 98..=99)),
                Token(Whitespace, (1, 99..=100)),
                Token(NumericLiteral("20".to_owned(), true), (1, 100..=102)),
                Token(Fact, (1, 102..=103)),
                Token(EoF, (1, 103..=103))
            ]
        );

        Ok(())
    }

    #[test]
    fn test_lex_keywords() -> SectorResult<()> {
        let source = "or is in and create proc insert table with into return select from where update set on count delete link match left right cursor drop move next transaction begin commit abort index for exec var if elif else string char dyn guid int bigint short long decimal bigdecimal hex scn bool scr obj dtm tsn dto bin json xml sql null true false";
        let mut lexer = Lexer::new(source);
        let lexed = lexer.lex()?;

        pretty_assert_eq!(lexed.len(), 126);
        pretty_assert_eq!(lexer.cursor, (1, 335));
        pretty_assert_eq!(
            lexed,
            vec![
                Token(XQOr, (1, 1..=3)),
                Token(Whitespace, (1, 3..=4)),
                Token(XQIs, (1, 4..=6)),
                Token(Whitespace, (1, 6..=7)),
                Token(XQIn, (1, 7..=9)),
                Token(Whitespace, (1, 9..=10)),
                Token(XQAnd, (1, 10..=13)),
                Token(Whitespace, (1, 13..=14)),
                Token(Keyword("create".to_owned()), (1, 14..=20)),
                Token(Whitespace, (1, 20..=21)),
                Token(Keyword("proc".to_owned()), (1, 21..=25)),
                Token(Whitespace, (1, 25..=26)),
                Token(Keyword("insert".to_owned()), (1, 26..=32)),
                Token(Whitespace, (1, 32..=33)),
                Token(Keyword("table".to_owned()), (1, 33..=38)),
                Token(Whitespace, (1, 38..=39)),
                Token(Keyword("with".to_owned()), (1, 39..=43)),
                Token(Whitespace, (1, 43..=44)),
                Token(Keyword("into".to_owned()), (1, 44..=48)),
                Token(Whitespace, (1, 48..=49)),
                Token(Keyword("return".to_owned()), (1, 49..=55)),
                Token(Whitespace, (1, 55..=56)),
                Token(Keyword("select".to_owned()), (1, 56..=62)),
                Token(Whitespace, (1, 62..=63)),
                Token(Keyword("from".to_owned()), (1, 63..=67)),
                Token(Whitespace, (1, 67..=68)),
                Token(Keyword("where".to_owned()), (1, 68..=73)),
                Token(Whitespace, (1, 73..=74)),
                Token(Keyword("update".to_owned()), (1, 74..=80)),
                Token(Whitespace, (1, 80..=81)),
                Token(Keyword("set".to_owned()), (1, 81..=84)),
                Token(Whitespace, (1, 84..=85)),
                Token(Keyword("on".to_owned()), (1, 85..=87)),
                Token(Whitespace, (1, 87..=88)),
                Token(Keyword("count".to_owned()), (1, 88..=93)),
                Token(Whitespace, (1, 93..=94)),
                Token(Keyword("delete".to_owned()), (1, 94..=100)),
                Token(Whitespace, (1, 100..=101)),
                Token(Keyword("link".to_owned()), (1, 101..=105)),
                Token(Whitespace, (1, 105..=106)),
                Token(Keyword("match".to_owned()), (1, 106..=111)),
                Token(Whitespace, (1, 111..=112)),
                Token(Keyword("left".to_owned()), (1, 112..=116)),
                Token(Whitespace, (1, 116..=117)),
                Token(Keyword("right".to_owned()), (1, 117..=122)),
                Token(Whitespace, (1, 122..=123)),
                Token(Keyword("cursor".to_owned()), (1, 123..=129)),
                Token(Whitespace, (1, 129..=130)),
                Token(Keyword("drop".to_owned()), (1, 130..=134)),
                Token(Whitespace, (1, 134..=135)),
                Token(Keyword("move".to_owned()), (1, 135..=139)),
                Token(Whitespace, (1, 139..=140)),
                Token(Keyword("next".to_owned()), (1, 140..=144)),
                Token(Whitespace, (1, 144..=145)),
                Token(Keyword("transaction".to_owned()), (1, 145..=156)),
                Token(Whitespace, (1, 156..=157)),
                Token(Keyword("begin".to_owned()), (1, 157..=162)),
                Token(Whitespace, (1, 162..=163)),
                Token(Keyword("commit".to_owned()), (1, 163..=169)),
                Token(Whitespace, (1, 169..=170)),
                Token(Keyword("abort".to_owned()), (1, 170..=175)),
                Token(Whitespace, (1, 175..=176)),
                Token(Keyword("index".to_owned()), (1, 176..=181)),
                Token(Whitespace, (1, 181..=182)),
                Token(Keyword("for".to_owned()), (1, 182..=185)),
                Token(Whitespace, (1, 185..=186)),
                Token(Keyword("exec".to_owned()), (1, 186..=190)),
                Token(Whitespace, (1, 190..=191)),
                Token(Keyword("var".to_owned()), (1, 191..=194)),
                Token(Whitespace, (1, 194..=195)),
                Token(Keyword("if".to_owned()), (1, 195..=197)),
                Token(Whitespace, (1, 197..=198)),
                Token(Keyword("elif".to_owned()), (1, 198..=202)),
                Token(Whitespace, (1, 202..=203)),
                Token(Keyword("else".to_owned()), (1, 203..=207)),
                Token(Whitespace, (1, 207..=208)),
                Token(Keyword("string".to_owned()), (1, 208..=214)),
                Token(Whitespace, (1, 214..=215)),
                Token(Keyword("char".to_owned()), (1, 215..=219)),
                Token(Whitespace, (1, 219..=220)),
                Token(Keyword("dyn".to_owned()), (1, 220..=223)),
                Token(Whitespace, (1, 223..=224)),
                Token(Keyword("guid".to_owned()), (1, 224..=228)),
                Token(Whitespace, (1, 228..=229)),
                Token(Keyword("int".to_owned()), (1, 229..=232)),
                Token(Whitespace, (1, 232..=233)),
                Token(Keyword("bigint".to_owned()), (1, 233..=239)),
                Token(Whitespace, (1, 239..=240)),
                Token(Keyword("short".to_owned()), (1, 240..=245)),
                Token(Whitespace, (1, 245..=246)),
                Token(Keyword("long".to_owned()), (1, 246..=250)),
                Token(Whitespace, (1, 250..=251)),
                Token(Keyword("decimal".to_owned()), (1, 251..=258)),
                Token(Whitespace, (1, 258..=259)),
                Token(Keyword("bigdecimal".to_owned()), (1, 259..=269)),
                Token(Whitespace, (1, 269..=270)),
                Token(Keyword("hex".to_owned()), (1, 270..=273)),
                Token(Whitespace, (1, 273..=274)),
                Token(Keyword("scn".to_owned()), (1, 274..=277)),
                Token(Whitespace, (1, 277..=278)),
                Token(Keyword("bool".to_owned()), (1, 278..=282)),
                Token(Whitespace, (1, 282..=283)),
                Token(Keyword("scr".to_owned()), (1, 283..=286)),
                Token(Whitespace, (1, 286..=287)),
                Token(Keyword("obj".to_owned()), (1, 287..=290)),
                Token(Whitespace, (1, 290..=291)),
                Token(Keyword("dtm".to_owned()), (1, 291..=294)),
                Token(Whitespace, (1, 294..=295)),
                Token(Keyword("tsn".to_owned()), (1, 295..=298)),
                Token(Whitespace, (1, 298..=299)),
                Token(Keyword("dto".to_owned()), (1, 299..=302)),
                Token(Whitespace, (1, 302..=303)),
                Token(Keyword("bin".to_owned()), (1, 303..=306)),
                Token(Whitespace, (1, 306..=307)),
                Token(Keyword("json".to_owned()), (1, 307..=311)),
                Token(Whitespace, (1, 311..=312)),
                Token(Keyword("xml".to_owned()), (1, 312..=315)),
                Token(Whitespace, (1, 315..=316)),
                Token(Keyword("sql".to_owned()), (1, 316..=319)),
                Token(Whitespace, (1, 319..=320)),
                Token(NullLiteral, (1, 320..=324)),
                Token(Whitespace, (1, 324..=325)),
                Token(BooleanLiteral(true), (1, 325..=329)),
                Token(Whitespace, (1, 329..=330)),
                Token(BooleanLiteral(false), (1, 330..=335)),
                Token(EoF, (1, 335..=335))
            ]
        );

        Ok(())
    }

    #[test]
    fn test_lex_user_defined_idents() -> SectorResult<()> {
        let source = "_meta _meta_data hell0 _hell0 _hell0_w0r1d users email password first_name last_name token _Meta _Meta_Data Hell0 _Hell0 _Hell0_W0r1d Users Email Password First_Name Last_Name Token";
        let mut lexer = Lexer::new(source);
        let lexed = lexer.lex()?;

        pretty_assert_eq!(lexed.len(), 44);
        pretty_assert_eq!(lexer.cursor, (1, 182));
        pretty_assert_eq!(
            lexed,
            vec![
                Token(Ident("_meta".to_owned()), (1, 1..=6)),
                Token(Whitespace, (1, 6..=7)),
                Token(Ident("_meta_data".to_owned()), (1, 7..=17)),
                Token(Whitespace, (1, 17..=18)),
                Token(Ident("hell0".to_owned()), (1, 18..=23)),
                Token(Whitespace, (1, 23..=24)),
                Token(Ident("_hell0".to_owned()), (1, 24..=30)),
                Token(Whitespace, (1, 30..=31)),
                Token(Ident("_hell0_w0r1d".to_owned()), (1, 31..=43)),
                Token(Whitespace, (1, 43..=44)),
                Token(Ident("users".to_owned()), (1, 44..=49)),
                Token(Whitespace, (1, 49..=50)),
                Token(Ident("email".to_owned()), (1, 50..=55)),
                Token(Whitespace, (1, 55..=56)),
                Token(Ident("password".to_owned()), (1, 56..=64)),
                Token(Whitespace, (1, 64..=65)),
                Token(Ident("first_name".to_owned()), (1, 65..=75)),
                Token(Whitespace, (1, 75..=76)),
                Token(Ident("last_name".to_owned()), (1, 76..=85)),
                Token(Whitespace, (1, 85..=86)),
                Token(Ident("token".to_owned()), (1, 86..=91)),
                Token(Whitespace, (1, 91..=92)),
                Token(Ident("_Meta".to_owned()), (1, 92..=97)),
                Token(Whitespace, (1, 97..=98)),
                Token(Ident("_Meta_Data".to_owned()), (1, 98..=108)),
                Token(Whitespace, (1, 108..=109)),
                Token(Ident("Hell0".to_owned()), (1, 109..=114)),
                Token(Whitespace, (1, 114..=115)),
                Token(Ident("_Hell0".to_owned()), (1, 115..=121)),
                Token(Whitespace, (1, 121..=122)),
                Token(Ident("_Hell0_W0r1d".to_owned()), (1, 122..=134)),
                Token(Whitespace, (1, 134..=135)),
                Token(Ident("Users".to_owned()), (1, 135..=140)),
                Token(Whitespace, (1, 140..=141)),
                Token(Ident("Email".to_owned()), (1, 141..=146)),
                Token(Whitespace, (1, 146..=147)),
                Token(Ident("Password".to_owned()), (1, 147..=155)),
                Token(Whitespace, (1, 155..=156)),
                Token(Ident("First_Name".to_owned()), (1, 156..=166)),
                Token(Whitespace, (1, 166..=167)),
                Token(Ident("Last_Name".to_owned()), (1, 167..=176)),
                Token(Whitespace, (1, 176..=177)),
                Token(Ident("Token".to_owned()), (1, 177..=182)),
                Token(EoF, (1, 182..=182))
            ]
        );

        Ok(())
    }

    #[test]
    fn test_lex_single_line_comment() -> SectorResult<()> {
        let source = "// hello world";
        let mut lexer = Lexer::new(source);
        let lexed = lexer.lex()?;

        pretty_assert_eq!(lexed.len(), 2);
        pretty_assert_eq!(lexer.cursor, (1, 15));
        pretty_assert_eq!(
            lexed,
            vec![
                Token(SingleLineComment("hello world".to_owned()), (1, 1..=15)),
                Token(EoF, (1, 15..=15))
            ]
        );

        Ok(())
    }

    // #[test]
    // fn test_lex_multi_line_comment() -> SectorResult<()> {
    //     let source = "/* hello\nworld */";
    //     let mut lexer = Lexer::new(source);
    //     let lexed = lexer.lex()?;

    //     pretty_assert_eq!(lexed.len(), 2);
    //     pretty_assert_eq!(lexer.cursor, (1, 1));
    //     pretty_assert_eq!(lexed, vec![]);

    //     Ok(())
    // }

    // #[test]
    // fn test_lex_literals() -> SectorResult<()> {
    //     let source = "'c' \"string\" 1 10 .1 0.1 0o10 0x10 10e5 10e-5 2.1e5 2.1e-5";
    //     let mut lexer = Lexer::new(source);
    //     let lexed = lexer.lex()?;

    //     pretty_assert_eq!(lexed.len(), 29);
    //     pretty_assert_eq!(lexer.cursor, (1, 59));
    //     pretty_assert_eq!(
    //         lexed,
    //         vec![
    //             Token(CharacterLiteral('c'), (1, 1..=4)),
    //             Token(Whitespace, (1, 4..=5)),
    //             Token(StringLiteral("string".to_owned()), (1, 5..=12)),
    //             Token(Whitespace, (1, 12..=13)),
    //             Token(NumericLiteral("1".to_owned(), true), (1, 13..=14)),
    //             Token(Whitespace, (1, 14..=15)),
    //             Token(NumericLiteral("10".to_owned(), true), (1, 15..=16)),
    //             Token(Whitespace, (1, 16..=17)),
    //             Token(NumericLiteral(".1".to_owned(), false), (1, 17..=19)),
    //             Token(Whitespace, (1, 19..=20)),
    //             Token(NumericLiteral("0.1".to_owned(), false), (1, 20..=23)),
    //             Token(Whitespace, (1, 23..=24)),
    //             Token(NumericLiteral("0o10".to_owned(), true), (1, 24..=28)),
    //             Token(Whitespace, (1, 28..=29)),
    //             Token(NumericLiteral("0x10".to_owned(), true), (1, 29..=33)),
    //             Token(Whitespace, (1, 33..=34)),
    //             Token(NumericLiteral("10e5".to_owned(), true), (1, 34..=38)),
    //             Token(Whitespace, (1, 38..=39)),
    //             Token(NumericLiteral("10e-5".to_owned(), false), (1, 39..=44)),
    //             Token(Whitespace, (1, 44..=45)),
    //             Token(NumericLiteral("2.1e5".to_owned(), true), (1, 45..=50)),
    //             Token(Whitespace, (1, 50..=51)),
    //             Token(NumericLiteral("2.1e-5".to_owned(), false), (1, 51..=56)),
    //             Token(EoF, (1, 56..=56))
    //         ]
    //     );

    //     Ok(())
    // }
}
