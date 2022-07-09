use super::RESERVED_IDENTIFIERS;
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
    Binding,
    This,

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
            Binding => write!(f, "->"),
            This => write!(f, "@"),
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
    /// that we want to advance by more than one character.
    fn advance_by(&mut self, times: usize) {
        for _ in 0..times {
            self.advance();
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
        if let Some(c) = self.advance() {
            return match c {
                '\n' => Ok(self.new_token(Newline, 1)),
                '\t' => Ok(self.new_token(Tab, 4)), // fix
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
                ';' => Ok(self.new_token(Semicolon, 1)),
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
                    .unwrap(),
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
                            self.advance_by(2);
                            Ok(self.new_token(CbRoot, 3))
                        })
                    })
                    .or_else(|| {
                        matches!(self.peek(), Some(&'>')).then(|| {
                            self.advance();
                            Ok(self.new_token(Binding, 2))
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
                                .ok_or(SectorError::ParseError("unexpected termination while lexing an operator starting with '<'. expected '>', found EoF", self.cursor.0, self.cursor.1))?
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
                    .unwrap(),
                'a'..='z' | 'A'..='Z' => Ok(self.lex_ident(c)),
                '0'..='9' => self.lex_numeric_literal(c),
                '\'' => self.lex_char_literal(),
                '"' => self.lex_string_literal(),
                _ => Err(Box::new(SectorError::ParseError("unexpected token", self.cursor.0, self.cursor.1))),
            };
        }

        // fix not reaching?
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

        if RESERVED_IDENTIFIERS.contains(&&*ident) {
            return match ident.as_ref() {
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
            return Err(Box::new(SectorError::ParseError("unexpected termination while lexing 'char' literal. expected codepoint, found closing quote", self.cursor.0, self.cursor.1)));
        } else if consumed.is_none() {
            return Err(Box::new(SectorError::ParseError(
                "unexpected termination while lexing 'char' literal. expected codepoint, found EoF",
                self.cursor.0,
                self.cursor.1,
            )));
        }

        if let Some(&c) = self.peek() {
            if c != '\'' {
                return Err(Box::new(SectorError::ParseError(
                    "'char' literal can only contain one codepoint",
                    self.cursor.0,
                    self.cursor.1,
                )));
            } else {
                self.advance();
            }
        } else {
            return Err(Box::new(SectorError::ParseError("unexpected termination while lexing 'char' literal. expected closing quote, found EoF", self.cursor.0, self.cursor.1)));
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
            return Err(Box::new(SectorError::ParseError("unexpected termination while lexing 'string' literal. expected closing quote, found EoF", self.cursor.0, self.cursor.1)));
        }

        self.advance();

        Ok(self.new_token(
            StringLiteral(string_literal.to_string()),
            string_literal.len() + 2,
        ))
    }

    fn lex_numeric_literal(&mut self, first: char) -> SectorResult<Token> {
        let mut integral = true;
        let mut value = Cow::from(String::from(first));

        if !self.at_end() {
            value
                .to_mut()
                .push_str(&self.consume_until(|c| !c.is_alphanumeric() && c != '.' && c != '-'));
        }

        if value.contains('.') && value.matches('.').count() > 1 {
            return Err(Box::new(SectorError::ParseError(
                "invalid numeric literal. multiple decimal points found",
                self.cursor.0,
                self.cursor.1,
            )));
        }

        if value.chars().any(|c| c == '.' || c == 'e' || c == 'E') {
            integral = false;
        }

        Ok(self.new_token(NumericLiteral(value.to_string(), integral), value.len()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_punctuation() -> SectorResult<()> {
        let source = r#"( ) [ ] { } . , ;"#;
        let mut lexer = Lexer::new(source);
        let lexed = lexer.lex()?;

        pretty_assert_eq!(lexed.len(), 18);
        pretty_assert_eq!(lexer.cursor, (1, 18));
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
                Token(Whitespace, (1, 16..=17)),
                Token(Semicolon, (1, 17..=18)),
                Token(EoF, (1, 18..=18)),
            ]
        );

        Ok(())
    }

    #[test]
    fn test_lex_operators() -> SectorResult<()> {
        let source = "-> == != @ ^ ~ | || & && 5! 20! > >= >> < <= << % / * ** - -5 -20 -/ --/ + +5 +20 => <0>";
        let mut lexer = Lexer::new(source);
        let lexed = lexer.lex()?;

        pretty_assert_eq!(lexed.len(), 70);
        pretty_assert_eq!(lexer.cursor, (1, 89));
        pretty_assert_eq!(
            lexed,
            vec![
                Token(Binding, (1, 1..=3)),
                Token(Whitespace, (1, 3..=4)),
                Token(EqTo, (1, 4..=6)),
                Token(Whitespace, (1, 6..=7)),
                Token(NotEqTo, (1, 7..=9)),
                Token(Whitespace, (1, 9..=10)),
                Token(This, (1, 10..=11)),
                Token(Whitespace, (1, 11..=12)),
                Token(BwXor, (1, 12..=13)),
                Token(Whitespace, (1, 13..=14)),
                Token(BwNot, (1, 14..=15)),
                Token(Whitespace, (1, 15..=16)),
                Token(BwOr, (1, 16..=17)),
                Token(Whitespace, (1, 17..=18)),
                Token(LogOr, (1, 18..=20)),
                Token(Whitespace, (1, 20..=21)),
                Token(BwAnd, (1, 21..=22)),
                Token(Whitespace, (1, 22..=23)),
                Token(LogAnd, (1, 23..=25)),
                Token(Whitespace, (1, 25..=26)),
                Token(NumericLiteral("5".to_owned(), true), (1, 26..=27)),
                Token(Fact, (1, 27..=28)),
                Token(Whitespace, (1, 28..=29)),
                Token(NumericLiteral("20".to_owned(), true), (1, 29..=31)),
                Token(Fact, (1, 31..=32)),
                Token(Whitespace, (1, 32..=33)),
                Token(Gt, (1, 33..=34)),
                Token(Whitespace, (1, 34..=35)),
                Token(GtEq, (1, 35..=37)),
                Token(Whitespace, (1, 37..=38)),
                Token(BwShiftR, (1, 38..=40)),
                Token(Whitespace, (1, 40..=41)),
                Token(Lt, (1, 41..=42)),
                Token(Whitespace, (1, 42..=43)),
                Token(LtEq, (1, 43..=45)),
                Token(Whitespace, (1, 45..=46)),
                Token(BwShiftL, (1, 46..=48)),
                Token(Whitespace, (1, 48..=49)),
                Token(Mod, (1, 49..=50)),
                Token(Whitespace, (1, 50..=51)),
                Token(Div, (1, 51..=52)),
                Token(Whitespace, (1, 52..=53)),
                Token(Asterisk, (1, 53..=54)),
                Token(Whitespace, (1, 54..=55)),
                Token(Pow, (1, 55..=57)),
                Token(Whitespace, (1, 57..=58)),
                Token(Sub, (1, 58..=59)),
                Token(Whitespace, (1, 59..=60)),
                Token(Neg, (1, 60..=61)),
                Token(NumericLiteral("5".to_owned(), true), (1, 61..=62)),
                Token(Whitespace, (1, 62..=63)),
                Token(Neg, (1, 63..=64)),
                Token(NumericLiteral("20".to_owned(), true), (1, 64..=66)),
                Token(Whitespace, (1, 66..=67)),
                Token(SqRoot, (1, 67..=69)),
                Token(Whitespace, (1, 69..=70)),
                Token(CbRoot, (1, 70..=73)),
                Token(Whitespace, (1, 73..=74)),
                Token(Add, (1, 74..=75)),
                Token(Whitespace, (1, 75..=76)),
                Token(Pos, (1, 76..=77)),
                Token(NumericLiteral("5".to_owned(), true), (1, 77..=78)),
                Token(Whitespace, (1, 78..=79)),
                Token(Pos, (1, 79..=80)),
                Token(NumericLiteral("20".to_owned(), true), (1, 80..=82)),
                Token(Whitespace, (1, 82..=83)),
                Token(In, (1, 83..=85)),
                Token(Whitespace, (1, 85..=86)),
                Token(Abs, (1, 86..=89)),
                Token(EoF, (1, 89..=89))
            ]
        );

        Ok(())
    }

    #[test]
    fn test_lex_keywords() -> SectorResult<()> {
        let source = "proc index on transaction exec return cursor drop from update where set match right left inner temp select insert into with bind within scale down up str int shortint tinyint bigint bool float bin hex scn guid arr obj char bit complex";
        let mut lexer = Lexer::new(source);
        let lexed = lexer.lex()?;

        pretty_assert_eq!(lexed.len(), 84);
        pretty_assert_eq!(lexer.cursor, (1, 235));
        pretty_assert_eq!(
            lexed,
            vec![
                Token(Keyword("proc".to_owned()), (1, 1..=5)),
                Token(Whitespace, (1, 5..=6)),
                Token(Keyword("index".to_owned()), (1, 6..=11)),
                Token(Whitespace, (1, 11..=12)),
                Token(Keyword("on".to_owned()), (1, 12..=14)),
                Token(Whitespace, (1, 14..=15)),
                Token(Keyword("transaction".to_owned()), (1, 15..=26)),
                Token(Whitespace, (1, 26..=27)),
                Token(Keyword("exec".to_owned()), (1, 27..=31)),
                Token(Whitespace, (1, 31..=32)),
                Token(Keyword("return".to_owned()), (1, 32..=38)),
                Token(Whitespace, (1, 38..=39)),
                Token(Keyword("cursor".to_owned()), (1, 39..=45)),
                Token(Whitespace, (1, 45..=46)),
                Token(Keyword("drop".to_owned()), (1, 46..=50)),
                Token(Whitespace, (1, 50..=51)),
                Token(Keyword("from".to_owned()), (1, 51..=55)),
                Token(Whitespace, (1, 55..=56)),
                Token(Keyword("update".to_owned()), (1, 56..=62)),
                Token(Whitespace, (1, 62..=63)),
                Token(Keyword("where".to_owned()), (1, 63..=68)),
                Token(Whitespace, (1, 68..=69)),
                Token(Keyword("set".to_owned()), (1, 69..=72)),
                Token(Whitespace, (1, 72..=73)),
                Token(Keyword("match".to_owned()), (1, 73..=78)),
                Token(Whitespace, (1, 78..=79)),
                Token(Keyword("right".to_owned()), (1, 79..=84)),
                Token(Whitespace, (1, 84..=85)),
                Token(Keyword("left".to_owned()), (1, 85..=89)),
                Token(Whitespace, (1, 89..=90)),
                Token(Keyword("inner".to_owned()), (1, 90..=95)),
                Token(Whitespace, (1, 95..=96)),
                Token(Keyword("temp".to_owned()), (1, 96..=100)),
                Token(Whitespace, (1, 100..=101)),
                Token(Keyword("select".to_owned()), (1, 101..=107)),
                Token(Whitespace, (1, 107..=108)),
                Token(Keyword("insert".to_owned()), (1, 108..=114)),
                Token(Whitespace, (1, 114..=115)),
                Token(Keyword("into".to_owned()), (1, 115..=119)),
                Token(Whitespace, (1, 119..=120)),
                Token(Keyword("with".to_owned()), (1, 120..=124)),
                Token(Whitespace, (1, 124..=125)),
                Token(Keyword("bind".to_owned()), (1, 125..=129)),
                Token(Whitespace, (1, 129..=130)),
                Token(Keyword("within".to_owned()), (1, 130..=136)),
                Token(Whitespace, (1, 136..=137)),
                Token(Keyword("scale".to_owned()), (1, 137..=142)),
                Token(Whitespace, (1, 142..=143)),
                Token(Keyword("down".to_owned()), (1, 143..=147)),
                Token(Whitespace, (1, 147..=148)),
                Token(Keyword("up".to_owned()), (1, 148..=150)),
                Token(Whitespace, (1, 150..=151)),
                Token(Keyword("str".to_owned()), (1, 151..=154)),
                Token(Whitespace, (1, 154..=155)),
                Token(Keyword("int".to_owned()), (1, 155..=158)),
                Token(Whitespace, (1, 158..=159)),
                Token(Keyword("shortint".to_owned()), (1, 159..=167)),
                Token(Whitespace, (1, 167..=168)),
                Token(Keyword("tinyint".to_owned()), (1, 168..=175)),
                Token(Whitespace, (1, 175..=176)),
                Token(Keyword("bigint".to_owned()), (1, 176..=182)),
                Token(Whitespace, (1, 182..=183)),
                Token(Keyword("bool".to_owned()), (1, 183..=187)),
                Token(Whitespace, (1, 187..=188)),
                Token(Keyword("float".to_owned()), (1, 188..=193)),
                Token(Whitespace, (1, 193..=194)),
                Token(Keyword("bin".to_owned()), (1, 194..=197)),
                Token(Whitespace, (1, 197..=198)),
                Token(Keyword("hex".to_owned()), (1, 198..=201)),
                Token(Whitespace, (1, 201..=202)),
                Token(Keyword("scn".to_owned()), (1, 202..=205)),
                Token(Whitespace, (1, 205..=206)),
                Token(Keyword("guid".to_owned()), (1, 206..=210)),
                Token(Whitespace, (1, 210..=211)),
                Token(Keyword("arr".to_owned()), (1, 211..=214)),
                Token(Whitespace, (1, 214..=215)),
                Token(Keyword("obj".to_owned()), (1, 215..=218)),
                Token(Whitespace, (1, 218..=219)),
                Token(Keyword("char".to_owned()), (1, 219..=223)),
                Token(Whitespace, (1, 223..=224)),
                Token(Keyword("bit".to_owned()), (1, 224..=227)),
                Token(Whitespace, (1, 227..=228)),
                Token(Keyword("complex".to_owned()), (1, 228..=235)),
                Token(EoF, (1, 235..=235))
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

    #[test]
    fn test_lex_literals() -> SectorResult<()> {
        let source = "'c' \"string\" 1 10 .1 0.1 0o10 0x10 0b10 10e5 10e-5 2.1e5 2.1e-5 true false";
        let mut lexer = Lexer::new(source);
        let lexed = lexer.lex()?;

        pretty_assert_eq!(lexed.len(), 30);
        pretty_assert_eq!(lexer.cursor, (1, 75));
        pretty_assert_eq!(
            lexed,
            vec![
                Token(CharacterLiteral('c'.to_owned()), (1, 1..=4)),
                Token(Whitespace, (1, 4..=5)),
                Token(StringLiteral("string".to_owned()), (1, 5..=13)),
                Token(Whitespace, (1, 13..=14)),
                Token(NumericLiteral("1".to_owned(), true), (1, 14..=15)),
                Token(Whitespace, (1, 15..=16)),
                Token(NumericLiteral("10".to_owned(), true), (1, 16..=18)),
                Token(Whitespace, (1, 18..=19)),
                Token(NumericLiteral(".1".to_owned(), false), (1, 19..=21)),
                Token(Whitespace, (1, 21..=22)),
                Token(NumericLiteral("0.1".to_owned(), false), (1, 22..=25)),
                Token(Whitespace, (1, 25..=26)),
                Token(NumericLiteral("0o10".to_owned(), true), (1, 26..=30)),
                Token(Whitespace, (1, 30..=31)),
                Token(NumericLiteral("0x10".to_owned(), true), (1, 31..=35)),
                Token(Whitespace, (1, 35..=36)),
                Token(NumericLiteral("0b10".to_owned(), true), (1, 36..=40)),
                Token(Whitespace, (1, 40..=41)),
                Token(NumericLiteral("10e5".to_owned(), false), (1, 41..=45)),
                Token(Whitespace, (1, 45..=46)),
                Token(NumericLiteral("10e-5".to_owned(), false), (1, 46..=51)),
                Token(Whitespace, (1, 51..=52)),
                Token(NumericLiteral("2.1e5".to_owned(), false), (1, 52..=57)),
                Token(Whitespace, (1, 57..=58)),
                Token(NumericLiteral("2.1e-5".to_owned(), false), (1, 58..=64)),
                Token(Whitespace, (1, 64..=65)),
                Token(BooleanLiteral(true), (1, 65..=69)),
                Token(Whitespace, (1, 69..=70)),
                Token(BooleanLiteral(false), (1, 70..=75)),
                Token(EoF, (1, 75..=75))
            ]
        );

        Ok(())
    }
}
