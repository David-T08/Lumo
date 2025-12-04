use ordered_float::OrderedFloat;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

use crate::auto_display_enum;

#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize, // Inclusive byte offset in source
    pub end: usize,   // Inclusive end byte offset

    pub line: u32, // Start line
    pub col: u32,  // Start column
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Token<'a> {
    file: &'a str,
    span: Span,

    kind: TokenKind<'a>,
}

#[allow(dead_code)]
impl<'a> Token<'a> {
    pub fn new(file: &'a str, span: Span, kind: TokenKind<'a>) -> Self {
        Token { file, span, kind }
    }

    pub fn file(&self) -> &'a str {
        self.file
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn kind(&self) -> &TokenKind<'a> {
        &self.kind
    }
}

// Convenient is wrappers
#[allow(dead_code)]
impl<'a> Token<'a> {
    pub fn is_identifier(&self) -> bool {
        matches!(self.kind, TokenKind::Identifier { .. })
    }

    pub fn is_operator(&self, operator: OperatorKind) -> bool {
        matches!(self.kind, TokenKind::Operator { kind } if kind == operator)
    }

    pub fn is_symbol(&self, symbol: SymbolKind) -> bool {
        matches!(self.kind, TokenKind::Symbol { kind } if kind == symbol)
    }

    pub fn is_keyword(&self, keyword: KeywordKind) -> bool {
        matches!(self.kind, TokenKind::Keyword { kind } if kind == keyword)
    }

    pub fn is_literal(&self, literal: &LiteralKind<'a>) -> bool {
        matches!(&self.kind, TokenKind::Literal { kind } if kind == literal)
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self.kind, TokenKind::Unknown)
    }

    pub fn is_eof(&self) -> bool {
        matches!(self.kind, TokenKind::Eof)
    }
}

// Integerkind is wrappers
#[allow(dead_code)]
impl<'a> Token<'a> {
    pub fn is_integer(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::Literal {
                kind: LiteralKind::Integer(_)
            }
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::Literal {
                kind: LiteralKind::Float(_)
            }
        )
    }

    pub fn is_string(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::Literal {
                kind: LiteralKind::String(_)
            }
        )
    }
}

// LiteralKind as wrappers
#[allow(dead_code)]
impl<'a> Token<'a> {
    pub fn as_integer(&self) -> Option<i64> {
        match self.kind {
            TokenKind::Literal {
                kind: LiteralKind::Integer(i),
            } => Some(i),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        match self.kind {
            TokenKind::Literal {
                kind: LiteralKind::Float(f),
            } => Some(f.0),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&str> {
        match self.kind {
            TokenKind::Literal {
                kind: LiteralKind::String(ref s),
            } => Some(s.as_ref()),
            _ => None,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind<'a> {
    Identifier { name: &'a str },
    Operator { kind: OperatorKind },
    Keyword { kind: KeywordKind },
    Literal { kind: LiteralKind<'a> },
    Symbol { kind: SymbolKind },
    Unknown,
    Eof,
}

// OperatorKind
auto_display_enum! {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    #[allow(dead_code)]
    pub enum OperatorKind {
        // Math
        Add => "+",
        AddAssign => "+=",
        Subtract => "-",
        SubtractAssign => "-=",
        Multiply => "*",
        MultiplyAssign => "*=",
        Divide => "/",
        DivideAssign => "/=",
        Modulo => "%",
        ModuloAssign => "%=",
        Increment => "++",
        Decrement => "--",

        Bang => "!",
        Assign => "=",

        // Comparison
        Equal => "==",
        NotEqual => "!=",
        LessThan => "<",
        LessThanEqual => "<=",
        GreaterThan => ">",
        GreaterThanEqual => ">=",
        CompAnd => "&&",
        CompOr => "||",

        // Binary operators
        LeftShift => "<<",
        LeftShiftAssign => "<<=",
        RightShift => ">>",
        RightShiftAssign => ">>=",

        BitOr => "|",
        BitOrAssign => "|=",
        BitXor => "^",
        BitXorAssign => "^=",
        BitAnd => "&",
        BitAndAssign => "&=",
        BitNot => "~",

        Error => "\0"
    }
}

type O = OperatorKind;
impl OperatorKind {
    pub fn is_math(&self) -> bool {
        matches!(
            self,
            O::Add | O::Subtract | O::Multiply | O::Divide | O::Modulo
        )
    }

    pub fn is_assignment(&self) -> bool {
        matches!(
            self,
            O::AddAssign
                | O::SubtractAssign
                | O::MultiplyAssign
                | O::DivideAssign
                | O::ModuloAssign
                | O::LeftShiftAssign
                | O::RightShiftAssign
                | O::BitOrAssign
                | O::BitXorAssign
                | O::BitAndAssign
        )
    }

    pub fn is_binary(&self) -> bool {
        matches!(
            self,
            O::LeftShift | O::RightShift | O::BitOr | O::BitXor | O::BitAnd | O::BitNot
        )
    }

    pub fn is_binary_infix(&self) -> bool {
        self.is_comparison() || self.is_math() || (!matches!(self, O::BitNot) && self.is_binary())
    }

    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            O::Equal
                | O::NotEqual
                | O::LessThan
                | O::LessThanEqual
                | O::GreaterThan
                | O::GreaterThanEqual
                | O::CompAnd
                | O::CompOr
        )
    }

    pub fn is_prefix(&self) -> bool {
        matches!(
            self,
            O::Increment | O::Decrement | O::Subtract | O::Bang | O::BitNot
        )
    }

    pub fn is_postfix(&self) -> bool {
        matches!(self, O::Increment | O::Decrement)
    }
}

// KeywordKind
auto_display_enum! {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    #[allow(dead_code)]
    pub enum KeywordKind {
        True => "true",
        False => "false",

        If => "if",
        Else => "else",

        Break => "break",
        Return => "return",

        Match => "match",
        Function => "function",

        Import => "import",

        While => "while",
    }
}

// SymbolKind
auto_display_enum! {
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    #[allow(dead_code)]
    pub enum SymbolKind {
        Arrow => "->",
        FatArrow => "=>",
        DoubleColon => "::",
        Colon => ":",
        Semicolon => ";",
        Comma => ",",
        Dot => ".",
        ParenOpen => "(",
        ParenClose => ")",
        BraceOpen => "{",
        BraceClose => "}",
        BracketOpen => "[",
        BracketClose => "]",
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
#[allow(dead_code)]
pub enum LiteralKind<'a> {
    Integer(i64),
    Float(OrderedFloat<f64>),
    String(Cow<'a, str>),
}

impl<'a> Display for LiteralKind<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralKind::Integer(i) => write!(f, "{}", i),
            LiteralKind::Float(fl) => write!(f, "{}", fl),
            LiteralKind::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

// Equality between a Token and OperatorKind
impl PartialEq<OperatorKind> for Token<'_> {
    fn eq(&self, other: &OperatorKind) -> bool {
        matches!(self.kind,
            TokenKind::Operator { kind } if kind == *other
        )
    }
}

impl PartialEq<Token<'_>> for OperatorKind {
    fn eq(&self, other: &Token) -> bool {
        other == self
    }
}

// Equality between a Token and KeywordKind
impl PartialEq<KeywordKind> for Token<'_> {
    fn eq(&self, other: &KeywordKind) -> bool {
        matches!(self.kind,
            TokenKind::Keyword { kind } if kind == *other
        )
    }
}

impl PartialEq<Token<'_>> for KeywordKind {
    fn eq(&self, other: &Token) -> bool {
        other == self
    }
}

// Equality between a Token and SymbolKind
impl PartialEq<SymbolKind> for Token<'_> {
    fn eq(&self, other: &SymbolKind) -> bool {
        matches!(self.kind,
            TokenKind::Symbol { kind } if kind == *other
        )
    }
}

impl PartialEq<Token<'_>> for SymbolKind {
    fn eq(&self, other: &Token) -> bool {
        other == self
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let path = format!("({} @ {})", self.file, self.span);

        use TokenKind::*;
        match &self.kind {
            Identifier { name, .. } => {
                write!(f, "{} {} {}", "[Identifier]", name, path)
            }

            Keyword { kind } => {
                write!(f, "[Keyword]    {} {}", kind, path)
            }

            Symbol { kind } => {
                write!(f, "[Symbol]     {} {}", kind, path)
            }

            Literal { kind } => {
                write!(f, "[Literal]    {} {}", kind, path)
            }

            Operator { kind } => {
                write!(f, "[Operator]   {} {}", kind, path)
            }

            Unknown => {
                write!(f, "[Unknown]")
            }

            Eof => {
                write!(f, "[EOF]")
            }
        }
    }
}
