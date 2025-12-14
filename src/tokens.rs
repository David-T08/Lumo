use crate::auto_display_enum;

use ordered_float::OrderedFloat;
use std::{
    fmt::{Display, Formatter},
    sync::{OnceLock, RwLock},
};
use string_interner::{DefaultBackend, DefaultSymbol, StringInterner};

static INTERNER: OnceLock<RwLock<StringInterner<DefaultBackend>>> = OnceLock::new();
pub fn interner() -> &'static RwLock<StringInterner<DefaultBackend>> {
    INTERNER.get_or_init(|| RwLock::new(StringInterner::default()))
}

pub type Sym = DefaultSymbol;

#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize, // Inclusive byte offset in source
    pub end: usize,   // Inclusive end byte offset

    pub line: u32, // Start line
    pub col: u32,  // Start column
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl Span {
    pub fn new(start: usize, end: usize, line: u32, col: u32) -> Self {
        Self {
            start,
            end,
            line,
            col,
        }
    }

    pub fn join(self, other: &Span) -> Span {
        let (start, line, col) =
            if (self.start, self.line, self.col) <= (other.start, other.line, other.col) {
                (self.start, self.line, self.col)
            } else {
                (other.start, other.line, other.col)
            };

        let end = self.end.max(other.end);
        Span {
            start,
            end,
            line,
            col,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Token {
    file: Sym,
    span: Span,

    kind: TokenKind,
}

#[allow(dead_code)]
impl Token {
    pub fn new(file: Sym, span: Span, kind: TokenKind) -> Self {
        Token { file, span, kind }
    }

    pub fn file(&self) -> Sym {
        self.file
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn name(&self) -> String {
        match &self.kind {
            TokenKind::Identifier { .. } => "IDENT".into(),
            TokenKind::Operator { kind } => format!("{kind}").into(),
            TokenKind::Keyword { kind } => format!("{kind}").into(),
            TokenKind::Symbol { kind } => format!("{kind}").into(),
            TokenKind::Literal { kind } => kind.name().into(),
            TokenKind::Unknown => "INVALID".into(),
            TokenKind::Eof => "EOF".into(),
        }
    }

    pub fn path(&self) -> String {
        let guard = interner().read().unwrap();

        format!(
            "({} @ {})",
            guard.resolve(self.file).unwrap_or(""),
            self.span
        )
    }
}

// Convenient is wrappers
#[allow(dead_code)]
impl Token {
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

    pub fn is_literal(&self, literal: &LiteralKind) -> bool {
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
impl Token {
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
impl Token {
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

    pub fn as_string(&self) -> Option<String> {
        match self.kind {
            TokenKind::Literal {
                kind: LiteralKind::String(s),
            } => {
                let guard = interner().read().unwrap();

                guard.resolve(s).map(|s| s.to_owned())
            }
            _ => None,
        }
    }

    pub fn as_interned_symbol(&self) -> Option<DefaultSymbol> {
        match self.kind {
            TokenKind::Literal {
                kind: LiteralKind::String(s),
            } => Some(s),

            TokenKind::Identifier { name } => Some(name),

            _ => None,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Identifier { name: Sym },
    Operator { kind: OperatorKind },
    Keyword { kind: KeywordKind },
    Literal { kind: LiteralKind },
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

        While => "while",
        Break => "break",
        Return => "return",

        Match => "match",
        Function => "function",

        Import => "import",
        Let => "let",
        Const => "const"
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
pub enum LiteralKind {
    Integer(i64),
    Float(OrderedFloat<f64>),
    String(Sym),
}

impl LiteralKind {
    pub fn name(&self) -> &str {
        match self {
            LiteralKind::Float { .. } => "FLOAT",
            LiteralKind::Integer { .. } => "INT",
            LiteralKind::String { .. } => "STR",
        }
        .into()
    }
}

impl Display for LiteralKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            LiteralKind::Integer(i) => write!(f, "{}", i),
            LiteralKind::Float(fl) => write!(f, "{}", fl),
            LiteralKind::String(s) => write!(f, "\"{}\"", {
                let guard = interner().read().unwrap();

                match guard.resolve(s) {
                    Some(s) => s.to_owned(),
                    None => "".into(),
                }
            }),
        }
    }
}

// Equality between a Token and OperatorKind
impl PartialEq<OperatorKind> for Token {
    fn eq(&self, other: &OperatorKind) -> bool {
        matches!(self.kind,
            TokenKind::Operator { kind } if kind == *other
        )
    }
}

impl PartialEq<Token> for OperatorKind {
    fn eq(&self, other: &Token) -> bool {
        other == self
    }
}

// Equality between a Token and KeywordKind
impl PartialEq<KeywordKind> for Token {
    fn eq(&self, other: &KeywordKind) -> bool {
        matches!(self.kind,
            TokenKind::Keyword { kind } if kind == *other
        )
    }
}

impl PartialEq<Token> for KeywordKind {
    fn eq(&self, other: &Token) -> bool {
        other == self
    }
}

// Equality between a Token and SymbolKind
impl PartialEq<SymbolKind> for Token {
    fn eq(&self, other: &SymbolKind) -> bool {
        matches!(self.kind,
            TokenKind::Symbol { kind } if kind == *other
        )
    }
}

impl PartialEq<Token> for SymbolKind {
    fn eq(&self, other: &Token) -> bool {
        other == self
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let guard = interner().read().unwrap();
        let path = format!(
            "({} @ {})",
            guard.resolve(self.file).unwrap_or(""),
            self.span
        );

        use TokenKind::*;
        match &self.kind {
            Identifier { name, .. } => {
                write!(
                    f,
                    "{} {} {}",
                    "[Identifier]",
                    guard.resolve(*name).unwrap_or(""),
                    path
                )
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
