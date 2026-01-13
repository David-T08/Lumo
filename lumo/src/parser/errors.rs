use string_interner::DefaultSymbol;

use crate::tokens::{KeywordKind, LiteralKind, OperatorKind, Span, SymbolKind, Token, interner};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum ParserError {
    UnexpectedToken {
        encountered: Token,
        expected: ExpectedToken,
    },

    UnexpectedEof {
        expected: ExpectedToken,
        file: DefaultSymbol,
        span: Span,
    },

    MissingToken {
        expected: ExpectedToken,
        file: DefaultSymbol,
        span: Span,
    },

    UnclosedDelimiter {
        opener: Token,
        expected_closer: ExpectedToken,
    },
    
    InvalidElseBranch {
        encountered: Token
    },

    InvalidPrefixFn {
        encountered: Token,
    },

    InvalidInfixFn {
        encountered: Token,
    },
}

impl ParserError {
    pub fn span(&self) -> Span {
        match self {
            ParserError::UnexpectedToken { encountered, .. } => encountered.span,
            ParserError::InvalidPrefixFn { encountered } => encountered.span,
            ParserError::InvalidInfixFn { encountered } => encountered.span,
            ParserError::InvalidElseBranch { encountered, .. } => encountered.span,
            ParserError::UnclosedDelimiter { opener, .. } => opener.span,
            ParserError::UnexpectedEof { span, .. } => *span,
            ParserError::MissingToken { span, .. } => *span,
        }
    }

    pub fn source_file(&self) -> String {
        let guard = interner().read().unwrap();
        let sym = match self {
            ParserError::UnexpectedToken { encountered, .. } => encountered.file,
            ParserError::InvalidPrefixFn { encountered } => encountered.file,
            ParserError::InvalidInfixFn { encountered } => encountered.file,
            ParserError::InvalidElseBranch { encountered, .. } => encountered.file,
            ParserError::UnclosedDelimiter { opener, .. } => opener.file,
            ParserError::UnexpectedEof { file, .. } => *file,
            ParserError::MissingToken { file, .. } => *file,
        };

        guard.resolve(sym).unwrap().into()
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "\x1b[31merror\x1b[0m: ")?;

        match self {
            ParserError::UnexpectedToken {
                encountered,
                expected,
            } => {
                writeln!(f, "expected {}, got `{}`", expected, encountered.name())?;
            }

            ParserError::InvalidPrefixFn { encountered } => {
                writeln!(f, "Invalid Prefix function for {}", encountered.name())?;
            }

            ParserError::UnexpectedEof { expected, .. } => {
                writeln!(f, "Unexpected EOF, expected {}", expected)?;
            }
            
            ParserError::InvalidElseBranch { encountered } => {
                writeln!(f, "Expected another if expression or block after else, got {}", encountered.name())?
            }

            _ => write!(f, "[UNHANDLED]")?,
        }

        writeln!(
            f,
            "\x1b[34m   --> {}:{}\x1b[0m",
            self.source_file(),
            self.span()
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpectedToken {
    Keyword(KeywordKind),
    Symbol(SymbolKind),
    Operator(OperatorKind),

    LiteralExact(LiteralKind),
    LiteralAny,
    LiteralString,
    LiteralInt,
    LiteralFloat,

    Identifier,
    IdentifierNamed(crate::tokens::Sym),
}

impl Display for ExpectedToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectedToken::Keyword(k) => write!(f, "keyword `{}`", k),
            ExpectedToken::Symbol(s) => write!(f, "symbol `{}`", s),
            ExpectedToken::Operator(o) => write!(f, "operator `{}`", o),

            ExpectedToken::LiteralExact(l) => write!(f, "literal `{}`", l),

            ExpectedToken::LiteralAny => write!(f, "a literal"),
            ExpectedToken::LiteralInt => write!(f, "an integer literal"),
            ExpectedToken::LiteralFloat => write!(f, "a float literal"),
            ExpectedToken::LiteralString => write!(f, "a string literal"),

            ExpectedToken::Identifier => write!(f, "an identifier"),
            ExpectedToken::IdentifierNamed(sym) => {
                let guard = interner().read().unwrap();
                let name = guard.resolve(*sym).unwrap_or("<unknown>");
                write!(f, "identifier `{}`", name)
            }
        }
    }
}

impl From<KeywordKind> for ExpectedToken {
    fn from(k: KeywordKind) -> Self {
        ExpectedToken::Keyword(k)
    }
}

impl From<SymbolKind> for ExpectedToken {
    fn from(s: SymbolKind) -> Self {
        ExpectedToken::Symbol(s)
    }
}

impl From<OperatorKind> for ExpectedToken {
    fn from(o: OperatorKind) -> Self {
        ExpectedToken::Operator(o)
    }
}

impl From<LiteralKind> for ExpectedToken {
    fn from(o: LiteralKind) -> Self {
        ExpectedToken::LiteralExact(o)
    }
}
