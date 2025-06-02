use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use Lumo::auto_display_enum;

#[allow(dead_code)]
pub struct Token<'a> {
    file: &'a str,
    line_content: Option<&'a str>,

    lineno: u32,
    columnno: u32,

    kind: TokenKind<'a>,
}

impl<'a> Token<'a> {
    pub fn new(
        file: &'a str,
        lineno: u32,
        columnno: u32,
        line_content: Option<&'a str>,
        kind: TokenKind<'a>,
    ) -> Self {
        Token {
            file,
            line_content,

            lineno,
            columnno,

            kind,
        }
    }

    pub fn file(&self) -> &'a str {
        self.file
    }

    pub fn line_content(&self) -> Option<&'a str> {
        self.line_content
    }

    pub fn lineno(&self) -> u32 {
        self.lineno
    }

    pub fn columnno(&self) -> u32 {
        self.columnno
    }

    pub fn kind(&self) -> &TokenKind<'a> {
        &self.kind
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self.kind, TokenKind::Unknown)
    }

    pub fn is_eof(&self) -> bool {
        matches!(self.kind, TokenKind::Eof)
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self.kind, TokenKind::Identifier { .. })
    }

    pub fn is_literal(&self) -> bool {
        matches!(self.kind, TokenKind::Literal { .. })
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind<'a> {
    Identifier { name: &'a str, constant: bool },
    Operator { kind: OperatorKind },
    Keyword { kind: KeywordKind },
    Literal { value: Cow<'a, str> },
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
        BitNotAssign => "~=",

        Error => "\0"
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

        Set => "set"
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

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let path = format!("({} @ {}:{})", self.file, self.lineno, self.columnno);

        use TokenKind::*;
        match &self.kind {
            Identifier { name, constant, .. } => {
                write!(
                    f,
                    "{} {} {}",
                    if *constant {
                        "[Constant]"
                    } else {
                        "[Identifier]"
                    },
                    name,
                    path
                )
            }

            Keyword { kind } => {
                write!(f, "[Keyword] {} {}", kind, path)
            }

            Literal { value } => {
                write!(f, "[Literal] {} {}", value, path)
            }

            Operator { kind } => {
                write!(f, "[Operator] {} {}", kind, path)
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