use std::fmt::{Display, Formatter};
use Lumo::auto_display_enum;

#[allow(dead_code)]
pub struct Token<'a> {
    file: &'a str,
    line_content: Option<&'a str>,

    lineno: u32,
    columnno: u32,

    token_kind: TokenKind<'a>,
}

impl<'a> Token<'a> {
    pub fn new(
        file: &'a str,
        lineno: u32,
        columnno: u32,
        line_content: Option<&'a str>,
        token_kind: TokenKind<'a>,
    ) -> Self {
        Token {
            file,
            line_content,

            lineno,
            columnno,

            token_kind,
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

    pub fn token_kind(&self) -> &TokenKind<'a> {
        &self.token_kind
    }
}

#[allow(dead_code)]
pub enum TokenKind<'a> {
    Identifier { name: &'a str, constant: bool },
    Keyword { kind: KeywordKind },
    Literal { value: &'a str },
    Operator { kind: OperatorKind },
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
    }
}

// KeywordKind
auto_display_enum! {
#[derive(Debug)]
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

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let path = format!("({} @ {}:{})", self.file, self.lineno, self.columnno);

        use TokenKind::*;
        match &self.token_kind {
            Identifier { name, constant, .. } => {
                write!(
                    f,
                    "{} {} {}",
                    if *constant { "Constant" } else { "Identifier" },
                    name,
                    path
                )
            }

            Keyword { kind } => {
                write!(f, "Keyword {} {}", kind, path)
            }

            Literal { value } => {
                write!(f, "Literal {} {}", value, path)
            }

            Operator { kind } => {
                write!(f, "Operator {} {}", kind, path)
            }

            Unknown => {
                write!(f, "Unknown")
            }

            Eof => {
                write!(f, "EOF")
            }
        }
    }
}
