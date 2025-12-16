use crate::tokens::{
    KeywordKind, LiteralKind, OperatorKind, Span, Sym, SymbolKind, Token, TokenKind, interner,
};
use std::borrow::Cow;
use tracing::{debug, instrument, trace, warn};

const TAB_WIDTH: u32 = 4;

fn is_operator(character: u8) -> bool {
    matches!(
        character,
        b'!' | b'#' |
        b'%' | b'&' |
        b'('..=b'/' |
        b':'..=b'>' |
        b'[' | b']' |
        b'^' | b'|' |
        b'{' | b'}' |
        b'~'
    )
}

#[allow(dead_code)]
pub struct Lexer<'a> {
    // Source file we're lexing
    file: Sym,
    stream: &'a Vec<u8>,

    finished: bool,

    // Human positions
    lineno: u32,
    columnno: u32,

    // Actual file position
    position: usize,
    read_position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(file_path: &'a str, data: &'a Vec<u8>) -> Lexer<'a> {
        let mut guard = interner().write().unwrap();
        Lexer {
            file: guard.get_or_intern(file_path),
            stream: data,

            finished: false,

            lineno: 1,
            columnno: 1,

            position: 0,
            read_position: 1,
        }
    }

    #[instrument(skip(self))]
    pub fn next_token(&mut self) -> Option<Token> {
        if self.finished {
            debug!("lexer finished");
            return None;
        }

        if self.position >= self.stream.len() {
            debug!("lexer reached eof");
            self.finished = true;

            return Some(Token::new(
                self.file,
                Span {
                    start: self.position,
                    end: self.position,

                    line: self.lineno,
                    col: self.columnno,
                },
                TokenKind::Eof,
            ));
        }

        self.skip_whitespace();
        let current_char = self.stream[self.position];

        let start_pos = self.position;
        let start_col = self.columnno;
        let start_line = self.lineno;

        let mut span = Span {
            start: start_pos,
            end: self.position,

            line: start_line,
            col: start_col,
        };

        if let Some(symbol) = self.match_symbol() {
            trace!("encountered symbol `{}`", symbol);
            let len = match symbol {
                SymbolKind::Arrow => 2,
                SymbolKind::FatArrow => 2,
                SymbolKind::DoubleColon => 2,

                _ => 1,
            };

            self.position += len;
            self.columnno += len as u32;
            self.read_position += len;

            trace!("advanced by {len}");
            span.end = self.position;

            return Some(Token::new(
                self.file,
                span,
                TokenKind::Symbol { kind: symbol },
            ));
        } else if matches!(current_char, b'"' | b'\'') {
            trace!("encountered a quote");
            let read = self.read_string();
            trace!("finished reading string");
            span.end = self.position;

            let mut guard = interner().write().unwrap();

            return Some(Token::new(
                self.file,
                span,
                TokenKind::Literal {
                    kind: LiteralKind::String(guard.get_or_intern(read)),
                },
            ));
        } else if is_operator(current_char) {
            // Operators
            let (kind, len) = self.match_operator();
            trace!("encountered operator `{kind}");

            self.position += len;
            self.columnno += len as u32;
            self.read_position += len;

            trace!("advanced by {len}");

            span.end = self.position;

            return Some(Token::new(self.file, span, TokenKind::Operator { kind }));
        } else if matches!(current_char, b'A'..=b'Z' | b'a'..=b'z' | b'_') {
            trace!("encountered alphabetical character (ident/keyword)");
            // Identifiers and keywords
            while self.read_position < self.stream.len()
                && matches!(self.stream[self.position], b'A'..=b'Z' | b'a'..=b'z' | b'_' | b'0'..=b'9')
            {
                self.next_char();
            }

            if self.read_position >= self.stream.len() {
                self.position += 1;
                self.read_position += 1;
            }

            let read = match std::str::from_utf8(&self.stream[start_pos..self.position]) {
                Ok(val) => val,
                Err(_) => {
                    panic!(
                        "Failed to lex at {}:{}, Malformed UTF-8",
                        start_col, start_col
                    );
                }
            };

            trace!("read => {read}");

            span.end = self.position;

            if let Some(kind) = self.match_keyword(read) {
                trace!("matched a keyword `{kind}");
                return Some(Token::new(self.file, span, TokenKind::Keyword { kind }));
            } else {
                let mut guard = interner().write().unwrap();

                return Some(Token::new(
                    self.file,
                    span,
                    TokenKind::Identifier {
                        name: guard.get_or_intern(read),
                    },
                ));
            }
        } else if matches!(current_char, b'0'..b'9') {
            trace!("encountered a numeric char");
            let mut radix = 10;
            if current_char == b'0'
                && let Some(peek) = self.peek_char()
            {
                trace!(
                    "has {} ({}) after",
                    peek,
                    match peek.to_ascii_lowercase() {
                        b'b' => "binary",
                        b'o' => "octal",
                        b'x' => "hex",
                        _ => "base-10",
                    }
                );

                radix = match peek.to_ascii_lowercase() {
                    b'b' => 2,
                    b'o' => 8,
                    b'x' => 16,
                    _ => 10,
                };

                // skip the character
                if radix != 10 {
                    self.next_char();
                }
            }

            while let Some(read) = self.next_char() {
                if read == b'_' {
                    continue;
                }

                let valid = match radix {
                    2 => matches!(read, b'0'..=b'1'),
                    8 => matches!(read, b'0'..=b'7'),
                    10 => matches!(read, b'0'..=b'9'),
                    16 => matches!(read, b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F'),
                    _ => false,
                };

                if !valid {
                    break;
                }
            }

            let read = match std::str::from_utf8(&self.stream[start_pos..self.position]) {
                Ok(val) => val,
                Err(_) => {
                    panic!(
                        "Failed to lex at {}:{}, Malformed UTF-8",
                        start_col, start_col
                    );
                }
            };

            trace!("read => {read}");

            span.end = self.position;
            if let Some(parsed) = self.parse_integer_literal(&read) {
                return Some(Token::new(
                    self.file,
                    span,
                    TokenKind::Literal {
                        kind: LiteralKind::Integer(parsed),
                    },
                ));
            } else {
                panic!()
            }
        }

        self.next_char();
        return Some(Token::new(self.file, span, TokenKind::Unknown));
    }

    #[instrument(skip(self))]
    fn match_operator(&self) -> (OperatorKind, usize) {
        let current_char = self.stream[self.position];
        let peek_char = self.peek_char().unwrap_or(0);

        match (current_char, peek_char) {
            (b'+', b'=') => (OperatorKind::AddAssign, 2),
            (b'+', b'+') => (OperatorKind::Increment, 2),
            (b'+', _) => (OperatorKind::Add, 1),

            (b'-', b'=') => (OperatorKind::SubtractAssign, 2),
            (b'-', b'-') => (OperatorKind::Decrement, 2),
            (b'-', _) => (OperatorKind::Subtract, 1),

            (b'*', b'=') => (OperatorKind::MultiplyAssign, 2),
            (b'*', _) => (OperatorKind::Multiply, 1),

            (b'/', b'=') => (OperatorKind::DivideAssign, 2),
            (b'/', _) => (OperatorKind::Divide, 1),

            (b'%', b'=') => (OperatorKind::ModuloAssign, 2),
            (b'%', _) => (OperatorKind::Modulo, 1),

            (b'=', b'=') => (OperatorKind::Equal, 2),
            (b'=', _) => (OperatorKind::Assign, 1),

            (b'!', b'=') => (OperatorKind::NotEqual, 2),
            (b'!', _) => (OperatorKind::Bang, 1),

            (b'<', b'<') if self.peek_nth_char(2) == Some(b'=') => {
                (OperatorKind::LeftShiftAssign, 3)
            }
            (b'<', b'<') => (OperatorKind::LeftShift, 2),
            (b'<', b'=') => (OperatorKind::LessThanEqual, 2),
            (b'<', _) => (OperatorKind::LessThan, 1),

            (b'>', b'>') if self.peek_nth_char(2) == Some(b'=') => {
                (OperatorKind::RightShiftAssign, 3)
            }
            (b'>', b'>') => (OperatorKind::RightShift, 2),
            (b'>', b'=') => (OperatorKind::GreaterThanEqual, 2),
            (b'>', _) => (OperatorKind::GreaterThan, 1),

            (b'&', b'&') => (OperatorKind::CompAnd, 2),
            (b'&', b'=') => (OperatorKind::BitAndAssign, 2),
            (b'&', _) => (OperatorKind::BitAnd, 1),

            (b'|', b'|') => (OperatorKind::CompOr, 2),
            (b'|', b'=') => (OperatorKind::BitOrAssign, 2),
            (b'|', _) => (OperatorKind::BitOr, 1),

            (b'^', b'=') => (OperatorKind::BitXorAssign, 2),
            (b'^', _) => (OperatorKind::BitXor, 1),

            (b'~', _) => (OperatorKind::BitNot, 1),

            _ => (OperatorKind::Error, 1), // fallback or error
        }
    }

    #[instrument(skip(self))]
    fn match_symbol(&self) -> Option<SymbolKind> {
        let current_char = self.stream[self.position];
        let peek_char = self.peek_char().unwrap_or(0);

        match (current_char, peek_char) {
            (b'-', b'>') => Some(SymbolKind::Arrow),
            (b'=', b'>') => Some(SymbolKind::FatArrow),
            (b':', b':') => Some(SymbolKind::DoubleColon),

            (b':', _) => Some(SymbolKind::Colon),
            (b';', _) => Some(SymbolKind::Semicolon),
            (b',', _) => Some(SymbolKind::Comma),
            (b'.', _) => Some(SymbolKind::Dot),
            (b'(', _) => Some(SymbolKind::ParenOpen),
            (b')', _) => Some(SymbolKind::ParenClose),
            (b'{', _) => Some(SymbolKind::BraceOpen),
            (b'}', _) => Some(SymbolKind::BraceClose),
            (b'[', _) => Some(SymbolKind::BracketOpen),
            (b']', _) => Some(SymbolKind::BracketClose),

            _ => None,
        }
    }

    #[instrument(skip(self))]
    fn match_keyword(&self, read: &str) -> Option<KeywordKind> {
        return match read {
            "true" => Some(KeywordKind::True),
            "false" => Some(KeywordKind::False),

            "if" => Some(KeywordKind::If),
            "else" => Some(KeywordKind::Else),

            "break" => Some(KeywordKind::Break),
            "return" => Some(KeywordKind::Return),

            "match" => Some(KeywordKind::Match),
            "function" => Some(KeywordKind::Function),

            "import" => Some(KeywordKind::Import),

            "while" => Some(KeywordKind::While),
            "let" => Some(KeywordKind::Let),
            "const" => Some(KeywordKind::Const),
            _ => None,
        };
    }

    #[instrument(skip(self))]
    fn skip_whitespace(&mut self) {
        while self.position < self.stream.len() {
            let current_char = self.stream[self.position] as char;

            if !current_char.is_whitespace() {
                break;
            }

            match current_char {
                '\n' => {
                    trace!("skipping newline");
                    self.lineno += 1;
                    self.columnno = 1;

                    self.position = self.read_position;
                    self.read_position += 1;
                }
                '\r' => {
                    // Handle CRLF
                    trace!("skipping \\r");
                    if self.peek_char() == Some(b'\n') {
                        self.position = self.read_position + 1;
                        self.read_position += 2;
                    } else {
                        self.position = self.read_position;
                        self.read_position += 1;
                    }
                    self.lineno += 1;
                    self.columnno = 1;
                }
                '\t' => {
                    trace!("skipping tab");
                    let spaces_to_next_tab_stop = TAB_WIDTH - (self.columnno - 1 % TAB_WIDTH);
                    self.columnno += spaces_to_next_tab_stop;
                    self.position = self.read_position;
                    self.read_position += 1;
                }
                _ => {
                    trace!("skipping generic whitespace");
                    self.columnno += 1;
                    self.position = self.read_position;
                    self.read_position += 1;
                }
            }
        }
    }

    #[instrument(skip(self))]
    fn parse_integer_literal(&self, s: &str) -> Option<i64> {
        let s = if s.contains('_') {
            Cow::Owned(s.replace('_', ""))
        } else {
            Cow::Borrowed(s)
        };

        if s.len() >= 2 {
            match &s[..2].to_ascii_lowercase()[..] {
                "0x" => i64::from_str_radix(&s[2..], 16).ok(),
                "0b" => i64::from_str_radix(&s[2..], 2).ok(),
                "0o" => i64::from_str_radix(&s[2..], 8).ok(),
                _ => s.parse::<i64>().ok(),
            }
        } else {
            s.parse::<i64>().ok()
        }
    }

    #[instrument(skip(self))]
    fn read_string(&mut self) -> Cow<'a, str> {
        let starting_quote = self.stream[self.position];
        trace!("starting_quote => {}", starting_quote as char);

        self.next_char();

        let str_start = self.position;
        let mut has_escapes = false;

        loop {
            // EOF
            if self.position >= self.stream.len() {
                break;
            }

            let char_byte = self.stream[self.position];

            if char_byte == b'\\' {
                trace!("preprocess: encountered escape");
                if self.position + 1 < self.stream.len() {
                    let next_byte = self.stream[self.position + 1];

                    if next_byte == b'"' || next_byte == b'\'' {
                        trace!("preprocess: is a {}", next_byte as char);
                        has_escapes = true;
                    }

                    self.next_char();
                    if self.position < self.stream.len() {
                        self.next_char();
                    }
                } else {
                    self.next_char();
                }
            } else if char_byte == starting_quote {
                trace!("preprocess: hit initial quote ({})", starting_quote as char);
                break;
            } else {
                self.next_char();
            }
        }

        let str_end = self.position;

        if self.position < self.stream.len() && self.stream[self.position] == starting_quote {
            self.next_char();
        }

        if !has_escapes {
            let result = &self.stream[str_start..str_end];
            match std::str::from_utf8(result) {
                Ok(s) => Cow::Borrowed(s),
                Err(_) => {
                    panic!("Malformed UTF-8");
                }
            }
        } else {
            let mut owned_string = String::new();
            let mut i = str_start;

            while i < str_end {
                let char_byte = self.stream[i];

                if char_byte == b'\\' && i + 1 < str_end {
                    let next_byte = self.stream[i + 1];
                    if next_byte == b'"' || next_byte == b'\'' {
                        owned_string.push(next_byte as char);
                        i += 2;
                        continue;
                    }
                }

                owned_string.push(char_byte as char);
                i += 1;
            }

            Cow::Owned(owned_string)
        }
    }
}

impl<'a> Lexer<'a> {
    #[instrument(skip(self))]
    fn next_char(&mut self) -> Option<u8> {
        self.position = self.read_position;
        self.read_position = self.position + 1;
        self.columnno += 1;

        if self.position >= self.stream.len() {
            None
        } else {
            Some(self.stream[self.position])
        }
    }

    #[instrument(skip(self))]
    fn peek_char(&self) -> Option<u8> {
        if self.read_position >= self.stream.len() {
            return None;
        }

        return Some(self.stream[self.read_position]);
    }

    #[instrument(skip(self))]
    fn peek_nth_char(&self, n: usize) -> Option<u8> {
        if self.position + n >= self.stream.len() {
            return None;
        }

        return Some(self.stream[self.position + n]);
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[test]
fn can_do_strings() {
    let test_cases = vec![
        ("\"Hello, World!\"", "Hello, World!"),
        ("\"hi\"", "hi"),
        (
            "\"Escaped \\\"quotes\\\" are cool\"",
            "Escaped \"quotes\" are cool",
        ),
        ("\"\"", ""),
        ("\"\\\"\\\"\"", "\"\""),
        ("\"NoEscapes\"", "NoEscapes"),
    ];

    for (input_str, expected_output) in test_cases {
        let data: Vec<u8> = input_str.as_bytes().into();
        let mut l = Lexer::new("testing", &data);

        let token = l.next_token();

        assert!(
            token.is_some(),
            "Lexer failed to make a token: {}",
            input_str
        );

        let token = token.unwrap();
        assert_eq!(
            token.as_string(),
            Some(expected_output.into()),
            "Mismatch for input: {}. Expected '{}', got '{:?}'",
            input_str,
            expected_output,
            token.as_string()
        );
    }
}

#[test]
fn can_do_operators() {
    let test_cases = vec![
        ("+", OperatorKind::Add),
        ("+=", OperatorKind::AddAssign),
        ("-", OperatorKind::Subtract),
        ("-=", OperatorKind::SubtractAssign),
        ("*", OperatorKind::Multiply),
        ("*=", OperatorKind::MultiplyAssign),
        ("/", OperatorKind::Divide),
        ("/=", OperatorKind::DivideAssign),
        ("%", OperatorKind::Modulo),
        ("%=", OperatorKind::ModuloAssign),
        ("++", OperatorKind::Increment),
        ("--", OperatorKind::Decrement),
        ("!", OperatorKind::Bang),
        ("=", OperatorKind::Assign),
        ("==", OperatorKind::Equal),
        ("!=", OperatorKind::NotEqual),
        ("<", OperatorKind::LessThan),
        ("<=", OperatorKind::LessThanEqual),
        (">", OperatorKind::GreaterThan),
        (">=", OperatorKind::GreaterThanEqual),
        ("&&", OperatorKind::CompAnd),
        ("||", OperatorKind::CompOr),
        ("<<", OperatorKind::LeftShift),
        ("<<=", OperatorKind::LeftShiftAssign),
        (">>", OperatorKind::RightShift),
        (">>=", OperatorKind::RightShiftAssign),
        ("|", OperatorKind::BitOr),
        ("|=", OperatorKind::BitOrAssign),
        ("^", OperatorKind::BitXor),
        ("^=", OperatorKind::BitXorAssign),
        ("&", OperatorKind::BitAnd),
        ("&=", OperatorKind::BitAndAssign),
        ("~", OperatorKind::BitNot),
    ];

    for (input_str, expected_output) in test_cases {
        let data: Vec<u8> = input_str.as_bytes().into();
        let mut l = Lexer::new("testing", &data);

        let token = l.next_token();

        assert!(
            token.is_some(),
            "Lexer failed to make a token: {}",
            input_str
        );

        assert!(token.unwrap() == expected_output);
    }
}

#[test]
fn can_do_keywords() {
    let test_cases = vec![
        ("true", KeywordKind::True),
        ("false", KeywordKind::False),
        ("if", KeywordKind::If),
        ("else", KeywordKind::Else),
        ("break", KeywordKind::Break),
        ("return", KeywordKind::Return),
        ("match", KeywordKind::Match),
        ("function", KeywordKind::Function),
        ("import", KeywordKind::Import),
        ("while", KeywordKind::While),
    ];

    for (input_str, expected_output) in test_cases {
        let data: Vec<u8> = input_str.as_bytes().into();
        let mut l = Lexer::new("testing", &data);

        let token = l.next_token();

        assert!(
            token.is_some(),
            "Lexer failed to make a token: {}",
            input_str
        );

        assert!(token.unwrap() == expected_output);
    }
}

#[test]
fn can_do_integers() {
    let test_cases = vec![
        // Hexadecimal
        ("0x00", 0),
        ("0x1F", 31),
        ("0xdeadbeef", 0xdeadbeef),
        ("0xDE_AD_BE_EF", 0xDEADBEEF),
        ("0XFF", 255),
        // Binary
        ("0b0", 0),
        ("0b1010", 10),
        ("0b1111_0000", 0b11110000),
        ("0B0101", 5),
        // Octal
        ("0o0", 0),
        ("0o77", 63),
        ("0o123", 83),
        ("0O7_7", 63),
        // Decimal
        ("0", 0),
        ("1", 1),
        ("10", 10),
        ("123456", 123_456),
        ("1_000_000", 1_000_000),
    ];

    for (input_str, expected_output) in test_cases {
        let data: Vec<u8> = input_str.as_bytes().into();
        let mut l = Lexer::new("testing", &data);

        let token = l.next_token();

        assert!(
            token.is_some(),
            "Lexer failed to make a token: {}",
            input_str
        );

        let token = token.unwrap();
        assert_eq!(
            token.as_integer(),
            Some(expected_output),
            "Mismatch for input: {}. Expected '{}', got '{:?}'",
            input_str,
            expected_output,
            token.as_integer()
        );
    }
}

#[test]
fn can_do_symbols() {
    let test_cases = vec![
        ("->", SymbolKind::Arrow),
        ("=>", SymbolKind::FatArrow),
        ("::", SymbolKind::DoubleColon),
        (":", SymbolKind::Colon),
        (";", SymbolKind::Semicolon),
        (",", SymbolKind::Comma),
        (".", SymbolKind::Dot),
        ("(", SymbolKind::ParenOpen),
        (")", SymbolKind::ParenClose),
        ("{", SymbolKind::BraceOpen),
        ("}", SymbolKind::BraceClose),
        ("[", SymbolKind::BracketOpen),
        ("]", SymbolKind::BracketClose),
    ];

    for (input_str, expected_output) in test_cases {
        let data: Vec<u8> = input_str.as_bytes().into();
        let mut l = Lexer::new("testing", &data);

        let token = l.next_token();

        assert!(
            token.is_some(),
            "Lexer failed to make a token: {}",
            input_str
        );

        trace!("{:?}", &token);

        assert!(token.unwrap() == expected_output);
    }
}
