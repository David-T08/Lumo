use crate::{KeywordKind, OperatorKind, Token, TokenKind};
use std::borrow::Cow;

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
    file: &'a str,
    stream: &'a Vec<u8>,

    // Human positions
    lineno: u32,
    columnno: u32,
    line_content: &'a str,

    // Actual file position
    position: usize,
    read_position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(file_path: &'a str, data: &'a Vec<u8>) -> Lexer<'a> {
        Lexer {
            file: file_path,
            stream: data,

            lineno: 1,
            columnno: 1,
            line_content: "",

            position: 0,
            read_position: 1,
        }
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        if self.read_position > self.stream.len() {
            return None;
        }

        self.skip_whitespace();

        let current_char = self.stream[self.position];

        // Strings
        if matches!(current_char, b'"' | b'\'') {
            return Some(Token::new(
                self.file,
                self.lineno,
                self.columnno,
                None,
                TokenKind::Literal {
                    value: self.read_string(),
                },
            ));
        } else if is_operator(current_char) {
            let lineno = self.lineno;
            let columnno = self.columnno;

            let (kind, len) = self.match_operator();

            self.position += len;
            self.read_position += len;

            return Some(Token::new(
                self.file,
                lineno,
                columnno,
                None,
                TokenKind::Operator { kind },
            ));
        } else if matches!(current_char, b'A'..=b'Z' | b'a'..=b'z') {
        }

        todo!();
    }

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

            (b'~', b'=') => (OperatorKind::BitNotAssign, 2),
            (b'~', _) => (OperatorKind::BitNot, 1),

            _ => (OperatorKind::Error, 1), // fallback or error
        }
    }

    fn skip_whitespace(&mut self) {
        while self.position < self.stream.len() {
            let current_char = self.stream[self.position] as char;

            if !current_char.is_whitespace() {
                break;
            }

            match current_char {
                '\n' => {
                    self.lineno += 1;
                    self.columnno = 1;

                    self.position = self.read_position;
                    self.read_position += 1;
                }
                '\r' => {
                    // Handle CRLF
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
                    let spaces_to_next_tab_stop = TAB_WIDTH - (self.columnno - 1 % TAB_WIDTH);
                    self.columnno += spaces_to_next_tab_stop;
                    self.position = self.read_position;
                    self.read_position += 1;
                }
                _ => {
                    self.columnno += 1;
                    self.position = self.read_position;
                    self.read_position += 1;
                }
            }
        }
    }

    fn next_char(&mut self) -> Option<char> {
        if self.read_position >= self.stream.len() {
            return None;
        }

        let c = self.stream[self.read_position] as char;
        self.position = self.read_position;
        self.read_position += 1;

        self.columnno += 1;

        Some(c)
    }

    fn peek_char(&self) -> Option<u8> {
        if self.read_position >= self.stream.len() {
            return None;
        }

        return Some(self.stream[self.read_position]);
    }

    fn peek_nth_char(&self, n: usize) -> Option<u8> {
        if self.position + n >= self.stream.len() {
            return None;
        }

        return Some(self.stream[self.position + n]);
    }

    fn read_string(&mut self) -> Cow<'a, str> {
        let starting_quote = self.stream[self.position];
        self.next_char(); // Skip the opening quote

        let str_start = self.position;
        let mut str_pos = self.position;
        let mut has_processed_escapes = false;

        let mut str_end = str_start;
        loop {
            // EOF
            if str_pos >= self.stream.len() {
                str_end = str_pos;
                break;
            }

            let char_byte = self.stream[str_pos];
            if char_byte == b'\\' {
                if str_pos + 1 < self.stream.len() {
                    let next_byte = self.stream[str_pos + 1];

                    if next_byte == b'"' || next_byte == b'\'' {
                        has_processed_escapes = true;
                    }

                    str_pos += 2; // Skip \ and escaped
                } else {
                    // EOF
                    str_pos += 1;
                    break;
                }
            } else if char_byte == starting_quote {
                str_end = str_pos;
                break;
            } else {
                str_pos += 1;
            }
        }

        if !has_processed_escapes {
            let final_lexer_pos =
                if str_end < self.stream.len() && self.stream[str_end] == starting_quote {
                    str_end + 1 // Position after closing quote
                } else {
                    str_end // EOF
                };

            let result = &self.stream[str_start..str_end];
            self.position = final_lexer_pos;
            self.read_position = final_lexer_pos + 1;

            match std::str::from_utf8(result) {
                Ok(s) => Cow::Borrowed(s),
                Err(_) => {
                    panic!("Malformed UTF-8");
                }
            }
        } else {
            let mut owned_string = String::new();
            let mut parse_cursor = str_start;

            while parse_cursor < str_end {
                let char_byte = self.stream[parse_cursor];
                if char_byte == b'\\' && parse_cursor + 1 < str_end {
                    let next_byte = self.stream[parse_cursor + 1];
                    if next_byte == b'"' || next_byte == b'\'' {
                        owned_string.push(next_byte as char);
                        parse_cursor += 2;
                        continue;
                    }

                    owned_string.push('\\');
                    parse_cursor += 1;
                } else {
                    owned_string.push(char_byte as char);
                    parse_cursor += 1;
                }
            }

            if str_end < self.stream.len() && self.stream[str_end] == starting_quote {
                self.position = str_end + 1;
            } else {
                self.position = str_end;
            }

            self.read_position = self.position + 1;

            Cow::Owned(owned_string)
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

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
        let mut l = super::Lexer::new("testing", &data);

        let token = l.next_token();

        assert!(
            token.is_some(),
            "Lexer failed to make a token: {}",
            input_str
        );

        let token = token.unwrap();
        match token.kind() {
            TokenKind::Literal { value } => {
                assert_eq!(
                    *value, expected_output,
                    "Mismatch for input: {}. Expected '{}', got '{}'",
                    input_str, expected_output, *value
                );
            }
            other => {
                panic!(
                    "Expected a Literal token for input: {}, got {:?}",
                    input_str, other
                );
            }
        }
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
        ("~=", OperatorKind::BitNotAssign),
    ];

    for (input_str, expected_output) in test_cases {
        let data: Vec<u8> = input_str.as_bytes().into();
        let mut l = super::Lexer::new("testing", &data);

        let token = l.next_token();

        assert!(
            token.is_some(),
            "Lexer failed to make a token: {}",
            input_str
        );
        
        assert!(token.unwrap() == expected_output);
    }
}
