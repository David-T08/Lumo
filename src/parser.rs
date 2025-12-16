use std::fmt::{Display, Formatter};
use tracing::{debug, error, info, instrument, trace, warn};

use crate::{
    ast::{self, BlockStatement, Expression, Spanned, Statement},
    tokens::{
        KeywordKind, LiteralKind, OperatorKind, Precedence, Span, SymbolKind, Token, TokenKind,
        interner,
    },
};

#[derive(Debug, Clone)]
pub enum ParserError {
    IncorrectToken {
        encountered: Option<Token>,
        expected: ExpectedToken,
    },
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::IncorrectToken {
                encountered,
                expected,
            } => {
                writeln!(
                    f,
                    "error: expected {}, got {} `{}`",
                    expected,
                    "a",
                    encountered
                        .as_ref()
                        .map(|t| t.name())
                        .unwrap_or("<none>".into())
                )?;

                write!(f, "hi")
            }
        }
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

pub struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    tokens: I,
    errors: Vec<ParserError>,

    current: Option<Token>,
    peek: Option<Token>,
}

// Peek methods
impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    // Generic
    fn peek_is<K>(&self, kind: K) -> bool
    where
        Token: PartialEq<K>,
        K: Copy + Clone,
    {
        self.peek.as_ref().is_some_and(|tok| *tok == kind)
    }

    fn expect_peek<K>(&mut self, kind: K) -> Option<Token>
    where
        Token: PartialEq<K> + Clone,
        K: Copy + Clone + std::fmt::Display + Into<ExpectedToken>,
    {
        if self.peek_is(kind) {
            info!(
                "Peek succeeded for {} {}",
                kind,
                self.peek.as_ref().unwrap().path()
            );

            self.advance();
            self.current.clone()
        } else {
            self.errors.push(ParserError::IncorrectToken {
                encountered: self.peek.clone(),
                expected: kind.into(),
            });
            None
        }
    }

    fn peek_is_identifier(&self) -> bool {
        self.peek.as_ref().is_some_and(|t| t.is_identifier())
    }

    fn expect_peek_identifier(&mut self) -> Option<Token> {
        if self.peek_is_identifier() {
            self.advance();
            self.current.clone()
        } else {
            self.errors.push(ParserError::IncorrectToken {
                encountered: self.peek.clone(),
                expected: ExpectedToken::Identifier,
            });
            None
        }
    }

    fn peek_is_literal_any(&self) -> bool {
        self.peek
            .as_ref()
            .is_some_and(|t| matches!(t.kind(), TokenKind::Literal { .. }))
    }

    fn expect_peek_literal_any(&mut self) -> Option<Token> {
        if self.peek_is_literal_any() {
            self.advance();
            self.current.clone()
        } else {
            self.errors.push(ParserError::IncorrectToken {
                encountered: self.peek.clone(),
                expected: ExpectedToken::LiteralAny,
            });
            None
        }
    }

    fn expect_peek_literal_int(&mut self) -> Option<Token> {
        if self.peek.as_ref().is_some_and(|t| t.is_integer()) {
            self.advance();
            self.current.clone()
        } else {
            self.errors.push(ParserError::IncorrectToken {
                encountered: self.peek.clone(),
                expected: ExpectedToken::LiteralInt,
            });
            None
        }
    }

    fn expect_peek_literal_float(&mut self) -> Option<Token> {
        if self.peek.as_ref().is_some_and(|t| t.is_float()) {
            self.advance();
            self.current.clone()
        } else {
            self.errors.push(ParserError::IncorrectToken {
                encountered: self.peek.clone(),
                expected: ExpectedToken::LiteralFloat,
            });
            None
        }
    }

    fn expect_peek_literal_string(&mut self) -> Option<Token> {
        if self.peek.as_ref().is_some_and(|t| t.is_string()) {
            self.advance();
            self.current.clone()
        } else {
            self.errors.push(ParserError::IncorrectToken {
                encountered: self.peek.clone(),
                expected: ExpectedToken::LiteralString,
            });
            None
        }
    }
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(mut tokens: I) -> Self {
        let current = tokens.next();
        let peek = tokens.next();

        Parser {
            tokens,
            errors: Vec::new(),
            current,
            peek,
        }
    }

    fn advance(&mut self) {
        self.current = self.peek.take();
        self.peek = self.tokens.next();
    }

    fn consume_until_statement_end(&mut self) {
        while let Some(tok) = self.peek.as_ref() {
            if *tok == SymbolKind::Semicolon || tok.is_eof() {
                break;
            }

            self.advance();
        }
    }

    #[instrument(skip(self))]
    fn parse_block_statement(&mut self) -> Option<Spanned<BlockStatement>> {
        todo!();
    }

    #[instrument(skip(self))]
    fn parse_let_statement(&mut self) -> Option<Spanned<Statement>> {
        let start_span = self.current.as_ref()?.span().clone();
        let is_constant = self.peek_is(KeywordKind::Const);
        if is_constant {
            self.advance();
        }

        debug!("is_constant = {is_constant}");

        let ident = self.expect_peek_identifier()?;
        debug!("ident = {:#?}", &ident);

        self.expect_peek(OperatorKind::Assign)?;
        self.advance();

        let expr = self.parse_expression(Precedence::Lowest)?;
        debug!("expr = {:#?}", &expr);

        let end_span = start_span.join(&expr.span());
        self.consume_until_statement_end();

        Some(Spanned::new(
            Statement::Declaration(ast::DeclarationStatement {
                name: Spanned::new(
                    ast::Identifier {
                        name: ident.as_interned_symbol().unwrap(),
                    },
                    ident.span().clone(),
                ),
                value: Some(expr),

                constant: is_constant,
                ty: None,
            }),
            end_span,
        ))
    }

    #[instrument(skip(self))]
    fn parse_return_statement(&mut self) -> Option<Spanned<Statement>> {
        let start_span = self.current.as_ref()?.span().clone();
        self.advance();

        let value = match self.parse_expression(Precedence::Lowest) {
            Some(v) => v,
            None => return None,
        };

        self.consume_until_statement_end();

        let end_span = start_span.join(value.span());
        Some(Spanned::new(
            Statement::Return(ast::ReturnStatement { value }),
            end_span,
        ))
    }

    #[instrument(skip(self))]
    fn parse_assignment_statement(&mut self) -> Option<Spanned<Statement>> {
        todo!();
    }

    #[instrument(skip(self))]
    fn parse_expression_statement(&mut self) -> Option<Spanned<Statement>> {
        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(e) => e,
            None => return None,
        };

        if self.peek_is(SymbolKind::Semicolon) {
            self.advance();
        }

        Some(Spanned::new(
            Statement::Expression(ast::ExpressionStatement { expr }),
            Span::new(0, 0, 0, 0),
        ))
    }

    #[instrument(skip(self))]
    fn parse_statement(&mut self) -> Option<Spanned<Statement>> {
        todo!();
    }

    // Expressions
    #[instrument(skip(self))]
    fn parse_prefix(&mut self) -> Option<Spanned<Expression>> {
        let tok = self.current.as_ref()?;

        match tok.kind() {
            TokenKind::Identifier { .. } => self.parse_identifier(),
            TokenKind::Literal { kind } => self.parse_literal(),
            TokenKind::Keyword { kind } => match *kind {
                KeywordKind::True | KeywordKind::False => Some(Spanned::new(
                    Expression::BooleanLiteral(matches!(kind, KeywordKind::True).into()),
                    tok.span().clone(),
                )),

                KeywordKind::If => todo!(),
                KeywordKind::Function => todo!(),

                _ => None,
            },
            TokenKind::Symbol { kind } => match *kind {
                SymbolKind::BraceOpen => todo!(),
                SymbolKind::BracketOpen => todo!(),
                SymbolKind::ParenOpen => todo!(),

                _ => None,
            },
            TokenKind::Operator { kind } => match *kind {
                OperatorKind::Bang => todo!(),
                OperatorKind::Subtract => todo!(),

                _ => None,
            },
            _ => None,
        }
    }

    #[instrument(skip(self))]
    fn parse_infix(&mut self, left: Spanned<Expression>) -> Option<Spanned<Expression>> {
        let start_span = left.span().clone();

        let (kind, op_span) = {
            let op_tok = self.current.as_ref()?;
            match op_tok.kind() {
                TokenKind::Operator { kind } => (*kind, op_tok.span().clone()),
                _ => return None,
            }
        };

        let prec = kind.precedence();
        let right = self.parse_expression(prec)?;

        let end_span = start_span.join(right.span());

        Some(Spanned::new(
            Expression::Binary(ast::BinaryExpression {
                left: Box::new(left),
                op: Spanned::new(kind.clone(), op_span),
                right: Box::new(right),
            }),
            end_span,
        ))
    }

    #[instrument(skip(self))]
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Spanned<Expression>> {
        debug!("Start parsing: {}", self.current.as_ref()?.name());
        let left = self.parse_prefix();

        while self.peek.as_ref().is_some_and(|t| !t.is_eof()) {
            if self.peek_is(SymbolKind::Semicolon) {
                break;
            }

            self.advance();
        }
        debug!("Finished parsing expression => {:#?}", &left);

        left
    }

    #[instrument(skip(self))]
    fn parse_literal(&mut self) -> Option<Spanned<Expression>> {
        let current = self.current.as_ref()?;
        let TokenKind::Literal { kind } = current.kind() else {
            return None;
        };

        let expr = match kind {
            LiteralKind::Integer(i) => Expression::IntegerLiteral(ast::Literal { value: *i }),
            LiteralKind::String(sym) => Expression::StringLiteral(ast::Literal { value: *sym }),
            LiteralKind::Float(_f) => todo!(),
        };

        Some(Spanned::new(expr, current.span().clone()))
    }

    #[instrument(skip(self))]
    fn parse_identifier(&mut self) -> Option<Spanned<Expression>> {
        let current = self.current.as_ref()?;

        let left = Spanned::new(
            Expression::Identifier(ast::Identifier {
                name: current.as_interned_symbol().unwrap(),
            }),
            current.span().clone(),
        );

        if self.peek_is(OperatorKind::Increment) || self.peek_is(OperatorKind::Decrement) {
            debug!("identifier has {}", self.peek.as_ref().unwrap().name());
            let start_span = current.span().clone();
            self.advance();

            let current = self.current.as_ref().unwrap();
            let (kind, op_span) = match current.kind() {
                TokenKind::Operator {
                    kind: OperatorKind::Increment,
                } => (OperatorKind::Increment, current.span().clone()),

                TokenKind::Operator {
                    kind: OperatorKind::Decrement,
                } => (OperatorKind::Decrement, current.span().clone()),

                _ => unreachable!("peek_is said ++/-- but current wasn't ++/--"),
            };

            Some(Spanned::new(
                Expression::Postfix(ast::PostfixExpression {
                    left: Box::new(left),
                    op: Spanned::new(kind, op_span.clone()),
                }),
                start_span.join(&op_span),
            ))
        } else {
            Some(left)
        }
    }

    #[instrument(skip(self))]
    pub fn parse_program(&mut self) -> ast::Program {
        let mut statements = Vec::new();

        while self.peek.is_some() {
            let tok = self.current.as_ref().unwrap();
            trace!("Encountered token: {} {}", tok.name(), tok.path());

            let statement = if *tok == KeywordKind::Let {
                self.parse_let_statement()
            } else if *tok == KeywordKind::Return {
                self.parse_return_statement()
            } else {
                if self.peek_is(OperatorKind::Assign) {
                    self.parse_assignment_statement()
                } else {
                    self.parse_expression_statement()
                }
            };

            if let Some(stmt) = statement {
                statements.push(stmt);
            }

            self.advance();
        }

        ast::Program {
            statements,
            errors: self.errors.clone(),
        }
    }
}
