use std::fmt::{Display, Formatter};
use tracing::{debug, error, info, instrument, trace, warn};

use crate::{
    ast::{self, Expression, Spanned, Statement},
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
    InvalidPrefixFn {
        encountered: Token,
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

            ParserError::InvalidPrefixFn { encountered } => {
                writeln!(f, "Invalid Prefix function for {}", encountered.name())
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

    fn current_span(&self) -> Option<Span> {
        Some(self.current.as_ref()?.span().clone())
    }

    fn starts_statement(&self) -> bool {
        let current = self.current.as_ref().unwrap();

        *current == KeywordKind::Let
            || *current == KeywordKind::Return
            || (current.is_identifier() && self.peek_is_assignment())
    }

    fn peek_precedence(&self) -> Option<Precedence> {
        self.peek.as_ref()?.precedence()
    }

    fn peek_is_identifier(&self) -> bool {
        self.peek.as_ref().is_some_and(|t| t.is_identifier())
    }

    fn peek_is_assignment(&self) -> bool {
        self.peek
            .as_ref()
            .is_some_and(|t| t.as_operator().is_some_and(|o| o.is_assignment()))
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
            .is_some_and(|t| matches!(t.kind, TokenKind::Literal { .. }))
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
    fn parse_let_statement(&mut self) -> Option<Spanned<ast::DeclarationStatement>> {
        let start_span = self.current_span()?;
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
        let end_span = start_span.join(&expr.span());
        self.consume_until_statement_end();
        
        debug!("expr = {:#?}", &expr);

        Some(Spanned::new(
            ast::DeclarationStatement {
                name: Spanned::new(
                    ast::Identifier {
                        name: ident.as_interned_symbol().unwrap(),
                    },
                    ident.span().clone(),
                ),
                value: Some(expr),

                constant: is_constant,
                ty: None,
            },
            end_span,
        ))
    }

    #[instrument(skip(self))]
    fn parse_return_statement(&mut self) -> Option<Spanned<ast::ReturnStatement>> {
        let start_span = self.current_span()?;
        self.advance();

        let value = self.parse_expression(Precedence::Lowest);
        let end_span = if let Some(value) = &value {
            start_span.join(value.span())
        } else {
            start_span
        };

        Some(Spanned::new(ast::ReturnStatement { value }, end_span))
    }

    #[instrument(skip(self))]
    fn parse_assignment_statement(&mut self) -> Option<Spanned<ast::AssignmentStatement>> {
        let op = self.peek.as_ref().unwrap().as_operator().unwrap();
        info!("ENCOUNTERED A {}", self.peek.as_ref().unwrap().name());
        todo!();
    }

    #[instrument(skip(self))]
    fn parse_expression_statement(&mut self) -> Option<Spanned<ast::ExpressionStatement>> {
        let start_span = self.current_span()?;
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_is(SymbolKind::Semicolon) {
            self.advance();
        }

        let end_span = start_span.join(&expr.span);

        Some(Spanned::new(ast::ExpressionStatement { expr }, end_span))
    }

    #[instrument(skip(self))]
    fn parse_function_statement(&mut self) -> Option<Spanned<ast::FunctionStatement>> {
        let start_span = self.current_span()?;

        let name = self.expect_peek_identifier()?;
        self.expect_peek(SymbolKind::ParenOpen)?;

        let parameters = self.parse_identifier_list(SymbolKind::ParenClose)?;
        self.expect_peek(SymbolKind::BraceOpen)?;

        let body = self.parse_block_expression()?;
        let end_span = start_span.join(&self.current_span()?);

        Some(Spanned::new(
            ast::FunctionStatement {
                name: Spanned::new(
                    ast::Identifier {
                        name: name.as_interned_symbol().unwrap(),
                    },
                    name.span().clone(),
                ),

                parameters,
                body,
            },
            end_span,
        ))
    }

    #[instrument(skip(self))]
    fn parse_statement(&mut self) -> Option<Spanned<Statement>> {
        let tok = self.current.as_ref().unwrap();

        match tok.kind {
            TokenKind::Keyword {
                kind: KeywordKind::Let,
            } => self
                .parse_let_statement()
                .map(|s| s.map(Statement::Declaration)),
            TokenKind::Keyword {
                kind: KeywordKind::Return,
            } => self
                .parse_return_statement()
                .map(|s| s.map(Statement::Return)),
            TokenKind::Keyword {
                kind: KeywordKind::Function,
            } => self
                .parse_function_statement()
                .map(|s| s.map(Statement::Function)),
            _ if self.peek_is_assignment() => self
                .parse_assignment_statement()
                .map(|s| s.map(Statement::Assignment)),
            _ => self
                .parse_expression_statement()
                .map(|s| s.map(Statement::Expression)),
        }
    }

    // Expressions
    #[instrument(skip(self))]
    fn parse_prefix(&mut self) -> Option<Spanned<Expression>> {
        let tok = self.current.as_ref()?;

        match tok.kind {
            TokenKind::Identifier { .. } => self.parse_identifier(),
            TokenKind::Literal { .. } => self.parse_literal(),
            TokenKind::Keyword { kind } => match kind {
                KeywordKind::True | KeywordKind::False => Some(Spanned::new(
                    Expression::BooleanLiteral(matches!(kind, KeywordKind::True).into()),
                    tok.span().clone(),
                )),

                KeywordKind::If => todo!(),

                _ => None,
            },
            TokenKind::Symbol { kind } => match kind {
                SymbolKind::BraceOpen => self
                    .parse_block_expression()
                    .map(|e| e.map(Expression::Block)),
                SymbolKind::BracketOpen => todo!(),
                SymbolKind::ParenOpen => self.parse_grouped_expression(),

                _ => None,
            },
            TokenKind::Operator { kind } => match kind {
                OperatorKind::Bang
                | OperatorKind::Subtract
                | OperatorKind::Decrement
                | OperatorKind::Increment => self
                    .parse_prefix_op(kind, tok.span().clone())
                    .map(|e| e.map(Expression::Prefix)),

                _ => None,
            },
            _ => None,
        }
    }

    #[instrument(skip(self, left))]
    fn parse_infix(&mut self, left: Spanned<Expression>) -> Option<Spanned<Expression>> {
        debug!("operator = {}", self.current.as_ref().unwrap().name());

        let op = self.current.as_ref().unwrap();
        if *op == SymbolKind::ParenOpen {
            return self
                .parse_call_expression(left)
                .map(|e| e.map(Expression::Call));
        } else if *op == SymbolKind::BracketOpen {
            return self.parse_index_expression();
        }

        let (prec, op_kind, op_span) = match self.current.as_ref().unwrap().kind {
            TokenKind::Operator { kind } => (kind.precedence(), kind.clone(), op.span().clone()),
            _ => return Some(left),
        };
        self.advance();

        let right = self.parse_expression(prec)?;
        let end_span = left.span().join(right.span());

        Some(Spanned::new(
            Expression::Binary(ast::BinaryExpression {
                left: Box::new(left),
                op: Spanned::new(op_kind, op_span),
                right: Box::new(right),
            }),
            end_span,
        ))
    }

    #[instrument(skip(self))]
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Spanned<Expression>> {
        debug!("Start parsing: {}", self.current.as_ref()?.name());
        let mut left = match self.parse_prefix() {
            Some(expr) => expr,
            None => {
                self.errors.push(ParserError::InvalidPrefixFn {
                    encountered: self.current.as_ref()?.clone(),
                });
                return None;
            }
        };

        error!(
            "WE BACK!!! {:#?}, peek: {}",
            left,
            self.peek
                .as_ref()
                .map(|p| p.name())
                .unwrap_or("NONE".into())
        );

        loop {
            let peek = self.peek.as_ref();
            if peek.is_none() || peek.is_some_and(|p| p.is_eof() || *p == SymbolKind::Semicolon) {
                break;
            }

            let peek = peek.unwrap();
            let current = self.current.as_ref()?;

            info!(
                "current: ({}, {}) | peek: ({}, {})",
                current.name(),
                precedence,
                peek.name(),
                peek.precedence().unwrap_or(Precedence::Lowest)
            );
            info!(
                "peek_precedence reports: {}",
                self.peek_precedence().unwrap_or(Precedence::Lowest)
            );

            if precedence >= self.peek_precedence().unwrap_or(Precedence::Lowest) {
                break;
            }

            self.advance();
            left = self.parse_infix(left)?
        }
        debug!("Finished parsing expression => {:#?}", &left);

        Some(left)
    }

    #[instrument(skip(self))]
    fn parse_block_expression(&mut self) -> Option<Spanned<ast::BlockExpression>> {
        let start_span = self.current_span()?;
        self.advance();

        let mut statements = Vec::new();
        let mut tail = None;

        while *self.current.as_ref().unwrap() != SymbolKind::BraceClose {
            if self.starts_statement() {
                let stmt = self.parse_statement()?;
                statements.push(stmt);
                self.advance();

                continue;
            }

            let expr = self.parse_expression(Precedence::Lowest)?;
            self.advance();

            info!(
                "current: {}, peek: {}",
                self.current.as_ref().unwrap().name(),
                self.peek
                    .as_ref()
                    .map(|p| p.name())
                    .unwrap_or("NONE".into())
            );

            if let Some(s) = self.current.as_ref().unwrap().as_symbol() {
                match s {
                    SymbolKind::Semicolon => {
                        let span = expr.span().clone();
                        self.advance();

                        statements.push(Spanned::new(
                            Statement::Expression(ast::ExpressionStatement { expr }),
                            span,
                        ));
                    }

                    SymbolKind::BraceClose => {
                        tail = Some(Box::new(expr));
                        break;
                    }

                    _ => {
                        // error("expected `;` or `}` after expression");
                        return None;
                    }
                }
            }
        }

        let end_span = self.current_span()?;
        let span = start_span.join(&end_span);
        self.advance();

        Some(Spanned::new(ast::BlockExpression { statements, tail }, span))
    }

    fn parse_prefix_op(
        &mut self,
        op: OperatorKind,
        op_span: Span,
    ) -> Option<Spanned<ast::PrefixExpression>> {
        self.advance();

        let right = self.parse_expression(Precedence::Prefix)?;
        let end_span = op_span.join(&right.span);

        Some(Spanned::new(
            ast::PrefixExpression {
                op: Spanned::new(op, op_span),
                right: Box::new(right),
            },
            end_span,
        ))
    }

    #[instrument(skip(self, callee))]
    fn parse_call_expression(
        &mut self,
        callee: Spanned<Expression>,
    ) -> Option<Spanned<ast::CallExpression>> {
        let start_span = callee.span().clone();
        let arguments = self.parse_expression_list(SymbolKind::ParenClose)?;

        let end_span = start_span.join(
            self.current
                .as_ref()
                .map(|t| t.span())
                .unwrap_or(&start_span),
        );

        Some(Spanned::new(
            ast::CallExpression {
                callee: Box::new(callee),
                arguments,
            },
            end_span,
        ))
    }

    #[instrument(skip(self))]
    fn parse_index_expression(&mut self) -> Option<Spanned<Expression>> {
        todo!();
    }

    #[instrument(skip(self))]
    fn parse_grouped_expression(&mut self) -> Option<Spanned<Expression>> {
        self.advance();

        let expr = self.parse_expression(Precedence::Lowest);
        self.expect_peek(SymbolKind::ParenClose)?;

        expr
    }

    #[instrument(skip(self))]
    fn parse_expression_list(
        &mut self,
        end_symbol: SymbolKind,
    ) -> Option<Vec<Spanned<Expression>>> {
        let mut args: Vec<Spanned<Expression>> = Vec::new();
        if self.peek_is(end_symbol) {
            self.advance();

            return Some(args);
        };

        self.advance();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_is(SymbolKind::Comma) {
            self.advance(); // go to comma
            self.advance(); // go to start of expr

            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(end_symbol)?;
        Some(args)
    }

    fn parse_identifier_list(
        &mut self,
        end_symbol: SymbolKind,
    ) -> Option<Vec<Spanned<ast::Identifier>>> {
        let mut idents = Vec::new();

        if self.peek_is(end_symbol) {
            self.advance();
            return Some(idents);
        }

        loop {
            let ident = self.expect_peek_identifier()?;
            idents.push(Spanned::new(
                ast::Identifier {
                    name: ident.as_interned_symbol().unwrap(),
                },
                ident.span().clone(),
            ));

            if let Some(s) = self.peek.as_ref().unwrap().as_symbol() {
                match s {
                    SymbolKind::Comma => {
                        self.advance();
                    }

                    k if s == end_symbol => {
                        self.advance();
                        break;
                    }

                    _ => {
                        // error("expected `,` after identifier");
                        return None;
                    }
                }
            }
        }

        Some(idents)
    }

    #[instrument(skip(self))]
    fn parse_literal(&mut self) -> Option<Spanned<Expression>> {
        let current = self.current.as_ref()?;
        let TokenKind::Literal { kind } = &current.kind else {
            return None;
        };

        let expr = match *kind {
            LiteralKind::Integer(i) => Expression::IntegerLiteral(ast::Literal { value: i }),
            LiteralKind::String(sym) => Expression::StringLiteral(ast::Literal { value: sym }),
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
            let (kind, op_span) = (current.as_operator().unwrap(), current.span().clone());

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

        while let Some(_) = &self.peek {
            let tok = self.current.as_ref().unwrap();
            trace!("Encountered token: {} {}", tok.name(), tok.path());

            if let Some(stmt) = self.parse_statement() {
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
