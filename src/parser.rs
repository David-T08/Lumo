use crate::{
    ast::{self, Expression, Spanned, Statement},
    tokens::{KeywordKind, OperatorKind, Span, SymbolKind, Token},
};

enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

#[derive(Debug, Clone, Copy)]
pub enum ParserError {
    IncorrectToken,
}

pub struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    tokens: std::iter::Peekable<I>,
    errors: Vec<ParserError>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: I) -> Self {
        Parser {
            tokens: tokens.peekable(),
            errors: Vec::new(),
        }
    }

    fn peek_is<K>(&mut self, kind: K) -> bool
    where
        Token: PartialEq<K>,
        K: Copy,
    {
        self.tokens.peek().is_some_and(|tok| *tok == kind)
    }

    fn expect_peek<K>(&mut self, kind: K) -> Option<Token>
    where
        Token: PartialEq<K>,
        K: Copy + std::fmt::Display,
    {
        if self.peek_is(kind) {
            println!("Peek succeeded for {}", kind);
            self.tokens.next()
        } else {
            self.errors.push(ParserError::IncorrectToken);
            None
        }
    }

    fn consume_until_statement_end(&mut self) {
        while let Some(tok) = self.tokens.peek() {
            if *tok == SymbolKind::Semicolon || tok.is_eof() {
                break;
            }

            self.tokens.next();
        }
    }

    fn parse_let_statement(&mut self) -> Option<Spanned<Statement>> {
        println!("Parsing let statement");
        let is_constant = self.peek_is(KeywordKind::Const);
        if is_constant {
            self.tokens.next();
        }
        dbg!(is_constant);

        let ident = match self.tokens.peek() {
            Some(tok) if tok.is_identifier() => self.tokens.next().unwrap(),
            _ => {
                self.errors.push(ParserError::IncorrectToken);
                return None;
            }
        };

        dbg!(&ident);

        if self.expect_peek(OperatorKind::Assign).is_none() {
            return None;
        }

        let value = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };

        dbg!(&value);

        self.consume_until_statement_end();

        Some(Spanned::new(
            Statement::Declaration(ast::DeclarationStatement {
                name: Spanned::new(
                    ast::Identifier {
                        name: ident.as_interned_symbol().unwrap(),
                    },
                    ident.span().clone(),
                ),
                value: Some(value),
                
                constant: is_constant,
                ty: None,
            }),
            Span::new(0, 0, 0, 0),
        ))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Spanned<Expression>> {
        Some(Spanned::new(
            Expression::BooleanLiteral(false.into()),
            Span::new(0,0,0,0)
        ))
    }

    fn parse_return_statement(&mut self) -> Option<Spanned<Statement>> {
        self.tokens.next();
        
        let value = match self.parse_expression(Precedence::Lowest) {
            Some(v) => v,
            None => return None
        };
        
        self.consume_until_statement_end();
        
        Some(Spanned::new(
            Statement::Return(ast::ReturnStatement {
                value
            }),
            Span::new(0,0,0,0)
        ))
    }

    fn parse_assignment_statement(&mut self) -> Option<Spanned<Statement>> {
        todo!();
    }

    fn parse_expression_statement(&mut self) -> Option<Spanned<Statement>> {
        todo!();
    }

    fn parse_statement(&mut self) -> Option<Spanned<Statement>> {
        todo!();
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut statements = Vec::new();

        while self.tokens.peek().is_some() {
            let tok = self.tokens.next().unwrap();
            println!("Encountered token: {}", tok.name());

            let statement = if tok == KeywordKind::Let {
                self.parse_let_statement()
            } else if tok == KeywordKind::Return {
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
        }

        ast::Program {
            statements,
            errors: self.errors.clone(),
        }
    }
}
