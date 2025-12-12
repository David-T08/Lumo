use crate::{
    ast::{self, BlockStatement, Expression, Identifier, Spanned, Statement},
    tokens::{KeywordKind, LiteralKind, OperatorKind, Span, SymbolKind, Token, TokenKind},
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

#[derive(Debug, Clone)]
pub enum ParserError {
    IncorrectToken {
        encountered: Option<Token>,
        expected: ExpectedToken
    },
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
    IdentifierNamed(crate::tokens::Sym)
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
    peek: Option<Token>
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
            println!("Peek succeeded for {} {}", kind, self.peek.as_ref().unwrap().path());
            
            self.advance();
            self.current.clone()
        } else {
            self.errors.push(ParserError::IncorrectToken {
                encountered: self.peek.clone(),
                expected: kind.into()
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
        self.peek.as_ref().is_some_and(|t|
            matches!(t.kind(), TokenKind::Literal { .. })
        )
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
    
    fn parse_block_statement(&mut self) -> Option<Spanned<BlockStatement>> {
        todo!();
    }

    fn parse_let_statement(&mut self) -> Option<Spanned<Statement>> {
        println!("Parsing let statement");
        let is_constant = self.peek_is(KeywordKind::Const);
        if is_constant {
            self.advance();
        }
        dbg!(is_constant);

        let ident = self.expect_peek_identifier()?;
        dbg!(&ident);

        self.expect_peek(OperatorKind::Assign)?;        
        self.advance();

        let expr = self.parse_expression(Precedence::Lowest)?;
        dbg!(&expr);

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
            Span::new(0, 0, 0, 0),
        ))
    }

    fn parse_return_statement(&mut self) -> Option<Spanned<Statement>> {
        self.advance();
        
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
        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(e) => e,
            None => return None
        };
        
        if self.peek_is(SymbolKind::Semicolon) {
            self.advance();
        }
        
        Some(Spanned::new(
            Statement::Expression(ast::ExpressionStatement {
                expr
            }),
            Span::new(0,0,0,0)
        ))
    }

    fn parse_statement(&mut self) -> Option<Spanned<Statement>> {
        todo!();
    }
    
    // Expressions
    fn parse_prefix(&mut self) -> Option<Spanned<Expression>> {
        let tok = self.current.as_ref()?;
        dbg!(tok);
        
        match tok.kind() {
            TokenKind::Identifier { .. } => self.parse_identifier(),
            TokenKind::Literal { kind } => todo!(), //self.parse_literal_expr(kind.clone())
            TokenKind::Keyword { kind } => match *kind {
                KeywordKind::True | KeywordKind::False => {
                    Some(Spanned::new(
                        Expression::BooleanLiteral(matches!(kind, KeywordKind::True).into()),
                        Span::new(0,0,0,0)
                    ))
                }
                
                KeywordKind::If => todo!(),
                KeywordKind::Function => todo!(),
                
                _ => None
            },
            TokenKind::Symbol { kind } => match *kind {
                SymbolKind::BraceOpen => todo!(),
                SymbolKind::BracketOpen => todo!(),
                SymbolKind::ParenOpen => todo!(),
                
                _ => None
            },
            TokenKind::Operator { kind } => match *kind {
                OperatorKind::Bang => todo!(),
                OperatorKind::Subtract => todo!(),
                
                _ => None
            },
            _ => None
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Spanned<Expression>> {
        let left = self.parse_prefix();
        
        println!("Starting to parse expression");
        while self.peek.as_ref().is_some_and(|t| !t.is_eof()) {
            if self.peek_is(SymbolKind::Semicolon) {break;}
            
            self.advance();
        }
        println!("Finished parsing expression");
        dbg!(&left);
        
        left
    }
    
    fn parse_identifier(&mut self) -> Option<Spanned<Expression>> {
        todo!();
    }
    
    


    pub fn parse_program(&mut self) -> ast::Program {
        let mut statements = Vec::new();

        while self.peek.is_some() {
            let tok = self.current.as_ref().unwrap();
            println!("Encountered token: {} {}", tok.name(), tok.path());

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
