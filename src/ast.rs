use crate::{
    parser::ParserError,
    tokens::{OperatorKind, Span},
};
use std::{
    fmt::{Display, Formatter},
    ops::Deref,
};
use string_interner::symbol::SymbolU32;

#[derive(Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> Display for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}|{}:{}",
            self.span.line, self.span.col, self.span.start, self.span.end
        )
    }
}

pub type Stmt = Spanned<Statement>;
pub type Expr = Spanned<Expression>;
pub type Ident = Spanned<Identifier>;
pub type SpannedOp = Spanned<OperatorKind>;

#[derive(Debug)]
pub enum PlaceExpression {
    Identifier(Ident),
    Index { array: Expr, index: Expr },
    Field { object: Expr, field: Ident },
}

#[derive(Debug)]
pub enum Statement {
    Block(BlockStatement),
    Declaration(DeclarationStatement),
    Assignment(AssignmentStatement),
    While(WhileStatement),
    Expression(ExpressionStatement),
    // Import,
    Break,
    Continue,
    Return(ReturnStatement),
}

#[derive(Debug)]
pub struct BlockStatement {
    pub statements: Vec<Stmt>,
}

#[derive(Debug)]
pub struct DeclarationStatement {
    pub ty: Option<Ident>,
    pub name: Ident,
    pub value: Option<Expr>,
    pub constant: bool,
}

#[derive(Debug)]
pub struct AssignmentStatement {
    pub op: SpannedOp,
    pub target: Spanned<PlaceExpression>,
    pub value: Expr,
}

#[derive(Debug)]
pub struct WhileStatement {
    pub condition: Expr,
    pub body: Spanned<BlockStatement>,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expr: Expr,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub value: Option<Expr>,
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),

    BooleanLiteral(Literal<bool>),
    StringLiteral(Literal<SymbolU32>),
    IntegerLiteral(Literal<i64>),
    FloatLiteral(Literal<f64>),
    FunctionLiteral(FunctionLiteral),
    ArrayLiteral(ArrayLiteral),

    Prefix(PrefixExpression),
    Postfix(PostfixExpression),

    Binary(BinaryExpression),

    Call(CallExpression),
    // Match,
    If,
}

#[derive(Debug)]
pub struct Identifier {
    pub name: SymbolU32,
}

#[derive(Debug)]
pub struct Literal<T> {
    pub value: T,
}

impl From<bool> for Literal<bool> {
    fn from(value: bool) -> Self {
        Literal { value }
    }
}

#[derive(Debug)]
pub struct FunctionLiteral {
    pub parameters: Vec<Ident>,
    pub body: Spanned<BlockStatement>,
}

#[derive(Debug)]
pub struct ArrayLiteral {
    pub elements: Vec<Expr>,
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub op: SpannedOp,
    pub right: Box<Expr>,
}

#[derive(Debug)]
pub struct PostfixExpression {
    pub left: Box<Expr>,
    pub op: SpannedOp,
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub left: Box<Expr>,
    pub op: SpannedOp,
    pub right: Box<Expr>,
}

#[derive(Debug)]
pub struct CallExpression {
    pub function: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug)]
pub struct IfExpression {
    pub condition: Box<Expr>,
    pub consequence: BlockStatement,
    // TODO: Represent if-else chain later
    pub alternate: Option<BlockStatement>,
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Stmt>,
    pub errors: Vec<ParserError>,
}
