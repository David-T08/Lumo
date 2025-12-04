use std::{ops::Deref};
use string_interner::symbol::SymbolU32;
use crate::tokens::{OperatorKind, Span};

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    value: T,
    span: Span,
}

impl<T> Spanned<T> {
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

pub type Stmt = Spanned<Statement>;
pub type Expr = Spanned<Expression>;
pub type Ident = Spanned<Identifier>;
pub type SpannedOp = Spanned<OperatorKind>;

pub enum PlaceExpression {
    Identifier(Ident),
    Index {
        array: Expr,
        index: Expr,
    },
    Field {
        object: Expr,
        field: Ident,
    },
}

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

pub struct BlockStatement {
    statements: Vec<Stmt>,
}

pub struct DeclarationStatement {
    ty: Ident,
    name: Ident,
    value: Option<Expr>,
    constant: bool,
}

pub struct AssignmentStatement {
    op: SpannedOp,
    target: Spanned<PlaceExpression>,
    value: Expr
}

pub struct WhileStatement {
    condition: Expr,
    body: Spanned<BlockStatement>
}

pub struct ExpressionStatement {
    expr: Expr
}

pub struct ReturnStatement {
    value: Expr
}


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

pub struct Identifier {
    name: SymbolU32,
}

pub struct Literal<T> {
    value: T,
}

pub struct FunctionLiteral {
    parameters: Vec<Ident>,
    body: Spanned<BlockStatement>,
}

pub struct ArrayLiteral {
    elements: Vec<Expr>,
}

pub struct PrefixExpression {
    op: SpannedOp,
    right: Box<Expr>,
}

pub struct PostfixExpression {
    left: Box<Expr>,
    op: SpannedOp
}

pub struct BinaryExpression {
    left: Box<Expr>,
    op: SpannedOp,
    right: Box<Expr>
}

pub struct CallExpression  {
    function: Box<Expr>,
    arguments: Vec<Expr>
}

pub struct IfExpression {
    condition: Box<Expr>,
    consequence: BlockStatement,
    // TODO: Represent if-else chain later
    alternate: Option<BlockStatement>
}

pub struct Program {
    pub statements: Vec<Stmt>,
}
