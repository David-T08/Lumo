use crate::{
    parser::ParserError,
    tokens::{OperatorKind, Span, interner},
};

use lumo_macros::AstFormatExt;

use std::{
    fmt::{Display, Formatter},
    ops::Deref,
};
use string_interner::symbol::SymbolU32;

#[derive(Debug, AstFormatExt)]
pub struct Spanned<T: AstFormat> {
    pub value: T,
    pub span: Span,
}

impl<T: AstFormat> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn map<U: AstFormat>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            value: f(self.value),
            span: self.span,
        }
    }
}

impl<T: AstFormat> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T: AstFormat> Display for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}|{}:{}",
            self.span.line, self.span.col, self.span.start, self.span.end
        )
    }
}

impl<T: AstFormat> AstFormat for Spanned<T> {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        if self.node_name().len() > 0 {
            write!(f, "[{}] ", self.node_name())?
        }

        if cfg.show_spans {
            write!(
                f,
                "[{}:{}|{}:{}] ",
                self.span.line, self.span.col, self.span.start, self.span.end
            )?;
        }

        self.value.fmt_with(f, cfg)
    }

    fn node_name(&self) -> &'static str {
        self.value.node_name()
    }
}

pub type Stmt = Spanned<Statement>;
pub type Expr = Spanned<Expression>;
pub type Ident = Spanned<Identifier>;
pub type SpannedOp = Spanned<OperatorKind>;

#[derive(Debug, AstFormatExt)]
pub enum PlaceExpression {
    Identifier(Ident),
    Index { array: Expr, index: Expr },
    Field { object: Expr, field: Ident },
}

impl AstFormat for PlaceExpression {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        todo!();
    }

    fn node_name(&self) -> &'static str {
        "Place Expression"
    }
}

#[derive(Debug, AstFormatExt)]
pub enum Statement {
    Declaration(DeclarationStatement),
    Assignment(AssignmentStatement),
    While(WhileStatement),
    Expression(ExpressionStatement),
    Function(FunctionStatement),
    Break,
    Continue,
    Return(ReturnStatement),
}

impl AstFormat for Statement {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        writeln!(f, "")?;

        let cfg = cfg.indent();
        cfg.fmt_padding(f)?;

        match self {
            Statement::Assignment(stmt) => stmt.fmt_with(f, cfg),
            Statement::Declaration(decl) => decl.fmt_with(f, cfg),
            Statement::Return(ret) => ret.fmt_with(f, cfg),
            Statement::While(stmt) => stmt.fmt_with(f, cfg),
            Statement::Expression(expr) => expr.fmt_with(f, cfg),
            Statement::Function(s) => s.fmt_with(f, cfg),

            _ => Ok(()),
        }
    }

    fn node_name(&self) -> &'static str {
        match self {
            Statement::Assignment(s) => s.node_name(),
            Statement::Break => "Break",
            Statement::Continue => "Continue",
            Statement::Declaration(s) => s.node_name(),
            Statement::Return(s) => s.node_name(),
            Statement::While(s) => s.node_name(),
            Statement::Expression(s) => s.node_name(),
            Statement::Function(s) => s.node_name(),
        }
    }
}

#[derive(Debug, AstFormatExt)]
pub struct DeclarationStatement {
    pub ty: Option<Ident>,
    pub name: Ident,
    pub value: Option<Expr>,
    pub constant: bool,
}

impl AstFormat for DeclarationStatement {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        write!(f, "[Constant]: {}", self.constant)?;
        writeln!(f, "")?;
        cfg.fmt_padding(f)?;
        
        self.name.fmt_with(f, cfg)?;
        self.value
            .as_ref()
            .map(|v| fmt_child(f, cfg, v))
            .unwrap_or_else(|| write!(f, "None"))
    }

    fn node_name(&self) -> &'static str {
        "Declaration"
    }
}

#[derive(Debug, AstFormatExt)]
pub struct AssignmentStatement {
    pub op: SpannedOp,
    pub target: Spanned<PlaceExpression>,
    pub value: Expr,
}

impl AstFormat for AssignmentStatement {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        todo!();
    }

    fn node_name(&self) -> &'static str {
        "Assignment"
    }
}

#[derive(Debug, AstFormatExt)]
pub struct WhileStatement {
    pub condition: Expr,
    pub body: Spanned<BlockExpression>,
}

impl AstFormat for WhileStatement {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        todo!();
    }

    fn node_name(&self) -> &'static str {
        "While"
    }
}

#[derive(Debug, AstFormatExt)]
pub struct ExpressionStatement {
    pub expr: Expr,
}

impl AstFormat for ExpressionStatement {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        self.expr.fmt_with(f, cfg)
    }

    fn node_name(&self) -> &'static str {
        "Expression Statement"
    }
}

#[derive(Debug, AstFormatExt)]
pub struct ReturnStatement {
    pub value: Option<Expr>,
}

impl AstFormat for ReturnStatement {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        self.value
            .as_ref()
            .map(|v| v.fmt_with(f, cfg))
            .unwrap_or_else(|| write!(f, "None"))
    }

    fn node_name(&self) -> &'static str {
        "Return"
    }
}

#[derive(Debug, AstFormatExt)]
pub struct FunctionStatement {
    pub name: Ident,
    pub parameters: Vec<Ident>,
    pub body: Spanned<BlockExpression>,
}

impl AstFormat for FunctionStatement {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        self.name.fmt_with(f, cfg)?;

        writeln!(f, "")?;
        cfg.fmt_padding(f)?;
        
        write!(f, "[Parameters]:")?;
        self.parameters.iter().for_each(|p| {
            let cfg = cfg.indent();
            fmt_child(f, cfg, p);
        });

        fmt_child(f, cfg, &self.body)
    }

    fn node_name(&self) -> &'static str {
        "Function"
    }
}

#[derive(Debug, AstFormatExt)]
pub enum Expression {
    Identifier(Identifier),

    BooleanLiteral(Literal<bool>),
    StringLiteral(Literal<SymbolU32>),
    IntegerLiteral(Literal<i64>),
    FloatLiteral(Literal<f64>),
    ArrayLiteral(ArrayLiteral),

    Prefix(PrefixExpression),
    Postfix(PostfixExpression),

    Binary(BinaryExpression),

    Block(BlockExpression),
    Call(CallExpression),
    // Match,
    If,
}

impl AstFormat for Expression {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        match self {
            Expression::Binary(b) => b.fmt_with(f, cfg),
            Expression::Identifier(e) => e.fmt_with(f, cfg),

            Expression::Prefix(e) => e.fmt_with(f, cfg),
            Expression::Postfix(e) => e.fmt_with(f, cfg),

            Expression::IntegerLiteral(e) => e.fmt_with(f, cfg),
            Expression::BooleanLiteral(e) => e.fmt_with(f, cfg),
            Expression::StringLiteral(e) => e.fmt_with(f, cfg),
            Expression::FloatLiteral(e) => e.fmt_with(f, cfg),

            Expression::Block(e) => e.fmt_with(f, cfg),

            Expression::Call(e) => e.fmt_with(f, cfg),

            _ => todo!(),
        }
    }

    fn node_name(&self) -> &'static str {
        match self {
            Expression::Binary(e) => e.node_name(),
            Expression::Call(e) => e.node_name(),
            Expression::Identifier(e) => e.node_name(),
            Expression::Postfix(e) => e.node_name(),
            Expression::Prefix(e) => e.node_name(),

            Expression::BooleanLiteral(e) => e.node_name(),
            Expression::IntegerLiteral(e) => e.node_name(),
            Expression::StringLiteral(e) => e.node_name(),
            Expression::FloatLiteral(e) => e.node_name(),
            Expression::ArrayLiteral(e) => e.node_name(),

            Expression::Block(e) => e.node_name(),

            _ => "<unknown expr>",
        }
    }
}

#[derive(Debug, AstFormatExt)]
pub struct Identifier {
    pub name: SymbolU32,
}

impl AstFormat for Identifier {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        let guard = interner().read().unwrap();

        write!(
            f,
            "{}",
            guard
                .resolve(self.name)
                .unwrap_or(&format!("<unknown symbol {:?}>", self.name))
        )
    }

    fn node_name(&self) -> &'static str {
        "Identifier"
    }
}

pub trait LiteralKind {
    const NODE_NAME: &'static str;
}

impl LiteralKind for f64 {
    const NODE_NAME: &'static str = "Float";
}
impl LiteralKind for bool {
    const NODE_NAME: &'static str = "Boolean";
}
impl LiteralKind for i64 {
    const NODE_NAME: &'static str = "Integer";
}
impl LiteralKind for SymbolU32 {
    const NODE_NAME: &'static str = "String";
}

#[derive(Debug, AstFormatExt)]
pub struct Literal<T: std::fmt::Debug + LiteralKind> {
    pub value: T,
}

impl<T: std::fmt::Debug + LiteralKind> AstFormat for Literal<T> {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        write!(f, "{:?}", self.value)
    }

    fn node_name(&self) -> &'static str {
        T::NODE_NAME
    }
}

impl From<bool> for Literal<bool> {
    fn from(value: bool) -> Self {
        Literal { value }
    }
}

#[derive(Debug, AstFormatExt)]
pub struct ArrayLiteral {
    pub elements: Vec<Expr>,
}

impl AstFormat for ArrayLiteral {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        todo!();
    }

    fn node_name(&self) -> &'static str {
        "Array"
    }
}

#[derive(Debug, AstFormatExt)]
pub struct PrefixExpression {
    pub op: SpannedOp,
    pub right: Box<Expr>,
}

impl AstFormat for PrefixExpression {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        let cfg = cfg.indent();

        fmt_child(f, cfg, &self.op)?;
        fmt_child(f, cfg, &self.right)
    }

    fn node_name(&self) -> &'static str {
        "Prefix"
    }
}

#[derive(Debug, AstFormatExt)]
pub struct PostfixExpression {
    pub left: Box<Expr>,
    pub op: SpannedOp,
}

impl AstFormat for PostfixExpression {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        let cfg = cfg.indent();

        fmt_child(f, cfg, &self.left)?;
        fmt_child(f, cfg, &self.op)
    }

    fn node_name(&self) -> &'static str {
        "Postfix"
    }
}

#[derive(Debug, AstFormatExt)]
pub struct BinaryExpression {
    pub left: Box<Expr>,
    pub op: SpannedOp,
    pub right: Box<Expr>,
}

impl AstFormat for BinaryExpression {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        let cfg = cfg.indent();

        if cfg.show_spans || cfg.newline_children {
            fmt_child(f, cfg, &self.left)?;
            fmt_child(f, cfg, &self.op)?;
            fmt_child(f, cfg, &self.right)?;
        } else {
            write!(
                f,
                "{} {} {}",
                self.left.with_cfg(cfg),
                self.op.with_cfg(cfg),
                self.right.with_cfg(cfg)
            )?
        }

        Ok(())
    }

    fn node_name(&self) -> &'static str {
        "Binary"
    }
}

#[derive(Debug, AstFormatExt)]
pub struct CallExpression {
    pub callee: Box<Expr>,
    pub arguments: Vec<Expr>,
}

impl AstFormat for CallExpression {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        let cfg = cfg.indent();
        fmt_child(f, cfg, &self.callee)?;

        writeln!(f, "")?;
        cfg.fmt_padding(f)?;

        write!(f, "[Arguments]:")?;
        let cfg = cfg.indent();

        for arg in &self.arguments {
            let _ = fmt_child(f, cfg, arg);
        }

        Ok(())
    }

    fn node_name(&self) -> &'static str {
        "Call"
    }
}

#[derive(Debug, AstFormatExt)]
pub struct BlockExpression {
    pub statements: Vec<Stmt>,
    pub tail: Option<Box<Expr>>,
}

impl AstFormat for BlockExpression {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        let cfg = cfg.indent();
        writeln!(f, "")?;
        cfg.fmt_padding(f)?;

        writeln!(f, "[Tail]:")?;
        cfg.indent().fmt_padding(f)?;

        self.tail
            .as_ref()
            .map(|v| v.fmt_with(f, cfg.indent()))
            .unwrap_or_else(|| write!(f, "None"))?;

        writeln!(f, "")?;
        cfg.fmt_padding(f)?;

        write!(f, "[Statements]:")?;
        let cfg = cfg.indent();

        for stmt in &self.statements {
            let _ = fmt_child(f, cfg, stmt);
        }

        Ok(())
    }

    fn node_name(&self) -> &'static str {
        "Block"
    }
}

#[derive(Debug, AstFormatExt)]
pub struct IfExpression {
    pub condition: Box<Expr>,
    pub consequence: BlockExpression,
    // TODO: Represent if-else chain later
    pub alternate: Option<BlockExpression>,
}

impl AstFormat for IfExpression {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        todo!();
    }

    fn node_name(&self) -> &'static str {
        "If"
    }
}

#[derive(Debug, AstFormatExt)]
pub struct Program {
    pub statements: Vec<Stmt>,
    pub errors: Vec<ParserError>,
}

impl AstFormat for Program {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        write!(f, "[Program]")?;

        let cfg = cfg.indent();
        for stmt in &self.statements {
            fmt_child(f, cfg, stmt)?;
        }

        Ok(())
    }

    fn node_name(&self) -> &'static str {
        "Program"
    }
}

// Formatting

fn fmt_child(
    f: &mut std::fmt::Formatter<'_>,
    cfg: AstFormatConfig,
    child: &impl AstFormat,
) -> std::fmt::Result {
    writeln!(f)?;
    cfg.fmt_padding(f)?;
    child.fmt_with(f, cfg)
}

#[derive(Clone, Copy, Debug)]
pub struct AstFormatConfig {
    // pub mode: PrintMode,
    pub show_spans: bool,
    pub draw_arrows: bool,
    pub newline_children: bool,
    pub indent: usize,
}

impl AstFormatConfig {
    pub fn indent(&self) -> AstFormatConfig {
        AstFormatConfig {
            indent: self.indent + 1,
            ..*self
        }
    }

    pub fn fmt_padding(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.indent {
            write!(f, "|   ")?
        }

        Ok(())
    }
}

impl Default for AstFormatConfig {
    fn default() -> Self {
        Self {
            show_spans: true,
            draw_arrows: true,
            newline_children: true,
            indent: 0,
        }
    }
}

pub trait AstFormat {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result;
    fn node_name(&self) -> &'static str;
}

pub struct WithCfg<'a, T> {
    pub node: &'a T,
    pub cfg: AstFormatConfig,
}

impl<'a, T: AstFormat> std::fmt::Display for WithCfg<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.node.fmt_with(f, self.cfg)
    }
}

pub trait AstFormatExt: AstFormat {
    fn with_cfg<'a>(&'a self, cfg: AstFormatConfig) -> WithCfg<'a, Self>
    where
        Self: Sized,
    {
        WithCfg { node: self, cfg }
    }
}

impl<T: AstFormat + ?Sized> AstFormat for Box<T> {
    fn fmt_with(&self, f: &mut std::fmt::Formatter<'_>, cfg: AstFormatConfig) -> std::fmt::Result {
        (**self).fmt_with(f, cfg)
    }

    fn node_name(&self) -> &'static str {
        (**self).node_name()
    }
}
