use crate::error::SourceLocation;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub body: Vec<Stmt>,
    pub locals: Vec<Obj>,
    pub stack_size: i32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtKind {
    Return(Expr),
    Block(Vec<Stmt>),
    If {
        cond: Expr,
        then: Box<Stmt>,
        els: Option<Box<Stmt>>,
    },
    For {
        init: Option<Box<Stmt>>,
        cond: Option<Expr>,
        inc: Option<Expr>,
        body: Box<Stmt>,
    },
    Expr(Expr),
    Decl(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Num(i64),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Var(usize),
    Assign {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Obj {
    pub name: String,
    pub offset: i32,
}
