#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Return(Expr),
    Expr(Expr),
    Decl(char),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i64),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Var(char),
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
