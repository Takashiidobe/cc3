use crate::error::SourceLocation;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub globals: Vec<Obj>,
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
    pub ty: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    Num(i64),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Call {
        name: String,
        args: Vec<Expr>,
    },
    Addr(Box<Expr>),
    Deref(Box<Expr>),
    Var {
        idx: usize,
        is_local: bool,
    },
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
    pub ty: Type,
    pub is_local: bool,

    // Local variable
    pub offset: i32,

    // Global variable or function
    pub is_function: bool,

    // Function
    pub params: Vec<Obj>,
    pub body: Vec<Stmt>,
    pub locals: Vec<Obj>,
    pub stack_size: i32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Ptr(Box<Type>),
    #[allow(dead_code)]
    Func(Box<Type>),
    Array { base: Box<Type>, len: i32 },
}

impl Type {
    pub fn is_integer(&self) -> bool {
        matches!(self, Type::Int)
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Type::Ptr(_))
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array { .. })
    }

    pub fn base(&self) -> Option<&Type> {
        match self {
            Type::Ptr(base) => Some(base),
            Type::Array { base, .. } => Some(base),
            _ => None,
        }
    }

    pub fn size(&self) -> i64 {
        match self {
            Type::Int => 8,
            Type::Ptr(_) => 8,
            Type::Func(_) => 8,
            Type::Array { base, len } => base.size() * (*len as i64),
        }
    }
}
