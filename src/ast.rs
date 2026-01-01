use crate::error::SourceLocation;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub globals: Vec<Obj>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Member {
    pub name: String,
    pub ty: Type,
    pub offset: i32,
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
    StmtExpr(Vec<Stmt>),
    Assign {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Comma {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Member {
        lhs: Box<Expr>,
        member: Member,
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

    // Global variable
    pub init_data: Option<Vec<u8>>,

    // Function
    pub params: Vec<Obj>,
    pub body: Vec<Stmt>,
    pub locals: Vec<Obj>,
    pub stack_size: i32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Char,
    Int,
    Ptr(Box<Type>),
    #[allow(dead_code)]
    Func(Box<Type>),
    Struct {
        members: Vec<Member>,
    },
    Array {
        base: Box<Type>,
        len: i32,
    },
}

impl Type {
    pub fn is_integer(&self) -> bool {
        matches!(self, Type::Char | Type::Int)
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
            Type::Char => 1,
            Type::Int => 8,
            Type::Ptr(_) => 8,
            Type::Func(_) => 8,
            Type::Struct { members } => {
                if members.is_empty() {
                    return 0;
                }
                let align = self.align();
                let last_member = members.last().unwrap();
                let size = last_member.offset as i64 + last_member.ty.size();
                // Round up to alignment
                ((size + align - 1) / align) * align
            }
            Type::Array { base, len } => base.size() * (*len as i64),
        }
    }

    pub fn align(&self) -> i64 {
        match self {
            Type::Char => 1,
            Type::Int => 8,
            Type::Ptr(_) => 8,
            Type::Func(_) => 8,
            Type::Struct { members } => {
                if members.is_empty() {
                    return 1;
                }
                members.iter().map(|member| member.ty.align()).max().unwrap_or(1)
            }
            Type::Array { base, .. } => base.align(),
        }
    }
}
