use crate::error::SourceLocation;
use std::sync::atomic::{AtomicUsize, Ordering};

static TYPE_ID: AtomicUsize = AtomicUsize::new(1);

fn next_type_id() -> usize {
    TYPE_ID.fetch_add(1, Ordering::Relaxed)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub globals: Vec<Obj>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Member {
    pub name: String,
    pub ty: Type,
    pub location: SourceLocation,
    pub idx: usize,
    pub align: i32,
    pub offset: i32,
    // Bitfield
    pub is_bitfield: bool,
    pub bit_offset: i32,
    pub bit_width: i32,
}

impl Member {
    /// Calculate the bit position after this member ends
    pub fn end_bit(&self) -> i64 {
        if self.is_bitfield {
            (self.offset as i64 * 8) + self.bit_offset as i64 + self.bit_width as i64
        } else {
            (self.offset as i64 + self.ty.size()) * 8
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub location: SourceLocation,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Return(Option<Expr>),
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
    DoWhile {
        body: Box<Stmt>,
        cond: Expr,
    },
    Switch {
        cond: Expr,
        body: Box<Stmt>,
        cases: Vec<SwitchCase>,
        default_label: Option<String>,
        break_label: String,
    },
    Case {
        value: Option<i64>,
        label: String,
        stmt: Box<Stmt>,
    },
    Goto {
        label: String,
    },
    Break,
    Continue,
    Label {
        label: String,
        stmt: Box<Stmt>,
    },
    Asm(String),
    Expr(Expr),
    Decl(usize),
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub location: SourceLocation,
    pub ty: Option<Type>,
}

#[derive(Default, Debug, Clone, PartialEq)]
pub enum ExprKind {
    #[default]
    Null,
    Num {
        value: i64,
        fval: f64,
    },
    /// Zero-clear a stack variable (for initializers)
    Memzero {
        idx: usize,
        is_local: bool,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        ret_buffer: Option<usize>,
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
    Cond {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Box<Expr>,
    },
    Comma {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Member {
        lhs: Box<Expr>,
        member: Member,
    },
    Cast {
        expr: Box<Expr>,
        ty: Type,
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
    Not,
    BitNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    LogAnd,
    LogOr,
    Eq,
    Ne,
    Lt,
    Le,
}

/// Global variable can be initialized either by a constant expression
/// or a pointer to another global variable. This struct represents the
/// latter.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Relocation {
    pub offset: usize,
    pub label: String,
    pub addend: i64,
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Obj {
    pub name: String,
    pub ty: Type,
    pub is_local: bool,
    pub align: i32,

    // Local variable
    pub offset: i32,

    // Global variable or function
    pub is_function: bool,
    pub is_definition: bool,
    pub is_static: bool,
    pub is_inline: bool,
    pub is_live: bool,
    pub is_root: bool,
    pub refs: Vec<String>,

    // Global variable
    pub is_tentative: bool,
    pub is_tls: bool,
    pub init_data: Option<Vec<u8>>,
    pub relocations: Vec<Relocation>,

    // Function
    pub params: Vec<Obj>,
    pub body: Vec<Stmt>,
    pub locals: Vec<Obj>,
    pub va_area: Option<usize>,
    pub alloca_bottom: Option<usize>,
    pub stack_size: i32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SwitchCase {
    pub value: i64,
    pub label: String,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum Type {
    #[default]
    Void,
    Bool,
    Char,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    Float,
    Double,
    Enum,
    Ptr(Box<Type>),
    Func {
        return_ty: Box<Type>,
        params: Vec<(String, Type)>,
        is_variadic: bool,
    },
    Struct {
        members: Vec<Member>,
        tag: Option<String>,
        is_incomplete: bool,
        is_flexible: bool,
        id: usize,
    },
    Union {
        members: Vec<Member>,
        tag: Option<String>,
        is_incomplete: bool,
        is_flexible: bool,
        id: usize,
    },
    Array {
        base: Box<Type>,
        len: i32,
    },
}

impl Type {
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Type::Bool
                | Type::Char
                | Type::UChar
                | Type::Short
                | Type::UShort
                | Type::Int
                | Type::UInt
                | Type::Long
                | Type::ULong
                | Type::Enum
        )
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
            Type::Void => 1,
            Type::Bool => 1,
            Type::Char => 1,
            Type::UChar => 1,
            Type::Short => 2,
            Type::UShort => 2,
            Type::Int => 4,
            Type::UInt => 4,
            Type::Long => 8,
            Type::ULong => 8,
            Type::Float => 4,
            Type::Double => 8,
            Type::Enum => 4,
            Type::Ptr(_) => 8,
            Type::Func { .. } => 1,
            Type::Struct {
                members,
                is_incomplete,
                ..
            } => {
                if *is_incomplete {
                    return -1;
                }
                if members.is_empty() {
                    return 0;
                }
                let align = self.align();
                // Calculate the highest bit used
                let bits = members.iter().map(|m| m.end_bit()).max().unwrap_or(0);
                // Round up to alignment
                ((bits + align * 8 - 1) / (align * 8)) * align
            }
            Type::Union {
                members,
                is_incomplete,
                ..
            } => {
                if *is_incomplete {
                    return -1;
                }
                if members.is_empty() {
                    return 0;
                }
                let align = self.align();
                let max_size = members.iter().map(|m| m.ty.size()).max().unwrap_or(0);
                // Round up to alignment
                ((max_size + align - 1) / align) * align
            }
            Type::Array { base, len } => {
                if *len < 0 {
                    return -1;
                }
                let base_size = base.size();
                if base_size < 0 {
                    return -1;
                }
                base_size * (*len as i64)
            }
        }
    }

    pub fn align(&self) -> i64 {
        match self {
            Type::Void => 1,
            Type::Bool => 1,
            Type::Char => 1,
            Type::UChar => 1,
            Type::Short => 2,
            Type::UShort => 2,
            Type::Int => 4,
            Type::UInt => 4,
            Type::Long => 8,
            Type::ULong => 8,
            Type::Float => 4,
            Type::Double => 8,
            Type::Enum => 4,
            Type::Ptr(_) => 8,
            Type::Func { .. } => 1,
            Type::Struct {
                members,
                is_incomplete,
                ..
            }
            | Type::Union {
                members,
                is_incomplete,
                ..
            } => {
                if *is_incomplete || members.is_empty() {
                    return 1;
                }
                members
                    .iter()
                    .map(|member| member.align as i64)
                    .max()
                    .unwrap_or(1)
            }
            Type::Array { base, .. } => base.align(),
        }
    }

    // Helper constructors to centralize creation and make future changes easier
    pub fn func(return_ty: Type, params: Vec<(String, Type)>, is_variadic: bool) -> Type {
        Type::Func {
            return_ty: Box::new(return_ty),
            params,
            is_variadic,
        }
    }

    pub fn array(base: Type, len: i32) -> Type {
        Type::Array {
            base: Box::new(base),
            len,
        }
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(self, Type::UChar | Type::UShort | Type::UInt | Type::ULong)
    }

    pub fn is_flonum(&self) -> bool {
        matches!(self, Type::Float | Type::Double)
    }

    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_flonum()
    }

    pub fn incomplete_struct(tag: Option<String>) -> Type {
        Type::Struct {
            members: Vec::new(),
            tag,
            is_incomplete: true,
            is_flexible: false,
            id: next_type_id(),
        }
    }

    pub fn complete_struct(members: Vec<Member>, tag: Option<String>, is_flexible: bool) -> Type {
        Type::Struct {
            members,
            tag,
            is_incomplete: false,
            is_flexible,
            id: next_type_id(),
        }
    }

    pub fn incomplete_union(tag: Option<String>) -> Type {
        Type::Union {
            members: Vec::new(),
            tag,
            is_incomplete: true,
            is_flexible: false,
            id: next_type_id(),
        }
    }

    pub fn complete_union(members: Vec<Member>, tag: Option<String>, is_flexible: bool) -> Type {
        Type::Union {
            members,
            tag,
            is_incomplete: false,
            is_flexible,
            id: next_type_id(),
        }
    }
}

pub fn is_compatible(t1: &Type, t2: &Type) -> bool {
    use Type::*;
    match (t1, t2) {
        (Void, Void)
        | (Bool, Bool)
        | (Char, Char)
        | (UChar, UChar)
        | (Short, Short)
        | (UShort, UShort)
        | (Int, Int)
        | (UInt, UInt)
        | (Long, Long)
        | (ULong, ULong)
        | (Float, Float)
        | (Double, Double)
        | (Enum, Enum) => true,
        (Ptr(base1), Ptr(base2)) => is_compatible(base1, base2),
        (
            Func {
                return_ty: ret1,
                params: params1,
                is_variadic: var1,
            },
            Func {
                return_ty: ret2,
                params: params2,
                is_variadic: var2,
            },
        ) => {
            if var1 != var2 || !is_compatible(ret1, ret2) || params1.len() != params2.len() {
                return false;
            }
            for ((_, ty1), (_, ty2)) in params1.iter().zip(params2.iter()) {
                if !is_compatible(ty1, ty2) {
                    return false;
                }
            }
            true
        }
        (
            Array {
                base: base1,
                len: len1,
            },
            Array {
                base: base2,
                len: len2,
            },
        ) => {
            if !is_compatible(base1, base2) {
                return false;
            }
            if *len1 < 0 || *len2 < 0 {
                return len1 == len2;
            }
            len1 == len2
        }
        (Struct { id: id1, .. }, Struct { id: id2, .. }) => id1 == id2,
        (Union { id: id1, .. }, Union { id: id2, .. }) => id1 == id2,
        _ => false,
    }
}
