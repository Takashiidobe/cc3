#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Return(Expr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Num(i64),
}
