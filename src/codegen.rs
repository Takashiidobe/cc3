use crate::ast::{BinaryOp, Expr, Program, Stmt, UnaryOp};

pub struct Codegen {
    buffer: String,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            buffer: String::new(),
        }
    }

    pub fn generate(mut self, program: &Program) -> String {
        self.emit_line("  .globl main");
        self.emit_line("main:");

        for stmt in &program.body {
            self.gen_stmt(stmt);
        }

        self.buffer
    }

    fn emit_line(&mut self, line: &str) {
        self.buffer.push_str(line);
        self.buffer.push('\n');
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Return(expr) => {
                self.gen_expr(expr);
                self.emit_line("  ret");
            }
        }
    }

    fn gen_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Num(value) => {
                self.emit_line(&format!("  mov ${}, %rax", value));
            }
            Expr::Unary { op, expr } => {
                self.gen_expr(expr);
                match op {
                    UnaryOp::Neg => {
                        self.emit_line("  neg %rax");
                    }
                }
            }
            Expr::Binary { op, lhs, rhs } => {
                self.gen_expr(rhs);
                self.emit_line("  push %rax");
                self.gen_expr(lhs);
                self.emit_line("  pop %rdi");
                match op {
                    BinaryOp::Add => {
                        self.emit_line("  add %rdi, %rax");
                    }
                    BinaryOp::Sub => {
                        self.emit_line("  sub %rdi, %rax");
                    }
                    BinaryOp::Mul => {
                        self.emit_line("  imul %rdi, %rax");
                    }
                    BinaryOp::Div => {
                        self.emit_line("  cqo");
                        self.emit_line("  idiv %rdi");
                    }
                }
            }
        }
    }
}
