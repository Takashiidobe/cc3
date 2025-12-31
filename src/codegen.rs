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
        self.emit_line("  push %rbp");
        self.emit_line("  mov %rsp, %rbp");
        self.emit_line("  sub $208, %rsp");

        let mut last_was_return = false;
        for stmt in &program.body {
            last_was_return = matches!(stmt, Stmt::Return(_));
            self.gen_stmt(stmt);
        }

        if !last_was_return {
            self.emit_line("  mov $0, %rax");
            self.emit_epilogue();
            self.emit_line("  ret");
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
                self.emit_epilogue();
                self.emit_line("  ret");
            }
            Stmt::Expr(expr) => {
                self.gen_expr(expr);
            }
            Stmt::Decl(_) => {}
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
            Expr::Var(name) => {
                self.gen_addr(*name);
                self.emit_line("  mov (%rax), %rax");
            }
            Expr::Assign { lhs, rhs } => {
                self.gen_lvalue(lhs);
                self.emit_line("  push %rax");
                self.gen_expr(rhs);
                self.emit_line("  pop %rdi");
                self.emit_line("  mov %rax, (%rdi)");
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
                    BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le => {
                        self.emit_line("  cmp %rdi, %rax");
                        match op {
                            BinaryOp::Eq => self.emit_line("  sete %al"),
                            BinaryOp::Ne => self.emit_line("  setne %al"),
                            BinaryOp::Lt => self.emit_line("  setl %al"),
                            BinaryOp::Le => self.emit_line("  setle %al"),
                            _ => {}
                        }
                        self.emit_line("  movzb %al, %rax");
                    }
                }
            }
        }
    }

    fn emit_epilogue(&mut self) {
        self.emit_line("  mov %rbp, %rsp");
        self.emit_line("  pop %rbp");
    }

    fn gen_addr(&mut self, name: char) {
        let offset = (name as i32 - 'a' as i32 + 1) * 8;
        self.emit_line(&format!("  lea -{}(%rbp), %rax", offset));
    }

    fn gen_lvalue(&mut self, expr: &Expr) {
        match expr {
            Expr::Var(name) => self.gen_addr(*name),
            _ => self.emit_line("  mov $0, %rax"),
        }
    }
}
