use crate::ast::{BinaryOp, Expr, ExprKind, Program, Stmt, StmtKind, UnaryOp};

pub struct Codegen {
    buffer: String,
    label_counter: usize,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            buffer: String::new(),
            label_counter: 0,
        }
    }

    pub fn generate(mut self, program: &Program) -> String {
        self.emit_line("  .globl main");
        self.emit_line("main:");
        self.emit_line("  push %rbp");
        self.emit_line("  mov %rsp, %rbp");
        self.emit_line(&format!("  sub ${}, %rsp", program.stack_size));

        let mut last_was_return = false;
        for stmt in &program.body {
            last_was_return = matches!(stmt.kind, StmtKind::Return(_));
            self.gen_stmt(stmt, program);
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

    fn next_label(&mut self) -> usize {
        self.label_counter += 1;
        self.label_counter
    }

    fn gen_stmt(&mut self, stmt: &Stmt, program: &Program) {
        match &stmt.kind {
            StmtKind::Return(expr) => {
                self.gen_expr(expr, program);
                self.emit_epilogue();
                self.emit_line("  ret");
            }
            StmtKind::If { cond, then, els } => {
                let label = self.next_label();
                self.gen_expr(cond, program);
                self.emit_line("  cmp $0, %rax");
                if let Some(els) = els {
                    self.emit_line(&format!("  je .L.else.{}", label));
                    self.gen_stmt(then, program);
                    self.emit_line(&format!("  jmp .L.end.{}", label));
                    self.emit_line(&format!(".L.else.{}:", label));
                    self.gen_stmt(els, program);
                    self.emit_line(&format!(".L.end.{}:", label));
                } else {
                    self.emit_line(&format!("  je .L.end.{}", label));
                    self.gen_stmt(then, program);
                    self.emit_line(&format!(".L.end.{}:", label));
                }
            }
            StmtKind::For {
                init,
                cond,
                inc,
                body,
            } => {
                let label = self.next_label();
                if let Some(init) = init {
                    self.gen_stmt(init, program);
                }
                self.emit_line(&format!(".L.begin.{}:", label));
                if let Some(cond) = cond {
                    self.gen_expr(cond, program);
                    self.emit_line("  cmp $0, %rax");
                    self.emit_line(&format!("  je .L.end.{}", label));
                }
                self.gen_stmt(body, program);
                if let Some(inc) = inc {
                    self.gen_expr(inc, program);
                }
                self.emit_line(&format!("  jmp .L.begin.{}", label));
                self.emit_line(&format!(".L.end.{}:", label));
            }
            StmtKind::Block(stmts) => {
                for stmt in stmts {
                    self.gen_stmt(stmt, program);
                }
            }
            StmtKind::Expr(expr) => {
                self.gen_expr(expr, program);
            }
            StmtKind::Decl(_) => {}
        }
    }

    fn gen_expr(&mut self, expr: &Expr, program: &Program) {
        match &expr.kind {
            ExprKind::Num(value) => {
                self.emit_line(&format!("  mov ${}, %rax", value));
            }
            ExprKind::Unary { op, expr } => {
                self.gen_expr(expr, program);
                match op {
                    UnaryOp::Neg => {
                        self.emit_line("  neg %rax");
                    }
                }
            }
            ExprKind::Call(name) => {
                self.emit_line("  mov $0, %rax");
                self.emit_line(&format!("  call {}", name));
            }
            ExprKind::Addr(expr) => {
                self.gen_lvalue(expr, program);
            }
            ExprKind::Deref(expr) => {
                self.gen_expr(expr, program);
                self.emit_line("  mov (%rax), %rax");
            }
            ExprKind::Var(idx) => {
                self.gen_addr(*idx, program);
                self.emit_line("  mov (%rax), %rax");
            }
            ExprKind::Assign { lhs, rhs } => {
                self.gen_lvalue(lhs, program);
                self.emit_line("  push %rax");
                self.gen_expr(rhs, program);
                self.emit_line("  pop %rdi");
                self.emit_line("  mov %rax, (%rdi)");
            }
            ExprKind::Binary { op, lhs, rhs } => {
                self.gen_expr(rhs, program);
                self.emit_line("  push %rax");
                self.gen_expr(lhs, program);
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

    fn gen_addr(&mut self, idx: usize, program: &Program) {
        let offset = program.locals[idx].offset;
        self.emit_line(&format!("  lea {}(%rbp), %rax", offset));
    }

    fn gen_lvalue(&mut self, expr: &Expr, program: &Program) {
        match &expr.kind {
            ExprKind::Var(idx) => self.gen_addr(*idx, program),
            ExprKind::Deref(expr) => self.gen_expr(expr, program),
            _ => self.emit_line("  mov $0, %rax"),
        }
    }
}
