use crate::ast::{BinaryOp, Expr, ExprKind, Obj, Program, Stmt, StmtKind, UnaryOp};

const ARG_REGS: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

pub struct Codegen {
    buffer: String,
    label_counter: usize,
    current_fn: Option<String>,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            buffer: String::new(),
            label_counter: 0,
            current_fn: None,
        }
    }

    pub fn generate(mut self, program: &Program) -> String {
        for obj in &program.globals {
            if obj.is_function {
                self.generate_function(obj);
            }
        }
        self.buffer
    }

    fn generate_function(&mut self, function: &Obj) {
        self.current_fn = Some(function.name.clone());

        self.emit_line(&format!("  .globl {}", function.name));
        self.emit_line("  .text");
        self.emit_line(&format!("{}:", function.name));
        self.emit_line("  push %rbp");
        self.emit_line("  mov %rsp, %rbp");
        self.emit_line(&format!("  sub ${}, %rsp", function.stack_size));

        // Save passed-by-register arguments to the stack
        for (i, param) in function.params.iter().enumerate() {
            self.emit_line(&format!("  mov {}, {}(%rbp)", ARG_REGS[i], param.offset));
        }

        let mut last_was_return = false;
        for stmt in &function.body {
            last_was_return = matches!(stmt.kind, StmtKind::Return(_));
            self.gen_stmt(stmt, function);
        }

        if !last_was_return {
            self.emit_line("  mov $0, %rax");
            self.emit_epilogue();
            self.emit_line("  ret");
        }
    }

    fn emit_line(&mut self, line: &str) {
        self.buffer.push_str(line);
        self.buffer.push('\n');
    }

    fn next_label(&mut self) -> usize {
        self.label_counter += 1;
        self.label_counter
    }

    fn gen_stmt(&mut self, stmt: &Stmt, function: &Obj) {
        match &stmt.kind {
            StmtKind::Return(expr) => {
                self.gen_expr(expr, function);
                self.emit_epilogue();
                self.emit_line("  ret");
            }
            StmtKind::If { cond, then, els } => {
                let label = self.next_label();
                self.gen_expr(cond, function);
                self.emit_line("  cmp $0, %rax");
                if let Some(els) = els {
                    self.emit_line(&format!("  je .L.else.{}", label));
                    self.gen_stmt(then, function);
                    self.emit_line(&format!("  jmp .L.end.{}", label));
                    self.emit_line(&format!(".L.else.{}:", label));
                    self.gen_stmt(els, function);
                    self.emit_line(&format!(".L.end.{}:", label));
                } else {
                    self.emit_line(&format!("  je .L.end.{}", label));
                    self.gen_stmt(then, function);
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
                    self.gen_stmt(init, function);
                }
                self.emit_line(&format!(".L.begin.{}:", label));
                if let Some(cond) = cond {
                    self.gen_expr(cond, function);
                    self.emit_line("  cmp $0, %rax");
                    self.emit_line(&format!("  je .L.end.{}", label));
                }
                self.gen_stmt(body, function);
                if let Some(inc) = inc {
                    self.gen_expr(inc, function);
                }
                self.emit_line(&format!("  jmp .L.begin.{}", label));
                self.emit_line(&format!(".L.end.{}:", label));
            }
            StmtKind::Block(stmts) => {
                for stmt in stmts {
                    self.gen_stmt(stmt, function);
                }
            }
            StmtKind::Expr(expr) => {
                self.gen_expr(expr, function);
            }
            StmtKind::Decl(_) => {}
        }
    }

    fn gen_expr(&mut self, expr: &Expr, function: &Obj) {
        match &expr.kind {
            ExprKind::Num(value) => {
                self.emit_line(&format!("  mov ${}, %rax", value));
            }
            ExprKind::Unary { op, expr } => {
                self.gen_expr(expr, function);
                match op {
                    UnaryOp::Neg => {
                        self.emit_line("  neg %rax");
                    }
                }
            }
            ExprKind::Call { name, args } => {
                let mut nargs = 0;
                for arg in args {
                    self.gen_expr(arg, function);
                    self.emit_line("  push %rax");
                    nargs += 1;
                }
                for i in (0..nargs).rev() {
                    self.emit_line(&format!("  pop {}", ARG_REGS[i]));
                }
                self.emit_line("  mov $0, %rax");
                self.emit_line(&format!("  call {}", name));
            }
            ExprKind::Addr(expr) => {
                self.gen_lvalue(expr, function);
            }
            ExprKind::Deref(inner) => {
                self.gen_expr(inner, function);
                self.load(expr.ty.as_ref());
            }
            ExprKind::Var(idx) => {
                self.gen_addr(*idx, function);
                self.load(expr.ty.as_ref());
            }
            ExprKind::Assign { lhs, rhs } => {
                self.gen_lvalue(lhs, function);
                self.emit_line("  push %rax");
                self.gen_expr(rhs, function);
                self.store();
            }
            ExprKind::Binary { op, lhs, rhs } => {
                self.gen_expr(rhs, function);
                self.emit_line("  push %rax");
                self.gen_expr(lhs, function);
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

    fn load(&mut self, ty: Option<&crate::ast::Type>) {
        // If it is an array, do not attempt to load a value to the
        // register because in general we can't load an entire array to a
        // register. As a result, the result of an evaluation of an array
        // becomes not the array itself but the address of the array.
        // This is where "array is automatically converted to a pointer to
        // the first element of the array in C" occurs.
        if let Some(ty) = ty {
            if ty.is_array() {
                return;
            }
        }
        self.emit_line("  mov (%rax), %rax");
    }

    fn store(&mut self) {
        self.emit_line("  pop %rdi");
        self.emit_line("  mov %rax, (%rdi)");
    }

    fn emit_epilogue(&mut self) {
        self.emit_line("  mov %rbp, %rsp");
        self.emit_line("  pop %rbp");
    }

    fn gen_addr(&mut self, idx: usize, function: &Obj) {
        let offset = function.locals[idx].offset;
        self.emit_line(&format!("  lea {}(%rbp), %rax", offset));
    }

    fn gen_lvalue(&mut self, expr: &Expr, function: &Obj) {
        match &expr.kind {
            ExprKind::Var(idx) => self.gen_addr(*idx, function),
            ExprKind::Deref(expr) => self.gen_expr(expr, function),
            _ => self.emit_line("  mov $0, %rax"),
        }
    }
}
