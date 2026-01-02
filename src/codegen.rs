use crate::ast::{BinaryOp, Expr, ExprKind, Obj, Program, Stmt, StmtKind, Type, UnaryOp};

const ARG_REGS_8: [&str; 6] = ["%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"];
const ARG_REGS_16: [&str; 6] = ["%di", "%si", "%dx", "%cx", "%r8w", "%r9w"];
const ARG_REGS_32: [&str; 6] = ["%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"];
const ARG_REGS_64: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

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
        self.emit_data(program);
        self.emit_text(program);
        self.buffer
    }

    fn emit_data(&mut self, program: &Program) {
        for obj in &program.globals {
            if obj.is_function {
                continue;
            }

            self.emit_line("  .data");
            if !obj.name.starts_with(".L") {
                self.emit_line(&format!("  .globl {}", obj.name));
            }
            self.emit_line(&format!("{}:", obj.name));
            if let Some(init_data) = &obj.init_data {
                for byte in init_data {
                    self.emit_line(&format!("  .byte {}", byte));
                }
            } else {
                self.emit_line(&format!("  .zero {}", obj.ty.size()));
            }
        }
    }

    fn emit_text(&mut self, program: &Program) {
        for obj in &program.globals {
            if obj.is_function && obj.is_definition {
                self.generate_function(obj, &program.globals);
            }
        }
    }

    fn store_gp(&mut self, r: usize, offset: i32, sz: i64) {
        match sz {
            1 => self.emit_line(&format!("  mov {}, {}(%rbp)", ARG_REGS_8[r], offset)),
            2 => self.emit_line(&format!("  mov {}, {}(%rbp)", ARG_REGS_16[r], offset)),
            4 => self.emit_line(&format!("  mov {}, {}(%rbp)", ARG_REGS_32[r], offset)),
            8 => self.emit_line(&format!("  mov {}, {}(%rbp)", ARG_REGS_64[r], offset)),
            _ => unreachable!(),
        }
    }

    fn generate_function(&mut self, function: &Obj, globals: &[Obj]) {
        self.current_fn = Some(function.name.clone());

        if function.is_static {
            self.emit_line(&format!("  .local {}", function.name));
        } else {
            self.emit_line(&format!("  .globl {}", function.name));
        }
        self.emit_line("  .text");
        self.emit_line(&format!("{}:", function.name));
        self.emit_line("  push %rbp");
        self.emit_line("  mov %rsp, %rbp");
        self.emit_line(&format!("  sub ${}, %rsp", function.stack_size));

        // Save passed-by-register arguments to the stack
        for (i, param) in function.params.iter().enumerate() {
            self.store_gp(i, param.offset, param.ty.size());
        }

        let mut last_was_return = false;
        for stmt in &function.body {
            last_was_return = matches!(stmt.kind, StmtKind::Return(_));
            self.gen_stmt(stmt, function, globals);
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

    fn label_symbol(&self, function: &Obj, label: &str) -> String {
        format!(".L.{}.{}", function.name, label)
    }

    fn next_label(&mut self) -> usize {
        self.label_counter += 1;
        self.label_counter
    }

    fn gen_stmt(&mut self, stmt: &Stmt, function: &Obj, globals: &[Obj]) {
        self.emit_line(&format!("  .loc 1 {}", stmt.location.line));
        match &stmt.kind {
            StmtKind::Return(expr) => {
                self.gen_expr(expr, function, globals);
                self.emit_epilogue();
                self.emit_line("  ret");
            }
            StmtKind::If { cond, then, els } => {
                let label = self.next_label();
                self.gen_expr(cond, function, globals);
                self.emit_line("  cmp $0, %rax");
                if let Some(els) = els {
                    self.emit_line(&format!("  je .L.else.{}", label));
                    self.gen_stmt(then, function, globals);
                    self.emit_line(&format!("  jmp .L.end.{}", label));
                    self.emit_line(&format!(".L.else.{}:", label));
                    self.gen_stmt(els, function, globals);
                    self.emit_line(&format!(".L.end.{}:", label));
                } else {
                    self.emit_line(&format!("  je .L.end.{}", label));
                    self.gen_stmt(then, function, globals);
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
                    self.gen_stmt(init, function, globals);
                }
                self.emit_line(&format!(".L.begin.{}:", label));
                if let Some(cond) = cond {
                    self.gen_expr(cond, function, globals);
                    self.emit_line("  cmp $0, %rax");
                    self.emit_line(&format!("  je .L.end.{}", label));
                }
                self.gen_stmt(body, function, globals);
                if let Some(inc) = inc {
                    self.gen_expr(inc, function, globals);
                }
                self.emit_line(&format!("  jmp .L.begin.{}", label));
                self.emit_line(&format!(".L.end.{}:", label));
            }
            StmtKind::Goto { label } => {
                let target = self.label_symbol(function, label);
                self.emit_line(&format!("  jmp {}", target));
            }
            StmtKind::Label { label, stmt } => {
                let symbol = self.label_symbol(function, label);
                self.emit_line(&format!("{}:", symbol));
                self.gen_stmt(stmt, function, globals);
            }
            StmtKind::Block(stmts) => {
                for stmt in stmts {
                    self.gen_stmt(stmt, function, globals);
                }
            }
            StmtKind::Expr(expr) => {
                self.gen_expr(expr, function, globals);
            }
            StmtKind::Decl(_) => {}
        }
    }

    fn gen_expr(&mut self, expr: &Expr, function: &Obj, globals: &[Obj]) {
        self.emit_line(&format!("  .loc 1 {}", expr.location.line));
        match &expr.kind {
            ExprKind::Num(value) => {
                self.emit_line(&format!("  mov ${}, %rax", value));
            }
            ExprKind::Unary { op, expr } => {
                self.gen_expr(expr, function, globals);
                match op {
                    UnaryOp::Neg => {
                        self.emit_line("  neg %rax");
                    }
                    UnaryOp::Not => {
                        self.emit_line("  cmp $0, %rax");
                        self.emit_line("  sete %al");
                        self.emit_line("  movzx %al, %rax");
                    }
                    UnaryOp::BitNot => {
                        self.emit_line("  not %rax");
                    }
                }
            }
            ExprKind::Cast { expr, ty } => {
                self.gen_expr(expr, function, globals);
                let from = expr.ty.as_ref().unwrap_or(&Type::Int);
                self.cast(from, ty);
            }
            ExprKind::Call { name, args } => {
                let mut nargs = 0;
                for arg in args {
                    self.gen_expr(arg, function, globals);
                    self.emit_line("  push %rax");
                    nargs += 1;
                }
                for i in (0..nargs).rev() {
                    self.emit_line(&format!("  pop {}", ARG_REGS_64[i]));
                }
                self.emit_line("  mov $0, %rax");
                self.emit_line(&format!("  call {}", name));
            }
            ExprKind::Addr(expr) => {
                self.gen_lvalue(expr, function, globals);
            }
            ExprKind::Deref(inner) => {
                self.gen_expr(inner, function, globals);
                self.load(expr.ty.as_ref());
            }
            ExprKind::Var { idx, is_local } => {
                self.gen_addr(*idx, *is_local, function, globals);
                self.load(expr.ty.as_ref());
            }
            ExprKind::Member { .. } => {
                self.gen_lvalue(expr, function, globals);
                self.load(expr.ty.as_ref());
            }
            ExprKind::StmtExpr(stmts) => {
                for stmt in stmts {
                    self.gen_stmt(stmt, function, globals);
                }
            }
            ExprKind::Assign { lhs, rhs } => {
                self.gen_lvalue(lhs, function, globals);
                self.emit_line("  push %rax");
                self.gen_expr(rhs, function, globals);
                self.store(expr.ty.as_ref());
            }
            ExprKind::Comma { lhs, rhs } => {
                self.gen_expr(lhs, function, globals);
                self.gen_expr(rhs, function, globals);
            }
            ExprKind::Binary { op, lhs, rhs } => {
                // Handle short-circuit operators specially
                if matches!(op, BinaryOp::LogAnd | BinaryOp::LogOr) {
                    let c = self.next_label();
                    if matches!(op, BinaryOp::LogAnd) {
                        // Logical AND: false if either operand is false
                        self.gen_expr(lhs, function, globals);
                        self.emit_line("  cmp $0, %rax");
                        self.emit_line(&format!("  je .L.false.{}", c));
                        self.gen_expr(rhs, function, globals);
                        self.emit_line("  cmp $0, %rax");
                        self.emit_line(&format!("  je .L.false.{}", c));
                        self.emit_line("  mov $1, %rax");
                        self.emit_line(&format!("  jmp .L.end.{}", c));
                        self.emit_line(&format!(".L.false.{}:", c));
                        self.emit_line("  mov $0, %rax");
                        self.emit_line(&format!(".L.end.{}:", c));
                    } else {
                        // Logical OR: true if either operand is true
                        self.gen_expr(lhs, function, globals);
                        self.emit_line("  cmp $0, %rax");
                        self.emit_line(&format!("  jne .L.true.{}", c));
                        self.gen_expr(rhs, function, globals);
                        self.emit_line("  cmp $0, %rax");
                        self.emit_line(&format!("  jne .L.true.{}", c));
                        self.emit_line("  mov $0, %rax");
                        self.emit_line(&format!("  jmp .L.end.{}", c));
                        self.emit_line(&format!(".L.true.{}:", c));
                        self.emit_line("  mov $1, %rax");
                        self.emit_line(&format!(".L.end.{}:", c));
                    }
                    return;
                }

                // Handle other binary operators
                self.gen_expr(rhs, function, globals);
                self.emit_line("  push %rax");
                self.gen_expr(lhs, function, globals);
                self.emit_line("  pop %rdi");
                let lhs_ty = lhs.ty.as_ref().unwrap_or(&Type::Int);
                let rhs_ty = rhs.ty.as_ref().unwrap_or(&Type::Int);
                let use_64 = matches!(lhs_ty, Type::Long | Type::Ptr(_) | Type::Array { .. })
                    || matches!(rhs_ty, Type::Long | Type::Ptr(_) | Type::Array { .. });
                let ax = if use_64 { "%rax" } else { "%eax" };
                let di = if use_64 { "%rdi" } else { "%edi" };
                if use_64 && rhs_ty.size() < 8 {
                    self.emit_line("  movslq %edi, %rdi");
                }
                match op {
                    BinaryOp::Add => {
                        self.emit_line(&format!("  add {}, {}", di, ax));
                    }
                    BinaryOp::Sub => {
                        self.emit_line(&format!("  sub {}, {}", di, ax));
                    }
                    BinaryOp::Mul => {
                        self.emit_line(&format!("  imul {}, {}", di, ax));
                    }
                    BinaryOp::Div | BinaryOp::Mod => {
                        if lhs_ty.size() == 8 {
                            self.emit_line("  cqo");
                        } else {
                            self.emit_line("  cdq");
                        }
                        self.emit_line(&format!("  idiv {}", di));
                        if matches!(op, BinaryOp::Mod) {
                            self.emit_line("  mov %rdx, %rax");
                        }
                    }
                    BinaryOp::BitAnd => {
                        self.emit_line(&format!("  and {}, {}", di, ax));
                    }
                    BinaryOp::BitOr => {
                        self.emit_line(&format!("  or {}, {}", di, ax));
                    }
                    BinaryOp::BitXor => {
                        self.emit_line(&format!("  xor {}, {}", di, ax));
                    }
                    BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le => {
                        self.emit_line(&format!("  cmp {}, {}", di, ax));
                        match op {
                            BinaryOp::Eq => self.emit_line("  sete %al"),
                            BinaryOp::Ne => self.emit_line("  setne %al"),
                            BinaryOp::Lt => self.emit_line("  setl %al"),
                            BinaryOp::Le => self.emit_line("  setle %al"),
                            _ => {}
                        }
                        self.emit_line("  movzb %al, %rax");
                    }
                    BinaryOp::LogAnd | BinaryOp::LogOr => {
                        unreachable!("LogAnd and LogOr should be handled earlier")
                    }
                }
            }
        }
    }

    fn type_id(&self, ty: &Type) -> usize {
        match ty {
            Type::Bool => 0,
            Type::Char => 0,
            Type::Short => 1,
            Type::Int => 2,
            Type::Enum => 2,
            _ => 3,
        }
    }

    fn cast(&mut self, from: &Type, to: &Type) {
        if matches!(to, Type::Void) {
            return;
        }

        if matches!(to, Type::Bool) {
            self.cmp_zero(from);
            self.emit_line("  setne %al");
            self.emit_line("  movzx %al, %eax");
            return;
        }

        const I32I8: &str = "movsbl %al, %eax";
        const I32I16: &str = "movswl %ax, %eax";
        const I32I64: &str = "movslq %eax, %rax";

        const CAST_TABLE: [[Option<&str>; 4]; 4] = [
            [None, None, None, Some(I32I64)],
            [Some(I32I8), None, None, Some(I32I64)],
            [Some(I32I8), Some(I32I16), None, Some(I32I64)],
            [Some(I32I8), Some(I32I16), None, None],
        ];

        let t1 = self.type_id(from);
        let t2 = self.type_id(to);
        if let Some(insn) = CAST_TABLE[t1][t2] {
            self.emit_line(&format!("  {}", insn));
        }
    }

    fn cmp_zero(&mut self, ty: &Type) {
        if ty.is_integer() && ty.size() <= 4 {
            self.emit_line("  cmp $0, %eax");
        } else {
            self.emit_line("  cmp $0, %rax");
        }
    }

    fn load(&mut self, ty: Option<&crate::ast::Type>) {
        use crate::ast::Type;
        // If it is an array, struct, or union, do not attempt to load a value to the
        // register because in general we can't load an entire aggregate to a
        // register. As a result, the result of an evaluation of an array/struct/union
        // becomes not the value itself but the address.
        // This is where "array is automatically converted to a pointer to
        // the first element of the array in C" occurs.
        if let Some(ty) = ty
            && let Type::Array { .. } | Type::Struct { .. } | Type::Union { .. } = ty
        {
            return;
        }

        if let Some(ty) = ty {
            match ty.size() {
                1 => self.emit_line("  movsbl (%rax), %eax"),
                2 => self.emit_line("  movswl (%rax), %eax"),
                4 => self.emit_line("  movslq (%rax), %rax"),
                8 => self.emit_line("  mov (%rax), %rax"),
                _ => unreachable!(),
            }
        }
    }

    fn store(&mut self, ty: Option<&crate::ast::Type>) {
        use crate::ast::Type;
        self.emit_line("  pop %rdi");

        // For struct/union, copy bytes one by one
        if let Some(ty) = ty {
            match ty {
                Type::Struct { .. } | Type::Union { .. } => {
                    for i in 0..ty.size() {
                        self.emit_line(&format!("  mov {}(%rax), %r8b", i));
                        self.emit_line(&format!("  mov %r8b, {}(%rdi)", i));
                    }
                    return;
                }
                _ => {}
            }
        }

        if let Some(ty) = ty {
            match ty.size() {
                1 => self.emit_line("  mov %al, (%rdi)"),
                2 => self.emit_line("  mov %ax, (%rdi)"),
                4 => self.emit_line("  mov %eax, (%rdi)"),
                8 => self.emit_line("  mov %rax, (%rdi)"),
                _ => unreachable!(),
            }
        }
    }

    fn emit_epilogue(&mut self) {
        self.emit_line("  mov %rbp, %rsp");
        self.emit_line("  pop %rbp");
    }

    fn gen_addr(&mut self, idx: usize, is_local: bool, function: &Obj, globals: &[Obj]) {
        if is_local {
            let offset = function.locals[idx].offset;
            self.emit_line(&format!("  lea {}(%rbp), %rax", offset));
            return;
        }
        let name = &globals[idx].name;
        self.emit_line(&format!("  lea {}(%rip), %rax", name));
    }

    fn gen_lvalue(&mut self, expr: &Expr, function: &Obj, globals: &[Obj]) {
        match &expr.kind {
            ExprKind::Var { idx, is_local } => self.gen_addr(*idx, *is_local, function, globals),
            ExprKind::Deref(expr) => self.gen_expr(expr, function, globals),
            ExprKind::Comma { lhs, rhs } => {
                self.gen_expr(lhs, function, globals);
                self.gen_lvalue(rhs, function, globals);
            }
            ExprKind::Member { lhs, member } => {
                self.gen_lvalue(lhs, function, globals);
                self.emit_line(&format!("  add ${}, %rax", member.offset));
            }
            _ => self.emit_line("  mov $0, %rax"),
        }
    }
}
