use crate::ast::{BinaryOp, Expr, ExprKind, Obj, Program, Stmt, StmtKind, Type, UnaryOp};

const ARG_REGS_8: [&str; 6] = ["%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"];
const ARG_REGS_16: [&str; 6] = ["%di", "%si", "%dx", "%cx", "%r8w", "%r9w"];
const ARG_REGS_32: [&str; 6] = ["%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"];
const ARG_REGS_64: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];

#[derive(Default)]
pub struct Codegen {
    buffer: String,
    label_counter: usize,
    current_fn: Option<String>,
    break_stack: Vec<String>,
    continue_stack: Vec<String>,
    depth: usize,
}

impl Codegen {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn generate(mut self, program: &Program) -> String {
        self.emit_data(program);
        self.emit_text(program);
        self.buffer
    }

    fn emit_data(&mut self, program: &Program) {
        for obj in &program.globals {
            if obj.is_function || !obj.is_definition {
                continue;
            }

            if !obj.name.starts_with(".L") {
                if obj.is_static {
                    self.emit_line(&format!("  .local {}", obj.name));
                } else {
                    self.emit_line(&format!("  .globl {}", obj.name));
                }
                self.emit_line(&format!("  .align {}", obj.align));
            }

            if let Some(init_data) = &obj.init_data {
                self.emit_line("  .data");
                self.emit_line(&format!("{}:", obj.name));

                let mut pos = 0;
                let mut rel_iter = obj.relocations.iter().peekable();

                while pos < init_data.len() {
                    if let Some(rel) = rel_iter.peek()
                        && rel.offset == pos
                    {
                        // Emit a relocation
                        let addend_str = if rel.addend >= 0 {
                            format!("+{}", rel.addend)
                        } else {
                            format!("{}", rel.addend)
                        };
                        self.emit_line(&format!("  .quad {}{}", rel.label, addend_str));
                        pos += 8;
                        rel_iter.next();
                        continue;
                    }

                    // Emit a regular byte
                    self.emit_line(&format!("  .byte {}", init_data[pos]));
                    pos += 1;
                }
                continue;
            }

            self.emit_line("  .bss");
            self.emit_line(&format!("{}:", obj.name));
            self.emit_line(&format!("  .zero {}", obj.ty.size()));
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

        // Save arg registers if function is variadic
        if let Some(va_area_idx) = function.va_area {
            let gp = function.params.len();
            let va_area = &function.locals[va_area_idx];
            let off = va_area.offset;

            // va_elem
            self.emit_line(&format!("  movl ${}, {}(%rbp)", gp * 8, off));
            self.emit_line(&format!("  movl $0, {}(%rbp)", off + 4));
            self.emit_line(&format!("  movq %rbp, {}(%rbp)", off + 16));
            self.emit_line(&format!("  addq ${}, {}(%rbp)", off + 24, off + 16));

            // __reg_save_area__
            self.emit_line(&format!("  movq %rdi, {}(%rbp)", off + 24));
            self.emit_line(&format!("  movq %rsi, {}(%rbp)", off + 32));
            self.emit_line(&format!("  movq %rdx, {}(%rbp)", off + 40));
            self.emit_line(&format!("  movq %rcx, {}(%rbp)", off + 48));
            self.emit_line(&format!("  movq %r8, {}(%rbp)", off + 56));
            self.emit_line(&format!("  movq %r9, {}(%rbp)", off + 64));
            self.emit_line(&format!("  movsd %xmm0, {}(%rbp)", off + 72));
            self.emit_line(&format!("  movsd %xmm1, {}(%rbp)", off + 80));
            self.emit_line(&format!("  movsd %xmm2, {}(%rbp)", off + 88));
            self.emit_line(&format!("  movsd %xmm3, {}(%rbp)", off + 96));
            self.emit_line(&format!("  movsd %xmm4, {}(%rbp)", off + 104));
            self.emit_line(&format!("  movsd %xmm5, {}(%rbp)", off + 112));
            self.emit_line(&format!("  movsd %xmm6, {}(%rbp)", off + 120));
            self.emit_line(&format!("  movsd %xmm7, {}(%rbp)", off + 128));
        }

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

    fn push(&mut self) {
        self.emit_line("  push %rax");
        self.depth += 1;
    }

    fn pop(&mut self, reg: &str) {
        self.emit_line(&format!("  pop {}", reg));
        self.depth -= 1;
    }

    fn pushf(&mut self) {
        self.emit_line("  sub $8, %rsp");
        self.emit_line("  movsd %xmm0, (%rsp)");
        self.depth += 1;
    }

    fn popf(&mut self, reg: &str) {
        self.emit_line(&format!("  movsd (%rsp), {}", reg));
        self.emit_line("  add $8, %rsp");
        self.depth -= 1;
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
                if let Some(expr) = expr {
                    self.gen_expr(expr, function, globals);
                }
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
                let continue_label = format!(".L.cont.{}", label);
                let break_label = format!(".L.end.{}", label);
                self.break_stack.push(break_label.clone());
                self.continue_stack.push(continue_label.clone());
                if let Some(init) = init {
                    self.gen_stmt(init, function, globals);
                }
                self.emit_line(&format!(".L.begin.{}:", label));
                if let Some(cond) = cond {
                    self.gen_expr(cond, function, globals);
                    self.emit_line("  cmp $0, %rax");
                    self.emit_line(&format!("  je {}", break_label));
                }
                self.gen_stmt(body, function, globals);
                self.emit_line(&format!("{}:", continue_label));
                if let Some(inc) = inc {
                    self.gen_expr(inc, function, globals);
                }
                self.emit_line(&format!("  jmp .L.begin.{}", label));
                self.emit_line(&format!("{}:", break_label));
                self.break_stack.pop();
                self.continue_stack.pop();
            }
            StmtKind::DoWhile { body, cond } => {
                let label = self.next_label();
                let continue_label = format!(".L.cont.{}", label);
                let break_label = format!(".L.end.{}", label);
                self.break_stack.push(break_label.clone());
                self.continue_stack.push(continue_label.clone());
                self.emit_line(&format!(".L.begin.{}:", label));
                self.gen_stmt(body, function, globals);
                self.emit_line(&format!("{}:", continue_label));
                self.gen_expr(cond, function, globals);
                self.emit_line("  cmp $0, %rax");
                self.emit_line(&format!("  jne .L.begin.{}", label));
                self.emit_line(&format!("{}:", break_label));
                self.break_stack.pop();
                self.continue_stack.pop();
            }
            StmtKind::Switch {
                cond,
                body,
                cases,
                default_label,
                break_label,
            } => {
                self.gen_expr(cond, function, globals);
                let use_64 = cond.ty.as_ref().map(|ty| ty.size() == 8).unwrap_or(true);
                let reg = if use_64 { "%rax" } else { "%eax" };

                for case in cases {
                    self.emit_line(&format!("  cmp ${}, {}", case.value, reg));
                    self.emit_line(&format!("  je {}", case.label));
                }

                if let Some(label) = default_label {
                    self.emit_line(&format!("  jmp {}", label));
                }

                self.emit_line(&format!("  jmp {}", break_label));
                self.break_stack.push(break_label.clone());
                self.gen_stmt(body, function, globals);
                self.break_stack.pop();
                self.emit_line(&format!("{}:", break_label));
            }
            StmtKind::Case { label, stmt, .. } => {
                self.emit_line(&format!("{}:", label));
                self.gen_stmt(stmt, function, globals);
            }
            StmtKind::Break => {
                if let Some(label) = self.break_stack.last() {
                    self.emit_line(&format!("  jmp {}", label));
                } else {
                    self.emit_line("  jmp .L..invalid");
                }
            }
            StmtKind::Continue => {
                if let Some(label) = self.continue_stack.last() {
                    self.emit_line(&format!("  jmp {}", label));
                } else {
                    self.emit_line("  jmp .L..invalid");
                }
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
            ExprKind::Null => {
                // Do nothing
            }
            ExprKind::Memzero { idx, is_local } => {
                // Zero-clear a stack variable using rep stosb
                // `rep stosb` is equivalent to `memset(%rdi, %al, %rcx)`
                let var = if *is_local {
                    &function.locals[*idx]
                } else {
                    &globals[*idx]
                };
                self.emit_line(&format!("  mov ${}, %rcx", var.ty.size()));
                self.emit_line(&format!("  lea {}(%rbp), %rdi", var.offset));
                self.emit_line("  mov $0, %al");
                self.emit_line("  rep stosb");
            }
            ExprKind::Num { value, fval } => {
                if let Some(ty) = &expr.ty {
                    match ty {
                        Type::Float => {
                            // Convert f32 to u32 to get bit pattern
                            let bits = (*fval as f32).to_bits();
                            self.emit_line(&format!("  mov ${}, %eax  # float {}", bits, fval));
                            self.emit_line("  movq %rax, %xmm0");
                        }
                        Type::Double => {
                            // Convert f64 to u64 to get bit pattern
                            let bits = fval.to_bits();
                            self.emit_line(&format!("  mov ${}, %rax  # double {}", bits, fval));
                            self.emit_line("  movq %rax, %xmm0");
                        }
                        _ => {
                            self.emit_line(&format!("  mov ${}, %rax", value));
                        }
                    }
                } else {
                    self.emit_line(&format!("  mov ${}, %rax", value));
                }
            }
            ExprKind::Unary { op, expr } => {
                self.gen_expr(expr, function, globals);
                match op {
                    UnaryOp::Neg => {
                        let ty = expr.ty.as_ref().unwrap_or(&Type::Int);
                        match ty {
                            Type::Float => {
                                self.emit_line("  mov $1, %rax");
                                self.emit_line("  shl $31, %rax");
                                self.emit_line("  movq %rax, %xmm1");
                                self.emit_line("  xorps %xmm1, %xmm0");
                            }
                            Type::Double => {
                                self.emit_line("  mov $1, %rax");
                                self.emit_line("  shl $63, %rax");
                                self.emit_line("  movq %rax, %xmm1");
                                self.emit_line("  xorpd %xmm1, %xmm0");
                            }
                            _ => {
                                self.emit_line("  neg %rax");
                            }
                        }
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
                    self.push();
                    nargs += 1;
                }
                for i in (0..nargs).rev() {
                    self.pop(ARG_REGS_64[i]);
                }
                self.emit_line("  mov $0, %rax");

                if self.depth.is_multiple_of(2) {
                    self.emit_line(&format!("  call {}", name));
                } else {
                    self.emit_line("  sub $8, %rsp");
                    self.emit_line(&format!("  call {}", name));
                    self.emit_line("  add $8, %rsp");
                }

                // It looks like the most significant 48 or 56 bits in RAX may
                // contain garbage if a function return type is short or bool/char,
                // respectively. We clear the upper bits here.
                if let Some(ty) = &expr.ty {
                    match ty {
                        Type::Bool => {
                            self.emit_line("  movzx %al, %eax");
                        }
                        Type::Char => {
                            self.emit_line("  movsbl %al, %eax");
                        }
                        Type::UChar => {
                            self.emit_line("  movzbl %al, %eax");
                        }
                        Type::Short => {
                            self.emit_line("  movswl %ax, %eax");
                        }
                        Type::UShort => {
                            self.emit_line("  movzwl %ax, %eax");
                        }
                        _ => {}
                    }
                }
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
                self.push();
                self.gen_expr(rhs, function, globals);
                self.store(expr.ty.as_ref());
            }
            ExprKind::Cond { cond, then, els } => {
                let label = self.next_label();
                self.gen_expr(cond, function, globals);
                self.emit_line("  cmp $0, %rax");
                self.emit_line(&format!("  je .L.else.{}", label));
                self.gen_expr(then, function, globals);
                self.emit_line(&format!("  jmp .L.end.{}", label));
                self.emit_line(&format!(".L.else.{}:", label));
                self.gen_expr(els, function, globals);
                self.emit_line(&format!(".L.end.{}:", label));
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

                // Handle floating-point operations
                if matches!(
                    op,
                    BinaryOp::Add
                        | BinaryOp::Sub
                        | BinaryOp::Mul
                        | BinaryOp::Div
                        | BinaryOp::Eq
                        | BinaryOp::Ne
                        | BinaryOp::Lt
                        | BinaryOp::Le
                ) {
                    let lhs_ty = lhs.ty.as_ref().unwrap_or(&Type::Int);
                    if matches!(lhs_ty, Type::Float | Type::Double) {
                        self.gen_expr(rhs, function, globals);
                        self.pushf();
                        self.gen_expr(lhs, function, globals);
                        self.popf("%xmm1");

                        let sz = if matches!(lhs_ty, Type::Float) {
                            "ss"
                        } else {
                            "sd"
                        };

                        match op {
                            BinaryOp::Add => {
                                self.emit_line(&format!("  add{} %xmm1, %xmm0", sz));
                                return;
                            }
                            BinaryOp::Sub => {
                                self.emit_line(&format!("  sub{} %xmm1, %xmm0", sz));
                                return;
                            }
                            BinaryOp::Mul => {
                                self.emit_line(&format!("  mul{} %xmm1, %xmm0", sz));
                                return;
                            }
                            BinaryOp::Div => {
                                self.emit_line(&format!("  div{} %xmm1, %xmm0", sz));
                                return;
                            }
                            _ => {}
                        }

                        self.emit_line(&format!("  ucomi{} %xmm0, %xmm1", sz));

                        match op {
                            BinaryOp::Eq => {
                                self.emit_line("  sete %al");
                                self.emit_line("  setnp %dl");
                                self.emit_line("  and %dl, %al");
                            }
                            BinaryOp::Ne => {
                                self.emit_line("  setne %al");
                                self.emit_line("  setp %dl");
                                self.emit_line("  or %dl, %al");
                            }
                            BinaryOp::Lt => {
                                self.emit_line("  seta %al");
                            }
                            BinaryOp::Le => {
                                self.emit_line("  setae %al");
                            }
                            _ => unreachable!(),
                        }

                        self.emit_line("  and $1, %al");
                        self.emit_line("  movzb %al, %rax");
                        return;
                    }
                }

                // Handle other binary operators
                self.gen_expr(rhs, function, globals);
                self.push();
                self.gen_expr(lhs, function, globals);
                self.pop("%rdi");
                let lhs_ty = lhs.ty.as_ref().unwrap_or(&Type::Int);
                let rhs_ty = rhs.ty.as_ref().unwrap_or(&Type::Int);
                let use_64 = matches!(
                    lhs_ty,
                    Type::Long | Type::ULong | Type::Ptr(_) | Type::Array { .. }
                ) || matches!(
                    rhs_ty,
                    Type::Long | Type::ULong | Type::Ptr(_) | Type::Array { .. }
                );
                let ax = if use_64 { "%rax" } else { "%eax" };
                let di = if use_64 { "%rdi" } else { "%edi" };
                if use_64 && rhs_ty.size() < 8 {
                    if rhs_ty.is_unsigned() {
                        self.emit_line("  mov %edi, %edi");
                    } else {
                        self.emit_line("  movslq %edi, %rdi");
                    }
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
                        let dx = if use_64 { "%rdx" } else { "%edx" };
                        if lhs_ty.is_unsigned() {
                            self.emit_line(&format!("  mov $0, {}", dx));
                            self.emit_line(&format!("  div {}", di));
                        } else {
                            if lhs_ty.size() == 8 {
                                self.emit_line("  cqo");
                            } else {
                                self.emit_line("  cdq");
                            }
                            self.emit_line(&format!("  idiv {}", di));
                        }
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
                    BinaryOp::Shl => {
                        self.emit_line("  mov %rdi, %rcx");
                        self.emit_line(&format!("  shl %cl, {}", ax));
                    }
                    BinaryOp::Shr => {
                        self.emit_line("  mov %rdi, %rcx");
                        if lhs_ty.is_unsigned() {
                            self.emit_line(&format!("  shr %cl, {}", ax));
                        } else {
                            self.emit_line(&format!("  sar %cl, {}", ax));
                        }
                    }
                    BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le => {
                        self.emit_line(&format!("  cmp {}, {}", di, ax));
                        let unsigned_cmp = lhs_ty.is_unsigned() || matches!(lhs_ty, Type::Ptr(_));
                        match op {
                            BinaryOp::Eq => self.emit_line("  sete %al"),
                            BinaryOp::Ne => self.emit_line("  setne %al"),
                            BinaryOp::Lt => {
                                if unsigned_cmp {
                                    self.emit_line("  setb %al");
                                } else {
                                    self.emit_line("  setl %al");
                                }
                            }
                            BinaryOp::Le => {
                                if unsigned_cmp {
                                    self.emit_line("  setbe %al");
                                } else {
                                    self.emit_line("  setle %al");
                                }
                            }
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
            Type::Bool => 0,   // I8
            Type::Char => 0,   // I8
            Type::Short => 1,  // I16
            Type::Int => 2,    // I32
            Type::Enum => 2,   // I32
            Type::Long => 3,   // I64
            Type::UChar => 4,  // U8
            Type::UShort => 5, // U16
            Type::UInt => 6,   // U32
            Type::ULong => 7,  // U64
            Type::Float => 8,  // F32
            Type::Double => 9, // F64
            _ => 7,            // U64
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
        const I32U8: &str = "movzbl %al, %eax";
        const I32I16: &str = "movswl %ax, %eax";
        const I32U16: &str = "movzwl %ax, %eax";
        const I32F32: &str = "cvtsi2ssl %eax, %xmm0";
        const I32I64: &str = "movslq %eax, %rax";
        const I32F64: &str = "cvtsi2sdl %eax, %xmm0";

        const U32F32: &str = "mov %eax, %eax; cvtsi2ssq %rax, %xmm0";
        const U32I64: &str = "mov %eax, %eax";
        const U32F64: &str = "mov %eax, %eax; cvtsi2sdq %rax, %xmm0";

        const I64F32: &str = "cvtsi2ssq %rax, %xmm0";
        const I64F64: &str = "cvtsi2sdq %rax, %xmm0";

        const U64F32: &str = "cvtsi2ssq %rax, %xmm0";
        const U64F64: &str = "test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2sd %rax,%xmm0; jmp 2f; 1: mov %rax,%rdi; and $1,%eax; pxor %xmm0,%xmm0; shr %rdi; or %rax,%rdi; cvtsi2sd %rdi,%xmm0; addsd %xmm0,%xmm0; 2:";

        const F32I8: &str = "cvttss2sil %xmm0, %eax; movsbl %al, %eax";
        const F32U8: &str = "cvttss2sil %xmm0, %eax; movzbl %al, %eax";
        const F32I16: &str = "cvttss2sil %xmm0, %eax; movswl %ax, %eax";
        const F32U16: &str = "cvttss2sil %xmm0, %eax; movzwl %ax, %eax";
        const F32I32: &str = "cvttss2sil %xmm0, %eax";
        const F32U32: &str = "cvttss2siq %xmm0, %rax";
        const F32I64: &str = "cvttss2siq %xmm0, %rax";
        const F32U64: &str = "cvttss2siq %xmm0, %rax";
        const F32F64: &str = "cvtss2sd %xmm0, %xmm0";

        const F64I8: &str = "cvttsd2sil %xmm0, %eax; movsbl %al, %eax";
        const F64U8: &str = "cvttsd2sil %xmm0, %eax; movzbl %al, %eax";
        const F64I16: &str = "cvttsd2sil %xmm0, %eax; movswl %ax, %eax";
        const F64U16: &str = "cvttsd2sil %xmm0, %eax; movzwl %ax, %eax";
        const F64I32: &str = "cvttsd2sil %xmm0, %eax";
        const F64U32: &str = "cvttsd2siq %xmm0, %rax";
        const F64F32: &str = "cvtsd2ss %xmm0, %xmm0";
        const F64I64: &str = "cvttsd2siq %xmm0, %rax";
        const F64U64: &str = "cvttsd2siq %xmm0, %rax";

        #[rustfmt::skip]
        const CAST_TABLE: [[Option<&str>; 10]; 10] = [
            // i8            i16            i32            i64            u8             u16            u32            u64            f32            f64
            [None,          None,          None,          Some(I32I64),  Some(I32U8),   Some(I32U16),  None,          Some(I32I64),  Some(I32F32),  Some(I32F64)], // i8
            [Some(I32I8),   None,          None,          Some(I32I64),  Some(I32U8),   Some(I32U16),  None,          Some(I32I64),  Some(I32F32),  Some(I32F64)], // i16
            [Some(I32I8),   Some(I32I16),  None,          Some(I32I64),  Some(I32U8),   Some(I32U16),  None,          Some(I32I64),  Some(I32F32),  Some(I32F64)], // i32
            [Some(I32I8),   Some(I32I16),  None,          None,          Some(I32U8),   Some(I32U16),  None,          None,          Some(I64F32),  Some(I64F64)], // i64

            [Some(I32I8),   None,          None,          Some(I32I64),  None,          None,          None,          Some(I32I64),  Some(I32F32),  Some(I32F64)], // u8
            [Some(I32I8),   Some(I32I16),  None,          Some(I32I64),  Some(I32U8),   None,          None,          Some(I32I64),  Some(I32F32),  Some(I32F64)], // u16
            [Some(I32I8),   Some(I32I16),  None,          Some(U32I64),  Some(I32U8),   Some(I32U16),  None,          Some(U32I64),  Some(U32F32),  Some(U32F64)], // u32
            [Some(I32I8),   Some(I32I16),  None,          None,          Some(I32U8),   Some(I32U16),  None,          None,          Some(U64F32),  Some(U64F64)], // u64

            [Some(F32I8),   Some(F32I16),  Some(F32I32),  Some(F32I64),  Some(F32U8),   Some(F32U16),  Some(F32U32),  Some(F32U64),  None,          Some(F32F64)], // f32
            [Some(F64I8),   Some(F64I16),  Some(F64I32),  Some(F64I64),  Some(F64U8),   Some(F64U16),  Some(F64U32),  Some(F64U64),  Some(F64F32),  None        ], // f64
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

        if let Some(ty) = ty {
            match ty {
                // If it is an array, struct, or union, do not attempt to load a value to the
                // register because in general we can't load an entire aggregate to a
                // register. As a result, the result of an evaluation of an array/struct/union
                // becomes not the value itself but the address.
                // This is where "array is automatically converted to a pointer to
                // the first element of the array in C" occurs.
                Type::Array { .. } | Type::Struct { .. } | Type::Union { .. } => return,
                Type::Float => {
                    self.emit_line("  movss (%rax), %xmm0");
                    return;
                }
                Type::Double => {
                    self.emit_line("  movsd (%rax), %xmm0");
                    return;
                }
                _ => {}
            }

            match ty.size() {
                1 => {
                    if ty.is_unsigned() {
                        self.emit_line("  movzbl (%rax), %eax");
                    } else {
                        self.emit_line("  movsbl (%rax), %eax");
                    }
                }
                2 => {
                    if ty.is_unsigned() {
                        self.emit_line("  movzwl (%rax), %eax");
                    } else {
                        self.emit_line("  movswl (%rax), %eax");
                    }
                }
                4 => self.emit_line("  movslq (%rax), %rax"),
                8 => self.emit_line("  mov (%rax), %rax"),
                _ => unreachable!(),
            }
        }
    }

    fn store(&mut self, ty: Option<&crate::ast::Type>) {
        use crate::ast::Type;
        self.pop("%rdi");

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
                Type::Float => {
                    self.emit_line("  movss %xmm0, (%rdi)");
                    return;
                }
                Type::Double => {
                    self.emit_line("  movsd %xmm0, (%rdi)");
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
