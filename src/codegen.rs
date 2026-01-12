use crate::ast::{BinaryOp, Expr, ExprKind, Obj, Program, Stmt, StmtKind, Type, UnaryOp};
use crate::error::SourceLocation;
use crate::lexer::get_input_files;
use std::sync::atomic::{AtomicBool, Ordering};

static OPT_FCOMMON: AtomicBool = AtomicBool::new(true);
static OPT_FPIC: AtomicBool = AtomicBool::new(false);

pub fn set_opt_fcommon(value: bool) {
    OPT_FCOMMON.store(value, Ordering::Relaxed);
}

fn get_opt_fcommon() -> bool {
    OPT_FCOMMON.load(Ordering::Relaxed)
}

pub fn set_opt_fpic(value: bool) {
    OPT_FPIC.store(value, Ordering::Relaxed);
}

fn get_opt_fpic() -> bool {
    OPT_FPIC.load(Ordering::Relaxed)
}
const ARG_REGS_8: [&str; 6] = ["%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"];
const ARG_REGS_16: [&str; 6] = ["%di", "%si", "%dx", "%cx", "%r8w", "%r9w"];
const ARG_REGS_32: [&str; 6] = ["%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"];
const ARG_REGS_64: [&str; 6] = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
const GP_MAX: usize = 6;
const FP_MAX: usize = 8;

#[derive(Clone, Copy)]
enum ArgClass {
    Gp,
    Fp,
}

struct ArgInfo {
    pass_by_stack: bool,
    reg_classes: Vec<ArgClass>,
    stack_slots: usize,
}

fn align_to(n: i64, align: i64) -> i64 {
    (n + align - 1) / align * align
}

fn has_flonum(ty: &Type, lo: i64, hi: i64, offset: i64) -> bool {
    match ty {
        Type::Struct { members, .. } | Type::Union { members, .. } => members
            .iter()
            .all(|member| has_flonum(&member.ty, lo, hi, offset + member.offset as i64)),
        Type::Array { base, len } => {
            (0..*len).all(|idx| has_flonum(base, lo, hi, offset + base.size() * idx as i64))
        }
        _ => {
            let is_flonum = matches!(ty, Type::Float | Type::Double);
            offset < lo || hi <= offset || is_flonum
        }
    }
}

fn has_flonum1(ty: &Type) -> bool {
    has_flonum(ty, 0, 8, 0)
}

fn has_flonum2(ty: &Type) -> bool {
    has_flonum(ty, 8, 16, 0)
}

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
        self.emit_files();
        self.emit_data(program);
        self.emit_text(program);
        self.buffer
    }

    fn asm_symbol(&self, name: &str) -> String {
        if name
            .as_bytes()
            .iter()
            .all(|b| b.is_ascii_alphanumeric() || matches!(b, b'_' | b'.' | b'$'))
        {
            return name.to_string();
        }

        let mut out = String::from("__cc3_u8_");
        for b in name.as_bytes() {
            use std::fmt::Write;
            write!(&mut out, "{:02x}", b).expect("utf-8 symbol encode");
        }
        out
    }

    fn section_name(&self, base: &str, symbol: &str) -> String {
        format!("{base}.{symbol}")
    }

    fn emit_files(&mut self) {
        for file in get_input_files() {
            self.emit_line(&format!(
                "  .file {} \"{}\"",
                file.file_no,
                file.name.display()
            ));
        }
    }

    fn emit_data(&mut self, program: &Program) {
        for obj in &program.globals {
            if obj.is_function || !obj.is_definition {
                continue;
            }

            let symbol = self.asm_symbol(&obj.name);
            if !obj.name.starts_with(".L") {
                if obj.is_static {
                    self.emit_line(&format!("  .local {}", symbol));
                } else {
                    self.emit_line(&format!("  .globl {}", symbol));
                }

                // AMD64 System V ABI has a special alignment rule for an array of
                // length at least 16 bytes. We need to align such array to at least
                // 16-byte boundaries. See p.14 of
                // https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-draft.pdf.
                let align = if matches!(obj.ty, Type::Array { .. }) && obj.ty.size() >= 16 {
                    obj.align.max(16)
                } else {
                    obj.align
                };

                if get_opt_fcommon() && obj.is_tentative {
                    self.emit_line(&format!("  .comm {}, {}, {}", symbol, obj.ty.size(), align));
                    continue;
                }
            }

            if let Some(init_data) = &obj.init_data {
                // .data/.rodata or .tdata
                if obj.is_tls {
                    self.emit_line(&format!(
                        "  .section {},\"awT\",@progbits",
                        self.section_name(".tdata", &symbol)
                    ));
                } else {
                    let section = if obj.is_readonly { ".rodata" } else { ".data" };
                    let flags = if obj.is_readonly { "\"a\"" } else { "\"aw\"" };
                    self.emit_line(&format!(
                        "  .section {},{},@progbits",
                        self.section_name(section, &symbol),
                        flags
                    ));
                }

                let align = if matches!(obj.ty, Type::Array { .. }) && obj.ty.size() >= 16 {
                    obj.align.max(16)
                } else {
                    obj.align
                };
                self.emit_line(&format!("  .type {}, @object", symbol));
                self.emit_line(&format!("  .size {}, {}", symbol, obj.ty.size()));
                self.emit_line(&format!("  .align {}", align));
                self.emit_line(&format!("{}:", symbol));

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
                        self.emit_line(&format!(
                            "  .quad {}{}",
                            self.asm_symbol(&rel.label),
                            addend_str
                        ));
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

            // .bss or .tbss
            if obj.is_tls {
                self.emit_line(&format!(
                    "  .section {},\"awT\",@nobits",
                    self.section_name(".tbss", &symbol)
                ));
            } else {
                self.emit_line(&format!(
                    "  .section {},\"aw\",@nobits",
                    self.section_name(".bss", &symbol)
                ));
            }
            let align = if matches!(obj.ty, Type::Array { .. }) && obj.ty.size() >= 16 {
                obj.align.max(16)
            } else {
                obj.align
            };
            self.emit_line(&format!("  .align {}", align));
            self.emit_line(&format!("{}:", symbol));
            self.emit_line(&format!("  .zero {}", obj.ty.size()));
        }
    }

    fn emit_text(&mut self, program: &Program) {
        for obj in &program.globals {
            if !obj.is_function || !obj.is_definition {
                continue;
            }
            if !obj.is_live {
                continue;
            }
            self.generate_function(obj, &program.globals);
        }
    }

    fn store_gp(&mut self, r: usize, offset: i32, sz: i64) {
        match sz {
            1 => self.emit_line(&format!("  mov {}, {}(%rbp)", ARG_REGS_8[r], offset)),
            2 => self.emit_line(&format!("  mov {}, {}(%rbp)", ARG_REGS_16[r], offset)),
            4 => self.emit_line(&format!("  mov {}, {}(%rbp)", ARG_REGS_32[r], offset)),
            8 => self.emit_line(&format!("  mov {}, {}(%rbp)", ARG_REGS_64[r], offset)),
            _ => {
                for i in 0..sz {
                    self.emit_line(&format!(
                        "  mov {}, {}(%rbp)",
                        ARG_REGS_8[r],
                        offset + i as i32
                    ));
                    self.emit_line(&format!("  shr $8, {}", ARG_REGS_64[r]));
                }
            }
        }
    }

    fn store_fp(&mut self, r: usize, offset: i32, sz: i64) {
        match sz {
            4 => self.emit_line(&format!("  movss %xmm{}, {}(%rbp)", r, offset)),
            8 => self.emit_line(&format!("  movsd %xmm{}, {}(%rbp)", r, offset)),
            _ => unreachable!(),
        }
    }

    fn generate_function(&mut self, function: &Obj, globals: &[Obj]) {
        self.current_fn = Some(function.name.clone());
        let symbol = self.asm_symbol(&function.name);

        if function.is_static {
            self.emit_line(&format!("  .local {}", symbol));
        } else {
            self.emit_line(&format!("  .globl {}", symbol));
        }
        self.emit_line(&format!(
            "  .section {},\"ax\",@progbits",
            self.section_name(".text", &symbol)
        ));
        self.emit_line(&format!("  .type {}, @function", symbol));
        self.emit_line(&format!("{}:", symbol));
        self.emit_line("  push %rbp");
        self.emit_line("  mov %rsp, %rbp");
        self.emit_line(&format!("  sub ${}, %rsp", function.stack_size));

        // Initialize alloca_bottom to current stack pointer
        if let Some(alloca_bottom_idx) = function.alloca_bottom {
            let alloca_bottom = &function.locals[alloca_bottom_idx];
            self.emit_line(&format!("  mov %rsp, {}(%rbp)", alloca_bottom.offset));
        }

        // Save arg registers if function is variadic
        if let Some(va_area_idx) = function.va_area {
            let mut gp = 0;
            let mut fp = 0;
            for param in &function.params {
                match param.ty {
                    Type::Float | Type::Double => {
                        fp += 1;
                    }
                    Type::LDouble => {}
                    _ => {
                        gp += 1;
                    }
                }
            }
            let va_area = &function.locals[va_area_idx];
            let off = va_area.offset;

            // va_elem
            self.emit_line(&format!("  movl ${}, {}(%rbp)", gp * 8, off));
            self.emit_line(&format!("  movl ${}, {}(%rbp)", fp * 8 + 48, off + 4));
            self.emit_line(&format!("  movq %rbp, {}(%rbp)", off + 8));
            self.emit_line(&format!("  addq $16, {}(%rbp)", off + 8));
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
        let mut gp = 0;
        let mut fp = 0;
        for param in &function.params {
            if param.offset > 0 {
                continue;
            }
            match &param.ty {
                Type::Struct { .. } | Type::Union { .. } => {
                    debug_assert!(param.ty.size() <= 16);
                    let first = std::cmp::min(8, param.ty.size());
                    if has_flonum(&param.ty, 0, 8, 0) {
                        self.store_fp(fp, param.offset, first);
                        fp += 1;
                    } else {
                        self.store_gp(gp, param.offset, first);
                        gp += 1;
                    }

                    if param.ty.size() > 8 {
                        let second = param.ty.size() - 8;
                        if has_flonum(&param.ty, 8, 16, 0) {
                            self.store_fp(fp, param.offset + 8, second);
                            fp += 1;
                        } else {
                            self.store_gp(gp, param.offset + 8, second);
                            gp += 1;
                        }
                    }
                }
                Type::Float | Type::Double => {
                    self.store_fp(fp, param.offset, param.ty.size());
                    fp += 1;
                }
                Type::LDouble => {}
                _ => {
                    self.store_gp(gp, param.offset, param.ty.size());
                    gp += 1;
                }
            }
        }

        let mut last_was_return = false;
        for stmt in &function.body {
            last_was_return = matches!(stmt.kind, StmtKind::Return(_));
            self.gen_stmt(stmt, function, globals);
        }

        // The C spec defines a special rule for the main function. Reaching the end
        // of the main function is equivalent to returning 0, even though the
        // behavior is undefined for other functions.
        // https://www.sigbus.info/n1570#5.1.2.2.3p1
        if !last_was_return {
            if function.name == "main" {
                self.emit_line("  mov $0, %rax");
            }
            self.emit_epilogue();
            self.emit_line("  ret");
        }
    }

    fn emit_line(&mut self, line: &str) {
        self.buffer.push_str(line);
        self.buffer.push('\n');
    }

    fn emit_loc(&mut self, location: SourceLocation) {
        let file_no = if location.file_no == 0 {
            1
        } else {
            location.file_no
        };
        self.emit_line(&format!("  .loc {} {}", file_no, location.line));
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

    fn pushld(&mut self) {
        self.emit_line("  sub $16, %rsp");
        self.emit_line("  fstpt (%rsp)");
        self.depth += 2;
    }

    fn popf(&mut self, reg: i32) {
        self.emit_line(&format!("  movsd (%rsp), %xmm{}", reg));
        self.emit_line("  add $8, %rsp");
        self.depth -= 1;
    }

    fn label_symbol(&self, function: &Obj, label: &str) -> String {
        format!(".L.{}.{}", self.asm_symbol(&function.name), label)
    }

    fn next_label(&mut self) -> usize {
        self.label_counter += 1;
        self.label_counter
    }

    fn push_struct(&mut self, ty: &Type) {
        let size = ty.size();
        let aligned = align_to(size, 8);
        self.emit_line(&format!("  sub ${}, %rsp", aligned));
        self.depth += (aligned / 8) as usize;

        for i in 0..size {
            self.emit_line(&format!("  mov {}(%rax), %r10b", i));
            self.emit_line(&format!("  mov %r10b, {}(%rsp)", i));
        }
    }

    fn classify_args(&self, args: &[Expr], reserve_gp: bool) -> Vec<ArgInfo> {
        let mut gp = if reserve_gp { 1 } else { 0 };
        let mut fp = 0;
        let mut infos = Vec::with_capacity(args.len());

        for arg in args {
            let ty = arg.ty.as_ref().unwrap_or(&Type::Int);
            match ty {
                Type::Struct { .. } | Type::Union { .. } => {
                    if ty.size() > 16 {
                        let slots = (align_to(ty.size(), 8) / 8) as usize;
                        infos.push(ArgInfo {
                            pass_by_stack: true,
                            reg_classes: Vec::new(),
                            stack_slots: slots,
                        });
                        continue;
                    }

                    let fp1 = has_flonum1(ty);
                    let fp2 = has_flonum2(ty);
                    let fp_needed = fp1 as usize + fp2 as usize;
                    let gp_needed = (!fp1) as usize + (!fp2) as usize;

                    if fp + fp_needed < FP_MAX && gp + gp_needed < GP_MAX {
                        fp += fp_needed;
                        gp += gp_needed;
                        let mut reg_classes = Vec::new();
                        reg_classes.push(if fp1 { ArgClass::Fp } else { ArgClass::Gp });
                        if ty.size() > 8 {
                            reg_classes.push(if fp2 { ArgClass::Fp } else { ArgClass::Gp });
                        }
                        infos.push(ArgInfo {
                            pass_by_stack: false,
                            reg_classes,
                            stack_slots: 0,
                        });
                    } else {
                        let slots = (align_to(ty.size(), 8) / 8) as usize;
                        infos.push(ArgInfo {
                            pass_by_stack: true,
                            reg_classes: Vec::new(),
                            stack_slots: slots,
                        });
                    }
                }
                Type::Float | Type::Double => {
                    if fp >= FP_MAX {
                        infos.push(ArgInfo {
                            pass_by_stack: true,
                            reg_classes: Vec::new(),
                            stack_slots: 1,
                        });
                    } else {
                        fp += 1;
                        infos.push(ArgInfo {
                            pass_by_stack: false,
                            reg_classes: vec![ArgClass::Fp],
                            stack_slots: 0,
                        });
                    }
                }
                Type::LDouble => {
                    infos.push(ArgInfo {
                        pass_by_stack: true,
                        reg_classes: Vec::new(),
                        stack_slots: 2,
                    });
                }
                _ => {
                    if gp >= GP_MAX {
                        infos.push(ArgInfo {
                            pass_by_stack: true,
                            reg_classes: Vec::new(),
                            stack_slots: 1,
                        });
                    } else {
                        gp += 1;
                        infos.push(ArgInfo {
                            pass_by_stack: false,
                            reg_classes: vec![ArgClass::Gp],
                            stack_slots: 0,
                        });
                    }
                }
            }
        }

        infos
    }

    fn push_args_inner(
        &mut self,
        args: &[Expr],
        arg_info: &[ArgInfo],
        function: &Obj,
        globals: &[Obj],
        push_stack: bool,
    ) {
        for (arg, info) in args.iter().zip(arg_info).rev() {
            if info.pass_by_stack != push_stack {
                continue;
            }
            self.gen_expr(arg, function, globals);
            if let Some(ty) = &arg.ty {
                match ty {
                    Type::Struct { .. } | Type::Union { .. } => self.push_struct(ty),
                    Type::Float | Type::Double => self.pushf(),
                    Type::LDouble => self.pushld(),
                    _ => self.push(),
                }
            } else {
                self.push();
            }
        }
    }

    fn push_args(
        &mut self,
        args: &[Expr],
        arg_info: &[ArgInfo],
        ret_buffer: Option<usize>,
        sret: bool,
        function: &Obj,
        globals: &[Obj],
    ) -> usize {
        let mut stack: usize = arg_info.iter().map(|info| info.stack_slots).sum();
        if (self.depth + stack) % 2 == 1 {
            self.emit_line("  sub $8, %rsp");
            self.depth += 1;
            stack += 1;
        }

        self.push_args_inner(args, arg_info, function, globals, true);
        self.push_args_inner(args, arg_info, function, globals, false);

        if sret && let Some(idx) = ret_buffer {
            let offset = function.locals[idx].offset;
            self.emit_line(&format!("  lea {}(%rbp), %rax", offset));
            self.push();
        }
        stack
    }

    fn copy_ret_buffer(&mut self, var: &Obj) {
        let ty = &var.ty;
        let mut gp = 0;
        let mut fp = 0;

        if has_flonum1(ty) {
            if ty.size() == 4 {
                self.emit_line(&format!("  movss %xmm0, {}(%rbp)", var.offset));
            } else {
                self.emit_line(&format!("  movsd %xmm0, {}(%rbp)", var.offset));
            }
            fp += 1;
        } else {
            for i in 0..std::cmp::min(8, ty.size()) {
                self.emit_line(&format!("  mov %al, {}(%rbp)", var.offset + i as i32));
                self.emit_line("  shr $8, %rax");
            }
            gp += 1;
        }

        if ty.size() > 8 {
            if has_flonum2(ty) {
                let reg = fp;
                if ty.size() == 12 {
                    self.emit_line(&format!("  movss %xmm{}, {}(%rbp)", reg, var.offset + 8));
                } else {
                    self.emit_line(&format!("  movsd %xmm{}, {}(%rbp)", reg, var.offset + 8));
                }
            } else {
                let (reg8, reg64) = if gp == 0 {
                    ("%al", "%rax")
                } else {
                    ("%dl", "%rdx")
                };
                for i in 8..std::cmp::min(16, ty.size()) {
                    self.emit_line(&format!("  mov {}, {}(%rbp)", reg8, var.offset + i as i32));
                    self.emit_line(&format!("  shr $8, {}", reg64));
                }
            }
        }
    }

    fn copy_struct_reg(&mut self, ty: &Type) {
        let size = ty.size();
        let mut gp = 0;
        let mut fp = 0;

        self.emit_line("  mov %rax, %rdi");

        if has_flonum1(ty) {
            if size == 4 {
                self.emit_line("  movss (%rdi), %xmm0");
            } else {
                self.emit_line("  movsd (%rdi), %xmm0");
            }
            fp += 1;
        } else {
            self.emit_line("  mov $0, %rax");
            let first = std::cmp::min(8, size) as i32;
            for i in (0..first).rev() {
                self.emit_line("  shl $8, %rax");
                self.emit_line(&format!("  mov {}(%rdi), %al", i));
            }
            gp += 1;
        }

        if size > 8 {
            if has_flonum2(ty) {
                if size == 12 {
                    self.emit_line(&format!("  movss 8(%rdi), %xmm{}", fp));
                } else {
                    self.emit_line(&format!("  movsd 8(%rdi), %xmm{}", fp));
                }
            } else {
                let (reg8, reg64) = if gp == 0 {
                    ("%al", "%rax")
                } else {
                    ("%dl", "%rdx")
                };
                self.emit_line(&format!("  mov $0, {}", reg64));
                let end = std::cmp::min(16, size) as i32;
                for i in (8..end).rev() {
                    self.emit_line(&format!("  shl $8, {}", reg64));
                    self.emit_line(&format!("  mov {}(%rdi), {}", i, reg8));
                }
            }
        }
    }

    fn copy_struct_mem(&mut self, ty: &Type, function: &Obj) {
        let Some(var) = function.params.first() else {
            return;
        };

        self.emit_line(&format!("  mov {}(%rbp), %rdi", var.offset));
        let size = ty.size();
        for i in 0..size {
            self.emit_line(&format!("  mov {}(%rax), %dl", i));
            self.emit_line(&format!("  mov %dl, {}(%rdi)", i));
        }
    }

    fn gen_stmt(&mut self, stmt: &Stmt, function: &Obj, globals: &[Obj]) {
        self.emit_loc(stmt.location);
        match &stmt.kind {
            StmtKind::Return(expr) => {
                if let Some(expr) = expr {
                    self.gen_expr(expr, function, globals);
                    if let Some(ty) = &expr.ty
                        && matches!(ty, Type::Struct { .. } | Type::Union { .. })
                    {
                        if ty.size() <= 16 {
                            self.copy_struct_reg(ty);
                        } else {
                            self.copy_struct_mem(ty, function);
                        }
                    }
                }
                self.emit_epilogue();
                self.emit_line("  ret");
            }
            StmtKind::If { cond, then, els } => {
                let label = self.next_label();
                self.gen_expr(cond, function, globals);
                let cond_ty = cond.ty.as_ref().unwrap_or(&Type::Int);
                self.cmp_zero(cond_ty);
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
                    let cond_ty = cond.ty.as_ref().unwrap_or(&Type::Int);
                    self.cmp_zero(cond_ty);
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
                let cond_ty = cond.ty.as_ref().unwrap_or(&Type::Int);
                self.cmp_zero(cond_ty);
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
                let ax = if use_64 { "%rax" } else { "%eax" };
                let di = if use_64 { "%rdi" } else { "%edi" };

                for case in cases {
                    if case.begin == case.end {
                        self.emit_line(&format!("  cmp ${}, {}", case.begin, ax));
                        self.emit_line(&format!("  je {}", case.label));
                        continue;
                    }

                    // [GNU] Case ranges.
                    self.emit_line(&format!("  mov {}, {}", ax, di));
                    self.emit_line(&format!("  sub ${}, {}", case.begin, di));
                    self.emit_line(&format!("  cmp ${}, {}", case.end - case.begin, di));
                    self.emit_line(&format!("  jbe {}", case.label));
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
            StmtKind::GotoExpr { target } => {
                self.gen_expr(target, function, globals);
                self.emit_line("  jmp *%rax");
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
            StmtKind::Asm(text) => {
                self.emit_line(&format!("  {text}"));
            }
            StmtKind::Block(stmts) => {
                for stmt in stmts {
                    self.gen_stmt(stmt, function, globals);
                }
            }
            StmtKind::Expr(expr) => {
                self.gen_expr(expr, function, globals);
                if expr
                    .ty
                    .as_ref()
                    .is_some_and(|ty| matches!(ty, Type::LDouble))
                {
                    self.emit_line("  fstp %st(0)");
                }
            }
            StmtKind::Decl(_) => {}
        }
    }

    fn builtin_alloca(&mut self, function: &Obj) {
        // Align size to 16 bytes
        self.emit_line("  add $15, %rdi");
        self.emit_line("  and $0xfffffff0, %edi");

        // Shift the temporary area by %rdi
        let alloca_bottom_idx = function.alloca_bottom.expect("alloca_bottom should exist");
        let alloca_bottom = &function.locals[alloca_bottom_idx];
        self.emit_line(&format!("  mov {}(%rbp), %rcx", alloca_bottom.offset));
        self.emit_line("  sub %rsp, %rcx");
        self.emit_line("  mov %rsp, %rax");
        self.emit_line("  sub %rdi, %rsp");
        self.emit_line("  mov %rsp, %rdx");
        self.emit_line("1:");
        self.emit_line("  cmp $0, %rcx");
        self.emit_line("  je 2f");
        self.emit_line("  mov (%rax), %r8b");
        self.emit_line("  mov %r8b, (%rdx)");
        self.emit_line("  inc %rdx");
        self.emit_line("  inc %rax");
        self.emit_line("  dec %rcx");
        self.emit_line("  jmp 1b");
        self.emit_line("2:");

        // Move alloca_bottom pointer
        self.emit_line(&format!("  mov {}(%rbp), %rax", alloca_bottom.offset));
        self.emit_line("  sub %rdi, %rax");
        self.emit_line(&format!("  mov %rax, {}(%rbp)", alloca_bottom.offset));
    }

    fn gen_expr(&mut self, expr: &Expr, function: &Obj, globals: &[Obj]) {
        self.emit_loc(expr.location);
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
                        Type::LDouble => {
                            let bits = fval.to_bits();
                            self.emit_line(&format!(
                                "  mov ${}, %rax  # long double {}",
                                bits, fval
                            ));
                            self.emit_line("  mov %rax, -8(%rsp)");
                            self.emit_line("  fldl -8(%rsp)");
                        }
                        _ => {
                            self.emit_line(&format!("  mov ${}, %rax", value));
                        }
                    }
                } else {
                    self.emit_line(&format!("  mov ${}, %rax", value));
                }
            }
            ExprKind::LabelVal { label } => {
                let symbol = self.label_symbol(function, label);
                self.emit_line(&format!("  lea {}(%rip), %rax", symbol));
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
                            Type::LDouble => {
                                self.emit_line("  fchs");
                            }
                            _ => {
                                self.emit_line("  neg %rax");
                            }
                        }
                    }
                    UnaryOp::Not => {
                        let ty = expr.ty.as_ref().unwrap_or(&Type::Int);
                        self.cmp_zero(ty);
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
            ExprKind::Call {
                callee,
                args,
                ret_buffer,
            } => {
                // Check if this is a call to alloca
                if let ExprKind::Var { idx, is_local } = &callee.kind
                    && !is_local
                {
                    let var = &globals[*idx];
                    if var.name == "alloca" && args.len() == 1 {
                        // Generate the argument and put it in %rdi
                        self.gen_expr(&args[0], function, globals);
                        self.emit_line("  mov %rax, %rdi");
                        self.builtin_alloca(function);
                        return;
                    }
                }

                let sret =
                    ret_buffer.is_some() && expr.ty.as_ref().is_some_and(|ty| ty.size() > 16);
                let arg_info = self.classify_args(args, sret);
                let stack_args =
                    self.push_args(args, &arg_info, *ret_buffer, sret, function, globals);
                self.gen_expr(callee, function, globals);

                let mut gp = 0;
                let mut fp = 0;
                if sret {
                    self.pop(ARG_REGS_64[gp]);
                    gp += 1;
                }
                for info in &arg_info {
                    if info.pass_by_stack {
                        continue;
                    }
                    for class in &info.reg_classes {
                        match class {
                            ArgClass::Fp => {
                                self.popf(fp);
                                fp += 1;
                            }
                            ArgClass::Gp => {
                                self.pop(ARG_REGS_64[gp]);
                                gp += 1;
                            }
                        }
                    }
                }

                self.emit_line("  mov %rax, %r10");
                self.emit_line(&format!("  mov ${}, %rax", fp));
                self.emit_line("  call *%r10");
                if stack_args > 0 {
                    self.emit_line(&format!("  add ${}, %rsp", stack_args * 8));
                    self.depth -= stack_args;
                }

                if let Some(ret_idx) = *ret_buffer {
                    if expr.ty.as_ref().is_some_and(|ty| ty.size() <= 16) {
                        let var = &function.locals[ret_idx];
                        self.copy_ret_buffer(var);
                    }
                    let offset = function.locals[ret_idx].offset;
                    self.emit_line(&format!("  lea {}(%rbp), %rax", offset));
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
            ExprKind::VlaPtr { idx, is_local } => {
                // VlaPtr loads the address of the pointer variable
                if *is_local {
                    let offset = function.locals[*idx].offset;
                    self.emit_line(&format!("  lea {}(%rbp), %rax", offset));
                } else {
                    let obj = &globals[*idx];
                    let symbol = self.asm_symbol(&obj.name);
                    self.emit_line(&format!("  lea {}(%rip), %rax", symbol));
                }
                self.load(expr.ty.as_ref());
            }
            ExprKind::Member { member, .. } => {
                self.gen_lvalue(expr, function, globals);
                self.load(expr.ty.as_ref());

                if member.is_bitfield {
                    // Extract bitfield bits using shifts
                    self.emit_line(&format!(
                        "  shl ${}, %rax",
                        64 - member.bit_width - member.bit_offset
                    ));
                    if member.ty.is_unsigned() {
                        self.emit_line(&format!("  shr ${}, %rax", 64 - member.bit_width));
                    } else {
                        self.emit_line(&format!("  sar ${}, %rax", 64 - member.bit_width));
                    }
                }
            }
            ExprKind::StmtExpr(stmts) => {
                for (idx, stmt) in stmts.iter().enumerate() {
                    let is_last = idx + 1 == stmts.len();
                    if is_last && let StmtKind::Expr(expr) = &stmt.kind {
                        self.gen_expr(expr, function, globals);
                        continue;
                    }
                    self.gen_stmt(stmt, function, globals);
                }
            }
            ExprKind::Assign { lhs, rhs } => {
                self.gen_lvalue(lhs, function, globals);
                self.push();
                self.gen_expr(rhs, function, globals);

                let mut restore_rax = false;
                // Handle bitfield assignment
                if let ExprKind::Member { member, .. } = &lhs.kind
                    && member.is_bitfield
                {
                    self.emit_line("  mov %rax, %r8");
                    restore_rax = true;

                    // Mask and position the new value
                    self.emit_line("  mov %rax, %rdi");
                    let mask = (1i64 << member.bit_width) - 1;
                    self.emit_line(&format!("  and ${}, %rdi", mask));
                    self.emit_line(&format!("  shl ${}, %rdi", member.bit_offset));

                    // Load current value from memory
                    self.emit_line("  mov (%rsp), %rax");
                    self.load(Some(&member.ty));

                    // Mask out the bitfield bits and OR in new value
                    let bit_mask = mask << member.bit_offset;
                    self.emit_line(&format!("  mov ${}, %r9", !bit_mask));
                    self.emit_line("  and %r9, %rax");
                    self.emit_line("  or %rdi, %rax");
                }

                self.store(expr.ty.as_ref());
                if restore_rax {
                    self.emit_line("  mov %r8, %rax");
                }
            }
            ExprKind::Cond { cond, then, els } => {
                let label = self.next_label();
                self.gen_expr(cond, function, globals);
                let cond_ty = cond.ty.as_ref().unwrap_or(&Type::Int);
                self.cmp_zero(cond_ty);
                self.emit_line(&format!("  je .L.else.{}", label));
                self.gen_expr(then, function, globals);
                self.emit_line(&format!("  jmp .L.end.{}", label));
                self.emit_line(&format!(".L.else.{}:", label));
                self.gen_expr(els, function, globals);
                self.emit_line(&format!(".L.end.{}:", label));
            }
            ExprKind::Comma { lhs, rhs } => {
                self.gen_expr(lhs, function, globals);
                if lhs
                    .ty
                    .as_ref()
                    .is_some_and(|ty| matches!(ty, Type::LDouble))
                {
                    self.emit_line("  fstp %st(0)");
                }
                self.gen_expr(rhs, function, globals);
            }
            ExprKind::Binary { op, lhs, rhs } => {
                // Handle short-circuit operators specially
                if matches!(op, BinaryOp::LogAnd | BinaryOp::LogOr) {
                    let c = self.next_label();
                    if matches!(op, BinaryOp::LogAnd) {
                        // Logical AND: false if either operand is false
                        self.gen_expr(lhs, function, globals);
                        let lhs_ty = lhs.ty.as_ref().unwrap_or(&Type::Int);
                        self.cmp_zero(lhs_ty);
                        self.emit_line(&format!("  je .L.false.{}", c));
                        self.gen_expr(rhs, function, globals);
                        let rhs_ty = rhs.ty.as_ref().unwrap_or(&Type::Int);
                        self.cmp_zero(rhs_ty);
                        self.emit_line(&format!("  je .L.false.{}", c));
                        self.emit_line("  mov $1, %rax");
                        self.emit_line(&format!("  jmp .L.end.{}", c));
                        self.emit_line(&format!(".L.false.{}:", c));
                        self.emit_line("  mov $0, %rax");
                        self.emit_line(&format!(".L.end.{}:", c));
                    } else {
                        // Logical OR: true if either operand is true
                        self.gen_expr(lhs, function, globals);
                        let lhs_ty = lhs.ty.as_ref().unwrap_or(&Type::Int);
                        self.cmp_zero(lhs_ty);
                        self.emit_line(&format!("  jne .L.true.{}", c));
                        self.gen_expr(rhs, function, globals);
                        let rhs_ty = rhs.ty.as_ref().unwrap_or(&Type::Int);
                        self.cmp_zero(rhs_ty);
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
                        self.popf(1);

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
                    if matches!(lhs_ty, Type::LDouble) {
                        self.gen_expr(lhs, function, globals);
                        self.gen_expr(rhs, function, globals);

                        match op {
                            BinaryOp::Add => {
                                self.emit_line("  faddp");
                                return;
                            }
                            BinaryOp::Sub => {
                                self.emit_line("  fsubrp");
                                return;
                            }
                            BinaryOp::Mul => {
                                self.emit_line("  fmulp");
                                return;
                            }
                            BinaryOp::Div => {
                                self.emit_line("  fdivrp");
                                return;
                            }
                            BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le => {
                                self.emit_line("  fcomip");
                                self.emit_line("  fstp %st(0)");

                                match op {
                                    BinaryOp::Eq => self.emit_line("  sete %al"),
                                    BinaryOp::Ne => self.emit_line("  setne %al"),
                                    BinaryOp::Lt => self.emit_line("  seta %al"),
                                    BinaryOp::Le => self.emit_line("  setae %al"),
                                    _ => unreachable!(),
                                }

                                self.emit_line("  movzb %al, %rax");
                                return;
                            }
                            _ => {}
                        }
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
            ExprKind::Cas { addr, old, new } => {
                // Generate: __builtin_compare_and_swap(addr, old, new)
                // This atomically compares *addr with *old, and if equal, swaps in new
                // Returns 1 if successful, 0 otherwise
                // If not equal, *old is updated with the actual value at *addr

                // Evaluate addr and push
                self.gen_expr(addr, function, globals);
                self.push();

                // Evaluate new and push
                self.gen_expr(new, function, globals);
                self.push();

                // Evaluate old and save pointer in r8
                self.gen_expr(old, function, globals);
                self.emit_line("  mov %rax, %r8");

                // Load the value pointed to by old into rax
                if let Some(Type::Ptr(base)) = &old.ty {
                    self.load(Some(base));
                }

                // Pop new into rdx, addr into rdi
                self.pop("%rdx");
                self.pop("%rdi");

                // Get the size of the value being compared
                let sz = if let Some(Type::Ptr(base)) = &addr.ty {
                    base.size()
                } else {
                    8
                };

                // Use the appropriate register size for rdx and rax
                let reg_dx = match sz {
                    1 => "%dl",
                    2 => "%dx",
                    4 => "%edx",
                    _ => "%rdx",
                };

                let reg_ax = match sz {
                    1 => "%al",
                    2 => "%ax",
                    4 => "%eax",
                    _ => "%rax",
                };

                // Atomic compare-and-swap
                self.emit_line(&format!("  lock cmpxchg {}, (%rdi)", reg_dx));

                // Set %cl to 1 if successful (ZF=1)
                self.emit_line("  sete %cl");

                // If successful (ZF=1), skip updating *old
                self.emit_line("  je 1f");

                // If failed, store actual value into *old
                self.emit_line(&format!("  mov {}, (%r8)", reg_ax));

                self.emit_line("1:");

                // Return result (0 or 1) in %rax
                self.emit_line("  movzbl %cl, %eax");
            }
            ExprKind::Exch { addr, val } => {
                // Generate: __builtin_atomic_exchange(addr, val)
                // This atomically swaps *addr with val and returns the old value

                // Evaluate addr and push
                self.gen_expr(addr, function, globals);
                self.push();

                // Evaluate val into rax
                self.gen_expr(val, function, globals);

                // Pop addr into rdi
                self.pop("%rdi");

                // Get the size of the value being exchanged
                let sz = if let Some(Type::Ptr(base)) = &addr.ty {
                    base.size()
                } else {
                    8
                };

                // Use the appropriate register size for rax
                let reg_ax = match sz {
                    1 => "%al",
                    2 => "%ax",
                    4 => "%eax",
                    _ => "%rax",
                };

                // Atomic exchange using xchg
                // xchg automatically has lock semantics (no lock prefix needed)
                self.emit_line(&format!("  xchg {}, (%rdi)", reg_ax));

                // Result (old value) is now in %rax
            }
        }
    }

    fn type_id(&self, ty: &Type) -> usize {
        match ty {
            Type::Bool => 0,     // I8
            Type::Char => 0,     // I8
            Type::Short => 1,    // I16
            Type::Int => 2,      // I32
            Type::Enum => 2,     // I32
            Type::Long => 3,     // I64
            Type::UChar => 4,    // U8
            Type::UShort => 5,   // U16
            Type::UInt => 6,     // U32
            Type::ULong => 7,    // U64
            Type::Float => 8,    // F32
            Type::Double => 9,   // F64
            Type::LDouble => 10, // F80
            _ => 7,              // U64
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
        const I32F80: &str = "mov %eax, -4(%rsp); fildl -4(%rsp)";

        const U32F32: &str = "mov %eax, %eax; cvtsi2ssq %rax, %xmm0";
        const U32I64: &str = "mov %eax, %eax";
        const U32F64: &str = "mov %eax, %eax; cvtsi2sdq %rax, %xmm0";
        const U32F80: &str = "mov %eax, %eax; mov %rax, -8(%rsp); fildll -8(%rsp)";

        const I64F32: &str = "cvtsi2ssq %rax, %xmm0";
        const I64F64: &str = "cvtsi2sdq %rax, %xmm0";
        const I64F80: &str = "movq %rax, -8(%rsp); fildll -8(%rsp)";

        const U64F32: &str = "cvtsi2ssq %rax, %xmm0";
        const U64F64: &str = "test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2sd %rax,%xmm0; jmp 2f; 1: mov %rax,%rdi; and $1,%eax; pxor %xmm0,%xmm0; shr %rdi; or %rax,%rdi; cvtsi2sd %rdi,%xmm0; addsd %xmm0,%xmm0; 2:";
        const U64F80: &str = "mov %rax, -8(%rsp); fildq -8(%rsp); test %rax, %rax; jns 1f; mov $1602224128, %eax; mov %eax, -4(%rsp); fadds -4(%rsp); 1:";

        const F32I8: &str = "cvttss2sil %xmm0, %eax; movsbl %al, %eax";
        const F32U8: &str = "cvttss2sil %xmm0, %eax; movzbl %al, %eax";
        const F32I16: &str = "cvttss2sil %xmm0, %eax; movswl %ax, %eax";
        const F32U16: &str = "cvttss2sil %xmm0, %eax; movzwl %ax, %eax";
        const F32I32: &str = "cvttss2sil %xmm0, %eax";
        const F32U32: &str = "cvttss2siq %xmm0, %rax";
        const F32I64: &str = "cvttss2siq %xmm0, %rax";
        const F32U64: &str = "cvttss2siq %xmm0, %rax";
        const F32F64: &str = "cvtss2sd %xmm0, %xmm0";
        const F32F80: &str = "movss %xmm0, -4(%rsp); flds -4(%rsp)";

        const F64I8: &str = "cvttsd2sil %xmm0, %eax; movsbl %al, %eax";
        const F64U8: &str = "cvttsd2sil %xmm0, %eax; movzbl %al, %eax";
        const F64I16: &str = "cvttsd2sil %xmm0, %eax; movswl %ax, %eax";
        const F64U16: &str = "cvttsd2sil %xmm0, %eax; movzwl %ax, %eax";
        const F64I32: &str = "cvttsd2sil %xmm0, %eax";
        const F64U32: &str = "cvttsd2siq %xmm0, %rax";
        const F64F32: &str = "cvtsd2ss %xmm0, %xmm0";
        const F64I64: &str = "cvttsd2siq %xmm0, %rax";
        const F64U64: &str = "cvttsd2siq %xmm0, %rax";
        const F64F80: &str = "movsd %xmm0, -8(%rsp); fldl -8(%rsp)";

        const F80I8: &str = "fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; mov %ax, -12(%rsp); fldcw -12(%rsp); fistps -24(%rsp); fldcw -10(%rsp); movsbl -24(%rsp), %eax";
        const F80U8: &str = "fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; mov %ax, -12(%rsp); fldcw -12(%rsp); fistps -24(%rsp); fldcw -10(%rsp); movzbl -24(%rsp), %eax";
        const F80I16: &str = "fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; mov %ax, -12(%rsp); fldcw -12(%rsp); fistps -24(%rsp); fldcw -10(%rsp); movzbl -24(%rsp), %eax";
        const F80U16: &str = "fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; mov %ax, -12(%rsp); fldcw -12(%rsp); fistpl -24(%rsp); fldcw -10(%rsp); movswl -24(%rsp), %eax";
        const F80I32: &str = "fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; mov %ax, -12(%rsp); fldcw -12(%rsp); fistpl -24(%rsp); fldcw -10(%rsp); mov -24(%rsp), %eax";
        const F80U32: &str = "fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; mov %ax, -12(%rsp); fldcw -12(%rsp); fistpl -24(%rsp); fldcw -10(%rsp); mov -24(%rsp), %eax";
        const F80I64: &str = "fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; mov %ax, -12(%rsp); fldcw -12(%rsp); fistpq -24(%rsp); fldcw -10(%rsp); mov -24(%rsp), %rax";
        const F80U64: &str = "fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; mov %ax, -12(%rsp); fldcw -12(%rsp); fistpq -24(%rsp); fldcw -10(%rsp); mov -24(%rsp), %rax";
        const F80F32: &str = "fstps -8(%rsp); movss -8(%rsp), %xmm0";
        const F80F64: &str = "fstpl -8(%rsp); movsd -8(%rsp), %xmm0";

        #[rustfmt::skip]
        const CAST_TABLE: [[Option<&str>; 11]; 11] = [
            // i8            i16            i32            i64            u8             u16            u32            u64            f32            f64            f80
            [None,          None,          None,          Some(I32I64),  Some(I32U8),   Some(I32U16),  None,          Some(I32I64),  Some(I32F32),  Some(I32F64),  Some(I32F80)], // i8
            [Some(I32I8),   None,          None,          Some(I32I64),  Some(I32U8),   Some(I32U16),  None,          Some(I32I64),  Some(I32F32),  Some(I32F64),  Some(I32F80)], // i16
            [Some(I32I8),   Some(I32I16),  None,          Some(I32I64),  Some(I32U8),   Some(I32U16),  None,          Some(I32I64),  Some(I32F32),  Some(I32F64),  Some(I32F80)], // i32
            [Some(I32I8),   Some(I32I16),  None,          None,          Some(I32U8),   Some(I32U16),  None,          None,          Some(I64F32),  Some(I64F64),  Some(I64F80)], // i64

            [Some(I32I8),   None,          None,          Some(I32I64),  None,          None,          None,          Some(I32I64),  Some(I32F32),  Some(I32F64),  Some(I32F80)], // u8
            [Some(I32I8),   Some(I32I16),  None,          Some(I32I64),  Some(I32U8),   None,          None,          Some(I32I64),  Some(I32F32),  Some(I32F64),  Some(I32F80)], // u16
            [Some(I32I8),   Some(I32I16),  None,          Some(U32I64),  Some(I32U8),   Some(I32U16),  None,          Some(U32I64),  Some(U32F32),  Some(U32F64),  Some(U32F80)], // u32
            [Some(I32I8),   Some(I32I16),  None,          None,          Some(I32U8),   Some(I32U16),  None,          None,          Some(U64F32),  Some(U64F64),  Some(U64F80)], // u64

            [Some(F32I8),   Some(F32I16),  Some(F32I32),  Some(F32I64),  Some(F32U8),   Some(F32U16),  Some(F32U32),  Some(F32U64),  None,          Some(F32F64),  Some(F32F80)], // f32
            [Some(F64I8),   Some(F64I16),  Some(F64I32),  Some(F64I64),  Some(F64U8),   Some(F64U16),  Some(F64U32),  Some(F64U64),  Some(F64F32),  None,          Some(F64F80)], // f64
            [Some(F80I8),   Some(F80I16),  Some(F80I32),  Some(F80I64),  Some(F80U8),   Some(F80U16),  Some(F80U32),  Some(F80U64),  Some(F80F32),  Some(F80F64),  None        ], // f80
        ];

        let t1 = self.type_id(from);
        let t2 = self.type_id(to);
        if let Some(insn) = CAST_TABLE[t1][t2] {
            self.emit_line(&format!("  {}", insn));
        }
    }

    fn cmp_zero(&mut self, ty: &Type) {
        match ty {
            Type::Float => {
                self.emit_line("  xorps %xmm1, %xmm1");
                self.emit_line("  ucomiss %xmm1, %xmm0");
                return;
            }
            Type::Double => {
                self.emit_line("  xorpd %xmm1, %xmm1");
                self.emit_line("  ucomisd %xmm1, %xmm0");
                return;
            }
            Type::LDouble => {
                self.emit_line("  fldz");
                self.emit_line("  fucomip");
                self.emit_line("  fstp %st(0)");
                return;
            }
            _ => {}
        }

        if ty.is_integer() && ty.size() <= 4 {
            self.emit_line("  cmp $0, %eax");
        } else {
            self.emit_line("  cmp $0, %rax");
        }
    }

    fn load(&mut self, ty: Option<&Type>) {
        if let Some(ty) = ty {
            match ty {
                // If it is an array, struct, or union, do not attempt to load a value to the
                // register because in general we can't load an entire aggregate to a
                // register. As a result, the result of an evaluation of an array/struct/union
                // becomes not the value itself but the address.
                // This is where "array is automatically converted to a pointer to
                // the first element of the array in C" occurs.
                // VLA is also treated as an aggregate - we want the pointer, not the value.
                Type::Array { .. }
                | Type::Vla { .. }
                | Type::Struct { .. }
                | Type::Union { .. }
                | Type::Func { .. } => return,
                Type::Float => {
                    self.emit_line("  movss (%rax), %xmm0");
                    return;
                }
                Type::Double => {
                    self.emit_line("  movsd (%rax), %xmm0");
                    return;
                }
                Type::LDouble => {
                    self.emit_line("  fldt (%rax)");
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
                _ => panic!("Cannot load type: {:?}", ty),
            }
        }
    }

    fn store(&mut self, ty: Option<&Type>) {
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
                Type::LDouble => {
                    self.emit_line("  fstpt (%rdi)");
                    self.emit_line("  fldt (%rdi)");
                    return;
                }
                Type::Vla { .. } => {
                    // VLA variables are pointers, store as 8 bytes
                    self.emit_line("  mov %rax, (%rdi)");
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
                _ => panic!("Cannot store type: {:?}", ty),
            }
        }
    }

    fn emit_epilogue(&mut self) {
        self.emit_line("  mov %rbp, %rsp");
        self.emit_line("  pop %rbp");
    }

    fn gen_addr(&mut self, idx: usize, is_local: bool, function: &Obj, globals: &[Obj]) {
        if is_local {
            let var = &function.locals[idx];
            // Variable-length array, which is always local
            if matches!(var.ty, Type::Vla { .. }) {
                // For VLA, the variable holds a pointer, so we load it
                self.emit_line(&format!("  mov {}(%rbp), %rax", var.offset));
                return;
            }
            let offset = var.offset;
            self.emit_line(&format!("  lea {}(%rbp), %rax", offset));
            return;
        }
        let obj = &globals[idx];
        let symbol = self.asm_symbol(&obj.name);

        if get_opt_fpic() {
            // Thread-local variable
            if obj.is_tls {
                self.emit_line(&format!("  data16 lea {}@tlsgd(%rip), %rdi", symbol));
                self.emit_line("  .value 0x6666");
                self.emit_line("  rex64");
                self.emit_line("  call __tls_get_addr@PLT");
                return;
            }

            // Function or global variable
            self.emit_line(&format!("  mov {}@GOTPCREL(%rip), %rax", symbol));
            return;
        }

        // Thread-local variable
        if obj.is_tls {
            self.emit_line("  mov %fs:0, %rax");
            self.emit_line(&format!("  add ${}@tpoff, %rax", symbol));
            return;
        }

        if obj.is_function {
            if obj.is_definition {
                self.emit_line(&format!("  lea {}(%rip), %rax", symbol));
            } else {
                self.emit_line(&format!("  mov {}@GOTPCREL(%rip), %rax", symbol));
            }
            return;
        }
        self.emit_line(&format!("  lea {}(%rip), %rax", symbol));
    }

    fn gen_lvalue(&mut self, expr: &Expr, function: &Obj, globals: &[Obj]) {
        match &expr.kind {
            ExprKind::Var { idx, is_local } => self.gen_addr(*idx, *is_local, function, globals),
            ExprKind::VlaPtr { idx, is_local } => {
                // VlaPtr: for VLA designator in assignment, emit lea to get address of the pointer variable
                if *is_local {
                    let offset = function.locals[*idx].offset;
                    self.emit_line(&format!("  lea {}(%rbp), %rax", offset));
                } else {
                    let obj = &globals[*idx];
                    let symbol = self.asm_symbol(&obj.name);
                    self.emit_line(&format!("  lea {}(%rip), %rax", symbol));
                }
            }
            ExprKind::Deref(expr) => self.gen_expr(expr, function, globals),
            ExprKind::Comma { lhs, rhs } => {
                self.gen_expr(lhs, function, globals);
                self.gen_lvalue(rhs, function, globals);
            }
            ExprKind::Member { lhs, member } => {
                self.gen_lvalue(lhs, function, globals);
                self.emit_line(&format!("  add ${}, %rax", member.offset));
            }
            ExprKind::Assign { .. } | ExprKind::Cond { .. } => {
                if let Some(ty) = expr.ty.as_ref()
                    && matches!(ty, Type::Struct { .. } | Type::Union { .. })
                {
                    self.gen_expr(expr, function, globals);
                } else {
                    self.emit_line("  mov $0, %rax");
                }
            }
            ExprKind::Call {
                ret_buffer: Some(_),
                ..
            } => {
                self.gen_expr(expr, function, globals);
            }
            _ => self.emit_line("  mov $0, %rax"),
        }
    }
}
