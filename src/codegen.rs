use crate::ast::{BinaryOp, Expr, Program, Stmt};
use std::fmt::Write;

pub fn generate(program: &Program) -> String {
    let mut out = String::new();
    writeln!(&mut out, "  .globl main").expect("write to string");
    writeln!(&mut out, "main:").expect("write to string");

    for stmt in &program.body {
        gen_stmt(stmt, &mut out);
    }

    out
}

fn gen_stmt(stmt: &Stmt, out: &mut String) {
    match stmt {
        Stmt::Return(expr) => {
            gen_expr(expr, out);
            writeln!(out, "  ret").expect("write to string");
        }
    }
}

fn gen_expr(expr: &Expr, out: &mut String) {
    match expr {
        Expr::Num(value) => {
            writeln!(out, "  mov ${}, %rax", value).expect("write to string");
        }
        Expr::Binary { op, lhs, rhs } => {
            gen_expr(lhs, out);
            writeln!(out, "  push %rax").expect("write to string");
            gen_expr(rhs, out);
            writeln!(out, "  pop %rdi").expect("write to string");
            match op {
                BinaryOp::Add => {
                    writeln!(out, "  add %rdi, %rax").expect("write to string");
                }
                BinaryOp::Sub => {
                    writeln!(out, "  sub %rax, %rdi").expect("write to string");
                    writeln!(out, "  mov %rdi, %rax").expect("write to string");
                }
            }
        }
    }
}
