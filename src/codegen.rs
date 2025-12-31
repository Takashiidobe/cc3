use crate::ast::{Expr, Program, Stmt};
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
    }
}
