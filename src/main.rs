mod ast;
mod codegen;
mod error;
mod lexer;
mod parser;

use clap::Parser;
use colored::Colorize;
use std::{fs, io, path::PathBuf};

use crate::error::{CompileError, CompileResult};

#[derive(Parser, Debug)]
#[command(name = "cc3")]
#[command(about = "A tiny C compiler in Rust")]
struct Args {
    /// Input C source file.
    input: PathBuf,
    /// Output assembly file. Writes to stdout if omitted.
    #[arg(short = 'o', long = "output")]
    output: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();
    if let Err(err) = run(&args) {
        eprintln!("{}", format_diagnostic(&err, args.input.as_path()));
        std::process::exit(1);
    }
}

fn run(args: &Args) -> CompileResult<()> {
    let source = fs::read_to_string(&args.input).map_err(|err| {
        CompileError::new(format!("failed to read {}: {err}", args.input.display()))
    })?;

    let tokens = lexer::tokenize(&source)?;
    let program = parser::parse(&tokens)?;
    let mut asm = format!(".file 1 \"{}\"\n", args.input.display());
    asm.push_str(&codegen::Codegen::new().generate(&program));

    match &args.output {
        Some(path) => {
            fs::write(path, asm).map_err(|err| {
                CompileError::new(format!("failed to write {}: {err}", path.display()))
            })?;
        }
        None => {
            let mut stdout = io::stdout();
            use io::Write;
            stdout
                .write_all(asm.as_bytes())
                .map_err(|err| CompileError::new(format!("failed to write stdout: {err}")))?;
        }
    }

    Ok(())
}

fn format_diagnostic(err: &CompileError, path: &std::path::Path) -> String {
    let header = format!("{}: {}", "error".red().bold(), err.message().bold());
    let Some(location) = err.location() else {
        return header;
    };

    let source = fs::read_to_string(path).unwrap_or_default();
    let line_text = source.lines().nth(location.line.saturating_sub(1));
    let width = location.line.to_string().len().max(3);

    let mut out = String::new();
    out.push_str(&header);
    out.push('\n');
    out.push_str(&format!(
        "  --> {}:{}:{}\n",
        path.display(),
        location.line,
        location.column
    ));
    out.push_str(&format!("{:>width$} |\n", "", width = width));

    if let Some(text) = line_text {
        out.push_str(&format!(
            "{:>width$} | {}\n",
            location.line,
            text,
            width = width
        ));
        let caret_pad = " ".repeat(location.column.saturating_sub(1));
        out.push_str(&format!(
            "{:>width$} | {}{}\n",
            "",
            caret_pad,
            "^".red(),
            width = width
        ));
    }

    out
}
