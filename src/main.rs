mod ast;
mod codegen;
mod error;
mod lexer;
mod parser;

use clap::Parser;
use colored::Colorize;
use std::{fs, io, path::PathBuf, process::Command};

use crate::error::{CompileError, CompileResult};

#[derive(Parser, Debug)]
#[command(name = "cc3")]
#[command(about = "A tiny C compiler in Rust")]
struct Args {
    /// Run cc1 (compiler proper).
    #[arg(long = "cc1", action = clap::ArgAction::SetTrue, hide = true)]
    cc1: bool,
    /// Print subprocess command lines.
    #[arg(long = "hash-hash-hash", action = clap::ArgAction::SetTrue, hide = true)]
    hash_hash_hash: bool,
    /// Input C source file.
    input: PathBuf,
    /// Output assembly file. Writes to stdout if omitted.
    #[arg(short = 'o', long = "output")]
    output: Option<PathBuf>,
}

fn main() {
    let raw_args: Vec<String> = std::env::args().collect();
    let args = Args::parse_from(preprocess_args(&raw_args));

    if args.cc1 {
        if let Err(err) = run_cc1(&args) {
            eprintln!("{}", format_diagnostic(&err, args.input.as_path()));
            std::process::exit(1);
        }
        return;
    }

    if let Err(err) = run_cc1_subprocess(&raw_args, args.hash_hash_hash) {
        eprintln!("error: {err}");
        std::process::exit(1);
    }
}

fn preprocess_args(args: &[String]) -> Vec<String> {
    args.iter()
        .map(|arg| match arg.as_str() {
            "-cc1" => "--cc1".to_string(),
            "-###" => "--hash-hash-hash".to_string(),
            _ => arg.clone(),
        })
        .collect()
}

fn run_cc1_subprocess(args: &[String], show_cmd: bool) -> io::Result<()> {
    let mut cmd_args = args.to_vec();
    cmd_args.push("-cc1".to_string());

    if show_cmd {
        eprintln!("{}", cmd_args.join(" "));
    }

    let status = Command::new(&cmd_args[0]).args(&cmd_args[1..]).status()?;

    if !status.success() {
        std::process::exit(1);
    }

    Ok(())
}

fn run_cc1(args: &Args) -> CompileResult<()> {
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
