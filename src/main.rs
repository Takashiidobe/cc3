mod ast;
mod codegen;
mod error;
mod lexer;
mod parser;

use clap::Parser;
use colored::Colorize;
use std::{fs, io, path::Path, path::PathBuf, process::Command};

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
    /// Emit assembly instead of object code.
    #[arg(short = 'S', action = clap::ArgAction::SetTrue)]
    emit_asm: bool,
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

    if let Err(err) = run_driver(&args) {
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

fn run_subprocess(argv: &[String], show_cmd: bool) -> io::Result<()> {
    if show_cmd {
        eprintln!("{}", argv.join(" "));
    }

    let status = Command::new(&argv[0]).args(&argv[1..]).status()?;
    if !status.success() {
        std::process::exit(1);
    }

    Ok(())
}

fn run_cc1_subprocess(input: &Path, output: &Path, show_cmd: bool) -> io::Result<()> {
    let exe = std::env::args()
        .next()
        .ok_or_else(|| io::Error::other("missing argv[0]"))?;
    let argv = vec![
        exe,
        "-cc1".to_string(),
        input.display().to_string(),
        "-o".to_string(),
        output.display().to_string(),
    ];
    run_subprocess(&argv, show_cmd)
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

fn run_driver(args: &Args) -> io::Result<()> {
    let output = match &args.output {
        Some(path) => path.clone(),
        None => replace_ext(&args.input, if args.emit_asm { ".s" } else { ".o" }),
    };

    if args.emit_asm {
        return run_cc1_subprocess(&args.input, &output, args.hash_hash_hash);
    }

    let tmp_asm = create_tmpfile("cc3", ".s")?;
    run_cc1_subprocess(&args.input, &tmp_asm, args.hash_hash_hash)?;
    assemble(&tmp_asm, &output, args.hash_hash_hash)?;
    let _ = fs::remove_file(&tmp_asm);
    Ok(())
}

fn replace_ext(input: &Path, ext: &str) -> PathBuf {
    let stem = input.file_stem().and_then(|s| s.to_str()).unwrap_or("out");
    PathBuf::from(format!("{stem}{ext}"))
}

fn create_tmpfile(prefix: &str, ext: &str) -> io::Result<PathBuf> {
    use std::fs::OpenOptions;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let base = std::env::temp_dir();
    for _ in 0..100 {
        let n = COUNTER.fetch_add(1, Ordering::Relaxed);
        let name = format!("{prefix}-{}-{n}{ext}", std::process::id());
        let path = base.join(name);
        match OpenOptions::new().write(true).create_new(true).open(&path) {
            Ok(_) => return Ok(path),
            Err(err) if err.kind() == io::ErrorKind::AlreadyExists => continue,
            Err(err) => return Err(err),
        }
    }
    Err(io::Error::new(
        io::ErrorKind::AlreadyExists,
        "failed to create temp file",
    ))
}

fn assemble(input: &Path, output: &Path, show_cmd: bool) -> io::Result<()> {
    let argv = vec![
        "as".to_string(),
        "-c".to_string(),
        input.display().to_string(),
        "-o".to_string(),
        output.display().to_string(),
    ];
    run_subprocess(&argv, show_cmd)
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
