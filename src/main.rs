mod ast;
mod codegen;
mod error;
mod lexer;
mod parser;

use clap::Parser;
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
    if let Err(err) = run(args) {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

fn run(args: Args) -> CompileResult<()> {
    let source = fs::read_to_string(&args.input).map_err(|err| {
        CompileError::new(format!("failed to read {}: {err}", args.input.display()))
    })?;

    let tokens = lexer::tokenize(&source)?;
    let program = parser::parse(&tokens)?;
    let asm = codegen::generate(&program);

    match args.output {
        Some(path) => {
            fs::write(&path, asm).map_err(|err| {
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
