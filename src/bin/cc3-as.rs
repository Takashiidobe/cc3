// Standalone assembler binary for cc3
// Usage: cc3-as <input.s> [-o output.o]

use std::{env, fs, process};

// Declare the asm module - it's in the parent crate
#[path = "../asm/mod.rs"]
mod asm;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: cc3-as <input.s> [-o output.o]");
        eprintln!("  -o <file>    Output file (default: a.out)");
        eprintln!();
        eprintln!("For Phase 1, outputs parsed AST as YAML to stdout");
        process::exit(1);
    }

    let mut input_file = None;
    let mut _output_file = String::from("a.out");
    let mut i = 1;

    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                if i + 1 >= args.len() {
                    eprintln!("error: -o requires an argument");
                    process::exit(1);
                }
                _output_file = args[i + 1].clone();
                i += 2;
            }
            arg if !arg.starts_with('-') => {
                input_file = Some(arg.to_string());
                i += 1;
            }
            arg => {
                eprintln!("error: unknown option: {}", arg);
                process::exit(1);
            }
        }
    }

    let input_file = match input_file {
        Some(f) => f,
        None => {
            eprintln!("error: no input file specified");
            process::exit(1);
        }
    };

    // Read input file
    let input = match fs::read_to_string(&input_file) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("error: failed to read {}: {}", input_file, err);
            process::exit(1);
        }
    };

    // Lex
    let tokens = match asm::lex(&input) {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("{}:{}:{}: error: {}", input_file, err.line, err.column, err.message);
            process::exit(1);
        }
    };

    // Parse
    let nodes = match asm::parse(tokens) {
        Ok(nodes) => nodes,
        Err(err) => {
            eprintln!("{}:{}:{}: error: {}", input_file, err.line, err.column, err.message);
            process::exit(1);
        }
    };

    // For Phase 2, output parsed AST and section/symbol info.
    // In later phases, this will output an actual object file.
    println!("Parsed {} nodes:", nodes.len());
    for (i, node) in nodes.iter().enumerate() {
        println!("{}: {:?}", i, node);
    }

    let program = match asm::build_program(&nodes) {
        Ok(program) => program,
        Err(err) => {
            eprintln!("{}:{}:{}: error: {}", input_file, err.line, err.column, err.message);
            process::exit(1);
        }
    };

    println!("Sections:");
    for (i, section) in program.sections.iter().enumerate() {
        println!("{}: {:?}", i, section);
    }

    println!("Symbols:");
    for (i, symbol) in program.symbols.iter().enumerate() {
        println!("{}: {:?}", i, symbol);
    }
}
