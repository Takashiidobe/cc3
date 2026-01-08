mod ast;
mod codegen;
mod error;
mod lexer;
mod parser;
mod preprocessor;

use clap::Parser;
use colored::Colorize;
use std::{fs, io, path::Path, path::PathBuf, process::Command};

use crate::error::{CompileError, CompileResult};
use crate::lexer::{Token, TokenKind};

#[derive(Parser, Debug)]
#[command(name = "cc3")]
#[command(about = "A tiny C compiler in Rust")]
struct Args {
    /// Run cc1 (compiler proper).
    #[arg(long = "cc1", action = clap::ArgAction::SetTrue, hide = true)]
    cc1: bool,
    /// Input for cc1 mode.
    #[arg(long = "cc1-input", hide = true)]
    cc1_input: Option<PathBuf>,
    /// Output for cc1 mode.
    #[arg(long = "cc1-output", hide = true)]
    cc1_output: Option<PathBuf>,
    /// Print subprocess command lines.
    #[arg(long = "hash-hash-hash", action = clap::ArgAction::SetTrue, hide = true)]
    hash_hash_hash: bool,
    /// Compile and assemble only; do not link.
    #[arg(short = 'c', action = clap::ArgAction::SetTrue)]
    compile_only: bool,
    /// Emit assembly instead of object code.
    #[arg(short = 'S', action = clap::ArgAction::SetTrue)]
    emit_asm: bool,
    /// Preprocess only.
    #[arg(short = 'E', action = clap::ArgAction::SetTrue)]
    preprocess_only: bool,
    /// Input C source file(s).
    inputs: Vec<PathBuf>,
    /// Output file. Writes to stdout if omitted in cc1 mode.
    #[arg(short = 'o', long = "output")]
    output: Option<PathBuf>,
    /// Add include search path.
    #[arg(short = 'I', value_name = "DIR")]
    include_dirs: Vec<PathBuf>,
    /// Define macro.
    #[arg(short = 'D', value_name = "MACRO[=VAL]")]
    defines: Vec<String>,
    /// Undefine macro.
    #[arg(short = 'U', value_name = "MACRO")]
    undefs: Vec<String>,
}

fn parse_define(s: &str) -> (String, String) {
    if let Some(eq_pos) = s.find('=') {
        let name = s[..eq_pos].to_string();
        let value = s[eq_pos + 1..].to_string();
        (name, value)
    } else {
        (s.to_string(), "1".to_string())
    }
}

fn main() {
    let raw_args: Vec<String> = std::env::args().collect();
    let argv0 = raw_args
        .first()
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("cc3"));
    let args = Args::parse_from(preprocess_args(&raw_args));

    let cmdline_defines: Vec<(String, String)> =
        args.defines.iter().map(|s| parse_define(s)).collect();
    let cmdline_undefs: Vec<String> = args.undefs.clone();

    if args.cc1 {
        let mut include_paths = args.include_dirs.clone();
        include_paths.extend(default_include_paths(&argv0));
        preprocessor::set_include_paths(include_paths);
        let input = match args.cc1_input.as_ref().or_else(|| args.inputs.first()) {
            Some(path) => path,
            None => {
                eprintln!("error: no input files");
                std::process::exit(1);
            }
        };
        let output = args
            .cc1_output
            .as_ref()
            .or(args.output.as_ref())
            .map(|path| path.as_path());
        if let Err(err) = run_cc1(
            input,
            output,
            args.preprocess_only,
            cmdline_defines,
            cmdline_undefs,
        ) {
            eprintln!("{}", format_diagnostic(&err, input.as_path()));
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
            "-cc1-input" => "--cc1-input".to_string(),
            "-cc1-output" => "--cc1-output".to_string(),
            "-###" => "--hash-hash-hash".to_string(),
            _ => arg.clone(),
        })
        .collect()
}

fn default_include_paths(argv0: &Path) -> Vec<PathBuf> {
    let mut paths = Vec::new();
    let base_dir = argv0.parent().unwrap_or(Path::new("."));
    paths.push(base_dir.join("include"));
    paths.push(PathBuf::from("/usr/local/include"));
    paths.push(PathBuf::from("/usr/include/x86_64-linux-gnu"));
    paths.push(PathBuf::from("/usr/include"));
    paths
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

fn run_cc1_subprocess(
    input: &Path,
    output: Option<&Path>,
    preprocess_only: bool,
    include_dirs: &[PathBuf],
    defines: &[String],
    undefs: &[String],
    show_cmd: bool,
) -> io::Result<()> {
    let exe = std::env::args()
        .next()
        .ok_or_else(|| io::Error::other("missing argv[0]"))?;
    let mut argv = vec![exe, "-cc1".to_string()];
    if preprocess_only {
        argv.push("-E".to_string());
    }
    argv.push("-cc1-input".to_string());
    argv.push(input.display().to_string());
    if let Some(output) = output {
        argv.push("-cc1-output".to_string());
        argv.push(output.display().to_string());
    }
    for dir in include_dirs {
        argv.push(format!("-I{}", dir.display()));
    }
    for def in defines {
        argv.push(format!("-D{}", def));
    }
    for undef in undefs {
        argv.push(format!("-U{}", undef));
    }
    run_subprocess(&argv, show_cmd)
}

fn run_cc1(
    input: &Path,
    output: Option<&Path>,
    preprocess_only: bool,
    cmdline_defines: Vec<(String, String)>,
    cmdline_undefs: Vec<String>,
) -> CompileResult<()> {
    let tokens = lexer::tokenize_file(input)?;
    let tokens = preprocessor::preprocess(tokens, cmdline_defines, cmdline_undefs)?;
    if preprocess_only {
        print_tokens(&tokens, output)?;
        return Ok(());
    }
    let program = parser::parse(&tokens)?;
    let asm = codegen::Codegen::new().generate(&program);

    if let Some(path) = output {
        fs::write(path, asm).map_err(|err| {
            CompileError::new(format!("failed to write {}: {err}", path.display()))
        })?;
    } else {
        let mut stdout = io::stdout();
        use io::Write;
        stdout
            .write_all(asm.as_bytes())
            .map_err(|err| CompileError::new(format!("failed to write stdout: {err}")))?;
    }

    Ok(())
}

fn print_tokens(tokens: &[Token], output: Option<&Path>) -> CompileResult<()> {
    use std::io::Write;

    let mut writer: Box<dyn Write> = if let Some(path) = output {
        Box::new(fs::File::create(path).map_err(|err| {
            CompileError::new(format!("failed to write {}: {err}", path.display()))
        })?)
    } else {
        Box::new(io::stdout())
    };

    let mut line = 1;
    for tok in tokens {
        if matches!(tok.kind, TokenKind::Eof) {
            break;
        }
        if line > 1 && tok.at_bol {
            writeln!(writer)
                .map_err(|err| CompileError::new(format!("failed to write output: {err}")))?;
        }
        let text = token_lexeme(tok);
        write!(writer, " {text}")
            .map_err(|err| CompileError::new(format!("failed to write output: {err}")))?;
        line += 1;
    }
    writeln!(writer).map_err(|err| CompileError::new(format!("failed to write output: {err}")))?;
    Ok(())
}

fn token_lexeme(token: &Token) -> String {
    if let Some(file) = lexer::get_input_file(token.location.file_no) {
        let bytes = file.contents.as_bytes();
        let start = token.location.byte;
        let end = start.saturating_add(token.len);
        if end <= bytes.len() {
            return String::from_utf8_lossy(&bytes[start..end]).into_owned();
        }
    }
    match &token.kind {
        TokenKind::Ident(name) => name.clone(),
        TokenKind::Punct(punct) => punct.to_string(),
        TokenKind::Keyword(_) => "<keyword>".to_string(),
        TokenKind::Num { .. } => "<number>".to_string(),
        TokenKind::PPNum => "<pp-number>".to_string(),
        TokenKind::Str { .. } => "<string>".to_string(),
        TokenKind::Eof => String::new(),
    }
}

fn run_driver(args: &Args) -> io::Result<()> {
    if args.inputs.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "no input files",
        ));
    }
    if args.inputs.len() > 1
        && args.output.is_some()
        && (args.compile_only || args.emit_asm || args.preprocess_only)
    {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "cannot specify '-o' with '-c', '-S' or '-E' with multiple files",
        ));
    }

    let mut link_inputs: Vec<PathBuf> = Vec::new();
    let mut temp_objects: Vec<PathBuf> = Vec::new();

    for input in &args.inputs {
        let output = if args.preprocess_only {
            args.output.clone()
        } else if let Some(path) = &args.output {
            Some(path.clone())
        } else {
            Some(replace_ext(input, if args.emit_asm { ".s" } else { ".o" }))
        };

        if args.preprocess_only {
            if !is_c_like_file(input) && !is_stdin(input) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("unknown file extension: {}", input.display()),
                ));
            }
            run_cc1_subprocess(
                input,
                output.as_deref(),
                true,
                &args.include_dirs,
                &args.defines,
                &args.undefs,
                args.hash_hash_hash,
            )?;
            continue;
        }

        if is_object_file(input) {
            if !args.compile_only && !args.emit_asm {
                link_inputs.push(input.clone());
            }
            continue;
        }

        if is_asm_file(input) {
            if !args.emit_asm {
                assemble(input, output.as_ref().unwrap(), args.hash_hash_hash)?;
            }
            continue;
        }

        if !is_c_like_file(input) && !is_stdin(input) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("unknown file extension: {}", input.display()),
            ));
        }

        if args.emit_asm {
            run_cc1_subprocess(
                input,
                output.as_deref(),
                false,
                &args.include_dirs,
                &args.defines,
                &args.undefs,
                args.hash_hash_hash,
            )?;
            continue;
        }

        if args.compile_only {
            let tmp_asm = create_tmpfile("cc3", ".s")?;
            run_cc1_subprocess(
                input,
                Some(&tmp_asm),
                false,
                &args.include_dirs,
                &args.defines,
                &args.undefs,
                args.hash_hash_hash,
            )?;
            assemble(&tmp_asm, output.as_ref().unwrap(), args.hash_hash_hash)?;
            let _ = fs::remove_file(&tmp_asm);
            continue;
        }

        let tmp_asm = create_tmpfile("cc3", ".s")?;
        let tmp_obj = create_tmpfile("cc3", ".o")?;
        run_cc1_subprocess(
            input,
            Some(&tmp_asm),
            false,
            &args.include_dirs,
            &args.defines,
            &args.undefs,
            args.hash_hash_hash,
        )?;
        assemble(&tmp_asm, &tmp_obj, args.hash_hash_hash)?;
        let _ = fs::remove_file(&tmp_asm);
        link_inputs.push(tmp_obj.clone());
        temp_objects.push(tmp_obj);
    }

    if !link_inputs.is_empty() && !args.compile_only && !args.emit_asm {
        let output = args
            .output
            .clone()
            .unwrap_or_else(|| PathBuf::from("a.out"));
        run_linker(&link_inputs, &output, args.hash_hash_hash)?;
    }

    for path in temp_objects {
        let _ = fs::remove_file(path);
    }
    Ok(())
}

fn replace_ext(input: &Path, ext: &str) -> PathBuf {
    let stem = input.file_stem().and_then(|s| s.to_str()).unwrap_or("out");
    PathBuf::from(format!("{stem}{ext}"))
}

fn is_object_file(path: &Path) -> bool {
    matches!(path.extension().and_then(|s| s.to_str()), Some("o"))
}

fn is_asm_file(path: &Path) -> bool {
    matches!(path.extension().and_then(|s| s.to_str()), Some("s"))
}

fn is_c_like_file(path: &Path) -> bool {
    matches!(
        path.extension().and_then(|s| s.to_str()),
        Some("c") | Some("i")
    )
}

fn is_stdin(path: &Path) -> bool {
    path == Path::new("-")
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

fn run_linker(inputs: &[PathBuf], output: &Path, show_cmd: bool) -> io::Result<()> {
    let mut argv = link_command();
    argv.push("-o".to_string());
    argv.push(output.display().to_string());
    for input in inputs {
        argv.push(input.display().to_string());
    }
    run_subprocess(&argv, show_cmd)
}

fn link_command() -> Vec<String> {
    if cfg!(target_arch = "x86_64") {
        vec!["clang".to_string()]
    } else {
        vec![
            "zig".to_string(),
            "cc".to_string(),
            "--target=x86_64-linux-musl".to_string(),
            "-static".to_string(),
        ]
    }
}

fn format_diagnostic(err: &CompileError, path: &std::path::Path) -> String {
    let header = format!("{}: {}", "error".red().bold(), err.message().bold());
    let Some(location) = err.location() else {
        return header;
    };

    let (source_path, source) = if let Some(file) = lexer::get_input_file(location.file_no) {
        (file.name, file.contents)
    } else {
        (
            path.to_path_buf(),
            fs::read_to_string(path).unwrap_or_default(),
        )
    };
    let line_text = source.lines().nth(location.line.saturating_sub(1));
    let width = location.line.to_string().len().max(3);

    let mut out = String::new();
    out.push_str(&header);
    out.push('\n');
    out.push_str(&format!(
        "  --> {}:{}:{}\n",
        source_path.display(),
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
