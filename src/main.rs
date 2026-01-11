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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FileType {
    None,
    C,
    Asm,
    Obj,
    Ar,
    Dso,
}

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
    /// Print dependency list for make.
    #[arg(short = 'M', action = clap::ArgAction::SetTrue)]
    dep_only: bool,
    /// Write dependency output to a .d file while compiling.
    #[arg(long = "MD", action = clap::ArgAction::SetTrue)]
    dep_generate: bool,
    /// Write dependency output to a .d file without system headers.
    #[arg(long = "MMD", action = clap::ArgAction::SetTrue)]
    dep_generate_no_std: bool,
    /// Add phony targets for dependencies.
    #[arg(long = "MP", action = clap::ArgAction::SetTrue)]
    dep_phony: bool,
    /// Specify dependency rule target.
    #[arg(long = "MT", value_name = "TARGET", action = clap::ArgAction::Append)]
    dep_targets: Vec<String>,
    /// Write dependency output to file.
    #[arg(long = "MF", value_name = "FILE")]
    dep_output: Option<PathBuf>,
    /// Input C source file(s).
    inputs: Vec<PathBuf>,
    /// Output file. Writes to stdout if omitted in cc1 mode.
    #[arg(short = 'o', long = "output")]
    output: Option<PathBuf>,
    /// Add include search path.
    #[arg(short = 'I', value_name = "DIR")]
    include_dirs: Vec<PathBuf>,
    /// Add library search path.
    #[arg(short = 'L', value_name = "DIR")]
    library_dirs: Vec<PathBuf>,
    /// Link with library.
    #[arg(short = 'l', value_name = "LIB", action = clap::ArgAction::Append)]
    libraries: Vec<String>,
    /// Pass options to the linker.
    #[arg(long = "Wl", value_name = "OPTS", action = clap::ArgAction::Append)]
    linker_opts: Vec<String>,
    /// Pass a single option to the linker.
    #[arg(
        long = "Xlinker",
        value_name = "OPT",
        action = clap::ArgAction::Append,
        allow_hyphen_values = true
    )]
    linker_args: Vec<String>,
    /// Add include search path (searched after -I paths).
    #[arg(long = "idirafter", value_name = "DIR")]
    idirafter_dirs: Vec<PathBuf>,
    /// Include file before processing main input.
    #[arg(long = "include", value_name = "FILE")]
    include_files: Vec<PathBuf>,
    /// Define macro.
    #[arg(short = 'D', value_name = "MACRO[=VAL]")]
    defines: Vec<String>,
    /// Undefine macro.
    #[arg(short = 'U', value_name = "MACRO")]
    undefs: Vec<String>,
    /// Emit common symbols (default).
    #[arg(long = "fcommon", action = clap::ArgAction::SetTrue, hide = true)]
    fcommon: bool,
    /// Do not emit common symbols.
    #[arg(long = "fno-common", action = clap::ArgAction::SetTrue, hide = true)]
    fno_common: bool,
    /// Generate position independent code.
    #[arg(long = "fpic", action = clap::ArgAction::SetTrue, aliases = ["fPIC"])]
    fpic: bool,
    /// Specify language for input files (can be specified multiple times, last one wins).
    #[arg(short = 'x', value_name = "LANG")]
    languages: Vec<String>,
    /// Strip symbols from executable.
    #[arg(short = 's', action = clap::ArgAction::SetTrue)]
    strip_symbols: bool,
    /// Link statically.
    #[arg(long = "static", action = clap::ArgAction::SetTrue)]
    static_link: bool,
    /// Produce a shared object.
    #[arg(long = "shared", action = clap::ArgAction::SetTrue)]
    shared: bool,
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

fn parse_language(s: &str) -> io::Result<FileType> {
    match s {
        "c" => Ok(FileType::C),
        "assembler" => Ok(FileType::Asm),
        "none" => Ok(FileType::None),
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("unknown argument for -x: {s}"),
        )),
    }
}

fn get_file_type(path: &Path, opt_x: FileType) -> io::Result<FileType> {
    // If -x is specified and not "none", use it
    if opt_x != FileType::None {
        return Ok(opt_x);
    }

    // Determine from extension
    if let Some(ext) = path.extension() {
        match ext.to_str() {
            Some("a") => return Ok(FileType::Ar),
            Some("so") => return Ok(FileType::Dso),
            Some("o") => return Ok(FileType::Obj),
            Some("c") => return Ok(FileType::C),
            Some("s") | Some("S") => return Ok(FileType::Asm),
            _ => {}
        }
    }

    // Check for stdin
    if path.as_os_str() == "-" {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "unknown file type for stdin (use -x to specify)",
        ));
    }

    Err(io::Error::new(
        io::ErrorKind::InvalidInput,
        format!("unknown file extension: {}", path.display()),
    ))
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

    // Handle -fcommon/-fno-common flags
    if args.fno_common {
        codegen::set_opt_fcommon(false);
    } else if args.fcommon {
        codegen::set_opt_fcommon(true);
    }
    if args.fpic {
        codegen::set_opt_fpic(true);
    }

    if args.cc1 {
        let std_include_paths = default_include_paths(&argv0);
        let mut include_paths = args.include_dirs.clone();
        include_paths.extend(std_include_paths.clone());
        include_paths.extend(args.idirafter_dirs.clone());
        preprocessor::set_include_paths(include_paths);
        let dep_generate = args.dep_generate || args.dep_generate_no_std;
        let dep_exclude_std = args.dep_generate_no_std;
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
            args.dep_only,
            dep_generate,
            args.dep_phony,
            &args.dep_targets,
            args.dep_output.as_deref(),
            dep_exclude_std,
            &std_include_paths,
            cmdline_defines,
            cmdline_undefs,
            args.include_files.clone(),
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
    let mut out = Vec::with_capacity(args.len());
    let mut i = 0;

    while i < args.len() {
        let arg = &args[i];
        let arg_str = arg.as_str();

        // These options are ignored for now
        if arg_str.starts_with("-O")
            || arg_str.starts_with("-W")
            || arg_str.starts_with("-g")
            || arg_str.starts_with("-std=")
            || arg_str == "-ffreestanding"
            || arg_str == "-fno-builtin"
            || arg_str == "-fno-omit-frame-pointer"
            || arg_str == "-fno-stack-protector"
            || arg_str == "-fno-strict-aliasing"
            || arg_str == "-m64"
            || arg_str == "-mno-red-zone"
            || arg_str == "-w"
        {
            i += 1;
            continue;
        }

        if let Some(value) = arg_str.strip_prefix("-Wl,") {
            out.push("--Wl".to_string());
            out.push(value.to_string());
            i += 1;
            continue;
        }

        if arg_str.starts_with("-l") && arg_str.len() > 2 {
            out.push("-l".to_string());
            out.push(arg_str[2..].to_string());
            i += 1;
            continue;
        }

        match arg_str {
            "-cc1" => out.push("--cc1".to_string()),
            "-cc1-input" => out.push("--cc1-input".to_string()),
            "-cc1-output" => out.push("--cc1-output".to_string()),
            "-###" => out.push("--hash-hash-hash".to_string()),
            "-fcommon" => out.push("--fcommon".to_string()),
            "-fno-common" => out.push("--fno-common".to_string()),
            "-fpic" => out.push("--fpic".to_string()),
            "-fPIC" => out.push("--fpic".to_string()),
            "-static" => out.push("--static".to_string()),
            "-shared" => out.push("--shared".to_string()),
            "-L" => out.push("-L".to_string()),
            "-l" => out.push("-l".to_string()),
            "-Wl" => out.push("--Wl".to_string()),
            "-Xlinker" => out.push("--Xlinker".to_string()),
            "-MF" => out.push("--MF".to_string()),
            "-MD" => out.push("--MD".to_string()),
            "-MMD" => out.push("--MMD".to_string()),
            "-MP" => out.push("--MP".to_string()),
            "-MT" => out.push("--MT".to_string()),
            "-MQ" | "--MQ" => {
                out.push("--MT".to_string());
                if i + 1 < args.len() {
                    out.push(quote_makefile(&args[i + 1]));
                    i += 1;
                }
            }
            _ if arg_str.starts_with("--MQ=") => {
                let value = &arg_str["--MQ=".len()..];
                out.push(format!("--MT={}", quote_makefile(value)));
            }
            _ => out.push(arg.clone()),
        }

        i += 1;
    }

    out
}

fn default_include_paths(argv0: &Path) -> Vec<PathBuf> {
    let mut paths = Vec::new();
    let base_dir = argv0.parent().unwrap_or(Path::new("."));
    paths.push(base_dir.join("include"));
    paths.push(PathBuf::from("/usr/local/include"));
    paths.push(PathBuf::from("/usr/include/x86_64-linux-gnu"));
    paths.push(PathBuf::from("/usr/include"));
    paths.extend(probe_toolchain_includes());
    paths
}

fn probe_toolchain_includes() -> Vec<PathBuf> {
    let mut paths = Vec::new();

    if let Ok(out) = Command::new("clang").arg("-print-resource-dir").output()
        && out.status.success()
    {
        let dir = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if !dir.is_empty() {
            paths.push(PathBuf::from(dir).join("include"));
        }
    }

    for arg in ["-print-file-name=include", "-print-file-name=include-fixed"] {
        if let Ok(out) = Command::new("gcc").arg(arg).output()
            && out.status.success()
        {
            let dir = String::from_utf8_lossy(&out.stdout).trim().to_string();
            if !dir.is_empty() && dir != "include" && dir != "include-fixed" {
                paths.push(PathBuf::from(dir));
            }
        }
    }

    paths.retain(|path| path.is_dir());
    paths.sort();
    paths.dedup();
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

#[allow(clippy::too_many_arguments)]
fn run_cc1_subprocess(
    input: &Path,
    output: Option<&Path>,
    preprocess_only: bool,
    dep_only: bool,
    dep_generate: bool,
    dep_exclude_std: bool,
    dep_output: Option<&Path>,
    dep_phony: bool,
    dep_targets: &[String],
    include_dirs: &[PathBuf],
    idirafter_dirs: &[PathBuf],
    include_files: &[PathBuf],
    defines: &[String],
    undefs: &[String],
    fcommon: bool,
    fno_common: bool,
    fpic: bool,
    show_cmd: bool,
) -> io::Result<()> {
    let exe = std::env::args()
        .next()
        .ok_or_else(|| io::Error::other("missing argv[0]"))?;
    let mut argv = vec![exe, "-cc1".to_string()];
    if preprocess_only {
        argv.push("-E".to_string());
    }
    if dep_only {
        argv.push("-M".to_string());
    }
    if dep_exclude_std {
        argv.push("--MMD".to_string());
    } else if dep_generate {
        argv.push("--MD".to_string());
    }
    if let Some(path) = dep_output {
        argv.push("--MF".to_string());
        argv.push(path.display().to_string());
    }
    if dep_phony {
        argv.push("--MP".to_string());
    }
    for target in dep_targets {
        argv.push("--MT".to_string());
        argv.push(target.clone());
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
    for dir in idirafter_dirs {
        argv.push(format!("--idirafter={}", dir.display()));
    }
    for file in include_files {
        argv.push(format!("--include={}", file.display()));
    }
    for def in defines {
        argv.push(format!("-D{}", def));
    }
    for undef in undefs {
        argv.push(format!("-U{}", undef));
    }
    if fno_common {
        argv.push("-fno-common".to_string());
    } else if fcommon {
        argv.push("-fcommon".to_string());
    }
    if fpic {
        argv.push("-fpic".to_string());
    }
    run_subprocess(&argv, show_cmd)
}

#[allow(clippy::too_many_arguments)]
fn run_cc1(
    input: &Path,
    output: Option<&Path>,
    preprocess_only: bool,
    dep_only: bool,
    dep_generate: bool,
    dep_phony: bool,
    dep_targets: &[String],
    dep_output: Option<&Path>,
    dep_exclude_std: bool,
    std_include_paths: &[PathBuf],
    cmdline_defines: Vec<(String, String)>,
    cmdline_undefs: Vec<String>,
    include_files: Vec<PathBuf>,
) -> CompileResult<()> {
    // Process -include files first
    let mut tokens = Vec::new();
    for include_file in &include_files {
        let path = if include_file.exists() {
            include_file.clone()
        } else {
            preprocessor::search_include_paths(include_file.to_str().unwrap()).ok_or_else(|| {
                CompileError::new(format!(
                    "-include: {}: {}",
                    include_file.display(),
                    io::Error::from(io::ErrorKind::NotFound)
                ))
            })?
        };
        let mut include_tokens = lexer::tokenize_file(&path)?;
        // Remove EOF from include tokens to allow appending
        if let Some(last) = include_tokens.last()
            && matches!(last.kind, TokenKind::Eof)
        {
            include_tokens.pop();
        }
        tokens.extend(include_tokens);
    }

    // Then process main input file
    let mut main_tokens = lexer::tokenize_file(input)?;
    tokens.append(&mut main_tokens);

    let tokens = preprocessor::preprocess(tokens, cmdline_defines, cmdline_undefs)?;
    let mut dep_output = dep_output.map(PathBuf::from);
    if dep_output.is_none() && dep_generate {
        let base = output.unwrap_or(input);
        dep_output = Some(replace_ext(base, ".d"));
    }
    if dep_only || dep_generate {
        print_dependencies(
            input,
            dep_targets,
            dep_output.as_deref(),
            output,
            dep_phony,
            dep_exclude_std,
            std_include_paths,
        )?;
        if dep_only {
            return Ok(());
        }
    }
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

fn print_dependencies(
    input: &Path,
    dep_targets: &[String],
    dep_output: Option<&Path>,
    output: Option<&Path>,
    dep_phony: bool,
    dep_exclude_std: bool,
    std_include_paths: &[PathBuf],
) -> CompileResult<()> {
    use std::io::Write;

    let mut writer: Box<dyn Write> = if let Some(path) = dep_output.or(output) {
        Box::new(fs::File::create(path).map_err(|err| {
            CompileError::new(format!("failed to write {}: {err}", path.display()))
        })?)
    } else {
        Box::new(io::stdout())
    };

    let target = if dep_targets.is_empty() {
        quote_makefile(&replace_ext(input, ".o").display().to_string())
    } else {
        dep_targets.join(" ")
    };
    write!(writer, "{}:", target)
        .map_err(|err| CompileError::new(format!("failed to write output: {err}")))?;

    for file in lexer::get_input_files() {
        if dep_exclude_std && in_std_include_path(&file.name, std_include_paths) {
            continue;
        }
        write!(writer, " \\\n  {}", file.name.display())
            .map_err(|err| CompileError::new(format!("failed to write output: {err}")))?;
    }

    writeln!(writer, "\n")
        .map_err(|err| CompileError::new(format!("failed to write output: {err}")))?;

    if dep_phony {
        for file in lexer::get_input_files().into_iter().skip(1) {
            if dep_exclude_std && in_std_include_path(&file.name, std_include_paths) {
                continue;
            }
            writeln!(
                writer,
                "{}:\n",
                quote_makefile(&file.name.display().to_string())
            )
            .map_err(|err| CompileError::new(format!("failed to write output: {err}")))?;
        }
    }
    Ok(())
}

fn quote_makefile(input: &str) -> String {
    let bytes = input.as_bytes();
    let mut out = String::with_capacity(bytes.len() * 2 + 1);

    for (idx, &byte) in bytes.iter().enumerate() {
        match byte {
            b'$' => out.push_str("$$"),
            b'#' => {
                out.push('\\');
                out.push('#');
            }
            b' ' | b'\t' => {
                let mut k = idx;
                while k > 0 && bytes[k - 1] == b'\\' {
                    out.push('\\');
                    k -= 1;
                }
                out.push('\\');
                out.push(byte as char);
            }
            _ => out.push(byte as char),
        }
    }

    out
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

fn in_std_include_path(path: &Path, std_include_paths: &[PathBuf]) -> bool {
    std_include_paths.iter().any(|dir| {
        path.strip_prefix(dir)
            .map(|rest| !rest.as_os_str().is_empty())
            .unwrap_or(false)
    })
}

fn run_driver(args: &Args) -> io::Result<()> {
    if args.inputs.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "no input files",
        ));
    }
    let dep_generate = args.dep_generate || args.dep_generate_no_std;
    let dep_exclude_std = args.dep_generate_no_std;
    if args.inputs.len() > 1
        && args.output.is_some()
        && (args.compile_only || args.emit_asm || args.preprocess_only || args.dep_only)
    {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "cannot specify '-o' with '-c', '-S', '-E' or '-M' with multiple files",
        ));
    }

    let opt_x = if let Some(lang) = args.languages.last() {
        parse_language(lang)?
    } else if args.preprocess_only || args.dep_only {
        // -E/-M implies -xc (C language)
        FileType::C
    } else {
        FileType::None
    };

    let mut link_inputs: Vec<PathBuf> = Vec::new();
    let mut temp_objects: Vec<PathBuf> = Vec::new();
    let mut linker_args: Vec<String> = Vec::new();
    for lib in &args.libraries {
        linker_args.push(format!("-l{lib}"));
    }
    for arg in &args.linker_args {
        linker_args.push("-Xlinker".to_string());
        linker_args.push(arg.clone());
    }
    for opt in &args.linker_opts {
        if !opt.is_empty() {
            linker_args.push(format!("-Wl,{opt}"));
        }
    }

    for input in &args.inputs {
        // Handle -l library flags
        if let Some(input_str) = input.to_str()
            && input_str.starts_with("-L")
        {
            linker_args.push(input_str.to_string());
            continue;
        }

        if let Some(input_str) = input.to_str()
            && input_str.starts_with("-l")
        {
            linker_args.push(input_str.to_string());
            continue;
        }

        let file_type = get_file_type(input, opt_x)?;

        let output = if args.preprocess_only || args.dep_only {
            args.output.clone()
        } else if let Some(path) = &args.output {
            Some(path.clone())
        } else {
            Some(replace_ext(input, if args.emit_asm { ".s" } else { ".o" }))
        };
        let dep_output = if dep_generate && args.dep_output.is_none() {
            let base = if args.inputs.len() == 1
                && (args.compile_only || args.emit_asm || args.preprocess_only || args.dep_only)
            {
                args.output.clone().unwrap_or_else(|| input.clone())
            } else {
                input.clone()
            };
            Some(replace_ext(&base, ".d"))
        } else {
            args.dep_output.clone()
        };

        // Handle object files, archives, and shared objects
        if file_type == FileType::Obj || file_type == FileType::Ar || file_type == FileType::Dso {
            if !args.compile_only && !args.emit_asm {
                link_inputs.push(input.clone());
            }
            continue;
        }

        // Handle assembly files
        if file_type == FileType::Asm {
            if !args.emit_asm {
                assemble(input, output.as_ref().unwrap(), args.hash_hash_hash)?;
            }
            continue;
        }

        // Handle C files
        assert_eq!(file_type, FileType::C);

        if args.preprocess_only || args.dep_only {
            run_cc1_subprocess(
                input,
                output.as_deref(),
                args.preprocess_only,
                args.dep_only,
                dep_generate,
                dep_exclude_std,
                dep_output.as_deref(),
                args.dep_phony,
                &args.dep_targets,
                &args.include_dirs,
                &args.idirafter_dirs,
                &args.include_files,
                &args.defines,
                &args.undefs,
                args.fcommon,
                args.fno_common,
                args.fpic,
                args.hash_hash_hash,
            )?;
            continue;
        }

        if args.emit_asm {
            run_cc1_subprocess(
                input,
                output.as_deref(),
                false,
                false,
                dep_generate,
                dep_exclude_std,
                dep_output.as_deref(),
                args.dep_phony,
                &args.dep_targets,
                &args.include_dirs,
                &args.idirafter_dirs,
                &args.include_files,
                &args.defines,
                &args.undefs,
                args.fcommon,
                args.fno_common,
                args.fpic,
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
                false,
                dep_generate,
                dep_exclude_std,
                dep_output.as_deref(),
                args.dep_phony,
                &args.dep_targets,
                &args.include_dirs,
                &args.idirafter_dirs,
                &args.include_files,
                &args.defines,
                &args.undefs,
                args.fcommon,
                args.fno_common,
                args.fpic,
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
            false,
            dep_generate,
            dep_exclude_std,
            dep_output.as_deref(),
            args.dep_phony,
            &args.dep_targets,
            &args.include_dirs,
            &args.idirafter_dirs,
            &args.include_files,
            &args.defines,
            &args.undefs,
            args.fcommon,
            args.fno_common,
            args.fpic,
            args.hash_hash_hash,
        )?;
        assemble(&tmp_asm, &tmp_obj, args.hash_hash_hash)?;
        let _ = fs::remove_file(&tmp_asm);
        link_inputs.push(tmp_obj.clone());
        temp_objects.push(tmp_obj);
    }

    if (!link_inputs.is_empty() || !linker_args.is_empty()) && !args.compile_only && !args.emit_asm
    {
        let output = args
            .output
            .clone()
            .unwrap_or_else(|| PathBuf::from("a.out"));
        run_linker(
            &link_inputs,
            &linker_args,
            &output,
            args.strip_symbols,
            args.static_link,
            args.shared,
            &args.library_dirs,
            args.hash_hash_hash,
        )?;
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

#[allow(clippy::too_many_arguments)]
fn run_linker(
    inputs: &[PathBuf],
    linker_args: &[String],
    output: &Path,
    strip_symbols: bool,
    static_link: bool,
    shared: bool,
    library_dirs: &[PathBuf],
    show_cmd: bool,
) -> io::Result<()> {
    let mut argv = link_command();
    if shared {
        argv.push("-shared".to_string());
    }
    if static_link {
        argv.push("-static".to_string());
    }
    argv.push("-o".to_string());
    argv.push(output.display().to_string());
    if strip_symbols {
        argv.push("-s".to_string());
    }
    for input in inputs {
        argv.push(input.display().to_string());
    }
    for dir in library_dirs {
        argv.push(format!("-L{}", dir.display()));
    }
    for arg in linker_args {
        argv.push(arg.clone());
    }
    run_subprocess(&argv, show_cmd)
}

fn link_command() -> Vec<String> {
    if cfg!(target_arch = "x86_64") {
        vec!["cc".to_string()]
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

    let (source_name, source) = if let Some(file) = lexer::get_input_file(location.file_no) {
        (file.display_name, file.contents)
    } else {
        (
            path.display().to_string(),
            fs::read_to_string(path).unwrap_or_default(),
        )
    };
    let line_text = line_at_byte(&source, location.byte);
    let width = location.line.to_string().len().max(3);

    let mut out = String::new();
    out.push_str(&header);
    out.push('\n');
    out.push_str(&format!(
        "  --> {}:{}:{}\n",
        source_name, location.line, location.column
    ));
    out.push_str(&format!("{:>width$} |\n", "", width = width));

    if let Some(text) = line_text {
        out.push_str(&format!(
            "{:>width$} | {}\n",
            location.line,
            text,
            width = width
        ));
        let caret_width = lexer::display_width(text, location.column.saturating_sub(1));
        let caret_pad = " ".repeat(caret_width);
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

fn line_at_byte(contents: &str, byte: usize) -> Option<&str> {
    if contents.is_empty() {
        return None;
    }
    let bytes = contents.as_bytes();
    let pos = byte.min(bytes.len());
    let start = bytes[..pos]
        .iter()
        .rposition(|&b| b == b'\n')
        .map(|idx| idx + 1)
        .unwrap_or(0);
    let end = bytes[pos..]
        .iter()
        .position(|&b| b == b'\n')
        .map(|idx| pos + idx)
        .unwrap_or(bytes.len());
    Some(&contents[start..end])
}
