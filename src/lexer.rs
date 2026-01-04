use crate::ast::Type;
use crate::error::{CompileError, CompileResult, SourceLocation};
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, OnceLock};

#[derive(Debug, Clone)]
pub struct File {
    pub name: PathBuf,
    pub file_no: usize,
    pub contents: String,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct HideSet {
    names: HashSet<String>,
}

impl HideSet {
    pub fn union(&self, other: &Self) -> Self {
        Self {
            names: self.names.union(&other.names).cloned().collect(),
        }
    }

    pub fn intersection(&self, other: &Self) -> Self {
        Self {
            names: self.names.intersection(&other.names).cloned().collect(),
        }
    }

    pub fn contains(&self, name: &str) -> bool {
        self.names.contains(name)
    }

    pub fn add(&mut self, name: impl Into<String>) {
        self.names.insert(name.into());
    }

    pub fn add_tokens(&self, tokens: &mut [Token]) {
        for tok in tokens {
            tok.hideset = tok.hideset.union(self);
        }
    }
}

static INPUT_FILES: OnceLock<Mutex<Vec<File>>> = OnceLock::new();

fn input_files() -> &'static Mutex<Vec<File>> {
    INPUT_FILES.get_or_init(|| Mutex::new(Vec::new()))
}

pub fn get_input_files() -> Vec<File> {
    input_files()
        .lock()
        .map(|files| files.clone())
        .unwrap_or_default()
}

pub fn get_input_file(file_no: usize) -> Option<File> {
    if file_no == 0 {
        return None;
    }
    input_files()
        .lock()
        .ok()
        .and_then(|files| files.get(file_no - 1).cloned())
}

fn register_file(path: PathBuf, contents: String) -> File {
    let mut files = input_files().lock().expect("input files lock poisoned");
    let file_no = files.len() + 1;
    let file = File {
        name: path,
        file_no,
        contents,
    };
    files.push(file.clone());
    file
}

fn remove_backslash_newline(input: &str) -> String {
    let bytes = input.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    let mut pending_newlines = 0;

    while i < bytes.len() {
        if bytes[i] == b'\\' && i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
            i += 2;
            pending_newlines += 1;
            continue;
        }

        if bytes[i] == b'\n' {
            out.push(b'\n');
            i += 1;
            if pending_newlines > 0 {
                out.extend(std::iter::repeat_n(b'\n', pending_newlines));
                pending_newlines = 0;
            }
            continue;
        }

        out.push(bytes[i]);
        i += 1;
    }

    if pending_newlines > 0 {
        out.extend(std::iter::repeat_n(b'\n', pending_newlines));
    }

    String::from_utf8(out).expect("line continuation removal")
}

pub fn tokenize_file(path: &Path) -> CompileResult<Vec<Token>> {
    let contents = fs::read_to_string(path)
        .map_err(|err| CompileError::new(format!("failed to read {}: {err}", path.display())))?;
    let contents = remove_backslash_newline(&contents);
    let file = register_file(path.to_path_buf(), contents);
    tokenize(&file.contents, file.file_no)
}

pub fn tokenize_builtin(name: &str, contents: &str) -> CompileResult<Vec<Token>> {
    let file = register_file(PathBuf::from(name), contents.to_string());
    tokenize(&file.contents, file.file_no)
}

/// Parse hexadecimal floating-point literal (e.g., 0x1.2p3)
/// Format: 0x[hex_digits][.hex_digits]p[+/-][decimal_exponent]
fn parse_hex_float(s: &str) -> Option<f64> {
    let s = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X"))?;

    // Find the 'p' or 'P' that separates mantissa from exponent
    let (mantissa_str, exp_str) = if let Some(pos) = s.find(['p', 'P']) {
        (&s[..pos], &s[pos + 1..])
    } else {
        (s, "0")
    };

    // Parse the mantissa (integer and fractional parts)
    let (int_part, frac_part) = if let Some(dot_pos) = mantissa_str.find('.') {
        (&mantissa_str[..dot_pos], &mantissa_str[dot_pos + 1..])
    } else {
        (mantissa_str, "")
    };

    // Parse integer part as hex
    let int_value = if int_part.is_empty() {
        0.0
    } else {
        u64::from_str_radix(int_part, 16).ok()? as f64
    };

    // Parse fractional part as hex
    let mut frac_value = 0.0;
    if !frac_part.is_empty() {
        let frac_int = u64::from_str_radix(frac_part, 16).ok()? as f64;
        frac_value = frac_int / 16f64.powi(frac_part.len() as i32);
    }

    // Parse binary exponent (power of 2)
    let exponent: i32 = exp_str.parse().ok()?;

    // Combine: (integer + fraction) * 2^exponent
    Some((int_value + frac_value) * 2f64.powi(exponent))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Void,
    Bool,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Enum,
    Struct,
    Union,
    Typedef,
    Static,
    Extern,
    Return,
    If,
    Else,
    For,
    While,
    Do,
    Sizeof,
    Alignof,
    Alignas,
    Goto,
    Break,
    Continue,
    Switch,
    Case,
    Default,
    Signed,
    Unsigned,
    Const,
    Volatile,
    Auto,
    Register,
    Restrict,
    Noreturn,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            Keyword::Void => "void",
            Keyword::Bool => "_Bool",
            Keyword::Char => "char",
            Keyword::Short => "short",
            Keyword::Int => "int",
            Keyword::Long => "long",
            Keyword::Float => "float",
            Keyword::Double => "double",
            Keyword::Enum => "enum",
            Keyword::Struct => "struct",
            Keyword::Union => "union",
            Keyword::Typedef => "typedef",
            Keyword::Static => "static",
            Keyword::Extern => "extern",
            Keyword::Return => "return",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::For => "for",
            Keyword::While => "while",
            Keyword::Do => "do",
            Keyword::Sizeof => "sizeof",
            Keyword::Alignof => "_Alignof",
            Keyword::Alignas => "_Alignas",
            Keyword::Goto => "goto",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::Switch => "switch",
            Keyword::Case => "case",
            Keyword::Default => "default",
            Keyword::Signed => "signed",
            Keyword::Unsigned => "unsigned",
            Keyword::Const => "const",
            Keyword::Volatile => "volatile",
            Keyword::Auto => "auto",
            Keyword::Register => "register",
            Keyword::Restrict => "restrict",
            Keyword::Noreturn => "_Noreturn",
        };
        f.write_str(text)
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum TokenKind {
    Keyword(Keyword),
    Ident(String),
    Num {
        value: i64,
        fval: f64,
        ty: Type,
    },
    Str {
        bytes: Vec<u8>,
        ty: Type,
    },
    Punct(Punct),
    #[default]
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Punct {
    Plus,
    Minus,
    Star,
    Amp,
    Hash,
    HashHash,
    Pipe,
    Caret,
    Slash,
    Mod,
    Backtick,
    Dot,
    Ellipsis,
    Arrow,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Comma,
    Colon,
    Question,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    ShlAssign,
    ShrAssign,
    Inc,
    Dec,
    Not,
    BitNot,
    LogAnd,
    LogOr,
    Shl,
    Shr,
    EqEq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

impl std::fmt::Display for Punct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            Punct::Plus => "+",
            Punct::Minus => "-",
            Punct::Star => "*",
            Punct::Amp => "&",
            Punct::Hash => "#",
            Punct::HashHash => "##",
            Punct::Pipe => "|",
            Punct::Caret => "^",
            Punct::Slash => "/",
            Punct::Mod => "%",
            Punct::Backtick => "`",
            Punct::Dot => ".",
            Punct::Ellipsis => "...",
            Punct::Arrow => "->",
            Punct::LParen => "(",
            Punct::RParen => ")",
            Punct::LBrace => "{",
            Punct::RBrace => "}",
            Punct::LBracket => "[",
            Punct::RBracket => "]",
            Punct::Semicolon => ";",
            Punct::Comma => ",",
            Punct::Colon => ":",
            Punct::Question => "?",
            Punct::Assign => "=",
            Punct::AddAssign => "+=",
            Punct::SubAssign => "-=",
            Punct::MulAssign => "*=",
            Punct::DivAssign => "/=",
            Punct::ModAssign => "%=",
            Punct::AndAssign => "&=",
            Punct::OrAssign => "|=",
            Punct::XorAssign => "^=",
            Punct::ShlAssign => "<<=",
            Punct::ShrAssign => ">>=",
            Punct::Inc => "++",
            Punct::Dec => "--",
            Punct::Not => "!",
            Punct::BitNot => "~",
            Punct::LogAnd => "&&",
            Punct::LogOr => "||",
            Punct::Shl => "<<",
            Punct::Shr => ">>",
            Punct::EqEq => "==",
            Punct::NotEq => "!=",
            Punct::Less => "<",
            Punct::LessEq => "<=",
            Punct::Greater => ">",
            Punct::GreaterEq => ">=",
        };
        f.write_str(text)
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub location: SourceLocation,
    pub at_bol: bool,
    pub has_space: bool, // True if this token follows a space character
    pub len: usize,
    pub hideset: HideSet,               // For macro expansion
    pub origin: Option<SourceLocation>, // Macro expansion origin
}

pub fn tokenize(input: &str, file_no: usize) -> CompileResult<Vec<Token>> {
    let bytes = input.as_bytes();
    let mut tokens = Vec::new();
    let mut i = 0;
    let mut line = 1;
    let mut column = 1;
    let mut at_bol = true;
    let mut has_space = false;

    while i < bytes.len() {
        let b = bytes[i];
        if b == b'/' && i + 1 < bytes.len() {
            if bytes[i + 1] == b'/' {
                i += 2;
                column += 2;
                while i < bytes.len() && bytes[i] != b'\n' {
                    i += 1;
                    column += 1;
                }
                has_space = true;
                continue;
            }
            if bytes[i + 1] == b'*' {
                let start_location = SourceLocation {
                    line,
                    column,
                    byte: i,
                    file_no,
                };
                i += 2;
                column += 2;
                let mut closed = false;
                while i + 1 < bytes.len() {
                    if bytes[i] == b'*' && bytes[i + 1] == b'/' {
                        i += 2;
                        column += 2;
                        closed = true;
                        break;
                    }
                    if bytes[i] == b'\n' {
                        line += 1;
                        column = 1;
                        i += 1;
                        at_bol = true;
                    } else {
                        i += 1;
                        column += 1;
                    }
                }
                if !closed {
                    return Err(CompileError::at("unclosed block comment", start_location));
                }
                has_space = true;
                continue;
            }
        }
        if b == b'\n' {
            line += 1;
            column = 1;
            i += 1;
            at_bol = true;
            has_space = false;
            continue;
        }
        if b.is_ascii_whitespace() {
            column += 1;
            i += 1;
            has_space = true;
            continue;
        }

        // Handle numeric literals (including those starting with '.')
        if b.is_ascii_digit() || (b == b'.' && i + 1 < bytes.len() && bytes[i + 1].is_ascii_digit())
        {
            let start = i;
            let location = SourceLocation {
                line,
                column,
                byte: start,
                file_no,
            };

            // If starts with '.', it's a float
            if b == b'.' {
                let float_str = &input[start..];
                let mut end_idx = 0;
                let mut prev_char = '\0';
                for (idx, c) in float_str.char_indices() {
                    // Allow '+' or '-' only immediately after 'e', 'E', 'p', or 'P'
                    if matches!(c, '+' | '-') {
                        if !matches!(prev_char, 'e' | 'E' | 'p' | 'P') {
                            break;
                        }
                    } else if !matches!(c, '0'..='9' | '.' | 'e' | 'E' | 'f' | 'F' | 'l' | 'L') {
                        break;
                    }
                    end_idx = idx + c.len_utf8();
                    prev_char = c;
                }
                if end_idx == 0 {
                    end_idx = float_str.len();
                }

                let float_literal = &float_str[..end_idx];
                let mut parse_str = float_literal;

                // Check for suffix
                let ty = if float_literal.ends_with('f') || float_literal.ends_with('F') {
                    parse_str = &float_literal[..float_literal.len() - 1];
                    Type::Float
                } else if float_literal.ends_with('l') || float_literal.ends_with('L') {
                    parse_str = &float_literal[..float_literal.len() - 1];
                    Type::Double
                } else {
                    Type::Double
                };

                let fval = if parse_str.starts_with("0x") || parse_str.starts_with("0X") {
                    parse_hex_float(parse_str).ok_or_else(|| {
                        CompileError::at(
                            format!("invalid floating-point literal: {}", parse_str),
                            location,
                        )
                    })?
                } else {
                    parse_str.parse::<f64>().map_err(|_| {
                        CompileError::at(
                            format!("invalid floating-point literal: {}", parse_str),
                            location,
                        )
                    })?
                };

                i = start + end_idx;
                tokens.push(Token {
                    kind: TokenKind::Num { value: 0, fval, ty },
                    location,
                    at_bol,
                    has_space,
                    len: i - start,
                    hideset: HideSet::default(),
                    origin: None,
                });
                at_bol = false;
                has_space = false;
                column += i - start;
                continue;
            }

            // Determine base from prefix
            let (base, prefix_len) = if i + 1 < bytes.len() && bytes[i] == b'0' {
                if bytes[i + 1] == b'x' || bytes[i + 1] == b'X' {
                    // Hexadecimal: 0x or 0X
                    if i + 2 < bytes.len() && bytes[i + 2].is_ascii_hexdigit() {
                        (16, 2)
                    } else {
                        (10, 0) // Just "0x" without digits - parse as 0
                    }
                } else if bytes[i + 1] == b'b' || bytes[i + 1] == b'B' {
                    // Binary: 0b or 0B
                    if i + 2 < bytes.len() && (bytes[i + 2] == b'0' || bytes[i + 2] == b'1') {
                        (2, 2)
                    } else {
                        (10, 0) // Just "0b" without digits - parse as 0
                    }
                } else {
                    // Octal: leading 0
                    (8, 0)
                }
            } else {
                // Decimal
                (10, 0)
            };

            i += prefix_len;
            let digits_start = i;

            // Parse digits
            while i < bytes.len() {
                let ch = bytes[i];
                let is_valid = match base {
                    2 => ch == b'0' || ch == b'1',
                    8 => (b'0'..=b'7').contains(&ch),
                    10 => ch.is_ascii_digit(),
                    16 => ch.is_ascii_hexdigit(),
                    _ => false,
                };
                if is_valid {
                    i += 1;
                } else {
                    break;
                }
            }

            let num_str = &input[digits_start..i];
            let value = u128::from_str_radix(num_str, base).map_err(|err| {
                CompileError::at(format!("invalid number literal: {err}"), location)
            })?;
            let value = value.min(u64::MAX as u128) as u64;

            // Read U, L or LL suffixes.
            let mut has_long = false;
            let mut has_unsigned = false;
            let suffix = &input[i..];

            if suffix.starts_with("LLU")
                || suffix.starts_with("LLu")
                || suffix.starts_with("llU")
                || suffix.starts_with("llu")
                || suffix.starts_with("ULL")
                || suffix.starts_with("Ull")
                || suffix.starts_with("uLL")
                || suffix.starts_with("ull")
            {
                i += 3;
                has_long = true;
                has_unsigned = true;
            } else if suffix.len() >= 2
                && (suffix[..2].eq_ignore_ascii_case("lu")
                    || suffix[..2].eq_ignore_ascii_case("ul"))
            {
                i += 2;
                has_long = true;
                has_unsigned = true;
            } else if suffix.starts_with("LL") || suffix.starts_with("ll") {
                i += 2;
                has_long = true;
            } else if suffix.starts_with('L') || suffix.starts_with('l') {
                i += 1;
                has_long = true;
            } else if suffix.starts_with('U') || suffix.starts_with('u') {
                i += 1;
                has_unsigned = true;
            }

            // Check if this might be a floating-point literal
            let next_char = if i < bytes.len() {
                bytes[i] as char
            } else {
                '\0'
            };

            if matches!(next_char, '.' | 'e' | 'E' | 'f' | 'F' | 'p' | 'P') {
                // Parse as floating-point
                let float_str = &input[start..];
                let mut end_idx = 0;
                let mut prev_char = '\0';
                for (idx, c) in float_str.char_indices() {
                    // Allow '+' or '-' only immediately after 'e', 'E', 'p', or 'P'
                    if matches!(c, '+' | '-') {
                        if !matches!(prev_char, 'e' | 'E' | 'p' | 'P') {
                            break;
                        }
                    } else if !matches!(c, '0'..='9' | '.' | 'e' | 'E' | 'f' | 'F' | 'l' | 'L' | 'p' | 'P' | 'x' | 'X' | 'a'..='d' | 'A'..='D')
                    {
                        break;
                    }
                    end_idx = idx + c.len_utf8();
                    prev_char = c;
                }
                if end_idx == 0 {
                    end_idx = float_str.len();
                }

                let float_literal = &float_str[..end_idx];
                let mut parse_str = float_literal;

                // Check for suffix
                let ty = if float_literal.ends_with('f') || float_literal.ends_with('F') {
                    parse_str = &float_literal[..float_literal.len() - 1];
                    Type::Float
                } else if float_literal.ends_with('l') || float_literal.ends_with('L') {
                    parse_str = &float_literal[..float_literal.len() - 1];
                    Type::Double
                } else {
                    Type::Double
                };

                let fval = if parse_str.starts_with("0x") || parse_str.starts_with("0X") {
                    parse_hex_float(parse_str).ok_or_else(|| {
                        CompileError::at(
                            format!("invalid floating-point literal: {}", parse_str),
                            location,
                        )
                    })?
                } else {
                    parse_str.parse::<f64>().map_err(|_| {
                        CompileError::at(
                            format!("invalid floating-point literal: {}", parse_str),
                            location,
                        )
                    })?
                };

                i = start + end_idx;
                tokens.push(Token {
                    kind: TokenKind::Num { value: 0, fval, ty },
                    location,
                    at_bol,
                    has_space,
                    len: i - start,
                    hideset: HideSet::default(),
                    origin: None,
                });
                at_bol = false;
                has_space = false;
                column += i - start;
                continue;
            }

            // Check for invalid trailing alphanumeric
            if i < bytes.len() && bytes[i].is_ascii_alphanumeric() {
                return Err(CompileError::at(
                    "invalid digit in number literal".to_string(),
                    location,
                ));
            }

            let ty = if base == 10 {
                if has_long && has_unsigned {
                    Type::ULong
                } else if has_long {
                    Type::Long
                } else if has_unsigned {
                    if (value >> 32) != 0 {
                        Type::ULong
                    } else {
                        Type::UInt
                    }
                } else if (value >> 31) != 0 {
                    Type::Long
                } else {
                    Type::Int
                }
            } else if has_long && has_unsigned {
                Type::ULong
            } else if has_long {
                if (value >> 63) != 0 {
                    Type::ULong
                } else {
                    Type::Long
                }
            } else if has_unsigned {
                if (value >> 32) != 0 {
                    Type::ULong
                } else {
                    Type::UInt
                }
            } else if (value >> 63) != 0 {
                Type::ULong
            } else if (value >> 32) != 0 {
                Type::Long
            } else if (value >> 31) != 0 {
                Type::UInt
            } else {
                Type::Int
            };

            tokens.push(Token {
                kind: TokenKind::Num {
                    value: value as i64,
                    fval: 0.0,
                    ty,
                },
                location,
                at_bol,
                has_space,
                len: i - start,
                hideset: HideSet::default(),
                origin: None,
            });
            at_bol = false;
            has_space = false;
            column += i - start;
            continue;
        }

        if b == b'L' && i + 1 < bytes.len() && bytes[i + 1] == b'\'' {
            let start = i;
            let location = SourceLocation {
                line,
                column,
                byte: start,
                file_no,
            };
            let (value, end) = read_char_literal(bytes, start, start + 1, location)?;
            tokens.push(Token {
                kind: TokenKind::Num {
                    value,
                    fval: 0.0,
                    ty: Type::Int,
                },
                location,
                at_bol,
                has_space,
                len: end - start,
                hideset: HideSet::default(),
                origin: None,
            });
            at_bol = false;
            has_space = false;
            i = end;
            column += i - start;
            continue;
        }

        if is_ident_start(b) {
            let start = i;
            let location = SourceLocation {
                line,
                column,
                byte: start,
                file_no,
            };
            i += 1;
            while i < bytes.len() && is_ident_continue(bytes[i]) {
                i += 1;
            }
            let word = &input[start..i];
            let kind = match word {
                "void" => TokenKind::Keyword(Keyword::Void),
                "_Bool" => TokenKind::Keyword(Keyword::Bool),
                "char" => TokenKind::Keyword(Keyword::Char),
                "short" => TokenKind::Keyword(Keyword::Short),
                "int" => TokenKind::Keyword(Keyword::Int),
                "long" => TokenKind::Keyword(Keyword::Long),
                "float" => TokenKind::Keyword(Keyword::Float),
                "double" => TokenKind::Keyword(Keyword::Double),
                "enum" => TokenKind::Keyword(Keyword::Enum),
                "struct" => TokenKind::Keyword(Keyword::Struct),
                "union" => TokenKind::Keyword(Keyword::Union),
                "typedef" => TokenKind::Keyword(Keyword::Typedef),
                "static" => TokenKind::Keyword(Keyword::Static),
                "extern" => TokenKind::Keyword(Keyword::Extern),
                "return" => TokenKind::Keyword(Keyword::Return),
                "if" => TokenKind::Keyword(Keyword::If),
                "else" => TokenKind::Keyword(Keyword::Else),
                "for" => TokenKind::Keyword(Keyword::For),
                "while" => TokenKind::Keyword(Keyword::While),
                "do" => TokenKind::Keyword(Keyword::Do),
                "sizeof" => TokenKind::Keyword(Keyword::Sizeof),
                "_Alignof" => TokenKind::Keyword(Keyword::Alignof),
                "_Alignas" => TokenKind::Keyword(Keyword::Alignas),
                "goto" => TokenKind::Keyword(Keyword::Goto),
                "break" => TokenKind::Keyword(Keyword::Break),
                "continue" => TokenKind::Keyword(Keyword::Continue),
                "switch" => TokenKind::Keyword(Keyword::Switch),
                "case" => TokenKind::Keyword(Keyword::Case),
                "default" => TokenKind::Keyword(Keyword::Default),
                "signed" => TokenKind::Keyword(Keyword::Signed),
                "unsigned" => TokenKind::Keyword(Keyword::Unsigned),
                "const" => TokenKind::Keyword(Keyword::Const),
                "volatile" => TokenKind::Keyword(Keyword::Volatile),
                "auto" => TokenKind::Keyword(Keyword::Auto),
                "register" => TokenKind::Keyword(Keyword::Register),
                "restrict" => TokenKind::Keyword(Keyword::Restrict),
                "__restrict" => TokenKind::Keyword(Keyword::Restrict),
                "__restrict__" => TokenKind::Keyword(Keyword::Restrict),
                "_Noreturn" => TokenKind::Keyword(Keyword::Noreturn),
                _ => TokenKind::Ident(word.to_string()),
            };
            tokens.push(Token {
                kind,
                location,
                at_bol,
                has_space,
                len: i - start,
                hideset: HideSet::default(),
                origin: None,
            });
            at_bol = false;
            has_space = false;
            column += i - start;
            continue;
        }

        if b == b'"' {
            let start = i;
            let location = SourceLocation {
                line,
                column,
                byte: start,
                file_no,
            };
            let end = string_literal_end(bytes, start + 1, location)?;
            let mut str_bytes = Vec::with_capacity(end - (start + 1) + 1);
            let mut p = start + 1;
            while p < end {
                if bytes[p] == b'\\' {
                    let mut escaped_pos = p + 1;
                    let escaped = read_escaped_char(bytes, &mut escaped_pos, location)?;
                    str_bytes.push(escaped);
                    p = escaped_pos;
                } else {
                    str_bytes.push(bytes[p]);
                    p += 1;
                }
            }
            str_bytes.push(0);
            let ty = Type::Array {
                base: Box::new(Type::Char),
                len: str_bytes.len() as i32,
            };
            tokens.push(Token {
                kind: TokenKind::Str {
                    bytes: str_bytes,
                    ty,
                },
                location,
                at_bol,
                has_space,
                len: end + 1 - start,
                hideset: HideSet::default(),
                origin: None,
            });
            at_bol = false;
            has_space = false;
            i = end + 1;
            column += i - start;
            continue;
        }

        if b == b'\'' {
            let start = i;
            let location = SourceLocation {
                line,
                column,
                byte: start,
                file_no,
            };
            let (value, end) = read_char_literal(bytes, start, start, location)?;
            tokens.push(Token {
                kind: TokenKind::Num {
                    value,
                    fval: 0.0,
                    ty: Type::Int,
                },
                location,
                at_bol,
                has_space,
                len: end - start,
                hideset: HideSet::default(),
                origin: None,
            });
            at_bol = false;
            has_space = false;
            i = end;
            column += i - start;
            continue;
        }

        if let Some((punct, len)) = read_punct(&input[i..]) {
            tokens.push(Token {
                kind: TokenKind::Punct(punct),
                location: SourceLocation {
                    line,
                    column,
                    byte: i,
                    file_no,
                },
                at_bol,
                has_space,
                len,
                hideset: HideSet::default(),
                origin: None,
            });
            at_bol = false;
            has_space = false;
            i += len;
            column += len;
            continue;
        }

        return Err(CompileError::at(
            format!("invalid token: {}", b as char),
            SourceLocation {
                line,
                column,
                byte: i,
                file_no,
            },
        ));
    }

    tokens.push(Token {
        kind: TokenKind::Eof,
        location: SourceLocation {
            line,
            column,
            byte: input.len(),
            file_no,
        },
        at_bol,
        has_space: false,
        len: 0,
        hideset: HideSet::default(),
        origin: None,
    });

    Ok(tokens)
}

fn read_escaped_char(input: &[u8], pos: &mut usize, location: SourceLocation) -> CompileResult<u8> {
    if *pos >= input.len() {
        return Ok(0);
    }

    let b = input[*pos];
    if b.is_ascii_digit() && b <= b'7' {
        let mut value = b - b'0';
        *pos += 1;
        if *pos < input.len() && input[*pos].is_ascii_digit() && input[*pos] <= b'7' {
            value = (value << 3) + (input[*pos] - b'0');
            *pos += 1;
            if *pos < input.len() && input[*pos].is_ascii_digit() && input[*pos] <= b'7' {
                value = (value << 3) + (input[*pos] - b'0');
                *pos += 1;
            }
        }
        return Ok(value);
    }

    if b == b'x' {
        *pos += 1;
        if *pos >= input.len() || !is_hex_digit(input[*pos]) {
            return Err(CompileError::at("invalid hex escape sequence", location));
        }
        let mut value: u32 = 0;
        while *pos < input.len() && is_hex_digit(input[*pos]) {
            value = (value << 4) + from_hex(input[*pos]);
            *pos += 1;
        }
        return Ok(value as u8);
    }

    *pos += 1;
    Ok(match b {
        b'a' => b'\x07',
        b'b' => b'\x08',
        b't' => b'\t',
        b'n' => b'\n',
        b'v' => b'\x0b',
        b'f' => b'\x0c',
        b'r' => b'\r',
        b'e' => 27,
        _ => b,
    })
}

fn read_char_literal(
    input: &[u8],
    _start: usize,
    quote: usize,
    location: SourceLocation,
) -> CompileResult<(i64, usize)> {
    let mut i = quote + 1;
    if i >= input.len() {
        return Err(CompileError::at("unclosed char literal", location));
    }

    let value = if input[i] == b'\\' {
        let mut escaped_pos = i + 1;
        let escaped = read_escaped_char(input, &mut escaped_pos, location)?;
        i = escaped_pos;
        escaped as i8 as i64
    } else {
        let escaped = input[i];
        i += 1;
        escaped as i8 as i64
    };

    if i >= input.len() || input[i] != b'\'' {
        return Err(CompileError::at("unclosed char literal", location));
    }
    i += 1;

    Ok((value, i))
}

fn string_literal_end(
    input: &[u8],
    mut pos: usize,
    start_location: SourceLocation,
) -> CompileResult<usize> {
    while pos < input.len() {
        match input[pos] {
            b'"' => return Ok(pos),
            b'\n' | b'\0' => {
                return Err(CompileError::at("unclosed string literal", start_location));
            }
            b'\\' => {
                pos += 1;
            }
            _ => {}
        }
        pos += 1;
    }
    Err(CompileError::at("unclosed string literal", start_location))
}

fn is_hex_digit(b: u8) -> bool {
    b.is_ascii_hexdigit()
}

fn from_hex(b: u8) -> u32 {
    match b {
        b'0'..=b'9' => (b - b'0') as u32,
        b'a'..=b'f' => (b - b'a' + 10) as u32,
        _ => (b - b'A' + 10) as u32,
    }
}

fn is_ident_start(b: u8) -> bool {
    b.is_ascii_alphabetic() || b == b'_'
}

fn is_ident_continue(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

fn read_punct(input: &str) -> Option<(Punct, usize)> {
    if input.starts_with("...") {
        return Some((Punct::Ellipsis, 3));
    }
    if input.starts_with("<<=") {
        return Some((Punct::ShlAssign, 3));
    }
    if input.starts_with(">>=") {
        return Some((Punct::ShrAssign, 3));
    }
    if input.starts_with("##") {
        return Some((Punct::HashHash, 2));
    }
    if input.starts_with("==") {
        return Some((Punct::EqEq, 2));
    }
    if input.starts_with("!=") {
        return Some((Punct::NotEq, 2));
    }
    if input.starts_with("<=") {
        return Some((Punct::LessEq, 2));
    }
    if input.starts_with(">=") {
        return Some((Punct::GreaterEq, 2));
    }
    if input.starts_with("->") {
        return Some((Punct::Arrow, 2));
    }
    if input.starts_with("<<") {
        return Some((Punct::Shl, 2));
    }
    if input.starts_with(">>") {
        return Some((Punct::Shr, 2));
    }
    if input.starts_with("+=") {
        return Some((Punct::AddAssign, 2));
    }
    if input.starts_with("-=") {
        return Some((Punct::SubAssign, 2));
    }
    if input.starts_with("*=") {
        return Some((Punct::MulAssign, 2));
    }
    if input.starts_with("/=") {
        return Some((Punct::DivAssign, 2));
    }
    if input.starts_with("%=") {
        return Some((Punct::ModAssign, 2));
    }
    if input.starts_with("&&") {
        return Some((Punct::LogAnd, 2));
    }
    if input.starts_with("&=") {
        return Some((Punct::AndAssign, 2));
    }
    if input.starts_with("||") {
        return Some((Punct::LogOr, 2));
    }
    if input.starts_with("|=") {
        return Some((Punct::OrAssign, 2));
    }
    if input.starts_with("^=") {
        return Some((Punct::XorAssign, 2));
    }
    if input.starts_with("++") {
        return Some((Punct::Inc, 2));
    }
    if input.starts_with("--") {
        return Some((Punct::Dec, 2));
    }
    if input.starts_with("=") {
        return Some((Punct::Assign, 1));
    }

    let ch = input.as_bytes().first().copied()?;
    let punct = match ch {
        b'+' => Punct::Plus,
        b'-' => Punct::Minus,
        b'*' => Punct::Star,
        b'&' => Punct::Amp,
        b'#' => Punct::Hash,
        b'|' => Punct::Pipe,
        b'^' => Punct::Caret,
        b'/' => Punct::Slash,
        b'%' => Punct::Mod,
        b'`' => Punct::Backtick,
        b'.' => Punct::Dot,
        b'!' => Punct::Not,
        b'~' => Punct::BitNot,
        b'(' => Punct::LParen,
        b')' => Punct::RParen,
        b'{' => Punct::LBrace,
        b'}' => Punct::RBrace,
        b'[' => Punct::LBracket,
        b']' => Punct::RBracket,
        b';' => Punct::Semicolon,
        b',' => Punct::Comma,
        b':' => Punct::Colon,
        b'?' => Punct::Question,
        b'<' => Punct::Less,
        b'>' => Punct::Greater,
        _ => return None,
    };
    Some((punct, 1))
}
