use crate::ast::Type;
use crate::error::{CompileError, CompileResult, SourceLocation};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Char,
    Int,
    Return,
    If,
    Else,
    For,
    While,
    Sizeof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Keyword(Keyword),
    Ident(String),
    Num(i64),
    Str { bytes: Vec<u8>, ty: Type },
    Punct(Punct),
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Punct {
    Plus,
    Minus,
    Star,
    Amp,
    Slash,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Comma,
    Assign,
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
            Punct::Slash => "/",
            Punct::LParen => "(",
            Punct::RParen => ")",
            Punct::LBrace => "{",
            Punct::RBrace => "}",
            Punct::LBracket => "[",
            Punct::RBracket => "]",
            Punct::Semicolon => ";",
            Punct::Comma => ",",
            Punct::Assign => "=",
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub location: SourceLocation,
}

pub fn tokenize(input: &str) -> CompileResult<Vec<Token>> {
    let bytes = input.as_bytes();
    let mut tokens = Vec::new();
    let mut i = 0;
    let mut line = 1;
    let mut column = 1;

    while i < bytes.len() {
        let b = bytes[i];
        if b.is_ascii_whitespace() {
            if b == b'\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
            i += 1;
            continue;
        }

        if b.is_ascii_digit() {
            let start = i;
            let location = SourceLocation {
                line,
                column,
                byte: start,
            };
            i += 1;
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
            let num_str = &input[start..i];
            let value = num_str.parse::<i64>().map_err(|err| {
                CompileError::at(format!("invalid number literal {num_str}: {err}"), location)
            })?;
            tokens.push(Token {
                kind: TokenKind::Num(value),
                location,
            });
            column += i - start;
            continue;
        }

        if is_ident_start(b) {
            let start = i;
            let location = SourceLocation {
                line,
                column,
                byte: start,
            };
            i += 1;
            while i < bytes.len() && is_ident_continue(bytes[i]) {
                i += 1;
            }
            let word = &input[start..i];
            let kind = match word {
                "char" => TokenKind::Keyword(Keyword::Char),
                "int" => TokenKind::Keyword(Keyword::Int),
                "return" => TokenKind::Keyword(Keyword::Return),
                "if" => TokenKind::Keyword(Keyword::If),
                "else" => TokenKind::Keyword(Keyword::Else),
                "for" => TokenKind::Keyword(Keyword::For),
                "while" => TokenKind::Keyword(Keyword::While),
                "sizeof" => TokenKind::Keyword(Keyword::Sizeof),
                _ => TokenKind::Ident(word.to_string()),
            };
            tokens.push(Token { kind, location });
            column += i - start;
            continue;
        }

        if b == b'"' {
            let start = i;
            let location = SourceLocation {
                line,
                column,
                byte: start,
            };
            let end = string_literal_end(bytes, start + 1, location)?;
            let mut str_bytes = Vec::with_capacity(end - (start + 1) + 1);
            let mut p = start + 1;
            while p < end {
                if bytes[p] == b'\\' {
                    let escaped = read_escaped_char(bytes.get(p + 1).copied().unwrap_or(b'\0'));
                    str_bytes.push(escaped);
                    p += 2;
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
            });
            i = end + 1;
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
                },
            });
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
            },
        ));
    }

    tokens.push(Token {
        kind: TokenKind::Eof,
        location: SourceLocation {
            line,
            column,
            byte: input.len(),
        },
    });

    Ok(tokens)
}

fn read_escaped_char(b: u8) -> u8 {
    match b {
        b'a' => b'\x07',
        b'b' => b'\x08',
        b't' => b'\t',
        b'n' => b'\n',
        b'v' => b'\x0b',
        b'f' => b'\x0c',
        b'r' => b'\r',
        b'e' => 27,
        _ => b,
    }
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

fn is_ident_start(b: u8) -> bool {
    b.is_ascii_alphabetic() || b == b'_'
}

fn is_ident_continue(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

fn read_punct(input: &str) -> Option<(Punct, usize)> {
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
    if input.starts_with("=") {
        return Some((Punct::Assign, 1));
    }

    let ch = input.as_bytes().first().copied()?;
    let punct = match ch {
        b'+' => Punct::Plus,
        b'-' => Punct::Minus,
        b'*' => Punct::Star,
        b'&' => Punct::Amp,
        b'/' => Punct::Slash,
        b'(' => Punct::LParen,
        b')' => Punct::RParen,
        b'{' => Punct::LBrace,
        b'}' => Punct::RBrace,
        b'[' => Punct::LBracket,
        b']' => Punct::RBracket,
        b';' => Punct::Semicolon,
        b',' => Punct::Comma,
        b'<' => Punct::Less,
        b'>' => Punct::Greater,
        _ => return None,
    };
    Some((punct, 1))
}
