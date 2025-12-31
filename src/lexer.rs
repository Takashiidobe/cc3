use crate::error::{CompileError, CompileResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Int,
    Return,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Keyword(Keyword),
    Ident(String),
    Num(i64),
    Punct(char),
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: usize,
}

pub fn tokenize(input: &str) -> CompileResult<Vec<Token>> {
    let bytes = input.as_bytes();
    let mut tokens = Vec::new();
    let mut i = 0;

    while i < bytes.len() {
        let b = bytes[i];
        if b.is_ascii_whitespace() {
            i += 1;
            continue;
        }

        if b.is_ascii_digit() {
            let start = i;
            i += 1;
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
            let num_str = &input[start..i];
            let value = num_str.parse::<i64>().map_err(|err| {
                CompileError::at(format!("invalid number literal {num_str}: {err}"), start)
            })?;
            tokens.push(Token {
                kind: TokenKind::Num(value),
                pos: start,
            });
            continue;
        }

        if is_ident_start(b) {
            let start = i;
            i += 1;
            while i < bytes.len() && is_ident_continue(bytes[i]) {
                i += 1;
            }
            let word = &input[start..i];
            let kind = match word {
                "int" => TokenKind::Keyword(Keyword::Int),
                "return" => TokenKind::Keyword(Keyword::Return),
                _ => TokenKind::Ident(word.to_string()),
            };
            tokens.push(Token { kind, pos: start });
            continue;
        }

        if is_punct(b) {
            tokens.push(Token {
                kind: TokenKind::Punct(b as char),
                pos: i,
            });
            i += 1;
            continue;
        }

        return Err(CompileError::at(format!("invalid token: {}", b as char), i));
    }

    tokens.push(Token {
        kind: TokenKind::Eof,
        pos: input.len(),
    });

    Ok(tokens)
}

fn is_ident_start(b: u8) -> bool {
    b.is_ascii_alphabetic() || b == b'_'
}

fn is_ident_continue(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

fn is_punct(b: u8) -> bool {
    matches!(b, b'(' | b')' | b'{' | b'}' | b';' | b'+' | b'-')
}
