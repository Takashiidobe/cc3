use crate::error::{CompileError, CompileResult};
use crate::lexer::{Punct, Token, TokenKind, get_input_file, tokenize_file};
use crate::parser::const_expr;
use std::path::Path;

fn is_hash(tok: &Token) -> bool {
    tok.at_bol && matches!(tok.kind, TokenKind::Punct(Punct::Hash))
}

fn new_eof(tok: &Token) -> Token {
    let mut eof = tok.clone();
    eof.kind = TokenKind::Eof;
    eof.len = 0;
    eof
}

fn warn_tok(tok: &Token, message: &str) {
    let header = format!("warning: {message}");
    eprintln!("{header}");

    let Some(file) = get_input_file(tok.location.file_no) else {
        return;
    };

    let line_text = file
        .contents
        .lines()
        .nth(tok.location.line.saturating_sub(1));
    let width = tok.location.line.to_string().len().max(3);

    eprintln!(
        "  --> {}:{}:{}",
        file.name.display(),
        tok.location.line,
        tok.location.column
    );
    eprintln!("{:>width$} |", "", width = width);
    if let Some(text) = line_text {
        eprintln!("{:>width$} | {}", tok.location.line, text, width = width);
        let caret_pad = " ".repeat(tok.location.column.saturating_sub(1));
        eprintln!("{:>width$} | {}^", "", caret_pad, width = width);
    }
}

fn skip_line(tokens: &[Token], mut idx: usize) -> usize {
    if idx >= tokens.len() {
        return idx;
    }
    if tokens[idx].at_bol {
        return idx;
    }
    warn_tok(&tokens[idx], "extra token");
    while idx < tokens.len() && !tokens[idx].at_bol {
        idx += 1;
    }
    idx
}

fn skip_cond_incl(tokens: &[Token], mut idx: usize) -> usize {
    while idx < tokens.len() {
        let tok = &tokens[idx];
        if matches!(tok.kind, TokenKind::Eof) {
            return idx;
        }
        if is_hash(tok)
            && let Some(TokenKind::Ident(name)) = tokens.get(idx + 1).map(|tok| &tok.kind)
        {
            if name == "if" {
                idx = skip_cond_incl(tokens, idx + 2);
                if idx < tokens.len() {
                    idx += 1;
                }
                continue;
            }
            if name == "endif" {
                break;
            }
        }
        idx += 1;
    }
    idx
}

fn copy_line(tokens: &[Token], mut idx: usize) -> (Vec<Token>, usize) {
    let mut out = Vec::new();
    while idx < tokens.len() && !tokens[idx].at_bol {
        out.push(tokens[idx].clone());
        idx += 1;
    }
    let eof_source = tokens.get(idx).or_else(|| tokens.last()).expect("tokens");
    out.push(new_eof(eof_source));
    (out, idx)
}

fn eval_const_expr(tokens: &[Token], idx: usize) -> CompileResult<(i64, usize)> {
    let start = tokens
        .get(idx)
        .cloned()
        .ok_or_else(|| CompileError::new("no expression"))?;
    let (expr_tokens, rest_idx) = copy_line(tokens, idx + 1);

    if matches!(
        expr_tokens.first().map(|tok| &tok.kind),
        Some(TokenKind::Eof)
    ) {
        return Err(CompileError::at("no expression", start.location));
    }

    let value = const_expr(&expr_tokens)?;
    Ok((value, rest_idx))
}

fn preprocess_tokens(tokens: Vec<Token>) -> CompileResult<Vec<Token>> {
    let mut out = Vec::with_capacity(tokens.len());
    let mut i = 0;
    let mut cond_incl: Vec<Token> = Vec::new();

    while i < tokens.len() {
        let tok = &tokens[i];
        if matches!(tok.kind, TokenKind::Eof) {
            out.push(tok.clone());
            break;
        }

        if !is_hash(tok) {
            out.push(tok.clone());
            i += 1;
            continue;
        }

        let start = tok.clone();
        i += 1;
        if i >= tokens.len() {
            break;
        }

        if let TokenKind::Ident(name) = &tokens[i].kind
            && name == "include"
        {
            i += 1;
            let Some(token) = tokens.get(i) else {
                return Err(CompileError::at(
                    "expected a filename",
                    tokens[i - 1].location,
                ));
            };

            let filename = match &token.kind {
                TokenKind::Str { bytes, .. } => {
                    let slice = bytes.strip_suffix(&[0]).unwrap_or(bytes.as_slice());
                    String::from_utf8_lossy(slice).into_owned()
                }
                _ => return Err(CompileError::at("expected a filename", token.location)),
            };

            let include_path = if Path::new(&filename).is_absolute() {
                Path::new(&filename).to_path_buf()
            } else {
                let base = get_input_file(token.location.file_no)
                    .map(|file| file.name)
                    .unwrap_or_else(|| Path::new(".").to_path_buf());
                let dir = base.parent().unwrap_or(Path::new("."));
                dir.join(filename)
            };

            let included = tokenize_file(&include_path)
                .map_err(|err| CompileError::at(err.message().to_string(), token.location))?;
            for inc in included {
                if !matches!(inc.kind, TokenKind::Eof) {
                    out.push(inc);
                }
            }
            i = skip_line(&tokens, i + 1);
            continue;
        }

        if let TokenKind::Ident(name) = &tokens[i].kind
            && name == "if"
        {
            let (value, rest_idx) = eval_const_expr(&tokens, i)?;
            cond_incl.push(start);
            if value == 0 {
                i = skip_cond_incl(&tokens, rest_idx);
            } else {
                i = rest_idx;
            }
            continue;
        }

        if let TokenKind::Ident(name) = &tokens[i].kind
            && name == "endif"
        {
            if cond_incl.is_empty() {
                return Err(CompileError::at("stray #endif", start.location));
            }
            cond_incl.pop();
            i = skip_line(&tokens, i + 1);
            continue;
        }

        if tokens[i].at_bol {
            continue;
        }

        return Err(CompileError::at(
            "invalid preprocessor directive",
            tokens[i].location,
        ));
    }

    if let Some(start) = cond_incl.first() {
        return Err(CompileError::at(
            "unterminated conditional directive",
            start.location,
        ));
    }

    Ok(out)
}

/// Entry point of the preprocessor.
pub fn preprocess(tokens: Vec<Token>) -> CompileResult<Vec<Token>> {
    preprocess_tokens(tokens)
}
