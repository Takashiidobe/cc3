use crate::error::{CompileError, CompileResult};
use crate::lexer::{Punct, Token, TokenKind, get_input_file, tokenize_file};
use crate::parser::const_expr;
use std::path::Path;

#[derive(Clone, Debug)]
struct Macro {
    name: String,
    body: Vec<Token>,
    deleted: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CondCtx {
    Then,
    Elif,
    Else,
}

#[derive(Clone, Debug)]
struct CondIncl {
    ctx: CondCtx,
    tok: Token,
    included: bool,
}

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

fn skip_cond_incl2(tokens: &[Token], mut idx: usize) -> usize {
    while idx < tokens.len() {
        let tok = &tokens[idx];
        if matches!(tok.kind, TokenKind::Eof) {
            return idx;
        }
        if is_hash(tok)
            && let Some(TokenKind::Ident(name)) = tokens.get(idx + 1).map(|tok| &tok.kind)
        {
            if name == "if" {
                idx = skip_cond_incl2(tokens, idx + 2);
                continue;
            }
            if name == "endif" {
                return idx + 2;
            }
        }
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
                idx = skip_cond_incl2(tokens, idx + 2);
                continue;
            }
            if name == "elif" || name == "else" || name == "endif" {
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

fn eval_const_expr(macros: &[Macro], tokens: &[Token], idx: usize) -> CompileResult<(i64, usize)> {
    let start = tokens
        .get(idx)
        .cloned()
        .ok_or_else(|| CompileError::new("no expression"))?;
    let (expr_tokens, rest_idx) = copy_line(tokens, idx + 1);

    // Expand macros in the expression
    let expr_tokens = expand_macros_only(macros, expr_tokens);

    if matches!(
        expr_tokens.first().map(|tok| &tok.kind),
        Some(TokenKind::Eof)
    ) {
        return Err(CompileError::at("no expression", start.location));
    }

    let value = const_expr(&expr_tokens)?;
    Ok((value, rest_idx))
}

fn find_macro<'a>(macros: &'a [Macro], tok: &Token) -> Option<&'a Macro> {
    let TokenKind::Ident(name) = &tok.kind else {
        return None;
    };
    macros.iter().find(|m| &m.name == name && !m.deleted)
}

fn add_macro(macros: &mut Vec<Macro>, name: String, body: Vec<Token>, deleted: bool) {
    macros.push(Macro {
        name,
        body,
        deleted,
    });
}

fn expand_macro(macros: &[Macro], tokens: &[Token], idx: usize) -> Option<Vec<Token>> {
    let m = find_macro(macros, &tokens[idx])?;
    let mut result = m.body.clone();
    result.extend_from_slice(&tokens[idx + 1..]);
    Some(result)
}

// Expand all macros in a token list (used for #if and #elif expressions)
fn expand_macros_only(macros: &[Macro], mut tokens: Vec<Token>) -> Vec<Token> {
    let mut out = Vec::new();
    let mut i = 0;

    while i < tokens.len() {
        if matches!(tokens[i].kind, TokenKind::Eof) {
            out.push(tokens[i].clone());
            break;
        }

        // Try to expand macros
        if let Some(expanded) = expand_macro(macros, &tokens, i) {
            tokens = expanded;
            continue;
        }

        out.push(tokens[i].clone());
        i += 1;
    }

    out
}

fn preprocess_tokens(mut tokens: Vec<Token>) -> CompileResult<Vec<Token>> {
    let mut out = Vec::with_capacity(tokens.len());
    let mut i = 0;
    let mut cond_incl: Vec<CondIncl> = Vec::new();
    let mut macros: Vec<Macro> = Vec::new();

    while i < tokens.len() {
        // Try to expand macros
        if let Some(expanded) = expand_macro(&macros, &tokens, i) {
            tokens = expanded;
            continue;
        }
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
            && name == "define"
        {
            i += 1;
            let Some(tok) = tokens.get(i) else {
                return Err(CompileError::at(
                    "macro name must be an identifier",
                    start.location,
                ));
            };
            let TokenKind::Ident(name) = &tok.kind else {
                return Err(CompileError::at(
                    "macro name must be an identifier",
                    tok.location,
                ));
            };
            let macro_name = name.clone();
            let (body, rest_idx) = copy_line(&tokens, i + 1);
            add_macro(&mut macros, macro_name, body, false);
            i = rest_idx;
            continue;
        }

        if let TokenKind::Ident(name) = &tokens[i].kind
            && name == "undef"
        {
            i += 1;
            let Some(tok) = tokens.get(i) else {
                return Err(CompileError::at(
                    "macro name must be an identifier",
                    start.location,
                ));
            };
            let TokenKind::Ident(name) = &tok.kind else {
                return Err(CompileError::at(
                    "macro name must be an identifier",
                    tok.location,
                ));
            };
            let macro_name = name.clone();
            i = skip_line(&tokens, i + 1);
            add_macro(&mut macros, macro_name, Vec::new(), true);
            continue;
        }

        if let TokenKind::Ident(name) = &tokens[i].kind
            && name == "if"
        {
            let (value, rest_idx) = eval_const_expr(&macros, &tokens, i)?;
            cond_incl.push(CondIncl {
                ctx: CondCtx::Then,
                tok: start,
                included: value != 0,
            });
            if value == 0 {
                i = skip_cond_incl(&tokens, rest_idx);
            } else {
                i = rest_idx;
            }
            continue;
        }

        if let TokenKind::Ident(name) = &tokens[i].kind
            && name == "else"
        {
            let Some(last) = cond_incl.last_mut() else {
                return Err(CompileError::at("stray #else", start.location));
            };
            if last.ctx == CondCtx::Else {
                return Err(CompileError::at("stray #else", start.location));
            }
            last.ctx = CondCtx::Else;
            i = skip_line(&tokens, i + 1);
            if last.included {
                i = skip_cond_incl(&tokens, i);
            }
            continue;
        }

        if let TokenKind::Ident(name) = &tokens[i].kind
            && name == "elif"
        {
            let Some(last) = cond_incl.last_mut() else {
                return Err(CompileError::at("stray #elif", start.location));
            };
            if last.ctx == CondCtx::Else {
                return Err(CompileError::at("stray #elif", start.location));
            }
            last.ctx = CondCtx::Elif;

            let (value, rest_idx) = eval_const_expr(&macros, &tokens, i)?;
            if !last.included && value != 0 {
                last.included = true;
                i = rest_idx;
            } else {
                i = skip_cond_incl(&tokens, rest_idx);
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
            start.tok.location,
        ));
    }

    Ok(out)
}

/// Entry point of the preprocessor.
pub fn preprocess(tokens: Vec<Token>) -> CompileResult<Vec<Token>> {
    preprocess_tokens(tokens)
}
