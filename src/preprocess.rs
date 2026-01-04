// This file implements the C preprocessor.
//
// The preprocessor takes a list of tokens as an input and returns a
// new list of tokens as an output.
//
// The preprocessing language is designed in such a way that that's
// guaranteed to stop even if there is a recursive macro.
// Informally speaking, a macro is applied only once for each token.
// That is, if a macro token T appears in a result of direct or
// indirect macro expansion of T, T won't be expanded any further.
// For example, if T is defined as U, and U is defined as T, then
// token T is expanded to U and then to T and the macro expansion
// stops at that point.
//
// To achieve the above behavior, we attach for each token a set of
// macro names from which the token is expanded. The set is called
// "hideset". Hideset is initially empty, and every time we expand a
// macro, the macro name is added to the resulting tokens' hidesets.
//
// The above macro expansion algorithm is explained in this document,
// which is used as a basis for the standard's wording:
// https://github.com/rui314/chibicc/wiki/cpp.algo.pdf

use crate::error::{CompileError, CompileResult};
use crate::lexer::{Punct, Token, TokenKind, get_input_file, tokenize_file};
use crate::parser::const_expr;
use std::collections::HashSet;
use std::path::Path;

#[derive(Clone, Debug)]
struct MacroParam {
    name: String,
}

#[derive(Clone, Debug)]
struct MacroArg {
    name: String,
    tokens: Vec<Token>,
}

#[derive(Clone, Debug)]
struct Macro {
    name: String,
    is_objlike: bool, // Object-like or function-like
    params: Vec<MacroParam>,
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

struct Preprocessor {
    macros: Vec<Macro>,
}

impl Preprocessor {
    fn new() -> Self {
        Self { macros: Vec::new() }
    }

    fn find_macro(&self, tok: &Token) -> Option<&Macro> {
        let TokenKind::Ident(name) = &tok.kind else {
            return None;
        };
        self.macros.iter().find(|m| &m.name == name && !m.deleted)
    }

    fn add_macro(
        &mut self,
        name: String,
        is_objlike: bool,
        params: Vec<MacroParam>,
        body: Vec<Token>,
        deleted: bool,
    ) {
        self.macros.push(Macro {
            name,
            is_objlike,
            params,
            body,
            deleted,
        });
    }

    fn read_macro_params(&self, tokens: &[Token], mut idx: usize) -> (Vec<MacroParam>, usize) {
        let mut params = Vec::new();

        while idx < tokens.len() && !matches!(tokens[idx].kind, TokenKind::Punct(Punct::RParen)) {
            if !params.is_empty() {
                // Expect comma between parameters
                if !matches!(tokens[idx].kind, TokenKind::Punct(Punct::Comma)) {
                    // Error: expected comma
                    break;
                }
                idx += 1;
            }

            // Expect identifier
            let TokenKind::Ident(name) = &tokens[idx].kind else {
                // Error: expected identifier
                break;
            };
            params.push(MacroParam { name: name.clone() });
            idx += 1;
        }

        // Skip the closing ')'
        if idx < tokens.len() && matches!(tokens[idx].kind, TokenKind::Punct(Punct::RParen)) {
            idx += 1;
        }

        (params, idx)
    }

    fn read_macro_definition(&mut self, tokens: &[Token], idx: usize) -> usize {
        let tok = &tokens[idx];
        let TokenKind::Ident(name) = &tok.kind else {
            // Error: macro name must be an identifier (would need error handling)
            return idx;
        };
        let macro_name = name.clone();
        let mut i = idx + 1;

        // Check if this is a function-like macro (no space before '(')
        if i < tokens.len()
            && !tokens[i].has_space
            && matches!(&tokens[i].kind, TokenKind::Punct(Punct::LParen))
        {
            // Function-like macro - read parameters
            i += 1; // skip '('
            let (params, param_end) = self.read_macro_params(tokens, i);
            let (body, rest_idx) = copy_line(tokens, param_end);
            self.add_macro(macro_name, false, params, body, false);
            rest_idx
        } else {
            // Object-like macro
            let (body, rest_idx) = copy_line(tokens, i);
            self.add_macro(macro_name, true, Vec::new(), body, false);
            rest_idx
        }
    }

    fn read_macro_arg_one(&self, tokens: &[Token], mut idx: usize) -> (Vec<Token>, usize) {
        let mut arg_tokens = Vec::new();

        while idx < tokens.len()
            && !matches!(tokens[idx].kind, TokenKind::Punct(Punct::Comma))
            && !matches!(tokens[idx].kind, TokenKind::Punct(Punct::RParen))
        {
            if matches!(tokens[idx].kind, TokenKind::Eof) {
                // Error: premature end of input
                break;
            }
            arg_tokens.push(tokens[idx].clone());
            idx += 1;
        }

        arg_tokens.push(new_eof(tokens.get(idx).or_else(|| tokens.last()).unwrap()));
        (arg_tokens, idx)
    }

    fn read_macro_args(
        &self,
        tokens: &[Token],
        idx: usize,
        params: &[MacroParam],
    ) -> (Vec<MacroArg>, usize) {
        // idx should be pointing at the macro name, next should be '('
        let mut i = idx + 2; // skip macro name and '('
        let mut args = Vec::new();

        for param in params {
            if !args.is_empty() {
                // Expect comma between arguments
                if i < tokens.len() && matches!(tokens[i].kind, TokenKind::Punct(Punct::Comma)) {
                    i += 1;
                }
            }

            let (arg_tokens, next_idx) = self.read_macro_arg_one(tokens, i);
            args.push(MacroArg {
                name: param.name.clone(),
                tokens: arg_tokens,
            });
            i = next_idx;
        }

        // Skip the closing ')'
        if i < tokens.len() && matches!(tokens[i].kind, TokenKind::Punct(Punct::RParen)) {
            i += 1;
        }

        (args, i)
    }

    fn expand_macro(&self, tokens: &[Token], idx: usize) -> Option<Vec<Token>> {
        let tok = &tokens[idx];

        // Check if this macro is in the token's hideset
        let TokenKind::Ident(name) = &tok.kind else {
            return None;
        };
        if hideset_contains(&tok.hideset, name) {
            return None;
        }

        let m = self.find_macro(tok)?;

        // Object-like macro application
        if m.is_objlike {
            let hs = hideset_union(&tok.hideset, &new_hideset(m.name.clone()));
            let body = add_hideset(m.body.clone(), hs);

            let mut result = body;
            result.extend_from_slice(&tokens[idx + 1..]);
            return Some(result);
        }

        // If a funclike macro token is not followed by an argument list,
        // treat it as a normal identifier.
        if idx + 1 >= tokens.len()
            || !matches!(tokens[idx + 1].kind, TokenKind::Punct(Punct::LParen))
        {
            return None;
        }

        // Function-like macro application - read arguments and substitute
        let (args, i) = self.read_macro_args(tokens, idx, &m.params);
        let mut result = subst(self, &m.body, &args);
        result.extend_from_slice(&tokens[i..]);
        Some(result)
    }

    fn expand_macros_only(&self, mut tokens: Vec<Token>) -> Vec<Token> {
        let mut out = Vec::new();
        let mut i = 0;

        while i < tokens.len() {
            if matches!(tokens[i].kind, TokenKind::Eof) {
                out.push(tokens[i].clone());
                break;
            }

            // Try to expand macros
            if let Some(expanded) = self.expand_macro(&tokens, i) {
                tokens = expanded;
                continue;
            }

            out.push(tokens[i].clone());
            i += 1;
        }

        out
    }
}

fn find_arg<'a>(args: &'a [MacroArg], tok: &Token) -> Option<&'a MacroArg> {
    let TokenKind::Ident(name) = &tok.kind else {
        return None;
    };
    args.iter().find(|arg| &arg.name == name)
}

fn subst(pp: &Preprocessor, body: &[Token], args: &[MacroArg]) -> Vec<Token> {
    let mut result = Vec::new();

    for tok in body {
        if matches!(tok.kind, TokenKind::Eof) {
            break;
        }

        // Check if this token is a parameter that should be substituted
        if let Some(arg) = find_arg(args, tok) {
            // Macro arguments are completely macro-expanded before substitution
            let expanded = pp.expand_macros_only(arg.tokens.clone());
            for exp_tok in &expanded {
                if matches!(exp_tok.kind, TokenKind::Eof) {
                    break;
                }
                result.push(exp_tok.clone());
            }
        } else {
            // Not a parameter, just copy the token
            result.push(tok.clone());
        }
    }

    result
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

fn new_hideset(name: String) -> HashSet<String> {
    let mut hs = HashSet::new();
    hs.insert(name);
    hs
}

fn hideset_union(hs1: &HashSet<String>, hs2: &HashSet<String>) -> HashSet<String> {
    hs1.union(hs2).cloned().collect()
}

fn hideset_contains(hs: &HashSet<String>, name: &str) -> bool {
    hs.contains(name)
}

fn add_hideset(tokens: Vec<Token>, hs: HashSet<String>) -> Vec<Token> {
    tokens
        .into_iter()
        .map(|mut tok| {
            tok.hideset = hideset_union(&tok.hideset, &hs);
            tok
        })
        .collect()
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
            if name == "if" || name == "ifdef" || name == "ifndef" {
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
            if name == "if" || name == "ifdef" || name == "ifndef" {
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

fn eval_const_expr(pp: &Preprocessor, tokens: &[Token], idx: usize) -> CompileResult<(i64, usize)> {
    let start = tokens
        .get(idx)
        .cloned()
        .ok_or_else(|| CompileError::new("no expression"))?;
    let (expr_tokens, rest_idx) = copy_line(tokens, idx + 1);

    // Expand macros in the expression
    let expr_tokens = pp.expand_macros_only(expr_tokens);

    if matches!(
        expr_tokens.first().map(|tok| &tok.kind),
        Some(TokenKind::Eof)
    ) {
        return Err(CompileError::at("no expression", start.location));
    }

    let value = const_expr(&expr_tokens)?;
    Ok((value, rest_idx))
}

fn preprocess_tokens(mut tokens: Vec<Token>) -> CompileResult<Vec<Token>> {
    let mut out = Vec::with_capacity(tokens.len());
    let mut i = 0;
    let mut cond_incl: Vec<CondIncl> = Vec::new();
    let mut pp = Preprocessor::new();

    while i < tokens.len() {
        // Try to expand macros
        if let Some(expanded) = pp.expand_macro(&tokens, i) {
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
            let TokenKind::Ident(_name) = &tok.kind else {
                return Err(CompileError::at(
                    "macro name must be an identifier",
                    tok.location,
                ));
            };
            i = pp.read_macro_definition(&tokens, i);
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
            pp.add_macro(macro_name, true, Vec::new(), Vec::new(), true);
            continue;
        }

        if let TokenKind::Ident(name) = &tokens[i].kind
            && (name == "ifdef" || name == "ifndef")
        {
            let is_ifdef = name == "ifdef";
            i += 1;
            let defined = pp.find_macro(&tokens[i]).is_some();
            let included = if is_ifdef { defined } else { !defined };
            cond_incl.push(CondIncl {
                ctx: CondCtx::Then,
                tok: start,
                included,
            });
            i = skip_line(&tokens, i + 1);
            if !included {
                i = skip_cond_incl(&tokens, i);
            }
            continue;
        }

        if let TokenKind::Ident(name) = &tokens[i].kind
            && name == "if"
        {
            let (value, rest_idx) = eval_const_expr(&pp, &tokens, i)?;
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

            let (value, rest_idx) = eval_const_expr(&pp, &tokens, i)?;
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
