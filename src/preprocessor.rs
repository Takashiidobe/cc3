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
// The above macro expansion algorithm is explained in this document
// written by Dave Prossor, which is used as a basis for the
// standard's wording:
// https://github.com/rui314/chibicc/wiki/cpp.algo.pdf

use crate::error::{CompileError, CompileResult, SourceLocation};
use crate::lexer::{HideSet, Punct, Token, TokenKind, get_input_file, tokenize, tokenize_file};
use crate::parser::const_expr;
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

#[derive(Clone, Debug, Default)]
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
    tokens: Vec<Token>,
    macros: Vec<Macro>,
    pos: usize, // Current position in token stream
}

impl Preprocessor {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            macros: Vec::new(),
            pos: 0,
        }
    }

    fn peek(&self, offset: usize) -> &Token {
        &self.tokens[self.pos + offset]
    }

    fn cur_tok(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn error<T>(&self, message: impl Into<String>, location: SourceLocation) -> CompileResult<T> {
        Err(CompileError::at(message, location))
    }

    fn find_macro(&self, tok: &Token) -> Option<&Macro> {
        let TokenKind::Ident(name) = &tok.kind else {
            return None;
        };
        self.macros.iter().find(|m| &m.name == name && !m.deleted)
    }

    fn read_macro_params(&mut self) -> Vec<MacroParam> {
        let mut params = Vec::new();

        while self.pos < self.tokens.len()
            && !matches!(self.cur_tok().kind, TokenKind::Punct(Punct::RParen))
        {
            if !params.is_empty() {
                // Expect comma between parameters
                if !matches!(self.cur_tok().kind, TokenKind::Punct(Punct::Comma)) {
                    // Error: expected comma
                    break;
                }
                self.pos += 1;
            }

            // Expect identifier
            let TokenKind::Ident(name) = &self.cur_tok().kind else {
                // Error: expected identifier
                break;
            };
            params.push(MacroParam { name: name.clone() });
            self.pos += 1;
        }

        // Skip the closing ')'
        if self.pos < self.tokens.len()
            && matches!(self.cur_tok().kind, TokenKind::Punct(Punct::RParen))
        {
            self.pos += 1;
        }

        params
    }

    fn read_macro_definition(&mut self) {
        if self.pos >= self.tokens.len() {
            return;
        };
        let tok = self.cur_tok();
        let TokenKind::Ident(name) = &tok.kind else {
            // Error: macro name must be an identifier (would need error handling)
            return;
        };
        let macro_name = name.clone();
        self.pos += 1;

        // Check if this is a function-like macro (no space before '(')
        if self.pos < self.tokens.len()
            && !self.cur_tok().has_space
            && matches!(self.cur_tok().kind, TokenKind::Punct(Punct::LParen))
        {
            // Function-like macro - read parameters
            self.pos += 1; // skip '('
            let params = self.read_macro_params();
            let body = self.copy_line();
            self.macros.push(Macro {
                name: macro_name,
                is_objlike: false,
                params,
                body,
                ..Macro::default()
            });
            return;
        }

        // Object-like macro
        let body = self.copy_line();
        self.macros.push(Macro {
            name: macro_name,
            is_objlike: true,
            body,
            ..Macro::default()
        });
    }

    fn read_macro_arg_one(&mut self) -> Vec<Token> {
        let mut arg_tokens = Vec::new();

        while self.pos < self.tokens.len()
            && !matches!(self.cur_tok().kind, TokenKind::Punct(Punct::Comma))
            && !matches!(self.cur_tok().kind, TokenKind::Punct(Punct::RParen))
        {
            if matches!(self.cur_tok().kind, TokenKind::Eof) {
                // Error: premature end of input
                break;
            }
            arg_tokens.push(self.cur_tok().clone());
            self.pos += 1;
        }

        let eof_source = self
            .tokens
            .get(self.pos)
            .or_else(|| self.tokens.last())
            .expect("tokens");
        arg_tokens.push(new_eof(eof_source));
        arg_tokens
    }

    fn read_macro_args(&mut self, params: &[MacroParam]) -> Vec<MacroArg> {
        // self.pos should be pointing at the macro name, next should be '('
        self.pos += 2; // skip macro name and '('
        let mut args = Vec::new();

        for param in params {
            if !args.is_empty() {
                // Expect comma between arguments
                if self.pos < self.tokens.len()
                    && matches!(self.cur_tok().kind, TokenKind::Punct(Punct::Comma))
                {
                    self.pos += 1;
                }
            }

            let arg_tokens = self.read_macro_arg_one();
            args.push(MacroArg {
                name: param.name.clone(),
                tokens: arg_tokens,
            });
        }

        // Skip the closing ')'
        if self.pos < self.tokens.len()
            && matches!(self.cur_tok().kind, TokenKind::Punct(Punct::RParen))
        {
            self.pos += 1;
        }

        args
    }

    fn expand_macro(&mut self) -> CompileResult<Option<Vec<Token>>> {
        let tok = self.cur_tok().clone();

        // Check if this macro is in the token's hideset
        let TokenKind::Ident(name) = &tok.kind else {
            return Ok(None);
        };
        if tok.hideset.contains(name) {
            return Ok(None);
        }

        let (is_objlike, params, body, macro_name) = {
            let Some(m) = self.find_macro(&tok) else {
                return Ok(None);
            };
            (
                m.is_objlike,
                m.params.clone(),
                m.body.clone(),
                m.name.clone(),
            )
        };

        // Object-like macro application
        if is_objlike {
            let mut hs = HideSet::default();
            hs.add(macro_name);
            let hs = tok.hideset.union(&hs);
            let mut body = body;
            hs.add_tokens(&mut body);

            let mut result = Vec::new();
            result.extend_from_slice(&self.tokens[..self.pos]);
            result.extend(body);
            result.extend_from_slice(&self.tokens[self.pos + 1..]);
            return Ok(Some(result));
        }

        // If a funclike macro token is not followed by an argument list,
        // treat it as a normal identifier.
        if self.pos + 1 >= self.tokens.len()
            || !matches!(self.peek(1).kind, TokenKind::Punct(Punct::LParen))
        {
            return Ok(None);
        }

        // Function-like macro application - read arguments and substitute
        let saved_pos = self.pos;
        let args = self.read_macro_args(&params);
        let rest_pos = self.pos;
        let rparen_pos = rest_pos.saturating_sub(1);
        let mut hs = tok.hideset.intersection(&self.tokens[rparen_pos].hideset);
        hs.add(macro_name);
        self.pos = saved_pos;
        let mut result = Vec::new();
        result.extend_from_slice(&self.tokens[..saved_pos]);
        let mut expanded = self.subst(&body, &args)?;
        hs.add_tokens(&mut expanded);
        result.extend(expanded);
        result.extend_from_slice(&self.tokens[rest_pos..]);
        Ok(Some(result))
    }

    fn expand_macros_stream(&mut self) -> CompileResult<Vec<Token>> {
        let mut out = Vec::new();
        self.pos = 0;

        while self.pos < self.tokens.len() {
            if matches!(self.cur_tok().kind, TokenKind::Eof) {
                out.push(self.cur_tok().clone());
                break;
            }

            // Try to expand macros
            if let Some(expanded) = self.expand_macro()? {
                self.tokens = expanded;
                continue;
            }

            out.push(self.cur_tok().clone());
            self.pos += 1;
        }

        Ok(out)
    }

    fn expand_macros_only(&self, tokens: Vec<Token>) -> CompileResult<Vec<Token>> {
        let mut expander = Preprocessor::new(tokens);
        expander.macros = self.macros.clone();
        expander.expand_macros_stream()
    }

    fn skip_line(&mut self) {
        if self.pos >= self.tokens.len() {
            return;
        }
        if self.cur_tok().at_bol {
            return;
        }
        warn_tok(self.cur_tok(), "extra token");
        while self.pos < self.tokens.len() && !self.cur_tok().at_bol {
            self.pos += 1;
        }
    }

    fn skip_cond_incl2(&mut self) {
        while self.pos < self.tokens.len() {
            let tok = self.cur_tok();
            if matches!(tok.kind, TokenKind::Eof) {
                return;
            }
            if is_hash(tok)
                && self.pos + 1 < self.tokens.len()
                && let TokenKind::Ident(name) = &self.peek(1).kind
            {
                if name == "if" || name == "ifdef" || name == "ifndef" {
                    self.pos += 2;
                    self.skip_cond_incl2();
                    continue;
                }
                if name == "endif" {
                    self.pos += 2;
                    return;
                }
            }
            self.pos += 1;
        }
    }

    fn skip_cond_incl(&mut self) {
        while self.pos < self.tokens.len() {
            let tok = self.cur_tok();
            if matches!(tok.kind, TokenKind::Eof) {
                return;
            }
            if is_hash(tok)
                && self.pos + 1 < self.tokens.len()
                && let TokenKind::Ident(name) = &self.peek(1).kind
            {
                if name == "if" || name == "ifdef" || name == "ifndef" {
                    self.pos += 2;
                    self.skip_cond_incl2();
                    continue;
                }
                if name == "elif" || name == "else" || name == "endif" {
                    break;
                }
            }
            self.pos += 1;
        }
    }

    fn copy_line(&mut self) -> Vec<Token> {
        let mut out = Vec::new();
        while self.pos < self.tokens.len() && !self.cur_tok().at_bol {
            out.push(self.cur_tok().clone());
            self.pos += 1;
        }
        let eof_source = self
            .tokens
            .get(self.pos)
            .or_else(|| self.tokens.last())
            .expect("tokens");
        out.push(new_eof(eof_source));
        out
    }

    fn eval_const_expr(&mut self) -> CompileResult<i64> {
        let start = self
            .tokens
            .get(self.pos.saturating_sub(1))
            .or_else(|| self.tokens.get(self.pos))
            .cloned()
            .ok_or_else(|| CompileError::new("no expression"))?;
        let expr_tokens = self.copy_line();

        // Expand macros in the expression
        let expr_tokens = self.expand_macros_only(expr_tokens)?;

        if matches!(
            expr_tokens.first().map(|tok| &tok.kind),
            Some(TokenKind::Eof)
        ) {
            self.error("no expression", start.location)?;
        }

        let value = const_expr(&expr_tokens)?;
        Ok(value)
    }
}

fn find_arg<'a>(args: &'a [MacroArg], tok: &Token) -> Option<&'a MacroArg> {
    let TokenKind::Ident(name) = &tok.kind else {
        return None;
    };
    args.iter().find(|arg| &arg.name == name)
}

fn token_text(tok: &Token) -> String {
    if tok.len != 0
        && let Some(file) = get_input_file(tok.location.file_no)
    {
        let bytes = file.contents.as_bytes();
        let start = tok.location.byte;
        let end = start.saturating_add(tok.len);
        if end <= bytes.len() {
            return String::from_utf8_lossy(&bytes[start..end]).into_owned();
        }
    }

    match &tok.kind {
        TokenKind::Keyword(keyword) => keyword.to_string(),
        TokenKind::Ident(name) => name.clone(),
        TokenKind::Punct(punct) => punct.to_string(),
        TokenKind::Num { value, .. } => value.to_string(),
        TokenKind::Str { bytes, .. } => {
            let inner = String::from_utf8_lossy(bytes)
                .trim_end_matches('\0')
                .replace('\\', "\\\\")
                .replace('"', "\\\"");
            format!("\"{inner}\"")
        }
        TokenKind::Eof => String::new(),
    }
}

fn quote_string(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 2);
    out.push('"');
    for ch in input.chars() {
        if ch == '\\' || ch == '"' {
            out.push('\\');
        }
        out.push(ch);
    }
    out.push('"');
    out
}

fn join_tokens(tokens: &[Token]) -> String {
    let mut parts = Vec::new();
    for (idx, tok) in tokens
        .iter()
        .take_while(|tok| !matches!(tok.kind, TokenKind::Eof))
        .enumerate()
    {
        if idx > 0 && tok.has_space {
            parts.push(" ".to_string());
        }
        parts.push(token_text(tok));
    }
    parts.join("")
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

impl Preprocessor {
    fn preprocess_tokens(&mut self) -> CompileResult<Vec<Token>> {
        let mut out = Vec::with_capacity(self.tokens.len());
        self.pos = 0;
        let mut cond_incl = vec![];

        while self.pos < self.tokens.len() {
            // Try to expand macros
            if let Some(expanded) = self.expand_macro()? {
                self.tokens = expanded;
                continue;
            }
            let tok = self.cur_tok();
            if matches!(tok.kind, TokenKind::Eof) {
                out.push(tok.clone());
                break;
            }

            if !is_hash(tok) {
                out.push(tok.clone());
                self.pos += 1;
                continue;
            }

            let start = tok.clone();
            self.pos += 1;
            if self.pos >= self.tokens.len() {
                break;
            }

            if let TokenKind::Ident(name) = &self.cur_tok().kind
                && name == "include"
            {
                self.pos += 1;
                if self.pos >= self.tokens.len() {
                    let loc = self.tokens[self.pos - 1].location;
                    self.error("expected a filename", loc)?;
                    unreachable!("expected error to return");
                }
                let token = self.cur_tok();

                let filename = match &token.kind {
                    TokenKind::Str { bytes, .. } => {
                        let slice = bytes.strip_suffix(&[0]).unwrap_or(bytes.as_slice());
                        String::from_utf8_lossy(slice).into_owned()
                    }
                    _ => self.error("expected a filename", token.location)?,
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
                self.pos += 1;
                self.skip_line();
                continue;
            }

            if let TokenKind::Ident(name) = &self.cur_tok().kind
                && name == "define"
            {
                self.pos += 1;
                if self.pos >= self.tokens.len() {
                    self.error("macro name must be an identifier", start.location)?;
                    unreachable!("expected error to return");
                }
                let tok = self.cur_tok();
                let TokenKind::Ident(_name) = &tok.kind else {
                    self.error("macro name must be an identifier", tok.location)?;
                    unreachable!("expected error to return");
                };
                self.read_macro_definition();
                continue;
            }

            if let TokenKind::Ident(name) = &self.cur_tok().kind
                && name == "undef"
            {
                self.pos += 1;
                if self.pos >= self.tokens.len() {
                    self.error("macro name must be an identifier", start.location)?;
                    unreachable!("expected error to return");
                }
                let tok = self.cur_tok();
                let TokenKind::Ident(name) = &tok.kind else {
                    self.error("macro name must be an identifier", tok.location)?;
                    unreachable!("expected error to return");
                };
                let macro_name = name.clone();
                self.pos += 1;
                self.skip_line();
                self.macros.push(Macro {
                    name: macro_name,
                    is_objlike: true,
                    deleted: true,
                    ..Macro::default()
                });
                continue;
            }

            if let TokenKind::Ident(name) = &self.cur_tok().kind
                && (name == "ifdef" || name == "ifndef")
            {
                let is_ifdef = name == "ifdef";
                self.pos += 1;
                let defined = self.find_macro(self.cur_tok()).is_some();
                let included = if is_ifdef { defined } else { !defined };
                cond_incl.push(CondIncl {
                    ctx: CondCtx::Then,
                    tok: start,
                    included,
                });
                self.pos += 1;
                self.skip_line();
                if !included {
                    self.skip_cond_incl();
                }
                continue;
            }

            if let TokenKind::Ident(name) = &self.cur_tok().kind
                && name == "if"
            {
                self.pos += 1;
                let value = self.eval_const_expr()?;
                cond_incl.push(CondIncl {
                    ctx: CondCtx::Then,
                    tok: start,
                    included: value != 0,
                });
                if value == 0 {
                    self.skip_cond_incl();
                }
                continue;
            }

            if let TokenKind::Ident(name) = &self.cur_tok().kind
                && name == "else"
            {
                let Some(last) = cond_incl.last_mut() else {
                    self.error("stray #else", start.location)?;
                    unreachable!("expected error to return");
                };
                if last.ctx == CondCtx::Else {
                    self.error("stray #else", start.location)?;
                    unreachable!("expected error to return");
                }
                last.ctx = CondCtx::Else;
                self.pos += 1;
                self.skip_line();
                if last.included {
                    self.skip_cond_incl();
                }
                continue;
            }

            if let TokenKind::Ident(name) = &self.cur_tok().kind
                && name == "elif"
            {
                let Some(last) = cond_incl.last_mut() else {
                    self.error("stray #elif", start.location)?;
                    unreachable!("expected error to return");
                };
                if last.ctx == CondCtx::Else {
                    self.error("stray #elif", start.location)?;
                    unreachable!("expected error to return");
                }
                last.ctx = CondCtx::Elif;

                self.pos += 1;
                let value = self.eval_const_expr()?;
                if !last.included && value != 0 {
                    last.included = true;
                } else {
                    self.skip_cond_incl();
                }
                continue;
            }

            if let TokenKind::Ident(name) = &self.cur_tok().kind
                && name == "endif"
            {
                if cond_incl.is_empty() {
                    self.error("stray #endif", start.location)?;
                }
                cond_incl.pop();
                self.pos += 1;
                self.skip_line();
                continue;
            }

            if self.cur_tok().at_bol {
                continue;
            }

            self.error("invalid preprocessor directive", self.cur_tok().location)?;
            unreachable!("expected error to return");
        }

        if let Some(start) = cond_incl.first() {
            self.error("unterminated conditional directive", start.tok.location)?;
        }

        Ok(out)
    }

    fn new_str_token(&self, text: &str, tmpl: &Token) -> CompileResult<Token> {
        let mut tokens = tokenize(text, tmpl.location.file_no)?;
        let mut tok = tokens
            .drain(..)
            .next()
            .ok_or_else(|| CompileError::new("failed to tokenize string"))?;
        tok.location = tmpl.location;
        tok.at_bol = tmpl.at_bol;
        tok.has_space = tmpl.has_space;
        Ok(tok)
    }

    fn stringize(&self, hash: &Token, arg: &[Token]) -> CompileResult<Token> {
        let joined = join_tokens(arg);
        let quoted = quote_string(&joined);
        self.new_str_token(&quoted, hash)
    }

    fn paste(&self, lhs: &Token, rhs: &Token) -> CompileResult<Token> {
        let buf = format!("{}{}", token_text(lhs), token_text(rhs));
        let mut tokens = tokenize(&buf, lhs.location.file_no)?;
        let mut tok = tokens
            .drain(..)
            .next()
            .ok_or_else(|| CompileError::new("failed to tokenize pasted token"))?;
        let next = tokens
            .drain(..)
            .next()
            .ok_or_else(|| CompileError::new("failed to tokenize pasted token"))?;
        if !matches!(next.kind, TokenKind::Eof) || !tokens.is_empty() {
            return self.error(
                format!("pasting forms '{buf}', an invalid token"),
                lhs.location,
            );
        }
        tok.location = lhs.location;
        tok.at_bol = lhs.at_bol;
        tok.has_space = lhs.has_space;
        Ok(tok)
    }

    fn subst(&self, body: &[Token], args: &[MacroArg]) -> CompileResult<Vec<Token>> {
        let mut result = Vec::new();
        let mut i = 0;

        while i < body.len() {
            let tok = &body[i];
            if matches!(tok.kind, TokenKind::Eof) {
                break;
            }

            if matches!(tok.kind, TokenKind::Punct(Punct::Hash)) {
                let Some(next) = body.get(i + 1) else {
                    self.error("'#' is not followed by a macro parameter", tok.location)?;
                    unreachable!("expected error to return");
                };
                let Some(arg) = find_arg(args, next) else {
                    self.error("'#' is not followed by a macro parameter", next.location)?;
                    unreachable!("expected error to return");
                };
                let stringized = self.stringize(tok, &arg.tokens)?;
                result.push(stringized);
                i += 2;
                continue;
            }

            if matches!(tok.kind, TokenKind::Punct(Punct::HashHash)) {
                if result.is_empty() {
                    self.error(
                        "'##' cannot appear at start of macro expansion",
                        tok.location,
                    )?;
                    unreachable!("expected error to return");
                }
                let Some(next) = body.get(i + 1) else {
                    self.error("'##' cannot appear at end of macro expansion", tok.location)?;
                    unreachable!("expected error to return");
                };
                if matches!(next.kind, TokenKind::Eof) {
                    self.error("'##' cannot appear at end of macro expansion", tok.location)?;
                    unreachable!("expected error to return");
                }

                if let Some(arg) = find_arg(args, next) {
                    let is_empty = arg
                        .tokens
                        .first()
                        .is_some_and(|tok| matches!(tok.kind, TokenKind::Eof));
                    if !is_empty {
                        let pasted =
                            self.paste(result.last().expect("pasted lhs"), &arg.tokens[0])?;
                        *result.last_mut().expect("pasted lhs") = pasted;
                        for tok in arg.tokens.iter().skip(1) {
                            if matches!(tok.kind, TokenKind::Eof) {
                                break;
                            }
                            result.push(tok.clone());
                        }
                    }
                    i += 2;
                    continue;
                }

                let pasted = self.paste(result.last().expect("pasted lhs"), next)?;
                *result.last_mut().expect("pasted lhs") = pasted;
                i += 2;
                continue;
            }

            if let Some(arg) = find_arg(args, tok)
                && let Some(next) = body.get(i + 1)
                && matches!(next.kind, TokenKind::Punct(Punct::HashHash))
            {
                let rhs = body.get(i + 2).ok_or_else(|| {
                    CompileError::at(
                        "'##' cannot appear at end of macro expansion",
                        next.location,
                    )
                })?;
                let is_empty = arg
                    .tokens
                    .first()
                    .is_some_and(|tok| matches!(tok.kind, TokenKind::Eof));
                if is_empty {
                    if let Some(arg2) = find_arg(args, rhs) {
                        for tok in &arg2.tokens {
                            if matches!(tok.kind, TokenKind::Eof) {
                                break;
                            }
                            result.push(tok.clone());
                        }
                    } else {
                        result.push(rhs.clone());
                    }
                    i += 3;
                    continue;
                }

                for tok in &arg.tokens {
                    if matches!(tok.kind, TokenKind::Eof) {
                        break;
                    }
                    result.push(tok.clone());
                }
                i += 1;
                continue;
            }

            // Check if this token is a parameter that should be substituted.
            if let Some(arg) = find_arg(args, tok) {
                // Macro arguments are completely macro-expanded before substitution.
                let expanded = self.expand_macros_only(arg.tokens.clone())?;
                for exp_tok in &expanded {
                    if matches!(exp_tok.kind, TokenKind::Eof) {
                        break;
                    }
                    result.push(exp_tok.clone());
                }
            } else {
                // Not a parameter, just copy the token.
                result.push(tok.clone());
            }

            i += 1;
        }

        Ok(result)
    }
}

/// Entry point of the preprocessor.
pub fn preprocess(tokens: Vec<Token>) -> CompileResult<Vec<Token>> {
    let mut preprocessor = Preprocessor::new(tokens);
    preprocessor.preprocess_tokens()
}
