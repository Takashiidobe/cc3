use crate::error::{CompileError, CompileResult};
use crate::lexer::{Punct, Token, TokenKind};

fn is_hash(tok: &Token) -> bool {
    tok.at_bol && matches!(tok.kind, TokenKind::Punct(Punct::Hash))
}

fn preprocess_tokens(tokens: Vec<Token>) -> CompileResult<Vec<Token>> {
    let mut out = Vec::with_capacity(tokens.len());
    let mut i = 0;

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

        i += 1;
        if i >= tokens.len() {
            break;
        }

        if tokens[i].at_bol {
            continue;
        }

        return Err(CompileError::at(
            "invalid preprocessor directive",
            tokens[i].location,
        ));
    }

    Ok(out)
}

/// Entry point of the preprocessor.
pub fn preprocess(tokens: Vec<Token>) -> CompileResult<Vec<Token>> {
    preprocess_tokens(tokens)
}
