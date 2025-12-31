use crate::ast::{BinaryOp, Expr, Program, Stmt};
use crate::error::{CompileError, CompileResult};
use crate::lexer::{Keyword, Token, TokenKind};

pub fn parse(tokens: &[Token]) -> CompileResult<Program> {
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, pos: 0 }
    }

    fn parse_program(&mut self) -> CompileResult<Program> {
        self.expect_keyword(Keyword::Int)?;
        let name = self.expect_ident()?;
        if name != "main" {
            return Err(self.error_here(format!("expected function name 'main', got '{name}'")));
        }

        self.expect_punct('(')?;
        self.expect_punct(')')?;
        self.expect_punct('{')?;

        let body = vec![self.parse_stmt()?];

        self.expect_punct('}')?;
        self.expect_eof()?;

        Ok(Program { body })
    }

    fn parse_stmt(&mut self) -> CompileResult<Stmt> {
        if self.consume_keyword(Keyword::Return) {
            let expr = self.parse_expr()?;
            self.expect_punct(';')?;
            return Ok(Stmt::Return(expr));
        }

        Err(self.error_expected("a statement"))
    }

    fn parse_expr(&mut self) -> CompileResult<Expr> {
        self.parse_add()
    }

    fn parse_add(&mut self) -> CompileResult<Expr> {
        let mut expr = self.parse_mul()?;

        loop {
            let op = if self.consume_punct('+') {
                BinaryOp::Add
            } else if self.consume_punct('-') {
                BinaryOp::Sub
            } else {
                break;
            };

            let rhs = self.parse_mul()?;
            expr = Expr::Binary {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            };
        }

        Ok(expr)
    }

    fn parse_mul(&mut self) -> CompileResult<Expr> {
        let mut expr = self.parse_primary()?;

        loop {
            let op = if self.consume_punct('*') {
                BinaryOp::Mul
            } else if self.consume_punct('/') {
                BinaryOp::Div
            } else {
                break;
            };

            let rhs = self.parse_primary()?;
            expr = Expr::Binary {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            };
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> CompileResult<Expr> {
        let token = self.peek().clone();
        match token.kind {
            TokenKind::Punct('(') => {
                self.pos += 1;
                let expr = self.parse_expr()?;
                self.expect_punct(')')?;
                Ok(expr)
            }
            TokenKind::Num(value) => {
                self.pos += 1;
                Ok(Expr::Num(value))
            }
            _ => Err(self.error_expected("a primary expression")),
        }
    }

    fn expect_keyword(&mut self, kw: Keyword) -> CompileResult<()> {
        let token = self.peek().clone();
        match token.kind {
            TokenKind::Keyword(found) if found == kw => {
                self.pos += 1;
                Ok(())
            }
            _ => Err(self.error_expected(format!("keyword '{kw:?}'"))),
        }
    }

    fn consume_keyword(&mut self, kw: Keyword) -> bool {
        let token = self.peek().clone();
        if matches!(token.kind, TokenKind::Keyword(found) if found == kw) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn expect_ident(&mut self) -> CompileResult<String> {
        let token = self.peek().clone();
        match token.kind {
            TokenKind::Ident(name) => {
                self.pos += 1;
                Ok(name)
            }
            _ => Err(self.error_expected("identifier")),
        }
    }

    fn expect_punct(&mut self, ch: char) -> CompileResult<()> {
        let token = self.peek().clone();
        match token.kind {
            TokenKind::Punct(found) if found == ch => {
                self.pos += 1;
                Ok(())
            }
            _ => Err(self.error_expected(format!("'{ch}'"))),
        }
    }

    fn consume_punct(&mut self, ch: char) -> bool {
        let token = self.peek().clone();
        if matches!(token.kind, TokenKind::Punct(found) if found == ch) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn expect_eof(&mut self) -> CompileResult<()> {
        let token = self.peek().clone();
        match token.kind {
            TokenKind::Eof => Ok(()),
            _ => Err(self.error_expected("end of file")),
        }
    }

    fn peek(&self) -> &Token {
        self.tokens
            .get(self.pos)
            .unwrap_or_else(|| &self.tokens[self.tokens.len().saturating_sub(1)])
    }

    fn error_here(&self, message: impl Into<String>) -> CompileError {
        let location = self.peek().location;
        CompileError::at(message, location)
    }

    fn error_expected(&self, expected: impl Into<String>) -> CompileError {
        let expected = expected.into();
        let found = self.token_desc(self.peek());
        self.error_here(format!("expected {expected}, found {found}"))
    }

    fn token_desc(&self, token: &Token) -> String {
        match &token.kind {
            TokenKind::Keyword(kw) => format!("keyword '{kw:?}'"),
            TokenKind::Ident(name) => format!("identifier '{name}'"),
            TokenKind::Num(value) => format!("number {value}"),
            TokenKind::Punct(ch) => format!("'{ch}'"),
            TokenKind::Eof => "end of file".to_string(),
        }
    }
}
