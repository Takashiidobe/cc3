use crate::ast::{BinaryOp, Expr, Program, Stmt, UnaryOp};
use crate::error::{CompileError, CompileResult};
use crate::lexer::{Keyword, Punct, Token, TokenKind};

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

        self.expect_punct(Punct::LParen)?;
        self.expect_punct(Punct::RParen)?;
        self.expect_punct(Punct::LBrace)?;

        let body = vec![self.parse_stmt()?];

        self.expect_punct(Punct::RBrace)?;
        self.expect_eof()?;

        Ok(Program { body })
    }

    fn parse_stmt(&mut self) -> CompileResult<Stmt> {
        if self.consume_keyword(Keyword::Return) {
            let expr = self.parse_expr()?;
            self.expect_punct(Punct::Semi)?;
            return Ok(Stmt::Return(expr));
        }

        Err(self.error_expected("a statement"))
    }

    fn parse_expr(&mut self) -> CompileResult<Expr> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> CompileResult<Expr> {
        let mut expr = self.parse_relational()?;

        loop {
            let op = if self.consume_punct(Punct::EqEq) {
                BinaryOp::Eq
            } else if self.consume_punct(Punct::NotEq) {
                BinaryOp::Ne
            } else {
                break;
            };

            let rhs = self.parse_relational()?;
            expr = Expr::Binary {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            };
        }

        Ok(expr)
    }

    fn parse_relational(&mut self) -> CompileResult<Expr> {
        let mut expr = self.parse_add()?;

        loop {
            if self.consume_punct(Punct::Less) {
                let rhs = self.parse_add()?;
                expr = Expr::Binary {
                    op: BinaryOp::Lt,
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                };
                continue;
            }

            if self.consume_punct(Punct::LessEq) {
                let rhs = self.parse_add()?;
                expr = Expr::Binary {
                    op: BinaryOp::Le,
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                };
                continue;
            }

            if self.consume_punct(Punct::Greater) {
                let rhs = self.parse_add()?;
                expr = Expr::Binary {
                    op: BinaryOp::Lt,
                    lhs: Box::new(rhs),
                    rhs: Box::new(expr),
                };
                continue;
            }

            if self.consume_punct(Punct::GreaterEq) {
                let rhs = self.parse_add()?;
                expr = Expr::Binary {
                    op: BinaryOp::Le,
                    lhs: Box::new(rhs),
                    rhs: Box::new(expr),
                };
                continue;
            }

            break;
        }

        Ok(expr)
    }

    fn parse_add(&mut self) -> CompileResult<Expr> {
        let mut expr = self.parse_mul()?;

        loop {
            let op = if self.consume_punct(Punct::Plus) {
                BinaryOp::Add
            } else if self.consume_punct(Punct::Minus) {
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
        let mut expr = self.parse_unary()?;

        loop {
            let op = if self.consume_punct(Punct::Star) {
                BinaryOp::Mul
            } else if self.consume_punct(Punct::Slash) {
                BinaryOp::Div
            } else {
                break;
            };

            let rhs = self.parse_unary()?;
            expr = Expr::Binary {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            };
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> CompileResult<Expr> {
        if self.consume_punct(Punct::Plus) {
            return self.parse_unary();
        }
        if self.consume_punct(Punct::Minus) {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(expr),
            });
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> CompileResult<Expr> {
        let token = self.peek().clone();
        match token.kind {
            TokenKind::Punct(Punct::LParen) => {
                self.pos += 1;
                let expr = self.parse_expr()?;
                self.expect_punct(Punct::RParen)?;
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

    fn expect_punct(&mut self, punct: Punct) -> CompileResult<()> {
        let token = self.peek().clone();
        match token.kind {
            TokenKind::Punct(found) if found == punct => {
                self.pos += 1;
                Ok(())
            }
            _ => Err(self.error_expected(format!("'{punct}'"))),
        }
    }

    fn consume_punct(&mut self, punct: Punct) -> bool {
        let token = self.peek().clone();
        if matches!(token.kind, TokenKind::Punct(found) if found == punct) {
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
            TokenKind::Punct(punct) => format!("'{punct}'"),
            TokenKind::Eof => "end of file".to_string(),
        }
    }
}
