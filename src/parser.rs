use crate::ast::{BinaryOp, Expr, ExprKind, Obj, Program, Stmt, StmtKind, Type, UnaryOp};
use crate::error::{CompileError, CompileResult};
use crate::lexer::{Keyword, Punct, Token, TokenKind};

pub fn parse(tokens: &[Token]) -> CompileResult<Program> {
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    locals: Vec<Obj>,
    globals: Vec<Obj>,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            pos: 0,
            locals: Vec::new(),
            globals: Vec::new(),
        }
    }

    fn parse_program(&mut self) -> CompileResult<Program> {
        while !self.check_eof() {
            let basety = self.parse_declspec()?;

            // Function
            if self.is_function()? {
                self.parse_function_with_basety(basety)?;
                continue;
            }

            // Global variable
            self.parse_global_variable(basety)?;
        }
        Ok(Program {
            globals: std::mem::take(&mut self.globals),
        })
    }

    fn parse_declspec(&mut self) -> CompileResult<Type> {
        self.expect_keyword(Keyword::Int)?;
        Ok(Type::Int)
    }

    fn parse_function_with_basety(&mut self, _basety: Type) -> CompileResult<()> {
        let name = self.expect_ident()?;

        self.expect_punct(Punct::LParen)?;

        self.locals.clear();

        // Parse function parameters
        let mut params = Vec::new();
        if !self.check_punct(Punct::RParen) {
            loop {
                self.expect_keyword(Keyword::Int)?;
                let param_name = self.expect_ident()?;
                // Use new_lvar to assign correct offset
                let _idx = self.new_lvar(param_name.clone(), Type::Int);
                // Get the newly added param
                if let Some(param) = self.locals.last() {
                    params.push(param.clone());
                }

                if !self.consume_punct(Punct::Comma) {
                    break;
                }
            }
        }

        self.expect_punct(Punct::RParen)?;
        self.expect_punct(Punct::LBrace)?;

        let mut body = Vec::new();
        while !self.check_punct(Punct::RBrace) {
            body.push(self.parse_stmt()?);
        }

        self.expect_punct(Punct::RBrace)?;

        for stmt in &mut body {
            self.add_type_stmt(stmt)?;
        }

        // Calculate stack size based on the total size of all locals
        let total_size = if self.locals.is_empty() {
            0
        } else {
            -self.locals.last().unwrap().offset
        };
        let stack_size = align_to(total_size, 16);
        let locals = std::mem::take(&mut self.locals);

        // Create function object
        let func = Obj {
            name,
            ty: Type::Func(Box::new(Type::Int)),
            is_local: false,
            offset: 0,
            is_function: true,
            params,
            body,
            locals,
            stack_size,
        };
        self.globals.push(func);
        Ok(())
    }

    fn is_function(&self) -> CompileResult<bool> {
        // Look ahead to see if this is a function or variable declaration
        // A function has '(' after the identifier, a variable has ',' or ';'
        let mut pos = self.pos;

        // Skip past the declarator to find LParen (function) or semicolon/comma (variable)
        while pos < self.tokens.len() {
            match &self.tokens[pos].kind {
                TokenKind::Punct(Punct::LParen) => return Ok(true),
                TokenKind::Punct(Punct::Semicolon) | TokenKind::Punct(Punct::Comma) => {
                    return Ok(false);
                }
                _ => pos += 1,
            }
        }
        Ok(false)
    }

    fn parse_global_variable(&mut self, basety: Type) -> CompileResult<()> {
        let mut first = true;

        while !self.check_punct(Punct::Semicolon) {
            if !first {
                self.expect_punct(Punct::Comma)?;
            }
            first = false;

            let (ty, name_token) = self.parse_declarator(basety.clone())?;
            let name = match name_token.kind {
                TokenKind::Ident(name) => name,
                _ => return Err(self.error_at(name_token.location, "variable name expected")),
            };
            self.new_gvar(name, ty);
        }

        self.expect_punct(Punct::Semicolon)?;
        Ok(())
    }

    fn parse_stmt(&mut self) -> CompileResult<Stmt> {
        if matches!(self.peek().kind, TokenKind::Keyword(Keyword::Int)) {
            return self.parse_declaration();
        }

        if self.consume_keyword(Keyword::Return) {
            let expr = self.parse_expr()?;
            self.expect_punct(Punct::Semicolon)?;
            return Ok(self.stmt_last(StmtKind::Return(expr)));
        }

        if self.consume_keyword(Keyword::If) {
            self.expect_punct(Punct::LParen)?;
            let cond = self.parse_expr()?;
            self.expect_punct(Punct::RParen)?;
            let then = self.parse_stmt()?;
            let els = if self.consume_keyword(Keyword::Else) {
                Some(Box::new(self.parse_stmt()?))
            } else {
                None
            };
            return Ok(self.stmt_last(StmtKind::If {
                cond,
                then: Box::new(then),
                els,
            }));
        }

        if self.consume_keyword(Keyword::For) {
            self.expect_punct(Punct::LParen)?;
            let init = self.parse_expr_stmt()?;
            let cond = if self.consume_punct(Punct::Semicolon) {
                None
            } else {
                let expr = self.parse_expr()?;
                self.expect_punct(Punct::Semicolon)?;
                Some(expr)
            };
            let inc = if self.consume_punct(Punct::RParen) {
                None
            } else {
                let expr = self.parse_expr()?;
                self.expect_punct(Punct::RParen)?;
                Some(expr)
            };
            let body = self.parse_stmt()?;
            return Ok(self.stmt_last(StmtKind::For {
                init: Some(Box::new(init)),
                cond,
                inc,
                body: Box::new(body),
            }));
        }

        if self.consume_keyword(Keyword::While) {
            self.expect_punct(Punct::LParen)?;
            let cond = self.parse_expr()?;
            self.expect_punct(Punct::RParen)?;
            let body = self.parse_stmt()?;
            return Ok(self.stmt_last(StmtKind::For {
                init: None,
                cond: Some(cond),
                inc: None,
                body: Box::new(body),
            }));
        }

        if self.consume_punct(Punct::LBrace) {
            let mut stmts = Vec::new();
            while !self.check_punct(Punct::RBrace) {
                stmts.push(self.parse_stmt()?);
            }
            self.expect_punct(Punct::RBrace)?;
            return Ok(self.stmt_last(StmtKind::Block(stmts)));
        }

        self.parse_expr_stmt()
    }

    fn parse_declaration(&mut self) -> CompileResult<Stmt> {
        let location = self.peek().location;
        self.expect_keyword(Keyword::Int)?;

        let mut stmts = Vec::new();
        let mut first = true;

        while !self.check_punct(Punct::Semicolon) {
            if !first {
                self.expect_punct(Punct::Comma)?;
            }
            first = false;

            let (ty, name_token) = self.parse_declarator(Type::Int)?;
            let name = match name_token.kind {
                TokenKind::Ident(name) => name,
                _ => unreachable!("parse_declarator only returns identifiers"),
            };
            let idx = self.new_lvar(name, ty);
            stmts.push(self.stmt_at(StmtKind::Decl(idx), name_token.location));

            if self.consume_punct(Punct::Assign) {
                let assign_location = self.last_location();
                let rhs = self.parse_assign()?;
                let lhs = self.expr_at(
                    ExprKind::Var {
                        idx,
                        is_local: true,
                    },
                    name_token.location,
                );
                let assign = self.expr_at(
                    ExprKind::Assign {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    assign_location,
                );
                stmts.push(self.stmt_at(StmtKind::Expr(assign), assign_location));
            }
        }

        self.expect_punct(Punct::Semicolon)?;
        Ok(self.stmt_at(StmtKind::Block(stmts), location))
    }

    fn parse_declarator(&mut self, mut ty: Type) -> CompileResult<(Type, Token)> {
        while self.consume_punct(Punct::Star) {
            ty = Type::Ptr(Box::new(ty));
        }

        let token = self.expect_ident_token()?;

        // Parse array suffixes recursively to get correct nesting order
        ty = self.parse_array_suffix(ty)?;

        Ok((ty, token))
    }

    fn parse_array_suffix(&mut self, ty: Type) -> CompileResult<Type> {
        if !self.consume_punct(Punct::LBracket) {
            return Ok(ty);
        }

        let size_token = self.peek().clone();
        let len = match size_token.kind {
            TokenKind::Num(len) => {
                self.pos += 1;
                len as i32
            }
            _ => {
                return Err(self.error_expected("array size"));
            }
        };

        self.expect_punct(Punct::RBracket)?;

        // Recursively parse remaining array suffixes
        let ty = self.parse_array_suffix(ty)?;

        // Build array type with the result
        Ok(Type::Array {
            base: Box::new(ty),
            len,
        })
    }

    fn parse_expr_stmt(&mut self) -> CompileResult<Stmt> {
        if self.consume_punct(Punct::Semicolon) {
            return Ok(self.stmt_last(StmtKind::Block(Vec::new())));
        }

        let location = self.peek().location;
        let expr = self.parse_expr()?;
        self.expect_punct(Punct::Semicolon)?;
        Ok(self.stmt_at(StmtKind::Expr(expr), location))
    }

    fn parse_expr(&mut self) -> CompileResult<Expr> {
        self.parse_assign()
    }

    fn parse_assign(&mut self) -> CompileResult<Expr> {
        let expr = self.parse_equality()?;
        if self.consume_punct(Punct::Assign) {
            if !matches!(expr.kind, ExprKind::Var { .. } | ExprKind::Deref(_)) {
                return Err(self.error_here("invalid assignment target"));
            }
            let location = self.last_location();
            let rhs = self.parse_assign()?;
            return Ok(self.expr_at(
                ExprKind::Assign {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                },
                location,
            ));
        }
        Ok(expr)
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

            let location = self.last_location();
            let rhs = self.parse_relational()?;
            expr = self.expr_at(
                ExprKind::Binary {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                },
                location,
            );
        }

        Ok(expr)
    }

    fn parse_relational(&mut self) -> CompileResult<Expr> {
        let mut expr = self.parse_add()?;

        loop {
            if self.consume_punct(Punct::Less) {
                let location = self.last_location();
                let rhs = self.parse_add()?;
                expr = self.expr_at(
                    ExprKind::Binary {
                        op: BinaryOp::Lt,
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    },
                    location,
                );
                continue;
            }

            if self.consume_punct(Punct::LessEq) {
                let location = self.last_location();
                let rhs = self.parse_add()?;
                expr = self.expr_at(
                    ExprKind::Binary {
                        op: BinaryOp::Le,
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                    },
                    location,
                );
                continue;
            }

            if self.consume_punct(Punct::Greater) {
                let location = self.last_location();
                let rhs = self.parse_add()?;
                expr = self.expr_at(
                    ExprKind::Binary {
                        op: BinaryOp::Lt,
                        lhs: Box::new(rhs),
                        rhs: Box::new(expr),
                    },
                    location,
                );
                continue;
            }

            if self.consume_punct(Punct::GreaterEq) {
                let location = self.last_location();
                let rhs = self.parse_add()?;
                expr = self.expr_at(
                    ExprKind::Binary {
                        op: BinaryOp::Le,
                        lhs: Box::new(rhs),
                        rhs: Box::new(expr),
                    },
                    location,
                );
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
                Some(BinaryOp::Add)
            } else if self.consume_punct(Punct::Minus) {
                Some(BinaryOp::Sub)
            } else {
                None
            };

            let Some(op) = op else { break };
            let location = self.last_location();
            let rhs = self.parse_mul()?;
            expr = match op {
                BinaryOp::Add => self.new_add(expr, rhs, location)?,
                BinaryOp::Sub => self.new_sub(expr, rhs, location)?,
                _ => unreachable!("parse_add only handles +/-"),
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

            let location = self.last_location();
            let rhs = self.parse_unary()?;
            expr = self.expr_at(
                ExprKind::Binary {
                    op,
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                },
                location,
            );
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> CompileResult<Expr> {
        if self.consume_punct(Punct::Plus) {
            return self.parse_unary();
        }
        if self.consume_punct(Punct::Minus) {
            let location = self.last_location();
            let expr = self.parse_unary()?;
            return Ok(self.expr_at(
                ExprKind::Unary {
                    op: UnaryOp::Neg,
                    expr: Box::new(expr),
                },
                location,
            ));
        }

        if self.consume_punct(Punct::Amp) {
            let location = self.last_location();
            let expr = self.parse_unary()?;
            return Ok(self.expr_at(ExprKind::Addr(Box::new(expr)), location));
        }

        if self.consume_punct(Punct::Star) {
            let location = self.last_location();
            let expr = self.parse_unary()?;
            return Ok(self.expr_at(ExprKind::Deref(Box::new(expr)), location));
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> CompileResult<Expr> {
        let mut expr = self.parse_primary()?;

        // Handle array subscript: x[y] is equivalent to *(x+y)
        while self.consume_punct(Punct::LBracket) {
            let location = self.last_location();
            let index = self.parse_expr()?;
            self.expect_punct(Punct::RBracket)?;

            // Transform x[y] to *(x+y)
            let add_expr = self.new_add(expr, index, location)?;
            expr = self.expr_at(ExprKind::Deref(Box::new(add_expr)), location);
        }

        Ok(expr)
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
            TokenKind::Keyword(Keyword::Sizeof) => {
                let location = token.location;
                self.pos += 1;
                let mut expr = self.parse_unary()?;
                self.add_type_expr(&mut expr)?;
                let size = expr.ty.as_ref().map(|ty| ty.size()).unwrap_or(8);
                Ok(self.expr_at(ExprKind::Num(size), location))
            }
            TokenKind::Ident(ref name) => {
                if matches!(self.peek_n(1).kind, TokenKind::Punct(Punct::LParen)) {
                    return self.parse_funcall(token);
                }

                let name = name.clone();
                let (idx, is_local) = match self.find_var(&name) {
                    Some((idx, is_local)) => (idx, is_local),
                    None => return Err(self.error_at(token.location, "undefined variable")),
                };
                self.pos += 1;
                Ok(self.expr_at(ExprKind::Var { idx, is_local }, token.location))
            }
            TokenKind::Num(value) => {
                self.pos += 1;
                Ok(self.expr_at(ExprKind::Num(value), token.location))
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

    fn expect_ident_token(&mut self) -> CompileResult<Token> {
        let token = self.peek().clone();
        match token.kind {
            TokenKind::Ident(_) => {
                self.pos += 1;
                Ok(token)
            }
            _ => Err(self.error_expected("a variable name")),
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

    fn check_punct(&self, punct: Punct) -> bool {
        matches!(self.peek().kind, TokenKind::Punct(found) if found == punct)
    }

    #[allow(dead_code)]
    fn expect_eof(&mut self) -> CompileResult<()> {
        let token = self.peek().clone();
        match token.kind {
            TokenKind::Eof => Ok(()),
            _ => Err(self.error_expected("end of file")),
        }
    }

    fn check_eof(&self) -> bool {
        matches!(self.peek().kind, TokenKind::Eof)
    }

    fn peek(&self) -> &Token {
        self.tokens
            .get(self.pos)
            .unwrap_or_else(|| &self.tokens[self.tokens.len().saturating_sub(1)])
    }

    fn peek_n(&self, n: usize) -> &Token {
        self.tokens
            .get(self.pos + n)
            .unwrap_or_else(|| &self.tokens[self.tokens.len().saturating_sub(1)])
    }

    fn last_location(&self) -> crate::error::SourceLocation {
        self.tokens
            .get(self.pos.saturating_sub(1))
            .map(|token| token.location)
            .unwrap_or_else(|| self.peek().location)
    }

    fn stmt_last(&self, kind: StmtKind) -> Stmt {
        self.stmt_at(kind, self.last_location())
    }

    fn stmt_at(&self, kind: StmtKind, location: crate::error::SourceLocation) -> Stmt {
        Stmt { kind, location }
    }

    fn expr_at(&self, kind: ExprKind, location: crate::error::SourceLocation) -> Expr {
        Expr {
            kind,
            location,
            ty: None,
        }
    }

    fn error_here(&self, message: impl Into<String>) -> CompileError {
        let location = self.peek().location;
        CompileError::at(message, location)
    }

    fn error_at(
        &self,
        location: crate::error::SourceLocation,
        message: impl Into<String>,
    ) -> CompileError {
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

    fn find_var(&self, name: &str) -> Option<(usize, bool)> {
        // Search locals first
        if let Some(idx) = self.locals.iter().position(|var| var.name == name) {
            return Some((idx, true));
        }
        // Search globals
        if let Some(idx) = self.globals.iter().position(|var| var.name == name) {
            return Some((idx, false));
        }
        None
    }

    fn new_lvar(&mut self, name: String, ty: Type) -> usize {
        let offset = if self.locals.is_empty() {
            -(ty.size() as i32)
        } else {
            let last_offset = self.locals.last().unwrap().offset;
            last_offset - ty.size() as i32
        };
        self.locals.push(Obj {
            name,
            ty,
            is_local: true,
            offset,
            is_function: false,
            params: Vec::new(),
            body: Vec::new(),
            locals: Vec::new(),
            stack_size: 0,
        });
        self.locals.len() - 1
    }

    fn new_gvar(&mut self, name: String, ty: Type) -> usize {
        self.globals.push(Obj {
            name,
            ty,
            is_local: false,
            offset: 0,
            is_function: false,
            params: Vec::new(),
            body: Vec::new(),
            locals: Vec::new(),
            stack_size: 0,
        });
        self.globals.len() - 1
    }

    fn parse_funcall(&mut self, name_token: Token) -> CompileResult<Expr> {
        let name = match name_token.kind {
            TokenKind::Ident(name) => name,
            _ => unreachable!("parse_funcall expects identifier token"),
        };

        self.pos += 1;
        self.expect_punct(Punct::LParen)?;

        let mut args = Vec::new();
        if !self.check_punct(Punct::RParen) {
            loop {
                let arg = self.parse_assign()?;
                args.push(arg);
                if args.len() > 6 {
                    return Err(self.error_at(
                        name_token.location,
                        "function call can have up to 6 arguments",
                    ));
                }
                if self.consume_punct(Punct::Comma) {
                    continue;
                }
                break;
            }
        }

        self.expect_punct(Punct::RParen)?;
        Ok(self.expr_at(ExprKind::Call { name, args }, name_token.location))
    }

    fn new_add(
        &self,
        mut lhs: Expr,
        mut rhs: Expr,
        location: crate::error::SourceLocation,
    ) -> CompileResult<Expr> {
        self.add_type_expr(&mut lhs)?;
        self.add_type_expr(&mut rhs)?;

        let mut lhs_ty = lhs.ty.clone().unwrap_or(Type::Int);
        let mut rhs_ty = rhs.ty.clone().unwrap_or(Type::Int);

        if lhs_ty.is_integer() && rhs_ty.is_integer() {
            return Ok(self.expr_at(
                ExprKind::Binary {
                    op: BinaryOp::Add,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                location,
            ));
        }

        // Check if types have base (pointers or arrays)
        let lhs_has_base = lhs_ty.base().is_some();
        let rhs_has_base = rhs_ty.base().is_some();

        if lhs_has_base && rhs_has_base {
            return Err(self.error_at(location, "invalid operands"));
        }

        if !lhs_has_base && rhs_has_base {
            std::mem::swap(&mut lhs, &mut rhs);
            std::mem::swap(&mut lhs_ty, &mut rhs_ty);
        }

        let base_size = lhs_ty.base().map(|base| base.size()).unwrap_or(8);
        let scale = self.expr_at(ExprKind::Num(base_size), location);
        let rhs = self.expr_at(
            ExprKind::Binary {
                op: BinaryOp::Mul,
                lhs: Box::new(rhs),
                rhs: Box::new(scale),
            },
            location,
        );
        let mut expr = self.expr_at(
            ExprKind::Binary {
                op: BinaryOp::Add,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            location,
        );
        self.add_type_expr(&mut expr)?;
        Ok(expr)
    }

    fn new_sub(
        &self,
        mut lhs: Expr,
        mut rhs: Expr,
        location: crate::error::SourceLocation,
    ) -> CompileResult<Expr> {
        self.add_type_expr(&mut lhs)?;
        self.add_type_expr(&mut rhs)?;

        let lhs_ty = lhs.ty.clone().unwrap_or(Type::Int);
        let rhs_ty = rhs.ty.clone().unwrap_or(Type::Int);

        if lhs_ty.is_integer() && rhs_ty.is_integer() {
            return Ok(self.expr_at(
                ExprKind::Binary {
                    op: BinaryOp::Sub,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                location,
            ));
        }

        if lhs_ty.base().is_some() && rhs_ty.is_integer() {
            let base_size = lhs_ty.base().map(|base| base.size()).unwrap_or(8);
            let scale = self.expr_at(ExprKind::Num(base_size), location);
            let rhs = self.expr_at(
                ExprKind::Binary {
                    op: BinaryOp::Mul,
                    lhs: Box::new(rhs),
                    rhs: Box::new(scale),
                },
                location,
            );
            let mut expr = self.expr_at(
                ExprKind::Binary {
                    op: BinaryOp::Sub,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                location,
            );
            self.add_type_expr(&mut expr)?;
            return Ok(expr);
        }

        if lhs_ty.base().is_some() && rhs_ty.base().is_some() {
            let base_size = lhs_ty.base().map(|base| base.size()).unwrap_or(8);
            let scale = self.expr_at(ExprKind::Num(base_size), location);
            let mut sub_expr = self.expr_at(
                ExprKind::Binary {
                    op: BinaryOp::Sub,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                location,
            );
            sub_expr.ty = Some(Type::Int);
            let mut expr = self.expr_at(
                ExprKind::Binary {
                    op: BinaryOp::Div,
                    lhs: Box::new(sub_expr),
                    rhs: Box::new(scale),
                },
                location,
            );
            self.add_type_expr(&mut expr)?;
            return Ok(expr);
        }

        Err(self.error_at(location, "invalid operands"))
    }

    fn add_type_stmt(&self, stmt: &mut Stmt) -> CompileResult<()> {
        match &mut stmt.kind {
            StmtKind::Return(expr) => self.add_type_expr(expr)?,
            StmtKind::Block(stmts) => {
                for stmt in stmts {
                    self.add_type_stmt(stmt)?;
                }
            }
            StmtKind::If { cond, then, els } => {
                self.add_type_expr(cond)?;
                self.add_type_stmt(then)?;
                if let Some(els) = els {
                    self.add_type_stmt(els)?;
                }
            }
            StmtKind::For {
                init,
                cond,
                inc,
                body,
            } => {
                if let Some(init) = init {
                    self.add_type_stmt(init)?;
                }
                if let Some(cond) = cond {
                    self.add_type_expr(cond)?;
                }
                if let Some(inc) = inc {
                    self.add_type_expr(inc)?;
                }
                self.add_type_stmt(body)?;
            }
            StmtKind::Expr(expr) => self.add_type_expr(expr)?,
            StmtKind::Decl(_) => {}
        }
        Ok(())
    }

    fn add_type_expr(&self, expr: &mut Expr) -> CompileResult<()> {
        if expr.ty.is_some() {
            return Ok(());
        }

        let ty = match &mut expr.kind {
            ExprKind::Num(_) => Type::Int,
            ExprKind::Var { idx, is_local } => {
                if *is_local {
                    self.locals
                        .get(*idx)
                        .map(|obj| obj.ty.clone())
                        .unwrap_or(Type::Int)
                } else {
                    self.globals
                        .get(*idx)
                        .map(|obj| obj.ty.clone())
                        .unwrap_or(Type::Int)
                }
            }
            ExprKind::Call { args, .. } => {
                for arg in args {
                    self.add_type_expr(arg)?;
                }
                Type::Int
            }
            ExprKind::Unary { expr, .. } => {
                self.add_type_expr(expr)?;
                expr.ty.clone().unwrap_or(Type::Int)
            }
            ExprKind::Addr(expr) => {
                self.add_type_expr(expr)?;
                let expr_ty = expr.ty.clone().unwrap_or(Type::Int);
                // If taking address of array, return pointer to base type
                // since &array[10] should give int*, not int(*)[10]
                if let Type::Array { base, .. } = expr_ty {
                    Type::Ptr(base)
                } else {
                    Type::Ptr(Box::new(expr_ty))
                }
            }
            ExprKind::Deref(expr) => {
                self.add_type_expr(expr)?;
                let expr_ty = expr.ty.clone().unwrap_or(Type::Int);
                // Dereference works on pointers and arrays
                match expr_ty.base() {
                    Some(base) => base.clone(),
                    None => {
                        return Err(self.error_at(expr.location, "invalid pointer dereference"));
                    }
                }
            }
            ExprKind::Assign { lhs, rhs } => {
                self.add_type_expr(lhs)?;
                self.add_type_expr(rhs)?;
                let lhs_ty = lhs.ty.clone().unwrap_or(Type::Int);
                // Arrays are not assignable
                if lhs_ty.is_array() {
                    return Err(self.error_at(lhs.location, "not an lvalue"));
                }
                lhs_ty
            }
            ExprKind::Binary { op, lhs, rhs } => {
                self.add_type_expr(lhs)?;
                self.add_type_expr(rhs)?;
                match op {
                    BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le => Type::Int,
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                        lhs.ty.clone().unwrap_or(Type::Int)
                    }
                }
            }
        };

        expr.ty = Some(ty);
        Ok(())
    }
}

fn align_to(n: i32, align: i32) -> i32 {
    (n + align - 1) / align * align
}
