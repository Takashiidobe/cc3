use crate::asm::{AsmError, AsmResult};
use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Register {
    // 8-bit registers
    Al,
    Bl,
    Cl,
    Dl,
    Sil,
    Dil,
    Bpl,
    Spl,
    R8b,
    R9b,
    R10b,
    R11b,
    R12b,
    R13b,
    R14b,
    R15b,

    // 16-bit registers
    Ax,
    Bx,
    Cx,
    Dx,
    Si,
    Di,
    Bp,
    Sp,
    R8w,
    R9w,
    R10w,
    R11w,
    R12w,
    R13w,
    R14w,
    R15w,

    // 32-bit registers
    Eax,
    Ebx,
    Ecx,
    Edx,
    Esi,
    Edi,
    Ebp,
    Esp,
    R8d,
    R9d,
    R10d,
    R11d,
    R12d,
    R13d,
    R14d,
    R15d,

    // 64-bit registers
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rbp,
    Rsp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,

    // SSE registers (128-bit)
    Xmm0,
    Xmm1,
    Xmm2,
    Xmm3,
    Xmm4,
    Xmm5,
    Xmm6,
    Xmm7,
    Xmm8,
    Xmm9,
    Xmm10,
    Xmm11,
    Xmm12,
    Xmm13,
    Xmm14,
    Xmm15,

    // x87 FPU stack registers
    St0,
    St1,
    St2,
    St3,
    St4,
    St5,
    St6,
    St7,

    // Segment registers
    Cs,
    Ds,
    Es,
    Fs,
    Gs,
    Ss,

    // Instruction pointer
    Rip,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Directives
    Directive(String), // .text, .globl, .section, etc.

    // Identifiers and labels
    Ident(String), // symbol names
    Label(String), // foo: (without colon)

    // Instructions
    Instruction(String), // mov, add, push, etc.

    // Operands
    Register(Register), // %rax, %xmm0, etc.
    Immediate(i64),     // $123
    Symbol(String),     // main

    // Special suffixes for symbols
    At(String), // @GOTPCREL, @PLT, @tlsgd, @tpoff

    // Operators
    Plus,    // +
    Minus,   // -
    Star,    // *
    Dollar,  // $
    Percent, // %
    Comma,   // ,
    Colon,   // :
    LParen,  // (
    RParen,  // )

    // Whitespace and structure
    Newline,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            pos: 0,
            line: 1,
            column: 1,
        }
    }

    fn current(&self) -> Option<char> {
        self.input.get(self.pos).copied()
    }

    fn peek(&self, offset: usize) -> Option<char> {
        self.input.get(self.pos + offset).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.current()?;
        self.pos += 1;
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(ch)
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.current() {
            if ch == ' ' || ch == '\t' || ch == '\r' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        // AT&T syntax uses # for comments
        if self.current() == Some('#') {
            while let Some(ch) = self.current() {
                if ch == '\n' {
                    break;
                }
                self.advance();
            }
        }
    }

    fn read_number(&mut self) -> AsmResult<i64> {
        let start_line = self.line;
        let start_column = self.column;
        let mut num_str = String::new();
        let mut base = 10;

        // Check for hex prefix (0x)
        if self.current() == Some('0') && self.peek(1) == Some('x') {
            base = 16;
            self.advance(); // 0
            self.advance(); // x
        }

        while let Some(ch) = self.current() {
            if ch.is_ascii_digit() || (base == 16 && ch.is_ascii_hexdigit()) {
                num_str.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        if num_str.is_empty() {
            return Err(AsmError {
                message: "invalid number".to_string(),
                line: start_line,
                column: start_column,
            });
        }

        i64::from_str_radix(&num_str, base).map_err(|_| AsmError {
            message: format!("invalid number: {}", num_str),
            line: start_line,
            column: start_column,
        })
    }

    fn read_identifier(&mut self) -> String {
        let mut ident = String::new();
        while let Some(ch) = self.current() {
            if ch.is_alphanumeric() || ch == '_' || ch == '.' || ch == '$' {
                ident.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        ident
    }

    fn read_string(&mut self) -> AsmResult<String> {
        let start_line = self.line;
        let start_column = self.column;
        let quote = self.advance().unwrap(); // " or '
        let mut string = String::new();

        loop {
            match self.current() {
                Some(ch) if ch == quote => {
                    self.advance();
                    return Ok(string);
                }
                Some('\\') => {
                    self.advance();
                    match self.current() {
                        Some('n') => {
                            string.push('\n');
                            self.advance();
                        }
                        Some('t') => {
                            string.push('\t');
                            self.advance();
                        }
                        Some('\\') => {
                            string.push('\\');
                            self.advance();
                        }
                        Some(ch) => {
                            string.push(ch);
                            self.advance();
                        }
                        None => break,
                    }
                }
                Some(ch) => {
                    string.push(ch);
                    self.advance();
                }
                None => break,
            }
        }

        Err(AsmError {
            message: "unterminated string".to_string(),
            line: start_line,
            column: start_column,
        })
    }

    pub fn next_token(&mut self) -> AsmResult<Token> {
        self.skip_whitespace();

        let line = self.line;
        let column = self.column;

        let ch = match self.current() {
            Some(ch) => ch,
            None => {
                return Ok(Token {
                    kind: TokenKind::Eof,
                    line,
                    column,
                });
            }
        };

        // Handle comments
        if ch == '#' {
            self.skip_comment();
            return self.next_token();
        }

        // Handle newlines
        if ch == '\n' {
            self.advance();
            return Ok(Token {
                kind: TokenKind::Newline,
                line,
                column,
            });
        }

        // Handle directives
        if ch == '.' {
            self.advance();
            let ident = self.read_identifier();
            return Ok(Token {
                kind: TokenKind::Directive(format!(".{}", ident)),
                line,
                column,
            });
        }

        // Handle registers
        if ch == '%' {
            self.advance();
            let reg_name = self.read_identifier();

            // Special case for %st(N) - x87 FPU stack registers
            if reg_name == "st" && self.current() == Some('(') {
                self.advance(); // (
                let num = self.read_number()?;
                if self.current() != Some(')') {
                    return Err(AsmError {
                        message: "expected ')' after st register number".to_string(),
                        line: self.line,
                        column: self.column,
                    });
                }
                self.advance(); // )
                let reg = match num {
                    0 => Register::St0,
                    1 => Register::St1,
                    2 => Register::St2,
                    3 => Register::St3,
                    4 => Register::St4,
                    5 => Register::St5,
                    6 => Register::St6,
                    7 => Register::St7,
                    _ => {
                        return Err(AsmError {
                            message: format!("invalid st register number: {}", num),
                            line,
                            column,
                        });
                    }
                };
                return Ok(Token {
                    kind: TokenKind::Register(reg),
                    line,
                    column,
                });
            }

            let reg = parse_register(&reg_name).ok_or_else(|| AsmError {
                message: format!("unknown register: %{}", reg_name),
                line,
                column,
            })?;
            return Ok(Token {
                kind: TokenKind::Register(reg),
                line,
                column,
            });
        }

        // Handle immediates
        if ch == '$' {
            self.advance();
            // Could be $123 or $symbol
            if let Some(ch) = self.current() {
                if ch.is_ascii_digit() || ch == '-' {
                    let sign = if ch == '-' {
                        self.advance();
                        -1
                    } else {
                        1
                    };
                    let num = self.read_number()?;
                    return Ok(Token {
                        kind: TokenKind::Immediate(sign * num),
                        line,
                        column,
                    });
                } else if ch.is_alphabetic() || ch == '_' || ch == '.' {
                    let symbol = self.read_identifier();
                    return Ok(Token {
                        kind: TokenKind::Symbol(symbol),
                        line,
                        column,
                    });
                }
            }
            return Ok(Token {
                kind: TokenKind::Dollar,
                line,
                column,
            });
        }

        // Handle @ suffixes (like @GOTPCREL, @PLT)
        if ch == '@' {
            self.advance();
            let suffix = self.read_identifier();
            return Ok(Token {
                kind: TokenKind::At(suffix),
                line,
                column,
            });
        }

        // Handle operators
        match ch {
            '+' => {
                self.advance();
                Ok(Token {
                    kind: TokenKind::Plus,
                    line,
                    column,
                })
            }
            '-' => {
                self.advance();
                // Could be negative number or minus operator
                if let Some(next) = self.current() {
                    if next.is_ascii_digit() {
                        let num = self.read_number()?;
                        return Ok(Token {
                            kind: TokenKind::Immediate(-num),
                            line,
                            column,
                        });
                    }
                }
                Ok(Token {
                    kind: TokenKind::Minus,
                    line,
                    column,
                })
            }
            '*' => {
                self.advance();
                Ok(Token {
                    kind: TokenKind::Star,
                    line,
                    column,
                })
            }
            ',' => {
                self.advance();
                Ok(Token {
                    kind: TokenKind::Comma,
                    line,
                    column,
                })
            }
            ':' => {
                self.advance();
                Ok(Token {
                    kind: TokenKind::Colon,
                    line,
                    column,
                })
            }
            '(' => {
                self.advance();
                Ok(Token {
                    kind: TokenKind::LParen,
                    line,
                    column,
                })
            }
            ')' => {
                self.advance();
                Ok(Token {
                    kind: TokenKind::RParen,
                    line,
                    column,
                })
            }
            '"' | '\'' => {
                let _s = self.read_string()?;
                // For now, treat strings as identifiers
                // In a full implementation, we'd handle string directives
                self.next_token()
            }
            _ if ch.is_alphabetic() || ch == '_' => {
                let ident = self.read_identifier();
                // Check if followed by colon (label)
                if self.current() == Some(':') {
                    self.advance();
                    Ok(Token {
                        kind: TokenKind::Label(ident),
                        line,
                        column,
                    })
                } else {
                    // Could be instruction or symbol
                    Ok(Token {
                        kind: TokenKind::Ident(ident),
                        line,
                        column,
                    })
                }
            }
            _ if ch.is_ascii_digit() => {
                let num = self.read_number()?;
                Ok(Token {
                    kind: TokenKind::Immediate(num),
                    line,
                    column,
                })
            }
            _ => Err(AsmError {
                message: format!("unexpected character: '{}'", ch),
                line,
                column,
            }),
        }
    }
}

fn parse_register(name: &str) -> Option<Register> {
    Some(match name {
        // 8-bit
        "al" => Register::Al,
        "bl" => Register::Bl,
        "cl" => Register::Cl,
        "dl" => Register::Dl,
        "sil" => Register::Sil,
        "dil" => Register::Dil,
        "bpl" => Register::Bpl,
        "spl" => Register::Spl,
        "r8b" => Register::R8b,
        "r9b" => Register::R9b,
        "r10b" => Register::R10b,
        "r11b" => Register::R11b,
        "r12b" => Register::R12b,
        "r13b" => Register::R13b,
        "r14b" => Register::R14b,
        "r15b" => Register::R15b,

        // 16-bit
        "ax" => Register::Ax,
        "bx" => Register::Bx,
        "cx" => Register::Cx,
        "dx" => Register::Dx,
        "si" => Register::Si,
        "di" => Register::Di,
        "bp" => Register::Bp,
        "sp" => Register::Sp,
        "r8w" => Register::R8w,
        "r9w" => Register::R9w,
        "r10w" => Register::R10w,
        "r11w" => Register::R11w,
        "r12w" => Register::R12w,
        "r13w" => Register::R13w,
        "r14w" => Register::R14w,
        "r15w" => Register::R15w,

        // 32-bit
        "eax" => Register::Eax,
        "ebx" => Register::Ebx,
        "ecx" => Register::Ecx,
        "edx" => Register::Edx,
        "esi" => Register::Esi,
        "edi" => Register::Edi,
        "ebp" => Register::Ebp,
        "esp" => Register::Esp,
        "r8d" => Register::R8d,
        "r9d" => Register::R9d,
        "r10d" => Register::R10d,
        "r11d" => Register::R11d,
        "r12d" => Register::R12d,
        "r13d" => Register::R13d,
        "r14d" => Register::R14d,
        "r15d" => Register::R15d,

        // 64-bit
        "rax" => Register::Rax,
        "rbx" => Register::Rbx,
        "rcx" => Register::Rcx,
        "rdx" => Register::Rdx,
        "rsi" => Register::Rsi,
        "rdi" => Register::Rdi,
        "rbp" => Register::Rbp,
        "rsp" => Register::Rsp,
        "r8" => Register::R8,
        "r9" => Register::R9,
        "r10" => Register::R10,
        "r11" => Register::R11,
        "r12" => Register::R12,
        "r13" => Register::R13,
        "r14" => Register::R14,
        "r15" => Register::R15,

        // SSE
        "xmm0" => Register::Xmm0,
        "xmm1" => Register::Xmm1,
        "xmm2" => Register::Xmm2,
        "xmm3" => Register::Xmm3,
        "xmm4" => Register::Xmm4,
        "xmm5" => Register::Xmm5,
        "xmm6" => Register::Xmm6,
        "xmm7" => Register::Xmm7,
        "xmm8" => Register::Xmm8,
        "xmm9" => Register::Xmm9,
        "xmm10" => Register::Xmm10,
        "xmm11" => Register::Xmm11,
        "xmm12" => Register::Xmm12,
        "xmm13" => Register::Xmm13,
        "xmm14" => Register::Xmm14,
        "xmm15" => Register::Xmm15,

        // Segment
        "cs" => Register::Cs,
        "ds" => Register::Ds,
        "es" => Register::Es,
        "fs" => Register::Fs,
        "gs" => Register::Gs,
        "ss" => Register::Ss,

        // Special
        "rip" => Register::Rip,

        _ => return None,
    })
}

pub fn lex(input: &str) -> AsmResult<Vec<Token>> {
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();

    loop {
        let token = lexer.next_token()?;
        let is_eof = matches!(token.kind, TokenKind::Eof);
        tokens.push(token);
        if is_eof {
            break;
        }
    }

    Ok(tokens)
}
