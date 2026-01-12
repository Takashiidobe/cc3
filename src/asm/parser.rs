use crate::asm::lexer::{Register, Token, TokenKind};
use crate::asm::{AsmError, AsmResult};

#[derive(Debug, Clone, PartialEq)]
pub enum RelocType {
    None,
    GotPcRel,   // @GOTPCREL
    Plt,        // @PLT
    TlsGd,      // @tlsgd
    TpOff,      // @tpoff
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Register(Register),
    Immediate(i64),
    Symbol {
        name: String,
        addend: i64,
        reloc: RelocType,
    },
    Memory {
        displacement: i64,
        base: Option<Register>,
        index: Option<Register>,
        scale: u8, // 1, 2, 4, 8
        symbol: Option<(String, RelocType)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub mnemonic: String,
    pub operands: Vec<Operand>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DirectiveArg {
    String(String),
    Integer(i64),
    Symbol(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Directive {
    pub name: String,
    pub args: Vec<DirectiveArg>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmNode {
    Label(String),
    Directive(Directive),
    Instruction(Instruction),
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn peek(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.pos + offset)
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn expect(&mut self, expected: TokenKind) -> AsmResult<()> {
        let (kind, line, column) = match self.current() {
            Some(token) => (token.kind.clone(), token.line, token.column),
            None => {
                return Err(AsmError {
                    message: format!("expected {:?}, got EOF", expected),
                    line: 0,
                    column: 0,
                })
            }
        };

        if std::mem::discriminant(&kind) != std::mem::discriminant(&expected) {
            return Err(AsmError {
                message: format!("expected {:?}, got {:?}", expected, kind),
                line,
                column,
            });
        }

        self.advance();
        Ok(())
    }

    fn skip_newlines(&mut self) {
        while let Some(token) = self.current() {
            if matches!(token.kind, TokenKind::Newline) {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn parse_operand(&mut self) -> AsmResult<Operand> {
        let (kind, line, column) = match self.current() {
            Some(token) => (token.kind.clone(), token.line, token.column),
            None => {
                return Err(AsmError {
                    message: "unexpected end of input while parsing operand".to_string(),
                    line: 0,
                    column: 0,
                })
            }
        };

        match &kind {
            TokenKind::Register(reg) => {
                let reg = *reg;
                self.advance();
                Ok(Operand::Register(reg))
            }
            TokenKind::Dollar => {
                self.advance();
                // $symbol or $number already handled by lexer
                Err(AsmError {
                    message: "unexpected $ in operand".to_string(),
                    line,
                    column,
                })
            }
            TokenKind::Symbol(name) | TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();

                // Check for @suffix
                let reloc = match self.current() {
                    Some(token) if matches!(&token.kind, TokenKind::At(_)) => {
                        let suffix = if let TokenKind::At(s) = &token.kind {
                            s.clone()
                        } else {
                            String::new()
                        };
                        self.advance();
                        match suffix.as_str() {
                            "GOTPCREL" => RelocType::GotPcRel,
                            "PLT" => RelocType::Plt,
                            "tlsgd" => RelocType::TlsGd,
                            "tpoff" => RelocType::TpOff,
                            _ => RelocType::None,
                        }
                    }
                    _ => RelocType::None,
                };

                // Check for (reg) - RIP-relative addressing
                let has_lparen = self.current().map_or(false, |t| matches!(t.kind, TokenKind::LParen));
                if has_lparen {
                    self.advance();
                    let reg_opt = self.current().and_then(|t| {
                        if let TokenKind::Register(r) = t.kind {
                            Some(r)
                        } else {
                            None
                        }
                    });

                    if let Some(reg) = reg_opt {
                        self.advance();
                        self.expect(TokenKind::RParen)?;
                        return Ok(Operand::Memory {
                            displacement: 0,
                            base: Some(reg),
                            index: None,
                            scale: 1,
                            symbol: Some((name, reloc)),
                        });
                    } else {
                        return Err(AsmError {
                            message: "expected register after (".to_string(),
                            line,
                            column,
                        });
                    }
                }

                Ok(Operand::Symbol { name, addend: 0, reloc })
            }
            // Memory operand: disp(%base) or disp(%base,%index,scale)
            _ if matches!(kind, TokenKind::Immediate(_)) || matches!(kind, TokenKind::Minus) => {
                let displacement = if let TokenKind::Immediate(val) = &kind {
                    let val = *val;
                    self.advance();
                    val
                } else if matches!(kind, TokenKind::Minus) {
                    self.advance();
                    match self.current() {
                        Some(token) if matches!(token.kind, TokenKind::Immediate(_)) => {
                            if let TokenKind::Immediate(val) = token.kind {
                                self.advance();
                                -val
                            } else {
                                0
                            }
                        }
                        _ => {
                            return Err(AsmError {
                                message: "expected number after -".to_string(),
                                line,
                                column,
                            });
                        }
                    }
                } else {
                    0
                };

                if let Some(token) = self.current() {
                    if matches!(token.kind, TokenKind::LParen) {

                        self.advance();

                        // Parse base register
                        let base = if let Some(token) = self.current() {
                            if let TokenKind::Register(reg) = &token.kind {
                                let reg = reg.clone();
                                self.advance();
                                Some(reg)
                            } else {
                                None
                            }
                        } else {
                            None
                        };

                        // Check for index and scale
                        let (index, scale) = if let Some(token) = self.current() {
                            if matches!(token.kind, TokenKind::Comma) {
                                self.advance();

                                let index = if let Some(token) = self.current() {
                                    if let TokenKind::Register(reg) = &token.kind {
                                        let reg = reg.clone();
                                        self.advance();
                                        Some(reg)
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                };

                                let scale = if let Some(token) = self.current() {
                                    if matches!(token.kind, TokenKind::Comma) {
                                        self.advance();
                                        if let Some(token) = self.current() {
                                            if let TokenKind::Immediate(s) = token.kind {
                                                self.advance();
                                                s as u8
                                            } else {
                                                1
                                            }
                                        } else {
                                            1
                                        }
                                    } else {
                                        1
                                    }
                                } else {
                                    1
                                };

                                (index, scale)
                            } else {
                                (None, 1)
                            }
                        } else {
                            (None, 1)
                        };

                        self.expect(TokenKind::RParen)?;

                        return Ok(Operand::Memory {
                            displacement,
                            base,
                            index,
                            scale,
                            symbol: None,
                        });
                    }
                }

                Ok(Operand::Immediate(displacement))
            }
            TokenKind::Percent => {
                // Segment register prefix like %fs:0
                self.advance();
                if let Some(token) = self.current() {
                    if let TokenKind::Ident(seg) = &token.kind {
                        let _seg = seg.clone();
                        self.advance();
                        if let Some(token) = self.current() {
                            if matches!(token.kind, TokenKind::Colon) {
                                self.advance();
                                // Parse the rest as normal operand
                                return self.parse_operand();
                            }
                        }
                    }
                }
                Err(AsmError {
                    message: "invalid operand".to_string(),
                    line,
                    column,
                })
            }
            _ => Err(AsmError {
                message: format!("unexpected token in operand: {:?}", kind),
                line,
                column,
            }),
        }
    }

    fn parse_instruction(&mut self, mnemonic: String) -> AsmResult<Instruction> {
        let mut operands = Vec::new();

        // Parse operands separated by commas
        while let Some(token) = self.current() {
            if matches!(token.kind, TokenKind::Newline | TokenKind::Eof) {
                break;
            }

            let operand = self.parse_operand()?;
            operands.push(operand);

            // Check for comma
            if let Some(token) = self.current() {
                if matches!(token.kind, TokenKind::Comma) {
                    self.advance();
                } else if !matches!(token.kind, TokenKind::Newline | TokenKind::Eof) {
                    return Err(AsmError {
                        message: "expected comma or newline after operand".to_string(),
                        line: token.line,
                        column: token.column,
                    });
                }
            }
        }

        Ok(Instruction { mnemonic, operands })
    }

    fn parse_directive(&mut self, name: String) -> AsmResult<Directive> {
        let mut args = Vec::new();

        while let Some(token) = self.current() {
            if matches!(token.kind, TokenKind::Newline | TokenKind::Eof) {
                break;
            }

            match &token.kind {
                TokenKind::Immediate(val) => {
                    args.push(DirectiveArg::Integer(*val));
                    self.advance();
                }
                TokenKind::Ident(s) | TokenKind::Symbol(s) => {
                    args.push(DirectiveArg::Symbol(s.clone()));
                    self.advance();
                }
                TokenKind::Comma => {
                    self.advance();
                }
                _ => {
                    return Err(AsmError {
                        message: format!("unexpected token in directive: {:?}", token.kind),
                        line: token.line,
                        column: token.column,
                    });
                }
            }
        }

        Ok(Directive { name, args })
    }

    pub fn parse(&mut self) -> AsmResult<Vec<AsmNode>> {
        let mut nodes = Vec::new();

        loop {
            self.skip_newlines();

            let token = match self.current() {
                Some(t) => t,
                None => break,
            };

            if matches!(token.kind, TokenKind::Eof) {
                break;
            }

            match &token.kind {
                TokenKind::Label(name) => {
                    let name = name.clone();
                    self.advance();
                    nodes.push(AsmNode::Label(name));
                }
                TokenKind::Directive(name) => {
                    let name = name.clone();
                    self.advance();
                    let directive = self.parse_directive(name)?;
                    nodes.push(AsmNode::Directive(directive));
                }
                TokenKind::Ident(name) => {
                    let name = name.clone();
                    self.advance();
                    let instruction = self.parse_instruction(name)?;
                    nodes.push(AsmNode::Instruction(instruction));
                }
                TokenKind::Newline => {
                    self.advance();
                }
                _ => {
                    return Err(AsmError {
                        message: format!("unexpected token: {:?}", token.kind),
                        line: token.line,
                        column: token.column,
                    });
                }
            }
        }

        Ok(nodes)
    }
}

pub fn parse(tokens: Vec<Token>) -> AsmResult<Vec<AsmNode>> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asm::lexer::lex;

    #[test]
    fn test_parse_label() {
        let tokens = lex("main:").unwrap();
        let nodes = parse(tokens).unwrap();
        assert_eq!(nodes.len(), 1);
        match &nodes[0] {
            AsmNode::Label(name) => assert_eq!(name, "main"),
            _ => panic!("expected label"),
        }
    }

    #[test]
    fn test_parse_directive() {
        let tokens = lex(".globl main").unwrap();
        let nodes = parse(tokens).unwrap();
        assert_eq!(nodes.len(), 1);
        match &nodes[0] {
            AsmNode::Directive(d) => {
                assert_eq!(d.name, ".globl");
                assert_eq!(d.args.len(), 1);
            }
            _ => panic!("expected directive"),
        }
    }

    #[test]
    fn test_parse_instruction() {
        let tokens = lex("mov $42, %rax").unwrap();
        let nodes = parse(tokens).unwrap();
        assert_eq!(nodes.len(), 1);
        match &nodes[0] {
            AsmNode::Instruction(inst) => {
                assert_eq!(inst.mnemonic, "mov");
                assert_eq!(inst.operands.len(), 2);
                match &inst.operands[0] {
                    Operand::Immediate(42) => {}
                    _ => panic!("expected immediate 42"),
                }
                match &inst.operands[1] {
                    Operand::Register(Register::Rax) => {}
                    _ => panic!("expected %rax"),
                }
            }
            _ => panic!("expected instruction"),
        }
    }

    #[test]
    fn test_parse_memory_operand() {
        let tokens = lex("mov 8(%rbp), %rax").unwrap();
        let nodes = parse(tokens).unwrap();
        match &nodes[0] {
            AsmNode::Instruction(inst) => {
                match &inst.operands[0] {
                    Operand::Memory {
                        displacement,
                        base,
                        index,
                        scale,
                        symbol,
                    } => {
                        assert_eq!(*displacement, 8);
                        assert_eq!(*base, Some(Register::Rbp));
                        assert_eq!(*index, None);
                        assert_eq!(*scale, 1);
                        assert_eq!(*symbol, None);
                    }
                    _ => panic!("expected memory operand"),
                }
            }
            _ => panic!("expected instruction"),
        }
    }

    #[test]
    fn test_parse_symbol_with_reloc() {
        let tokens = lex("mov main@GOTPCREL(%rip), %rax").unwrap();
        let nodes = parse(tokens).unwrap();
        match &nodes[0] {
            AsmNode::Instruction(inst) => {
                match &inst.operands[0] {
                    Operand::Memory { symbol, base, .. } => {
                        assert_eq!(*base, Some(Register::Rip));
                        match symbol {
                            Some((name, reloc)) => {
                                assert_eq!(name, "main");
                                assert_eq!(*reloc, RelocType::GotPcRel);
                            }
                            _ => panic!("expected symbol with relocation"),
                        }
                    }
                    _ => panic!("expected memory operand with symbol"),
                }
            }
            _ => panic!("expected instruction"),
        }
    }

    #[test]
    fn test_parse_multiline() {
        let input = ".text\nmain:\n  mov $42, %rax\n  ret";
        let tokens = lex(input).unwrap();
        let nodes = parse(tokens).unwrap();
        assert_eq!(nodes.len(), 4); // .text, main:, mov, ret
    }
}
