pub mod assembler;
pub mod lexer;
pub mod parser;
pub mod section;
pub mod symbol;

pub use assembler::{build_program, AsmProgram};
pub use lexer::{lex, Register, Token, TokenKind};
pub use parser::{parse, AsmNode, Directive, Instruction, Operand};
pub use section::{Section, SectionType};
pub use symbol::{Symbol, SymbolBinding, SymbolType};

use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct AsmError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for AsmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.line, self.column, self.message)
    }
}

impl std::error::Error for AsmError {}

pub type AsmResult<T> = Result<T, AsmError>;
