use std::fmt;

#[derive(Debug, Clone)]
pub struct CompileError {
    message: String,
    pos: Option<usize>,
}

impl CompileError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            pos: None,
        }
    }

    pub fn at(message: impl Into<String>, pos: usize) -> Self {
        Self {
            message: message.into(),
            pos: Some(pos),
        }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.pos {
            Some(pos) => write!(f, "{} at byte {}", self.message, pos),
            None => write!(f, "{}", self.message),
        }
    }
}

impl std::error::Error for CompileError {}

pub type CompileResult<T> = Result<T, CompileError>;
