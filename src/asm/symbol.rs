use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum SymbolBinding {
    Local,
    Global,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum SymbolType {
    NoType,
    Func,
    Object,
    Tls,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Symbol {
    pub name: String,
    pub section: Option<usize>,
    pub value: u64,
    pub size: Option<u64>,
    pub binding: SymbolBinding,
    pub typ: SymbolType,
}

impl Symbol {
    pub fn new(name: String) -> Self {
        Self {
            name,
            section: None,
            value: 0,
            size: None,
            binding: SymbolBinding::Local,
            typ: SymbolType::NoType,
        }
    }
}
