use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum SectionType {
    ProgBits,
    NoBits,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Section {
    pub name: String,
    pub typ: SectionType,
    pub flags: Option<String>,
    pub align: u64,
    pub size: u64,
}

impl Section {
    pub fn new(name: String, typ: SectionType, flags: Option<String>) -> Self {
        Self {
            name,
            typ,
            flags,
            align: 1,
            size: 0,
        }
    }
}
