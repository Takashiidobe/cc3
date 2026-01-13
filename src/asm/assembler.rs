use crate::asm::parser::{AsmNode, DirectiveArg};
use crate::asm::section::{Section, SectionType};
use crate::asm::symbol::{Symbol, SymbolBinding, SymbolType};
use crate::asm::{AsmError, AsmResult};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct AsmProgram {
    pub sections: Vec<Section>,
    pub symbols: Vec<Symbol>,
}

pub struct Assembler {
    program: AsmProgram,
    section_map: HashMap<String, usize>,
    symbol_map: HashMap<String, usize>,
    current_section: Option<usize>,
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            program: AsmProgram {
                sections: Vec::new(),
                symbols: Vec::new(),
            },
            section_map: HashMap::new(),
            symbol_map: HashMap::new(),
            current_section: None,
        }
    }

    pub fn assemble(&mut self, nodes: &[AsmNode]) -> AsmResult<()> {
        for node in nodes {
            match node {
                AsmNode::Directive(directive) => {
                    self.handle_directive(&directive.name, &directive.args)?;
                }
                AsmNode::Label(name) => {
                    self.define_label(name)?;
                }
                AsmNode::Instruction(_) => {
                    self.ensure_current_section()?;
                }
            }
        }
        Ok(())
    }

    fn ensure_current_section(&mut self) -> AsmResult<usize> {
        if let Some(idx) = self.current_section {
            return Ok(idx);
        }
        let idx = self.switch_section(".text", SectionType::ProgBits, Some("ax".to_string()))?;
        Ok(idx)
    }

    fn switch_section(
        &mut self,
        name: &str,
        typ: SectionType,
        flags: Option<String>,
    ) -> AsmResult<usize> {
        if let Some(&idx) = self.section_map.get(name) {
            let section = &mut self.program.sections[idx];
            if section.typ != typ {
                return Err(AsmError {
                    message: format!(
                        "section type mismatch for {}: {:?} vs {:?}",
                        name, section.typ, typ
                    ),
                    line: 0,
                    column: 0,
                });
            }
            if section.flags.is_none() {
                section.flags = flags.clone();
            } else if let (Some(existing), Some(new_flags)) = (&section.flags, &flags) {
                if existing != new_flags {
                    return Err(AsmError {
                        message: format!(
                            "section flags mismatch for {}: {} vs {}",
                            name, existing, new_flags
                        ),
                        line: 0,
                        column: 0,
                    });
                }
            }
            self.current_section = Some(idx);
            return Ok(idx);
        }

        let idx = self.program.sections.len();
        self.program
            .sections
            .push(Section::new(name.to_string(), typ, flags));
        self.section_map.insert(name.to_string(), idx);
        self.current_section = Some(idx);
        Ok(idx)
    }

    fn define_label(&mut self, name: &str) -> AsmResult<()> {
        let section_idx = self.ensure_current_section()?;
        let section_size = self.program.sections[section_idx].size;
        let symbol_idx = self.get_or_create_symbol(name);
        let symbol = &mut self.program.symbols[symbol_idx];
        if symbol.section.is_some() {
            return Err(AsmError {
                message: format!("symbol redefined: {}", name),
                line: 0,
                column: 0,
            });
        }
        symbol.section = Some(section_idx);
        symbol.value = section_size;
        Ok(())
    }

    fn get_or_create_symbol(&mut self, name: &str) -> usize {
        if let Some(&idx) = self.symbol_map.get(name) {
            return idx;
        }
        let idx = self.program.symbols.len();
        self.program.symbols.push(Symbol::new(name.to_string()));
        self.symbol_map.insert(name.to_string(), idx);
        idx
    }

    fn handle_directive(&mut self, name: &str, args: &[DirectiveArg]) -> AsmResult<()> {
        match name {
            ".text" => {
                self.switch_section(".text", SectionType::ProgBits, Some("ax".to_string()))?;
            }
            ".data" => {
                self.switch_section(".data", SectionType::ProgBits, Some("aw".to_string()))?;
            }
            ".rodata" => {
                self.switch_section(".rodata", SectionType::ProgBits, Some("a".to_string()))?;
            }
            ".bss" => {
                self.switch_section(".bss", SectionType::NoBits, Some("aw".to_string()))?;
            }
            ".tdata" => {
                self.switch_section(".tdata", SectionType::ProgBits, Some("awT".to_string()))?;
            }
            ".tbss" => {
                self.switch_section(".tbss", SectionType::NoBits, Some("awT".to_string()))?;
            }
            ".section" => {
                let (name, flags, typ) = self.parse_section_args(args)?;
                self.switch_section(&name, typ, flags)?;
            }
            ".globl" | ".global" => {
                for symbol in self.symbol_args(args)? {
                    let idx = self.get_or_create_symbol(&symbol);
                    self.program.symbols[idx].binding = SymbolBinding::Global;
                }
            }
            ".local" => {
                for symbol in self.symbol_args(args)? {
                    let idx = self.get_or_create_symbol(&symbol);
                    self.program.symbols[idx].binding = SymbolBinding::Local;
                }
            }
            ".type" => {
                self.handle_type_directive(args)?;
            }
            ".size" => {
                self.handle_size_directive(args)?;
            }
            ".align" => {
                self.handle_align_directive(args)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn symbol_args(&self, args: &[DirectiveArg]) -> AsmResult<Vec<String>> {
        let mut symbols = Vec::new();
        for arg in args {
            match arg {
                DirectiveArg::Symbol(name) => symbols.push(name.clone()),
                DirectiveArg::String(name) => symbols.push(name.clone()),
                DirectiveArg::Integer(_) => {
                    return Err(AsmError {
                        message: "expected symbol name".to_string(),
                        line: 0,
                        column: 0,
                    });
                }
            }
        }
        Ok(symbols)
    }

    fn handle_type_directive(&mut self, args: &[DirectiveArg]) -> AsmResult<()> {
        if args.len() < 2 {
            return Err(AsmError {
                message: ".type expects a symbol and a type".to_string(),
                line: 0,
                column: 0,
            });
        }
        let symbol = self.single_symbol_arg(&args[0])?;
        let typ_name = self.single_symbol_arg(&args[1])?;
        let typ = match typ_name.as_str() {
            "@function" => SymbolType::Func,
            "@object" => SymbolType::Object,
            "@tls_object" => SymbolType::Tls,
            "@notype" => SymbolType::NoType,
            _ => {
                return Err(AsmError {
                    message: format!("unknown symbol type: {}", typ_name),
                    line: 0,
                    column: 0,
                });
            }
        };
        let idx = self.get_or_create_symbol(&symbol);
        self.program.symbols[idx].typ = typ;
        Ok(())
    }

    fn handle_size_directive(&mut self, args: &[DirectiveArg]) -> AsmResult<()> {
        if args.len() < 2 {
            return Err(AsmError {
                message: ".size expects a symbol and a size".to_string(),
                line: 0,
                column: 0,
            });
        }
        let symbol = self.single_symbol_arg(&args[0])?;
        let size = match &args[1] {
            DirectiveArg::Integer(val) => *val,
            _ => {
                return Err(AsmError {
                    message: ".size expects an integer size".to_string(),
                    line: 0,
                    column: 0,
                });
            }
        };
        if size < 0 {
            return Err(AsmError {
                message: ".size expects a non-negative size".to_string(),
                line: 0,
                column: 0,
            });
        }
        let idx = self.get_or_create_symbol(&symbol);
        self.program.symbols[idx].size = Some(size as u64);
        Ok(())
    }

    fn handle_align_directive(&mut self, args: &[DirectiveArg]) -> AsmResult<()> {
        let align = match args.first() {
            Some(DirectiveArg::Integer(val)) => *val,
            _ => {
                return Err(AsmError {
                    message: ".align expects an integer".to_string(),
                    line: 0,
                    column: 0,
                });
            }
        };
        if align <= 0 {
            return Err(AsmError {
                message: ".align expects a positive value".to_string(),
                line: 0,
                column: 0,
            });
        }
        let section_idx = self.ensure_current_section()?;
        let section = &mut self.program.sections[section_idx];
        let align = align as u64;
        if align > section.align {
            section.align = align;
        }
        let padding = (align - (section.size % align)) % align;
        section.size += padding;
        Ok(())
    }

    fn parse_section_args(
        &self,
        args: &[DirectiveArg],
    ) -> AsmResult<(String, Option<String>, SectionType)> {
        let name = args
            .first()
            .ok_or_else(|| AsmError {
                message: ".section expects a name".to_string(),
                line: 0,
                column: 0,
            })
            .and_then(|arg| self.single_symbol_arg(arg))?;

        let flags = if args.len() > 1 {
            match &args[1] {
                DirectiveArg::String(val) => Some(val.clone()),
                DirectiveArg::Symbol(val) => Some(val.clone()),
                DirectiveArg::Integer(_) => None,
            }
        } else {
            None
        };

        let mut typ = SectionType::ProgBits;
        for arg in args.iter().skip(1) {
            if let DirectiveArg::Symbol(val) = arg {
                match val.as_str() {
                    "@progbits" => typ = SectionType::ProgBits,
                    "@nobits" => typ = SectionType::NoBits,
                    _ => {}
                }
            }
        }

        Ok((name, flags, typ))
    }

    fn single_symbol_arg(&self, arg: &DirectiveArg) -> AsmResult<String> {
        match arg {
            DirectiveArg::Symbol(name) => Ok(name.clone()),
            DirectiveArg::String(name) => Ok(name.clone()),
            DirectiveArg::Integer(_) => Err(AsmError {
                message: "expected symbol name".to_string(),
                line: 0,
                column: 0,
            }),
        }
    }

    pub fn into_program(self) -> AsmProgram {
        self.program
    }
}

pub fn build_program(nodes: &[AsmNode]) -> AsmResult<AsmProgram> {
    let mut assembler = Assembler::new();
    assembler.assemble(nodes)?;
    Ok(assembler.into_program())
}
