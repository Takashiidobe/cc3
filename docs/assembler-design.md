# Assembler Design Document

## Overview

This document describes the design and implementation plan for a self-hosted x86_64 assembler for the cc3 compiler. The assembler will parse AT&T syntax assembly (GNU as-compatible) and emit ELF64 object files, eliminating our dependency on external assemblers like GAS.

## Goals

1. **Self-hosting**: Enable the compiler to assemble its own output without relying on GAS
2. **ELF64 output**: Generate proper ELF64 relocatable object files (`.o`)
3. **Subset support**: Support the specific x86_64 instructions and directives emitted by our codegen
4. **Relocations**: Handle all relocation types needed for linking
5. **TLS support**: Thread-local storage via GOT and TLS relocations
6. **PIC support**: Position-independent code generation

## Assembly Subset Requirements

Based on `src/codegen.rs`, our assembler must support:

### Directives
- **Section directives**: `.section`, `.text`, `.data`, `.bss`, `.rodata`, `.tdata`, `.tbss`
- **Symbol directives**: `.globl`, `.local`, `.type`, `.size`
- **Data directives**: `.byte`, `.word`, `.long`, `.quad`, `.zero`
- **Alignment**: `.align`, `.comm`
- **Debug info**: `.file`, `.loc`
- **Special**: `.value` (16-bit data)

### Instruction Set
- **Data movement**: `mov`, `movzbl`, `movsbl`, `movzwl`, `movswl`, `movslq`, `lea`, `push`, `pop`
- **Arithmetic**: `add`, `sub`, `imul`, `div`, `idiv`, `inc`, `dec`, `neg`, `cmp`
- **Bitwise**: `and`, `or`, `xor`, `not`, `shl`, `shr`, `sar`, `test`
- **Control flow**: `jmp`, `je`, `jne`, `jl`, `jle`, `jg`, `jge`, `jb`, `jbe`, `ja`, `jae`, `js`, `jns`, `call`, `ret`
- **Stack ops**: `push`, `pop`
- **SSE**: `movss`, `movsd`, `addss`, `addsd`, `subss`, `subsd`, `mulss`, `mulsd`, `divss`, `divsd`, `ucomiss`, `ucomisd`, `cvtsi2ss`, `cvtsi2sd`, `cvttss2si`, `cvttsd2si`, `cvtss2sd`, `cvtsd2ss`, `xorps`, `xorpd`, `pxor`, `movq`
- **x87 FPU**: `fld`, `flds`, `fldl`, `fldt`, `fldz`, `fst`, `fstp`, `fstps`, `fstpl`, `fstpt`, `fadd`, `fsub`, `fmul`, `fdiv`, `faddp`, `fsubrp`, `fmulp`, `fdivrp`, `fchs`, `fcomip`, `fucomip`, `fnstcw`, `fldcw`, `fistps`, `fistpl`, `fistpq`, `fildl`, `fildll`, `fildq`, `fadds`
- **Atomics**: `lock`, `cmpxchg`, `xchg`
- **Misc**: `sete`, `setne`, `setl`, `setle`, `setb`, `setbe`, `seta`, `setae`, `setp`, `setnp`, `cqo`, `cdq`, `rep stosb`

### Addressing Modes
- **Register direct**: `%rax`, `%xmm0`, `%st(0)`
- **Immediate**: `$123`, `$symbol`
- **Memory indirect**: `offset(%base)`, `offset(%base, %index, scale)`
- **RIP-relative**: `symbol(%rip)`, `symbol@GOTPCREL(%rip)`, `symbol@PLT`
- **TLS**: `%fs:0`, `symbol@tlsgd(%rip)`, `symbol@tpoff`

### Relocation Types Needed
- `R_X86_64_PC32` - 32-bit PC-relative
- `R_X86_64_PLT32` - PLT entry reference
- `R_X86_64_GOTPCREL` - GOT entry reference
- `R_X86_64_64` - Direct 64-bit (in `.quad` directives)
- `R_X86_64_32` - Direct 32-bit zero-extended
- `R_X86_64_32S` - Direct 32-bit sign-extended
- `R_X86_64_TLSGD` - TLS GD (General Dynamic)
- `R_X86_64_TPOFF32` - TLS TP offset

## Architecture

### Module Structure

```
src/asm/
├── mod.rs           # Public API
├── lexer.rs         # Tokenization
├── parser.rs        # Parse assembly to IR
├── encoder.rs       # Instruction encoding
├── x86_64.rs        # x86_64 opcode tables
├── section.rs       # Section management
├── symbol.rs        # Symbol table
├── reloc.rs         # Relocation tracking
└── elf.rs           # ELF64 object file writer
```

### Data Structures

#### Token
```rust
enum Token {
    Directive(String),       // .text, .globl, etc.
    Label(String),           // foo:
    Instruction(String),     // mov, add, etc.
    Register(Register),      // %rax, %xmm0, etc.
    Immediate(i64),          // $123
    Symbol(String),          // main
    Plus, Minus, Star,       // +, -, *
    Comma, Colon,            // ,, :
    LParen, RParen,          // (, )
    Newline,
    At(String),              // @GOTPCREL, @PLT, etc.
}
```

#### Operand
```rust
enum Operand {
    Register(Register),
    Immediate(i64),
    Memory {
        base: Option<Register>,
        index: Option<Register>,
        scale: u8,           // 1, 2, 4, 8
        displacement: i64,
    },
    Symbol {
        name: String,
        addend: i64,
        reloc_type: RelocType,
    },
}
```

#### Instruction
```rust
struct Instruction {
    mnemonic: String,
    operands: Vec<Operand>,
    size_override: Option<OperandSize>,
    prefix: InstructionPrefix,
}

struct InstructionPrefix {
    lock: bool,
    rep: bool,
    rex: u8,
    segment: Option<Segment>,
}
```

#### Section
```rust
struct Section {
    name: String,
    typ: SectionType,       // SHT_PROGBITS, SHT_NOBITS
    flags: SectionFlags,    // SHF_WRITE, SHF_EXECINSTR, etc.
    data: Vec<u8>,
    relocations: Vec<Relocation>,
    align: u64,
}
```

#### Symbol
```rust
struct Symbol {
    name: String,
    section_idx: usize,
    value: u64,
    size: u64,
    binding: SymbolBinding,  // STB_GLOBAL, STB_LOCAL
    typ: SymbolType,         // STT_FUNC, STT_OBJECT, STT_TLS
    visibility: SymbolVisibility,
}
```

#### Relocation
```rust
struct Relocation {
    offset: u64,
    symbol_idx: usize,
    typ: RelocationType,
    addend: i64,
}
```

## Implementation Phases

### Phase 1: Lexer & Parser
**Goal**: Tokenize and parse assembly into an intermediate representation

- Implement tokenizer for AT&T syntax
- Parse directives (sections, symbols, data)
- Parse instructions with operands
- Build symbol table during parsing
- Handle label definitions and references

### Phase 2: Section & Symbol Management
**Goal**: Manage sections and symbol tables

- Implement section switching
- Track current section and offset
- Build symbol table with proper attributes
- Handle `.globl`, `.local`, `.type`, `.size`
- Implement alignment and padding

### Phase 3: Data Directive Emission
**Goal**: Emit data from directives

- Implement `.byte`, `.word`, `.long`, `.quad`
- Implement `.zero` for zero-filled data
- Handle symbolic references in data (track relocations)
- Implement `.comm` and `.align`

### Phase 4: Instruction Encoder - Part 1 (Integer)
**Goal**: Encode basic integer instructions

- Build opcode lookup tables
- Implement ModR/M and SIB byte encoding
- Encode REX prefix for 64-bit operations
- Implement common integer ALU ops: mov, add, sub, and, or, xor, cmp
- Implement shifts: shl, shr, sar
- Implement control flow: jmp, conditional jumps, call, ret
- Implement push/pop

### Phase 5: Instruction Encoder - Part 2 (SSE)
**Goal**: Encode SSE floating-point instructions

- Implement SSE data movement: movss, movsd, movq
- Implement SSE arithmetic: addss, addsd, subss, subsd, mulss, mulsd, divss, divsd
- Implement SSE comparisons: ucomiss, ucomisd
- Implement SSE conversions: cvtsi2ss, cvtsi2sd, cvttss2si, cvttsd2si
- Implement SSE logical: xorps, xorpd, pxor

### Phase 6: Instruction Encoder - Part 3 (x87 & Special)
**Goal**: Encode x87 FPU and special instructions

- Implement x87 data movement: fld*, fst*, fstp*
- Implement x87 arithmetic: fadd*, fsub*, fmul*, fdiv*
- Implement x87 comparison: fcomip, fucomip
- Implement x87 control: fnstcw, fldcw
- Implement x87 integer ops: fist*, fild*
- Implement atomic ops: lock prefix, cmpxchg, xchg
- Implement string ops: rep stosb

### Phase 7: Relocation Handling
**Goal**: Track and emit relocations

- Implement relocation tracking during encoding
- Support PC-relative relocations (calls, jumps, RIP-relative loads)
- Support GOT relocations (@GOTPCREL)
- Support PLT relocations (@PLT)
- Support TLS relocations (@tlsgd, @tpoff)
- Support absolute relocations (`.quad symbol`)

### Phase 8: ELF64 Object File Writer
**Goal**: Emit valid ELF64 relocatable object files

- Write ELF64 header
- Write section headers
- Write section contents (.text, .data, .bss, etc.)
- Write symbol table (.symtab)
- Write string tables (.strtab, .shstrtab)
- Write relocation sections (.rela.text, .rela.data)
- Handle section alignment and offsets correctly

### Phase 9: Debug Info Support
**Goal**: Emit DWARF debug line information

- Parse `.file` and `.loc` directives
- Build line number table
- Emit `.debug_line` section
- Link line info to sections

### Phase 10: Testing & Integration
**Goal**: Ensure assembler works with compiler output

- Test against all codegen test cases
- Compare output with GAS-generated objects
- Add assembler integration to compiler pipeline
- Add flag to switch between GAS and self-hosted assembler
- Benchmark performance

## References

- [x86_64 Instruction Reference](https://www.felixcloutier.com/x86/)
- [AMD64 ABI](https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-draft.pdf)
- [ELF-64 Specification](https://refspecs.linuxfoundation.org/elf/elf.pdf)
- [GAS Syntax](https://sourceware.org/binutils/docs/as/)
- [Intel Software Developer Manual](https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html)

## Success Criteria

1. Assembler can process all output from `src/codegen.rs`
2. Generated object files link correctly with system linker
3. Compiled programs pass all existing tests
4. No dependency on external assembler (GAS)
5. Performance is acceptable (< 2x slower than GAS)
