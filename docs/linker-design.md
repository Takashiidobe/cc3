# Linker Design Document

## Overview

This document describes the design and implementation plan for a self-hosted x86_64 ELF linker for the cc3 compiler. The linker will combine relocatable object files into executable binaries or shared libraries, eliminating our dependency on external linkers like GNU ld.

## Goals

1. **Self-hosting**: Enable the compiler to link its own output without relying on system linker
2. **ELF64 output**: Generate proper ELF64 executables and shared libraries
3. **Static linking**: Support linking static executables with libc
4. **Dynamic linking**: Support dynamic linking with shared libraries and PLT/GOT
5. **Section merging**: Combine sections from multiple object files
6. **Symbol resolution**: Resolve symbols across object files and libraries
7. **Relocation application**: Apply relocations to generate final executable code
8. **TLS support**: Thread-local storage for both static and dynamic linking
9. **Link optimizations**: Dead code elimination via `--gc-sections`

## Linker Responsibilities

### Input Processing
- Parse multiple ELF64 relocatable object files (`.o`)
- Parse static archive files (`.a`)
- Parse shared library files (`.so`)
- Handle linker scripts (basic subset)

### Symbol Resolution
- Build global symbol table from all input files
- Resolve undefined symbols
- Handle strong vs weak symbols
- Detect multiple definitions and undefined symbols
- Symbol visibility (global, local, hidden, protected)

### Section Management
- Merge sections with the same name from different object files
- Apply section alignment requirements
- Assign virtual addresses to sections
- Create output sections (`.text`, `.data`, `.bss`, `.rodata`, etc.)

### Relocation Processing
- Parse relocation entries from input objects
- Apply relocations using resolved symbol values
- Handle PC-relative and absolute relocations
- Generate PLT and GOT for dynamic linking

### Output Generation
- Write ELF64 executable or shared library
- Create program headers for loading
- Create section headers
- Generate `.dynamic` section for dynamic linking
- Generate `.interp` section for dynamic loader path
- Generate symbol tables (`.symtab`, `.dynsym`)
- Generate hash tables (`.hash`, `.gnu.hash`) for dynamic symbol lookup

## Linker Features from `run_linker()`

Based on `src/main.rs`, the linker must support:

### Command-line Options
- `-o <output>` - Output file path
- `-static` - Static linking (no dynamic dependencies)
- `-shared` - Produce shared library
- `-s` - Strip symbols
- `-L<dir>` - Add library search path
- `-l<lib>` - Link with library
- `-Wl,<option>` - Pass option to linker:
  - `--gc-sections` - Dead code elimination
  - `--start-group` / `--end-group` - Archive grouping for circular dependencies
  - `-Map,<file>` - Generate link map file
- `-Xlinker <arg>` - Pass single argument to linker

### Library Search
- Search for libraries in specified directories (`-L`)
- Search system library paths (`/lib`, `/usr/lib`, `/usr/local/lib`)
- Handle both static (`.a`) and shared (`.so`) libraries
- Prefer shared libraries over static by default (unless `-static`)

### Section Handling
- Merge text sections: `.text.*` → `.text`
- Merge data sections: `.data.*` → `.data`
- Merge rodata sections: `.rodata.*` → `.rodata`
- Merge BSS sections: `.bss.*` → `.bss`
- Handle TLS sections: `.tdata`, `.tbss`
- Function sections support (for `--gc-sections`)

## Architecture

### Module Structure

```
src/link/
├── mod.rs           # Public API
├── object.rs        # ELF object file parser
├── archive.rs       # Static archive (.a) parser
├── symbol.rs        # Symbol table and resolution
├── section.rs       # Section management and merging
├── reloc.rs         # Relocation application
├── layout.rs        # Address assignment and layout
├── got_plt.rs       # GOT/PLT generation for dynamic linking
├── tls.rs           # TLS section handling
├── dynamic.rs       # Dynamic linking support
├── gc.rs            # Garbage collection (--gc-sections)
└── writer.rs        # ELF executable/shared library writer
```

### Data Structures

#### ObjectFile
```rust
struct ObjectFile {
    path: PathBuf,
    elf_header: Elf64Header,
    sections: Vec<InputSection>,
    symbols: Vec<Symbol>,
    relocations: Vec<Relocation>,
    is_dso: bool,  // Shared object
}
```

#### Archive
```rust
struct Archive {
    path: PathBuf,
    members: Vec<ArchiveMember>,
}

struct ArchiveMember {
    name: String,
    offset: u64,
    symbols: Vec<String>,  // Exported symbols
}
```

#### Symbol
```rust
struct Symbol {
    name: String,
    value: u64,
    size: u64,
    section_idx: Option<usize>,  // None for undefined
    binding: SymbolBinding,      // Global, Local, Weak
    typ: SymbolType,             // Func, Object, TLS, etc.
    visibility: SymbolVisibility,
    version: Option<String>,     // For versioned symbols
    file_idx: usize,             // Source file
}
```

#### InputSection
```rust
struct InputSection {
    name: String,
    typ: SectionType,
    flags: SectionFlags,
    data: Vec<u8>,
    align: u64,
    relocations: Vec<Relocation>,
    file_idx: usize,
    original_idx: usize,
}
```

#### OutputSection
```rust
struct OutputSection {
    name: String,
    typ: SectionType,
    flags: SectionFlags,
    vaddr: u64,
    offset: u64,
    size: u64,
    align: u64,
    input_sections: Vec<usize>,  // Indices into merged sections
}
```

#### Relocation
```rust
struct Relocation {
    offset: u64,
    typ: RelocationType,
    symbol_idx: usize,
    addend: i64,
}
```

#### LinkContext
```rust
struct LinkContext {
    inputs: Vec<ObjectFile>,
    archives: Vec<Archive>,
    symbols: HashMap<String, usize>,  // Name -> symbol index
    sections: Vec<InputSection>,
    output_sections: Vec<OutputSection>,
    got_entries: Vec<GotEntry>,
    plt_entries: Vec<PltEntry>,
    tls_size: u64,
    entry_point: u64,
    is_static: bool,
    is_shared: bool,
}
```

## ELF Executable Layout

For a typical static executable:

```
ELF Header
Program Headers:
  PT_LOAD (RX) - .text, .rodata
  PT_LOAD (RW) - .data, .bss
  PT_TLS       - .tdata, .tbss (if TLS used)
  PT_GNU_STACK - Stack permissions

Sections:
  .interp      - Path to dynamic linker (if dynamic)
  .text        - Executable code
  .rodata      - Read-only data
  .data        - Initialized data
  .bss         - Uninitialized data
  .tdata       - TLS initialized data
  .tbss        - TLS uninitialized data
  .got         - Global Offset Table (if dynamic)
  .plt         - Procedure Linkage Table (if dynamic)
  .dynamic     - Dynamic linking information (if dynamic)
  .dynsym      - Dynamic symbol table (if dynamic)
  .dynstr      - Dynamic string table (if dynamic)
  .hash        - Symbol hash table (if dynamic)
  .symtab      - Symbol table (debug/static)
  .strtab      - String table
  .shstrtab    - Section header string table
```

## Relocation Types

Must support all relocation types emitted by our assembler:

- `R_X86_64_64` - Direct 64-bit absolute
- `R_X86_64_PC32` - PC-relative 32-bit signed
- `R_X86_64_PLT32` - PC-relative to PLT entry
- `R_X86_64_GOTPCREL` - PC-relative to GOT entry
- `R_X86_64_32` - Direct 32-bit zero-extended
- `R_X86_64_32S` - Direct 32-bit sign-extended
- `R_X86_64_TLSGD` - TLS General Dynamic
- `R_X86_64_TPOFF32` - TLS Thread-local offset
- `R_X86_64_GOTTPOFF` - GOT offset for TLS IE

## Implementation Phases

### Phase 1: ELF Object File Parser
**Goal**: Parse ELF64 relocatable object files

- Parse ELF64 header
- Parse section headers
- Read section contents
- Parse symbol table (`.symtab`)
- Parse string table (`.strtab`)
- Parse relocation sections (`.rela.text`, `.rela.data`)
- Build internal representation

### Phase 2: Symbol Table & Resolution
**Goal**: Build global symbol table and resolve symbols

- Collect symbols from all input files
- Build symbol name → symbol mapping
- Implement symbol resolution rules (strong vs weak)
- Handle undefined symbols
- Detect multiple definitions
- Support `--start-group` / `--end-group` for archives

### Phase 3: Section Merging
**Goal**: Combine sections from multiple inputs

- Group sections by name and type
- Merge `.text.*` sections into `.text`
- Merge `.data.*`, `.rodata.*`, `.bss.*`
- Handle alignment requirements
- Preserve section order for reproducibility
- Create output section layout

### Phase 4: Basic Static Linking
**Goal**: Link simple static executables

- Assign virtual addresses to sections
- Apply relocations (PC-relative and absolute)
- Generate ELF64 executable header
- Generate program headers (PT_LOAD segments)
- Write output sections
- Set entry point (`_start`)
- Support `-o` output path

### Phase 5: Archive Support
**Goal**: Link with static libraries (`.a` files)

- Parse ar archive format
- Extract archive member symbols
- Implement lazy symbol resolution for archives
- Pull in only needed archive members
- Handle circular dependencies with `--start-group`

### Phase 6: Section Garbage Collection
**Goal**: Implement `--gc-sections` dead code elimination

- Track section dependencies via relocations
- Mark sections reachable from entry point
- Mark sections referenced by `--undefined` or exports
- Sweep unreachable sections
- Reduce output size significantly

### Phase 7: TLS Support (Static)
**Goal**: Thread-local storage for static executables

- Merge `.tdata` and `.tbss` sections
- Create PT_TLS program header
- Apply TLS relocations (TP-relative)
- Calculate TLS block size and alignment
- Handle `%fs:` segment register access

### Phase 8: GOT and PLT Generation
**Goal**: Support position-independent code

- Generate Global Offset Table (`.got`)
- Generate Procedure Linkage Table (`.plt`)
- Apply GOT-relative relocations
- Apply PLT-relative relocations
- Handle `-fpic` compiled objects

### Phase 9: Dynamic Linking Support
**Goal**: Link with shared libraries and create executables with dynamic dependencies

- Parse shared libraries (`.so`)
- Import symbols from shared libraries
- Create `.dynamic` section
- Create `.dynsym` and `.dynstr` sections
- Create `.interp` section with dynamic linker path
- Generate `.hash` or `.gnu.hash` for symbol lookup
- Apply dynamic relocations
- Handle symbol versions
- Create DT_NEEDED entries for dependencies

### Phase 10: Shared Library Creation
**Goal**: Create shared libraries with `-shared`

- Support `-shared` flag
- Export symbols appropriately
- Generate `.init` and `.fini` sections
- Handle SONAME
- Support symbol visibility
- Create proper dynamic relocations

### Phase 11: Link Map & Debugging
**Goal**: Generate link map and improve debugging

- Implement `-Wl,-Map,<file>` link map generation
- Show section addresses and sizes
- Show symbol definitions and references
- Show discarded sections (from `--gc-sections`)
- Improve error messages for undefined symbols
- Show file and section for each symbol

### Phase 12: Optimizations & Testing
**Goal**: Optimize and thoroughly test linker

- Optimize symbol lookup (hash tables)
- Optimize section merging
- Parallel section processing
- Test against all compiler test cases
- Compare output with GNU ld
- Benchmark linking performance
- Add integration tests for complex scenarios

## Special Considerations

### Entry Point
- Default entry point: `_start`
- Can be overridden with `-e` or `--entry`
- Must exist in static linking
- Optional for shared libraries

### Standard Sections
- `.text` - Executable code (RX)
- `.rodata` - Read-only data (R)
- `.data` - Initialized writable data (RW)
- `.bss` - Zero-initialized data (RW)
- `.tdata` - TLS initialized data
- `.tbss` - TLS zero-initialized data

### Library Search Order
1. Directories specified with `-L`
2. System directories: `/lib`, `/usr/lib`, `/usr/local/lib`
3. For `-lxxx`, search for:
   - `libxxx.so` (if not `-static`)
   - `libxxx.a`

### Symbol Resolution Priority
1. Strong symbols from object files
2. Weak symbols from object files
3. Strong symbols from archives
4. Weak symbols from archives
5. Symbols from shared libraries (lowest priority)

## Error Handling

### Fatal Errors
- Undefined symbol after all resolution
- Multiple strong definitions of same symbol
- Invalid ELF format
- Section address overflow
- Relocation overflow
- Missing required sections (`_start` in static)

### Warnings
- Weak symbol override
- Section alignment conflicts
- Unused sections
- Unused archive members

## References

- [ELF-64 Specification](https://refspecs.linuxfoundation.org/elf/elf.pdf)
- [System V ABI](https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-draft.pdf)
- [Linkers and Loaders by John Levine](https://www.iecc.com/linker/)
- [LLD Linker Documentation](https://lld.llvm.org/)
- [GNU ld Documentation](https://sourceware.org/binutils/docs/ld/)
- [A Whirlwind Tutorial on Creating Really Teensy ELF Executables for Linux](http://www.muppetlabs.com/~breadbox/software/tiny/teensy.html)

## Success Criteria

1. Linker can combine object files from our assembler
2. Generated executables run correctly
3. All compiler tests pass when using self-hosted toolchain
4. Static linking with libc works
5. Basic dynamic linking works
6. `--gc-sections` reduces binary size
7. Link map generation works
8. Performance is acceptable (< 3x slower than GNU ld for similar size)
9. No dependency on external linker
