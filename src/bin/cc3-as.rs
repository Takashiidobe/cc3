// Standalone assembler binary
//
// This is a placeholder for the standalone assembler.
// Since cc3 is a binary-only crate, this bin can't directly access the asm module.
//
// For Phase 1, the lexer and parser are tested via the test modules in:
//   - src/asm/lexer.rs
//   - src/asm/parser.rs
//
// In Phase 10 (Testing & Integration), we'll integrate the assembler into the
// main compiler driver where it can be accessed.

use std::process;

fn main() {
    eprintln!("cc3-as: Standalone assembler (placeholder)");
    eprintln!();
    eprintln!("The assembler lexer and parser are implemented and tested.");
    eprintln!("Run: cargo test --bin cc3");
    eprintln!();
    eprintln!("Integration into the compiler will happen in Phase 10.");
    process::exit(0);
}
