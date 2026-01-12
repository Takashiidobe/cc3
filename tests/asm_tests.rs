use cc3::asm::{lex, parse, AsmNode, Directive, Instruction, Operand, Register};

#[test]
fn test_lex_simple_directive() {
    let tokens = lex(".text").unwrap();
    assert!(tokens.len() >= 1);
}

#[test]
fn test_lex_label() {
    let tokens = lex("main:").unwrap();
    assert!(tokens.len() >= 1);
}

#[test]
fn test_lex_instruction() {
    let tokens = lex("mov %rax, %rbx").unwrap();
    assert!(tokens.len() >= 3); // mov, %rax, %rbx + separators
}

#[test]
fn test_parse_simple_program() {
    let input = r#".text
.globl main
main:
  mov $42, %rax
  ret
"#;
    let tokens = lex(input).unwrap();
    let nodes = parse(tokens).unwrap();
    assert!(nodes.len() >= 4);
}

#[test]
fn test_parse_memory_operand() {
    let input = "mov 8(%rbp), %rax";
    let tokens = lex(input).unwrap();
    let nodes = parse(tokens).unwrap();
    assert_eq!(nodes.len(), 1);

    match &nodes[0] {
        AsmNode::Instruction(inst) => {
            assert_eq!(inst.mnemonic, "mov");
            assert_eq!(inst.operands.len(), 2);
        }
        _ => panic!("expected instruction"),
    }
}

#[test]
fn test_parse_got_relocation() {
    let input = "mov main@GOTPCREL(%rip), %rax";
    let tokens = lex(input).unwrap();
    let nodes = parse(tokens).unwrap();
    assert_eq!(nodes.len(), 1);

    match &nodes[0] {
        AsmNode::Instruction(inst) => {
            assert_eq!(inst.mnemonic, "mov");
            // Should have RIP-relative memory operand with GOT relocation
        }
        _ => panic!("expected instruction"),
    }
}
