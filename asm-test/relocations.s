# Relocations
.text
.globl test_reloc
test_reloc:
  call printf@PLT
  mov global_var@GOTPCREL(%rip), %rax
  lea .L.str(%rip), %rdi
  ret

.data
global_var:
  .quad 0

.rodata
.L.str:
  .byte 72, 101, 108, 108, 111, 0  # "Hello"
