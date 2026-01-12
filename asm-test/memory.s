# Memory operands
.text
.globl test_memory
test_memory:
  push %rbp
  mov %rsp, %rbp
  mov 8(%rbp), %rax
  mov 16(%rbp), %rcx
  mov -8(%rbp), %rdx
  mov (%rax), %rbx
  mov 0(%rax,%rcx,8), %rsi
  pop %rbp
  ret
