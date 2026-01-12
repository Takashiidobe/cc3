# Floating point operations
.text
.globl test_float
test_float:
  # SSE operations
  movss %xmm0, %xmm1
  addss %xmm1, %xmm0
  movsd %xmm2, %xmm3
  mulsd %xmm3, %xmm2

  # x87 FPU operations
  flds 8(%rbp)
  fldl 16(%rbp)
  faddp %st(0), %st(1)
  fstpl -8(%rbp)

  ret
