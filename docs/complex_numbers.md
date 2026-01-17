# Complex Numbers Implementation Plan

## Overview

C11 introduces `_Complex` as a type specifier for complex numbers:

```c
#include <complex.h>

double _Complex z1 = 1.0 + 2.0*I;
float _Complex z2 = 3.0f + 4.0f*I;
long double _Complex z3 = 1.0L + 2.0L*I;

double _Complex sum = z1 + z2;
double _Complex prod = z1 * z2;
double re = creal(z1);  // Get real part
double im = cimag(z1);  // Get imaginary part
```

## Implementation Strategy

Leverage libgcc runtime functions for complex multiplication and division to avoid implementing tricky edge case handling (infinities, NaN, overflow).

### Type System (ast.rs)

Add three new type variants:

```rust
enum Type {
    // ... existing types ...
    FloatComplex,   // float _Complex - 8 bytes (2x float)
    DoubleComplex,  // double _Complex - 16 bytes (2x double)
    LDoubleComplex, // long double _Complex - 32 bytes (2x long double)
}
```

Size and alignment:
- `float _Complex`: 8 bytes, 4-byte aligned
- `double _Complex`: 16 bytes, 8-byte aligned
- `long double _Complex`: 32 bytes, 16-byte aligned

### Lexer (lexer.rs)

Add `_Complex` keyword:

```rust
enum Keyword {
    // ... existing keywords ...
    Complex,  // _Complex
}
```

Map `"_Complex"` to `Keyword::Complex` in `match_keyword()`.

### Parser (parser.rs)

Handle `_Complex` in `parse_declspec()`:

```rust
const COMPLEX: i32 = 1 << 20;  // New flag

// In the match for type combinations:
FLOAT | COMPLEX => Type::FloatComplex,
DOUBLE | COMPLEX => Type::DoubleComplex,
LONG | DOUBLE | COMPLEX => Type::LDoubleComplex,
```

### Codegen (codegen.rs)

#### Storage Layout

Complex numbers are stored as two adjacent values `{real, imag}`:

```
float _Complex:   [float real][float imag]     = 8 bytes
double _Complex:  [double real][double imag]   = 16 bytes
```

#### Load/Store

Treat as loading/storing two floating-point values. Can use SSE/AVX registers for efficiency.

#### Addition and Subtraction (inline)

Straightforward component-wise operations:

```
(a + bi) + (c + di) = (a + c) + (b + d)i
(a + bi) - (c + di) = (a - c) + (b - d)i
```

Generate inline code - just two floating-point add/sub operations.

#### Multiplication (call libgcc)

Complex multiplication has edge cases with infinities/NaN that are tricky to handle correctly. Call libgcc:

```c
// libgcc provides:
float _Complex __mulsc3(float a, float b, float c, float d);   // (a+bi)*(c+di)
double _Complex __muldc3(double a, double b, double c, double d);
long double _Complex __mulxc3(long double a, long double b, long double c, long double d);
```

Generate a call to the appropriate function based on type.

#### Division (call libgcc)

Complex division is even trickier. Call libgcc:

```c
// libgcc provides:
float _Complex __divsc3(float a, float b, float c, float d);   // (a+bi)/(c+di)
double _Complex __divdc3(double a, double b, double c, double d);
long double _Complex __divxc3(long double a, long double b, long double c, long double d);
```

#### Comparison

Only `==` and `!=` are defined for complex numbers. Compare both real and imaginary parts.

#### Casts

- Real to complex: `(double _Complex)x` becomes `x + 0i`
- Complex to real: `(double)z` extracts the real part (imaginary discarded)
- Between complex types: convert both parts

### Preprocessor

Remove `__STDC_NO_COMPLEX__` macro once implemented.

### Built-in Extensions (optional)

GCC provides `__real__` and `__imag__` to access parts:

```c
double _Complex z = 1.0 + 2.0*I;
double r = __real__ z;  // 1.0
double i = __imag__ z;  // 2.0
__real__ z = 3.0;       // Can also assign
```

These are useful but optional - `<complex.h>` provides `creal()` and `cimag()` macros/functions.

## What We Get For Free

The following come from `<complex.h>` and libm, no compiler work needed:
- `creal()`, `cimag()` - access parts
- `cabs()` - absolute value
- `carg()` - argument (phase angle)
- `conj()` - complex conjugate
- `cexp()`, `clog()`, `cpow()` - exponential/logarithm
- `csin()`, `ccos()`, `ctan()` - trigonometric
- `csqrt()` - square root
- `I` macro - imaginary unit

## Files to Modify

1. `src/ast.rs` - Add `FloatComplex`, `DoubleComplex`, `LDoubleComplex` to `Type` enum
2. `src/lexer.rs` - Add `Complex` keyword
3. `src/parser.rs` - Handle `_Complex` in type parsing
4. `src/codegen.rs` - Handle complex storage, arithmetic, casts
5. `src/preprocessor.rs` - Remove `__STDC_NO_COMPLEX__`

## Testing

Create `test/complex.c`:

```c
#include "test.h"
#include <complex.h>

int main() {
    // Size tests
    ASSERT(8, sizeof(float _Complex));
    ASSERT(16, sizeof(double _Complex));

    // Basic operations
    double _Complex z1 = 1.0 + 2.0*I;
    double _Complex z2 = 3.0 + 4.0*I;

    // Addition
    double _Complex sum = z1 + z2;
    ASSERT(4.0, creal(sum));
    ASSERT(6.0, cimag(sum));

    // Multiplication: (1+2i)*(3+4i) = 3+4i+6i+8i² = 3+10i-8 = -5+10i
    double _Complex prod = z1 * z2;
    ASSERT(-5.0, creal(prod));
    ASSERT(10.0, cimag(prod));

    // Division
    double _Complex quot = z2 / z1;  // (3+4i)/(1+2i)
    // = (3+4i)(1-2i)/((1+2i)(1-2i)) = (3-6i+4i-8i²)/(1+4) = (11-2i)/5
    ASSERT(2.2, creal(quot));
    ASSERT(-0.4, cimag(quot));

    printf("OK\n");
    return 0;
}
```

## Estimated Effort

 | Component                      | Lines of Code | Difficulty |
 |--------------------------------|---------------|------------|
 | Type enum + size/align         | ~30           | Low        |
 | Lexer keyword                  | ~5            | Low        |
 | Parser declspec                | ~20           | Low        |
 | Codegen storage                | ~50           | Medium     |
 | Codegen add/sub                | ~30           | Low        |
 | Codegen mul/div (libgcc calls) | ~50           | Medium     |
 | Codegen casts                  | ~40           | Medium     |
 | Codegen comparison             | ~20           | Low        |
 | **Total**                      | ~250          | Medium     |
