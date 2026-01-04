# CC3 Architecture

This is a C compiler written in Rust following Chibicc's diffs one by
one.

For now, there are 6 files:

- `src/main.rs`: uses `clap` to parse arguments and kickstart the
  compiler.
- `src/error.rs` defines the error type in use, `CompileError`.
- `src/lexer.rs` defines the lexer, which tokenizes input.
- `src/parser.rs` defines the recursive descent parser, with the
  entrypoint of `parse_program`.
- `src/codegen.rs` visits every ast node and generates code from it.
- `src/preprocessor.rs` implements the preprocessor.

## Testing

The test harness is defined in `tests/codegen.rs`. This harness loops
over all the `.c` files in tests and makes sure they return a status of
0 (pass). Each test file should have the structure:

```c
#include "test.h"

int main() {
  ASSERT(0, 0);
  // more test cases here

  printf("OK\n");
  return 0;
}
```

Run these with `cargo test` and accept snapshots with `cargo insta
accept`.
