#include "test.h"

int main() {
  // Required C11 macros
  ASSERT(1, __STDC__);
  ASSERT(1, __STDC_HOSTED__);
  ASSERT(201112, __STDC_VERSION__); // C11

  // Optional feature support
  ASSERT(1, __STDC_UTF_16__);
  ASSERT(1, __STDC_UTF_32__);
  ASSERT(1, __STDC_IEC_559__); // IEEE 754 floating point

  // Features NOT supported (1 means not available)
  ASSERT(1, __STDC_NO_COMPLEX__);
  ASSERT(1, __STDC_NO_THREADS__);

  // These should NOT be defined (we support atomics and VLAs)
#ifdef __STDC_NO_ATOMICS__
  ASSERT(0, 1); // fail if defined
#endif
#ifdef __STDC_NO_VLA__
  ASSERT(0, 1); // fail if defined
#endif

  printf("OK\n");
  return 0;
}
