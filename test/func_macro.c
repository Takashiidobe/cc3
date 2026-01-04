#include "test.h"

char *func_fn(void) {
  return __func__;
}

int main() {
  ASSERT(5, sizeof(__func__));
  ASSERT(0, strcmp("main", __func__));
  ASSERT(0, strcmp("func_fn", func_fn()));

  printf("OK\n");
  return 0;
}
