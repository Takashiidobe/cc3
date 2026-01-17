#include "test.h"

int main(void) {
  bool t = true;
  bool f = false;
  ASSERT(1, t);
  ASSERT(0, f);
  int *p = nullptr;
  ASSERT(1, p == nullptr);
  ASSERT(0, p != nullptr);
  return 0;
}
