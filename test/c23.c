#include "test.h"

int main(void) {
  bool t = true;
  bool f = false;
  ASSERT(1, t);
  ASSERT(0, f);
  return 0;
}
