#include "test.h"

int main(void) {
  bool t = true;
  bool f = false;
  ASSERT(1, t);
  ASSERT(0, f);
  int *p = nullptr;
  ASSERT(1, p == nullptr);
  ASSERT(0, p != nullptr);

#define WARN_MSG "c23 warning"
#warning WARN_MSG

  int bin = 0b101010;
  ASSERT(42, bin);
  int dec = 1'000'000;
  int hex = 0xFF'FF;
  ASSERT(1000000, dec);
  ASSERT(65535, hex);

#define FOO 1
#define BAR 1
  int x = 0;
#if 0
  x = 1;
#elifdef FOO
  x = 2;
#elifndef BAR
  x = 3;
#else
  x = 4;
#endif
  ASSERT(2, x);

#undef BAR
#ifdef FOO
  x = 5;
#elifdef FOO
  x = 6;
#elifndef BAR
  x = 7;
#else
  x = 8;
#endif
  ASSERT(5, x);

#undef FOO
#if 0
  x = 9;
#elifdef FOO
  x = 10;
#elifndef BAR
  x = 11;
#else
  x = 12;
#endif
  ASSERT(11, x);
  return 0;
}
