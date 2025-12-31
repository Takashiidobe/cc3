int main() {
  int x;
  int a;
  int b;
  a = (&x + 2) - &x;
  b = (&x - (-1)) - &x;
  return a + b;
}
