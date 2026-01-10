int main() {
  struct { int x:1; } s;
  &s.x;
  return 0;
}
