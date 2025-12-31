int main() {
  int x;
  x = 3;
  *(&*&x) = 5;
  return x;
}
