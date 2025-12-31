int main() {
  int x;
  int *y = &x;
  int **z = &y;
  x = 3;
  return **z;
}
